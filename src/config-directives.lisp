(in-package #:cl-tmux/config)

;;; ── Runtime sb-posix helpers ─────────────────────────────────────────────────
;;; ── Renderer mouse-reporting hook ────────────────────────────────────────────
;;;
;;; %apply-option-side-effects must call the renderer to enable/disable mouse
;;; reporting when the 'mouse' option changes, but the config layer cannot carry
;;; a compile-time dependency on cl-tmux/renderer (circular).  A registered
;;; callback (consistent with *command-hook-runner*) is the solution: the
;;; renderer/orchestrate layer sets this at startup; config calls it without
;;; knowing who owns the terminal.

(defvar *mouse-reporting-hook* nil
  "When non-NIL, a function (enable-p) called whenever the 'mouse' option changes.
   ENABLE-P is T to enable mouse reporting, NIL to disable it.  Set by the
   orchestrate layer (events-loop or main.lisp) to cl-tmux/renderer:enable/disable.")

;;; ── Environment-variable helper ─────────────────────────────────────────────
;;;
;;; set-environment / setenv directives need to mutate the process environment.
;;; SB-POSIX is looked up lazily at call time — it is not an ASDF dependency of
;;; cl-tmux so it may not be loaded when this file first loads, but it IS loaded
;;; before any runtime or test caller reaches these functions.

(defun %config-posix-fn (name)
  "Return the SB-POSIX function named NAME (by call-time find-symbol), or NIL
   when SB-POSIX is absent or the function is not exported.  Defers the lookup
   so the defvar-at-load-time NIL trap is avoided."
  (let ((pkg (find-package "SB-POSIX")))
    (and pkg (find-symbol name pkg))))

(defun %config-setenv (name value)
  "Set environment variable NAME to VALUE for child processes.
   Looks up SB-POSIX:SETENV lazily.  A no-op when sb-posix is absent."
  (let ((fn (%config-posix-fn "SETENV")))
    (when fn (ignore-errors (funcall fn name value 1)))))

;;; ── run-shell tilde expansion helper ─────────────────────────────────────────

(defun %expand-leading-tilde (cmd)
  "Expand a leading \"~/\" in CMD to \"$HOME/\" using the HOME environment
   variable, so `run '~/.tmux/plugins/tpm/tpm'` resolves to the user's home.
   Leaves absolute (\"/abs\") and relative (\"rel\") strings unchanged.  Pure
   string transformation: returns CMD unchanged when it does not begin with ~/."
  (if (and (> (length cmd) 2)
           (char= (char cmd 0) #\~)
           (char= (char cmd 1) #\/))
      (concatenate 'string
                   (or (ignore-errors (sb-ext:posix-getenv "HOME")) "~")
                   (subseq cmd 1))
      cmd))

;;; ── Declarative directive dispatch macro ──────────────────────────────────

(defmacro define-config-directives (&rest rules)
  "Build %APPLY-CONFIG-DIRECTIVE-INNER from a declarative table of directive RULES.

   Each RULE has one of two forms:
     (NAME ARITY (ARG...) &body BODY)
       NAME   – the directive keyword as a string (e.g. \"set-shell\")
       ARITY  – the exact number of arguments the directive takes
       (ARG…) – symbols bound to those arguments inside BODY
       BODY   – forms run when NAME matches with the right ARITY; their value is
                returned (non-NIL ⇒ the directive was applied).

     (:aliases (NAME...) ARITY (ARG...) &body BODY)
       Identical to the single-name form except CMD matches any string in (NAME...).
       Eliminates alias repetition (source-file/source, set/setw/…, etc.).

   The outer APPLY-CONFIG-DIRECTIVE function wraps this inner dispatcher and
   handles 'bind' with variable-arity flags separately."
  (flet ((expand-rule (rule)
           ;; Returns a list of cond arms (one arm per name).
           (if (eq (first rule) :aliases)
               ;; (:aliases (name...) arity arglist body...)
               (destructuring-bind (names arity arglist &body body) (rest rule)
                 (mapcar (lambda (name)
                           `((and (string= cmd ,name) (= (length args) ,arity))
                             (destructuring-bind ,arglist args
                               (declare (ignorable ,@arglist))
                               ,@body)))
                         names))
               ;; (name arity arglist body...)
               (destructuring-bind (name arity arglist &body body) rule
                 (list `((and (string= cmd ,name) (= (length args) ,arity))
                         (destructuring-bind ,arglist args
                           (declare (ignorable ,@arglist))
                           ,@body)))))))
    `(defun %apply-config-directive-inner (tokens)
       "Apply one non-bind config directive (list of string TOKENS) to live state.
        Returns T when applied, NIL for an unknown/invalid directive."
       (when tokens
         (let ((cmd (first tokens)) (args (rest tokens)))
           (declare (ignorable args))
           (cond
             ,@(mapcan #'expand-rule rules)
             (t nil)))))))

;;; ── bind-key flag parsing ────────────────────────────────────────────────
;;;
;;; %parse-bind-key-args handles the optional flags before key and command:
;;;   bind [-n] [-r] [-T table] key command
;;; Returns (values table key command repeatable) or NIL on parse failure.

(defun %parse-bind-key-args (args)
  "Parse the ARGS list for a bind directive (excludes the \"bind\" verb itself).
   Returns (values table key command repeatable note) where TABLE is +TABLE-PREFIX+
   by default and NOTE is the -N description string (or NIL), or NIL when ARGS do
   not form a valid binding."
  (let ((table      +table-prefix+)
        (repeatable nil)
        (note       nil)
        (remaining  args))
    (loop
      (cond
        ((null remaining) (return nil))
        ((string= (first remaining) "-n")
         (setf table     +table-root+)
         (setf remaining (rest remaining)))
        ((string= (first remaining) "-r")
         (setf repeatable t)
         (setf remaining  (rest remaining)))
        ((string= (first remaining) "-T")
         (setf remaining (rest remaining))
         (when (null remaining) (return nil))
         (setf table     (first remaining))
         (setf remaining (rest remaining)))
        ;; -N "note": tmux 3.1+ key-binding description.  Capture the (already
        ;; single-token, quote-joined) note argument so list-keys can display it.
        ;; It MUST be consumed here — otherwise the fall-through below would
        ;; mis-read "-N" as the key and the note as the command.
        ((string= (first remaining) "-N")
         (setf remaining (rest remaining))
         (when (null remaining) (return nil))
         (setf note      (first remaining))
         (setf remaining (rest remaining)))
        (t
         ;; Need a key plus at least one command token.
         (when (null (rest remaining)) (return nil))
         (let* ((key-token  (%parse-key-token (first remaining)))
                ;; Strip an optional { ... } block wrapper (tmux 3.x brace
                ;; syntax) so it reuses the semicolon-sequence machinery below.
                (cmd-tokens (%strip-brace-block (rest remaining)))
                ;; Split on ";" tokens to support multi-command sequences:
                ;; bind r source-file ~/.tmux.conf \; display "Reloaded!"
                ;; — or:  bind r { source-file ~/.tmux.conf ; display "Reloaded!" }
                (sequences  (%split-on-semicolons cmd-tokens)))
           ;; An empty block (`bind r { }`) leaves no command — reject it.
           (when (null cmd-tokens) (return nil))
           (return
             (if (= (length sequences) 1)
                 ;; Single command: use the existing single-command path.
                 (let ((tokens (first sequences)))
                   (if (= (length tokens) 1)
                       ;; Single word: resolve to a keyword.
                       (let ((keyword (%command-keyword (first tokens))))
                         (if keyword (values table key-token keyword repeatable note) nil))
                       ;; Multi-token: store as token list.
                       (values table key-token tokens repeatable note)))
                 ;; Multiple commands: store as :sequence list of token lists.
                 (values table key-token (cons :sequence sequences) repeatable note)))))))))

;;; ── Semicolon-sequence splitter ──────────────────────────────────────────
;;;
;;; tmux bind directives support ";" (from "\;" in the config line) as a
;;; command separator: bind r source-file ~/.tmux.conf \; display "Reloaded!"
;;; %split-on-semicolons splits a flat token list on ";" tokens,
;;; removing empty segments, yielding a list of per-command token lists.

(defun %strip-brace-block (tokens)
  "When TOKENS form a `{ ... }` block — first token \"{\" and last token \"}\" —
   return the inner tokens; otherwise return TOKENS unchanged.  This lets the
   tmux 3.x brace form `bind r { cmd1 ; cmd2 }` reuse %split-on-semicolons
   exactly like the older `bind r cmd1 \\; cmd2` form.  An empty block `{ }`
   yields NIL (no commands)."
  (if (and (cdr tokens)
           (string= (first tokens) "{")
           (string= (car (last tokens)) "}"))
      (butlast (rest tokens))
      tokens))

(defun %split-on-semicolons (tokens)
  "Split TOKENS on \";\" tokens, returning a list of per-command token lists.
   Empty segments (consecutive semicolons or trailing) are discarded.
   When no semicolons are present, returns (list tokens) unchanged."
  (let ((result  '())
        (current '()))
    (dolist (tok tokens)
      (if (string= tok ";")
          (progn (when current (push (nreverse current) result))
                 (setf current '()))
          (push tok current)))
    (when current (push (nreverse current) result))
    (if result (nreverse result) (list tokens))))

;;; ── unbind-key flag parsing ──────────────────────────────────────────────
;;;
;;; %parse-unbind-key-args handles optional [-n] [-T table] flags before the key.
;;; Returns (values table key) or (values nil nil) on parse failure.

(defun %parse-unbind-key-args (args)
  "Parse the ARGS list for an unbind directive (excludes the verb itself).
   Returns (values TABLE KEY ALL-P): TABLE is +TABLE-PREFIX+ by default, -n selects
   +TABLE-ROOT+, -T <table> a named table, and -a marks 'unbind every key in the
   table' (KEY is then NIL — the real tmux `unbind -a [-T table]` form).  Returns
   (values nil nil nil) on parse failure."
  (let ((table     +table-prefix+)
        (all-p     nil)
        (remaining args))
    (loop
      (cond
        ((null remaining)
         ;; End of args: valid only when -a was given (whole-table unbind).
         (return (if all-p (values table nil t) (values nil nil nil))))
        ((string= (first remaining) "-n")
         (setf table     +table-root+)
         (setf remaining (rest remaining)))
        ((string= (first remaining) "-a")
         (setf all-p     t)
         (setf remaining (rest remaining)))
        ((string= (first remaining) "-T")
         (setf remaining (rest remaining))
         (when (null remaining) (return (values nil nil nil)))
         (setf table     (first remaining))
         (setf remaining (rest remaining)))
        (t
         (unless (= (length remaining) 1) (return (values nil nil nil)))
         (return (values table (%parse-key-token (first remaining)) all-p)))))))

;;; ── Declarative bind/unbind verb dispatch ────────────────────────────────

(defmacro define-key-directive-handlers (&rest rules)
  "Build %APPLY-KEY-DIRECTIVE from a declarative table of verb RULES.
   Each RULE is (VERBS &body BODY) where VERBS is a list of verb strings
   and BODY is evaluated with CMD and ARGS in scope."
  `(defun %apply-key-directive (cmd args)
     "Dispatch a bind/unbind directive.  Returns T on success, NIL on failure."
     (cond
       ,@(mapcar
          (lambda (rule)
            (destructuring-bind (verbs &body body) rule
              `((member cmd ',verbs :test #'string=)
                ,@body)))
          rules)
       (t nil))))

(define-key-directive-handlers
  (("bind" "bind-key")
   (multiple-value-bind (table key command repeatable note)
       (%parse-bind-key-args args)
     (when command
       ;; COMMAND is a keyword (built-in) or a token list (`bind key cmd args`).
       ;; NOTE is the optional -N description, surfaced by list-keys.
       (key-table-bind table key command :repeatable repeatable :note note)
       t)))
  (("unbind" "unbind-key")
   (multiple-value-bind (table key all-p)
       (%parse-unbind-key-args args)
     (cond
       ;; -a: clear every binding in TABLE (the real tmux `unbind -a [-T t]`).
       (all-p
        (let ((inner (gethash table *key-tables*)))
          (when inner (clrhash inner)))
        t)
       ((and table key)
        (let ((tbl (gethash table *key-tables*)))
          (when tbl (remhash key tbl)))
        t)
       (t nil))))
  ;; unbind-all [-T table]: clear all bindings in a key-table (default: prefix).
  ;; -T specifies the table; without -T the prefix table is cleared.
  (("unbind-all")
   (let* ((t-pos  (position "-T" args :test #'string=))
          (table  (if (and t-pos (nth (1+ t-pos) args))
                      (nth (1+ t-pos) args)
                      +table-prefix+))
          (inner  (gethash table *key-tables*)))
     (when inner (clrhash inner))
     t)))

;;; ── Simple directive definitions ─────────────────────────────────────────
;;;
;;; The six set-option variants (set, set-option, setw, set-window-option,
;;; sets, set-session-option) all forward to cl-tmux/options:set-option at
;;; config-file load time, because no session/window/pane context is available
;;; during config parsing.
;;;
;;; Runtime commands that carry a window or pane context should call
;;; cl-tmux/options:set-option-for-window / set-option-for-pane directly to
;;; store in the per-struct local-options hash.

(define-config-directives
  ("set-shell" 1 (path)
    (setf *default-shell* path)
    t)
  ("set-status-height" 1 (n)
    (let ((height (parse-integer n :junk-allowed t)))
      (when (and height (plusp height))
        (setf *status-height* height)
        t)))
  (:aliases ("set" "set-option" "setw" "set-window-option" "sets" "set-session-option")
    2 (option-name option-value)
    (cl-tmux/options:set-option option-name option-value)
    t)
  ("set-hook" 2 (event-name command-name)
    (let ((keyword (%command-keyword command-name)))
      (when keyword
        (cl-tmux/hooks:set-command-hook event-name keyword)
        t)))
  ;; NOTE: source-file/source are handled entirely by %apply-source-file-directive
  ;; (wired into apply-config-directive before this table) to support -q/-n/-v
  ;; flags, glob patterns, and multiple paths.
  ;; NOTE: run-shell/run are handled entirely by %apply-run-shell-directive
  ;; (wired into apply-config-directive before this fixed-arity table), which
  ;; covers the bare 1-arg form as well as the flag-bearing forms.  No fixed-
  ;; arity entries are needed here.
  ;; set-environment / setenv 2-arg form: VAR VALUE (no flags).
  ;; The %apply-set-environment-directive handler in apply-config-directive
  ;; intercepts this first (handling -r/-g flags); these entries are fallbacks.
  (:aliases ("set-environment" "setenv") 2 (var-name var-value)
    (%config-setenv var-name var-value)
    t))

;;; ── set-option flag handling (set -g / -a / -s / ...) ──────────────────────
;;;
;;; The fixed-arity directive table cannot match `set -g status off` (3 tokens vs
;;; arity 2), so the canonical .tmux.conf form silently failed.  %apply-set-
;;; directive consumes leading scope flags:
;;;   -g global (default)  -s server  -w window  -o only-if-unset
;;;   -a append  -u unset
;;; -s routes the write to *server-options* instead of *global-options*.
;;;
;;; The set-verb list is derived from the :aliases declaration in define-config-
;;; directives rather than maintained as a separate defparameter.

(defun %set-directive-p (cmd)
  "Return T when CMD is one of the standard set-option directive verbs."
  (member cmd '("set" "set-option" "setw" "set-window-option" "sets" "set-session-option")
          :test #'string=))

(defun %strip-set-flags (args)
  "Consume leading -X flag tokens from a set directive's ARGS.
   Returns (values FLAG-PRESENT-P APPEND-P SERVER-P UNSET-P FORMAT-P POSITIONALS):
     FLAG-PRESENT-P – T when any flag was present
     APPEND-P       – T when -a appeared (append to existing value)
     SERVER-P       – T when -s appeared (route to server-options)
     UNSET-P        – T when -u appeared (remove the option)
     FORMAT-P       – T when -F appeared (expand value as format string)
   Recognised but currently treated as global: -g (global), -w (window),
   -p (pane), -o (only-if-unset — accepted, not enforced).  These scope
   flags cannot be applied to per-object instances at config-load time
   because no window or pane context exists yet; options fall through to
   the global store so they take effect at the nearest practical scope.
   POSITIONALS is the remaining non-flag tokens (name and optional value)."
  (let ((flag-present-p nil)
        (append-p       nil)
        (server-p       nil)
        (unset-p        nil)
        (format-p       nil)
        (remaining      args))
    (loop while (and remaining
                     (let ((tok (first remaining)))
                       (and (>= (length tok) 2) (char= (char tok 0) #\-))))
          do (let ((tok (pop remaining)))
               (setf flag-present-p t)
               (when (find #\a tok) (setf append-p t))
               (when (find #\s tok) (setf server-p t))
               (when (find #\u tok) (setf unset-p  t))
               ;; -F: expand the value as a format string before storing.
               (when (find #\F tok) (setf format-p t))
               ;; -g, -w, -p, -o, -q: accepted silently.
               ))
    (values flag-present-p append-p server-p unset-p format-p remaining)))

(defun %coerce-set-value (raw-value format-p)
  "Coerce RAW-VALUE for storage.  When FORMAT-P is T, expand it as a format
   string using a minimal context (hostname + version); on expansion failure
   the raw string is returned unchanged.  Pure: no side-effects."
  (if format-p
      (let ((ctx (list :hostname (machine-instance) :version "3.5")))
        (handler-case
            (cl-tmux/format:expand-format raw-value ctx)
          (error () raw-value)))
      raw-value))

(defun %route-set-value (name value server-p append-p unset-p)
  "Store VALUE under NAME in the appropriate option table, handling -u/-s/-a/-sa.
   Pure routing: all value coercion has already happened."
  (cond
    (unset-p
     (if server-p
         (remhash name cl-tmux/options:*server-options*)
         (remhash name cl-tmux/options:*global-options*)))
    ((and server-p append-p)
     (cl-tmux/options:set-server-option
      name (cl-tmux/options:append-option-value
            name (cl-tmux/options:get-server-option name nil) value)))
    (server-p
     (cl-tmux/options:set-server-option name value))
    (append-p
     (cl-tmux/options:set-option
      name (cl-tmux/options:append-option-value
            name (cl-tmux/options:get-option name nil) value)))
    (t
     (cl-tmux/options:set-option name value))))

(defun %apply-set-directive (cmd args)
  "Apply a flag-bearing set-family directive (e.g. `set -g status off`,
   `set -s escape-time 0`, `set -ag word-separators x`).
   Routes -s writes to *server-options*; handles -a (append) and -u (unset).
   Returns T when applied; NIL when CMD is not a set verb or carries no flags."
  (when (%set-directive-p cmd)
    (multiple-value-bind (flag-present-p append-p server-p unset-p format-p positionals)
        (%strip-set-flags args)
      (when (and flag-present-p (first positionals))
        (let* ((name      (first positionals))
               (raw-value (format nil "~{~A~^ ~}" (rest positionals)))
               (value     (%coerce-set-value raw-value format-p)))
          (%route-set-value name value server-p append-p unset-p)
          ;; Special: command-alias[N] alias=expansion array syntax.
          (%apply-command-alias-directive name value)
          ;; Side-effect: intercept special options that need runtime state updates.
          (%apply-option-side-effects name value)
          t)))))

;;; ── Declarative option-side-effect dispatch ──────────────────────────────────
;;;
;;; define-option-side-effect-handlers builds %apply-option-side-effects from a
;;; Prolog-style fact table: one (NAME-STRING &body BODY) arm per option.  Each arm
;;; is guarded by (string= name NAME-STRING); VALUE is bound in BODY.  This matches
;;; define-csi-rules / define-config-directives in style.

(defmacro define-option-side-effect-handlers (&rest rules)
  "Build %APPLY-OPTION-SIDE-EFFECTS from a declarative table of RULES.
   Each RULE has the form:
     (NAME-STRING &body BODY)   — NAME-STRING matched via STRING=; VALUE bound in BODY.
     (:any-of (NAME...) &body BODY) — VALUE bound in BODY when NAME is one of the list.
   Generates a COND dispatch over NAME."
  (flet ((expand-rule (rule)
           (if (eq (first rule) :any-of)
               (destructuring-bind (names &body body) (rest rule)
                 `((member name ',names :test #'string=) ,@body))
               (destructuring-bind (name-string &body body) rule
                 `((string= name ,name-string) ,@body)))))
    `(defun %apply-option-side-effects (name value)
       "Apply runtime side-effects for options that touch non-option state.
        Dispatches on NAME; VALUE holds the new option value string."
       (declare (ignorable value))
       (cond
         ,@(mapcar #'expand-rule rules)))))

(define-option-side-effect-handlers
  ;; prefix: update *prefix-key-code* and register the new key in the prefix table.
  ("prefix"
   (let ((byte (%parse-prefix-key value)))
     (when byte
       (setf *prefix-key-code* byte)
       (key-table-bind +table-prefix+ (code-char byte) :send-prefix))))
  ;; prefix2: a second prefix key that arms the prefix table.
  ("prefix2"
   (let ((byte (%parse-prefix-key value)))
     (when byte
       (setf *prefix2-key-code* byte)
       (key-table-bind +table-prefix+ (code-char byte) :send-prefix))))
  ;; default-shell: update the shell used for new panes immediately.
  ("default-shell"
   (when (and (stringp value) (plusp (length value)))
     (setf *default-shell* value)))
  ;; escape-time: sync into server-options so every set form takes effect.
  ("escape-time"
   (when (and (stringp value) (plusp (length value)))
     (cl-tmux/options:set-server-option "escape-time" value)))
  ;; status: off/false/0 hides the bar; numeric line count (capped at 5) or on/true → 1.
  ("status"
   (let* ((off-p (member value '("off" "false" "0") :test #'equal))
          (n     (parse-integer value :junk-allowed t)))
     (setf *status-height*
           (cond (off-p 0)
                 ((and n (> n 0)) (min n 5))
                 (t 1)))))
  ;; mouse: delegate to *mouse-reporting-hook* so config and renderer stay decoupled.
  ("mouse"
   (when *mouse-reporting-hook*
     (let ((on-p (member value '("on" "true" "1") :test #'equal)))
       (ignore-errors (funcall *mouse-reporting-hook* (and on-p t))))))
  ;; update-environment: propagate the space-separated variable list into the model.
  ("update-environment"
   (when (and (stringp value) (plusp (length value)))
     (setf cl-tmux/model:*update-environment*
           (remove-if (lambda (s) (zerop (length s)))
                      (uiop:split-string value :separator '(#\Space))))))
  ;; terminal-overrides / terminal-features: accepted silently — cl-tmux always
  ;; emits 24-bit SGR; the option is stored by the caller for show-options.
  (:any-of ("terminal-overrides" "terminal-features") nil))

(defun %apply-set-hook-directive (cmd args)
  "Handle 'set-hook [-r] [-u] event [command]' directives.
   -r or -u flag removes/unsets all hooks for the event; without them, registers
   the command.  The command is stored as a raw string (not converted to keyword)
   so that format variables and arguments (e.g. 'display-message #{session_name}')
   are expanded at hook-fire time via %run-command-line.
   Returns T when handled, NIL otherwise."
  (when (or (string= cmd "set-hook") (string= cmd "hook"))
    ;; Consume ALL leading -X flags (not just -r/-u): -g/-a/-R are accepted and
    ;; skipped so `set-hook -g <event> <cmd>` registers EVENT, not "-g".
    (let* ((remove-p nil)
           (rest     (loop for tail on args
                           while (let ((tok (first tail)))
                                   (and (> (length tok) 1) (char= (char tok 0) #\-)))
                           do (when (or (string= (first tail) "-r")
                                        (string= (first tail) "-u"))
                                (setf remove-p t))
                           finally (return tail)))
           (event    (first rest))
           ;; The command may be a single quoted token or split across tokens;
           ;; join all remaining tokens as a single command line string.
           (cmd-str  (when (rest rest)
                       (format nil "~{~A~^ ~}" (rest rest)))))
      (when event
        (if remove-p
            (progn (cl-tmux/hooks:clear-command-hooks event) t)
            (when cmd-str
              ;; Store the raw command string for execution at hook-fire time.
              (cl-tmux/hooks:set-command-hook event cmd-str)
              t))))))

;;; ── set-environment flag handling (set-environment -r VAR) ──────────────────
;;;
;;; The fixed-arity table handles only `set-environment VAR VALUE` (2 args).
;;; The `-r` form (unset) passes 2 args: "-r" and VAR, which the fixed-arity
;;; table rejects because arg[0] ≠ a variable name.  This handler intercepts
;;; the unset form before the fixed-arity table gets a chance to reject it.

(defun %apply-set-environment-directive (cmd args)
  "Handle 'set-environment [-g] [-u|-r] VAR [VALUE]' config directives.
   -u unsets the variable (tmux's unset flag); -r is accepted as a synonym for
   unset (cl-tmux has no separate update-environment list to remove from).
   -g is accepted and ignored (global scope is the only scope supported).
   Returns T when handled, NIL otherwise."
  (when (member cmd '("set-environment" "setenv") :test #'string=)
    (let* (;; Consume optional flags: -g (global, default), -u/-r (unset).
           (remove-p   nil)
           (remaining  args))
      (loop while (and remaining
                       (let ((tok (first remaining)))
                         (and (>= (length tok) 2) (char= (char tok 0) #\-))))
            do (let ((tok (pop remaining)))
                 (when (or (find #\u tok) (find #\r tok)) (setf remove-p t))))
      (let ((var-name  (first remaining))
            (var-value (second remaining)))
        (when var-name
          (if remove-p
              ;; Unset: lazy lookup so SB-POSIX need not be loaded before cl-tmux.
              (let ((fn (%config-posix-fn "UNSETENV")))
                (when fn (ignore-errors (funcall fn var-name))))
              ;; Set: value required for non-remove form.
              (when var-value
                (%config-setenv var-name var-value)))
          t)))))

;;; ── if-shell config-time conditional ────────────────────────────────────────
;;;
;;; tmux's `if-shell` can appear as a standalone directive in .tmux.conf:
;;;   if-shell 'uname | grep -q Darwin' 'set -g prefix C-a' 'set -g prefix C-b'
;;; It runs the condition via /bin/sh, then applies THEN-CMD or ELSE-CMD.
;;; This is different from the run-time :if-shell dispatch (which is interactive).

(defun %if-shell-format-true-p (condition)
  "tmux -F truthiness for if-shell: expand CONDITION as a format and treat an
   empty result or \"0\" as false, anything else as true.  A NIL context is used
   (config-time has no pane); global-scoped formats still resolve."
  (let ((result (ignore-errors (cl-tmux/format:expand-format condition nil))))
    (and result
         (not (string= result ""))
         (not (string= result "0")))))

(defun %take-brace-or-command (tokens)
  "Consume one command UNIT from the front of TOKENS (an if-shell then/else body).
   A unit is either a brace block { ... } — its inner tokens are split into command
   token-lists via %split-on-semicolons (depth-tracked for nesting; a missing close
   brace is tolerated, end-of-list closes) — or a single bare token, a complete
   quoted command string re-tokenised into one command.  Returns
   (values COMMAND-TOKEN-LISTS REST), each list ready for apply-config-directive."
  (cond
    ((null tokens) (values nil nil))
    ((string= (first tokens) "{")
     (let ((depth 1) (inner '()) (rest (rest tokens)))
       (loop for tok = (pop rest)
             while tok do
               (cond ((string= tok "{") (incf depth) (push tok inner))
                     ((string= tok "}") (decf depth)
                      (if (zerop depth) (return) (push tok inner)))
                     (t (push tok inner))))
       (values (%split-on-semicolons (nreverse inner)) rest)))
    (t
     (values (list (%config-tokens (first tokens))) (rest tokens)))))

(defun %apply-if-shell-directive (cmd args)
  "Handle 'if-shell [-bF] [-t target] CONDITION THEN-CMD [ELSE-CMD]' directives.
   Without -F, CONDITION is a shell command (exit 0 = true).  With -F, CONDITION
   is a format string (true unless it expands to empty or \"0\").  -b (background)
   and -t target are accepted and ignored at config time.  Returns T when CMD is
   if-shell/if (handled), NIL otherwise."
  (when (member cmd '("if-shell" "if") :test #'string=)
    (let ((format-mode nil)
          (remaining   args))
      ;; Consume leading flag tokens (clusters like -bF are allowed; -t takes the
      ;; next token).  Stop at the first non-flag token — the CONDITION.
      (loop while (and remaining
                       (let ((tok (first remaining)))
                         (and (> (length tok) 1) (char= (char tok 0) #\-))))
            do (let ((tok (pop remaining)))
                 (cond
                   ((string= tok "-t") (when remaining (pop remaining)))
                   (t (when (find #\F tok) (setf format-mode t))))))
      (when (>= (length remaining) 2)
        (let* ((condition (first remaining))
               (truthy-p  (if format-mode
                              (%if-shell-format-true-p condition)
                              ;; Run the condition shell command; treat any error
                              ;; (including a timeout signal from UIOP) as non-zero
                              ;; (falsy).  :timeout 30 guards against a hanging command
                              ;; blocking config loading indefinitely.
                              (handler-case
                                  (eql 0 (nth-value 2
                                           (uiop:run-program
                                            (list "/bin/sh" "-c" condition)
                                            :ignore-error-status t :timeout 30)))
                                (error () nil)))))
          ;; THEN/ELSE bodies are each either a brace block { ... } (tmux 3.x) or a
          ;; single quoted command token.  %take-brace-or-command consumes one unit
          ;; and returns its command(s) as ready-to-apply token-lists.
          (multiple-value-bind (then-cmds after-then)
              (%take-brace-or-command (rest remaining))
            (let ((else-cmds (and after-then
                                  (nth-value 0 (%take-brace-or-command after-then)))))
              (dolist (line (if truthy-p then-cmds else-cmds))
                (apply-config-directive line))))))
      t)))

;;; ── command-alias array syntax handling ─────────────────────────────────────
;;;
;;; tmux stores command aliases as an array option in .tmux.conf:
;;;   set -s command-alias[0] e='new-window -n'
;;; The option name carries the index (`command-alias[0]`).  After %strip-set-
;;; flags the positionals look like: ("command-alias[0]" "e=new-window -n").
;;; This function detects that pattern and routes it to the alias registry.

(defun %apply-command-alias-directive (name value)
  "If NAME looks like 'command-alias[N]', parse VALUE as 'alias=expansion'
   and register the alias.  Returns T when handled, NIL otherwise."
  (when (and (>= (length name) 13)
             (string= (subseq name 0 13) "command-alias"))
    (let ((eq-pos (position #\= value)))
      (when eq-pos
        (cl-tmux/options:register-command-alias
         (subseq value 0 eq-pos)
         (subseq value (1+ eq-pos)))
        t))))

;;; ── run-shell / run flag handling (run-shell -b/-t/-d/-C 'cmd') ──────────────
;;;
;;; The fixed-arity table only matches the bare 1-arg form `run-shell 'cmd'`, so
;;; the common real-world `run-shell -b 'cmd'` / `run -b '~/.tmux/...'` forms
;;; (with leading flags) silently failed.  This handler strips leading flags
;;; before the fixed-arity table and runs whatever shell command remains.

(defun %apply-run-shell-directive (cmd args)
  "Handle 'run-shell [-b] [-C] [-t target] [-d delay] shell-command' directives
   (alias 'run').  Consumes leading flags:
     -b           run in background (boolean; we run synchronously regardless)
     -C           run a tmux command instead of a shell command (boolean)
     -t <target>  target pane (takes the next token as its value)
     -d <delay>   delay (takes the next token as its value)
   Unknown leading -X flags: a single bare flag token is skipped to stay
   tolerant.  Stops at the first non-flag token; that token plus any remaining
   tokens (joined by spaces) form the shell command.
   Returns T when CMD is run-shell/run (handled), NIL otherwise."
  (when (member cmd '("run-shell" "run") :test #'string=)
    (let ((tmux-command-p nil)
          (remaining      args))
      ;; Consume leading flag tokens.
      (loop while (and remaining
                       (let ((tok (first remaining)))
                         (and (>= (length tok) 1) (char= (char tok 0) #\-))))
            do (let ((tok (pop remaining)))
                 (cond
                   ((string= tok "-C") (setf tmux-command-p t))
                   ((string= tok "-b")) ; background flag, no argument
                   ((or (string= tok "-t") (string= tok "-d"))
                    ;; These flags take the next token as their value.
                    (when remaining (pop remaining)))
                   ;; Unknown bare -X flag: skip the single flag token only.
                   (t nil))))
      ;; Remaining tokens (joined) form the shell command.
      (let ((command (when remaining
                       (format nil "~{~A~^ ~}" remaining))))
        (cond
          ;; No command after flags: a flag-only invocation is a no-op but handled.
          ((null command) t)
          ;; -C: the argument is a tmux command, not a shell command — run it
          ;; through the config dispatcher (same path if-shell uses for its
          ;; then/else commands).  e.g. `run-shell -C 'display-message hi'`.
          (tmux-command-p
           (ignore-errors (apply-config-directive (%config-tokens command)))
           t)
          ;; Shell command: run it the same way the fixed-arity entries do.
          (t
           (let ((expanded (%expand-leading-tilde command)))
             ;; :timeout 30 guards against a hanging run-shell blocking config loading.
             ;; handler-case makes a timeout signal (UIOP:SUBPROCESS-ERROR or similar)
             ;; explicit: the command is abandoned and loading continues rather than
             ;; silently treating the timeout as a non-zero exit.
             (handler-case
                 (uiop:run-program (list "/bin/sh" "-c" expanded)
                                   :ignore-error-status t :timeout 30)
               (error () nil)))
           t))))))

(defun %glob-expand (path)
  "Expand a shell glob PATH (one containing * ? or [) to the sorted namestrings of
   the matching regular files.  A path with no glob metacharacters is returned
   unchanged as a one-element list, so a plain (possibly missing) path still
   reaches load-config-file."
  (if (find-if (lambda (c) (member c '(#\* #\? #\[) :test #'char=)) path)
      (sort (loop for p in (ignore-errors (directory (pathname path)))
                  unless (ignore-errors (uiop:directory-pathname-p p))
                    collect (namestring p))
            #'string<)
      (list path)))

(defun %parse-source-file-flags (args)
  "Parse the leading -Fnqv flags of source-file.  Returns
   (values PARSE-ONLY-P QUIET-P VERBOSE-P FORMAT-P POSITIONALS).  Clustered flags
   (e.g. -qn) are supported; scanning stops at the first non-flag token (a path)."
  (let ((parse-only nil) (quiet nil) (verbose nil) (format-p nil) (rest args))
    (loop while (and rest
                     (let ((tok (first rest)))
                       (and (> (length tok) 1) (char= (char tok 0) #\-))))
          do (let ((tok (pop rest)))
               (when (find #\n tok) (setf parse-only t))
               (when (find #\q tok) (setf quiet t))
               (when (find #\v tok) (setf verbose t))
               (when (find #\F tok) (setf format-p t))))
    (values parse-only quiet verbose format-p rest)))

(defun %parse-config-file-only (file)
  "Parse-only loader for `source-file -n`: read FILE and tokenise each logical line
   (honouring # comments) WITHOUT applying any directive, so NO command executes —
   tmux's CMD_PARSE_PARSEONLY syntax check.  Errors are swallowed (lenient, like
   load-config-file)."
  (ignore-errors
    (with-open-file (in file :if-does-not-exist nil)
      (when in
        (loop for line = (read-line in nil nil)
              while line
              do (%config-tokens (%strip-config-comment line)))))))

(defun source-files (args)
  "Implement `source-file [-Fnqv] path...`: for each non-flag PATH, optionally
   expand it as a format string (-F), then expand a leading ~ and shell globs
   (* ? []), and load every matching config file.  With -n (parse-only), each file
   is read and tokenised but NO command runs (tmux's CMD_PARSE_PARSEONLY syntax
   check).  Errors (missing file, parse failure) are always swallowed, so cl-tmux is
   effectively -q whether or not -q is given; -v is accepted (no client sink to echo
   to).  Returns T."
  (multiple-value-bind (parse-only quiet verbose format-p positionals)
      (%parse-source-file-flags args)
    (declare (ignore quiet verbose))
    (dolist (raw positionals)
      (let ((path (if format-p
                      (or (ignore-errors (cl-tmux/format:expand-format raw nil)) raw)
                      raw)))
        (when (plusp (length path))
          (dolist (file (%glob-expand (%expand-leading-tilde path)))
            (if parse-only
                (%parse-config-file-only file)
                (ignore-errors (load-config-file file))))))))
  t)

(defun %apply-source-file-directive (cmd args)
  "Intercept source-file / source: -q/-n/-v flags, glob patterns, and multiple
   paths (the fixed-arity directive table only handled a single bare path).
   Returns T when CMD is a source verb, else NIL."
  (when (member cmd '("source-file" "source") :test #'string=)
    (source-files args)))

(defun apply-config-directive (tokens)
  "Apply one parsed config directive (list of string TOKENS) to live state.
   Returns T when applied, NIL for an unknown/invalid directive.
   Handles bind/unbind, set-hook, set[-g|-a|-s|-u|...], set-environment [-r],
   if-shell, run-shell/run [-b|-C|-t|-d], and the fixed-arity directive table."
  (when tokens
    (let ((cmd  (first tokens))
          (args (rest tokens)))
      (or (%apply-key-directive cmd args)
          (%apply-if-shell-directive cmd args)
          (%apply-set-environment-directive cmd args)
          (%apply-set-directive cmd args)
          (%apply-set-hook-directive cmd args)
          (%apply-run-shell-directive cmd args)
          (%apply-source-file-directive cmd args)
          (%apply-config-directive-inner tokens)))))

(defun %strip-config-comment (line)
  "Remove a trailing # comment from a config LINE.  Following tmux's lexer, a #
   begins a comment only when it is OUTSIDE single/double quotes and is NOT part of
   a format construct (#{ #( #[) nor an escaped ## .  Returns the line up to the
   comment, right-trimmed (or the whole line when there is no comment)."
  (let ((len (length line)) (i 0) (in-single nil) (in-double nil))
    (loop while (< i len) do
      (let ((c (char line i)))
        (cond
          (in-single (when (char= c #\') (setf in-single nil)))
          (in-double (cond ((char= c #\\) (incf i))   ; skip escaped char
                           ((char= c #\") (setf in-double nil))))
          ((char= c #\') (setf in-single t))
          ((char= c #\") (setf in-double t))
          ((char= c #\#)
           (cond
             ;; ## — escaped literal #, not a comment.
             ((and (< (1+ i) len) (char= (char line (1+ i)) #\#)) (incf i))
             ;; #{ / #( / #[ — a format construct, not a comment.
             ((and (< (1+ i) len) (member (char line (1+ i)) '(#\{ #\( #\[))) nil)
             ;; A # begins a comment only at the START of a token (line start or
             ;; right after whitespace).  A # in the MIDDLE of an unquoted word —
             ;; e.g. a hex colour bg=#0000ff or @var=#abc — is a literal character,
             ;; matching tmux's lexer, which only enters comment scanning at a token
             ;; boundary (cmd-parse.y yylex).  Without this guard such values were
             ;; silently truncated to "bg=" unless the user quoted them.
             ((and (> i 0)
                   (let ((p (char line (1- i))))
                     (not (or (char= p #\Space) (char= p #\Tab)))))
              nil)
             ;; Otherwise a comment begins here: drop the rest of the line.
             (t (return-from %strip-config-comment
                  (string-right-trim '(#\Space #\Tab) (subseq line 0 i))))))))
      (incf i))
    line))

(defun apply-config-line (line)
  "Apply a single config LINE.  Blank lines and # comments (full-line and inline,
   respecting quotes and #{...} formats) are ignored.  Returns T when applied."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Newline)
                              (%strip-config-comment line))))
    (and (plusp (length trimmed))
         (apply-config-directive (%config-tokens trimmed)))))

;;; ── %if / %else / %endif preprocessor support ───────────────────────────────
;;;
;;; tmux config files may contain conditional blocks:
;;;   %if <condition>
;;;   ...
;;;   %else
;;;   ...
;;;   %endif
;;;
;;; The condition is a tmux format string that evaluates to "1" (truthy) or
;;; "" / "0" (falsy).  A dynamic callback (*config-condition-evaluator*) is used
;;; so the config layer (which cannot depend on cl-tmux/format) can delegate
;;; evaluation to the top-level package which has access to full format expansion.
;;; When the callback is unset, all %if conditions are treated as truthy so that
;;; no directives are silently skipped.

(defvar *config-condition-evaluator* nil
  "When non-NIL, a function (string) → string that evaluates a %if condition.
   The string result is truthy when non-empty and not equal to \"0\".
   NIL means all %if conditions are treated as truthy (nothing skipped).")

(defun %eval-config-condition (cond-str)
  "Evaluate a %if condition string via *config-condition-evaluator*.
   Returns T when the condition is truthy, NIL otherwise.
   Defaults to T when *config-condition-evaluator* is NIL."
  (if *config-condition-evaluator*
      (let ((result (handler-case (funcall *config-condition-evaluator* cond-str)
                      (error () "1"))))
        (and result (plusp (length result)) (not (string= result "0"))))
      t))

(defun %preprocessor-line-p (trimmed)
  "Return :if, :else, :elif, :endif, or NIL indicating whether TRIMMED is a
   preprocessor directive line."
  (cond
    ((and (>= (length trimmed) 3) (string= (subseq trimmed 0 3) "%if")
          (or (= (length trimmed) 3) (not (alpha-char-p (char trimmed 3)))))
     :if)
    ((string= trimmed "%else")
     :else)
    ((and (>= (length trimmed) 5) (string= (subseq trimmed 0 5) "%elif")
          (or (= (length trimmed) 5) (not (alpha-char-p (char trimmed 5)))))
     :elif)
    ((string= trimmed "%endif")
     :endif)
    (t nil)))

(defun %line-brace-delta (line)
  "Net unquoted brace depth of LINE: count of '{' minus '}', ignoring braces
   inside single/double quotes or immediately after a backslash.  Used by
   load-config-from-stream to detect and join multi-line { ... } command blocks
   (tmux 3.x brace syntax)."
  (let ((delta 0) (i 0) (len (length line)))
    (loop while (< i len) do
      (let ((c (char line i)))
        (cond
          ((char= c #\\) (incf i 2))                    ; skip escaped char
          ((char= c #\")                                ; skip double-quoted span
           (incf i)
           (loop while (and (< i len) (char/= (char line i) #\"))
                 do (if (char= (char line i) #\\) (incf i 2) (incf i)))
           (incf i))
          ((char= c #\')                                ; skip single-quoted span
           (incf i)
           (loop while (and (< i len) (char/= (char line i) #\'))
                 do (incf i))
           (incf i))
          ((char= c #\{) (incf delta) (incf i))
          ((char= c #\}) (decf delta) (incf i))
          (t (incf i)))))
    delta))

(defun %read-brace-block (first-line stream)
  "FIRST-LINE has opened an unbalanced { ... } block; keep reading from STREAM
   until the brace depth returns to zero (or EOF), then return all the lines
   joined into one logical line with \" ; \" separators so the inner commands
   become a semicolon sequence the bind parser already understands.
   Each line's inline # comment is stripped FIRST — otherwise a comment on an
   inner line would survive into the joined block and truncate it at that #, and
   a brace inside a comment would corrupt the depth count."
  (let* ((stripped-first (%strip-config-comment first-line))
         (depth (%line-brace-delta stripped-first))
         (parts (list stripped-first)))
    (loop while (> depth 0)
          for next = (read-line stream nil nil)
          while next
          for stripped = (%strip-config-comment next)
          do (push stripped parts)
             (incf depth (%line-brace-delta stripped)))
    (format nil "~{~A~^ ; ~}" (nreverse parts))))

(defun %line-continues-p (line)
  "T when LINE ends with an ODD number of backslashes — a continuation backslash
   that escapes the newline (an even count is escaped backslashes, not a
   continuation)."
  (let ((n 0) (i (1- (length line))))
    (loop while (and (>= i 0) (char= (char line i) #\\))
          do (incf n) (decf i))
    (oddp n)))

(defun %read-logical-config-line (first-line stream)
  "Join trailing-backslash continuation lines into one logical line: while a line
   ends in a continuation backslash, drop that backslash and append the next line.
   Mirrors tmux: `cmd arg1 \\<newline>arg2` is one command.  Returns the joined line."
  (let ((line first-line))
    (loop while (%line-continues-p line)
          for next = (read-line stream nil nil)
          while next
          do (setf line (concatenate 'string
                                     (subseq line 0 (1- (length line)))
                                     next)))
    line))

(defun load-config-from-stream (stream)
  "Apply every directive line read from STREAM, honoring %if/%elif/%else/%endif
   blocks.  Multi-line { ... } command blocks (tmux 3.x brace syntax) are joined
   into a single logical directive before being applied.  Returns the count applied."
  ;; COND-STACK: one state per open %if level — :ACTIVE (this branch is taken),
  ;; :SEEKING (no branch matched yet; keep evaluating %elif/%else), :TAKEN (a branch
  ;; already matched; skip the rest), or :DEAD (an ancestor was skipping when this
  ;; %if began).  A line is applied only when EVERY level is :ACTIVE.  The four
  ;; states are what a plain skip flag cannot express: distinguishing "still seeking
  ;; a match" from "a branch already matched" is required for correct %elif chains.
  (let ((cond-stack nil)
        (count 0))
    (flet ((active-p () (every (lambda (s) (eq s :active)) cond-stack)))
      (loop for raw = (read-line stream nil nil)
            while raw
            ;; Join trailing-backslash continuation lines, then strip any inline #
            ;; comment, before classifying — so a continued/commented directive (or
            ;; `%if 1 # note`) is seen as one clean logical line.
            for line = (%strip-config-comment
                        (%read-logical-config-line raw stream)) do
        (let* ((trimmed (string-trim '(#\Space #\Tab #\Return #\Newline) line))
               (pp-type (%preprocessor-line-p trimmed)))
          (case pp-type
            (:if
             ;; Only evaluate the condition in an active context; a dead block
             ;; never evaluates (matching tmux's short-circuit).
             (let ((cond-str (string-trim " \t" (subseq trimmed 3))))
               (push (cond ((not (active-p)) :dead)
                           ((%eval-config-condition cond-str) :active)
                           (t :seeking))
                     cond-stack)))
            (:elif
             (when cond-stack
               (let ((cond-str (string-trim " \t" (subseq trimmed 5))))
                 (setf (first cond-stack)
                       (case (first cond-stack)
                         (:seeking (if (%eval-config-condition cond-str) :active :seeking))
                         (:active  :taken)   ; prior branch matched → skip the rest
                         (t        (first cond-stack)))))))   ; :taken / :dead unchanged
            (:else
             (when cond-stack
               (setf (first cond-stack)
                     (case (first cond-stack)
                       (:seeking :active)    ; no branch matched → take the else
                       (:active  :taken)
                       (t        (first cond-stack))))))
            (:endif
             (when cond-stack (pop cond-stack)))
            (otherwise
             ;; Normal line: apply only when every %if level is active.
             (when (active-p)
               ;; Join a multi-line { ... } command block into one logical line.
               (let ((full-line (if (> (%line-brace-delta line) 0)
                                    (%read-brace-block line stream)
                                    line)))
                 (when (apply-config-line full-line)
                   (incf count)))))))))
    count))

(defun load-config-from-string (text)
  "Apply every directive line in TEXT, honoring %if/%else/%endif blocks.
   Returns the count of directives applied."
  (with-input-from-string (in text)
    (load-config-from-stream in)))

(defun %env-set-p (env-string)
  "True when environment variable string ENV-STRING is set and non-empty."
  (and env-string (plusp (length env-string))))

(defun %config-path-from (override xdg home)
  "Resolve the config-file path from environment values (OVERRIDE = $CL_TMUX_CONF,
   XDG = $XDG_CONFIG_HOME, each a string or NIL) and HOME (a directory pathname).

   Precedence (XDG Base Directory spec):
     1. $CL_TMUX_CONF                              — explicit override
     2. $XDG_CONFIG_HOME/cl-tmux/cl-tmux.conf
     3. ~/.config/cl-tmux/cl-tmux.conf             — XDG default when unset
   Empty strings are treated as unset.  Pure: no I/O, no environment access."
  (if (%env-set-p override)
      (pathname override)
      (let ((base (if (%env-set-p xdg)
                      xdg
                      (namestring (merge-pathnames ".config/" home)))))
        (pathname (format nil "~A/cl-tmux/cl-tmux.conf"
                          (string-right-trim "/" base))))))

(defun %tmux-conf-paths (home)
  "Return a list of candidate .tmux.conf paths in priority order:
     1. $XDG_CONFIG_HOME/tmux/tmux.conf
     2. ~/.config/tmux/tmux.conf  (XDG default)
     3. ~/.tmux.conf              (traditional location)"
  (let* ((xdg  (sb-ext:posix-getenv "XDG_CONFIG_HOME"))
         (base (if (%env-set-p xdg)
                   xdg
                   (namestring (merge-pathnames ".config/" home)))))
    (list (pathname (format nil "~A/tmux/tmux.conf"
                            (string-right-trim "/" base)))
          (merge-pathnames ".tmux.conf" home))))

(defun config-file-path ()
  "Path to the user config file, honoring $CL_TMUX_CONF then the XDG Base
   Directory spec ($XDG_CONFIG_HOME, default ~/.config).  See %config-path-from."
  (%config-path-from (sb-ext:posix-getenv "CL_TMUX_CONF")
                     (sb-ext:posix-getenv "XDG_CONFIG_HOME")
                     (user-homedir-pathname)))

(defun load-config-file (&optional (path (config-file-path)))
  "Load and apply the config file at PATH if it exists (returns the count of
   directives applied), or NIL when no file is found.
   PATH defaults to the XDG/cl-tmux path; pass NIL to auto-detect, which also
   searches the standard .tmux.conf locations for compatibility."
  (if path
      (with-open-file (in path :direction :input :if-does-not-exist nil)
        (when in (load-config-from-stream in)))
      ;; Auto-detect: try each candidate path in priority order.
      (let ((home (user-homedir-pathname)))
        (dolist (candidate (cons (config-file-path)
                                 (%tmux-conf-paths home)))
          (with-open-file (in candidate :direction :input :if-does-not-exist nil)
            (when in
              (return (load-config-from-stream in))))))))
