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

