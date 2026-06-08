(in-package #:cl-tmux/config)

;;; ── Config file parsing + directive processing ───────────────────────────
;;;
;;; This file depends on the key-binding mutators defined in config.lisp
;;; (set-key-binding, remove-key-binding) and the mutable specials
;;; (*key-tables*, *default-shell*, *status-height*).

;;; ── Tokenizer phase helpers ──────────────────────────────────────────────

(defun %whitespace-p (ch)
  "True when CH is a configuration whitespace character (space or tab)."
  (or (char= ch #\Space) (char= ch #\Tab)))

;;; ── Tokenizer phase helpers ──────────────────────────────────────────────
;;;
;;; Each helper handles one tokenizer state; all share the PUSH-CHAR closure
;;; and return the updated character index.

(defun %tokenize-backslash-escape (line i len push-char)
  "Consume a backslash-escaped character starting at I.  Calls PUSH-CHAR on
   the escaped character.  Returns the new index past both characters."
  (let ((next (1+ i)))
    (if (< next len)
        (progn (funcall push-char (char line next))
               (+ next 1))
        (+ i 1))))

(defun %tokenize-double-quoted (line i len push-char)
  "Consume a double-quoted region beginning at I (the opening-quote position).
   Handles backslash escapes inside.  If no closing quote exists, treats the
   opening quote as a literal character.  Returns the new index."
  (let ((close-pos (position #\" line :start (1+ i))))
    (if (not close-pos)
        ;; No closing quote — treat the opening \" as a literal.
        (progn (funcall push-char (char line i))
               (1+ i))
        ;; Found a closing quote — process quoted content.
        (let ((j (1+ i)))            ; skip opening \"
          (loop while (and (< j len) (char/= (char line j) #\"))
                do (let ((quoted-char (char line j)))
                     (cond
                       ((and (char= quoted-char #\\) (< (1+ j) len))
                        (incf j)
                        (funcall push-char (char line j)))
                       (t
                        (funcall push-char quoted-char))))
                   (incf j))
          (when (< j len) (incf j))  ; skip closing \"
          j))))

(defun %tokenize-single-quoted (line i len push-char)
  "Consume a single-quoted region beginning at I.  No escapes inside.
   Returns the new index past the closing quote (or EOL if unmatched)."
  (let ((j (1+ i)))                  ; skip opening '
    (loop while (and (< j len) (char/= (char line j) #\'))
          do (funcall push-char (char line j))
             (incf j))
    (when (< j len) (incf j))        ; skip closing '
    j))

(defun %config-tokens (line)
  "Tokenize LINE into a list of strings, handling:
   - unquoted whitespace as delimiter
   - \"double quoted\" strings (spaces preserved, \\x escapes processed)
   - 'single quoted' strings (literal content, no escapes)
   - \\ (backslash) escaping of the next character outside quotes
   Returns a list of token strings."
  (let* ((tokens   '())
         (current  (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
         (in-token nil)
         (len      (length line)))
    (flet ((push-char (ch)
             (vector-push-extend ch current)
             (setf in-token t))
           (finish-token ()
             (when in-token
               (push (copy-seq current) tokens)
               (setf (fill-pointer current) 0)
               (setf in-token nil))))
      (let ((i 0))
        (loop while (< i len) do
          (let ((ch (char line i)))
            (cond
              ((char= ch #\\)
               (setf i (%tokenize-backslash-escape line i len #'push-char)))
              ((char= ch #\")
               (setf in-token t)
               (setf i (%tokenize-double-quoted line i len #'push-char)))
              ((char= ch #\')
               (setf in-token t)
               (setf i (%tokenize-single-quoted line i len #'push-char)))
              ((%whitespace-p ch)
               (finish-token)
               (incf i))
              (t
               (push-char ch)
               (incf i)))))
        (finish-token)))
    (nreverse tokens)))

(defun %parse-control-char (rest)
  "Map REST (the part after a \"C-\" prefix) to its control CHARACTER, or NIL
   when REST does not denote a single control-able key.
   C-a..C-z → ^A..^Z (1..26); C-Space / C-@ → NUL (0);
   C-[ C-\\ C-] C-^ C-_ → 27..31.  The control byte is (logand code #x1f)."
  (cond
    ((string-equal rest "Space") (code-char 0))
    ((= (length rest) 1)
     (let ((c (char-upcase (char rest 0))))
       (cond
         ((char= c #\@) (code-char 0))
         ((char<= #\A c #\Z) (code-char (logand (char-code c) #x1f)))
         ((member c '(#\[ #\\ #\] #\^ #\_) :test #'char=)
          (code-char (logand (char-code c) #x1f)))
         (t nil))))
    (t nil)))

(defun %parse-key-token (token)
  "Parse a bind-key key TOKEN into the key-table key.
   A single-character TOKEN denotes that character.  A \"C-<key>\" token denotes
   the corresponding control CHARACTER (C-a→^A, C-Space→NUL, ...) so that Ctrl
   bindings match the byte the event loop sees when the key is pressed (the loop
   looks keys up via (code-char byte)).  Any other multi-character token (named
   keys like F1, Up, Home, or modifier combos like M-x / C-Left that the event
   loop encodes as multi-byte sequences) is kept as the string itself, matching
   the key-table key format used by the lookup path."
  (cond
    ((= (length token) 1) (char token 0))
    ((and (> (length token) 2)
          (char-equal (char token 0) #\C)
          (char= (char token 1) #\-))
     ;; "C-<key>": convert to the control char when single-key; otherwise (e.g.
     ;; "C-Left") fall back to the string for the deferred modifier-key path.
     (or (%parse-control-char (subseq token 2)) token))
    (t token)))

(defparameter *bindable-commands*
  '(;; Window lifecycle
    :new-window :next-window :prev-window :last-window :find-window
    :rename-window :choose-window :list-windows :move-window-prompt :swap-window
    :rotate-window :rotate-window-reverse :next-layout
    :select-layout-even-h :select-layout-even-v :select-layout-tiled
    :select-layout-main-h :select-layout-main-v :select-layout-spread
    ;; Pane lifecycle
    :next-pane :prev-pane :last-pane :display-panes
    :split-horizontal :split-vertical
    :split-horizontal-no-focus :split-vertical-no-focus
    :kill-pane :kill-pane-confirm :kill-window :kill-window-confirm
    :respawn-pane :break-pane :join-pane
    :swap-pane-forward :swap-pane-backward
    :resize-left :resize-right :resize-up :resize-down
    :zoom-toggle :mark-pane :clear-mark
    :synchronize-panes :pipe-pane :display-info
    ;; Session lifecycle
    :new-session :kill-session :rename-session :detach
    :list-sessions :list-sessions-full :choose-session
    :switch-client-next :switch-client-prev :last-session
    :has-session :lock-session :unlock-session
    ;; Key bindings / config
    :list-keys :source-file :bind-key :unbind-key
    ;; Selection / navigation
    :select-window ; the pressed digit chooses the window
    :select-window-prompt :select-pane-left :select-pane-right
    :select-pane-up :select-pane-down
    ;; Copy / paste / buffers
    :paste-buffer :copy-mode-enter :send-prefix
    :list-buffers :show-buffer :choose-buffer :delete-buffer
    :save-buffer :load-buffer
    ;; Display / info
    :show-options :show-option
    :show-window-options :show-session-options :show-server-options
    :show-messages :show-hooks
    :display-message :display-popup
    :capture-pane :clear-history :clock-mode
    ;; Scripting / hooks
    :run-shell :if-shell :command-prompt :wait-for
    ;; Client management
    :choose-client :choose-tree :refresh-client :suspend-client
    ;; Server management
    :server-info :list-clients :lock-server :detach-all-clients
    :kill-server :start-server :lock-client
    ;; Window management (additional)
    :resize-window :respawn-window :attach-session :move-pane
    :previous-layout :link-window :unlink-window
    ;; Pane management (additional)
    :list-panes :set-buffer :select-pane-mark :detach-client
    ;; Info / listing
    :list-commands
    ;; Environment
    :show-environment :set-environment
    ;; Prompt history
    :show-prompt-history :clear-prompt-history
    ;; Set-option (interactive)
    :set-window-option :set-session-option)
  "Command keywords a config-file bind directive may target.
   Type: list of keyword symbols.
   This is the user-bindable subset of commands cl-tmux:dispatch-command handles.
   It deliberately EXCLUDES copy-mode-internal commands (:copy-mode-exit,
   :copy-mode-begin-selection, :copy-mode-yank), which are produced by copy-mode
   interception rather than by key lookup.
   Updated whenever a new dispatchable command is added to dispatch-handlers.")

(defun %command-keyword (name)
  "Return the bindable command keyword named by NAME (case-insensitive), or NIL
   if NAME is not a recognized command.  Uses FIND-SYMBOL so unknown command
   names are never interned into the keyword package."
  (let ((keyword (find-symbol (string-upcase name) :keyword)))
    (and keyword (member keyword *bindable-commands*) keyword)))

;;; ── Environment-variable helper ─────────────────────────────────────────────
;;;
;;; set-environment / setenv directives need to mutate the process environment.
;;; We use sb-posix:setenv resolved at runtime (not compile time) so that the
;;; config package does not need a compile-time dependency on sb-posix.

(defun %config-setenv (name value)
  "Set environment variable NAME to VALUE for child processes.
   Resolves sb-posix:setenv at runtime so the call is safe even when sb-posix
   has not been loaded at compile time.  A no-op when sb-posix is absent."
  (let ((pkg (find-package "SB-POSIX")))
    (when pkg
      (let ((fn (find-symbol "SETENV" pkg)))
        (when fn
          (ignore-errors (funcall fn name value 1)))))))

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
   Returns (values table key command repeatable) where TABLE is +TABLE-PREFIX+ by
   default, or NIL when ARGS do not form a valid binding."
  (let ((table      +table-prefix+)
        (repeatable nil)
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
        (t
         ;; Need a key plus at least one command token.
         (when (null (rest remaining)) (return nil))
         (let* ((key-token  (%parse-key-token (first remaining)))
                (cmd-tokens (rest remaining))
                ;; Split on ";" tokens to support multi-command sequences:
                ;; bind r source-file ~/.tmux.conf \; display "Reloaded!"
                (sequences  (%split-on-semicolons cmd-tokens)))
           (return
             (if (= (length sequences) 1)
                 ;; Single command: use the existing single-command path.
                 (let ((tokens (first sequences)))
                   (if (= (length tokens) 1)
                       ;; Single word: resolve to a keyword.
                       (let ((keyword (%command-keyword (first tokens))))
                         (if keyword (values table key-token keyword repeatable) nil))
                       ;; Multi-token: store as token list.
                       (values table key-token tokens repeatable)))
                 ;; Multiple commands: store as :sequence list of token lists.
                 (values table key-token (cons :sequence sequences) repeatable)))))))))

;;; ── Semicolon-sequence splitter ──────────────────────────────────────────
;;;
;;; tmux bind directives support ";" (from "\;" in the config line) as a
;;; command separator: bind r source-file ~/.tmux.conf \; display "Reloaded!"
;;; %split-on-semicolons splits a flat token list on ";" tokens,
;;; removing empty segments, yielding a list of per-command token lists.

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
   Returns (values table key) where TABLE is +TABLE-PREFIX+ by default,
   or (values nil nil) on parse failure."
  (let ((table     +table-prefix+)
        (remaining args))
    (loop
      (cond
        ((null remaining) (return (values nil nil)))
        ((string= (first remaining) "-n")
         (setf table     +table-root+)
         (setf remaining (rest remaining)))
        ((string= (first remaining) "-T")
         (setf remaining (rest remaining))
         (when (null remaining) (return (values nil nil)))
         (setf table     (first remaining))
         (setf remaining (rest remaining)))
        (t
         (unless (= (length remaining) 1) (return (values nil nil)))
         (return (values table (%parse-key-token (first remaining)))))))))

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
   (multiple-value-bind (table key command repeatable)
       (%parse-bind-key-args args)
     (when command
       ;; COMMAND is a keyword (built-in) or a token list (`bind key cmd args`).
       (key-table-bind table key command :repeatable repeatable)
       t)))
  (("unbind" "unbind-key")
   (multiple-value-bind (table key)
       (%parse-unbind-key-args args)
     (when (and table key)
       (let ((tbl (gethash table *key-tables*)))
         (when tbl (remhash key tbl)))
       t)))
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
  ("set" 2 (option-name option-value)
    (cl-tmux/options:set-option option-name option-value)
    t)
  ("set-option" 2 (option-name option-value)
    (cl-tmux/options:set-option option-name option-value)
    t)
  ("setw" 2 (option-name option-value)
    (cl-tmux/options:set-option option-name option-value)
    t)
  ("set-window-option" 2 (option-name option-value)
    (cl-tmux/options:set-option option-name option-value)
    t)
  ("sets" 2 (option-name option-value)
    (cl-tmux/options:set-option option-name option-value)
    t)
  ("set-session-option" 2 (option-name option-value)
    (cl-tmux/options:set-option option-name option-value)
    t)
  ("set-hook" 2 (event-name command-name)
    (let ((keyword (%command-keyword command-name)))
      (when keyword
        (cl-tmux/hooks:set-command-hook event-name keyword)
        t)))
  ("source-file" 1 (path)
    (ignore-errors (load-config-file (pathname path)))
    t)
  ("source" 1 (path)
    (ignore-errors (load-config-file (pathname path)))
    t)
  ("run-shell" 1 (cmd)
    ;; Expand leading ~/ to $HOME so run '~/.tmux/plugins/tpm/tpm' works.
    (let ((expanded (if (and (> (length cmd) 2)
                             (char= (char cmd 0) #\~)
                             (char= (char cmd 1) #\/))
                        (concatenate 'string
                                     (or (ignore-errors (sb-ext:posix-getenv "HOME")) "~")
                                     (subseq cmd 1))
                        cmd)))
      (ignore-errors
        (uiop:run-program (list "/bin/sh" "-c" expanded)
                          :ignore-error-status t)))
    t)
  ("run" 1 (cmd)
    ;; Expand leading ~/ to $HOME so run '~/.tmux/plugins/tpm/tpm' works.
    (let ((expanded (if (and (> (length cmd) 2)
                             (char= (char cmd 0) #\~)
                             (char= (char cmd 1) #\/))
                        (concatenate 'string
                                     (or (ignore-errors (sb-ext:posix-getenv "HOME")) "~")
                                     (subseq cmd 1))
                        cmd)))
      (ignore-errors
        (uiop:run-program (list "/bin/sh" "-c" expanded)
                          :ignore-error-status t)))
    t)
  ;; set-environment / setenv 2-arg form: VAR VALUE (no flags).
  ;; The %apply-set-environment-directive handler in apply-config-directive
  ;; intercepts this first (handling -r/-g flags); these entries are fallbacks.
  ("set-environment" 2 (var-name var-value)
    (%config-setenv var-name var-value)
    t)
  ("setenv" 2 (var-name var-value)
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

(defparameter *set-directive-names*
  '("set" "set-option" "setw" "set-window-option" "sets" "set-session-option")
  "Config directive verbs that forward to the global option store.")

(defun %strip-set-flags (args)
  "Consume leading -X flag tokens from a set directive's ARGS.
   Returns (values HAD-FLAG APPEND-P SERVER-P UNSET-P POSITIONALS):
     HAD-FLAG  – T when any flag was present
     APPEND-P  – T when -a appeared (append to existing value)
     SERVER-P  – T when -s appeared (route to server-options)
     UNSET-P   – T when -u appeared (remove the option)
   Recognised but currently treated as global: -g (global), -w (window),
   -p (pane), -o (only-if-unset — accepted, not enforced).  These scope
   flags cannot be applied to per-object instances at config-load time
   because no window or pane context exists yet; options fall through to
   the global store so they take effect at the nearest practical scope.
   POSITIONALS is the remaining non-flag tokens (name and optional value)."
  (let ((had-flag   nil)
        (append-p   nil)
        (server-p   nil)
        (unset-p    nil)
        (format-p   nil)
        (remaining  args))
    (loop while (and remaining
                     (let ((tok (first remaining)))
                       (and (>= (length tok) 2) (char= (char tok 0) #\-))))
          do (let ((tok (pop remaining)))
               (setf had-flag t)
               (when (find #\a tok) (setf append-p t))
               (when (find #\s tok) (setf server-p t))
               (when (find #\u tok) (setf unset-p  t))
               ;; -F: expand the value as a format string before storing.
               (when (find #\F tok) (setf format-p t))
               ;; -g, -w, -p, -o, -q: accepted silently.
               ))
    (values had-flag append-p server-p unset-p format-p remaining)))

(defun %apply-set-directive (cmd args)
  "Apply a flag-bearing set-family directive (e.g. `set -g status off`,
   `set -s escape-time 0`, `set -ag word-separators x`).
   Routes -s writes to *server-options*; handles -a (append) and -u (unset).
   Returns T when applied; NIL when CMD is not a set verb or carries no flags."
  (when (member cmd *set-directive-names* :test #'string=)
    (multiple-value-bind (had-flag append-p server-p unset-p format-p positionals)
        (%strip-set-flags args)
      (when (and had-flag (first positionals))
        (let* ((name      (first positionals))
               (raw-value (format nil "~{~A~^ ~}" (rest positionals)))
               ;; -F: expand value as a format string with basic hostname context.
               (value     (if format-p
                              (let ((ctx (list :hostname (machine-instance)
                                               :version "3.5")))
                                (handler-case
                                    (cl-tmux/format:expand-format raw-value ctx)
                                  (error () raw-value)))
                              raw-value)))
          (cond
            ;; -u: unset option — remove override, revert to registered default.
            (unset-p
             (if server-p
                 (remhash name cl-tmux/options:*server-options*)
                 (remhash name cl-tmux/options:*global-options*)))
            ;; -s + -a: append to server option.
            ((and server-p append-p)
             (cl-tmux/options:set-server-option
              name (concatenate 'string
                                (princ-to-string
                                 (or (cl-tmux/options:get-server-option name nil) ""))
                                value)))
            ;; -s: server option (escape-time, exit-empty, exit-unattached, etc.)
            (server-p
             (cl-tmux/options:set-server-option name value))
            ;; -a: append to global option.
            (append-p
             (cl-tmux/options:set-option
              name (concatenate 'string
                                (princ-to-string
                                 (or (cl-tmux/options:get-option name nil) ""))
                                value)))
            ;; Default: set global option.
            (t
             (cl-tmux/options:set-option name value)))
          ;; Special: command-alias[N] alias=expansion array syntax.
          (%apply-command-alias-directive name value)
          ;; Side-effect: intercept special options that need runtime state updates.
          (%apply-option-side-effects name value)
          t)))))

(defun %apply-option-side-effects (name value)
  "Apply runtime side-effects for options that touch non-option state.
   Handles: prefix, prefix2 (key routing), default-shell, status-height,
            escape-time, history-limit."
  (cond
    ((string= name "prefix")
     (let ((byte (%parse-prefix-key value)))
       (when byte
         (setf *prefix-key-code* byte)
         (key-table-bind +table-prefix+ (code-char byte) :send-prefix))))
    ;; prefix2: a second prefix key that arms the prefix table (same as primary prefix).
    ;; Stores the byte in *prefix2-key-code* so %ground-input-state transitions to
    ;; %after-prefix-input-state when this key is pressed — same as pressing C-b.
    ;; Real tmux: pressing prefix2 arms the prefix table; C-b and prefix2 are equivalent.
    ((string= name "prefix2")
     (let ((byte (%parse-prefix-key value)))
       (when byte
         (setf *prefix2-key-code* byte)
         ;; Also bind prefix2 in prefix table → send-prefix so C-b prefix2
         ;; (i.e., pressing prefix2 AFTER the prefix) sends prefix2 to the pane.
         (key-table-bind +table-prefix+ (code-char byte) :send-prefix))))
    ;; default-shell: update the shell used for new panes immediately.
    ((string= name "default-shell")
     (when (and (stringp value) (plusp (length value)))
       (setf *default-shell* value)))
    ;; status: off/on or a line count (tmux accepts 2..5 for a multi-line bar).
    ;; off/false/0 hides the bar; on/true or any positive integer shows it.
    ;; Multi-line rendering (status-format[0..N]) is not yet implemented, so a
    ;; line count >= 2 currently renders as a single-line bar — but it correctly
    ;; SHOWS the bar instead of the previous behaviour where `status 2` (any value
    ;; not literally "on"/"true"/"1") silently DISABLED the status bar.
    ((string= name "status")
     (let* ((off-p (member value '("off" "false" "0") :test #'equal))
            (n     (parse-integer value :junk-allowed t)))
       (setf *status-height*
             (cond (off-p 0)
                   ((and n (> n 0)) 1)     ; numeric line count → show (1 line for now)
                   (t 1)))))               ; on/true/other truthy → show
    ;; mouse: on/off enables/disables mouse reporting on the outer terminal.
    ;; We call the renderer's mouse-reporting functions via symbol lookup to
    ;; avoid a compile-time circular dependency.
    ((string= name "mouse")
     (let ((on-p (member value '("on" "true" "1") :test #'equal))
           (pkg   (find-package "CL-TMUX/RENDERER")))
       (when pkg
         (let ((fn (find-symbol (if on-p "ENABLE-MOUSE-REPORTING"
                                        "DISABLE-MOUSE-REPORTING")
                                pkg)))
           (when fn (ignore-errors (funcall fn)))))))
    ;; update-environment: propagate the space-separated name list into
    ;; *update-environment* so that get-update-environment-vars picks it up
    ;; via the dynamic variable rather than re-parsing the option on every call.
    ((string= name "update-environment")
     (when (and (stringp value) (plusp (length value)))
       (setf cl-tmux/model:*update-environment*
             (remove-if (lambda (s) (zerop (length s)))
                        (uiop:split-string value :separator '(#\Space))))))
    ;; status-position: top or bottom adjusts status bar position.
    ;; (stored in options; renderer reads it at render time — no extra side effect needed.)
    ;; terminal-overrides / terminal-features: parse known capability flags.
    ;; The most common use is setting Tc (true-color) or RGB for 24-bit support.
    ;; We accept these silently; the outer terminal already supports true-color,
    ;; so the actual rendering path always uses 24-bit when the color requires it.
    ;; Logging the intent preserves compatibility without behavioral impact.
    ((member name '("terminal-overrides" "terminal-features") :test #'string=)
     ;; No runtime side-effect needed: cl-tmux always emits 24-bit SGR sequences
     ;; when the color is true-color (the renderer handles this transparently).
     ;; The option is stored in *global-options* / *server-options* by the caller.
     nil)
    ))

(defun %apply-set-hook-directive (cmd args)
  "Handle 'set-hook [-r] [-u] event [command]' directives.
   -r or -u flag removes/unsets all hooks for the event; without them, registers
   the command.  The command is stored as a raw string (not converted to keyword)
   so that format variables and arguments (e.g. 'display-message #{session_name}')
   are expanded at hook-fire time via %run-command-line.
   Returns T when handled, NIL otherwise."
  (when (or (string= cmd "set-hook") (string= cmd "hook"))
    (let* ((remove-p (and (first args)
                          (or (string= (first args) "-r")
                              (string= (first args) "-u"))))
           (rest     (if remove-p (rest args) args))
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
  "Handle 'set-environment [-g] [-r] VAR [VALUE]' config directives.
   -r unsets the variable (removes it from child-process environment).
   -g is accepted and ignored (global scope is the only scope supported).
   Returns T when handled, NIL otherwise."
  (when (member cmd '("set-environment" "setenv") :test #'string=)
    (let* (;; Consume optional flags: -g (global, default), -r (remove/unset).
           (remove-p   nil)
           (remaining  args))
      (loop while (and remaining
                       (let ((tok (first remaining)))
                         (and (>= (length tok) 2) (char= (char tok 0) #\-))))
            do (let ((tok (pop remaining)))
                 (when (find #\r tok) (setf remove-p t))))
      (let ((var-name  (first remaining))
            (var-value (second remaining)))
        (when var-name
          (if remove-p
              ;; Unset: remove from process environment if sb-posix available.
              (let ((pkg (find-package "SB-POSIX")))
                (when pkg
                  (let ((fn (find-symbol "UNSETENV" pkg)))
                    (when fn (ignore-errors (funcall fn var-name))))))
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

(defun %apply-if-shell-directive (cmd args)
  "Handle 'if-shell CONDITION THEN-CMD [ELSE-CMD]' config directives.
   Runs CONDITION as a shell command; exit 0 means truthy.
   Returns T when handled, NIL otherwise."
  (when (member cmd '("if-shell" "if") :test #'string=)
    (when (>= (length args) 2)
      (let* ((condition (first args))
             (then-cmd  (second args))
             (else-cmd  (third args))
             (exit-code (nth-value 2
                          (ignore-errors
                            (uiop:run-program
                             (list "/bin/sh" "-c" condition)
                             :ignore-error-status t))))
             (truthy-p  (eql exit-code 0))
             (apply-str (if truthy-p then-cmd else-cmd)))
        (when apply-str
          (apply-config-directive (%config-tokens apply-str)))
        t))))

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

(defun apply-config-directive (tokens)
  "Apply one parsed config directive (list of string TOKENS) to live state.
   Returns T when applied, NIL for an unknown/invalid directive.
   Handles bind/unbind, set-hook, set[-g|-a|-s|-u|...], set-environment [-r],
   if-shell, and the fixed-arity directive table."
  (when tokens
    (let ((cmd  (first tokens))
          (args (rest tokens)))
      (or (%apply-key-directive cmd args)
          (%apply-if-shell-directive cmd args)
          (%apply-set-environment-directive cmd args)
          (%apply-set-directive cmd args)
          (%apply-set-hook-directive cmd args)
          (%apply-config-directive-inner tokens)))))

(defun apply-config-line (line)
  "Apply a single config LINE.  Blank lines and #-comments are ignored.
   Returns T when a directive was applied."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Newline) line)))
    (and (plusp (length trimmed))
         (char/= (char trimmed 0) #\#)
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

(defun load-config-from-stream (stream)
  "Apply every directive line read from STREAM, honoring %if/%else/%endif blocks.
   Returns the count of directives applied."
  ;; SKIP-STACK: list of booleans — T means 'skip until matching %else/%endif'.
  ;; Nested %if blocks push/pop the stack.
  (let ((skip-stack nil)
        (count 0))
    (loop for line = (read-line stream nil nil)
          while line do
      (let* ((trimmed  (string-trim '(#\Space #\Tab #\Return #\Newline) line))
             (pp-type  (%preprocessor-line-p trimmed)))
        (case pp-type
          (:if
           ;; Evaluate condition unless we are already skipping.
           (let ((cond-str (string-trim " \t" (subseq trimmed 3))))
             (push (or (and skip-stack (first skip-stack))
                       (not (%eval-config-condition cond-str)))
                   skip-stack)))
          (:elif
           ;; Flip the top skip flag if not already done; treat like else+if.
           (when skip-stack
             (let* ((cond-str (string-trim " \t" (subseq trimmed 5)))
                    (outer-skip (if (cdr skip-stack) (second skip-stack) nil))
                    (was-skip   (first skip-stack)))
               (setf (first skip-stack)
                     (or outer-skip
                         (if was-skip
                             (not (%eval-config-condition cond-str))
                             t))))))
          (:else
           (when skip-stack
             (setf (first skip-stack) (not (first skip-stack)))))
          (:endif
           (when skip-stack (pop skip-stack)))
          (otherwise
           ;; Normal line: apply only when not inside a skipped block.
           (when (or (null skip-stack) (not (first skip-stack)))
             (when (apply-config-line line)
               (incf count)))))))
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
