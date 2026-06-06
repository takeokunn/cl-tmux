(in-package #:cl-tmux/config)

;;; ── Config file parsing + directive processing ───────────────────────────
;;;
;;; This file depends on the key-binding mutators defined in config.lisp
;;; (set-key-binding, remove-key-binding) and the mutable specials
;;; (*key-tables*, *default-shell*, *status-height*).

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

(defun %parse-key-token (token)
  "A single-character TOKEN denotes that character; a longer token (e.g. M-1)
   is kept as the string itself, matching the key-table key format."
  (if (= (length token) 1) (char token 0) token))

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
    :rename-window :rename-session
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
    :display-message :display-panes :display-info :display-popup
    :capture-pane :clear-history :clock-mode
    ;; Scripting / hooks
    :run-shell :if-shell :command-prompt :wait-for
    ;; Client management
    :choose-client :choose-tree :refresh-client)
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

;;; ── Declarative directive dispatch macro ──────────────────────────────────

(defmacro define-config-directives (&rest rules)
  "Build %APPLY-CONFIG-DIRECTIVE-INNER from a declarative table of directive RULES.

   Each RULE is (NAME ARITY (ARG...) &body BODY):
     NAME   – the directive keyword as a string (e.g. \"set-shell\")
     ARITY  – the exact number of arguments the directive takes
     (ARG…) – symbols bound to those arguments inside BODY
     BODY   – forms run when NAME matches with the right ARITY; their value is
              returned (non-NIL ⇒ the directive was applied).

   The outer APPLY-CONFIG-DIRECTIVE function wraps this inner dispatcher and
   handles 'bind' with variable-arity flags separately."
  `(defun %apply-config-directive-inner (tokens)
     "Apply one non-bind config directive (list of string TOKENS) to live state.
      Returns T when applied, NIL for an unknown/invalid directive."
     (when tokens
       (let ((cmd (first tokens)) (args (rest tokens)))
         (declare (ignorable args))
         (cond
           ,@(mapcar
              (lambda (rule)
                (destructuring-bind (name arity arglist &body body) rule
                  `((and (string= cmd ,name) (= (length args) ,arity))
                    (destructuring-bind ,arglist args
                      (declare (ignorable ,@arglist))
                      ,@body))))
              rules)
           (t nil))))))

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
         (let ((key-token  (%parse-key-token (first remaining)))
               (cmd-tokens (rest remaining)))
           (return
             (if (= (length cmd-tokens) 1)
                 ;; Single command word: resolve to a keyword, rejecting an
                 ;; unknown command (preserves the original behaviour).
                 (let ((keyword (%command-keyword (first cmd-tokens))))
                   (if keyword (values table key-token keyword repeatable) nil))
                 ;; Command WITH arguments: store the token list, to be run via
                 ;; %run-command-tokens when the key is pressed.
                 (values table key-token cmd-tokens repeatable)))))))))

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
       t))))

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
    ;; Positive integers only; non-numeric / non-positive values are ignored.
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
    ;; Register a tmux command to run when the named hook fires.  COMMAND-NAME
    ;; must be a bindable command name; unknown commands are rejected (NIL).
    (let ((keyword (%command-keyword command-name)))
      (when keyword
        (cl-tmux/hooks:set-command-hook event-name keyword)
        t))))

;;; ── set-option flag handling (set -g / -a / ...) ────────────────────────────
;;;
;;; The fixed-arity directive table cannot match `set -g status off` (3 tokens vs
;;; arity 2), so the canonical .tmux.conf form silently failed.  %apply-set-
;;; directive consumes leading scope flags (-g global / -s server / -w window /
;;; -o only-if-unset — all the same flat global store here) and handles -a
;;; (append).  It only activates when a set-family directive carries a flag, so
;;; the plain `set name value` form still flows through the normal table below.

(defparameter +set-directive-names+
  '("set" "set-option" "setw" "set-window-option" "sets" "set-session-option")
  "Config directive verbs that forward to the global option store.")

(defun %strip-set-flags (args)
  "Consume leading -X flag tokens from a set directive's ARGS.
   Returns (values HAD-FLAG APPEND-P POSITIONALS): HAD-FLAG is T when any flag was
   present, APPEND-P is T when an -a flag appeared, POSITIONALS is the remaining
   non-flag tokens (the option name and value)."
  (let ((had-flag nil) (append-p nil) (rest args))
    (loop while (and rest
                     (let ((tok (first rest)))
                       (and (>= (length tok) 2) (char= (char tok 0) #\-))))
          do (let ((tok (pop rest)))
               (setf had-flag t)
               (when (find #\a tok) (setf append-p t))))
    (values had-flag append-p rest)))

(defun %apply-set-directive (cmd args)
  "Apply a flag-bearing set-family directive (e.g. `set -g status off`,
   `set -ag word-separators x`).  Returns T when applied; NIL when CMD is not a
   set verb or carries no flags (so the normal directive table handles the plain
   `set name value` form unchanged)."
  (when (member cmd +set-directive-names+ :test #'string=)
    (multiple-value-bind (had-flag append-p positionals) (%strip-set-flags args)
      (when (and had-flag (first positionals))
        (let ((name  (first positionals))
              (value (format nil "~{~A~^ ~}" (rest positionals))))
          (if append-p
              (cl-tmux/options:set-option
               name (concatenate 'string
                                 (princ-to-string
                                  (or (cl-tmux/options:get-option name nil) ""))
                                 value))
              (cl-tmux/options:set-option name value))
          t)))))

(defun apply-config-directive (tokens)
  "Apply one parsed config directive (list of string TOKENS) to live state.
   Returns T when applied, NIL for an unknown/invalid directive.
   Handles bind [-n] [-r] [-T table] key command, unbind/unbind-key [-n] [-T
   table] key, and set [-g|-a|...] name value, in addition to simple directives."
  (when tokens
    (let ((cmd  (first tokens))
          (args (rest tokens)))
      (or (%apply-key-directive cmd args)
          (%apply-set-directive cmd args)
          (%apply-config-directive-inner tokens)))))

(defun apply-config-line (line)
  "Apply a single config LINE.  Blank lines and #-comments are ignored.
   Returns T when a directive was applied."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Newline) line)))
    (and (plusp (length trimmed))
         (char/= (char trimmed 0) #\#)
         (apply-config-directive (%config-tokens trimmed)))))

(defun load-config-from-stream (stream)
  "Apply every directive line read from STREAM.  Returns the count applied."
  (loop for line = (read-line stream nil nil)
        while line
        count (apply-config-line line)))

(defun load-config-from-string (text)
  "Apply every directive line in TEXT.  Returns the count of directives applied."
  (with-input-from-string (in text)
    (load-config-from-stream in)))

(defun %env-set-p (s)
  "True when environment variable string S is set and non-empty."
  (and s (plusp (length s))))

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

(defun config-file-path ()
  "Path to the user config file, honoring $CL_TMUX_CONF then the XDG Base
   Directory spec ($XDG_CONFIG_HOME, default ~/.config).  See %config-path-from."
  (%config-path-from (sb-ext:posix-getenv "CL_TMUX_CONF")
                     (sb-ext:posix-getenv "XDG_CONFIG_HOME")
                     (user-homedir-pathname)))

(defun load-config-file (&optional (path (config-file-path)))
  "Load and apply the config file at PATH if it exists (returns the count of
   directives applied), or NIL when no file is found."
  (with-open-file (in path :direction :input :if-does-not-exist nil)
    (when in
      (load-config-from-stream in))))
