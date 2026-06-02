(in-package #:cl-tmux/config)

;;; ── Config file parsing + directive processing ───────────────────────────
;;;
;;; This file depends on the key-binding mutators defined in config.lisp
;;; (set-key-binding, remove-key-binding) and the mutable specials
;;; (*key-tables*, *default-shell*, *status-height*).

(defun %whitespace-p (ch)
  "True when CH is a configuration whitespace character."
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
                do (let ((qch (char line j)))
                     (cond
                       ((and (char= qch #\\) (< (1+ j) len))
                        (incf j)
                        (funcall push-char (char line j)))
                       (t
                        (funcall push-char qch))))
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
  (let* ((tokens  '())
         (current (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
         (in-token nil)
         (len     (length line)))
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
  '(:new-window :next-window :prev-window :next-pane :prev-pane
    :split-horizontal :split-vertical :detach :kill-pane :kill-window
    :rename-window :rename-session :list-keys :copy-mode-enter
    :resize-left :resize-right :resize-up :resize-down
    :select-window   ; the pressed digit chooses the window
    :paste-buffer
    :zoom-toggle
    :select-layout-even-h :select-layout-even-v :select-layout-tiled
    :run-shell :list-sessions :list-sessions-full :list-windows
    :swap-pane-forward :swap-pane-backward
    :last-pane :display-panes
    :new-session :kill-session :rename-session-prompt
    :switch-client-next :switch-client-prev :last-session
    :display-message :source-file
    :show-options :show-option)
  "Command keywords a config-file `bind` directive may target.  This is the
   user-bindable subset of the commands cl-tmux:dispatch-command handles — it
   deliberately EXCLUDES the copy-mode-internal commands (:copy-mode-exit,
   :copy-mode-begin-selection, :copy-mode-yank), which are produced by copy-mode
   interception, not by key lookup.")

(defun %command-keyword (name)
  "Return the bindable command keyword named by NAME (case-insensitive), or NIL
   if NAME is not a recognized command.  Uses FIND-SYMBOL so unknown command
   names are never interned into the keyword package."
  (let ((kw (find-symbol (string-upcase name) :keyword)))
    (and kw (member kw *bindable-commands*) kw)))

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
   Returns (values table key command repeatable) where TABLE is \"prefix\" by
   default, or NIL when ARGS do not form a valid binding."
  (let ((table "prefix")
        (repeatable nil)
        (rest args))
    (loop
      (cond
        ((null rest) (return nil))
        ((string= (first rest) "-n")
         (setf table "root")
         (setf rest (rest rest)))
        ((string= (first rest) "-r")
         (setf repeatable t)
         (setf rest (rest rest)))
        ((string= (first rest) "-T")
         (setf rest (rest rest))
         (when (null rest) (return nil))
         (setf table (first rest))
         (setf rest (rest rest)))
        (t
         (unless (= (length rest) 2) (return nil))
         (let* ((key-token (%parse-key-token (first rest)))
                (cmd-name  (second rest))
                (kw        (%command-keyword cmd-name)))
           (if kw
               (return (values table key-token kw repeatable))
               (return nil))))))))

;;; ── unbind-key flag parsing ──────────────────────────────────────────────
;;;
;;; %parse-unbind-key-args handles optional [-n] [-T table] flags before the key.
;;; Returns (values table key) or (values nil nil) on parse failure.

(defun %parse-unbind-key-args (args)
  "Parse the ARGS list for an unbind directive (excludes the verb itself).
   Returns (values table key) where TABLE is \"prefix\" by default,
   or (values nil nil) on parse failure."
  (let ((table "prefix")
        (rest args))
    (loop
      (cond
        ((null rest) (return (values nil nil)))
        ((string= (first rest) "-n")
         (setf table "root")
         (setf rest (rest rest)))
        ((string= (first rest) "-T")
         (setf rest (rest rest))
         (when (null rest) (return (values nil nil)))
         (setf table (first rest))
         (setf rest (rest rest)))
        (t
         (unless (= (length rest) 1) (return (values nil nil)))
         (return (values table (%parse-key-token (first rest)))))))))

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
   (multiple-value-bind (table key kw repeatable)
       (%parse-bind-key-args args)
     (when kw
       (key-table-bind table key kw :repeatable repeatable)
       t)))
  (("unbind" "unbind-key")
   (multiple-value-bind (table key)
       (%parse-unbind-key-args args)
     (when (and table key)
       (let ((tbl (gethash table *key-tables*)))
         (when tbl (remhash key tbl)))
       t))))

;;; ── Simple directive definitions ─────────────────────────────────────────

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
  ("set" 2 (name value)
    ;; set option value — stores in global options hash.
    (cl-tmux/options:set-option name value)
    t)
  ("set-option" 2 (name value)
    ;; set-option: canonical long form of set.
    (cl-tmux/options:set-option name value)
    t)
  ("setw" 2 (name value)
    ;; setw / set-window-option: same as set for now (global scope).
    (cl-tmux/options:set-option name value)
    t)
  ("set-window-option" 2 (name value)
    (cl-tmux/options:set-option name value)
    t)
  ("sets" 2 (name value)
    ;; sets / set-session-option: alias of set for session-scoped options.
    (cl-tmux/options:set-option name value)
    t)
  ("set-session-option" 2 (name value)
    ;; set-session-option: canonical long form of sets.
    (cl-tmux/options:set-option name value)
    t))

(defun apply-config-directive (tokens)
  "Apply one parsed config directive (list of string TOKENS) to live state.
   Returns T when applied, NIL for an unknown/invalid directive.
   Handles bind [-n] [-r] [-T table] key command and
   unbind/unbind-key [-n] [-T table] key, in addition to simple directives."
  (when tokens
    (let ((cmd  (first tokens))
          (args (rest tokens)))
      (or (%apply-key-directive cmd args)
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
