(in-package #:cl-tmux/config)

;;; ── Config file parsing + directive processing ───────────────────────────
;;;
;;; This file depends on the key-binding mutators defined in config.lisp
;;; (set-key-binding, remove-key-binding) and the mutable specials
;;; (*key-bindings*, *default-shell*, *status-height*).

(defun %whitespace-p (ch)
  "True when CH is a configuration whitespace character."
  (or (char= ch #\Space) (char= ch #\Tab)))

(defun %config-tokens (line)
  "Tokenize LINE into a list of strings, handling:
   - unquoted whitespace as delimiter
   - \"double quoted\" strings (spaces preserved, \\x escapes processed)
   - 'single quoted' strings (literal content, no escapes)
   - \\ (backslash) escaping of the next character outside quotes
   Returns a list of token strings."
  (let ((tokens '())
        (current (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        (in-token nil)
        (i 0)
        (len (length line)))
    (flet ((push-char (ch)
             (vector-push-extend ch current)
             (setf in-token t))
           (finish-token ()
             (when in-token
               (push (copy-seq current) tokens)
               (setf (fill-pointer current) 0)
               (setf in-token nil))))
      (loop while (< i len) do
        (let ((ch (char line i)))
          (cond
            ;; Backslash escape outside quotes
            ((char= ch #\\)
             (incf i)
             (when (< i len)
               (push-char (char line i))
               (incf i)))
            ;; Double-quoted string: only treat as quoted if there's a closing ".
            ;; If no closing " is found before EOL, treat the " as a literal char.
            ((char= ch #\")
             (let ((close-pos (position #\" line :start (1+ i))))
               (if close-pos
                   ;; Found closing quote — process as quoted string
                   (progn
                     (setf in-token t)
                     (incf i)  ; skip opening "
                     (loop while (and (< i len) (char/= (char line i) #\"))
                           do (let ((qch (char line i)))
                                (cond
                                  ((and (char= qch #\\) (< (1+ i) len))
                                   (incf i)
                                   (push-char (char line i)))
                                  (t
                                   (push-char qch))))
                              (incf i))
                     ;; Skip closing "
                     (when (< i len) (incf i)))
                   ;; No closing quote — treat " as a literal character
                   (progn
                     (push-char ch)
                     (incf i)))))
            ;; Single-quoted string
            ((char= ch #\')
             (setf in-token t)
             (incf i)
             (loop while (and (< i len) (char/= (char line i) #\'))
                   do (push-char (char line i))
                      (incf i))
             ;; Skip closing '
             (when (< i len) (incf i)))
            ;; Whitespace outside quotes
            ((%whitespace-p ch)
             (finish-token)
             (incf i))
            ;; Regular character
            (t
             (push-char ch)
             (incf i)))))
      (finish-token))
    (nreverse tokens)))

(defun %parse-key-token (token)
  "A single-character TOKEN denotes that character; a longer token (e.g. M-1)
   is kept as the string itself, matching the *KEY-BINDINGS* key format."
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
   :copy-mode-up, :copy-mode-down), which are produced by copy-mode
   interception, not by key lookup.")

(defun %command-keyword (name)
  "Return the bindable command keyword named by NAME (case-insensitive), or NIL
   if NAME is not a recognized command.  Uses FIND-SYMBOL so unknown command
   names are never interned into the keyword package."
  (let ((kw (find-symbol (string-upcase name) :keyword)))
    (and kw (member kw *bindable-commands*) kw)))

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
;;; parse-bind-key-args handles the optional flags before key and command:
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
        ((null rest) (return nil))             ; ran out of args without key+cmd
        ((string= (first rest) "-n")
         ;; -n: bind in the root table (no prefix required)
         (setf table "root")
         (setf rest (rest rest)))
        ((string= (first rest) "-r")
         ;; -r: mark binding as repeatable
         (setf repeatable t)
         (setf rest (rest rest)))
        ((string= (first rest) "-T")
         ;; -T table-name
         (setf rest (rest rest))
         (when (null rest) (return nil))
         (setf table (first rest))
         (setf rest (rest rest)))
        (t
         ;; Next args should be exactly: key command (no extra args allowed)
         (unless (= (length rest) 2) (return nil))
         (let* ((key-tok  (%parse-key-token (first rest)))
                (cmd-name (second rest))
                (kw       (%command-keyword cmd-name)))
           (if kw
               (return (values table key-tok kw repeatable))
               (return nil))))))))

;;; Note: "bind" with flags (-n, -r, -T) uses variable-arity dispatch which is
;;; handled separately in %apply-bind-with-flags below.
;;; The macro-generated apply-config-directive handles the simple 2-arg form.

(define-config-directives
  ("unbind" 1 (key)
    (remove-key-binding (%parse-key-token key))
    t)
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
  ("setw" 2 (name value)
    ;; setw / set-window-option: same as set for now (global scope).
    (cl-tmux/options:set-option name value)
    t)
  ("set-window-option" 2 (name value)
    (cl-tmux/options:set-option name value)
    t))

(defun apply-config-directive (tokens)
  "Apply one parsed config directive (list of string TOKENS) to live state.
   Returns T when applied, NIL for an unknown/invalid directive.
   Handles bind [-n] [-r] [-T table] key command in addition to
   simple fixed-arity directives."
  (when tokens
    (let ((cmd (first tokens))
          (args (rest tokens)))
      (cond
        ;; \"bind\" with any number of args — handle flags
        ((string= cmd "bind")
         (multiple-value-bind (table key kw repeatable)
             (%parse-bind-key-args args)
           (when kw
             ;; Update legacy *key-bindings* alist for backward compat
             ;; (only for the \"prefix\" table)
             (when (string= table "prefix")
               (set-key-binding key kw))
             ;; Update the key-tables system
             (key-table-bind table key kw :repeatable repeatable)
             t)))
        ;; Delegate everything else to the inner directive handler
        (t (%apply-config-directive-inner tokens))))))

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
