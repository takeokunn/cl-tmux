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
  "Split LINE into whitespace-separated tokens (no quoting support).
   Uses a functional loop: each iteration either starts or ends a token."
  (loop with start = nil
        for i from 0 below (length line)
        for wsp = (%whitespace-p (char line i))
        when (and wsp start)
          collect (subseq line start i) into tokens
          and do (setf start nil)
        when (and (not wsp) (null start))
          do (setf start i)
        finally
          (return (if start
                      (append tokens (list (subseq line start)))
                      tokens))))

(defun %parse-key-token (token)
  "A single-character TOKEN denotes that character; a longer token (e.g. M-1)
   is kept as the string itself, matching the *KEY-BINDINGS* key format."
  (if (= (length token) 1) (char token 0) token))

(defparameter *bindable-commands*
  '(:new-window :next-window :prev-window :next-pane :prev-pane
    :split-horizontal :split-vertical :detach :kill-pane :kill-window
    :rename-window :list-keys :copy-mode-enter
    :resize-left :resize-right :resize-up :resize-down
    :select-window   ; the pressed digit chooses the window
    :paste-buffer
    :select-layout-even-h :select-layout-even-v :select-layout-tiled)
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
  "Build APPLY-CONFIG-DIRECTIVE from a declarative table of directive RULES.

   Each RULE is (NAME ARITY (ARG...) &body BODY):
     NAME   – the directive keyword as a string (e.g. \"bind\")
     ARITY  – the exact number of arguments the directive takes
     (ARG…) – symbols bound to those arguments inside BODY
     BODY   – forms run when NAME matches with the right ARITY; their value is
              returned (non-NIL ⇒ the directive was applied).

   APPLY-CONFIG-DIRECTIVE takes a list of string TOKENS, dispatches on the
   leading command token and argument count, and returns T when a directive was
   applied or NIL for an unknown command, wrong arity, or invalid argument — so
   a single bad line never aborts the rest of the config."
  `(defun apply-config-directive (tokens)
     "Apply one parsed config directive (list of string TOKENS) to live state.
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

(define-config-directives
  ("bind" 2 (key command)
    ;; Bind KEY to COMMAND only when COMMAND names a recognized command.
    (let ((kw (%command-keyword command)))
      (when kw
        (set-key-binding (%parse-key-token key) kw)
        t)))
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
        t))))

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
