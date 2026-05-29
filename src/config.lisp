(in-package #:cl-tmux/config)

;;; ASCII 2 = ^B.  tmux uses C-b as the default prefix.
(defconstant +prefix-key-code+ 2)

(defparameter *default-shell*
  (or (sb-ext:posix-getenv "SHELL")
      "/bin/sh")
  "Shell binary launched for new panes.")

(defparameter *status-height* 1
  "Number of rows reserved for the status bar at the bottom.")

(defconstant +pty-buf-size+ 4096
  "Byte buffer size for PTY reads.")

;;; After receiving the prefix key, the next keystroke (a character or a
;;; multi-character string like \"M-1\") is looked up here.
;;; Each entry is (char-or-string . keyword).
(defparameter *key-bindings*
  (append
   ;; Fresh conses (not a quoted literal) so SET-KEY-BINDING may mutate entries.
   (list (cons #\c :new-window)
         (cons #\n :next-window)
         (cons #\p :prev-window)
         (cons #\" :split-horizontal)
         (cons #\% :split-vertical)
         (cons #\o :next-pane)
         (cons #\d :detach)
         (cons #\? :list-keys)
         (cons #\[ :copy-mode-enter)
         (cons #\x :kill-pane)
         (cons #\& :kill-window)
         (cons #\, :rename-window)
         (cons #\H :resize-left)
         (cons #\J :resize-down)
         (cons #\K :resize-up)
         (cons #\L :resize-right))
   ;; Digit keys 0-9 all map to :select-window; the digit byte picks the window.
   (loop for d from 0 to 9 collect (cons (digit-char d) :select-window)))
  "Prefix-key dispatch alist of (char-or-string . keyword).")

(defun lookup-key-binding (key)
  "Return the command keyword bound to KEY (a character or string), or NIL."
  (cdr (assoc key *key-bindings* :test #'equal)))

(defun describe-key-bindings ()
  "A newline-separated, key-sorted listing of the current prefix bindings
   (\"<key>  <command>\" per line) for the list-keys help overlay.
   Pure: reads *KEY-BINDINGS* without mutating it (copy-list before sort)."
  (flet ((key-label (k) (if (characterp k) (string k) k)))
    (with-output-to-string (out)
      (write-string "key bindings — press prefix (C-b) then:" out)
      (dolist (binding (sort (copy-list *key-bindings*) #'string<
                             :key (lambda (b) (key-label (car b)))))
        (format out "~%  ~A  ~(~A~)" (key-label (car binding)) (cdr binding))))))

(defun set-key-binding (key command)
  "Bind KEY (a character or string) to COMMAND (a keyword) in *KEY-BINDINGS*,
   replacing any existing binding for KEY.  Returns COMMAND."
  (let ((existing (assoc key *key-bindings* :test #'equal)))
    (if existing
        (setf (cdr existing) command)
        (push (cons key command) *key-bindings*)))
  command)

(defun remove-key-binding (key)
  "Remove any binding for KEY (a character or string) from *KEY-BINDINGS*."
  (setf *key-bindings* (remove key *key-bindings* :key #'car :test #'equal)))

;;; ── Config file ──────────────────────────────────────────────────────────

(defun %config-tokens (line)
  "Split LINE into whitespace-separated tokens (no quoting support)."
  (let ((tokens '()) (start nil) (len (length line)))
    (dotimes (i len)
      (let ((ws (member (char line i) '(#\Space #\Tab))))
        (cond ((and (not ws) (null start)) (setf start i))
              ((and ws start) (push (subseq line start i) tokens) (setf start nil)))))
    (when start (push (subseq line start len) tokens))
    (nreverse tokens)))

(defun %parse-key-token (token)
  "A single-character TOKEN denotes that character; a longer token (e.g. M-1)
   is kept as the string itself, matching the *KEY-BINDINGS* key format."
  (if (= (length token) 1) (char token 0) token))

(defparameter *bindable-commands*
  '(:new-window :next-window :prev-window :next-pane :prev-pane
    :split-horizontal :split-vertical :detach :kill-pane :kill-window
    :rename-window :list-keys :copy-mode-enter
    :resize-left :resize-right :resize-up :resize-down
    :select-window)   ; the pressed digit chooses the window
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

(defun %config-path-from (override xdg home)
  "Resolve the config-file path from environment values (OVERRIDE = $CL_TMUX_CONF,
   XDG = $XDG_CONFIG_HOME, each a string or NIL) and HOME (a directory pathname).

   Precedence (XDG Base Directory spec):
     1. $CL_TMUX_CONF                              — explicit override
     2. $XDG_CONFIG_HOME/cl-tmux/cl-tmux.conf
     3. ~/.config/cl-tmux/cl-tmux.conf             — XDG default when unset
   Empty strings are treated as unset.  Pure: no I/O, no environment access."
  (flet ((set-p (s) (and s (plusp (length s)))))
    (if (set-p override)
        (pathname override)
        (let ((base (if (set-p xdg)
                        xdg
                        (namestring (merge-pathnames ".config/" home)))))
          (pathname (format nil "~A/cl-tmux/cl-tmux.conf"
                            (string-right-trim "/" base)))))))

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
