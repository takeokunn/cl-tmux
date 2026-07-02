(in-package #:cl-tmux/commands)

;;;; run-shell / if-shell subprocess execution.
;;;;
;;;; run_shell(cmd)            :- subprocess(cmd, timeout=30, output=string).
;;;; if_shell(cmd, then, else) :- subprocess(cmd), exit_code=0 -> then ; else.
;;;;
;;;; Both run-shell and if-shell accept a :timeout keyword (seconds, default
;;;; +shell-command-timeout+).  Synchronous callers are bounded by both the Lisp
;;;; control path and the subprocess itself; background callers return
;;;; immediately but the worker still gives the subprocess a bounded lifetime.
;;;;
;;;; uiop:run-program is used instead of sb-ext:run-program so the code is
;;;; portable across all ASDF-supported implementations.
;;;;
;;;; if-shell is exported and wired to the :if-shell dispatch key in dispatch.lisp
;;;; so it is reachable from the prefix-key handler.

(defconstant +shell-command-timeout+ 30
  "Default wall-clock timeout, in seconds, for shell subprocesses.")

(defun %run-with-timeout (thunk timeout-seconds)
  "Run THUNK in a fresh thread; join it up to TIMEOUT-SECONDS.
   Returns (funcall thunk) result or NIL if the timeout expires."
  (handler-case
      (bt:with-timeout (timeout-seconds)
        (funcall thunk))
    (bt:timeout () nil)))

(defmacro with-shell-timeout ((shell-var timeout) &body body)
  "Bind SHELL-VAR to the active shell binary and run BODY with a TIMEOUT (seconds).
   TIMEOUT is evaluated at macro-expansion call time and passed directly to
   %RUN-WITH-TIMEOUT.  Returns the result of BODY or NIL when the timeout fires."
  `(%run-with-timeout
     (lambda ()
       (let ((,shell-var (or *default-shell* "/bin/sh")))
         ,@body))
     ,timeout))

(defun %run-shell-program (shell command &key output timeout)
  "Run COMMAND through SHELL with an explicit subprocess TIMEOUT."
  (uiop:run-program (list shell "-c" command)
                    :output output
                    :ignore-error-status t
                    :timeout timeout))

(defun run-shell (command &key background (timeout +shell-command-timeout+))
  "Run COMMAND in a subshell.  Returns the output string (stdout) when BACKGROUND
   is nil, or T immediately when BACKGROUND is T.
   Uses *default-shell* for the shell binary.
   TIMEOUT (seconds, default +shell-command-timeout+) limits how long the
   subprocess may run; when the synchronous limit is exceeded NIL is returned."
  (if background
      (progn
        (bt:make-thread
          (lambda ()
            (let ((shell (or *default-shell* "/bin/sh")))
              (ignore-errors
                (%run-shell-program shell command
                                    :output nil
                                    :timeout timeout))))
          :name "shell-bg")
        t)
      (with-shell-timeout (shell timeout)
        (%run-shell-program shell command
                            :output :string
                            :timeout timeout))))

(defun if-shell (command then-fn &key else-fn (timeout +shell-command-timeout+))
  "Run COMMAND; call THEN-FN if exit code is 0, ELSE-FN otherwise.
   THEN-FN and ELSE-FN are zero-argument functions (keyword arguments).
   TIMEOUT (seconds, default +shell-command-timeout+) limits how long the
   command may run; when the limit is exceeded ELSE-FN is called."
  (let ((exit-code
          (with-shell-timeout (shell timeout)
            (multiple-value-bind (output error-output code)
                (%run-shell-program shell command
                                    :output nil
                                    :timeout timeout)
              (declare (ignore output error-output))
              code))))
    (if (and exit-code (zerop exit-code))
        (when then-fn (funcall then-fn))
        (when else-fn (funcall else-fn)))))
