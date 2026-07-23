(in-package #:cl-tmux/config)

;;; Runtime services shared by config-time shell directives.

(defvar *process-boundary* (cl-boundary-kit:make-process-boundary)
  "The cl-boundary-kit process boundary run-shell/if-shell (commands-shell.lisp)
   and config-time shell directives (this file) shell out through.  Real
   subprocess execution by default; tests rebind it to
   cl-boundary-kit:make-test-process-boundary / make-recording-process-boundary
   for deterministic run-shell/if-shell specs that never touch a real shell.")

(defconstant +config-shell-command-timeout+ 30
  "Seconds to allow config-time shell directives to run.")

(defun %run-config-shell-command (command &key combine-stderr directory)
  "Run COMMAND through /bin/sh while loading config, with a bounded lifetime.
   Returns (values stdout-string stderr-string exit-code), matching
   uiop:run-program's prior :output :string calling convention."
  (let ((result (cl-boundary-kit:process-boundary-run
                 *process-boundary* "/bin/sh"
                 :arguments (list "-c" command)
                 :output :string
                 :error-output (when combine-stderr :output)
                 :timeout +config-shell-command-timeout+
                 :directory directory)))
    (values (getf result :stdout) (getf result :stderr) (getf result :exit-code))))

(defun %run-config-shell-command-safe (command &key combine-stderr directory delay)
  "Run COMMAND and return NIL if the shell process signals a serious condition."
  (handler-case
      (progn
        (when (and delay (plusp delay))
          (sleep delay))
        (%run-config-shell-command command
                                   :combine-stderr combine-stderr
                                   :directory directory))
    (serious-condition () nil)))

(defun %run-config-shell-command-background (command &key combine-stderr directory)
  "Run config COMMAND asynchronously and report the directive as handled."
  (bt:make-thread
   (lambda ()
     (%run-config-shell-command-safe command
                                     :combine-stderr combine-stderr
                                     :directory directory))
   :name "cl-tmux config run-shell")
  t)
