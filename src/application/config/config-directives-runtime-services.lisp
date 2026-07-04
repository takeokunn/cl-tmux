(in-package #:cl-tmux/config)

;;; Runtime services shared by config-time shell directives.

(defconstant +config-shell-command-timeout+ 30
  "Seconds to allow config-time shell directives to run.")

(defun %run-config-shell-command (command &key combine-stderr directory)
  "Run COMMAND through /bin/sh while loading config, with a bounded lifetime."
  (uiop:run-program (list "/bin/sh" "-c" command)
                    :output :string
                    :error-output (when combine-stderr :output)
                    :ignore-error-status t
                    :timeout +config-shell-command-timeout+
                    :directory directory))

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
