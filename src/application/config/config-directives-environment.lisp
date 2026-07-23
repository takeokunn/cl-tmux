(in-package #:cl-tmux/config)

;;; set-environment directive handling.

(defun %parse-set-environment-flags (args)
  "Parse the [-g] [-u|-r] [-t target] flags of a set-environment directive."
  (let ((remove-p nil) (global-p nil) (target-p nil) (target-name nil))
    (let ((remaining
            (%consuming-flags (args tok rest)
              ((member tok '("-u" "-r") :test #'string=)
               (setf remove-p t))
              ((string= tok "-g")
               (setf global-p t))
              ((string= tok "-t")
               (setf target-p t
                     target-name (first rest))
               (when rest (setf rest (cdr rest)))))))
      (values remaining remove-p global-p target-p target-name))))

(defun %apply-set-environment-to-session (target-name remove-p var-name var-value)
  "Apply a `set-environment -t TARGET-NAME` directive to a session overlay."
  (let ((session (and target-name (cl-tmux::server-find-session target-name))))
    (when (and session var-name)
      (if remove-p
          (cl-tmux/model:session-unset-environment session var-name)
          (when var-value
            (cl-tmux/model:session-set-environment session var-name var-value)))
      t)))

(defun %apply-set-environment-to-process (remove-p var-name var-value)
  "Apply a global set-environment directive to the process environment."
  (when var-name
    (if remove-p
        (let ((fn (find-posix-function "UNSETENV")))
          (when fn (ignore-errors (funcall fn var-name))))
        (when var-value
          (%config-setenv var-name var-value)))
    t))

(defun %apply-set-environment-directive (cmd args)
  "Handle set-environment [-g] [-u|-r] [-t target] VAR [VALUE]."
  (when (string= cmd "set-environment")
    (multiple-value-bind (remaining remove-p global-p target-p target-name)
        (%parse-set-environment-flags args)
      (let ((var-name (first remaining)) (var-value (second remaining)))
        (cond
          ((and global-p target-p) nil)
          (target-p (%apply-set-environment-to-session
                     target-name remove-p var-name var-value))
          (t (%apply-set-environment-to-process remove-p var-name var-value)))))))
