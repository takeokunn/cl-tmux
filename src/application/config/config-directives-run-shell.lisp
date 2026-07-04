(in-package #:cl-tmux/config)

;;; run-shell / run directive handling.

(defun %run-shell-flag-cluster-p (token)
  "True when TOKEN is a cluster containing only no-argument run-shell flags."
  (and (> (length token) 2)
       (char= (char token 0) #\-)
       (loop for index from 1 below (length token)
             always (member (char token index) '(#\b #\C #\E) :test #'char=))))

(defun %apply-run-shell-flag-character (flag)
  "Return the parser state assignment represented by no-argument FLAG."
  (case flag
    (#\b :background)
    (#\C :tmux-command)
    (#\E :combine-stderr)
    (otherwise nil)))

(defun %parse-run-shell-directive-args (args)
  "Parse config-time run-shell ARGS."
  (let ((remaining args)
        (background-p nil)
        (tmux-command-p nil)
        (combine-stderr-p nil)
        (start-directory nil)
        (invalid-p nil))
    (labels ((apply-state (state)
               (case state
                 (:background (setf background-p t))
                 (:tmux-command (setf tmux-command-p t))
                 (:combine-stderr (setf combine-stderr-p t)))))
      (loop while (and remaining (%leading-flag-token-p (first remaining)))
            for token = (pop remaining)
            do (cond
                 ((%run-shell-flag-cluster-p token)
                  (loop for index from 1 below (length token)
                        do (apply-state
                            (%apply-run-shell-flag-character
                             (char token index)))))
                 ((string= token "-b") (setf background-p t))
                 ((string= token "-C") (setf tmux-command-p t))
                 ((string= token "-E") (setf combine-stderr-p t))
                 ((string= token "-c")
                  (if remaining
                      (setf start-directory (pop remaining))
                      (setf invalid-p t)))
                 (t
                  (setf invalid-p t)))
            when invalid-p
              do (return)))
    (values remaining background-p tmux-command-p combine-stderr-p
            start-directory invalid-p)))

(defun %apply-run-shell-tmux-command (command &key background)
  "Apply a run-shell -C COMMAND, optionally in the background."
  (flet ((apply-command ()
           (ignore-errors (apply-config-directive (%config-tokens command)))))
    (if background
        (progn
          (bt:make-thread #'apply-command
                          :name "cl-tmux config run-shell -C")
          t)
        (progn
          (apply-command)
          t))))

(defun %apply-run-shell-directive (cmd args)
  "Handle run-shell [-bCE] [-c start-directory] shell-command."
  (when (member cmd '("run-shell" "run") :test #'string=)
    (multiple-value-bind (remaining background-p tmux-command-p combine-stderr-p
                          start-directory invalid-p)
        (%parse-run-shell-directive-args args)
      (when invalid-p
        (return-from %apply-run-shell-directive nil))
      (let ((command (%join-config-tokens remaining)))
        (cond
          ((null command) t)
          (tmux-command-p
           (%apply-run-shell-tmux-command command
                                          :background background-p))
          (t
           (let ((expanded-command (%expand-leading-tilde command))
                 (expanded-directory (and start-directory
                                          (%expand-leading-tilde start-directory))))
             (if background-p
                 (%run-config-shell-command-background
                  expanded-command
                  :combine-stderr combine-stderr-p
                  :directory expanded-directory)
                 (%run-config-shell-command-safe
                  expanded-command
                  :combine-stderr combine-stderr-p
                  :directory expanded-directory)))
           t))))))
