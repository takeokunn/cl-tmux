(in-package #:cl-tmux)

;;; Public mouse-event dispatcher inside the cl-tmux package.  It coordinates
;;; option gating, pane passthrough, key-table binding lookup, and built-ins.

(defun %dispatch-mouse-event-with-context (session active-window active-pane active-screen
                                          in-copy copy-table btn col row release-p)
  "Dispatch a mouse event after the active context has been resolved."
  (multiple-value-bind (location border-split border-orient)
      (%mouse-hit-location active-window col row)
    (let ((mouse-key (%mouse-key-name btn release-p location)))
      (unless (%mouse-binding-consumed-p session in-copy copy-table mouse-key)
        (%handle-mouse-built-in-action session active-window active-pane active-screen
                                       btn col row release-p location
                                       border-split border-orient)))))

(defun %dispatch-mouse-event (session btn col row release-p)
  "Handle a parsed mouse event."
  (let ((*current-mouse-event* (list :btn btn :col col :row row :release-p release-p)))
    (unwind-protect
         (multiple-value-bind (active-window active-pane active-screen in-copy copy-table)
             (%mouse-event-context session)
           (cond
             ((not (cl-tmux/options:get-option "mouse"))
              nil)
             ((%try-mouse-passthrough active-window active-pane btn col row release-p)
              nil)
             (t
              (%dispatch-mouse-event-with-context session active-window active-pane active-screen
                                                  in-copy copy-table btn col row release-p))))
      (setf *dirty* t))))
