(in-package #:cl-tmux)

;;; Mouse key names and dispatch actions are data classification, not effects.

(defun %mouse-location-name (location)
  "Return the human-readable suffix used in tmux mouse key names."
  (ecase location
    (:status "Status")
    (:border "Border")
    (:pane "Pane")))

(defun %mouse-button-name (btn)
  "Return tmux's mouse button suffix, or NIL for unnamed buttons."
  (cond
    ((= btn +mouse-btn-left+) "1")
    ((= btn +mouse-btn-middle+) "2")
    ((= btn 2) "3")
    (t nil)))

(defun %mouse-key-name (btn release-p location)
  "Build the tmux mouse key name for a mouse event."
  (let ((button (%mouse-button-name btn))
        (location-name (%mouse-location-name location)))
    (cond
      ((= btn +mouse-btn-scroll-up+) (concatenate 'string "WheelUp" location-name))
      ((= btn +mouse-btn-scroll-down+) (concatenate 'string "WheelDown" location-name))
      (button (concatenate 'string (if release-p "MouseUp" "MouseDown")
                           button location-name))
      (t nil))))

(defun %mouse-event-action (btn release-p location)
  "Classify a mouse event into a symbolic built-in action."
  (cond
    ((and (eq location :status) (not release-p) (= btn +mouse-btn-left+))
     :status-click)
    ((= btn +mouse-btn-scroll-up+) :scroll-up)
    ((= btn +mouse-btn-scroll-down+) :scroll-down)
    ((and (= btn +mouse-btn-left+) (not release-p) (not (eq location :status)))
     :left-press)
    ((and (= btn +mouse-btn-left+) release-p)
     :left-release)
    ((and (= btn +mouse-btn-middle+) (not release-p) (not (eq location :status)))
     :middle-press)
    ((= btn +mouse-btn-motion+) :motion)
    (t nil)))

(defun %mouse-hit-location (active-window col row)
  "Return the mouse location as (values location split orientation)."
  (let ((status-row (1- *term-rows*)))
    (cond
      ((= row status-row)
       (values :status nil nil))
      (active-window
       (multiple-value-bind (split orient)
           (%border-at-position active-window col row)
         (if split
             (values :border split orient)
             (values :pane nil nil))))
      (t
       (values :pane nil nil)))))

(defun %mouse-binding-consumed-p (session in-copy copy-table mouse-key)
  "Return T when a user mouse binding handled the event."
  (or (and in-copy (%try-bound-string-key session copy-table mouse-key))
      (%try-bound-string-key session +table-root+ mouse-key)))

(defun %mouse-event-context (session)
  "Return the active mouse dispatch context for SESSION."
  (let* ((active-window (session-active-window session))
         (active-pane (session-active-pane session))
         (active-screen (and active-pane (pane-screen active-pane))))
    (values active-window
            active-pane
            active-screen
            (and active-screen (screen-copy-mode-p active-screen))
            (%active-copy-mode-table))))
