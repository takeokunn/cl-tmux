(in-package #:cl-tmux)

;;; Pane passthrough belongs to the protocol boundary: it translates tmux
;;; screen coordinates into the X10/SGR protocol requested by pane applications.

(defun %encode-mouse-for-pane (pane screen btn col row release-p)
  "Encode a mouse event in the format the pane app requested and write to PTY."
  (let* ((pane-col (1+ (- col (pane-x pane))))
         (pane-row (1+ (- row (pane-y pane))))
         (encoded
          (if (screen-mouse-sgr-mode screen)
              (format nil "~C[<~D;~D;~D~C"
                      #\Escape btn pane-col pane-row
                      (if release-p #\m #\M))
              (let ((enc-btn (if release-p 35 (+ btn 32)))
                    (enc-col (min 255 (+ pane-col 32)))
                    (enc-row (min 255 (+ pane-row 32))))
                (format nil "~C[M~C~C~C"
                        #\Escape
                        (code-char enc-btn)
                        (code-char enc-col)
                        (code-char enc-row))))))
    (when (and encoded (cl-tmux/model:pane-live-p pane) (not *client-read-only*))
      (pty-write (pane-fd pane) encoded)
      t)))

(defun %mouse-passthrough-enabled-p (mode release-p btn)
  "Return T when mouse tracking MODE wants this event."
  (declare (ignore btn))
  (case mode
    (1 (not release-p))
    (2 t)
    (3 t)
    (otherwise nil)))

(defun %try-mouse-passthrough (active-window active-pane btn col row release-p)
  "Forward a mouse event to the pane application when it enabled mouse tracking."
  (let* ((target-pane (or (and active-window
                               (pane-at-position active-window col row))
                          active-pane))
         (target-screen (and target-pane (pane-screen target-pane)))
         (mode (and target-screen (screen-mouse-mode target-screen))))
    (when (and target-pane target-screen (plusp (or mode 0))
               (%mouse-passthrough-enabled-p mode release-p btn))
      (%encode-mouse-for-pane target-pane target-screen btn col row release-p))))

(defun %forward-current-mouse-event-to-pane (pane)
  "Forward the dynamically bound mouse event to PANE."
  (when *current-mouse-event*
    (destructuring-bind (&key btn col row release-p)
        *current-mouse-event*
      (let ((screen (pane-screen pane)))
        (and screen
             (%encode-mouse-for-pane pane screen btn col row release-p))))))
