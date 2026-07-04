(in-package #:cl-tmux)

;;; Built-in mouse actions mutate application state after key bindings decline
;;; the event.  Classification and protocol forwarding stay outside this file.

(defun %handle-mouse-built-in-action (session active-window active-pane active-screen
                                      btn col row release-p location
                                      border-split border-orient)
  "Run the built-in mouse action after key-table bindings have had a chance."
  (case (%mouse-event-action btn release-p location)
    (:status-click
     (%mouse-status-bar-click session col))
    (:scroll-up
     (%mouse-handle-scroll-up active-screen))
    (:scroll-down
     (%mouse-handle-scroll-down active-screen))
    (:left-press
     (let* ((now (%now-ms))
            (count (%mouse-click-count *last-mouse-click*
                                       now row col
                                       (or (cl-tmux/options:get-option "double-click-time")
                                           500))))
       (%mouse-handle-left-press active-window col row now count
                                 border-split border-orient)))
    (:left-release
     (%mouse-handle-left-release active-window active-pane))
    (:middle-press
     (%mouse-handle-middle-press active-window col row))
    (:motion
     (%mouse-handle-motion active-window active-pane col row))
    (t nil)))

(defun %mouse-handle-scroll-up (active-screen)
  "Enter copy mode if needed, then scroll back."
  (when active-screen
    (%mouse-enter-copy-mode-if-needed active-screen)
    (copy-mode-scroll active-screen 3)))

(defun %mouse-handle-scroll-down (active-screen)
  "Scroll forward, leaving copy mode at the bottom."
  (when active-screen
    (copy-mode-scroll active-screen -3)
    (when (and (screen-copy-mode-p active-screen)
               (zerop (screen-copy-offset active-screen)))
      (copy-mode-exit active-screen))))

(defun %mouse-handle-pane-click (target-pane screen col row count)
  "Focus TARGET-PANE and start a copy-mode selection appropriate to COUNT."
  (%mouse-enter-copy-mode-if-needed screen)
  (multiple-value-bind (pane-col pane-row)
      (%pane-local-coordinates target-pane col row)
    (copy-mode-set-cursor screen pane-row pane-col)
    (cond
      ((= count 2) (copy-mode-select-word screen))
      ((>= count 3) (copy-mode-begin-line-selection screen))
      (t (copy-mode-begin-selection screen)))))

(defun %mouse-handle-left-press (active-window col row now count border-split border-orient)
  "Handle a left-button press in pane or border space."
  (setf *last-mouse-click*
        (list now row col count))
  (when active-window
    (if border-split
        (setf *mouse-drag-state* (list border-split border-orient))
        (let ((target-pane (pane-at-position active-window col row)))
          (when target-pane
            (%select-pane-with-focus active-window target-pane)
            (%mouse-handle-pane-click target-pane (pane-screen target-pane) col row count))))))

(defun %mouse-handle-left-release (active-window active-pane)
  "End a border drag or yank a selection if one is active."
  (if *mouse-drag-state*
      (setf *mouse-drag-state* nil)
      (when (and active-window active-pane)
        (let ((screen (pane-screen active-pane)))
          (when (and (screen-copy-mode-p screen)
                     (screen-copy-selecting screen))
            (copy-mode-yank screen))))))

(defun %mouse-handle-middle-press (active-window col row)
  "Focus the clicked pane and paste the top paste buffer."
  (when active-window
    (let ((target-pane (pane-at-position active-window col row)))
      (when target-pane
        (%select-pane-with-focus active-window target-pane)
        (let ((text (cl-tmux/buffer:get-paste-buffer 0)))
          (when text
            (%paste-to-pane target-pane text)))))))

(defun %mouse-enter-copy-mode-if-needed (screen)
  "Enter copy mode and mark the session when SCREEN is not already in copy mode."
  (when screen
    (unless (screen-copy-mode-p screen)
      (copy-mode-enter screen)
      (setf (screen-copy-mode-entered-by-mouse-p screen) t))))

(defun %mouse-handle-motion (active-window active-pane col row)
  "Resize the active border drag or extend copy-mode selection."
  (if *mouse-drag-state*
      (destructuring-bind (split orient) *mouse-drag-state*
        (when active-window
          (%apply-drag-resize active-window split orient col row)))
      (when (and active-window active-pane)
        (let* ((target-pane (pane-at-position active-window col row))
               (screen (and target-pane (pane-screen target-pane))))
          (when (and screen (screen-copy-mode-p screen) (screen-copy-selecting screen))
            (multiple-value-bind (pane-col pane-row)
                (%pane-local-coordinates target-pane col row)
              (copy-mode-set-cursor screen pane-row pane-col)))))))
