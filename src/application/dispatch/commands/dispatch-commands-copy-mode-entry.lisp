(in-package #:cl-tmux)

(declaim (special *current-mouse-event*))

;;; -- Copy-mode entry %cmd-* handler ------------------------------------------

(defun %resolve-copy-mode-screen (session target-str)
  "Return the screen copy-mode should act on: target pane screen or active screen."
  (if target-str
      (with-target-context (tsession twin tpane session target-str)
        (declare (ignore tsession twin))
        (and tpane (pane-screen tpane)))
      (%active-screen session)))

(defun %copy-mode-mouse-entry (session screen flags)
  "copy-mode -M: place the copy cursor at the current mouse position."
  (when (and (%flag-present-p flags #\M) *current-mouse-event*)
    (let* ((event *current-mouse-event*)
           (col   (getf event :col))
           (row   (getf event :row))
           (win   (session-active-window session))
           (pane  (and win col row (pane-at-position win col row))))
      (when (and pane (eq (pane-screen pane) screen))
        (setf (screen-copy-cursor screen)
              (cons (min (max 0 (- row (pane-y pane)))
                         (1- (screen-height screen)))
                    (min (max 0 (- col (pane-x pane)))
                         (1- (screen-width screen)))))
        (copy-mode-begin-selection screen)))))

(defun %cmd-copy-mode-arg (session args)
  "copy-mode [-eHMqu] [-s src-pane] [-t target-pane]: enter or leave copy mode."
  (with-command-input (flags positionals args "ts"
                             :allowed-flags '(#\u #\e #\q #\t #\s #\M #\H)
                             :max-positionals 0
                             :message "copy-mode: unsupported argument")
    (let ((screen (%resolve-copy-mode-screen session
                                             (or (%flag-value flags #\s)
                                                 (%flag-value flags #\t)))))
      (when screen
        (if (%flag-present-p flags #\q)
            (when (screen-copy-mode-p screen)
              (copy-mode-exit screen)
              (setf *dirty* t))
            (progn
              (copy-mode-enter screen
                               :scroll-to-top (%copy-mode-scroll-to-top-p flags)
                               :exit-on-bottom (%copy-mode-exit-on-bottom-p flags))
              (setf (cl-tmux/terminal/types:screen-copy-hide-position screen)
                    (and (%flag-present-p flags #\H) t))
              (%copy-mode-mouse-entry session screen flags)
              (setf *dirty* t)))))))
