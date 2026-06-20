;;; events-mouse-status.lisp --- Status bar mouse handling -*- Lisp -*-

(in-package #:cl-tmux)

;;; ── Status bar column → window index mapping ─────────────────────────────────

(defun %status-col-to-window (session col)
  "Return the window at column COL of the status bar, or NIL.
   Mirrors the layout produced by %status-window-list-styled, including the
   per-window format, separator, and inline style blocks."
  (labels ((window-entry-width (window)
             (let* ((active-p (eq window (session-active-window session)))
                    (context  (cl-tmux/format:format-context-from-window session window))
                    (fmt      (cl-tmux/options:get-option-for-context
                               (if active-p "window-status-current-format"
                                   "window-status-format")
                               :window window))
                    (label    (cl-tmux/format:expand-format fmt context))
                    (style    (cl-tmux/renderer::%window-status-style session window active-p))
                    (sgr-code (when (and style (plusp (length style)))
                                (cl-tmux/renderer::%status-sgr-from-style style)))
                    (expanded (cl-tmux/renderer::%status-expand-style-blocks
                               label
                               (or sgr-code cl-tmux/renderer::+sgr-default-status+))))
               (cl-tmux/renderer::%visible-length expanded))))
    (let ((current-col (+ 1 (length (session-name session))))
          (separator-width (cl-tmux/renderer::%visible-length
                            (cl-tmux/options:get-option "window-status-separator" " ")))
          (first-p t))
      (loop for window in (session-windows session)
            do (unless first-p
                 (incf current-col separator-width))
               (setf first-p nil)
               (let ((entry-len (window-entry-width window)))
                 (when (and (>= col current-col)
                            (< col (+ current-col entry-len)))
                   (return window))
                 (incf current-col entry-len))))))

(defun %mouse-status-bar-click (session col)
  "Handle a click at COL on the status bar row: select the clicked window."
  (let ((window (%status-col-to-window session col)))
    (when window
      (%with-window-focus-transition (session)
        (session-select-window session window)))))
