(in-package #:cl-tmux/commands)

;;; Copy-mode selection entrypoints.

(defun copy-mode-begin-line-selection (screen)
  "Begin a full-line selection at the current row (tmux V binding).
   Sets copy-line-selection-p and activates the selection."
  (with-copy-mode-dirty screen
    (let* ((cur    (or (screen-copy-cursor screen) (cons 0 0)))
           (row    (car cur))
           ;; Mark at col 0, cursor at col width-1 to select full row.
           (mark   (cons row 0))
           (cursor (cons row (1- (screen-width screen)))))
      (setf (screen-copy-mark             screen) mark
            (screen-copy-mark-offset      screen) (screen-copy-offset screen)
            (screen-copy-cursor           screen) cursor
            (screen-copy-selecting        screen) t
            (screen-copy-line-selection-p screen) t))))

