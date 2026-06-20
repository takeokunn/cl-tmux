(in-package #:cl-tmux/commands)

;;; Copy-mode copy-to-buffer helpers.

(defun %copy-row-range-text (screen row from-col to-col)
  "Extract and right-trim text from SCREEN at ROW between FROM-COL and TO-COL."
  (string-right-trim " " (%extract-row-chars screen row from-col to-col)))

(defun %copy-row-range-to-paste-buffer (screen row from-col to-col)
  "Extract characters from SCREEN at ROW, right-trim trailing spaces, and push to the paste buffer."
  (let ((trimmed (%copy-row-range-text screen row from-col to-col)))
    (when (plusp (length trimmed))
      (cl-tmux/buffer:add-paste-buffer trimmed))))

(defmacro define-copy-to-buffer-commands (&rest specs)
  "Generate copy-to-paste-buffer command PAIRS from a declarative
   (name from-col-expr doc) table.  For each spec, generates NAME — which copies,
   clears the selection, and STAYS in copy mode (tmux copy-line / copy-end-of-line
   return WINDOW_COPY_CMD_REDRAW) — and NAME-AND-CANCEL, which copies then EXITS
   copy mode (the -and-cancel variant)."
  `(progn
     ,@(loop for spec in specs
             append
             (destructuring-bind (name from-col-expr doc) spec
               (let ((cancel-name (intern (format nil "~A-AND-CANCEL" name)
                                          (symbol-package name))))
                 `((defun ,name (screen)
                     ,doc
                     (when (screen-copy-mode-p screen)
                       (let* ((row (car (screen-copy-cursor screen)))
                              (col (cdr (screen-copy-cursor screen)))
                              (w   (screen-width screen)))
                         (declare (ignorable col))
                         (%copy-row-range-to-paste-buffer screen row ,from-col-expr w))
                       (copy-mode-clear-selection screen)
                       (setf (screen-dirty-p screen) t)))
                   (defun ,cancel-name (screen)
                     ,(concatenate 'string doc
                                   "  The -and-cancel variant exits copy mode after copying.")
                     (when (screen-copy-mode-p screen)
                       (let* ((row (car (screen-copy-cursor screen)))
                              (col (cdr (screen-copy-cursor screen)))
                              (w   (screen-width screen)))
                         (declare (ignorable col))
                         (%copy-row-range-to-paste-buffer screen row ,from-col-expr w))
                       (copy-mode-exit screen)))))))))

(define-copy-to-buffer-commands
  (copy-mode-copy-end-of-line col "Copy from the current cursor column to end of line; stay in copy mode.")
  (copy-mode-copy-line        0   "Copy the full current line (all columns); stay in copy mode."))
