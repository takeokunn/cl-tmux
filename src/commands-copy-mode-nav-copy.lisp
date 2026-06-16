(in-package #:cl-tmux/commands)

;;; Copy-mode copy-to-buffer helpers.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((source (or *load-truename* *compile-file-truename*))
         (path (and source
                    (merge-pathnames #P"commands-copy-mode-selection.lisp"
                                     (make-pathname :name nil
                                                    :type nil
                                                    :defaults source)))))
    (when (probe-file path)
      (load path))))

(defun %copy-row-range-text (screen row from-col to-col)
  "Extract and right-trim text from SCREEN at ROW between FROM-COL and TO-COL."
  (string-right-trim " " (%extract-row-chars screen row from-col to-col)))

(defun %copy-row-range-to-paste-buffer (screen row from-col to-col)
  "Extract characters from SCREEN at ROW, right-trim trailing spaces, and push to the paste buffer."
  (let ((trimmed (%copy-row-range-text screen row from-col to-col)))
    (when (plusp (length trimmed))
      (cl-tmux/buffer:add-paste-buffer trimmed))))

(defmacro define-copy-to-buffer-commands (&rest specs)
  "Generate copy-to-paste-buffer functions from a declarative (name from-col-expr doc) table."
  `(progn
     ,@(mapcar (lambda (spec)
                 (destructuring-bind (name from-col-expr doc) spec
                   `(defun ,name (screen)
                      ,doc
                      (when (screen-copy-mode-p screen)
                        (let* ((row (car (screen-copy-cursor screen)))
                               (col (cdr (screen-copy-cursor screen)))
                               (w   (screen-width screen)))
                          (declare (ignorable col))
                          (%copy-row-range-to-paste-buffer screen row ,from-col-expr w))
                        (copy-mode-exit screen)))))
               specs)))

(define-copy-to-buffer-commands
  (copy-mode-copy-end-of-line col "Copy from the current cursor column to end of line, then exit copy mode.")
  (copy-mode-copy-line        0   "Copy the full current line (all columns), then exit copy mode."))
