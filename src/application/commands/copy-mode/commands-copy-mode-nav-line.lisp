(in-package #:cl-tmux/commands)

;;; Copy-mode line / screen navigation.

(defmacro define-line-jump-commands (&rest specs)
  "Generate copy-mode column-jump functions from a declarative (name doc col-expr) table.
   COL-EXPR may reference SCREEN and the current ROW."
  `(progn
     ,@(mapcar (lambda (spec)
                 (destructuring-bind (name doc col-expr) spec
                   `(defun ,name (screen)
                      ,doc
                      (when (screen-copy-mode-p screen)
                        (let ((row (car (screen-copy-cursor screen))))
                          (declare (ignorable row))
                          (setf (screen-copy-cursor screen)
                                (cons row ,col-expr)
                                (screen-dirty-p screen) t))))))
               specs)))

(define-line-jump-commands
  (copy-mode-line-start
   "Move cursor to column 0 of the current row (vi 0)."
   0)
  (copy-mode-back-to-indentation
   "Move cursor to the first non-blank character of the current row (vi ^).
    Distinct from line-start: on an indented line ^ stops at the indent.
    Falls back to column 0 when the row is entirely blank."
   (or (position-if-not #'%space-separator-p (%copy-mode-row-chars screen row)) 0)))

(defun copy-mode-line-end (screen)
  "Move cursor to the last non-blank column of the current row (vi $).  Matches
   tmux's cursor-end-of-line, which stops at the end of the line CONTENT, not the
   screen edge; an entirely blank row goes to column 0.  In rectangle-select mode
   the cursor goes to the last screen column instead, since the rectangle extends
   to the pane edge."
  (when (screen-copy-mode-p screen)
    (let* ((row (car (screen-copy-cursor screen)))
           (col (if (screen-copy-rect-select-p screen)
                    (1- (screen-width screen))
                    (or (position-if-not #'%space-separator-p
                                         (%copy-mode-row-chars screen row)
                                         :from-end t)
                        0))))
      (setf (screen-copy-cursor screen) (cons row col)
            (screen-dirty-p screen) t))))

(defmacro define-copy-mode-cursor-jump (&rest rules)
  "Generate one copy-mode cursor-jump defun per rule.
   Each RULE is (function-name docstring row-expression).
   The generated function jumps the cursor to ROW-EXPR, keeping the current
   cursor column unchanged, and marks the screen dirty."
  `(progn
     ,@(mapcar (lambda (rule)
                 (destructuring-bind (name docstring row-expr) rule
                   `(defun ,name (screen)
                      ,docstring
                      (when (screen-copy-mode-p screen)
                        (let ((new-row ,row-expr)
                              (cur-col (cdr (screen-copy-cursor screen))))
                          (setf (screen-copy-cursor screen) (cons new-row cur-col)
                                (screen-dirty-p screen) t))))))
               rules)))

(defmacro define-copy-mode-scroll-commands (&rest rules)
  "Generate one copy-mode scroll-wrapper defun per rule.
   Each RULE is (function-name docstring delta-expression).
   The generated function guards on copy-mode-p and delegates to copy-mode-scroll."
  `(progn
     ,@(mapcar (lambda (rule)
                 (destructuring-bind (name docstring delta-expr) rule
                   `(defun ,name (screen)
                      ,docstring
                      (when (screen-copy-mode-p screen)
                        (copy-mode-scroll screen ,delta-expr)))))
               rules)))

(define-copy-mode-cursor-jump
  (copy-mode-high
   "Move cursor to row 0 (top of viewport), keeping column."
   0)
  (copy-mode-middle
   "Move cursor to the middle row of the viewport, keeping column."
   (floor (screen-height screen) 2))
  (copy-mode-low
   "Move cursor to the last row of the viewport (height-1), keeping column."
   (1- (screen-height screen))))

(defun %copy-mode-centre-cursor (screen row col)
  "Move the copy-mode cursor to ROW, COL while preserving the copy-mode guard."
  (when (screen-copy-mode-p screen)
    (copy-mode-set-cursor screen row col)))

(defun copy-mode-cursor-centre-vertical (screen)
  "Move cursor to the vertical centre row of the viewport, keeping column."
  (%copy-mode-centre-cursor screen
                            (floor (screen-height screen) 2)
                            (cdr (screen-copy-cursor screen))))

(defun copy-mode-cursor-centre-horizontal (screen)
  "Move cursor to the horizontal centre column of the viewport, keeping row."
  (%copy-mode-centre-cursor screen
                            (car (screen-copy-cursor screen))
                            (floor (screen-width screen) 2)))

(define-copy-mode-scroll-commands
  (copy-mode-page-up
   "Scroll the viewport back by one full screen height."
   (screen-height screen))
  (copy-mode-page-down
   "Scroll the viewport forward by one full screen height."
   (- (screen-height screen)))
  (copy-mode-half-page-up
   "Scroll the viewport back by half a screen height."
   (floor (screen-height screen) 2))
  (copy-mode-half-page-down
   "Scroll the viewport forward by half a screen height."
   (- (floor (screen-height screen) 2)))
  (copy-mode-scroll-up-line
   "Scroll the viewport back by 1 line (cursor stays fixed when possible)."
   1)
  (copy-mode-scroll-down-line
   "Scroll the viewport forward by 1 line (cursor stays fixed when possible)."
   -1)
  (copy-mode-top
   "Jump to the oldest scrollback line (maximum scroll-back offset)."
   +scroll-to-oldest+)
  (copy-mode-bottom
   "Jump to the live view bottom (scroll-offset = 0)."
   +scroll-to-newest+))

(defun copy-mode-scroll-middle (screen)
  "Scroll the viewport so the cursor row is centered (tmux copy-mode-vi z).
   Adjusts the copy-offset so the current cursor row appears at the middle of
   the viewport, then moves the cursor row to that center row.  If the history
   limit prevents full centering the offset is clamped and the cursor is placed
   at the achievable nearest-center row."
  (with-copy-mode-dirty screen
    (let* ((row     (car (screen-copy-cursor screen)))
           (h       (screen-height screen))
           (sb-n    (length (screen-scrollback screen)))
           (center  (floor h 2))
           (offset  (screen-copy-offset screen))
           (new-off (max 0 (min sb-n (+ offset (- center row)))))
           (delta   (- new-off offset))
           (new-row (max 0 (min (1- h) (+ row delta)))))
      (setf (screen-copy-offset screen) new-off
            (screen-copy-cursor screen) (cons new-row (cdr (screen-copy-cursor screen)))))))
