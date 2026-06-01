(in-package #:cl-tmux/terminal/actions)

;;;; Character and line editing: insert/delete within lines and rows.
;;;;
;;;; Loads after scroll.lisp (needs %copy-row, %clear-row) and erase.lisp.
;;;; These implement the DCH/ICH/IL/DL VT102 operations.

;;; ── Character-level editing (on the current line) ───────────────────────────

(defun delete-chars (screen n)
  "DCH — delete N characters at the cursor, shifting remaining chars left.
   The vacated cells at the end of the line are filled with blanks."
  (let ((cx (screen-cx screen))
        (cy (screen-cy screen))
        (w  (screen-width screen)))
    (loop for x from cx to (- w n 1)
          do (setf (screen-cell screen x cy)
                   (screen-cell screen (+ x n) cy)))
    (loop for x from (max cx (- w n)) to (1- w)
          do (setf (screen-cell screen x cy) (blank-cell)))))

(defun insert-chars (screen n)
  "ICH — insert N blank characters at the cursor, pushing existing chars right.
   Characters shifted past the right margin are lost."
  (let ((cx (screen-cx screen))
        (cy (screen-cy screen))
        (w  (screen-width screen)))
    (loop for x from (1- w) downto (+ cx n)
          do (setf (screen-cell screen x cy)
                   (screen-cell screen (- x n) cy)))
    (loop for x from cx to (min (1- w) (+ cx n -1))
          do (setf (screen-cell screen x cy) (blank-cell)))))

;;; ── Line-level editing (within the scroll region) ───────────────────────────

(defun insert-lines (screen n)
  "IL — insert N blank lines at the cursor row, pushing lower lines down within
   [cursor-row, scroll-bottom].  Lines pushed past the bottom are discarded."
  (let* ((top    (screen-cy screen))
         (bottom (screen-scroll-bottom screen)))
    (when (<= top bottom)
      (let ((count (min n (- bottom top -1))))
        (loop for row from bottom downto (+ top count)
              do (%copy-row screen row (- row count)))
        (loop for row from top to (+ top count -1)
              do (%clear-row screen row))))))

(defun delete-lines (screen n)
  "DL — delete N lines at the cursor row, pulling lower lines up within
   [cursor-row, scroll-bottom].  Lines exposed at the bottom become blank."
  (let* ((top    (screen-cy screen))
         (bottom (screen-scroll-bottom screen)))
    (when (<= top bottom)
      (let ((count (min n (- bottom top -1))))
        (loop for row from top to (- bottom count)
              do (%copy-row screen row (+ row count)))
        (loop for row from (- bottom count -1) to bottom
              do (%clear-row screen row))))))
