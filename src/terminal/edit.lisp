(in-package #:cl-tmux/terminal/actions)

;;;; Character and line editing: insert/delete within lines and rows.
;;;;
;;;; Loads after scroll.lisp (needs %copy-row, %clear-row) and erase.lisp.
;;;; These implement the DCH/ICH/IL/DL VT102 operations.

;;; ── Prolog-style char-edit dispatch macro ──────────────────────────────────
;;;
;;; delete-chars and insert-chars share the same two-loop skeleton:
;;;   1. shift loop  — copy cells in one direction
;;;   2. blank-fill loop — clear the vacated cells
;;;
;;; define-char-edit-rules builds both functions from a declarative table:
;;;   (name docstring shift-start shift-end shift-src blank-start blank-end)
;;;
;;; Prolog-like facts:
;;;   char_edit(delete, Screen, N) :- shift_loop(cx .. w-n-1, src=x+n),
;;;                                   blank_fill_loop(max(cx, w-n) .. w-1).
;;;   char_edit(insert, Screen, N) :- shift_loop(w-1 downto cx+n, src=x-n),
;;;                                   blank_fill_loop(cx .. min(w-1, cx+n-1)).

(defmacro define-char-edit-rules (&rest specs)
  "Generate character-edit functions from a Prolog-like two-loop rule table.
   Each SPEC is (name docstring shift-loop-form blank-loop-form).
   CX, CY, W, N, SCREEN are available in every loop form."
  `(progn
     ,@(mapcar
        (lambda (spec)
          (destructuring-bind (name docstring shift-loop blank-loop) spec
            `(defun ,name (screen n)
               ,docstring
               (let ((cx (screen-cursor-x screen))
                     (cy (screen-cursor-y screen))
                     (w  (screen-width    screen)))
                 (declare (ignorable cx cy w n))
                 ,shift-loop
                 ,blank-loop
                 (setf (screen-dirty-p screen) t)))))
        specs)))

(define-char-edit-rules
  (delete-chars
   "DCH — delete N characters at the cursor, shifting remaining chars left.
    The vacated cells at the end of the line are filled with blanks."
   (loop for x from cx to (- w n 1)
         do (setf (screen-cell screen x cy)
                  (screen-cell screen (+ x n) cy)))
   (loop for x from (max cx (- w n)) to (1- w)
         do (setf (screen-cell screen x cy) (blank-cell))))

  (insert-chars
   "ICH — insert N blank characters at the cursor, pushing existing chars right.
    Characters shifted past the right margin are lost."
   (loop for x from (1- w) downto (+ cx n)
         do (setf (screen-cell screen x cy)
                  (screen-cell screen (- x n) cy)))
   (loop for x from cx to (min (1- w) (+ cx n -1))
         do (setf (screen-cell screen x cy) (blank-cell)))))

;;; ── Prolog-style line-edit dispatch macro ──────────────────────────────────
;;;
;;; insert-lines and delete-lines share the same skeleton:
;;;   scroll-region guard + count-clamp + shift loop + blank-fill loop.
;;;
;;; define-line-edit-rules builds both from a declarative table:
;;;   (name docstring shift-loop-form blank-loop-form)
;;;
;;; Prolog-like facts:
;;;   line_edit(insert, Screen, N) :- guard(top <= bottom),
;;;     count = min(N, bottom - top + 1),
;;;     shift_loop(bottom downto top+count, src=row-count),
;;;     blank_fill_loop(top .. top+count-1).
;;;   line_edit(delete, Screen, N) :- guard(top <= bottom),
;;;     count = min(N, bottom - top + 1),
;;;     shift_loop(top .. bottom-count, src=row+count),
;;;     blank_fill_loop(bottom-count+1 .. bottom).

(defmacro define-line-edit-rules (&rest specs)
  "Generate line-edit functions from a Prolog-like guard+clamp+two-loop table.
   Each SPEC is (name docstring shift-loop-form blank-loop-form).
   TOP, BOTTOM, COUNT, N, SCREEN are available in every loop form."
  `(progn
     ,@(mapcar
        (lambda (spec)
          (destructuring-bind (name docstring shift-loop blank-loop) spec
            `(defun ,name (screen n)
               ,docstring
               (let* ((top    (screen-cursor-y      screen))
                      (bottom (screen-scroll-bottom screen)))
                 (when (<= top bottom)
                   (let ((count (min n (- bottom top -1))))
                     ,shift-loop
                     ,blank-loop
                     (setf (screen-dirty-p screen) t)))))))
        specs)))

(define-line-edit-rules
  (insert-lines
   "IL — insert N blank lines at the cursor row, pushing lower lines down within
    [cursor-row, scroll-bottom].  Lines pushed past the bottom are discarded."
   (loop for row from bottom downto (+ top count)
         do (%copy-row screen row (- row count)))
   (loop for row from top to (+ top count -1)
         do (%clear-row screen row)))

  (delete-lines
   "DL — delete N lines at the cursor row, pulling lower lines up within
    [cursor-row, scroll-bottom].  Lines exposed at the bottom become blank."
   (loop for row from top to (- bottom count)
         do (%copy-row screen row (+ row count)))
   (loop for row from (- bottom count -1) to bottom
         do (%clear-row screen row))))
