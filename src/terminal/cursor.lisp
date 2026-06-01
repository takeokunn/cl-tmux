(in-package #:cl-tmux/terminal/actions)

;;;; Cursor movement and character writing.
;;;; Loads AFTER scroll.lisp so cursor-down/scroll can call scroll-up-one
;;;; (defined there) without a forward-reference.

;;; ── Cursor movement ────────────────────────────────────────────────────────
;;;
;;; define-cursor-movements is a Prolog-like table:
;;;   cursor_move(up,    Screen, N) :- cy(Screen) := clamp(cy - N, scroll_top).
;;;   cursor_move(down,  Screen, N) :- cy(Screen) := clamp(cy + N, scroll_bottom).
;;;   cursor_move(right, Screen, N) :- cx(Screen) := clamp(cx + N, width-1).
;;;   cursor_move(left,  Screen, N) :- cx(Screen) := clamp(cx - N, 0).
;;;
;;; Each spec: (name docstring accessor clamped-expression)

(defmacro define-cursor-movements (&rest specs)
  "Build cursor movement functions from a Prolog-like fact table.
   Each SPEC is (name docstring slot-accessor clamped-new-value-expr)."
  `(progn
     ,@(mapcar
        (lambda (spec)
          (destructuring-bind (name docstring accessor limit-expr) spec
            `(defun ,name (screen n)
               ,docstring
               (setf (,accessor screen) ,limit-expr))))
        specs)))

(defun set-cursor (screen x y)
  "Move the cursor to (X, Y), clamping both coordinates into bounds."
  (setf (screen-cx screen) (clamp x 0 (1- (screen-width  screen)))
        (screen-cy screen) (clamp y 0 (1- (screen-height screen)))))

(define-cursor-movements
  (cursor-up    "Move the cursor up N rows, clamping to the scroll-top boundary."
   screen-cy  (max (screen-scroll-top    screen) (- (screen-cy screen) n)))
  (cursor-down  "Move the cursor down N rows, clamping to the scroll-bottom boundary."
   screen-cy  (min (screen-scroll-bottom screen) (+ (screen-cy screen) n)))
  (cursor-right "Move the cursor right N columns, clamping to width-1."
   screen-cx  (min (1- (screen-width     screen)) (+ (screen-cx screen) n)))
  (cursor-left  "Move the cursor left N columns, clamping to column 0."
   screen-cx  (max 0                              (- (screen-cx screen) n))))

(defun cursor-down/scroll (screen)
  "Move cursor down one line, scrolling when at the bottom of the scroll region.
   This is the internal helper used by write-char-at-cursor and the LF handler."
  (if (< (screen-cy screen) (screen-scroll-bottom screen))
      (incf (screen-cy screen))
      (scroll-up-one screen)))

(defun cursor-lf (screen)
  "Line feed: move cursor down, scrolling the scroll region if at the bottom."
  (cursor-down/scroll screen))

(defun cursor-ht (screen)
  "Horizontal tab: advance cursor to the next 8-column tab stop."
  (let ((nx (* 8 (ceiling (1+ (screen-cx screen)) 8))))
    (setf (screen-cx screen)
          (min nx (1- (screen-width screen))))))

(defun cursor-bs (screen)
  "Backspace: move cursor left one column if not already at column 0."
  (when (> (screen-cx screen) 0)
    (decf (screen-cx screen))))

(defun cursor-ri (screen)
  "Reverse index (ESC M): move cursor up one line, scrolling down if at top."
  (if (= (screen-cy screen) (screen-scroll-top screen))
      (scroll-down-one screen)
      (decf (screen-cy screen))))

(defun cursor-cr (screen)
  "Carriage return: move the cursor to column 0."
  (setf (screen-cx screen) 0))

;;; ── Character writing ──────────────────────────────────────────────────────

(defun %advance-cursor (screen n)
  "Advance the cursor N columns, wrapping to the next line (and scrolling the
   scroll region if needed) when it would pass the right edge."
  (let ((nx (+ (screen-cx screen) n)))
    (if (< nx (screen-width screen))
        (setf (screen-cx screen) nx)
        (progn (setf (screen-cx screen) 0)
               (cursor-down/scroll screen)))))

(defun %place-wide-char (screen x y ch fg bg at)
  "Place a double-width character at (X,Y) and write its continuation cell.
   The continuation cell is written only if (1+ X) is within the screen."
  (setf (screen-cell screen x y)
        (make-cell :char ch :fg fg :bg bg :attrs at :width 2))
  (when (< (1+ x) (screen-width screen))
    (setf (screen-cell screen (1+ x) y)
          (make-cell :char #\Space :fg fg :bg bg :attrs at :width 0))))

(defun write-char-at-cursor (screen ch)
  "Write CH at the cursor, then advance.  Double-width (CJK) characters occupy
   a lead cell plus a continuation placeholder and advance the cursor by two;
   a wide char that will not fit at the right edge wraps to the next line first."
  (let ((w  (char-width ch))
        (fg (screen-cur-fg    screen))
        (bg (screen-cur-bg    screen))
        (at (screen-cur-attrs screen)))
    (when (and (= w 2) (>= (1+ (screen-cx screen)) (screen-width screen)))
      (setf (screen-cell screen (screen-cx screen) (screen-cy screen)) (blank-cell))
      (setf (screen-cx screen) 0)
      (cursor-down/scroll screen))
    (let ((x (screen-cx screen))
          (y (screen-cy screen)))
      (if (= w 2)
          (%place-wide-char screen x y ch fg bg at)
          (setf (screen-cell screen x y)
                (make-cell :char ch :fg fg :bg bg :attrs at :width w)))
      (%advance-cursor screen w))))

(defun write-codepoint (screen cp)
  "Write Unicode code point CP at the cursor, converting it via SAFE-CODE-CHAR."
  (write-char-at-cursor screen (safe-code-char cp)))
