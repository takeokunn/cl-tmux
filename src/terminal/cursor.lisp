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

(defun cursor-cht (screen n)
  "CHT — cursor forward N tab stops (CSI N I).
   Advance the cursor to the Nth next 8-column tab stop, clamping to width-1."
  (dotimes (_ (max 1 n))
    (cursor-ht screen)))

(defun cursor-cbt (screen n)
  "CBT — cursor backward N tab stops (CSI N Z).
   Move the cursor back to the Nth previous 8-column tab stop, stopping at column 0."
  (let ((stops (max 1 n)))
    (dotimes (_ stops)
      (let ((prev (* 8 (floor (max 0 (1- (screen-cx screen))) 8))))
        (setf (screen-cx screen) prev)))))

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
   scroll region if needed) when it would pass the right edge.
   Respects the screen-autowrap flag: when autowrap is NIL and the cursor is at
   the right edge, the cursor stays in place (the write overwrites the last cell)."
  (let ((nx (+ (screen-cx screen) n)))
    (if (< nx (screen-width screen))
        (setf (screen-cx screen) nx)
        (if (screen-autowrap screen)
            (progn (setf (screen-cx screen) 0)
                   (cursor-down/scroll screen))
            ;; No-wrap: clamp to last column
            (setf (screen-cx screen) (1- (screen-width screen)))))))

(defun %place-wide-char (screen x y ch fg bg at at2 ulc)
  "Place a double-width character at (X,Y) and write its continuation cell.
   The continuation cell is written only if (1+ X) is within the screen."
  (setf (screen-cell screen x y)
        (make-cell :char ch :fg fg :bg bg :attrs at :attrs2 at2 :ul-color ulc :width 2))
  (when (< (1+ x) (screen-width screen))
    (setf (screen-cell screen (1+ x) y)
          (make-cell :char #\Space :fg fg :bg bg :attrs at :attrs2 at2 :ul-color ulc :width 0))))

;;; DEC special graphics character set (G1; activated by ESC ( 0).
;;; Maps ASCII code points (in the range used by line-drawing apps) to the
;;; corresponding Unicode box-drawing characters.

(defun %dec-graphics-char (ch)
  "Remap CH from the DEC special graphics set to the corresponding Unicode
   box-drawing character.  Returns CH unchanged for unmapped code points."
  (case ch
    (#\j #\┘) (#\k #\┐) (#\l #\┌) (#\m #\└) (#\n #\┼)
    (#\q #\─) (#\t #\├) (#\u #\┤) (#\v #\┴) (#\w #\┬) (#\x #\│)
    (#\a #\▒) (#\` #\◆) (#\f #\°) (#\g #\±) (#\o #\─) ; tilde
    (#\p #\─) (#\r #\─) (#\s #\─) ; various dashes
    (t ch)))

;;; Unicode combining character ranges (Category M*: combining marks).
;;; These code points have zero display width and should be appended to the
;;; previous cell rather than placed in a new cell.

(defun combining-char-p (ch)
  "Return T if CH is a Unicode combining character (zero-width mark)."
  (let ((cp (char-code ch)))
    (or (<= #x0300 cp #x036F)   ; Combining Diacritical Marks
        (<= #x1AB0 cp #x1AFF)   ; Combining Diacritical Marks Extended
        (<= #x1DC0 cp #x1DFF)   ; Combining Diacritical Marks Supplement
        (<= #x20D0 cp #x20FF)   ; Combining Diacritical Marks for Symbols
        (<= #xFE20 cp #xFE2F)))) ; Combining Half Marks

(defun write-char-at-cursor (screen ch)
  "Write CH at the cursor, then advance.  Double-width (CJK) characters occupy
   a lead cell plus a continuation placeholder and advance the cursor by two;
   a wide char that will not fit at the right edge wraps to the next line first.
   Records CH as the screen's LAST-CHAR for use by CSI REP sequences.

   When CH is a Unicode combining character, it is appended to the previous cell
   and the cursor is NOT advanced.

   When the screen's charset is :dec-graphics, CH is remapped through the DEC
   special graphics table before being written."
  ;; Combining character: append to the previous cell, no cursor advance.
  (when (combining-char-p ch)
    (let* ((prev-x (if (> (screen-cx screen) 0) (1- (screen-cx screen)) 0))
           (prev-y (screen-cy screen))
           (prev-cell (screen-cell screen prev-x prev-y)))
      (setf (screen-cell screen prev-x prev-y)
            (make-cell :char      (cell-char prev-cell)
                       :fg        (cell-fg prev-cell)
                       :bg        (cell-bg prev-cell)
                       :attrs     (cell-attrs prev-cell)
                       :attrs2    (cell-attrs2 prev-cell)
                       :ul-color  (cell-ul-color prev-cell)
                       :combining (append (cell-combining prev-cell) (list ch))
                       :width     (cell-width prev-cell))))
    (return-from write-char-at-cursor))
  ;; Apply DEC special graphics remapping when active.
  (when (eq (screen-charset screen) :dec-graphics)
    (setf ch (%dec-graphics-char ch)))
  (let ((w   (char-width ch))
        (fg  (screen-cur-fg       screen))
        (bg  (screen-cur-bg       screen))
        (at  (screen-cur-attrs    screen))
        (at2 (screen-cur-attrs2   screen))
        (ulc (screen-cur-ul-color screen)))
    (when (and (= w 2) (>= (1+ (screen-cx screen)) (screen-width screen)))
      (setf (screen-cell screen (screen-cx screen) (screen-cy screen)) (blank-cell))
      (setf (screen-cx screen) 0)
      (cursor-down/scroll screen))
    (let ((x (screen-cx screen))
          (y (screen-cy screen)))
      (if (= w 2)
          (%place-wide-char screen x y ch fg bg at at2 ulc)
          (setf (screen-cell screen x y)
                (make-cell :char ch :fg fg :bg bg :attrs at :attrs2 at2 :ul-color ulc :width w)))
      (setf (screen-last-char screen) ch)
      (%advance-cursor screen w))))

(defun write-codepoint (screen cp)
  "Write Unicode code point CP at the cursor, converting it via SAFE-CODE-CHAR."
  (write-char-at-cursor screen (safe-code-char cp)))
