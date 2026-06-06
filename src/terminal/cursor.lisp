(in-package #:cl-tmux/terminal/actions)

;;;; Cursor movement and character writing.
;;;; Loads AFTER scroll.lisp so cursor-down/scroll can call scroll-up-one
;;;; (defined there) without a forward-reference.

;;; ── Cursor movement ────────────────────────────────────────────────────────
;;;
;;; define-cursor-movements is a Prolog-like table:
;;;   cursor_move(up,    Screen, N) :- cursor-y(Screen) := clamp(cursor-y - N, scroll_top).
;;;   cursor_move(down,  Screen, N) :- cursor-y(Screen) := clamp(cursor-y + N, scroll_bottom).
;;;   cursor_move(right, Screen, N) :- cursor-x(Screen) := clamp(cursor-x + N, width-1).
;;;   cursor_move(left,  Screen, N) :- cursor-x(Screen) := clamp(cursor-x - N, 0).
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
  (setf (screen-cursor-x screen) (clamp x 0 (1- (screen-width  screen)))
        (screen-cursor-y screen) (clamp y 0 (1- (screen-height screen)))))

(define-cursor-movements
  (cursor-up    "Move the cursor up N rows, clamping to the scroll-top boundary."
   screen-cursor-y  (max (screen-scroll-top    screen) (- (screen-cursor-y screen) n)))
  (cursor-down  "Move the cursor down N rows, clamping to the scroll-bottom boundary."
   screen-cursor-y  (min (screen-scroll-bottom screen) (+ (screen-cursor-y screen) n)))
  (cursor-right "Move the cursor right N columns, clamping to width-1."
   screen-cursor-x  (min (1- (screen-width     screen)) (+ (screen-cursor-x screen) n)))
  (cursor-left  "Move the cursor left N columns, clamping to column 0."
   screen-cursor-x  (max 0                              (- (screen-cursor-x screen) n))))

(defun cursor-down/scroll (screen)
  "Move cursor down one line, scrolling when at the bottom of the scroll region.
   This is the internal helper used by write-char-at-cursor and the LF handler."
  (if (< (screen-cursor-y screen) (screen-scroll-bottom screen))
      (incf (screen-cursor-y screen))
      (scroll-up-one screen)))

(defun cursor-lf (screen)
  "Line feed: move cursor down, scrolling the scroll region if at the bottom."
  (cursor-down/scroll screen))

(defun %materialize-tab-stops (screen)
  "Return SCREEN's tab stops as a concrete sorted list of columns, expanding the
   :DEFAULT sentinel into the standard every-8-columns stops for the width."
  (let ((stops (screen-tab-stops screen)))
    (if (eq stops :default)
        (loop for c from 8 below (screen-width screen) by 8 collect c)
        stops)))

(defun set-tab-stop (screen)
  "HTS (ESC H) — set a horizontal tab stop at the current cursor column."
  (setf (screen-tab-stops screen)
        (sort (adjoin (screen-cursor-x screen) (%materialize-tab-stops screen)) #'<)))

(defun clear-tab-stops (screen mode)
  "TBC (CSI N g) — clear tab stops.  MODE 3 clears ALL stops; any other value
   (including 0) clears the stop at the current cursor column."
  (setf (screen-tab-stops screen)
        (if (= mode 3)
            '()
            (remove (screen-cursor-x screen) (%materialize-tab-stops screen)))))

(defun cursor-ht (screen)
  "Horizontal tab: advance the cursor to the next tab stop (default: every 8
   columns; HTS/TBC can customise the stops), clamping to the last column."
  (let ((stops (screen-tab-stops screen))
        (x     (screen-cursor-x screen))
        (max-x (1- (screen-width screen))))
    (setf (screen-cursor-x screen)
          (if (eq stops :default)
              (min (* 8 (ceiling (1+ x) 8)) max-x)
              (or (find-if (lambda (c) (> c x)) (sort (copy-list stops) #'<))
                  max-x)))))

(defun cursor-cht (screen n)
  "CHT — cursor forward N tab stops (CSI N I).
   Advance the cursor to the Nth next tab stop, clamping to width-1."
  (dotimes (_ (max 1 n))
    (cursor-ht screen)))

(defun cursor-cbt (screen n)
  "CBT — cursor backward N tab stops (CSI N Z).
   Move the cursor back to the Nth previous tab stop, stopping at column 0."
  (dotimes (_ (max 1 n))
    (let ((stops (screen-tab-stops screen))
          (x     (screen-cursor-x screen)))
      (setf (screen-cursor-x screen)
            (if (eq stops :default)
                (* 8 (floor (max 0 (1- x)) 8))
                (or (find-if (lambda (c) (< c x)) (sort (copy-list stops) #'>))
                    0))))))

(defun cursor-bs (screen)
  "Backspace: move cursor left one column if not already at column 0."
  (when (> (screen-cursor-x screen) 0)
    (decf (screen-cursor-x screen))))

(defun cursor-ri (screen)
  "Reverse index (ESC M): move cursor up one line, scrolling down if at top."
  (if (= (screen-cursor-y screen) (screen-scroll-top screen))
      (scroll-down-one screen)
      (decf (screen-cursor-y screen))))

(defun cursor-cr (screen)
  "Carriage return: move the cursor to column 0."
  (setf (screen-cursor-x screen) 0))

(defun cursor-nel (screen)
  "NEL (ESC E) — Next Line: carriage return then line feed, i.e. move the cursor
   to column 0 of the next row (scrolling at the bottom margin like LF)."
  (cursor-cr screen)
  (cursor-lf screen))

;;; ── Character writing ──────────────────────────────────────────────────────

(declaim (inline %mark-dirty))
(defun %mark-dirty (screen)
  "Mark SCREEN dirty: signal the renderer that cells have changed."
  (setf (screen-dirty-p screen) t))

(defun %advance-cursor (screen n)
  "Advance the cursor N columns, wrapping to the next line (and scrolling the
   scroll region if needed) when it would pass the right edge.
   Respects the screen-autowrap flag: when autowrap is NIL and the cursor is at
   the right edge, the cursor stays in place (the write overwrites the last cell)."
  (let ((next-x (+ (screen-cursor-x screen) n)))
    (cond
      ;; Advance: fits within the current row.
      ((< next-x (screen-width screen))
       (setf (screen-cursor-x screen) next-x))
      ;; Wrap: reached the right margin and autowrap is on.
      ((screen-autowrap screen)
       (setf (screen-cursor-x screen) 0)
       (cursor-down/scroll screen))
      ;; Clamp: reached the right margin and autowrap is off.
      (t
       (setf (screen-cursor-x screen) (1- (screen-width screen)))))))

(defun %place-wide-char (screen x y char fg bg attrs attrs2 ul-color)
  "Place a double-width character at (X,Y) and write its continuation cell.
   The continuation cell is written only if (1+ X) is within the screen."
  (setf (screen-cell screen x y)
        (make-cell :char char :fg fg :bg bg :attrs attrs :attrs2 attrs2
                   :ul-color ul-color :width 2))
  (when (< (1+ x) (screen-width screen))
    (setf (screen-cell screen (1+ x) y)
          (make-cell :char #\Space :fg fg :bg bg :attrs attrs :attrs2 attrs2
                     :ul-color ul-color :width 0))))

;;; DEC special graphics character set (G1; activated by ESC ( 0).
;;; Maps ASCII code points (in the range used by line-drawing apps) to the
;;; corresponding Unicode box-drawing characters.
;;;
;;; Prolog-like fact table — each entry is one character mapping:
;;;   dec_graphics(j, '┘').  dec_graphics(k, '┐').  ...
;;; The define-dec-graphics-table macro builds the case form from this table.

(defmacro define-dec-graphics-table (&rest mappings)
  "Generate %DEC-GRAPHICS-CHAR from a declarative character-mapping table.
   Each MAPPING is (ascii-char unicode-char description) where description is
   a compile-time annotation only."
  `(defun %dec-graphics-char (ch)
     "Remap CH from the DEC special graphics set to the corresponding Unicode
      box-drawing character.  Returns CH unchanged for unmapped code points."
     (case ch
       ,@(mapcar (lambda (m) `(,(first m) ,(second m))) mappings)
       (t ch))))

(define-dec-graphics-table
  ;; Box-drawing corners
  (#\j #\┘ "lower-right corner")
  (#\k #\┐ "upper-right corner")
  (#\l #\┌ "upper-left corner")
  (#\m #\└ "lower-left corner")
  ;; Box-drawing junctions
  (#\n #\┼ "crossing")
  (#\t #\├ "left tee")
  (#\u #\┤ "right tee")
  (#\v #\┴ "bottom tee")
  (#\w #\┬ "top tee")
  ;; Lines
  (#\q #\─ "horizontal line")
  (#\x #\│ "vertical line")
  ;; Special characters
  (#\a #\▒ "checkerboard")
  (#\` #\◆ "diamond")
  (#\f #\° "degree symbol")
  (#\g #\± "plus-minus")
  ;; Dash variants — all map to horizontal line
  (#\o #\─ "top horizontal dash")
  (#\p #\─ "upper horizontal dash")
  (#\r #\─ "lower horizontal dash")
  (#\s #\─ "bottom horizontal dash"))

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

(defun %append-combining-char (screen ch)
  "Append combining character CH to the cell immediately left of the cursor.
   The cursor is NOT advanced.  If the cursor is at column 0, the combining
   char is appended to column 0 (the only cell available on that row)."
  (let* ((prev-x    (if (> (screen-cursor-x screen) 0) (1- (screen-cursor-x screen)) 0))
         (prev-y    (screen-cursor-y screen))
         (prev-cell (screen-cell screen prev-x prev-y)))
    (setf (screen-cell screen prev-x prev-y)
          (make-cell :char      (cell-char     prev-cell)
                     :fg        (cell-fg       prev-cell)
                     :bg        (cell-bg       prev-cell)
                     :attrs     (cell-attrs    prev-cell)
                     :attrs2    (cell-attrs2   prev-cell)
                     :ul-color  (cell-ul-color prev-cell)
                     :combining (append (cell-combining prev-cell) (list ch))
                     :width     (cell-width    prev-cell)))
    (%mark-dirty screen)))

(defun %remap-charset-char (screen ch)
  "When SCREEN's charset is :dec-graphics, remap CH through the DEC special
   graphics table; otherwise return CH unchanged."
  (if (eq (screen-charset screen) :dec-graphics)
      (%dec-graphics-char ch)
      ch))

(defun %write-wide-cell (screen ch)
  "Write double-width character CH at the cursor.
   Wraps to the next row first if the character does not fit in the last column.
   Advances the cursor by 2 after placing the lead + continuation cells."
  (let ((fg       (screen-cur-fg       screen))
        (bg       (screen-cur-bg       screen))
        (attrs    (screen-cur-attrs    screen))
        (attrs2   (screen-cur-attrs2   screen))
        (ul-color (screen-cur-ul-color screen)))
    ;; If the wide char cannot fit (only one column remains), blank the last
    ;; column and wrap to the next row before placing it.
    (when (>= (1+ (screen-cursor-x screen)) (screen-width screen))
      (setf (screen-cell screen (screen-cursor-x screen) (screen-cursor-y screen)) (blank-cell))
      (setf (screen-cursor-x screen) 0)
      (cursor-down/scroll screen))
    (%place-wide-char screen (screen-cursor-x screen) (screen-cursor-y screen)
                      ch fg bg attrs attrs2 ul-color)
    (%mark-dirty screen)
    (%advance-cursor screen 2)))

(defun %write-normal-cell (screen ch)
  "Write single-width character CH at the cursor and advance by 1."
  (let ((x        (screen-cursor-x    screen))
        (y        (screen-cursor-y    screen))
        (fg       (screen-cur-fg      screen))
        (bg       (screen-cur-bg      screen))
        (attrs    (screen-cur-attrs   screen))
        (attrs2   (screen-cur-attrs2  screen))
        (ul-color (screen-cur-ul-color screen)))
    (setf (screen-cell screen x y)
          (make-cell :char ch :fg fg :bg bg :attrs attrs :attrs2 attrs2
                     :ul-color ul-color :width 1))
    (%mark-dirty screen)
    (%advance-cursor screen 1)))

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
    (%append-combining-char screen ch)
    (return-from write-char-at-cursor))
  ;; Apply DEC special graphics remapping when active.
  (setf ch (%remap-charset-char screen ch))
  (setf (screen-last-char screen) ch)
  (if (= (char-width ch) 2)
      (%write-wide-cell   screen ch)
      (%write-normal-cell screen ch)))

(defun write-codepoint (screen cp)
  "Write Unicode code point CP at the cursor, converting it via SAFE-CODE-CHAR."
  (write-char-at-cursor screen (safe-code-char cp)))
