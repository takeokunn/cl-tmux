(in-package #:cl-tmux/terminal/actions)

;;;; Character writing: combining marks, DEC special graphics remapping, and
;;;; wide/normal cell placement at the cursor.
;;;; Loads after cursor.lisp (needs cursor-down/scroll) and edit.lisp (needs
;;;; insert-chars for IRM).

(declaim (inline %mark-dirty))
(defun %mark-dirty (screen)
  "Mark SCREEN dirty: signal the renderer that cells have changed."
  (setf (screen-dirty-p screen) t))

(defun %advance-cursor (screen n)
  "Advance the cursor N columns after a write.  When the write reaches the right
   margin with autowrap on, the wrap is DEFERRED (VT100 last-column flag): the
   cursor stays parked at the last column and screen-pending-wrap is set, so the
   wrap happens only when the next character arrives (see write-char-at-cursor).
   With autowrap off the cursor clamps at the last column (the write overwrites)."
  (let ((next-x (+ (screen-cursor-x screen) n)))
    (cond
      ;; Advance: fits within the current row.
      ((< next-x (screen-width screen))
       (setf (screen-cursor-x screen) next-x))
      ;; Reached the right margin with autowrap on: defer the wrap, park at the
      ;; last column.  The next printable char performs the wrap.
      ((screen-autowrap screen)
       (setf (screen-cursor-x screen) (1- (screen-width screen))
             (screen-pending-wrap screen) t))
      ;; Clamp: reached the right margin and autowrap is off.
      (t
       (setf (screen-cursor-x screen) (1- (screen-width screen)))))))

(defun %place-wide-char (screen x y char fg bg attrs attrs2 ul-color hyperlink)
  "Place a double-width character at (X,Y) and write its continuation cell.
   The continuation cell is written only if (1+ X) is within the screen."
  (setf (screen-cell screen x y)
        (make-cell :char char :fg fg :bg bg :attrs attrs :attrs2 attrs2
                   :ul-color ul-color :hyperlink hyperlink :width 2))
  (when (< (1+ x) (screen-width screen))
    (setf (screen-cell screen (1+ x) y)
          (make-cell :char #\Space :fg fg :bg bg :attrs attrs :attrs2 attrs2
                     :ul-color ul-color :hyperlink hyperlink :width 0))))

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
  ;; Vertical line + the nine horizontal scan lines.  The DEC set places nine
  ;; horizontal rules at distinct vertical positions; q (scan line 5) is the
  ;; middle = the box-drawing horizontal, o/p sit above it, r/s below.  Mapping
  ;; each to its exact scan-line glyph (not all to ─) preserves the rule height an
  ;; app intends (e.g. a double rule drawn with o + s).
  (#\x #\│ "vertical line")
  (#\o #\⎺ "scan line 1 (top)")
  (#\p #\⎻ "scan line 3")
  (#\q #\─ "scan line 5 / horizontal line")
  (#\r #\⎼ "scan line 7")
  (#\s #\⎽ "scan line 9 (bottom)")
  ;; Special characters
  (#\a #\▒ "checkerboard")
  (#\` #\◆ "diamond")
  (#\f #\° "degree symbol")
  (#\g #\± "plus-minus")
  ;; Math / relational symbols (upper half of the DEC special-graphics set —
  ;; these are emitted by real apps and were previously passed through literally).
  (#\y #\≤ "less-than-or-equal")
  (#\z #\≥ "greater-than-or-equal")
  (#\{ #\π "pi")
  (#\| #\≠ "not-equal")
  (#\} #\£ "UK pound sign")
  (#\~ #\· "centred dot / bullet")
  (#\_ #\Space "blank")
  ;; Control-code picture glyphs (rarely emitted; included to complete the set).
  (#\b #\␉ "horizontal tab (HT)")
  (#\c #\␌ "form feed (FF)")
  (#\d #\␍ "carriage return (CR)")
  (#\e #\␊ "line feed (LF)")
  (#\h #\␤ "newline (NL)")
  (#\i #\␋ "vertical tab (VT)"))

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
      (setf (screen-cell screen (screen-cursor-x screen) (screen-cursor-y screen)) (blank-cell)
            (screen-cursor-x screen) 0)
      (cursor-down/scroll screen))
    (%place-wide-char screen (screen-cursor-x screen) (screen-cursor-y screen)
                      ch fg bg attrs attrs2 ul-color
                      (screen-current-hyperlink screen))
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
                     :ul-color ul-color :hyperlink (screen-current-hyperlink screen)
                     :width 1))
    (%mark-dirty screen)
    (%advance-cursor screen 1)))

(defun %consume-pending-wrap (screen)
  "Perform a deferred VT100 wrap if one is pending: the previous write parked
   the cursor at the last column with autowrap on, so the NEXT character
   triggers the actual wrap to column 0 of the following row.  Records the
   row as wrapped for capture-pane -J before cursor-down/scroll, which may
   shift the wrap flags."
  (when (screen-pending-wrap screen)
    (%mark-line-wrapped screen (screen-cursor-y screen))
    (setf (screen-pending-wrap screen) nil
          (screen-cursor-x screen) 0)
    (cursor-down/scroll screen)))

(defun %apply-insert-mode-gap (screen ch)
  "IRM (insert mode): when active, open a gap of CH's display width at the
   cursor so the new character pushes the rest of the line right instead of
   overwriting it."
  (when (screen-insert-mode screen)
    (insert-chars screen (char-width ch))))

(defun write-char-at-cursor (screen ch)
  "Write CH at the cursor, then advance.  Double-width (CJK) characters occupy
   a lead cell plus a continuation placeholder and advance the cursor by two;
   a wide char that will not fit at the right edge wraps to the next line first.
   Records CH as the screen's LAST-CHAR for use by CSI REP sequences.

   When CH is a Unicode combining character, it is appended to the previous cell
   and the cursor is NOT advanced.

   When the screen's charset is :dec-graphics, CH is remapped through the DEC
   special graphics table before being written."
  (if (combining-char-p ch)
      (%append-combining-char screen ch)
      (let ((remapped-ch (progn
                            (%consume-pending-wrap screen)
                            (%remap-charset-char screen ch))))
        (setf (screen-last-char screen) remapped-ch)
        (%apply-insert-mode-gap screen remapped-ch)
        (if (= (char-width remapped-ch) 2)
            (%write-wide-cell   screen remapped-ch)
            (%write-normal-cell screen remapped-ch)))))

(defun write-codepoint (screen cp)
  "Write Unicode code point CP at the cursor, converting it via SAFE-CODE-CHAR."
  (write-char-at-cursor screen (safe-code-char cp)))
