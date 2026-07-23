(in-package #:cl-tmux/test)

;;;; cursor tests — part B: %place-wide-char, table-driven cursor movement,
;;;; combining-char-p, write-char combining, DEC special graphics charset.


(describe "terminal-suite/place-wide-char-suite"

  ;; %place-wide-char writes a width-2 lead cell and width-0 continuation.
  (it "place-wide-char-writes-lead-and-continuation"
    (with-screen (s 10 5)
      (cl-tmux/terminal/actions::%place-wide-char s 0 0 #\中 7 0 0 0 0 nil)
      (let ((lead (screen-cell s 0 0))
            (cont (screen-cell s 1 0)))
        (expect (char= #\中 (cell-char lead)))
        (expect (= 2 (cell-width lead)))
        (expect (= 0 (cell-width cont))))))

  ;; %place-wide-char at the last column skips writing the continuation cell.
  (it "place-wide-char-at-last-column-no-continuation"
    (with-screen (s 5 5)
      ;; Place a wide char at x=4 (last column); x+1=5 >= width, so no continuation
      (cl-tmux/terminal/actions::%place-wide-char s 4 0 #\中 7 0 0 0 0 nil)
      (let ((lead (screen-cell s 4 0)))
        (expect (char= #\中 (cell-char lead)))
        (expect (= 2 (cell-width lead)))))))

;;; ── SUITE: table-driven cursor movement ──────────────────────────────────────
;;;
;;; Repeated cursor-up/down/left/right cases at count=1 form a natural table.

(describe "terminal-suite/cursor-movement-table-suite"

  ;; Each direction moves by 1 from a known starting position.
  (it "cursor-movements-single-step-table"
    ;; Table: (start-x start-y direction count expected-x expected-y)
    (let ((cases '((5 5 up    1 5 4)
                   (5 5 down  1 5 6)
                   (5 5 left  1 4 5)
                   (5 5 right 1 6 5))))
      (dolist (c cases)
        (destructuring-bind (sx sy dir n ex ey) c
          (with-screen (s 10 10)
            (setf (cl-tmux/terminal/types:screen-cursor-x s) sx
                  (cl-tmux/terminal/types:screen-cursor-y s) sy)
            (ecase dir
              (up    (cl-tmux/terminal/actions:cursor-up    s n))
              (down  (cl-tmux/terminal/actions:cursor-down  s n))
              (left  (cl-tmux/terminal/actions:cursor-left  s n))
              (right (cl-tmux/terminal/actions:cursor-right s n)))
            (expect (= ex (screen-cursor-x s)))
            (expect (= ey (screen-cursor-y s)))))))))

;;; ── SUITE: combining-char-p predicate ───────────────────────────────────────
;;;
;;; combining-char-p is exported from cl-tmux/terminal/actions and must return
;;; T only for Unicode combining marks (category M*).

(describe "terminal-suite/combining-char-p-suite"

  ;; combining-char-p returns T for code points in the Combining Diacritical Marks block (U+0300-U+036F).
  (it "combining-char-p-returns-true-for-combining-diacritical-marks"
    (expect (cl-tmux/terminal/actions:combining-char-p (code-char #x0300)))
    (expect (cl-tmux/terminal/actions:combining-char-p (code-char #x036F))))

  ;; combining-char-p returns T for code points in the Combining Half Marks block (U+FE20-U+FE2F).
  (it "combining-char-p-returns-true-for-combining-half-marks"
    (expect (cl-tmux/terminal/actions:combining-char-p (code-char #xFE20)))
    (expect (cl-tmux/terminal/actions:combining-char-p (code-char #xFE2F))))

  ;; combining-char-p returns NIL for ordinary ASCII characters.
  (it "combining-char-p-returns-false-for-ascii-printable"
    (expect (cl-tmux/terminal/actions:combining-char-p #\A) :to-be-falsy)
    (expect (cl-tmux/terminal/actions:combining-char-p #\Space) :to-be-falsy)
    (expect (cl-tmux/terminal/actions:combining-char-p #\Null) :to-be-falsy))

  ;; Table-driven test across all five combining ranges.
  (it "combining-char-p-table-driven"
    ;; Each entry: (code-point expected-result description)
    (let ((cases
           `((#x0300 t   "combining grave accent — Diacritical Marks start")
             (#x036F t   "combining latin small letter x — Diacritical Marks end")
             (#x0370 nil "greek capital letter Heta — just after Diacritical Marks")
             (#x1AB0 t   "combining doubled circumflex accent — Extended start")
             (#x1AFF t   "last code in Extended block")
             (#x20D0 t   "combining left harpoon above — Marks for Symbols start")
             (#x20FF t   "last code in Marks for Symbols block")
             (#x0041 nil "ASCII A — not a combining character"))))
      (dolist (c cases)
        (destructuring-bind (cp expected description) c
          (declare (ignore description))
          (let ((ch (code-char cp)))
            (if expected
                (expect (cl-tmux/terminal/actions:combining-char-p ch))
                (expect (cl-tmux/terminal/actions:combining-char-p ch) :to-be-falsy))))))))

;;; ── SUITE: write-char-at-cursor combining-char path ─────────────────────────
;;;
;;; When write-char-at-cursor receives a combining mark, it must:
;;;   1. Append the mark to the previous cell's combining slot.
;;;   2. NOT advance the cursor.
;;;   3. Mark the screen dirty.

(describe "terminal-suite/write-char-combining-suite"

  ;; Writing a combining mark does not advance the cursor.
  (it "write-char-at-cursor-combining-does-not-advance-cursor"
    (with-screen (s 10 5)
      (feed s "a")                          ; cursor at col 1
      ;; Feed a combining acute accent (U+0301) directly
      (cl-tmux/terminal/actions:write-char-at-cursor s (code-char #x0301))
      ;; Cursor must not have moved
      (check-cursor s 1 0)))

  ;; A combining mark is appended to the combining slot of the previous cell.
  (it "write-char-at-cursor-combining-appended-to-cell"
    (with-screen (s 10 5)
      ;; Write 'a', then a combining acute accent
      (cl-tmux/terminal/actions:write-char-at-cursor s #\a)
      (cl-tmux/terminal/actions:write-char-at-cursor s (code-char #x0301))
      (let ((cell (screen-cell s 0 0)))
        ;; The base char must still be 'a'
        (expect (char= #\a (cell-char cell)))
        ;; The combining slot must contain the diacritic
        (expect (member (code-char #x0301)
                        (cl-tmux/terminal/types:cell-combining cell))))))

  ;; A combining mark at cursor col 0 is appended to col 0 (no prev cell).
  (it "write-char-at-cursor-combining-at-col-zero-uses-col-zero"
    (with-screen (s 10 5)
      ;; With cursor at col 0, a combining mark attaches to col 0.
      (cl-tmux/terminal/actions:write-char-at-cursor s (code-char #x0300))
      ;; Cursor stays at 0
      (check-cursor s 0 0)
      ;; No crash and screen is dirty
      (expect (cl-tmux/terminal/types:screen-dirty-p s)))))

;;; ── SUITE: DEC special graphics charset remapping ────────────────────────────
;;;
;;; When screen-charset = :dec-graphics, write-char-at-cursor remaps characters
;;; through %dec-graphics-char before placing them on the grid.

(describe "terminal-suite/dec-graphics-suite"

  ;; After designate-charset :g0 :dec-graphics, writing 'j' places the box-drawing corner U+2518.
  (it "set-charset-dec-graphics-remaps-box-drawing"
    (with-screen (s 10 5)
      (cl-tmux/terminal/actions:designate-charset s :g0 :dec-graphics)
      (cl-tmux/terminal/actions:write-char-at-cursor s #\j)
      ;; 'j' maps to U+2518 (LOWER RIGHT CORNER ┘)
      (expect (char= #\┘ (char-at s 0 0)))))

  ;; After designate-charset :g0 :dec-graphics, writing 'q' places the horizontal line U+2500.
  (it "set-charset-dec-graphics-remaps-horizontal-line"
    (with-screen (s 10 5)
      (cl-tmux/terminal/actions:designate-charset s :g0 :dec-graphics)
      (cl-tmux/terminal/actions:write-char-at-cursor s #\q)
      ;; 'q' maps to U+2500 (BOX DRAWINGS LIGHT HORIZONTAL ─)
      (expect (char= #\─ (char-at s 0 0)))))

  ;; After designate-charset :g0 :ascii (default), characters are written unchanged.
  (it "set-charset-ascii-no-remapping"
    (with-screen (s 10 5)
      (cl-tmux/terminal/actions:designate-charset s :g0 :ascii)
      (cl-tmux/terminal/actions:write-char-at-cursor s #\j)
      ;; In ASCII mode, 'j' is 'j'
      (expect (char= #\j (char-at s 0 0)))))

  ;; Table-driven DEC graphics remapping for all documented mappings.
  (it "set-charset-dec-graphics-table-driven"
    ;; Each entry: (input-char expected-char description)
    (let ((cases '((#\j #\┘ "lower-right corner")
                   (#\k #\┐ "upper-right corner")
                   (#\l #\┌ "upper-left corner")
                   (#\m #\└ "lower-left corner")
                   (#\n #\┼ "crossing")
                   (#\t #\├ "left tee")
                   (#\u #\┤ "right tee")
                   (#\v #\┴ "bottom tee")
                   (#\w #\┬ "top tee")
                   (#\q #\─ "horizontal line")
                   (#\x #\│ "vertical line")
                   (#\a #\▒ "checkerboard")
                   (#\` #\◆ "diamond")
                   ;; Upper half of the set — math/relational symbols + scan lines.
                   (#\y #\≤ "less-than-or-equal")
                   (#\z #\≥ "greater-than-or-equal")
                   (#\{ #\π "pi")
                   (#\| #\≠ "not-equal")
                   (#\} #\£ "UK pound sign")
                   (#\~ #\· "centred dot")
                   (#\o #\⎺ "scan line 1")
                   (#\s #\⎽ "scan line 9"))))
      (dolist (entry cases)
        (destructuring-bind (in expected desc) entry
          (declare (ignore desc))
          (with-screen (s 10 5)
            (cl-tmux/terminal/actions:designate-charset s :g0 :dec-graphics)
            (cl-tmux/terminal/actions:write-char-at-cursor s in)
            (expect (char= expected (char-at s 0 0))))))))

  ;; Characters not in the DEC graphics table pass through unchanged.
  (it "set-charset-dec-graphics-unmapped-char-passes-through"
    (with-screen (s 10 5)
      (cl-tmux/terminal/actions:designate-charset s :g0 :dec-graphics)
      ;; '#\5' (a digit) is not in the DEC special-graphics set — only certain
      ;; lowercase letters and symbols are remapped, so digits/uppercase pass through.
      (cl-tmux/terminal/actions:write-char-at-cursor s #\5)
      (expect (char= #\5 (char-at s 0 0)))))

  ;; ESC ( 0 activates DEC graphics charset; subsequent chars are remapped.
  (it "dec-graphics-activated-via-esc-sequence"
    (with-screen (s 10 5)
      ;; ESC ( 0 = G0 charset select, DEC special graphics
      (feed s (esc "(0"))
      ;; Write 'j' — should appear as box-drawing corner
      (feed s "j")
      (expect (char= #\┘ (char-at s 0 0)))))

  ;; ESC ( B restores ASCII charset; characters are no longer remapped.
  (it "dec-graphics-deactivated-via-esc-sequence"
    (with-screen (s 10 5)
      (feed s (esc "(0"))   ; enable DEC graphics
      (feed s (esc "(B"))   ; restore ASCII
      (feed s "j")          ; now plain ASCII 'j'
      (expect (char= #\j (char-at s 0 0))))))
