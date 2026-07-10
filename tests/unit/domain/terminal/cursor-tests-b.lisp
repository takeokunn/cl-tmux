(in-package #:cl-tmux/test)

;;;; cursor tests — part B: %place-wide-char, table-driven cursor movement,
;;;; combining-char-p, write-char combining, DEC special graphics charset.


(def-suite place-wide-char-suite
  :description "%place-wide-char lead and continuation cell layout"
  :in terminal-suite)
(in-suite place-wide-char-suite)

(test place-wide-char-writes-lead-and-continuation
  :description "%place-wide-char writes a width-2 lead cell and width-0 continuation."
  (with-screen (s 10 5)
    (cl-tmux/terminal/actions::%place-wide-char s 0 0 #\中 7 0 0 0 0 nil)
    (let ((lead (screen-cell s 0 0))
          (cont (screen-cell s 1 0)))
      (is (char= #\中 (cell-char lead)) "lead cell char must be 中")
      (is (= 2 (cell-width lead))       "lead cell width must be 2")
      (is (= 0 (cell-width cont))       "continuation cell width must be 0"))))

(test place-wide-char-at-last-column-no-continuation
  :description "%place-wide-char at the last column skips writing the continuation cell."
  (with-screen (s 5 5)
    ;; Place a wide char at x=4 (last column); x+1=5 >= width, so no continuation
    (cl-tmux/terminal/actions::%place-wide-char s 4 0 #\中 7 0 0 0 0 nil)
    (let ((lead (screen-cell s 4 0)))
      (is (char= #\中 (cell-char lead)) "lead cell must be 中 even at last column")
      (is (= 2 (cell-width lead))       "lead cell width must be 2"))))

;;; ── SUITE: table-driven cursor movement ──────────────────────────────────────
;;;
;;; Repeated cursor-up/down/left/right cases at count=1 form a natural table.

(def-suite cursor-movement-table-suite
  :description "Table-driven single-step cursor movement"
  :in terminal-suite)
(in-suite cursor-movement-table-suite)

(test cursor-movements-single-step-table
  :description "Each direction moves by 1 from a known starting position."
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
          (is (= ex (screen-cursor-x s))
              "cursor-x after ~A from (~D,~D) expected ~D got ~D"
              dir sx sy ex (screen-cursor-x s))
          (is (= ey (screen-cursor-y s))
              "cursor-y after ~A from (~D,~D) expected ~D got ~D"
              dir sx sy ey (screen-cursor-y s)))))))

;;; ── SUITE: combining-char-p predicate ───────────────────────────────────────
;;;
;;; combining-char-p is exported from cl-tmux/terminal/actions and must return
;;; T only for Unicode combining marks (category M*).

(def-suite combining-char-p-suite
  :description "combining-char-p predicate: true for combining marks, false otherwise"
  :in terminal-suite)
(in-suite combining-char-p-suite)

(test combining-char-p-returns-true-for-combining-diacritical-marks
  :description "combining-char-p returns T for code points in the Combining Diacritical Marks block (U+0300-U+036F)."
  (is (cl-tmux/terminal/actions:combining-char-p (code-char #x0300))
      "U+0300 (combining grave accent) must be a combining character")
  (is (cl-tmux/terminal/actions:combining-char-p (code-char #x036F))
      "U+036F (last in block) must be a combining character"))

(test combining-char-p-returns-true-for-combining-half-marks
  :description "combining-char-p returns T for code points in the Combining Half Marks block (U+FE20-U+FE2F)."
  (is (cl-tmux/terminal/actions:combining-char-p (code-char #xFE20))
      "U+FE20 must be a combining character")
  (is (cl-tmux/terminal/actions:combining-char-p (code-char #xFE2F))
      "U+FE2F must be a combining character"))

(test combining-char-p-returns-false-for-ascii-printable
  :description "combining-char-p returns NIL for ordinary ASCII characters."
  (is-false (cl-tmux/terminal/actions:combining-char-p #\A)
            "ASCII 'A' must not be a combining character")
  (is-false (cl-tmux/terminal/actions:combining-char-p #\Space)
            "ASCII space must not be a combining character")
  (is-false (cl-tmux/terminal/actions:combining-char-p #\Null)
            "NUL must not be a combining character"))

(test combining-char-p-table-driven
  :description "Table-driven test across all five combining ranges."
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
        (let ((ch (code-char cp)))
          (if expected
              (is (cl-tmux/terminal/actions:combining-char-p ch)
                  "U+~4,'0X should be a combining char: ~A" cp description)
              (is-false (cl-tmux/terminal/actions:combining-char-p ch)
                        "U+~4,'0X should NOT be a combining char: ~A" cp description)))))))

;;; ── SUITE: write-char-at-cursor combining-char path ─────────────────────────
;;;
;;; When write-char-at-cursor receives a combining mark, it must:
;;;   1. Append the mark to the previous cell's combining slot.
;;;   2. NOT advance the cursor.
;;;   3. Mark the screen dirty.

(def-suite write-char-combining-suite
  :description "write-char-at-cursor: combining character appended to previous cell"
  :in terminal-suite)
(in-suite write-char-combining-suite)

(test write-char-at-cursor-combining-does-not-advance-cursor
  :description "Writing a combining mark does not advance the cursor."
  (with-screen (s 10 5)
    (feed s "a")                          ; cursor at col 1
    ;; Feed a combining acute accent (U+0301) directly
    (cl-tmux/terminal/actions:write-char-at-cursor s (code-char #x0301))
    ;; Cursor must not have moved
    (check-cursor s 1 0)))

(test write-char-at-cursor-combining-appended-to-cell
  :description "A combining mark is appended to the combining slot of the previous cell."
  (with-screen (s 10 5)
    ;; Write 'a', then a combining acute accent
    (cl-tmux/terminal/actions:write-char-at-cursor s #\a)
    (cl-tmux/terminal/actions:write-char-at-cursor s (code-char #x0301))
    (let ((cell (screen-cell s 0 0)))
      ;; The base char must still be 'a'
      (is (char= #\a (cell-char cell))
          "base cell char must remain 'a' after combining mark")
      ;; The combining slot must contain the diacritic
      (is (member (code-char #x0301)
                  (cl-tmux/terminal/types:cell-combining cell))
          "combining slot must contain the acute accent (U+0301)"))))

(test write-char-at-cursor-combining-at-col-zero-uses-col-zero
  :description "A combining mark at cursor col 0 is appended to col 0 (no prev cell)."
  (with-screen (s 10 5)
    ;; With cursor at col 0, a combining mark attaches to col 0.
    (cl-tmux/terminal/actions:write-char-at-cursor s (code-char #x0300))
    ;; Cursor stays at 0
    (check-cursor s 0 0)
    ;; No crash and screen is dirty
    (is (cl-tmux/terminal/types:screen-dirty-p s)
        "screen must be marked dirty after combining mark write")))

;;; ── SUITE: DEC special graphics charset remapping ────────────────────────────
;;;
;;; When screen-charset = :dec-graphics, write-char-at-cursor remaps characters
;;; through %dec-graphics-char before placing them on the grid.

(def-suite dec-graphics-suite
  :description "DEC special graphics charset: character remapping via designate-charset"
  :in terminal-suite)
(in-suite dec-graphics-suite)

(test set-charset-dec-graphics-remaps-box-drawing
  :description "After designate-charset :g0 :dec-graphics, writing 'j' places the box-drawing corner U+2518."
  (with-screen (s 10 5)
    (cl-tmux/terminal/actions:designate-charset s :g0 :dec-graphics)
    (cl-tmux/terminal/actions:write-char-at-cursor s #\j)
    ;; 'j' maps to U+2518 (LOWER RIGHT CORNER ┘)
    (is (char= #\┘ (char-at s 0 0))
        "DEC graphics: 'j' must map to '┘' (U+2518), got ~C"
        (char-at s 0 0))))

(test set-charset-dec-graphics-remaps-horizontal-line
  :description "After designate-charset :g0 :dec-graphics, writing 'q' places the horizontal line U+2500."
  (with-screen (s 10 5)
    (cl-tmux/terminal/actions:designate-charset s :g0 :dec-graphics)
    (cl-tmux/terminal/actions:write-char-at-cursor s #\q)
    ;; 'q' maps to U+2500 (BOX DRAWINGS LIGHT HORIZONTAL ─)
    (is (char= #\─ (char-at s 0 0))
        "DEC graphics: 'q' must map to '─' (U+2500), got ~C"
        (char-at s 0 0))))

(test set-charset-ascii-no-remapping
  :description "After designate-charset :g0 :ascii (default), characters are written unchanged."
  (with-screen (s 10 5)
    (cl-tmux/terminal/actions:designate-charset s :g0 :ascii)
    (cl-tmux/terminal/actions:write-char-at-cursor s #\j)
    ;; In ASCII mode, 'j' is 'j'
    (is (char= #\j (char-at s 0 0))
        "ASCII charset: 'j' must remain 'j', got ~C"
        (char-at s 0 0))))

(test set-charset-dec-graphics-table-driven
  :description "Table-driven DEC graphics remapping for all documented mappings."
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
        (with-screen (s 10 5)
          (cl-tmux/terminal/actions:designate-charset s :g0 :dec-graphics)
          (cl-tmux/terminal/actions:write-char-at-cursor s in)
          (is (char= expected (char-at s 0 0))
              "DEC graphics ~C: expected ~C got ~C (~A)"
              in expected (char-at s 0 0) desc))))))

(test set-charset-dec-graphics-unmapped-char-passes-through
  :description "Characters not in the DEC graphics table pass through unchanged."
  (with-screen (s 10 5)
    (cl-tmux/terminal/actions:designate-charset s :g0 :dec-graphics)
    ;; '#\5' (a digit) is not in the DEC special-graphics set — only certain
    ;; lowercase letters and symbols are remapped, so digits/uppercase pass through.
    (cl-tmux/terminal/actions:write-char-at-cursor s #\5)
    (is (char= #\5 (char-at s 0 0))
        "unmapped DEC graphics char '5' must pass through unchanged")))

(test dec-graphics-activated-via-esc-sequence
  :description "ESC ( 0 activates DEC graphics charset; subsequent chars are remapped."
  (with-screen (s 10 5)
    ;; ESC ( 0 = G0 charset select, DEC special graphics
    (feed s (esc "(0"))
    ;; Write 'j' — should appear as box-drawing corner
    (feed s "j")
    (is (char= #\┘ (char-at s 0 0))
        "ESC ( 0 + 'j' must render as '┘', got ~C"
        (char-at s 0 0))))

(test dec-graphics-deactivated-via-esc-sequence
  :description "ESC ( B restores ASCII charset; characters are no longer remapped."
  (with-screen (s 10 5)
    (feed s (esc "(0"))   ; enable DEC graphics
    (feed s (esc "(B"))   ; restore ASCII
    (feed s "j")          ; now plain ASCII 'j'
    (is (char= #\j (char-at s 0 0))
        "ESC ( B + 'j' must render as 'j', got ~C"
        (char-at s 0 0))))
