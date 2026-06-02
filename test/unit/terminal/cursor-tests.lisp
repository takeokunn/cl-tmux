(in-package #:cl-tmux/test)

;;;; Cursor movement and character-writing tests (src/terminal/cursor.lisp).
;;;; Tests: scroll-region clamping, direct action functions, %advance-cursor
;;;;        autowrap behaviour, set-cursor, cursor-ri, cursor-cht/cbt,
;;;;        combining-char handling, DEC-graphics remapping, and %place-wide-char.

;;; ── SUITE: scroll-region cursor clamping ────────────────────────────────────
;;;
;;; cl-tmux/terminal/actions::cursor-up/down clamp to the SCROLL REGION
;;; boundaries (scroll-top / scroll-bottom), which diverges from set-cursor
;;; (the CSI A/B handler), which clamps to the full screen (0 .. height-1).
;;; cursor-left/right clamp to column 0 / width-1.  These tests call the
;;; action functions directly, having first installed a non-trivial scroll
;;; region via the real slot accessors.

(def-suite scroll-region-clamp
  :description "Direct cursor-up/down/left/right clamp to scroll-region margins"
  :in terminal-suite)
(in-suite scroll-region-clamp)

(test cursor-up-clamps-to-scroll-top
  "cursor-up with a large count stops at scroll-top, NOT at row 0.
   This is the divergence from set-cursor, which would clamp to 0."
  (with-screen (s 10 10)
    ;; Scroll region = rows 3..7 (0-based inclusive).
    (setf (cl-tmux/terminal/types::screen-scroll-top s) 3
          (cl-tmux/terminal/types::screen-scroll-bottom s) 7)
    ;; Position cursor inside the region.
    (setf (cl-tmux/terminal/types::screen-cy s) 6)
    (cl-tmux/terminal/actions::cursor-up s 100)
    (is (= 3 (screen-cursor-y s))
        "cursor-up should clamp to scroll-top 3, got ~D" (screen-cursor-y s))
    ;; Sanity: it stopped at scroll-top, not at the screen top (0).
    (is (/= 0 (screen-cursor-y s))
        "cursor-up must not pass scroll-top down to row 0")))

(test cursor-down-clamps-to-scroll-bottom
  "cursor-down with a large count stops at scroll-bottom, NOT at height-1."
  (with-screen (s 10 10)
    (setf (cl-tmux/terminal/types::screen-scroll-top s) 3
          (cl-tmux/terminal/types::screen-scroll-bottom s) 7)
    (setf (cl-tmux/terminal/types::screen-cy s) 4)
    (cl-tmux/terminal/actions::cursor-down s 100)
    (is (= 7 (screen-cursor-y s))
        "cursor-down should clamp to scroll-bottom 7, got ~D" (screen-cursor-y s))
    ;; Sanity: it stopped at scroll-bottom, not at the screen bottom (9).
    (is (/= 9 (screen-cursor-y s))
        "cursor-down must not pass scroll-bottom down to height-1")))

(test cursor-left-clamps-to-column-zero
  "cursor-left with a large count clamps to column 0."
  (with-screen (s 10 10)
    (setf (cl-tmux/terminal/types::screen-cx s) 5)
    (cl-tmux/terminal/actions::cursor-left s 100)
    (is (= 0 (screen-cursor-x s))
        "cursor-left should clamp to column 0, got ~D" (screen-cursor-x s))))

(test cursor-right-clamps-to-width-minus-one
  "cursor-right with a large count clamps to width-1."
  (with-screen (s 10 10)
    (setf (cl-tmux/terminal/types::screen-cx s) 2)
    (cl-tmux/terminal/actions::cursor-right s 100)
    (is (= 9 (screen-cursor-x s))
        "cursor-right should clamp to width-1 (9), got ~D" (screen-cursor-x s))))

(test cursor-up-down-respect-region-from-mid
  "From a row inside the region, a small cursor-up/down stays within the
   region and does not overshoot the margins."
  (with-screen (s 10 10)
    (setf (cl-tmux/terminal/types::screen-scroll-top s) 2
          (cl-tmux/terminal/types::screen-scroll-bottom s) 8)
    (setf (cl-tmux/terminal/types::screen-cy s) 5)
    (cl-tmux/terminal/actions::cursor-up s 2)
    (is (= 3 (screen-cursor-y s)) "cursor-up 2 from row 5 -> row 3")
    (cl-tmux/terminal/actions::cursor-down s 4)
    (is (= 7 (screen-cursor-y s)) "cursor-down 4 from row 3 -> row 7")))

;;; ── SUITE: set-cursor ────────────────────────────────────────────────────────

(def-suite set-cursor-suite
  :description "set-cursor: clamping to screen bounds"
  :in terminal-suite)
(in-suite set-cursor-suite)

(test set-cursor-places-cursor-at-exact-position
  :description "set-cursor moves cursor to the specified (x, y) within bounds."
  (with-screen (s 10 10)
    (cl-tmux/terminal/actions:set-cursor s 3 7)
    (check-cursor s 3 7)))

(test set-cursor-clamps-x-to-width-minus-one
  :description "set-cursor clamps x to width-1 when x >= width."
  (with-screen (s 10 10)
    (cl-tmux/terminal/actions:set-cursor s 99 0)
    (is (= 9 (screen-cursor-x s))
        "cursor-x must be clamped to 9 (width-1)")))

(test set-cursor-clamps-y-to-height-minus-one
  :description "set-cursor clamps y to height-1 when y >= height."
  (with-screen (s 10 10)
    (cl-tmux/terminal/actions:set-cursor s 0 99)
    (is (= 9 (screen-cursor-y s))
        "cursor-y must be clamped to 9 (height-1)")))

(test set-cursor-clamps-negative-x-to-zero
  :description "set-cursor clamps a negative x to 0."
  (with-screen (s 10 10)
    (cl-tmux/terminal/actions:set-cursor s -5 3)
    (is (= 0 (screen-cursor-x s)) "cursor-x must be clamped to 0 for negative input")))

;;; ── SUITE: direct-action-cursor ─────────────────────────────────────────────
;;;
;;; These tests call action functions directly rather than through
;;; screen-process-bytes, targeting edge cases that the CSI/parser path
;;; may not hit explicitly.

(def-suite direct-action-cursor
  :description "Direct calls to cursor-bs/cr/lf/ht/ri, write-codepoint, scroll helpers"
  :in terminal-suite)
(in-suite direct-action-cursor)

(test cursor-bs-moves-left-one-column
  "cursor-bs decrements the cursor column by 1."
  (with-screen (s 10 5)
    (feed s "abc")                        ; cursor at col 3
    (cl-tmux/terminal/actions:cursor-bs s)
    (check-cursor s 2 0)))

(test cursor-bs-noop-at-column-zero
  "cursor-bs at column 0 does nothing."
  (with-screen (s 10 5)
    (cl-tmux/terminal/actions:cursor-bs s) ; cursor already at 0
    (check-cursor s 0 0)))

(test cursor-cr-resets-column
  "cursor-cr moves the cursor to column 0 on the current row."
  (with-screen (s 10 5)
    (feed s "hello")
    (cl-tmux/terminal/actions:cursor-cr s)
    (check-cursor s 0 0)))

(test cursor-lf-scrolls-at-bottom
  "cursor-lf at the bottom of the scroll region scrolls up."
  (with-screen (s 5 3)
    (feed s "A")
    (cl-tmux/terminal/actions:cursor-lf s)
    (cl-tmux/terminal/actions:cursor-lf s) ; now at row 2 (bottom)
    (cl-tmux/terminal/actions:cursor-lf s) ; should scroll, not go to row 3
    (is (<= (screen-cursor-y s) 2)
        "cursor must stay within screen bounds after lf at bottom")))

(test cursor-ht-advances-to-next-tab-stop
  "cursor-ht advances the cursor to the next 8-column tab stop."
  (with-screen (s 20 5)
    ;; From col 0 -> next stop is col 8
    (cl-tmux/terminal/actions:cursor-ht s)
    (check-cursor s 8 0)
    ;; From col 8 -> next stop is col 16
    (cl-tmux/terminal/actions:cursor-ht s)
    (check-cursor s 16 0)
    ;; From col 16 -> next stop would be 24, but screen is 20 wide -> clamp to 19
    (cl-tmux/terminal/actions:cursor-ht s)
    (check-cursor s 19 0)))

;;; ── cursor-cht (CHT — cursor forward tab stops) ──────────────────────────────

(test cursor-cht-advances-n-tab-stops
  :description "cursor-cht N advances the cursor by N tab stops."
  (with-screen (s 40 5)
    ;; From col 0, 2 tab stops → col 16
    (cl-tmux/terminal/actions:cursor-cht s 2)
    (check-cursor s 16 0)))

(test cursor-cht-one-is-same-as-cursor-ht
  :description "cursor-cht 1 behaves identically to cursor-ht."
  (let ((s1 (make-screen 20 5))
        (s2 (make-screen 20 5)))
    (cl-tmux/terminal/actions:cursor-ht  s1)
    (cl-tmux/terminal/actions:cursor-cht s2 1)
    (is (= (screen-cursor-x s1) (screen-cursor-x s2))
        "cursor-cht 1 must give same result as cursor-ht")))

(test cursor-cht-zero-treated-as-one
  :description "cursor-cht 0 advances one tab stop (n is treated as max 1)."
  (with-screen (s 20 5)
    (cl-tmux/terminal/actions:cursor-cht s 0)
    (is (= 8 (screen-cursor-x s))
        "cursor-cht 0 must advance one tab stop to col 8")))

;;; ── cursor-cbt (CBT — cursor backward tab stops) ─────────────────────────────

(test cursor-cbt-moves-back-n-tab-stops
  :description "cursor-cbt N moves the cursor back by N 8-column tab stops."
  (with-screen (s 40 5)
    (setf (cl-tmux/terminal/types::screen-cx s) 16)
    ;; Back 2 stops: 16 → 8 → 0
    (cl-tmux/terminal/actions:cursor-cbt s 2)
    (is (= 0 (screen-cursor-x s))
        "cursor-cbt 2 from col 16 must reach col 0")))

(test cursor-cbt-clamps-at-column-zero
  :description "cursor-cbt with a large count stops at column 0."
  (with-screen (s 40 5)
    (setf (cl-tmux/terminal/types::screen-cx s) 5)
    (cl-tmux/terminal/actions:cursor-cbt s 99)
    (is (= 0 (screen-cursor-x s))
        "cursor-cbt must not go past column 0")))

(test cursor-cbt-zero-treated-as-one
  :description "cursor-cbt 0 moves back one tab stop."
  (with-screen (s 40 5)
    (setf (cl-tmux/terminal/types::screen-cx s) 16)
    (cl-tmux/terminal/actions:cursor-cbt s 0)
    (is (= 8 (screen-cursor-x s))
        "cursor-cbt 0 must move back one tab stop")))

;;; ── cursor-ri (ESC M — reverse index) ───────────────────────────────────────

(test cursor-ri-moves-up-within-region
  :description "cursor-ri moves the cursor up one row when not at the scroll-top."
  (with-screen (s 10 10)
    (setf (cl-tmux/terminal/types::screen-cy s) 5)
    (cl-tmux/terminal/actions:cursor-ri s)
    (is (= 4 (screen-cursor-y s))
        "cursor-ri from row 5 must move to row 4")))

(test cursor-ri-at-scroll-top-scrolls-down
  :description "cursor-ri at the scroll-top scrolls the region down instead of moving up."
  (with-screen (s 5 5)
    ;; Write a recognisable line at the top, then move to top and reverse-index
    (feed s "LINE0")
    (cl-tmux/terminal/actions:set-cursor s 0 0)
    (cl-tmux/terminal/actions:cursor-ri s)
    ;; The old row 0 should now be at row 1, and row 0 should be blank
    (is (row-blank-p s 0) "row 0 must be blank after reverse index scroll")
    (check-row s 1 "LINE0")))

(test cursor-ri-at-scroll-top-non-default-region
  :description "cursor-ri at a custom scroll-top scrolls that region only."
  (with-screen (s 10 10)
    (setf (cl-tmux/terminal/types::screen-scroll-top    s) 3
          (cl-tmux/terminal/types::screen-scroll-bottom s) 7)
    (setf (cl-tmux/terminal/types::screen-cy s) 3)   ; at scroll-top
    (cl-tmux/terminal/actions:cursor-ri s)
    ;; cursor stays at scroll-top (scroll happened, not cursor move)
    (is (= 3 (screen-cursor-y s))
        "cursor-ri at scroll-top must stay at scroll-top after scrolling down")))

;;; ── write-char-at-cursor wide char ───────────────────────────────────────────

(test write-char-at-cursor-wide-char-wraps-at-right-edge
  "A double-width character that cannot fit in the last column wraps to the next row."
  (with-screen (s 3 3)
    ;; Position cursor at column 2 (last col of a 3-wide screen)
    (feed s "ab")                          ; cursor at col 2
    ;; Feed a wide (CJK) character -- must wrap since only 1 column remains
    (utf8-feed s "あ")
    (is (char= #\Space (char-at s 2 0))
        "last column of row 0 must be blank (wide char did not fit)")
    (is (char= #\あ (char-at s 0 1))
        "wide char must appear on row 1 after wrap")))

(test write-codepoint-places-character
  "write-codepoint converts a code point to a char and writes it at the cursor."
  (with-screen (s 10 5)
    (cl-tmux/terminal/actions:write-codepoint s 65)  ; U+0041 = 'A'
    (is (char= #\A (char-at s 0 0)))
    (check-cursor s 1 0)))

(test cursor-down-slash-scroll-advances-within-region
  "cursor-down/scroll increments cy when not at scroll-bottom."
  (with-screen (s 10 5)
    (cl-tmux/terminal/actions:cursor-down/scroll s)  ; internal
    (check-cursor s 0 1)))

(test cursor-down-slash-scroll-scrolls-at-bottom
  "cursor-down/scroll scrolls up when at scroll-bottom."
  (with-screen (s 5 3)
    (feed s "L0")
    (cl-tmux/terminal/actions:set-cursor s 0 2) ; bottom row
    (cl-tmux/terminal/actions:cursor-down/scroll s)
    ;; cursor must still be at row 2 (scroll happened)
    (check-cursor s 0 2)
    ;; scrollback must have grown
    (is (plusp (length (cl-tmux/terminal/types:screen-scrollback s)))
        "scroll-down should have pushed a row to scrollback")))

(test advance-cursor-stays-in-line-when-room
  "%advance-cursor increments cx when not at the right edge."
  (with-screen (s 10 5)
    (cl-tmux/terminal/actions::%advance-cursor s 3)
    (check-cursor s 3 0)))

(test advance-cursor-wraps-to-next-line-at-right-edge
  "%advance-cursor wraps cursor to column 0 of the next row when it would pass width."
  (with-screen (s 5 3)
    (cl-tmux/terminal/actions:set-cursor s 4 0)  ; last column
    (cl-tmux/terminal/actions::%advance-cursor s 1)
    (check-cursor s 0 1)))

;;; ── %advance-cursor no-wrap mode ─────────────────────────────────────────────
;;;
;;; Coverage gap: screen-autowrap=NIL clamps the cursor at the right edge instead
;;; of wrapping to the next row.  This path (lines 100-104 of cursor.lisp) was
;;; previously untested; modes-tests.lisp only verified the flag itself.

(test advance-cursor-clamps-when-autowrap-off
  "%advance-cursor with autowrap=NIL clamps the cursor to the last column and
   does NOT wrap to the next row when advancing past the right edge."
  (with-screen (s 5 3)
    ;; Disable autowrap
    (setf (cl-tmux/terminal/types:screen-autowrap s) nil)
    ;; Position at the last column
    (cl-tmux/terminal/actions:set-cursor s 4 0)
    ;; Attempt to advance by 1 (would normally wrap to row 1 col 0)
    (cl-tmux/terminal/actions::%advance-cursor s 1)
    ;; Cursor must remain at column 4 (width-1), row 0
    (check-cursor s 4 0)))

(test write-char-overwrites-at-right-edge-when-autowrap-off
  "With autowrap=NIL, writing a character at the rightmost column overwrites
   that cell in place; the cursor stays at the last column."
  (with-screen (s 5 3)
    (feed s (esc "[?7l"))        ; disable auto-wrap
    ;; Move to last column and write two chars
    (cl-tmux/terminal/actions:set-cursor s 4 0)
    (cl-tmux/terminal/actions:write-char-at-cursor s #\A)
    ;; Cursor still at last column (no wrap)
    (check-cursor s 4 0)
    ;; The cell at (4, 0) must contain A
    (is (char= #\A (char-at s 4 0))
        "char at right edge must be A, got ~C" (char-at s 4 0))
    ;; row 1 must be completely blank (no wrap occurred)
    (is (row-blank-p s 1)
        "row 1 must stay blank when autowrap is off")))

;;; ── define-cursor-movements macro ────────────────────────────────────────────

(test define-cursor-movements-macro-is-defined
  "define-cursor-movements is a defined macro in the actions package."
  (is (macro-function 'cl-tmux/terminal/actions::define-cursor-movements)))

(test define-cursor-movements-generates-all-four-functions
  "The four cursor movement functions are all fbound (generated by the macro)."
  (is (fboundp 'cl-tmux/terminal/actions:cursor-up)    "cursor-up must be fbound")
  (is (fboundp 'cl-tmux/terminal/actions:cursor-down)  "cursor-down must be fbound")
  (is (fboundp 'cl-tmux/terminal/actions:cursor-right) "cursor-right must be fbound")
  (is (fboundp 'cl-tmux/terminal/actions:cursor-left)  "cursor-left must be fbound"))

;;; ── SUITE: %place-wide-char ──────────────────────────────────────────────────

(def-suite place-wide-char-suite
  :description "%place-wide-char lead and continuation cell layout"
  :in terminal-suite)
(in-suite place-wide-char-suite)

(test place-wide-char-writes-lead-and-continuation
  :description "%place-wide-char writes a width-2 lead cell and width-0 continuation."
  (with-screen (s 10 5)
    (cl-tmux/terminal/actions::%place-wide-char s 0 0 #\中 7 0 0 0 0)
    (let ((lead (screen-cell s 0 0))
          (cont (screen-cell s 1 0)))
      (is (char= #\中 (cell-char lead)) "lead cell char must be 中")
      (is (= 2 (cell-width lead))       "lead cell width must be 2")
      (is (= 0 (cell-width cont))       "continuation cell width must be 0"))))

(test place-wide-char-at-last-column-no-continuation
  :description "%place-wide-char at the last column skips writing the continuation cell."
  (with-screen (s 5 5)
    ;; Place a wide char at x=4 (last column); x+1=5 >= width, so no continuation
    (cl-tmux/terminal/actions::%place-wide-char s 4 0 #\中 7 0 0 0 0)
    (let ((lead (screen-cell s 4 0)))
      (is (char= #\中 (cell-char lead)) "lead cell must be 中 even at last column")
      (is (= 2 (cell-width lead))       "lead cell width must be 2"))))

;;; ── SUITE: table-driven cursor movement ──────────────────────────────────────
;;;
;;; Repeated cursor-up/down/left/right cases at count=1 form a natural table.

(def-suite cursor-movement-table
  :description "Table-driven single-step cursor movement"
  :in terminal-suite)
(in-suite cursor-movement-table)

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
          (setf (cl-tmux/terminal/types::screen-cx s) sx
                (cl-tmux/terminal/types::screen-cy s) sy)
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
