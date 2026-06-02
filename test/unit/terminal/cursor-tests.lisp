(in-package #:cl-tmux/test)

;;;; Cursor movement clamping tests (src/terminal/cursor.lisp).
;;;; Tests: scroll-region-clamp suite.

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

;;; ── SUITE: direct-action-cursor ─────────────────────────────────────────────
;;;
;;; These tests call action functions directly rather than through
;;; screen-process-bytes, targeting edge cases that the CSI/parser path
;;; may not hit explicitly.

(def-suite direct-action-cursor
  :description "Direct calls to cursor-bs/cr/lf/ht, write-codepoint, scroll helpers"
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
