(in-package #:cl-tmux/test)

;;;; Cursor movement and character-writing tests (src/terminal/cursor.lisp).
;;;; Tests: scroll-region clamping, direct action functions, %advance-cursor
;;;;        autowrap behaviour, set-cursor, cursor-ri, cursor-cht/cbt,
;;;;        combining-char handling, DEC-graphics remapping, and %place-wide-char.
;;;; Also covers: combining-char-p predicate, DEC graphics charset remapping,
;;;;              write-char-at-cursor combining-char path, write-codepoint.

;;; ── Shared fixture: with-scroll-region ──────────────────────────────────────
;;;
;;; Several tests in this file set a non-trivial scroll region and position the
;;; cursor within it before calling action functions.  This macro captures the
;;; repeated setup to remove the inline duplication.

(defmacro with-scroll-region ((screen-var w h top bottom cy) &body body)
  "Bind SCREEN-VAR to a W×H screen with scroll region TOP..BOTTOM and cursor
   initially on row CY.  Used by scroll-region clamping and cursor-ri tests."
  `(with-screen (,screen-var ,w ,h)
     (setf (cl-tmux/terminal/types::screen-scroll-top    ,screen-var) ,top
           (cl-tmux/terminal/types::screen-scroll-bottom ,screen-var) ,bottom
           (cl-tmux/terminal/types::screen-cursor-y      ,screen-var) ,cy)
     ,@body))

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
  (with-scroll-region (s 10 10 3 7 6)
    (cl-tmux/terminal/actions::cursor-up s 100)
    (is (= 3 (screen-cursor-y s))
        "cursor-up should clamp to scroll-top 3, got ~D" (screen-cursor-y s))
    ;; Sanity: it stopped at scroll-top, not at the screen top (0).
    (is (/= 0 (screen-cursor-y s))
        "cursor-up must not pass scroll-top down to row 0")))

(test cursor-down-clamps-to-scroll-bottom
  "cursor-down with a large count stops at scroll-bottom, NOT at height-1."
  (with-scroll-region (s 10 10 3 7 4)
    (cl-tmux/terminal/actions::cursor-down s 100)
    (is (= 7 (screen-cursor-y s))
        "cursor-down should clamp to scroll-bottom 7, got ~D" (screen-cursor-y s))
    ;; Sanity: it stopped at scroll-bottom, not at the screen bottom (9).
    (is (/= 9 (screen-cursor-y s))
        "cursor-down must not pass scroll-bottom down to height-1")))

(test cursor-left-clamps-to-column-zero
  "cursor-left with a large count clamps to column 0."
  (with-screen (s 10 10)
    (setf (cl-tmux/terminal/types:screen-cursor-x s) 5)
    (cl-tmux/terminal/actions::cursor-left s 100)
    (is (= 0 (screen-cursor-x s))
        "cursor-left should clamp to column 0, got ~D" (screen-cursor-x s))))

(test cursor-right-clamps-to-width-minus-one
  "cursor-right with a large count clamps to width-1."
  (with-screen (s 10 10)
    (setf (cl-tmux/terminal/types:screen-cursor-x s) 2)
    (cl-tmux/terminal/actions::cursor-right s 100)
    (is (= 9 (screen-cursor-x s))
        "cursor-right should clamp to width-1 (9), got ~D" (screen-cursor-x s))))

(test cursor-up-down-respect-region-from-mid
  "From a row inside the region, a small cursor-up/down stays within the
   region and does not overshoot the margins."
  (with-scroll-region (s 10 10 2 8 5)
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
    (setf (cl-tmux/terminal/types:screen-cursor-x s) 16)
    ;; Back 2 stops: 16 → 8 → 0
    (cl-tmux/terminal/actions:cursor-cbt s 2)
    (is (= 0 (screen-cursor-x s))
        "cursor-cbt 2 from col 16 must reach col 0")))

(test cursor-cbt-clamps-at-column-zero
  :description "cursor-cbt with a large count stops at column 0."
  (with-screen (s 40 5)
    (setf (cl-tmux/terminal/types:screen-cursor-x s) 5)
    (cl-tmux/terminal/actions:cursor-cbt s 99)
    (is (= 0 (screen-cursor-x s))
        "cursor-cbt must not go past column 0")))

(test cursor-cbt-zero-treated-as-one
  :description "cursor-cbt 0 moves back one tab stop."
  (with-screen (s 40 5)
    (setf (cl-tmux/terminal/types:screen-cursor-x s) 16)
    (cl-tmux/terminal/actions:cursor-cbt s 0)
    (is (= 8 (screen-cursor-x s))
        "cursor-cbt 0 must move back one tab stop")))

;;; ── HTS / TBC custom tab stops (ESC H / CSI g) ───────────────────────────────

(test hts-set-tab-stop-makes-cursor-ht-land-on-custom-stop
  :description "set-tab-stop (HTS) adds a stop at the cursor column; cursor-ht lands on it."
  (with-screen (s 40 5)
    (setf (cl-tmux/terminal/types:screen-cursor-x s) 3)
    (cl-tmux/terminal/actions:set-tab-stop s)        ; HTS at col 3
    (setf (cl-tmux/terminal/types:screen-cursor-x s) 0)
    (cl-tmux/terminal/actions:cursor-ht s)           ; HT from col 0
    (is (= 3 (screen-cursor-x s))
        "after HTS at col 3, HT from col 0 must land on the custom stop 3")))

(test tbc-3-clears-all-stops-so-ht-goes-to-last-column
  :description "clear-tab-stops 3 (TBC 3) removes every stop; HT then goes to width-1."
  (with-screen (s 40 5)
    (cl-tmux/terminal/actions:clear-tab-stops s 3)   ; TBC 3 — clear all
    (setf (cl-tmux/terminal/types:screen-cursor-x s) 0)
    (cl-tmux/terminal/actions:cursor-ht s)
    (is (= 39 (screen-cursor-x s))
        "with no tab stops, HT must advance to the last column (width-1)")))

(test tbc-0-clears-stop-at-cursor-column
  :description "clear-tab-stops 0 (TBC 0) removes the default stop at the cursor column."
  (with-screen (s 40 5)
    (setf (cl-tmux/terminal/types:screen-cursor-x s) 8)
    (cl-tmux/terminal/actions:clear-tab-stops s 0)   ; TBC 0 at col 8
    (setf (cl-tmux/terminal/types:screen-cursor-x s) 0)
    (cl-tmux/terminal/actions:cursor-ht s)
    (is (= 16 (screen-cursor-x s))
        "after clearing the stop at col 8, HT from col 0 must skip to 16")))

(test esc-h-hts-sets-tab-stop-via-parser
  :description "ESC H (HTS) through the parser sets a tab stop at the cursor column."
  (with-screen (s 40 5)
    (feed s (esc "[1;4H"))   ; CUP → col 4 (1-based) = col 3 (0-based)
    (feed s (esc "H"))       ; ESC H → HTS at col 3
    (feed s (esc "[1;1H"))   ; CUP → col 0
    (feed s (string (code-char 9)))  ; HT → custom stop 3
    (is (= 3 (screen-cursor-x s))
        "ESC H then HT must land on the custom tab stop")))

(test csi-3-g-tbc-clears-all-stops-via-parser
  :description "CSI 3 g (TBC) through the parser clears all tab stops."
  (with-screen (s 40 5)
    (feed s (esc "[3g"))     ; CSI 3 g → TBC clear all
    (feed s (esc "[1;1H"))   ; cursor to col 0
    (feed s (string (code-char 9)))  ; HT → last column
    (is (= 39 (screen-cursor-x s))
        "after CSI 3 g, HT must advance to the last column")))

;;; ── cursor-ri (ESC M — reverse index) ───────────────────────────────────────

(test cursor-ri-moves-up-within-region
  :description "cursor-ri moves the cursor up one row when not at the scroll-top."
  (with-screen (s 10 10)
    (setf (cl-tmux/terminal/types:screen-cursor-y s) 5)
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
  (with-scroll-region (s 10 10 3 7 3)
    (cl-tmux/terminal/actions:cursor-ri s)
    ;; cursor stays at scroll-top (scroll happened, not cursor move)
    (is (= 3 (screen-cursor-y s))
        "cursor-ri at scroll-top must stay at scroll-top after scrolling down")))

;;; ── cursor-nel (ESC E — Next Line) ──────────────────────────────────────────
;;;
;;; cursor-nel is exported from cl-tmux/terminal/actions and used by the parser
;;; (ESC E handler).  It performs CR then LF: moves the cursor to column 0 of
;;; the next row, scrolling at the bottom margin exactly like LF.

(def-suite cursor-nel-suite
  :description "cursor-nel: composite CR+LF with scroll at the bottom margin"
  :in terminal-suite)
(in-suite cursor-nel-suite)

(test cursor-nel-moves-to-column-zero-of-next-row
  "cursor-nel from an interior column moves to col 0 of the next row."
  (with-screen (s 10 5)
    (feed s "hello")                       ; cursor at col 5, row 0
    (cl-tmux/terminal/actions:cursor-nel s)
    (check-cursor s 0 1)))

(test cursor-nel-at-right-edge-moves-to-next-row
  "cursor-nel from the last column also moves to col 0 of the next row."
  (with-screen (s 5 5)
    (setf (cl-tmux/terminal/types:screen-cursor-x s) 4
          (cl-tmux/terminal/types:screen-cursor-y s) 2)
    (cl-tmux/terminal/actions:cursor-nel s)
    (check-cursor s 0 3)))

(test cursor-nel-at-bottom-margin-scrolls
  "cursor-nel at the bottom of the scroll region scrolls up; cursor stays on the
   bottom row at column 0 (identical behaviour to LF at the bottom margin)."
  (with-screen (s 5 3)
    (feed s "LINE0")
    (cl-tmux/terminal/actions:set-cursor s 3 2)  ; col 3, last row
    (cl-tmux/terminal/actions:cursor-nel s)
    ;; After scroll, cursor is on row 2 col 0; the old row 0 is in scrollback.
    (check-cursor s 0 2)
    (is (plusp (length (cl-tmux/terminal/types:screen-scrollback s)))
        "cursor-nel at bottom margin must push a row to scrollback")))

(test cursor-nel-via-parser
  "ESC E (NEL) through the parser advances to col 0 of the next row."
  (with-screen (s 10 5)
    (feed s "hello")                       ; cursor at col 5, row 0
    (feed s (esc "E"))                     ; ESC E = NEL
    (check-cursor s 0 1)))

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

(test advance-cursor-defers-wrap-at-right-edge
  "%advance-cursor at the right margin DEFERS the wrap (VT100 last-column flag):
   the cursor parks at the last column and pending-wrap is set, rather than
   wrapping immediately."
  (with-screen (s 5 3)
    (cl-tmux/terminal/actions:set-cursor s 4 0)  ; last column
    (cl-tmux/terminal/actions::%advance-cursor s 1)
    (check-cursor s 4 0)                          ; parked, NOT wrapped
    (is-true (cl-tmux/terminal/types:screen-pending-wrap s)
             "pending-wrap must be set after advancing past the right margin")))

(test deferred-wrap-next-char-wraps-and-clears
  "A character written while pending-wrap is set wraps to col 0 of the next row
   first, then writes there; pending-wrap is cleared."
  (with-screen (s 3 3)
    (feed s "abc")                  ; fills row 0; cursor parks at col 2, wrap pending
    (is-true (cl-tmux/terminal/types:screen-pending-wrap s) "wrap pending after full row")
    (check-cursor s 2 0)            ; parked at last column of row 0
    (feed s "d")                    ; triggers the deferred wrap
    (is (char= #\d (char-at s 0 1)) "next char must land at col 0 of row 1")
    (check-cursor s 1 1)
    (is-false (cl-tmux/terminal/types:screen-pending-wrap s) "pending-wrap cleared")))

(test deferred-wrap-newline-no-spurious-blank-line
  "Filling a row then CR/LF must NOT insert a blank line: the next write lands on
   the immediately following row (the classic pending-wrap correctness case)."
  (with-screen (s 3 4)
    (feed s "abc")                  ; fills row 0 (pending wrap)
    (feed s (format nil "~C~C" #\Return #\Linefeed))  ; CR LF
    (feed s "d")
    (is (char= #\a (char-at s 0 0)) "row 0 keeps its content")
    (is (char= #\d (char-at s 0 1)) "d lands on row 1, col 0 — no blank line")
    (is (char= #\Space (char-at s 1 1))
        "row 1 has only 'd' at col 0 (no content pushed down a line)")))

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

;;; ── cursor movement behavioral tests ────────────────────────────────────────
;;;
;;; These tests verify observable behavior: that cursor-up, cursor-down,
;;; cursor-left, and cursor-right actually move the cursor as documented.
;;; They replace implementation-probing tests that only checked fbound status.

(test cursor-up-moves-cursor-by-n-rows
  "cursor-up N decrements the cursor row by N (within the scroll region)."
  (with-screen (s 10 10)
    (setf (cl-tmux/terminal/types::screen-cursor-y s) 6)
    (cl-tmux/terminal/actions:cursor-up s 2)
    (is (= 4 (screen-cursor-y s))
        "cursor-up 2 from row 6 must reach row 4, got ~D" (screen-cursor-y s))))

(test cursor-down-moves-cursor-by-n-rows
  "cursor-down N increments the cursor row by N (within the scroll region)."
  (with-screen (s 10 10)
    (setf (cl-tmux/terminal/types::screen-cursor-y s) 3)
    (cl-tmux/terminal/actions:cursor-down s 3)
    (is (= 6 (screen-cursor-y s))
        "cursor-down 3 from row 3 must reach row 6, got ~D" (screen-cursor-y s))))

(test cursor-left-moves-cursor-by-n-columns
  "cursor-left N decrements the cursor column by N."
  (with-screen (s 10 10)
    (setf (cl-tmux/terminal/types::screen-cursor-x s) 7)
    (cl-tmux/terminal/actions:cursor-left s 3)
    (is (= 4 (screen-cursor-x s))
        "cursor-left 3 from col 7 must reach col 4, got ~D" (screen-cursor-x s))))

(test cursor-right-moves-cursor-by-n-columns
  "cursor-right N increments the cursor column by N."
  (with-screen (s 10 10)
    (setf (cl-tmux/terminal/types::screen-cursor-x s) 2)
    (cl-tmux/terminal/actions:cursor-right s 4)
    (is (= 6 (screen-cursor-x s))
        "cursor-right 4 from col 2 must reach col 6, got ~D" (screen-cursor-x s))))

;;; ── SUITE: %place-wide-char ──────────────────────────────────────────────────
