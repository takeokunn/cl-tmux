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

;;; ── Shared fixture: with-cursor-at ──────────────────────────────────────────
;;;
;;; Several tests position the cursor column (and, optionally, row) before
;;; calling an action function.  This macro captures the repeated
;;; (setf screen-cursor-x ...) / (setf screen-cursor-y ...) inline setup.

(defmacro with-cursor-at ((screen-var w h x &optional (y 0)) &body body)
  "Bind SCREEN-VAR to a W×H screen with the cursor initially at column X,
   row Y (defaulting to row 0)."
  `(with-screen (,screen-var ,w ,h)
     (setf (cl-tmux/terminal/types:screen-cursor-x ,screen-var) ,x
           (cl-tmux/terminal/types:screen-cursor-y ,screen-var) ,y)
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

(test cursor-horizontal-clamping-table
  "cursor-left clamps to column 0; cursor-right clamps to width-1 (9)."
  (dolist (row (list (list 5 #'cl-tmux/terminal/actions::cursor-left  0 "cursor-left  clamps to 0")
                     (list 2 #'cl-tmux/terminal/actions::cursor-right 9 "cursor-right clamps to 9")))
    (destructuring-bind (init-x fn expected desc) row
      (with-cursor-at (s 10 10 init-x)
        (funcall fn s 100)
        (is (= expected (screen-cursor-x s)) "~A (got ~D)" desc (screen-cursor-x s))))))

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

(test set-cursor-table
  "set-cursor moves cursor to (x,y) within bounds, clamping out-of-range values to screen edges."
  (dolist (row '(( 3  7  3  7 "in-bounds: (3,7) → cursor at (3,7)")
                 (99  0  9  0 "x ≥ width → clamped to width-1=9")
                 ( 0 99  0  9 "y ≥ height → clamped to height-1=9")
                 (-5  3  0  3 "negative x → clamped to 0")))
    (destructuring-bind (x y expected-x expected-y desc) row
      (with-screen (s 10 10)
        (cl-tmux/terminal/actions:set-cursor s x y)
        (is (= expected-x (screen-cursor-x s)) "~A: cursor-x" desc)
        (is (= expected-y (screen-cursor-y s)) "~A: cursor-y" desc)))))

;;; ── SUITE: direct-action-cursor ─────────────────────────────────────────────
;;;
;;; These tests call action functions directly rather than through
;;; screen-process-bytes, targeting edge cases that the CSI/parser path
;;; may not hit explicitly.

(def-suite direct-action-cursor
  :description "Direct calls to cursor-bs/cr/lf/ht/ri, write-codepoint, scroll helpers"
  :in terminal-suite)
(in-suite direct-action-cursor)

(test cursor-bs-moves-left-and-clamps-at-zero
  "cursor-bs decrements the cursor column by 1; at column 0 it is a no-op."
  (with-screen (s 10 5)
    (feed s "abc")
    (cl-tmux/terminal/actions:cursor-bs s)
    (check-cursor s 2 0))
  (with-screen (s 10 5)
    (cl-tmux/terminal/actions:cursor-bs s)
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

(test cursor-cht-count-table
  "cursor-cht advances N 8-column tab stops from col 0; n=0 is treated as 1."
  (dolist (row '((2 16 "n=2: advance 2 stops → col 16")
                 (0  8 "n=0: treated as 1 stop → col 8")))
    (destructuring-bind (n expected desc) row
      (with-screen (s 40 5)
        (cl-tmux/terminal/actions:cursor-cht s n)
        (is (= expected (screen-cursor-x s)) "~A" desc)))))

(test cursor-cht-one-is-same-as-cursor-ht
  :description "cursor-cht 1 behaves identically to cursor-ht."
  (let ((s1 (make-screen 20 5))
        (s2 (make-screen 20 5)))
    (cl-tmux/terminal/actions:cursor-ht  s1)
    (cl-tmux/terminal/actions:cursor-cht s2 1)
    (is (= (screen-cursor-x s1) (screen-cursor-x s2))
        "cursor-cht 1 must give same result as cursor-ht")))

;;; ── cursor-cbt (CBT — cursor backward tab stops) ─────────────────────────────

(test cursor-cbt-table
  "cursor-cbt moves back N 8-column tab stops, clamping at col 0; n=0 is treated as 1."
  (dolist (row '((16  2  0 "from col 16, back 2 stops: 16→8→0")
                 ( 5 99  0 "large n clamps at col 0")
                 (16  0  8 "n=0 treated as 1: 16→8")))
    (destructuring-bind (start-col n expected desc) row
      (with-cursor-at (s 40 5 start-col)
        (cl-tmux/terminal/actions:cursor-cbt s n)
        (is (= expected (screen-cursor-x s)) "~A: got ~D" desc (screen-cursor-x s))))))

;;; ── HTS / TBC custom tab stops (ESC H / CSI g) ───────────────────────────────

(test hts-set-tab-stop-makes-cursor-ht-land-on-custom-stop
  :description "set-tab-stop (HTS) adds a stop at the cursor column; cursor-ht lands on it."
  (with-cursor-at (s 40 5 3)
    (cl-tmux/terminal/actions:set-tab-stop s)        ; HTS at col 3
    (setf (cl-tmux/terminal/types:screen-cursor-x s) 0)
    (cl-tmux/terminal/actions:cursor-ht s)           ; HT from col 0
    (is (= 3 (screen-cursor-x s))
        "after HTS at col 3, HT from col 0 must land on the custom stop 3")))

(test tbc-3-clears-all-stops-so-ht-goes-to-last-column
  :description "clear-tab-stops 3 (TBC 3) removes every stop; HT then goes to width-1."
  (with-cursor-at (s 40 5 0)
    (cl-tmux/terminal/actions:clear-tab-stops s 3)   ; TBC 3 — clear all
    (cl-tmux/terminal/actions:cursor-ht s)
    (is (= 39 (screen-cursor-x s))
        "with no tab stops, HT must advance to the last column (width-1)")))

(test tbc-0-clears-stop-at-cursor-column
  :description "clear-tab-stops 0 (TBC 0) removes the default stop at the cursor column."
  (with-cursor-at (s 40 5 8)
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
