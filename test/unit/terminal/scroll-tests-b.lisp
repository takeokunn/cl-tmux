(in-package #:cl-tmux/test)

;;;; scroll tests — part B: direct-row-primitives (%copy-row, %clear-row),
;;;; direct-action-erase suites, and scroll edge cases.

;;; ── SUITE: direct-row-primitives ────────────────────────────────────────────
;;;
;;; Coverage gap: %copy-row and %clear-row are used by scroll and edit operations
;;; but were previously only tested indirectly.  These tests call them directly.

(def-suite direct-row-primitives
  :description "Direct calls to %copy-row and %clear-row row primitives"
  :in terminal-suite)
(in-suite direct-row-primitives)

(test copy-row-copies-all-cells
  "%copy-row copies every cell from the source row to the destination row."
  (with-screen (s 5 3)
    (feed s "hello")                       ; row 0 = "hello"
    (cl-tmux/terminal/actions::%copy-row s 1 0)  ; copy row 0 to row 1
    (is (string= "hello" (row-string s 1))
        "row 1 must equal row 0 after %copy-row, got ~S"
        (row-string s 1))))

(test clear-row-blanks-all-cells
  "%clear-row replaces every cell in the target row with a blank cell."
  (with-screen (s 5 3)
    (feed s "hello")                       ; row 0 = "hello"
    (cl-tmux/terminal/actions::%clear-row s 0)
    (is (row-blank-p s 0) "row 0 must be blank after %clear-row")))

(test trim-scroll-history-caps-at-limit
  "trim-scroll-history removes entries beyond the effective history-limit."
  (with-screen (s 5 3)
    (let ((cap 5))
      ;; Pre-populate scrollback beyond the cap
      (setf (cl-tmux/terminal/types:screen-scrollback s)
            (loop repeat (+ cap 3)
                  collect (make-array 5 :initial-element
                                        (cl-tmux/terminal/types:blank-cell))))
      ;; Install a temporary limit function
      (let ((cl-tmux/terminal/actions:*history-limit-function* (lambda () cap)))
        (cl-tmux/terminal/actions:trim-scroll-history s))
      (is (<= (length (cl-tmux/terminal/types:screen-scrollback s)) cap)
          "scrollback must not exceed cap (~D) after trim-scroll-history" cap))))

;;; ── SUITE: direct-action-erase ───────────────────────────────────────────────
;;;
;;; These tests call erase-region, erase-display, erase-line directly rather
;;; than through the CSI parser path, targeting edge cases that high-level
;;; tests are unlikely to assert explicitly.

(def-suite direct-action-erase
  :description "Direct calls to erase-region, erase-display (mode 3), erase-line"
  :in terminal-suite)
(in-suite direct-action-erase)

(test erase-region-clears-span-across-rows
  "erase-region blanks a linear span from (x0,y0) to (x1,y1) inclusive."
  (with-screen (s 5 4)
    (feed s "aabbccddee")           ; rows 0 and 1 filled
    ;; Erase from (3,0) to (1,1): last 2 cells of row 0 + first 2 of row 1.
    (cl-tmux/terminal/actions:erase-region s 3 0 1 1)
    (is (char= #\a (char-at s 0 0)) "col 0 row 0 must be preserved")
    (is (char= #\a (char-at s 1 0)) "col 1 row 0 must be preserved")
    (is (char= #\b (char-at s 2 0)) "col 2 row 0 must be preserved")
    (is (char= #\Space (char-at s 3 0)) "col 3 row 0 must be erased")
    (is (char= #\Space (char-at s 4 0)) "col 4 row 0 must be erased")
    (is (char= #\Space (char-at s 0 1)) "col 0 row 1 must be erased")
    (is (char= #\Space (char-at s 1 1)) "col 1 row 1 must be erased")))

(test erase-display-mode-3-clears-scrollback
  "erase-display mode 3 (ED 3) also clears the scrollback buffer."
  (with-screen (s 5 3)
    ;; Build up some scrollback by feeding lines that force scrolling.
    (feed-lines s "L0" "L1" "L2" "L3")
    (is (plusp (length (cl-tmux/terminal/types:screen-scrollback s)))
        "scrollback must be non-empty after filling the screen")
    ;; Mode 3 = clear screen + clear scrollback
    (cl-tmux/terminal/actions:erase-display s 3)
    (is (null (cl-tmux/terminal/types:screen-scrollback s))
        "scrollback must be NIL after erase-display mode 3")))

(test erase-line-mode-0-erases-to-end
  "erase-line mode 0 erases from the cursor column to the end of the line."
  (with-screen (s 10 5)
    (feed s "hello")
    ;; Move cursor to col 2 via cursor-left.
    (cl-tmux/terminal/actions:cursor-left s 3)   ; cursor at col 2
    (cl-tmux/terminal/actions:erase-line s 0)
    (is (char= #\h (char-at s 0 0)) "col 0 must be preserved")
    (is (char= #\e (char-at s 1 0)) "col 1 must be preserved")
    (is (char= #\Space (char-at s 2 0)) "col 2 must be erased")
    (is (char= #\Space (char-at s 4 0)) "col 4 must be erased")))

;;; ── SUITE: direct-decstbm ─────────────────────────────────────────────────────
;;;
;;; Direct tests for the decstbm function, covering boundary conditions
;;; that the CSI parser integration tests do not exercise explicitly.

(def-suite direct-decstbm
  :description "Direct calls to decstbm scroll-region setter"
  :in terminal-suite)
(in-suite direct-decstbm)

(test decstbm-valid-region-sets-scroll-boundaries
  "decstbm with a valid top < bottom sets scroll-top and scroll-bottom."
  (with-screen (s 5 5)
    (cl-tmux/terminal/actions:decstbm s 1 3)
    (is (= 1 (cl-tmux/terminal/types:screen-scroll-top s))
        "scroll-top must be 1 after decstbm 1 3")
    (is (= 3 (cl-tmux/terminal/types:screen-scroll-bottom s))
        "scroll-bottom must be 3 after decstbm 1 3")))

(test decstbm-valid-region-homes-cursor
  "decstbm with a valid region homes the cursor to (0,0)."
  (with-screen (s 5 5)
    (cl-tmux/terminal/actions:set-cursor s 3 3)
    (cl-tmux/terminal/actions:decstbm s 0 4)
    (check-cursor s 0 0)))

(test decstbm-equal-top-bottom-is-rejected
  "decstbm with top == bottom does not change the scroll region."
  (with-screen (s 5 5)
    ;; Default scroll region is 0..4.
    (let ((orig-top    (cl-tmux/terminal/types:screen-scroll-top s))
          (orig-bottom (cl-tmux/terminal/types:screen-scroll-bottom s)))
      (cl-tmux/terminal/actions:decstbm s 2 2)  ; top = bottom = 2
      (is (= orig-top    (cl-tmux/terminal/types:screen-scroll-top s))
          "scroll-top must not change when top == bottom")
      (is (= orig-bottom (cl-tmux/terminal/types:screen-scroll-bottom s))
          "scroll-bottom must not change when top == bottom"))))

(test decstbm-inverted-region-is-rejected
  "decstbm with top > bottom does not change the scroll region."
  (with-screen (s 5 5)
    (let ((orig-top    (cl-tmux/terminal/types:screen-scroll-top s))
          (orig-bottom (cl-tmux/terminal/types:screen-scroll-bottom s)))
      (cl-tmux/terminal/actions:decstbm s 4 1)  ; top > bottom — invalid
      (is (= orig-top    (cl-tmux/terminal/types:screen-scroll-top s))
          "scroll-top must not change for inverted region")
      (is (= orig-bottom (cl-tmux/terminal/types:screen-scroll-bottom s))
          "scroll-bottom must not change for inverted region"))))

(test decstbm-out-of-range-clamped-to-screen
  "decstbm clamps out-of-range values to the screen height."
  (with-screen (s 5 5)
    ;; Negative top → clamped to 0; bottom beyond height-1 → clamped to 4.
    (cl-tmux/terminal/actions:decstbm s -5 99)
    (is (= 0 (cl-tmux/terminal/types:screen-scroll-top s))
        "negative top must be clamped to 0")
    (is (= 4 (cl-tmux/terminal/types:screen-scroll-bottom s))
        "bottom beyond height-1 must be clamped to 4")))

;;; ── SUITE: constrained-scroll ─────────────────────────────────────────────────
;;;
;;; Tests that scroll-up-one and scroll-down-one respect an active scroll region
;;; set by decstbm and leave rows outside the region untouched.
;;;
;;; The shared with-5-row-scroll-region fixture eliminates the repeated inline
;;; 5-row fill + decstbm setup pattern from both tests.

(def-suite constrained-scroll
  :description "Scroll operations respect a restricted scroll region"
  :in terminal-suite)
(in-suite constrained-scroll)

(defmacro with-5-row-scroll-region ((screen-var) &body body)
  "Bind SCREEN-VAR to a 5-row screen with rows labeled R0-R4 and scroll
   region restricted to rows 1-3.  Used by constrained-scroll tests."
  `(with-screen (,screen-var 5 5)
     (feed-lines ,screen-var "R0" "R1" "R2" "R3" "R4")
     (cl-tmux/terminal/actions:decstbm ,screen-var 1 3)
     ,@body))

(test scroll-up-one-respects-scroll-region
  "scroll-up-one moves only the rows within the active scroll region."
  (with-5-row-scroll-region (s)
    (cl-tmux/terminal/actions:scroll-up-one s)
    ;; Row 0 must be untouched (outside the scroll region).
    (check-row s 0 "R0")
    ;; Row 4 must also be untouched.
    (check-row s 4 "R4")))

(test scroll-down-one-respects-scroll-region
  "scroll-down-one moves only the rows within the active scroll region."
  (with-5-row-scroll-region (s)
    (cl-tmux/terminal/actions:scroll-down-one s)
    ;; Row 0 must be untouched.
    (check-row s 0 "R0")
    ;; Row 4 must be untouched.
    (check-row s 4 "R4")
    ;; Row 1 (the new top of the region) must be blank.
    (is (row-blank-p s 1) "row 1 (top of scroll region) must be blank after scroll-down-one")))

;;; ── SUITE: scroll-dirty-flag ─────────────────────────────────────────────────
;;;
;;; Both scroll-up-one and scroll-down-one must mark screen-dirty-p after they
;;; operate, so the renderer knows a repaint is needed.

(def-suite scroll-dirty-flag
  :description "scroll-up-one and scroll-down-one set screen-dirty-p"
  :in terminal-suite)
(in-suite scroll-dirty-flag)

(test scroll-up-one-marks-screen-dirty
  "scroll-up-one sets screen-dirty-p to T."
  (with-screen (s 5 3)
    (screen-clear-dirty s)
    (is-false (cl-tmux/terminal/types:screen-dirty-p s)
              "dirty flag must be NIL before scroll-up-one")
    (cl-tmux/terminal/actions:scroll-up-one s)
    (is (cl-tmux/terminal/types:screen-dirty-p s)
        "screen must be marked dirty after scroll-up-one")))

(test scroll-down-one-marks-screen-dirty
  "scroll-down-one sets screen-dirty-p to T."
  (with-screen (s 5 3)
    (screen-clear-dirty s)
    (is-false (cl-tmux/terminal/types:screen-dirty-p s)
              "dirty flag must be NIL before scroll-down-one")
    (cl-tmux/terminal/actions:scroll-down-one s)
    (is (cl-tmux/terminal/types:screen-dirty-p s)
        "screen must be marked dirty after scroll-down-one")))

;;; ── SUITE: history-limit-function nil path ────────────────────────────────────
;;;
;;; When *history-limit-function* is NIL, trim-scroll-history falls back to
;;; +max-scrollback-lines+.  %effective-history-limit must return a positive
;;; integer in this case.

(def-suite history-limit-fn-nil
  :description "*history-limit-function* NIL falls back to +max-scrollback-lines+"
  :in terminal-suite)
(in-suite history-limit-fn-nil)

(test history-limit-fn-nil-falls-back-to-constant
  "*history-limit-function* = NIL causes trim-scroll-history to use +max-scrollback-lines+."
  (with-screen (s 5 3)
    (let ((cap cl-tmux/config:+max-scrollback-lines+))
      ;; Pre-populate scrollback at the cap
      (setf (cl-tmux/terminal/types:screen-scrollback s)
            (loop repeat cap
                  collect (make-array 5 :initial-element
                                        (cl-tmux/terminal/types:blank-cell))))
      ;; With *history-limit-fn* bound to NIL, push one more row
      (let ((cl-tmux/terminal/actions:*history-limit-function* nil))
        (cl-tmux/terminal/actions:scroll-up-one s))
      ;; Scrollback must not exceed the constant cap
      (is (<= (length (cl-tmux/terminal/types:screen-scrollback s)) cap)
          "scrollback must not exceed +max-scrollback-lines+ (~D) when fn is NIL"
          cap))))

(test history-limit-fn-callback-overrides-constant
  "When *history-limit-function* returns a value, it overrides +max-scrollback-lines+."
  (with-screen (s 5 3)
    (let* ((custom-cap 3)
           (cl-tmux/terminal/actions:*history-limit-function* (lambda () custom-cap)))
      ;; Scroll enough to exceed the custom cap
      (dotimes (_ (+ custom-cap 5))
        (cl-tmux/terminal/actions:scroll-up-one s))
      (is (<= (length (cl-tmux/terminal/types:screen-scrollback s)) custom-cap)
          "scrollback must be capped at custom-cap (~D)" custom-cap))))

;;; ── SUITE: insert-lines / delete-lines direct calls ─────────────────────────
;;;
;;; insert-lines and delete-lines are generated by define-line-edit-rules and
;;; were previously only tested via the CSI path.  These direct-call tests
;;; cover edge cases: n=0, n > region-size.

(def-suite direct-line-edit
  :description "Direct calls to insert-lines and delete-lines"
  :in terminal-suite)
(in-suite direct-line-edit)

(test insert-lines-at-row-zero-pushes-content-down
  "insert-lines 1 at row 0 pushes all existing rows down."
  (with-screen (s 5 4)
    (feed-lines s "AA" "BB" "CC")
    (cl-tmux/terminal/actions:set-cursor s 0 0)
    (cl-tmux/terminal/actions:insert-lines s 1)
    ;; Row 0 must be blank (newly inserted); old row 0 moves to row 1.
    (is (row-blank-p s 0) "row 0 must be blank after insert-lines 1 at row 0")
    (check-row s 1 "AA")))

(test delete-lines-at-row-zero-pulls-content-up
  "delete-lines 1 at row 0 pulls all rows up; the bottom row becomes blank."
  (with-screen (s 5 4)
    (feed-lines s "AA" "BB" "CC" "DD")
    (cl-tmux/terminal/actions:set-cursor s 0 0)
    (cl-tmux/terminal/actions:delete-lines s 1)
    ;; Old row 1 ("BB") becomes row 0; bottom row becomes blank.
    (check-row s 0 "BB")
    (is (row-blank-p s 3) "bottom row must be blank after delete-lines 1 at row 0")))

(test insert-lines-ignored-when-cursor-above-scroll-region
  "IL is a no-op when the cursor is above the scroll-top (outside the region): it
   must not shift rows that lie outside the scroll region."
  (with-screen (s 5 5)
    (feed-lines s "AA" "BB" "CC" "DD" "EE")
    (feed s (esc "[3;5r"))      ; DECSTBM region rows 3-5 (0-based 2-4); homes cursor (0,0)
    (check-cursor s 0 0)        ; cursor is above scroll-top (row 2)
    (cl-tmux/terminal/actions:insert-lines s 1)
    (check-row s 0 "AA")        ; rows above the region must be untouched
    (check-row s 1 "BB")
    (check-row s 2 "CC")))

(test delete-lines-ignored-when-cursor-above-scroll-region
  "DL is likewise a no-op above the scroll region."
  (with-screen (s 5 5)
    (feed-lines s "AA" "BB" "CC" "DD" "EE")
    (feed s (esc "[3;5r"))      ; region rows 2-4 (0-based); cursor homed to (0,0)
    (cl-tmux/terminal/actions:delete-lines s 1)
    (check-row s 0 "AA")
    (check-row s 1 "BB")))

(test delete-lines-n-larger-than-region-blanks-all-region-rows
  "delete-lines with n >= region size blanks every row in the region."
  (with-screen (s 5 4)
    (feed-lines s "AA" "BB" "CC" "DD")
    (cl-tmux/terminal/actions:set-cursor s 0 0)
    (cl-tmux/terminal/actions:delete-lines s 99)
    ;; Every row must be blank
    (dotimes (y 4)
      (is (row-blank-p s y)
          "row ~D must be blank after oversized delete-lines" y))))

(test insert-lines-n-larger-than-region-blanks-all-region-rows
  "insert-lines with n >= region size blanks every row in the region."
  (with-screen (s 5 4)
    (feed-lines s "AA" "BB" "CC" "DD")
    (cl-tmux/terminal/actions:set-cursor s 0 0)
    (cl-tmux/terminal/actions:insert-lines s 99)
    ;; Every row must be blank
    (dotimes (y 4)
      (is (row-blank-p s y)
          "row ~D must be blank after oversized insert-lines" y))))

;;; ── SUITE: scroll-screen-to-history direct tests ─────────────────────────────
;;;
;;; Coverage gap: scroll-screen-to-history was only exercised indirectly through
;;; the scroll-on-clear integration test.  These direct tests verify:
;;;   1. Row-ordering in the scrollback (top row ends up oldest, newest-first list).
;;;   2. The alt-screen no-op guard.
;;;   3. That the history cap is respected after the push.

(def-suite scroll-screen-to-history-suite
  :description "Direct tests for scroll-screen-to-history row-ordering and guards"
  :in terminal-suite)
(in-suite scroll-screen-to-history-suite)

(test scroll-screen-to-history-pushes-all-rows
  "scroll-screen-to-history pushes every visible row into the scrollback buffer."
  (with-screen (s 5 3)
    (feed-lines s "AA" "BB" "CC")
    (cl-tmux/terminal/actions:scroll-screen-to-history s)
    (is (= 3 (length (cl-tmux/terminal/types:screen-scrollback s)))
        "scrollback must have 3 entries after pushing a 3-row screen")))

(test scroll-screen-to-history-top-row-is-oldest
  "scroll-screen-to-history pushes rows top→bottom so the top row ends up OLDEST
   (deepest in the newest-first scrollback list).  The bottom row is the most-recent
   (first) entry after the push."
  (with-screen (s 5 3)
    ;; Write distinct content so we can identify which row is which.
    (feed-lines s "ROW0" "ROW1" "ROW2")
    (cl-tmux/terminal/actions:scroll-screen-to-history s)
    (let ((scrollback (cl-tmux/terminal/types:screen-scrollback s)))
      ;; newest-first: index 0 = last pushed = row 2 (bottom row = most recent).
      (let ((newest-row (first scrollback))
            (oldest-row (first (last scrollback))))
        (is (char= #\R (cell-char (aref newest-row 0)))
            "first scrollback entry (newest) must start with 'R' from ROW2")
        ;; Distinguish ROW0 (oldest) from ROW2 (newest) by checking col 3:
        ;; ROW0 has char '0' at col 3 (0-indexed: R=0,O=1,W=2,0=3)
        ;; ROW2 has char '2' at col 3.
        (is (char= #\0 (cell-char (aref oldest-row 3)))
            "last scrollback entry (oldest) must be ROW0 — char at col 3 must be '0'")
        (is (char= #\2 (cell-char (aref newest-row 3)))
            "first scrollback entry (newest) must be ROW2 — char at col 3 must be '2'")))))

(test scroll-screen-to-history-is-noop-on-alt-screen
  "scroll-screen-to-history must be a no-op when the alternate screen is active."
  (with-screen (s 5 3)
    (feed-lines s "AA" "BB" "CC")
    ;; Enter the alternate screen.
    (cl-tmux/terminal/actions:enter-alt-screen s)
    (cl-tmux/terminal/actions:scroll-screen-to-history s)
    ;; Scrollback must remain empty — the alt screen has no history.
    (is (null (cl-tmux/terminal/types:screen-scrollback s))
        "scroll-screen-to-history must not push to scrollback on the alt screen")))

(test scroll-screen-to-history-respects-history-cap
  "scroll-screen-to-history enforces the history limit after pushing all rows."
  (with-screen (s 5 10)
    ;; Pre-fill 10 rows with content.
    (dotimes (_ 10) (feed s "AAAAA"))
    ;; Install a small cap so the push will trim.
    (let ((cl-tmux/terminal/actions:*history-limit-function* (lambda () 5)))
      (cl-tmux/terminal/actions:scroll-screen-to-history s))
    (is (<= (length (cl-tmux/terminal/types:screen-scrollback s)) 5)
        "scrollback must be trimmed to 5 after scroll-screen-to-history with cap 5")))

;;; ── SUITE: DEC Rectangle operations (DECERA / DECFRA / DECCRA) ───────────────
;;;
;;; Coverage gap: DECERA, DECFRA, and DECCRA had no dedicated unit tests.
;;; These tests cover normal operation, degenerate (empty) rectangles,
;;; out-of-bounds clamping, and overlapping DECCRA regions.

(def-suite dec-rect-ops-suite
  :description "DECERA / DECFRA / DECCRA DEC rectangle operation unit tests"
  :in terminal-suite)
(in-suite dec-rect-ops-suite)

;;; ── DECERA — Erase Rectangular Area ─────────────────────────────────────────

(test decera-erases-rectangle
  "DECERA blanks every cell in the specified 1-based rectangle."
  (with-screen (s 10 5)
    (feed s "AAAAAAAAAA")     ; row 0 = "AAAAAAAAAA"
    (feed s "BBBBBBBBBB")     ; row 1
    ;; Erase a 3×2 rectangle: rows 1-2 (1-based), cols 2-4 (1-based)
    (cl-tmux/terminal/actions:decera s 1 2 2 4)
    ;; Row 0 (1-based row 1), cols 1-3 (0-based) must be blank
    (is (char= #\Space (char-at s 1 0)) "decera: col 1 row 0 must be blank")
    (is (char= #\Space (char-at s 2 0)) "decera: col 2 row 0 must be blank")
    (is (char= #\Space (char-at s 3 0)) "decera: col 3 row 0 must be blank")
    ;; Cells outside the rectangle must be unchanged
    (is (char= #\A (char-at s 0 0)) "decera: col 0 row 0 must be preserved")
    (is (char= #\A (char-at s 4 0)) "decera: col 4 row 0 must be preserved")))

(test decera-degenerate-rectangle-is-noop
  "DECERA with a degenerate rectangle (top > bottom) is a no-op."
  (with-screen (s 5 5)
    (feed s "AAAAA")
    ;; top > bottom (3 > 1 in 1-based) → degenerate
    (cl-tmux/terminal/actions:decera s 3 1 1 5)
    (is (char= #\A (char-at s 0 0))
        "decera with degenerate rect must not erase any cells")))

(test decera-clamps-out-of-bounds-rectangle
  "DECERA parameters beyond the screen edge are clamped to the screen bounds."
  (with-screen (s 5 3)
    (feed s "AAAAA")
    ;; Rectangle extends beyond screen: bottom=99, right=99 → clamped to height-1, width-1
    (cl-tmux/terminal/actions:decera s 1 1 99 99)
    ;; Entire screen must be erased (clamped to full screen)
    (dotimes (y 3)
      (is (row-blank-p s y) "decera out-of-bounds: row ~D must be blank" y))))

;;; ── DECFRA — Fill Rectangular Area ──────────────────────────────────────────

(test decfra-fills-rectangle-with-char
  "DECFRA fills every cell in the specified 1-based rectangle with CHAR-CODE."
  (with-screen (s 10 5)
    ;; Fill rows 1-2 (0-based 0-1), cols 2-4 (0-based 1-3) with 'X' (code 88).
    (cl-tmux/terminal/actions:decfra s 88 1 2 2 4)
    (is (char= #\X (char-at s 1 0)) "decfra: col 1 row 0 must be 'X'")
    (is (char= #\X (char-at s 2 0)) "decfra: col 2 row 0 must be 'X'")
    (is (char= #\X (char-at s 3 0)) "decfra: col 3 row 0 must be 'X'")
    (is (char= #\X (char-at s 1 1)) "decfra: col 1 row 1 must be 'X'")
    ;; Cells outside the rectangle must remain blank.
    (is (char= #\Space (char-at s 0 0)) "decfra: col 0 row 0 must remain blank")
    (is (char= #\Space (char-at s 4 0)) "decfra: col 4 row 0 must remain blank")))

(test decfra-zero-char-code-fills-with-space
  "DECFRA with char-code 0 substitutes space (code 32) per VT specification."
  (with-screen (s 5 3)
    (feed s "AAAAA")
    (cl-tmux/terminal/actions:decfra s 0 1 1 1 5)
    (dotimes (x 5)
      (is (char= #\Space (char-at s x 0))
          "decfra char-code 0: col ~D must be space" x))))

(test decfra-degenerate-rectangle-is-noop
  "DECFRA with a degenerate rectangle (left > right) is a no-op."
  (with-screen (s 5 3)
    (feed s "AAAAA")
    ;; left > right: 5 > 1 (inverted) → degenerate
    (cl-tmux/terminal/actions:decfra s 88 1 5 1 1)
    (is (char= #\A (char-at s 0 0))
        "decfra with degenerate rect must not change any cells")))

;;; ── DECCRA — Copy Rectangular Area ──────────────────────────────────────────

(test deccra-copies-rectangle-to-target
  "DECCRA copies a source rectangle to a non-overlapping target."
  (with-screen (s 10 5)
    ;; Write 'A' in a 3×2 block at rows 1-2 cols 1-3 (1-based).
    (cl-tmux/terminal/actions:decfra s 65 1 1 2 3)  ; 'A'=65
    ;; Copy that block to rows 4-5 cols 6-8 (1-based).
    (cl-tmux/terminal/actions:deccra s 1 1 2 3 4 6)
    ;; Target: 0-based rows 3-4, cols 5-7 must contain 'A'.
    (is (char= #\A (char-at s 5 3)) "deccra: target col 5 row 3 must be 'A'")
    (is (char= #\A (char-at s 6 3)) "deccra: target col 6 row 3 must be 'A'")
    (is (char= #\A (char-at s 7 3)) "deccra: target col 7 row 3 must be 'A'")
    ;; Source must be unchanged.
    (is (char= #\A (char-at s 0 0)) "deccra: source col 0 row 0 must remain 'A'")))

(test deccra-overlapping-regions-are-handled-correctly
  "DECCRA with overlapping source and target regions must not corrupt data.
   The source is buffered before writing, so a shift-right overlap is safe."
  (with-screen (s 10 3)
    ;; Write 'A','B','C' in cols 0-2 of row 0.
    (cl-tmux/terminal/actions:write-char-at-cursor s #\A)
    (cl-tmux/terminal/actions:write-char-at-cursor s #\B)
    (cl-tmux/terminal/actions:write-char-at-cursor s #\C)
    ;; Copy 1-based cols 1-3 row 1 to cols 2-4 row 1 (shift right by 1).
    (cl-tmux/terminal/actions:deccra s 1 1 1 3 1 2)
    ;; After the copy, cols 1-3 (0-based) of row 0 must contain A,B,C.
    (is (char= #\A (char-at s 1 0)) "overlapping deccra: col 1 must be 'A'")
    (is (char= #\B (char-at s 2 0)) "overlapping deccra: col 2 must be 'B'")
    (is (char= #\C (char-at s 3 0)) "overlapping deccra: col 3 must be 'C'")))

(test deccra-degenerate-source-is-noop
  "DECCRA with a degenerate source rectangle (top > bottom) is a no-op."
  (with-screen (s 5 3)
    (cl-tmux/terminal/actions:decfra s 65 1 1 3 5)   ; fill whole screen with 'A'
    ;; Degenerate source: top=3 > bottom=1
    (cl-tmux/terminal/actions:deccra s 3 1 1 5 1 1)
    ;; Target must remain unchanged (still 'A' or blank — no copy occurred).
    (is (char= #\A (char-at s 0 0))
        "deccra with degenerate source must not modify the target")))

(test deccra-target-clamped-to-screen-bounds
  "DECCRA clamps the target rectangle to the screen bounds when it extends beyond."
  (with-screen (s 5 3)
    ;; Write 'Z' in a 2×2 block at rows 1-2 cols 1-2 (1-based).
    (cl-tmux/terminal/actions:decfra s 90 1 1 2 2)   ; 'Z'=90
    ;; Copy to rows 3-99 cols 4-99 (1-based) — extends well past the screen.
    ;; Only the in-bounds portion (row 2 col 3, i.e. 0-based row 2 col 3) should be written.
    (cl-tmux/terminal/actions:deccra s 1 1 2 2 3 4)
    ;; 0-based: target starts at row 2, col 3 — at least that cell must be 'Z'.
    (is (char= #\Z (char-at s 3 2))
        "deccra: clamped target cell (col 3, row 2) must be 'Z'")))
