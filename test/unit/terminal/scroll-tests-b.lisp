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
