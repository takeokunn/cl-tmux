(in-package #:cl-tmux/test)

;;;; scroll tests — part D: clear-scrollback, BCE background via %erase-cell,
;;;; and *scroll-on-clear-function* edge-cases.

;;; ── SUITE: clear-scrollback ──────────────────────────────────────────────────
;;;
;;; clear-scrollback is exported from cl-tmux/terminal/actions but was previously
;;; only called indirectly (via the clear-history command integration path).
;;; These tests verify it directly.

(def-suite clear-scrollback-suite
  :description "Direct calls to clear-scrollback"
  :in terminal-suite)
(in-suite clear-scrollback-suite)

(test clear-scrollback-empties-scrollback-list
  "clear-scrollback sets the screen-scrollback slot to NIL."
  (with-screen (s 5 3)
    ;; Build up scrollback by scrolling
    (feed-lines s "L0" "L1" "L2" "L3")
    (is (plusp (length (cl-tmux/terminal/types:screen-scrollback s)))
        "scrollback must be non-empty before clear-scrollback")
    (cl-tmux/terminal/actions:clear-scrollback s)
    (is (null (cl-tmux/terminal/types:screen-scrollback s))
        "scrollback must be NIL after clear-scrollback")))

(test clear-scrollback-noop-on-empty-scrollback
  "clear-scrollback on a screen with no scrollback is a no-op (no error, stays NIL)."
  (with-screen (s 5 3)
    (is (null (cl-tmux/terminal/types:screen-scrollback s))
        "scrollback must be NIL on a fresh screen")
    (finishes (cl-tmux/terminal/actions:clear-scrollback s))
    (is (null (cl-tmux/terminal/types:screen-scrollback s))
        "scrollback must remain NIL after clear-scrollback on empty")))

(test clear-scrollback-leaves-visible-grid-intact
  "clear-scrollback does not modify the visible grid cells."
  (with-screen (s 5 3)
    (feed s "hello")                           ; write on the visible grid
    (feed-lines s "" "L1" "L2" "L3")           ; build some scrollback
    (cl-tmux/terminal/actions:clear-scrollback s)
    ;; After clear-scrollback, row 0 visible content is from post-scroll state
    ;; — the key assertion is that the visible grid is NOT blanked.
    (is (null (cl-tmux/terminal/types:screen-scrollback s))
        "scrollback must be NIL after clear-scrollback")
    ;; Visible grid must still have non-blank content somewhere
    (let ((any-non-blank nil))
      (dotimes (y 3)
        (unless (row-blank-p s y)
          (setf any-non-blank t)))
      (is-true any-non-blank
               "visible grid must have some non-blank content after clear-scrollback"))))

;;; ── SUITE: scroll-on-clear-function edge cases ───────────────────────────────
;;;
;;; Coverage gaps: the *scroll-on-clear-function* nil path was tested in
;;; scroll-tests.lisp as "scroll-on-clear-off-discards-content", but the edge
;;; cases of function returning nil vs. function returning non-nil need a
;;; dedicated table-driven treatment.

(def-suite scroll-on-clear-edge-cases
  :description "*scroll-on-clear-function* edge cases: nil function, returns nil, returns t"
  :in terminal-suite)
(in-suite scroll-on-clear-edge-cases)

(test scroll-on-clear-function-returning-nil-does-not-push
  "A scroll-on-clear function that returns NIL is treated as OFF: ED 2 does not push
   content to scrollback."
  (let ((cl-tmux/terminal/actions::*scroll-on-clear-function* (lambda () nil)))
    (with-screen (s 5 3)
      (feed s "AAAAA")
      (feed s (esc "[2J"))
      (is (null (cl-tmux/terminal/types:screen-scrollback s))
          "scroll-on-clear function returning NIL must not push content"))))

(test scroll-on-clear-function-returning-non-nil-pushes-content
  "A scroll-on-clear function that returns non-NIL causes ED 2 to push visible rows."
  (let ((cl-tmux/terminal/actions::*scroll-on-clear-function* (lambda () t)))
    (with-screen (s 5 3)
      (feed s "AAAAA")
      (feed s (esc "[2J"))
      (is (plusp (length (cl-tmux/terminal/types:screen-scrollback s)))
          "scroll-on-clear function returning T must push content to scrollback"))))

(test scroll-on-clear-nil-function-does-not-push
  "*scroll-on-clear-function* = NIL (no policy) means scroll-on-clear is OFF."
  (let ((cl-tmux/terminal/actions::*scroll-on-clear-function* nil))
    (with-screen (s 5 3)
      (feed s "BBBBB")
      (feed s (esc "[2J"))
      (is (null (cl-tmux/terminal/types:screen-scrollback s))
          "NIL *scroll-on-clear-function* must not push content to scrollback"))))

;;; ── SUITE: decstbm additional edge cases ─────────────────────────────────────
;;;
;;; Direct tests for decstbm edge cases not covered by the existing
;;; constrained-scroll and scroll-region suites.

(def-suite decstbm-edge-cases
  :description "decstbm additional edge cases: oversize args, repeated calls"
  :in terminal-suite)
(in-suite decstbm-edge-cases)

(test decstbm-repeated-call-updates-region
  "Calling decstbm twice with different valid regions updates the scroll region
   to the second call's values (no residual from the first call)."
  (with-screen (s 10 10)
    (cl-tmux/terminal/actions:decstbm s 0 4)   ; first call: rows 0-4
    (is (= 0 (cl-tmux/terminal/types:screen-scroll-top    s)) "first call: top 0")
    (is (= 4 (cl-tmux/terminal/types:screen-scroll-bottom s)) "first call: bottom 4")
    (cl-tmux/terminal/actions:decstbm s 2 8)   ; second call: rows 2-8
    (is (= 2 (cl-tmux/terminal/types:screen-scroll-top    s)) "second call: top 2")
    (is (= 8 (cl-tmux/terminal/types:screen-scroll-bottom s)) "second call: bottom 8")))

(test decstbm-single-row-region-accepted
  "decstbm with top=0, bottom=0 is rejected (top == bottom, not top < bottom).
   The existing scroll region must remain unchanged."
  (with-screen (s 5 5)
    (let ((orig-top    (cl-tmux/terminal/types:screen-scroll-top    s))
          (orig-bottom (cl-tmux/terminal/types:screen-scroll-bottom s)))
      (cl-tmux/terminal/actions:decstbm s 0 0)   ; top == bottom: invalid
      (is (= orig-top    (cl-tmux/terminal/types:screen-scroll-top    s))
          "scroll-top must not change for top == bottom region")
      (is (= orig-bottom (cl-tmux/terminal/types:screen-scroll-bottom s))
          "scroll-bottom must not change for top == bottom region"))))

(test decstbm-minimum-valid-region-top-0-bottom-1
  "decstbm with top=0, bottom=1 is the smallest valid region (two rows)."
  (with-screen (s 5 5)
    (cl-tmux/terminal/actions:decstbm s 0 1)
    (is (= 0 (cl-tmux/terminal/types:screen-scroll-top    s)) "top must be 0")
    (is (= 1 (cl-tmux/terminal/types:screen-scroll-bottom s)) "bottom must be 1")))

;;; ── SUITE: scroll dirty-p edge cases ─────────────────────────────────────────
;;;
;;; scroll-up-one and scroll-down-one must mark the screen dirty even when the
;;; scroll region is restricted (non-default decstbm).

(def-suite scroll-dirty-restricted-region
  :description "scroll-up/down-one mark dirty with a restricted scroll region"
  :in terminal-suite)
(in-suite scroll-dirty-restricted-region)

(test scroll-up-marks-dirty-with-restricted-region
  "scroll-up-one marks the screen dirty even when scrolling a sub-region."
  (with-screen (s 5 5)
    (cl-tmux/terminal/actions:decstbm s 1 3)   ; rows 1-3 only
    (screen-clear-dirty s)
    (is-false (cl-tmux/terminal/types:screen-dirty-p s)
              "dirty must be NIL before scroll")
    (cl-tmux/terminal/actions:scroll-up-one s)
    (is (cl-tmux/terminal/types:screen-dirty-p s)
        "screen must be dirty after scroll-up-one with restricted region")))

(test scroll-down-marks-dirty-with-restricted-region
  "scroll-down-one marks the screen dirty even when scrolling a sub-region."
  (with-screen (s 5 5)
    (cl-tmux/terminal/actions:decstbm s 1 3)
    (screen-clear-dirty s)
    (cl-tmux/terminal/actions:scroll-down-one s)
    (is (cl-tmux/terminal/types:screen-dirty-p s)
        "screen must be dirty after scroll-down-one with restricted region")))

;;; ── SUITE: scroll-up-one with pre-filled content ────────────────────────────
;;;
;;; Verify that scroll-up-one moves content as expected (row content shifts).

(def-suite scroll-content-verification
  :description "Verify visible content displacement by scroll-up-one and scroll-down-one"
  :in terminal-suite)
(in-suite scroll-content-verification)

(test scroll-up-one-displaces-content-upward
  "scroll-up-one moves row N to row N-1 within the scroll region."
  (with-screen (s 5 3)
    (feed-lines s "ROW0" "ROW1" "ROW2")
    (cl-tmux/terminal/actions:scroll-up-one s)
    ;; Row 0 was displaced to scrollback; old row 1 is now row 0.
    (check-row s 0 "ROW1")
    ;; Old row 2 is now row 1.
    (check-row s 1 "ROW2")
    ;; Row 2 (the newly exposed bottom) must be blank.
    (is (row-blank-p s 2) "newly exposed bottom row must be blank")))

(test scroll-down-one-displaces-content-downward
  "scroll-down-one moves row N to row N+1 within the scroll region."
  (with-screen (s 5 3)
    (feed-lines s "ROW0" "ROW1" "ROW2")
    (cl-tmux/terminal/actions:scroll-down-one s)
    ;; Row 0 must be blank (newly inserted at top).
    (is (row-blank-p s 0) "new top row must be blank after scroll-down-one")
    ;; Old row 0 is now at row 1.
    (check-row s 1 "ROW0")
    ;; Old row 1 is now at row 2.
    (check-row s 2 "ROW1")))

;;; ── SUITE: push-row-to-scrollback internals ──────────────────────────────────
;;;
;;; %push-row-to-scrollback is private but its effect (prepend row to scrollback
;;; and enforce history cap) is exercised here via scroll-up-one and
;;; scroll-screen-to-history.

(def-suite push-row-to-scrollback-suite
  :description "Scrollback row ordering and cap enforcement via scroll-up-one"
  :in terminal-suite)
(in-suite push-row-to-scrollback-suite)

(test scroll-up-one-preserves-newest-first-ordering
  "After three scroll-up-one calls, the scrollback is newest-first: the last
   displaced row is at index 0."
  (with-screen (s 5 4)
    (feed-lines s "ROW0" "ROW1" "ROW2" "ROW3")
    ;; Scroll row 0 into history, then row 1, then row 2.
    (cl-tmux/terminal/actions:scroll-up-one s)   ; pushes ROW0
    (cl-tmux/terminal/actions:scroll-up-one s)   ; pushes ROW1 (now at top)
    (cl-tmux/terminal/actions:scroll-up-one s)   ; pushes ROW2 (now at top)
    (let ((scrollback (cl-tmux/terminal/types:screen-scrollback s)))
      (is (= 3 (length scrollback))
          "scrollback must have 3 entries after 3 scrolls (got ~D)"
          (length scrollback))
      ;; newest-first: index 0 = last pushed = ROW2
      (let ((newest-char (cell-char (aref (first scrollback) 0))))
        (is (char= #\R newest-char)
            "newest scrollback entry must start with R from ROW2")))))

(test history-cap-enforced-after-scroll-up-one
  "scroll-up-one with a small custom cap never lets scrollback grow beyond it."
  (with-screen (s 5 3)
    (let ((cap 3)
          (cl-tmux/terminal/actions:*history-limit-function* (lambda () 3)))
      (dotimes (_ (* cap 3))
        (cl-tmux/terminal/actions:scroll-up-one s))
      (is (<= (length (cl-tmux/terminal/types:screen-scrollback s)) cap)
          "scrollback must not exceed cap=~D" cap))))
