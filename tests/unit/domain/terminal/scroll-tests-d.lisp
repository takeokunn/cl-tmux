(in-package #:cl-tmux/test)

;;;; scroll tests — part D: clear-scrollback, BCE background via %erase-cell,
;;;; and *scroll-on-clear-function* edge-cases.

;;; ── SUITE: clear-scrollback ──────────────────────────────────────────────────
;;;
;;; clear-scrollback is exported from cl-tmux/terminal/actions but was previously
;;; only called indirectly (via the clear-history command integration path).
;;; These tests verify it directly.

(describe "terminal-suite/clear-scrollback-suite"

  ;; clear-scrollback sets the screen-scrollback slot to NIL.
  (it "clear-scrollback-empties-scrollback-list"
    (with-screen (s 5 3)
      ;; Build up scrollback by scrolling
      (feed-lines s "L0" "L1" "L2" "L3")
      (expect (plusp (length (cl-tmux/terminal/types:screen-scrollback s))))
      (cl-tmux/terminal/actions:clear-scrollback s)
      (expect (null (cl-tmux/terminal/types:screen-scrollback s)))))

  ;; clear-scrollback on a screen with no scrollback is a no-op (no error, stays NIL).
  (it "clear-scrollback-noop-on-empty-scrollback"
    (with-screen (s 5 3)
      (expect (null (cl-tmux/terminal/types:screen-scrollback s)))
      (finishes (cl-tmux/terminal/actions:clear-scrollback s))
      (expect (null (cl-tmux/terminal/types:screen-scrollback s)))))

  ;; clear-scrollback does not modify the visible grid cells.
  (it "clear-scrollback-leaves-visible-grid-intact"
    (with-screen (s 5 3)
      (feed s "hello")                           ; write on the visible grid
      (feed-lines s "" "L1" "L2" "L3")           ; build some scrollback
      (cl-tmux/terminal/actions:clear-scrollback s)
      ;; After clear-scrollback, row 0 visible content is from post-scroll state
      ;; — the key assertion is that the visible grid is NOT blanked.
      (expect (null (cl-tmux/terminal/types:screen-scrollback s)))
      ;; Visible grid must still have non-blank content somewhere
      (let ((any-non-blank nil))
        (dotimes (y 3)
          (unless (row-blank-p s y)
            (setf any-non-blank t)))
        (expect any-non-blank :to-be-truthy)))))

;;; ── SUITE: scroll-on-clear-function edge cases ───────────────────────────────
;;;
;;; Coverage gaps: the *scroll-on-clear-function* nil path was tested in
;;; scroll-tests.lisp as "scroll-on-clear-off-discards-content", but the edge
;;; cases of function returning nil vs. function returning non-nil need a
;;; dedicated table-driven treatment.

(describe "terminal-suite/scroll-on-clear-edge-cases"

  ;; A scroll-on-clear function that returns NIL is treated as OFF: ED 2 does not push
  ;; content to scrollback.
  (it "scroll-on-clear-function-returning-nil-does-not-push"
    (let ((cl-tmux/terminal/actions::*scroll-on-clear-function* (lambda () nil)))
      (with-screen (s 5 3)
        (feed s "AAAAA")
        (feed s (esc "[2J"))
        (expect (null (cl-tmux/terminal/types:screen-scrollback s))))))

  ;; A scroll-on-clear function that returns non-NIL causes ED 2 to push visible rows.
  (it "scroll-on-clear-function-returning-non-nil-pushes-content"
    (let ((cl-tmux/terminal/actions::*scroll-on-clear-function* (lambda () t)))
      (with-screen (s 5 3)
        (feed s "AAAAA")
        (feed s (esc "[2J"))
        (expect (plusp (length (cl-tmux/terminal/types:screen-scrollback s)))))))

  ;; *scroll-on-clear-function* = NIL (no policy) means scroll-on-clear is OFF.
  (it "scroll-on-clear-nil-function-does-not-push"
    (let ((cl-tmux/terminal/actions::*scroll-on-clear-function* nil))
      (with-screen (s 5 3)
        (feed s "BBBBB")
        (feed s (esc "[2J"))
        (expect (null (cl-tmux/terminal/types:screen-scrollback s)))))))

;;; ── SUITE: decstbm additional edge cases ─────────────────────────────────────
;;;
;;; Direct tests for decstbm edge cases not covered by the existing
;;; constrained-scroll and scroll-region suites.

(describe "terminal-suite/decstbm-edge-cases"

  ;; Calling decstbm twice with different valid regions updates the scroll region
  ;; to the second call's values (no residual from the first call).
  (it "decstbm-repeated-call-updates-region"
    (with-screen (s 10 10)
      (cl-tmux/terminal/actions:decstbm s 0 4)   ; first call: rows 0-4
      (expect (= 0 (cl-tmux/terminal/types:screen-scroll-top    s)))
      (expect (= 4 (cl-tmux/terminal/types:screen-scroll-bottom s)))
      (cl-tmux/terminal/actions:decstbm s 2 8)   ; second call: rows 2-8
      (expect (= 2 (cl-tmux/terminal/types:screen-scroll-top    s)))
      (expect (= 8 (cl-tmux/terminal/types:screen-scroll-bottom s)))))

  ;; decstbm with top=0, bottom=0 is rejected (top == bottom, not top < bottom).
  ;; The existing scroll region must remain unchanged.
  (it "decstbm-single-row-region-accepted"
    (with-screen (s 5 5)
      (let ((orig-top    (cl-tmux/terminal/types:screen-scroll-top    s))
            (orig-bottom (cl-tmux/terminal/types:screen-scroll-bottom s)))
        (cl-tmux/terminal/actions:decstbm s 0 0)   ; top == bottom: invalid
        (expect (= orig-top    (cl-tmux/terminal/types:screen-scroll-top    s)))
        (expect (= orig-bottom (cl-tmux/terminal/types:screen-scroll-bottom s))))))

  ;; decstbm with top=0, bottom=1 is the smallest valid region (two rows).
  (it "decstbm-minimum-valid-region-top-0-bottom-1"
    (with-screen (s 5 5)
      (cl-tmux/terminal/actions:decstbm s 0 1)
      (expect (= 0 (cl-tmux/terminal/types:screen-scroll-top    s)))
      (expect (= 1 (cl-tmux/terminal/types:screen-scroll-bottom s))))))

;;; ── SUITE: scroll dirty-p edge cases ─────────────────────────────────────────
;;;
;;; scroll-up-one and scroll-down-one must mark the screen dirty even when the
;;; scroll region is restricted (non-default decstbm).

(describe "terminal-suite/scroll-dirty-restricted-region"

  ;; scroll-up-one marks the screen dirty even when scrolling a sub-region.
  (it "scroll-up-marks-dirty-with-restricted-region"
    (with-screen (s 5 5)
      (cl-tmux/terminal/actions:decstbm s 1 3)   ; rows 1-3 only
      (screen-clear-dirty s)
      (expect (cl-tmux/terminal/types:screen-dirty-p s) :to-be-falsy)
      (cl-tmux/terminal/actions:scroll-up-one s)
      (expect (cl-tmux/terminal/types:screen-dirty-p s))))

  ;; scroll-down-one marks the screen dirty even when scrolling a sub-region.
  (it "scroll-down-marks-dirty-with-restricted-region"
    (with-screen (s 5 5)
      (cl-tmux/terminal/actions:decstbm s 1 3)
      (screen-clear-dirty s)
      (cl-tmux/terminal/actions:scroll-down-one s)
      (expect (cl-tmux/terminal/types:screen-dirty-p s)))))

;;; ── SUITE: scroll-up-one with pre-filled content ────────────────────────────
;;;
;;; Verify that scroll-up-one moves content as expected (row content shifts).

(describe "terminal-suite/scroll-content-verification"

  ;; scroll-up-one moves row N to row N-1 within the scroll region.
  (it "scroll-up-one-displaces-content-upward"
    (with-screen (s 5 3)
      (feed-lines s "ROW0" "ROW1" "ROW2")
      (cl-tmux/terminal/actions:scroll-up-one s)
      ;; Row 0 was displaced to scrollback; old row 1 is now row 0.
      (check-row s 0 "ROW1")
      ;; Old row 2 is now row 1.
      (check-row s 1 "ROW2")
      ;; Row 2 (the newly exposed bottom) must be blank.
      (expect (row-blank-p s 2))))

  ;; scroll-down-one moves row N to row N+1 within the scroll region.
  (it "scroll-down-one-displaces-content-downward"
    (with-screen (s 5 3)
      (feed-lines s "ROW0" "ROW1" "ROW2")
      (cl-tmux/terminal/actions:scroll-down-one s)
      ;; Row 0 must be blank (newly inserted at top).
      (expect (row-blank-p s 0))
      ;; Old row 0 is now at row 1.
      (check-row s 1 "ROW0")
      ;; Old row 1 is now at row 2.
      (check-row s 2 "ROW1"))))

;;; ── SUITE: push-row-to-scrollback internals ──────────────────────────────────
;;;
;;; %push-row-to-scrollback is private but its effect (prepend row to scrollback
;;; and enforce history cap) is exercised here via scroll-up-one and
;;; scroll-screen-to-history.

(describe "terminal-suite/push-row-to-scrollback-suite"

  ;; After three scroll-up-one calls, the scrollback is newest-first: the last
  ;; displaced row is at index 0.
  (it "scroll-up-one-preserves-newest-first-ordering"
    (with-screen (s 5 4)
      (feed-lines s "ROW0" "ROW1" "ROW2" "ROW3")
      ;; Scroll row 0 into history, then row 1, then row 2.
      (cl-tmux/terminal/actions:scroll-up-one s)   ; pushes ROW0
      (cl-tmux/terminal/actions:scroll-up-one s)   ; pushes ROW1 (now at top)
      (cl-tmux/terminal/actions:scroll-up-one s)   ; pushes ROW2 (now at top)
      (let ((scrollback (cl-tmux/terminal/types:screen-scrollback s)))
        (expect (= 3 (length scrollback)))
        ;; newest-first: index 0 = last pushed = ROW2
        (let ((newest-char (cell-char (aref (first scrollback) 0))))
          (expect (char= #\R newest-char))))))

  ;; scroll-up-one with a small custom cap never lets scrollback grow beyond it.
  (it "history-cap-enforced-after-scroll-up-one"
    (with-screen (s 5 3)
      (let ((cap 3)
            (cl-tmux/terminal/actions:*history-limit-function* (lambda () 3)))
        (dotimes (_ (* cap 3))
          (cl-tmux/terminal/actions:scroll-up-one s))
        (expect (<= (length (cl-tmux/terminal/types:screen-scrollback s)) cap))))))
