(in-package #:cl-tmux/test)

;;;; Emulator tests (src/terminal/emulator.lisp).
;;;; Tests: copy-mode suite.

;;; ── SUITE: copy-mode scrollback projection ──────────────────────────────────

(def-suite copy-mode
  :description "Scrollback capture and copy-mode viewport projection"
  :in terminal-suite)
(in-suite copy-mode)

(test scrollback-accumulates
  "Auto-scrolling a full screen pushes displaced top rows into the scrollback."
  (with-screen (s 5 3)
    (feed-lines s "L0" "L1" "L2" "L3" "L4")   ; 5 lines into a 3-row screen
    ;; Two scrolls happened, so the two oldest rows are in scrollback,
    ;; newest-first: L1 then L0.
    (is (= 2 (length (screen-scrollback s))))
    ;; Live grid now shows the most recent three lines.
    (is (string= "L2" (row-string s 0 :end 2)))
    (is (string= "L4" (row-string s 2 :end 2)))))

(test copy-offset-projects-history
  "screen-display-cell shifts the viewport into scrollback by copy-offset rows."
  (with-screen (s 5 3)
    (feed-lines s "L0" "L1" "L2" "L3" "L4")
    (setf (screen-copy-mode-p s) t)
    ;; Offset 0: viewport is the live grid unchanged.
    (setf (screen-copy-offset s) 0)
    (is (string= "L2" (display-row-string s 0 :end 2)))
    (is (string= "L4" (display-row-string s 2 :end 2)))
    ;; Offset 1: top row is newest scrollback line (L1); live grid pushed down.
    (setf (screen-copy-offset s) 1)
    (is (string= "L1" (display-row-string s 0 :end 2)))
    (is (string= "L2" (display-row-string s 1 :end 2)))
    (is (string= "L3" (display-row-string s 2 :end 2)))
    ;; Offset 2: the two scrollback lines (L0, L1) sit above the live top (L2).
    (setf (screen-copy-offset s) 2)
    (is (string= "L0" (display-row-string s 0 :end 2)))
    (is (string= "L1" (display-row-string s 1 :end 2)))
    (is (string= "L2" (display-row-string s 2 :end 2)))))

(test copy-mode-off-ignores-offset
  "A stale copy-offset is ignored entirely when copy mode is off."
  (with-screen (s 5 3)
    (feed-lines s "L0" "L1" "L2" "L3" "L4")
    (setf (screen-copy-mode-p s) nil
          (screen-copy-offset s) 2)  ; should have no effect
    (is (string= "L2" (display-row-string s 0 :end 2)))
    (is (string= "L4" (display-row-string s 2 :end 2)))))

;;; ── Coverage: screen-display-cell out-of-range reads ────────────────────────
;;;
;;; modes.lisp returns *display-blank-cell* for two out-of-range conditions:
;;;   1. col exceeds the length of a scrollback row-vector
;;;   2. live-row exceeds screen-height (i.e. offset > scrollback depth)
;;; These paths were previously uncovered by any test.

(def-suite display-cell-oob
  :description "screen-display-cell fallback to *display-blank-cell* for out-of-range reads"
  :in copy-mode)
(in-suite display-cell-oob)

(test display-cell-scrollback-col-oob-returns-blank
  "screen-display-cell returns the blank-cell fallback when COL exceeds the
   length of the requested scrollback row-vector.
   This happens when an old row was narrower than the current screen width."
  (with-screen (s 10 3)
    ;; Build a scrollback row that is only 3 wide (narrower than screen width 10).
    (let ((narrow-row (make-array 3 :initial-element
                                    (cl-tmux/terminal/types:blank-cell))))
      (setf (cl-tmux/terminal/types:screen-scrollback s) (list narrow-row))
      (setf (cl-tmux/terminal/types:screen-copy-mode-p s) t
            (cl-tmux/terminal/types:screen-copy-offset  s) 1))
    ;; col 5 is outside the 3-wide row — should return the blank-cell fallback.
    (let ((cell (cl-tmux/terminal/actions:screen-display-cell s 5 0)))
      (is (char= #\Space (cl-tmux/terminal/types:cell-char cell))
          "out-of-range col in scrollback must return a blank cell"))))

(test display-cell-live-row-oob-returns-blank
  "screen-display-cell returns the blank-cell fallback when live-row exceeds
   screen-height.  This happens when the caller passes a row argument that is
   >= height with offset=0, which can occur in renderers that probe beyond the
   live grid boundary."
  (with-screen (s 5 3)
    (feed-lines s "L0" "L1" "L2")
    ;; With copy-offset=0 (live grid mode) and height=3, valid live rows are 0-2.
    ;; Querying row=3 gives live-row=3 which equals height → OOB → blank cell.
    (let ((cell (cl-tmux/terminal/actions:screen-display-cell s 0 3)))
      (is (char= #\Space (cl-tmux/terminal/types:cell-char cell))
          "live-row beyond screen-height must return a blank cell"))))
