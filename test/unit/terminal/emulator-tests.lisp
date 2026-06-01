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
