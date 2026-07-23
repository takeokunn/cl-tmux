(in-package #:cl-tmux/test)

;;;; cursor tests — part E: custom multi-stop tab-stop navigation
;;;; (%next-tab-stop / %prev-tab-stop via HTS/TBC with several stops set),
;;;; plus a table-driven regression pass over cursor-cr and cursor-bs.

;;; ── SUITE: custom multi-stop tab navigation ─────────────────────────────────
;;;
;;; Coverage gap: set-tab-stop/cursor-ht/cursor-cbt were only exercised with a
;;; single custom stop.  With MULTIPLE custom stops installed, %next-tab-stop
;;; and %prev-tab-stop must search the sorted stop list (not just fall back to
;;; the :default every-8-columns grid) — the branch taken when
;;; (screen-tab-stops screen) is a concrete list with more than one entry.

(describe "terminal-suite/multi-stop-tab-navigation"

  (defun %install-tab-stops (screen &rest columns)
    "Clear SCREEN's tab stops, then move the cursor to each column in COLUMNS and
   call set-tab-stop (HTS), installing a clean multi-entry custom tab-stop list
   with exactly the given columns (set-tab-stop otherwise merges new stops into
   whatever list — including the expanded :default grid — is already present)."
    (cl-tmux/terminal/actions:clear-tab-stops screen 3)   ; TBC 3: clear ALL stops
    (dolist (col columns)
      (setf (cl-tmux/terminal/types:screen-cursor-x screen) col)
      (cl-tmux/terminal/actions:set-tab-stop screen)))

  ;; With custom stops at columns 3, 10, 15, cursor-ht from col 0 lands on 3, then
  ;; from col 3 lands on 10, then from col 10 lands on 15.
  (it "cursor-ht-with-three-custom-stops-advances-in-order"
    (with-screen (s 40 5)
      (%install-tab-stops s 3 10 15)
      (setf (cl-tmux/terminal/types:screen-cursor-x s) 0)
      (cl-tmux/terminal/actions:cursor-ht s)
      (expect (= 3 (screen-cursor-x s)))
      (cl-tmux/terminal/actions:cursor-ht s)
      (expect (= 10 (screen-cursor-x s)))
      (cl-tmux/terminal/actions:cursor-ht s)
      (expect (= 15 (screen-cursor-x s)))))

  ;; cursor-ht past the last custom stop clamps to width-1 when no further stop exists.
  (it "cursor-ht-past-last-custom-stop-clamps-to-width-minus-one"
    (with-screen (s 20 5)
      (%install-tab-stops s 3 10)
      (setf (cl-tmux/terminal/types:screen-cursor-x s) 10)
      (cl-tmux/terminal/actions:cursor-ht s)
      (expect (= 19 (screen-cursor-x s)))))

  ;; cursor-cbt with custom stops at 3, 10, 15 moves backward through them in
  ;; reverse order: from col 18, first CBT lands on 15, second on 10, third on 3.
  (it "cursor-cbt-with-three-custom-stops-moves-back-in-order"
    (with-screen (s 40 5)
      (%install-tab-stops s 3 10 15)
      (setf (cl-tmux/terminal/types:screen-cursor-x s) 18)
      (cl-tmux/terminal/actions:cursor-cbt s 1)
      (expect (= 15 (screen-cursor-x s)))
      (cl-tmux/terminal/actions:cursor-cbt s 1)
      (expect (= 10 (screen-cursor-x s)))
      (cl-tmux/terminal/actions:cursor-cbt s 1)
      (expect (= 3 (screen-cursor-x s)))))

  ;; cursor-cbt before the first custom stop clamps to column 0.
  (it "cursor-cbt-before-first-custom-stop-clamps-to-zero"
    (with-screen (s 40 5)
      (%install-tab-stops s 10 20)
      (setf (cl-tmux/terminal/types:screen-cursor-x s) 5)
      (cl-tmux/terminal/actions:cursor-cbt s 1)
      (expect (= 0 (screen-cursor-x s)))))

  ;; %materialize-tab-stops returns the concrete custom list as-is once installed
  ;; (does not fall back to the :default every-8-columns grid).
  (it "materialize-tab-stops-returns-custom-list-unchanged"
    (with-screen (s 40 5)
      (%install-tab-stops s 5 12)
      (let ((stops (cl-tmux/terminal/actions::%materialize-tab-stops s)))
        (expect (equal '(5 12) stops)))))

  ;; %materialize-tab-stops expands the :default sentinel into the standard
  ;; every-8-columns grid for the screen's width.
  (it "materialize-tab-stops-expands-default-sentinel"
    (with-screen (s 20 5)
      (let ((stops (cl-tmux/terminal/actions::%materialize-tab-stops s)))
        (expect (equal '(8 16) stops))))))

;;; ── SUITE: cursor-cr / cursor-bs table-driven regression ────────────────────
;;;
;;; Both functions are already covered by dedicated tests elsewhere in this
;;; group; this table consolidates a quick regression pass across several
;;; starting columns in one place, per the "convert repeated patterns to
;;; table-driven form" goal.

(describe "terminal-suite/cursor-cr-bs-table-suite"

  ;; cursor-cr resets the column to 0 regardless of the starting column.
  (it "cursor-cr-from-various-columns-table"
    (dolist (row '((0 0 "col 0 stays at 0")
                   (5 0 "col 5 resets to 0")
                   (9 0 "last column resets to 0")))
      (destructuring-bind (start expected desc) row
        (declare (ignore desc))
        (with-cursor-at (s 10 5 start)
          (cl-tmux/terminal/actions:cursor-cr s)
          (expect (= expected (screen-cursor-x s)))))))

  ;; cursor-bs decrements the column by 1, clamping at 0.
  (it "cursor-bs-from-various-columns-table"
    (dolist (row '((0 0 "col 0 stays at 0 (no-op)")
                   (1 0 "col 1 moves to 0")
                   (5 4 "col 5 moves to 4")
                   (9 8 "last column moves to 8")))
      (destructuring-bind (start expected desc) row
        (declare (ignore desc))
        (with-cursor-at (s 10 5 start)
          (cl-tmux/terminal/actions:cursor-bs s)
          (expect (= expected (screen-cursor-x s))))))))
