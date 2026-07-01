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

(def-suite multi-stop-tab-navigation
  :description "cursor-ht/cursor-cbt with several custom tab stops installed"
  :in terminal-suite)
(in-suite multi-stop-tab-navigation)

(defun %install-tab-stops (screen &rest columns)
  "Clear SCREEN's tab stops, then move the cursor to each column in COLUMNS and
   call set-tab-stop (HTS), installing a clean multi-entry custom tab-stop list
   with exactly the given columns (set-tab-stop otherwise merges new stops into
   whatever list — including the expanded :default grid — is already present)."
  (cl-tmux/terminal/actions:clear-tab-stops screen 3)   ; TBC 3: clear ALL stops
  (dolist (col columns)
    (setf (cl-tmux/terminal/types:screen-cursor-x screen) col)
    (cl-tmux/terminal/actions:set-tab-stop screen)))

(test cursor-ht-with-three-custom-stops-advances-in-order
  "With custom stops at columns 3, 10, 15, cursor-ht from col 0 lands on 3, then
   from col 3 lands on 10, then from col 10 lands on 15."
  (with-screen (s 40 5)
    (%install-tab-stops s 3 10 15)
    (setf (cl-tmux/terminal/types:screen-cursor-x s) 0)
    (cl-tmux/terminal/actions:cursor-ht s)
    (is (= 3 (screen-cursor-x s)) "first HT from col 0 must land on custom stop 3")
    (cl-tmux/terminal/actions:cursor-ht s)
    (is (= 10 (screen-cursor-x s)) "second HT from col 3 must land on custom stop 10")
    (cl-tmux/terminal/actions:cursor-ht s)
    (is (= 15 (screen-cursor-x s)) "third HT from col 10 must land on custom stop 15")))

(test cursor-ht-past-last-custom-stop-clamps-to-width-minus-one
  "cursor-ht past the last custom stop clamps to width-1 when no further stop exists."
  (with-screen (s 20 5)
    (%install-tab-stops s 3 10)
    (setf (cl-tmux/terminal/types:screen-cursor-x s) 10)
    (cl-tmux/terminal/actions:cursor-ht s)
    (is (= 19 (screen-cursor-x s))
        "HT past the last custom stop must clamp to width-1 (19), got ~D"
        (screen-cursor-x s))))

(test cursor-cbt-with-three-custom-stops-moves-back-in-order
  "cursor-cbt with custom stops at 3, 10, 15 moves backward through them in
   reverse order: from col 18, first CBT lands on 15, second on 10, third on 3."
  (with-screen (s 40 5)
    (%install-tab-stops s 3 10 15)
    (setf (cl-tmux/terminal/types:screen-cursor-x s) 18)
    (cl-tmux/terminal/actions:cursor-cbt s 1)
    (is (= 15 (screen-cursor-x s)) "first CBT from col 18 must land on 15")
    (cl-tmux/terminal/actions:cursor-cbt s 1)
    (is (= 10 (screen-cursor-x s)) "second CBT from col 15 must land on 10")
    (cl-tmux/terminal/actions:cursor-cbt s 1)
    (is (= 3 (screen-cursor-x s)) "third CBT from col 10 must land on 3")))

(test cursor-cbt-before-first-custom-stop-clamps-to-zero
  "cursor-cbt before the first custom stop clamps to column 0."
  (with-screen (s 40 5)
    (%install-tab-stops s 10 20)
    (setf (cl-tmux/terminal/types:screen-cursor-x s) 5)
    (cl-tmux/terminal/actions:cursor-cbt s 1)
    (is (= 0 (screen-cursor-x s))
        "CBT before the first custom stop must clamp to 0, got ~D"
        (screen-cursor-x s))))

(test materialize-tab-stops-returns-custom-list-unchanged
  "%materialize-tab-stops returns the concrete custom list as-is once installed
   (does not fall back to the :default every-8-columns grid)."
  (with-screen (s 40 5)
    (%install-tab-stops s 5 12)
    (let ((stops (cl-tmux/terminal/actions::%materialize-tab-stops s)))
      (is (equal '(5 12) stops)
          "materialized stops must equal the installed custom list, got ~S" stops))))

(test materialize-tab-stops-expands-default-sentinel
  "%materialize-tab-stops expands the :default sentinel into the standard
   every-8-columns grid for the screen's width."
  (with-screen (s 20 5)
    (let ((stops (cl-tmux/terminal/actions::%materialize-tab-stops s)))
      (is (equal '(8 16) stops)
          "default stops for a 20-wide screen must be (8 16), got ~S" stops))))

;;; ── SUITE: cursor-cr / cursor-bs table-driven regression ────────────────────
;;;
;;; Both functions are already covered by dedicated tests elsewhere in this
;;; group; this table consolidates a quick regression pass across several
;;; starting columns in one place, per the "convert repeated patterns to
;;; table-driven form" goal.

(def-suite cursor-cr-bs-table-suite
  :description "Table-driven regression for cursor-cr and cursor-bs from several columns"
  :in terminal-suite)
(in-suite cursor-cr-bs-table-suite)

(test cursor-cr-from-various-columns-table
  "cursor-cr resets the column to 0 regardless of the starting column."
  (dolist (row '((0 0 "col 0 stays at 0")
                 (5 0 "col 5 resets to 0")
                 (9 0 "last column resets to 0")))
    (destructuring-bind (start expected desc) row
      (with-cursor-at (s 10 5 start)
        (cl-tmux/terminal/actions:cursor-cr s)
        (is (= expected (screen-cursor-x s)) "~A (got ~D)" desc (screen-cursor-x s))))))

(test cursor-bs-from-various-columns-table
  "cursor-bs decrements the column by 1, clamping at 0."
  (dolist (row '((0 0 "col 0 stays at 0 (no-op)")
                 (1 0 "col 1 moves to 0")
                 (5 4 "col 5 moves to 4")
                 (9 8 "last column moves to 8")))
    (destructuring-bind (start expected desc) row
      (with-cursor-at (s 10 5 start)
        (cl-tmux/terminal/actions:cursor-bs s)
        (is (= expected (screen-cursor-x s)) "~A (got ~D)" desc (screen-cursor-x s))))))
