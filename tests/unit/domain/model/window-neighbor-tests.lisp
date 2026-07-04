(in-package #:cl-tmux/test)

(in-suite model-suite)

;;; ── pane-neighbor (directional navigation) ───────────────────────────────────
;;;
;;; Uses make-two-pane-h-window from tests/helpers-layout-fixtures.lisp.
;;; %two-pane-h-window was removed to eliminate the 81x24 two-pane fixture
;;; defined in two places with identical construction logic.

(test pane-neighbor-h-split-table
  "In an h-split: left pane's :right neighbor is the right pane, and vice versa."
  (multiple-value-bind (win p0 p1) (make-two-pane-h-window)
    (dolist (row (list (list p0 :right p1 "p0 :right -> p1")
                       (list p1 :left  p0 "p1 :left -> p0")))
      (destructuring-bind (pane dir expected desc) row
        (is (eq expected (cl-tmux/model::pane-neighbor win pane dir)) "~A" desc)))))

(test pane-neighbor-nil-for-single-pane
  "A single pane has no neighbor in any direction."
  (let* ((p0  (make-no-pty-pane 1 0 0 80 24))
         (win (make-window :id 1 :name "w" :width 80 :height 24
                           :panes (list p0)
                           :tree (make-layout-leaf p0))))
    (window-select-pane win p0)
    (dolist (dir '(:right :left :up :down))
      (is (null (cl-tmux/model::pane-neighbor win p0 dir))
          "single pane must have no ~A neighbor" dir))))

(test pane-neighbor-v-split-table
  "In a v-split: top pane's :down neighbor is bottom pane, bottom's :up is top pane."
  (with-v-split-window (win p0 p1)
    (dolist (row (list (list p0 :down p1 "p0 :down -> p1")
                       (list p1 :up   p0 "p1 :up -> p0")))
      (destructuring-bind (pane dir expected desc) row
        (is (eq expected (cl-tmux/model::pane-neighbor win pane dir)) "~A" desc)))))

(test pane-neighbor-nil-outside-split-axis
  "A pane in an h-split has no up or down neighbor."
  (multiple-value-bind (win p0 p1) (make-two-pane-h-window)
    (declare (ignore p1))
    (is (null (cl-tmux/model::pane-neighbor win p0 :up))
        "left pane in h-split must have no up neighbor")
    (is (null (cl-tmux/model::pane-neighbor win p0 :down))
        "left pane in h-split must have no down neighbor")))

(test pane-neighbor-nil-when-window-zoomed
  "Zoomed windows behave like single-pane windows for neighbor lookup."
  (multiple-value-bind (win p0 p1) (make-two-pane-h-window)
    (declare (ignore p1))
    (window-select-pane win p0)
    (cl-tmux/model:window-zoom-toggle win)
    (dolist (dir '(:right :left :up :down))
      (is (null (cl-tmux/model::pane-neighbor win p0 dir))
          "zoomed window must have no ~A neighbor" dir))))
