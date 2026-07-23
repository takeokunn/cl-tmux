(in-package #:cl-tmux/test)

;;;; layout-geometry tests — part B: %ranges-overlap-p, pane-center-x/y,
;;;; closest-to-center, layout-split-axis-extent v-split, resize-find-split nested,
;;;; pane-at-position out-of-bounds, orient-case, split-child-geometry table,
;;;; pane-neighbor three-panes, define-axis-rules/neighbor-finders macros, layout-min-extent nested.

(describe "layout-geometry-suite"

  ;;; It is not exported, so we access it through the internal package name.

  ;; %ranges-overlap-p: T when [s1,s1+e1) and [s2,s2+e2) share at least one point.
  (it "ranges-overlap-p-table"
    (dolist (row '((t   0  5 3  5 "[0,5) and [3,8) overlap at 3..4")
                   (t   0 10 5  3 "[0,10) and [5,8) overlap")
                   (t   5  5 4  2 "[5,10) and [4,6) share 5")
                   (nil 0  5 5  5 "[0,5) and [5,10) touch but do not overlap")
                   (nil 5  5 0  5 "[5,10) and [0,5) touch but do not overlap")
                   (nil 0  3 10 5 "[0,3) and [10,15) are disjoint")
                   (nil 10 5 0  3 "[10,15) and [0,3) are disjoint")))
      (destructuring-bind (expected s1 e1 s2 e2 desc) row
        (declare (ignore desc))
        (expect (eq expected (cl-tmux/model::%ranges-overlap-p s1 e1 s2 e2))))))

  ;;; ── %pane-center-x / %pane-center-y direct tests ────────────────────────────

  ;; %pane-center-x returns pane-x + half the width (integer arithmetic).
  (it "pane-center-x-returns-midpoint"
    (let ((pane (make-pane :id 1 :fd -1 :pid -1 :x 10 :y 0 :width 20 :height 5
                           :screen (make-screen 20 5))))
      (expect (= 20 (cl-tmux/model::%pane-center-x pane)))))

  ;; %pane-center-y returns pane-y + half the height (integer arithmetic).
  (it "pane-center-y-returns-midpoint"
    (let ((pane (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 4 :width 10 :height 8
                           :screen (make-screen 10 8))))
      (expect (= 8 (cl-tmux/model::%pane-center-y pane)))))

  ;;; ── %closest-to-center direct tests ─────────────────────────────────────────
  ;;;
  ;;; Exercises the tie-breaking: when two candidates are equidistant the first
  ;;; one in the list is preferred, and when a third candidate is farther away
  ;;; the closer two compete normally.

  ;; %closest-to-center picks the candidate whose center-fn value is closest to pane.
  (it "closest-to-center-picks-nearest"
    ;; center-y of pane = 10 + 4>>1 = 12
    ;; candidate a: center-y = 0 + 4>>1 = 2 → distance 10
    ;; candidate b: center-y = 8 + 4>>1 = 10 → distance 2 (closest)
    (with-center-test-panes ((pane 0 0 10 10 4)
                              (a    1 0  0 10 4)
                              (b    2 0  8 10 4))
      (expect (eq b (cl-tmux/model::%closest-to-center (list a b) pane
                                                    #'cl-tmux/model::%pane-center-y)))))

  ;; %closest-to-center favors the earlier candidate on an exact tie.
  (it "closest-to-center-tie-favors-first-candidate"
    ;; center-y of pane = 12
    ;; candidate a: center-y = 8 + 4>>1 = 10 → distance 2
    ;; candidate b: center-y = 12 + 4>>1 = 14 → distance 2  (tied)
    ;; On a tie, reduce returns whichever of a and b was seen first, because
    ;; the predicate uses <=, so when distances are equal it keeps the first.
    (with-center-test-panes ((pane 0 0 10 10 4)
                              (a    1 0  8 10 4)
                              (b    2 0 12 10 4))
      (expect (eq a (cl-tmux/model::%closest-to-center (list a b) pane
                                                    #'cl-tmux/model::%pane-center-y)))))

  ;; %closest-to-center correctly picks the middle candidate among three.
  (it "closest-to-center-three-candidates-non-trivial"
    ;; center-x of pane = 20 + 4>>1 = 22
    ;; candidate far-left:  center-x = 0  + 4>>1 = 2  → distance 20
    ;; candidate near-left: center-x = 16 + 4>>1 = 18 → distance 4  (closest)
    ;; candidate far-right: center-x = 40 + 4>>1 = 42 → distance 20
    (with-center-test-panes ((pane       0 20 0 4 10)
                              (far-left  1  0 0 4 10)
                              (near-left 2 16 0 4 10)
                              (far-right 3 40 0 4 10))
      (expect (eq near-left
              (cl-tmux/model::%closest-to-center (list far-left near-left far-right)
                                                  pane #'cl-tmux/model::%pane-center-x)))))

  ;;; ── layout-split-axis-extent with :v split ───────────────────────────────────

  ;; For a :v split, axis-extent along :v = total height of both panes + separator.
  (it "layout-split-axis-extent-v-split"
    (let* ((l0   (tl-leaf 1 1 1))
           (l1   (tl-leaf 2 1 1))
           (tree (make-layout-split :v l0 l1)))
      (cl-tmux/model::layout-assign tree 0 0 80 21)
      ;; The :v extent = total bounding box height = 21.
      (expect (= 21 (cl-tmux/model::layout-split-axis-extent tree :v)))
      ;; The :h extent = 80 (full width).
      (expect (= 80 (cl-tmux/model::layout-split-axis-extent tree :h)))))

  ;;; ── resize-find-split in a nested tree ───────────────────────────────────────

  ;; resize-find-split climbs past an intermediate split to find the correct ancestor.
  (it "resize-find-split-nested-tree-climbs-to-ancestor"
    ;; Tree: (outer :h (left) (inner :v (top) (bot)))
    ;; Searching for :h ancestor of inner :v children climbs past the :v split.
    (let* ((left  (tl-leaf 1 1 1))
           (top   (tl-leaf 2 1 1))
           (bot   (tl-leaf 3 1 1))
           (inner (make-layout-split :v top bot))
           (outer (make-layout-split :h left inner)))
      ;; 'top' is :first of inner (:v). Searching for :h ancestor reaches outer.
      (multiple-value-bind (split side)
          (cl-tmux/model::resize-find-split outer top :h)
        (expect (eq outer split))
        (expect (eq :second side)))))

  ;;; ── pane-at-position out-of-bounds y ────────────────────────────────────────

  ;; pane-at-position returns NIL for coordinates outside all pane rectangles.
  (it "pane-at-position-out-of-bounds-returns-nil"
    (with-h-split-81-24 (p0 p1 win)
      ;; Row 24 is one past the bottom of all 24-row panes.
      (expect (null (cl-tmux/model:pane-at-position win 0 24)))
      ;; Column 81 is one past the right edge of the 81-column window.
      (expect (null (cl-tmux/model:pane-at-position win 81 0)))))

  ;;; ── orient-case with non-keyword signals ecase error ────────────────────────

  ;; orient-case raises a condition for an orientation other than :h or :v.
  (it "orient-case-signals-on-unknown-orientation"
    (signals error
      (cl-tmux/model::orient-case :diagonal :h :horiz :v :vert)))

  ;;; ── Table-driven: split-child-geometry boundary dimensions ───────────────────
  ;;;
  ;;; split-child-geometry uses floor/ceiling arithmetic; test with both
  ;;; even and odd extents to cover both rounding paths.

  ;; Vertical split of an odd-height pane: avail=odd, fh=floor(odd/2), child gets remainder.
  (it "split-child-geometry-v-odd-height-correct-division"
    ;; pane height = 11; avail = 10; fh = floor(10/2) = 5
    ;; child: y = 0 + 5 + 1 = 6, height = 10 - 5 = 5
    (let ((pane (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 40 :height 11
                           :screen (make-screen 40 11))))
      (multiple-value-bind (nx ny nw nh)
          (cl-tmux/model::split-child-geometry pane :v)
        (expect (= 0  nx))
        (expect (= 6  ny))
        (expect (= 40 nw))
        (expect (= 5  nh)))))

  ;; Horizontal split of an odd-width pane: avail=odd, fw=floor(odd/2), child gets remainder.
  (it "split-child-geometry-h-odd-width-correct-division"
    ;; pane width = 11; avail = 10; fw = floor(10/2) = 5
    ;; child: x = 0 + 5 + 1 = 6, width = 10 - 5 = 5
    (let ((pane (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 11 :height 24
                           :screen (make-screen 11 24))))
      (multiple-value-bind (nx ny nw nh)
          (cl-tmux/model::split-child-geometry pane :h)
        (expect (= 6  nx))
        (expect (= 0  ny))
        (expect (= 5  nw))
        (expect (= 24 nh)))))

  ;;; ── pane-neighbor with three panes (non-trivial tie-breaking) ────────────────

  ;; In a 3-pane horizontal layout, the middle pane finds both left and right neighbors.
  (it "pane-neighbor-three-panes-middle-pane-finds-both-neighbors"
    ;; Manual fixture: three panes side by side.
    ;; p0: x=0 w=20, p1: x=21 w=20, p2: x=42 w=20, all h=24.
    (let* ((p0  (make-pane :id 1 :fd -1 :pid -1 :x  0 :y 0 :width 20 :height 24
                            :screen (make-screen 20 24)))
           (p1  (make-pane :id 2 :fd -1 :pid -1 :x 21 :y 0 :width 20 :height 24
                            :screen (make-screen 20 24)))
           (p2  (make-pane :id 3 :fd -1 :pid -1 :x 42 :y 0 :width 20 :height 24
                            :screen (make-screen 20 24)))
           (win (make-window :id 1 :name "w" :width 62 :height 24
                             :panes (list p0 p1 p2)
                             :tree  (make-layout-split :h
                                       (make-layout-leaf p0)
                                       (make-layout-split :h
                                          (make-layout-leaf p1)
                                          (make-layout-leaf p2))))))
      (window-select-pane win p1)
      (expect (eq p0 (pane-neighbor win p1 :left)))
      (expect (eq p2 (pane-neighbor win p1 :right)))))

  ;;; ── define-axis-rules / define-neighbor-finders macros ──────────────────────

  ;; define-axis-rules generates %orient-pane-extent with :v→height, :h→width.
  (it "define-axis-rules-generates-correct-dispatch"
    (let ((pane (make-pane :id 1 :fd -1 :pid -1 :width 30 :height 12
                           :screen (make-screen 30 12))))
      (expect (= 12 (cl-tmux/model::%orient-pane-extent pane :v)))
      (expect (= 30 (cl-tmux/model::%orient-pane-extent pane :h)))))

  ;; *neighbor-filters* alist contains entries for :right :left :down :up.
  (it "neighbor-filters-alist-has-all-four-directions"
    (let ((dirs (mapcar #'car cl-tmux/model::*neighbor-filters*)))
      (expect (member :right dirs))
      (expect (member :left  dirs))
      (expect (member :down  dirs))
      (expect (member :up    dirs))))

  ;; *neighbor-center-fn* alist maps all four directions to center functions.
  (it "neighbor-center-fn-alist-has-all-four-directions"
    (let ((dirs (mapcar #'car cl-tmux/model::*neighbor-center-fn*)))
      (expect (member :right dirs))
      (expect (member :left  dirs))
      (expect (member :down  dirs))
      (expect (member :up    dirs))))

  ;;; ── Table-driven: layout-min-extent for mixed nested trees ──────────────────

  ;; A :v split wrapping a :h split has correct min extents in both axes.
  (it "layout-min-extent-nested-v-h-split"
    ;; Tree: (outer :v (inner :h (l0) (l1)) (l2))
    ;; :h child min extent  → :h = 2+1+2=5, :v = 1
    ;; outer :v min extent  → :v = max(1,1) + 1 + min-height = 1+1+1 = 3
    ;;                           (wait: :v split sums :v children + 1 separator)
    ;; :v extent = (inner :h :v = 1) + 1 separator + (l2 :v = 1) = 3
    ;; :h extent = max(inner :h :h = 5, l2 :h = 2) = 5
    (let* ((l0    (tl-leaf 1 1 1))
           (l1    (tl-leaf 2 1 1))
           (l2    (tl-leaf 3 1 1))
           (inner (make-layout-split :h l0 l1))
           (outer (make-layout-split :v inner l2)))
      (expect (= 3 (cl-tmux/model::layout-min-extent outer :v)))
      (expect (= 5 (cl-tmux/model::layout-min-extent outer :h))))))
