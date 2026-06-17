(in-package #:cl-tmux/test)

;;;; layout-geometry tests — part B: %ranges-overlap-p, pane-center-x/y,
;;;; closest-to-center, layout-split-axis-extent v-split, resize-find-split nested,
;;;; pane-at-position out-of-bounds, orient-case, split-child-geometry table,
;;;; pane-neighbor three-panes, define-axis-rules/neighbor-finders macros, layout-min-extent nested.

(in-suite layout-geometry-suite)

;;; It is not exported, so we access it through the internal package name.

(test ranges-overlap-p-table
  "%ranges-overlap-p: T when [s1,s1+e1) and [s2,s2+e2) share at least one point."
  (dolist (row '((t   0  5 3  5 "[0,5) and [3,8) overlap at 3..4")
                 (t   0 10 5  3 "[0,10) and [5,8) overlap")
                 (t   5  5 4  2 "[5,10) and [4,6) share 5")
                 (nil 0  5 5  5 "[0,5) and [5,10) touch but do not overlap")
                 (nil 5  5 0  5 "[5,10) and [0,5) touch but do not overlap")
                 (nil 0  3 10 5 "[0,3) and [10,15) are disjoint")
                 (nil 10 5 0  3 "[10,15) and [0,3) are disjoint")))
    (destructuring-bind (expected s1 e1 s2 e2 desc) row
      (is (eq expected (cl-tmux/model::%ranges-overlap-p s1 e1 s2 e2)) "~A" desc))))

;;; ── %pane-center-x / %pane-center-y direct tests ────────────────────────────

(test pane-center-x-returns-midpoint
  "%pane-center-x returns pane-x + half the width (integer arithmetic)."
  (let ((pane (make-pane :id 1 :fd -1 :pid -1 :x 10 :y 0 :width 20 :height 5
                         :screen (make-screen 20 5))))
    (is (= 20 (cl-tmux/model::%pane-center-x pane))
        "center-x: 10 + 20>>1 = 10 + 10 = 20")))

(test pane-center-y-returns-midpoint
  "%pane-center-y returns pane-y + half the height (integer arithmetic)."
  (let ((pane (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 4 :width 10 :height 8
                         :screen (make-screen 10 8))))
    (is (= 8 (cl-tmux/model::%pane-center-y pane))
        "center-y: 4 + 8>>1 = 4 + 4 = 8")))

;;; ── %closest-to-center direct tests ─────────────────────────────────────────
;;;
;;; Exercises the tie-breaking: when two candidates are equidistant the first
;;; one in the list is preferred, and when a third candidate is farther away
;;; the closer two compete normally.

(test closest-to-center-picks-nearest
  "%closest-to-center picks the candidate whose center-fn value is closest to pane."
  (let* ((pane (make-pane :id 0 :fd -1 :pid -1 :x 0 :y 10 :width 10 :height 4
                           :screen (make-screen 10 4)))
         ;; center-y of pane = 10 + 4>>1 = 12
         ;; candidate a: center-y = 0 + 4>>1 = 2 → distance 10
         ;; candidate b: center-y = 8 + 4>>1 = 10 → distance 2 (closest)
         (a    (make-pane :id 1 :fd -1 :pid -1 :x 0 :y  0 :width 10 :height 4
                           :screen (make-screen 10 4)))
         (b    (make-pane :id 2 :fd -1 :pid -1 :x 0 :y  8 :width 10 :height 4
                           :screen (make-screen 10 4))))
    (is (eq b (cl-tmux/model::%closest-to-center (list a b) pane
                                                  #'cl-tmux/model::%pane-center-y))
        "b is closer by center-y and must be returned")))

(test closest-to-center-tie-favors-first-candidate
  "%closest-to-center favors the earlier candidate on an exact tie."
  (let* ((pane (make-pane :id 0 :fd -1 :pid -1 :x 0 :y 10 :width 10 :height 4
                           :screen (make-screen 10 4)))
         ;; center-y of pane = 12
         ;; candidate a: center-y = 8 + 4>>1 = 10 → distance 2
         ;; candidate b: center-y = 12 + 4>>1 = 14 → distance 2  (tied)
         (a    (make-pane :id 1 :fd -1 :pid -1 :x 0 :y  8 :width 10 :height 4
                           :screen (make-screen 10 4)))
         (b    (make-pane :id 2 :fd -1 :pid -1 :x 0 :y 12 :width 10 :height 4
                           :screen (make-screen 10 4))))
    ;; On a tie, reduce returns whichever of a and b was seen first, because
    ;; the predicate uses <=, so when distances are equal it keeps the first.
    (is (eq a (cl-tmux/model::%closest-to-center (list a b) pane
                                                  #'cl-tmux/model::%pane-center-y))
        "on a tie, the first candidate (a) must be returned")))

(test closest-to-center-three-candidates-non-trivial
  "%closest-to-center correctly picks the middle candidate among three."
  (let* ((pane (make-pane :id 0 :fd -1 :pid -1 :x 20 :y 0 :width 4 :height 10
                           :screen (make-screen 4 10)))
         ;; center-x of pane = 20 + 4>>1 = 22
         ;; candidate far-left:  center-x = 0  + 4>>1 = 2  → distance 20
         ;; candidate near-left: center-x = 16 + 4>>1 = 18 → distance 4  (closest)
         ;; candidate far-right: center-x = 40 + 4>>1 = 42 → distance 20
         (far-left  (make-pane :id 1 :fd -1 :pid -1 :x  0 :y 0 :width 4 :height 10
                                :screen (make-screen 4 10)))
         (near-left (make-pane :id 2 :fd -1 :pid -1 :x 16 :y 0 :width 4 :height 10
                                :screen (make-screen 4 10)))
         (far-right (make-pane :id 3 :fd -1 :pid -1 :x 40 :y 0 :width 4 :height 10
                                :screen (make-screen 4 10))))
    (is (eq near-left
            (cl-tmux/model::%closest-to-center (list far-left near-left far-right)
                                                pane #'cl-tmux/model::%pane-center-x))
        "near-left has the smallest center-x distance and must win")))

;;; ── layout-split-axis-extent with :v split ───────────────────────────────────

(test layout-split-axis-extent-v-split
  "For a :v split, axis-extent along :v = total height of both panes + separator."
  (let* ((l0   (tl-leaf 1 1 1))
         (l1   (tl-leaf 2 1 1))
         (tree (make-layout-split :v l0 l1)))
    (cl-tmux/model::layout-assign tree 0 0 80 21)
    ;; The :v extent = total bounding box height = 21.
    (is (= 21 (cl-tmux/model::layout-split-axis-extent tree :v)))
    ;; The :h extent = 80 (full width).
    (is (= 80 (cl-tmux/model::layout-split-axis-extent tree :h)))))

;;; ── resize-find-split in a nested tree ───────────────────────────────────────

(test resize-find-split-nested-tree-climbs-to-ancestor
  "resize-find-split climbs past an intermediate split to find the correct ancestor."
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
      (is (eq outer split)
          "nearest :h ancestor of 'top' is outer, not inner")
      (is (eq :second side)
          "'top' descends from the :second branch of outer"))))

;;; ── pane-at-position out-of-bounds y ────────────────────────────────────────

(test pane-at-position-out-of-bounds-returns-nil
  "pane-at-position returns NIL for coordinates outside all pane rectangles."
  (with-h-split-81-24 (p0 p1 win)
    ;; Row 24 is one past the bottom of all 24-row panes.
    (is (null (cl-tmux/model:pane-at-position win 0 24))
        "row 24 is out-of-bounds and must return NIL")
    ;; Column 81 is one past the right edge of the 81-column window.
    (is (null (cl-tmux/model:pane-at-position win 81 0))
        "col 81 is out-of-bounds and must return NIL")))

;;; ── orient-case with non-keyword signals ecase error ────────────────────────

(test orient-case-signals-on-unknown-orientation
  "orient-case raises a condition for an orientation other than :h or :v."
  (signals error
    (cl-tmux/model::orient-case :diagonal :h :horiz :v :vert)))

;;; ── Table-driven: split-child-geometry boundary dimensions ───────────────────
;;;
;;; split-child-geometry uses floor/ceiling arithmetic; test with both
;;; even and odd extents to cover both rounding paths.

(test split-child-geometry-v-odd-height-correct-division
  "Vertical split of an odd-height pane: avail=odd, fh=floor(odd/2), child gets remainder."
  ;; pane height = 11; avail = 10; fh = floor(10/2) = 5
  ;; child: y = 0 + 5 + 1 = 6, height = 10 - 5 = 5
  (let ((pane (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 40 :height 11
                         :screen (make-screen 40 11))))
    (multiple-value-bind (nx ny nw nh)
        (cl-tmux/model::split-child-geometry pane :v)
      (is (= 0  nx)  "v-split odd: new child x must equal parent x (0)")
      (is (= 6  ny)  "v-split odd: new child y must be 6")
      (is (= 40 nw)  "v-split odd: new child width must equal parent width")
      (is (= 5  nh)  "v-split odd: new child height must be 5"))))

(test split-child-geometry-h-odd-width-correct-division
  "Horizontal split of an odd-width pane: avail=odd, fw=floor(odd/2), child gets remainder."
  ;; pane width = 11; avail = 10; fw = floor(10/2) = 5
  ;; child: x = 0 + 5 + 1 = 6, width = 10 - 5 = 5
  (let ((pane (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 11 :height 24
                         :screen (make-screen 11 24))))
    (multiple-value-bind (nx ny nw nh)
        (cl-tmux/model::split-child-geometry pane :h)
      (is (= 6  nx)  "h-split odd: new child x must be 6")
      (is (= 0  ny)  "h-split odd: new child y must equal parent y (0)")
      (is (= 5  nw)  "h-split odd: new child width must be 5")
      (is (= 24 nh)  "h-split odd: new child height must equal parent height"))))

;;; ── pane-neighbor with three panes (non-trivial tie-breaking) ────────────────

(test pane-neighbor-three-panes-middle-pane-finds-both-neighbors
  "In a 3-pane horizontal layout, the middle pane finds both left and right neighbors."
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
    (is (eq p0 (pane-neighbor win p1 :left))
        "left neighbor of middle pane must be left pane")
    (is (eq p2 (pane-neighbor win p1 :right))
        "right neighbor of middle pane must be right pane")))

;;; ── define-axis-rules / define-neighbor-finders macros ──────────────────────

(test define-axis-rules-generates-correct-dispatch
  "define-axis-rules generates %orient-pane-extent with :v→height, :h→width."
  (let ((pane (make-pane :id 1 :fd -1 :pid -1 :width 30 :height 12
                         :screen (make-screen 30 12))))
    (is (= 12 (cl-tmux/model::%orient-pane-extent pane :v))
        ":v extent must return pane-height (12)")
    (is (= 30 (cl-tmux/model::%orient-pane-extent pane :h))
        ":h extent must return pane-width (30)")))

(test neighbor-filters-alist-has-all-four-directions
  "*neighbor-filters* alist contains entries for :right :left :down :up."
  (let ((dirs (mapcar #'car cl-tmux/model::*neighbor-filters*)))
    (is (member :right dirs) "*neighbor-filters* must have :right entry")
    (is (member :left  dirs) "*neighbor-filters* must have :left entry")
    (is (member :down  dirs) "*neighbor-filters* must have :down entry")
    (is (member :up    dirs) "*neighbor-filters* must have :up entry")))

(test neighbor-center-fn-alist-has-all-four-directions
  "*neighbor-center-fn* alist maps all four directions to center functions."
  (let ((dirs (mapcar #'car cl-tmux/model::*neighbor-center-fn*)))
    (is (member :right dirs) "*neighbor-center-fn* must have :right entry")
    (is (member :left  dirs) "*neighbor-center-fn* must have :left entry")
    (is (member :down  dirs) "*neighbor-center-fn* must have :down entry")
    (is (member :up    dirs) "*neighbor-center-fn* must have :up entry")))

;;; ── Table-driven: layout-min-extent for mixed nested trees ──────────────────

(test layout-min-extent-nested-v-h-split
  "A :v split wrapping a :h split has correct min extents in both axes."
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
    (is (= 3 (cl-tmux/model::layout-min-extent outer :v))
        "outer :v extent must be inner-v + 1 + leaf-v = 1+1+1 = 3")
    (is (= 5 (cl-tmux/model::layout-min-extent outer :h))
        "outer :h extent must be max(inner-h=5, leaf-h=2) = 5")))
