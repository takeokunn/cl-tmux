(in-package #:cl-tmux/test)

;;;; Tests for layout-geometry.lisp — rectangle assignment and resize helpers.

(def-suite layout-geometry-suite :description "Rectangle assignment and resize helpers")
(in-suite layout-geometry-suite)

;;; ── Orientation helpers (%axis-floor, %orient-pane-extent) ──────────────────

(test axis-floor-returns-correct-minimum
  "%axis-floor returns +pane-min-height+ for :v, +pane-min-width+ for :h."
  (is (= cl-tmux/model::+pane-min-height+ (cl-tmux/model::%axis-floor :v))
      ":v axis minimum must equal +pane-min-height+")
  (is (= cl-tmux/model::+pane-min-width+  (cl-tmux/model::%axis-floor :h))
      ":h axis minimum must equal +pane-min-width+"))

(test orient-pane-extent-returns-dimension
  "%orient-pane-extent returns height for :v, width for :h."
  (let ((pane (make-pane :id 1 :fd -1 :pid -1 :width 40 :height 15
                         :screen (make-screen 40 15))))
    (is (= 15 (cl-tmux/model::%orient-pane-extent pane :v))
        ":v extent must equal pane height (15)")
    (is (= 40 (cl-tmux/model::%orient-pane-extent pane :h))
        ":h extent must equal pane width (40)")))

;;; ── layout-assign direct tests (pure geometry, no PTY) ─────────────────────

(test layout-assign-single-leaf-fills-rect
  "A single leaf gets the full rectangle."
  (let* ((p    (make-pane :id 1 :fd -1 :pid -1 :width 1 :height 1
                          :screen (make-screen 1 1)))
         (leaf (make-layout-leaf p)))
    (cl-tmux/model::layout-assign leaf 3 5 40 20)
    (is (= 3  (pane-x p)))
    (is (= 5  (pane-y p)))
    (is (= 40 (pane-width p)))
    (is (= 20 (pane-height p)))))

(test layout-assign-h-split-divides-width
  "A :h split divides width: left gets ratio share, right gets remainder, one separator."
  (let* ((p0   (make-pane :id 1 :fd -1 :pid -1 :width 1 :height 1 :screen (make-screen 1 1)))
         (p1   (make-pane :id 2 :fd -1 :pid -1 :width 1 :height 1 :screen (make-screen 1 1)))
         (tree (make-layout-split :h (make-layout-leaf p0) (make-layout-leaf p1) 1/2)))
    (cl-tmux/model::layout-assign tree 0 0 81 24)
    ;; 81 cols - 1 separator = 80, split 50/50 → 40 each
    (is (= 0  (pane-x p0)))
    (is (= 40 (pane-width p0)))
    (is (= 41 (pane-x p1)))
    (is (= 40 (pane-width p1)))
    (is (= 24 (pane-height p0)))
    (is (= 24 (pane-height p1)))))

;;; ── layout-split-axis-extent direct tests ─────────────────────────────────

(test layout-split-axis-extent-h-split
  "For a :h split, axis-extent along :h = total width of both panes + separator."
  (let* ((l0   (tl-leaf 1 1 1))
         (l1   (tl-leaf 2 1 1))
         (tree (make-layout-split :h l0 l1)))
    ;; Lay out first so pane x/y/w/h are set.
    (cl-tmux/model::layout-assign tree 0 0 81 24)
    ;; The :h extent should be 81 (total width of the bounding box of all leaves).
    (is (= 81 (cl-tmux/model::layout-split-axis-extent tree :h)))
    ;; The :v extent should be 24.
    (is (= 24 (cl-tmux/model::layout-split-axis-extent tree :v)))))

;;; ── resize-find-split direct tests ──────────────────────────────────────────

(test resize-find-split-finds-nearest-ancestor
  "resize-find-split returns the nearest :h ancestor and the leaf's side."
  (let* ((l0   (tl-leaf 1 1 1))
         (l1   (tl-leaf 2 1 1))
         (tree (make-layout-split :h l0 l1)))
    ;; l0 is :first child of the :h split
    (multiple-value-bind (split side)
        (cl-tmux/model::resize-find-split tree l0 :h)
      (is (eq tree split))
      (is (eq :first side)))
    ;; l1 is :second child
    (multiple-value-bind (split side)
        (cl-tmux/model::resize-find-split tree l1 :h)
      (is (eq tree split))
      (is (eq :second side)))))

(test resize-find-split-returns-nil-for-wrong-orientation
  "No :v split exists in a pure :h tree — returns NIL."
  (let* ((l0   (tl-leaf 1 1 1))
         (l1   (tl-leaf 2 1 1))
         (tree (make-layout-split :h l0 l1)))
    (multiple-value-bind (split side)
        (cl-tmux/model::resize-find-split tree l0 :v)
      (is (null split))
      (is (null side)))))

;;; ── resize-direction-orientation mapping ─────────────────────────────────

(test resize-direction-orientation-mapping
  ":left/:right map to :h; :up/:down map to :v."
  (is (eq :h (cl-tmux/model::resize-direction-orientation :left)))
  (is (eq :h (cl-tmux/model::resize-direction-orientation :right)))
  (is (eq :v (cl-tmux/model::resize-direction-orientation :up)))
  (is (eq :v (cl-tmux/model::resize-direction-orientation :down))))

;;; ── pane-neighbor tests ──────────────────────────────────────────────────────
;;;
;;; Shared fixture: with-h-split-window and with-v-split-window from helpers.lisp
;;; replace the previously triplicated inline 81x24 two-pane window construction.

(test pane-neighbor-right
  "Right neighbor of the left pane in a side-by-side split is the right pane."
  ;; Window 81 wide x 24 tall, split :h 50/50 → left pane x=0 w=40, right pane x=41 w=40
  (with-h-split-window (win p0 p1)
    (is (eq p1 (pane-neighbor win p0 :right))
        "Right neighbor of p0 must be p1")))

(test pane-neighbor-left
  "Left neighbor of the right pane in a side-by-side split is the left pane."
  (with-h-split-window (win p0 p1)
    (window-select-pane win p1)
    (is (eq p0 (pane-neighbor win p1 :left))
        "Left neighbor of p1 must be p0")))

(test pane-neighbor-nil
  "A single pane has no neighbors in any direction."
  (let* ((p0  (make-pane :id 1 :fd -1 :pid -1
                          :x 0 :y 0 :width 80 :height 24
                          :screen (make-screen 80 24)))
         (win (make-window :id 1 :name "w" :width 80 :height 24
                           :panes (list p0)
                           :tree (make-layout-leaf p0))))
    (window-select-pane win p0)
    (is (null (pane-neighbor win p0 :right))  "No right neighbor when alone")
    (is (null (pane-neighbor win p0 :left))   "No left neighbor when alone")
    (is (null (pane-neighbor win p0 :up))     "No up neighbor when alone")
    (is (null (pane-neighbor win p0 :down))   "No down neighbor when alone")))

(test pane-neighbor-down
  "Down neighbor of the top pane in a top/bottom split is the bottom pane."
  ;; Window 80 wide x 21 tall, split :v → top pane y=0 h=10, bottom pane y=11 h=10
  (with-v-split-window (win p0 p1)
    (is (eq p1 (pane-neighbor win p0 :down))
        "Down neighbor of top pane (p0) must be the bottom pane (p1)")))

(test pane-neighbor-up
  "Up neighbor of the bottom pane in a top/bottom split is the top pane."
  (with-v-split-window (win p0 p1)
    (window-select-pane win p1)
    (is (eq p0 (pane-neighbor win p1 :up))
        "Up neighbor of bottom pane (p1) must be the top pane (p0)")))

;;; ── pane-at-position tests ───────────────────────────────────────────────────
;;;
;;; All three tests share with-h-split-81-24 to avoid repeating the fixture.

(test pane-at-position-finds-pane-in-left-half
  "pane-at-position returns the left pane when clicking in its column range."
  (with-h-split-81-24 (p0 p1 win)
    (declare (ignore p1))
    (is (eq p0 (cl-tmux/model:pane-at-position win 0 0))
        "Top-left corner must be in p0")
    (is (eq p0 (cl-tmux/model:pane-at-position win 39 23))
        "Bottom-right corner of p0 must be in p0")))

(test pane-at-position-finds-pane-in-right-half
  "pane-at-position returns the right pane when clicking in its column range."
  (with-h-split-81-24 (p0 p1 win)
    (declare (ignore p0))
    (is (eq p1 (cl-tmux/model:pane-at-position win 41 0))
        "Start of right pane must be in p1")
    (is (eq p1 (cl-tmux/model:pane-at-position win 80 23))
        "Bottom-right of right pane must be in p1")))

(test pane-at-position-separator-returns-nil
  "pane-at-position returns NIL for the separator column between panes."
  (with-h-split-81-24 (p0 p1 win)
    (declare (ignore p0 p1))
    (is (null (cl-tmux/model:pane-at-position win 40 0))
        "Separator column 40 must not be in any pane")))

(test pane-at-position-single-pane
  "pane-at-position with a single full-screen pane returns it for any in-bounds coord."
  (let* ((p0  (make-pane :id 1 :fd -1 :pid -1
                          :x 0 :y 0 :width 80 :height 24
                          :screen (make-screen 80 24)))
         (win (make-window :id 1 :name "w" :width 80 :height 24
                           :panes (list p0)
                           :tree  (make-layout-leaf p0))))
    (is (eq p0 (cl-tmux/model:pane-at-position win 0 0))   "origin")
    (is (eq p0 (cl-tmux/model:pane-at-position win 79 23)) "max corner")
    (is (null (cl-tmux/model:pane-at-position win 80 0))   "out-of-bounds col")))

;;; ── orient-case macro ────────────────────────────────────────────────────────

(test orient-case-dispatches-on-h
  "orient-case evaluates the :h branch for orientation :h."
  (is (eq :horizontal
          (cl-tmux/model::orient-case :h :h :horizontal :v :vertical))
      ":h orientation must evaluate the :h branch")
  (is (= 40
         (cl-tmux/model::orient-case :h :h 40 :v 20))
      ":h orientation must return the :h value (40)"))

(test orient-case-dispatches-on-v
  "orient-case evaluates the :v branch for orientation :v."
  (is (eq :vertical
          (cl-tmux/model::orient-case :v :h :horizontal :v :vertical))
      ":v orientation must evaluate the :v branch")
  (is (= 20
         (cl-tmux/model::orient-case :v :h 40 :v 20))
      ":v orientation must return the :v value (20)"))

;;; ── split-child-geometry tests ──────────────────────────────────────────────
;;;
;;; split-child-geometry returns provisional x,y,w,h for the NEW child pane.
;;; The geometry is recomputed by window-relayout; this only seeds a sensible size.

(test split-child-geometry-vertical-split-dimensions
  "Vertical split: new child gets roughly half the height below the divider."
  (let ((pane (make-pane :id 1 :fd -1 :pid -1
                          :x 0 :y 0 :width 80 :height 21
                          :screen (make-screen 80 21))))
    (multiple-value-bind (nx ny nw nh)
        (cl-tmux/model::split-child-geometry pane :v)
      ;; avail = 21 - 1 = 20; fh = floor(20/2) = 10 → child starts at y=11, h=10
      (is (= 0  nx) ":v split: new child x must equal parent x")
      (is (= 11 ny) ":v split: new child y must be pane-y + fh + 1")
      (is (= 80 nw) ":v split: new child width must equal parent width")
      (is (= 10 nh) ":v split: new child height must be avail - fh"))))

(test split-child-geometry-horizontal-split-dimensions
  "Horizontal split: new child gets roughly half the width to the right of the divider."
  (let ((pane (make-pane :id 1 :fd -1 :pid -1
                          :x 0 :y 0 :width 81 :height 24
                          :screen (make-screen 81 24))))
    (multiple-value-bind (nx ny nw nh)
        (cl-tmux/model::split-child-geometry pane :h)
      ;; avail = 81 - 1 = 80; fw = floor(80/2) = 40 → child starts at x=41, w=40
      (is (= 41 nx) ":h split: new child x must be pane-x + fw + 1")
      (is (= 0  ny) ":h split: new child y must equal parent y")
      (is (= 40 nw) ":h split: new child width must be avail - fw")
      (is (= 24 nh) ":h split: new child height must equal parent height"))))

;;; ── layout-min-extent tests ──────────────────────────────────────────────────

(test layout-min-extent-single-leaf-v
  "A single leaf has minimum vertical extent of +pane-min-height+."
  (let* ((p    (make-pane :id 1 :fd -1 :pid -1 :width 40 :height 15
                          :screen (make-screen 40 15)))
         (leaf (make-layout-leaf p)))
    (is (= cl-tmux/model::+pane-min-height+
           (cl-tmux/model::layout-min-extent leaf :v))
        "leaf :v extent must equal +pane-min-height+")))

(test layout-min-extent-single-leaf-h
  "A single leaf has minimum horizontal extent of +pane-min-width+."
  (let* ((p    (make-pane :id 1 :fd -1 :pid -1 :width 40 :height 15
                          :screen (make-screen 40 15)))
         (leaf (make-layout-leaf p)))
    (is (= cl-tmux/model::+pane-min-width+
           (cl-tmux/model::layout-min-extent leaf :h))
        "leaf :h extent must equal +pane-min-width+")))

(test layout-min-extent-h-split-same-axis
  "A :h split's minimum :h extent = sum of children's :h extents + 1 separator."
  (let* ((l0   (tl-leaf 1 1 1))
         (l1   (tl-leaf 2 1 1))
         (tree (make-layout-split :h l0 l1)))
    (let ((expected (+ cl-tmux/model::+pane-min-width+
                       1
                       cl-tmux/model::+pane-min-width+)))
      (is (= expected (cl-tmux/model::layout-min-extent tree :h))
          ":h split :h extent must be min-width + 1 + min-width"))))

(test layout-min-extent-h-split-cross-axis
  "A :h split's minimum :v extent = max of children's :v extents (no separator)."
  (let* ((l0   (tl-leaf 1 1 1))
         (l1   (tl-leaf 2 1 1))
         (tree (make-layout-split :h l0 l1)))
    (is (= cl-tmux/model::+pane-min-height+
           (cl-tmux/model::layout-min-extent tree :v))
        ":h split :v extent must be max of children's :v extents")))

(test layout-min-extent-nil-node
  "layout-min-extent on NIL returns 0."
  (is (= 0 (cl-tmux/model::layout-min-extent nil :h))
      "nil node :h extent must be 0")
  (is (= 0 (cl-tmux/model::layout-min-extent nil :v))
      "nil node :v extent must be 0"))

;;; ── layout-assign v-split geometry ──────────────────────────────────────────

(test layout-assign-v-split-divides-height
  "A :v split divides height: top gets ratio share, bottom gets remainder, one separator."
  (let* ((p0   (make-pane :id 1 :fd -1 :pid -1 :width 1 :height 1 :screen (make-screen 1 1)))
         (p1   (make-pane :id 2 :fd -1 :pid -1 :width 1 :height 1 :screen (make-screen 1 1)))
         (tree (make-layout-split :v (make-layout-leaf p0) (make-layout-leaf p1) 1/2)))
    (cl-tmux/model::layout-assign tree 0 0 80 21)
    ;; 21 rows - 1 separator = 20, split 50/50 → 10 each
    (is (= 0  (pane-y p0)))
    (is (= 10 (pane-height p0)))
    (is (= 11 (pane-y p1)))
    (is (= 10 (pane-height p1)))
    (is (= 80 (pane-width p0)))
    (is (= 80 (pane-width p1)))))

;;; ── %assign-split boundary clamping ──────────────────────────────────────────
;;;
;;; %assign-split clamps fext to [1, avail-1] so neither child vanishes even
;;; when the ratio is extreme (near 0 or near 1).  These tests exercise both
;;; extremes to confirm the clamping invariant holds.

(test assign-split-extreme-ratio-near-zero-clamps-first-child
  "%assign-split clamps a near-zero ratio so the first child has at least 1 cell."
  ;; ratio = 1/100: avail = 80-1 = 79, round(79/100) = 1 → fext = max(1, ...) = 1.
  (let* ((p0   (make-pane :id 1 :fd -1 :pid -1 :width 1 :height 1 :screen (make-screen 1 1)))
         (p1   (make-pane :id 2 :fd -1 :pid -1 :width 1 :height 1 :screen (make-screen 1 1)))
         (tree (make-layout-split :h (make-layout-leaf p0) (make-layout-leaf p1) 1/100)))
    (cl-tmux/model::layout-assign tree 0 0 80 24)
    (is (>= (pane-width p0) 1)  "first child must be at least 1 cell wide")
    (is (>= (pane-width p1) 1)  "second child must be at least 1 cell wide")
    (is (= 79 (+ (pane-width p0) (pane-width p1)))
        "first + second children must equal avail (80-1=79)")))

(test assign-split-extreme-ratio-near-one-clamps-second-child
  "%assign-split clamps a near-unity ratio so the second child has at least 1 cell."
  ;; ratio = 99/100: avail = 80-1 = 79, round(79*99/100) = 78 → sext = 79-78 = 1.
  (let* ((p0   (make-pane :id 1 :fd -1 :pid -1 :width 1 :height 1 :screen (make-screen 1 1)))
         (p1   (make-pane :id 2 :fd -1 :pid -1 :width 1 :height 1 :screen (make-screen 1 1)))
         (tree (make-layout-split :h (make-layout-leaf p0) (make-layout-leaf p1) 99/100)))
    (cl-tmux/model::layout-assign tree 0 0 80 24)
    (is (>= (pane-width p0) 1)  "first child must be at least 1 cell wide")
    (is (>= (pane-width p1) 1)  "second child must be at least 1 cell wide")
    (is (= 79 (+ (pane-width p0) (pane-width p1)))
        "first + second children must equal avail (80-1=79)")))

(test assign-split-exact-half-ratio-distributes-evenly
  "%assign-split with ratio=1/2 on an even avail gives equal children."
  ;; 81 cols: avail = 80, fext = round(40) = 40, sext = 40.
  (let* ((p0   (make-pane :id 1 :fd -1 :pid -1 :width 1 :height 1 :screen (make-screen 1 1)))
         (p1   (make-pane :id 2 :fd -1 :pid -1 :width 1 :height 1 :screen (make-screen 1 1)))
         (tree (make-layout-split :h (make-layout-leaf p0) (make-layout-leaf p1) 1/2)))
    (cl-tmux/model::layout-assign tree 0 0 81 24)
    (is (= 40 (pane-width p0)) "equal split: first child must be 40 cols")
    (is (= 40 (pane-width p1)) "equal split: second child must be 40 cols")))

;;; ── %ranges-overlap-p direct tests ──────────────────────────────────────────
;;;
;;; %ranges-overlap-p is a pure predicate used by the neighbor-filter lambdas.
;;; It is not exported, so we access it through the internal package name.

(test ranges-overlap-p-overlapping-ranges
  "%ranges-overlap-p returns T for two overlapping integer ranges."
  (is (cl-tmux/model::%ranges-overlap-p 0 5 3 5)
      "[0,5) and [3,8) overlap at 3..4")
  (is (cl-tmux/model::%ranges-overlap-p 0 10 5 3)
      "[0,10) and [5,8) overlap")
  (is (cl-tmux/model::%ranges-overlap-p 5 5 4 2)
      "[5,10) and [4,6) share 5"))

(test ranges-overlap-p-touching-but-not-overlapping
  "%ranges-overlap-p returns NIL for adjacent (touching) but non-overlapping ranges."
  (is (null (cl-tmux/model::%ranges-overlap-p 0 5 5 5))
      "[0,5) and [5,10) are adjacent — no overlap")
  (is (null (cl-tmux/model::%ranges-overlap-p 5 5 0 5))
      "[5,10) and [0,5) are adjacent — no overlap"))

(test ranges-overlap-p-disjoint-ranges
  "%ranges-overlap-p returns NIL for fully disjoint ranges."
  (is (null (cl-tmux/model::%ranges-overlap-p 0 3 10 5))
      "[0,3) and [10,15) are disjoint")
  (is (null (cl-tmux/model::%ranges-overlap-p 10 5 0 3))
      "[10,15) and [0,3) are disjoint"))

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
    (declare (ignore p0 p1))
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
