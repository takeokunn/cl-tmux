(in-package #:cl-tmux/test)

;;;; Tests for layout-geometry.lisp — rectangle assignment and resize helpers.

(describe "layout-geometry-suite"

  ;; ── Orientation helpers (%axis-floor) ──────────────────────────────────────

  ;; %axis-floor returns +pane-min-height+ for :v, +pane-min-width+ for :h.
  (it "axis-floor-returns-correct-minimum"
    (expect (= cl-tmux/model::+pane-min-height+ (cl-tmux/model::%axis-floor :v)))
    (expect (= cl-tmux/model::+pane-min-width+  (cl-tmux/model::%axis-floor :h))))

  ;; orient-pane-extent coverage lives in layout-geometry-tests-b.lisp
  ;; (define-axis-rules-generates-correct-dispatch), not here:
  ;; %orient-pane-extent is defined in window.lisp, not layout-geometry.lisp.

  ;; ── layout-assign direct tests (pure geometry, no PTY) ─────────────────────

  ;; A single leaf gets the full rectangle.
  (it "layout-assign-single-leaf-fills-rect"
    (let* ((p    (make-pane :id 1 :fd -1 :pid -1 :width 1 :height 1
                            :screen (make-screen 1 1)))
           (leaf (make-layout-leaf p)))
      (cl-tmux/model::layout-assign leaf 3 5 40 20)
      (check-table (list (list (pane-x p) 3 "pane-x")
                         (list (pane-y p) 5 "pane-y")
                         (list (pane-width p) 40 "pane-width")
                         (list (pane-height p) 20 "pane-height")))))

  ;; A :h split divides width: left gets ratio share, right gets remainder, one separator.
  (it "layout-assign-h-split-divides-width"
    (with-two-1x1-panes (p0 p1)
      (let ((tree (make-layout-split :h (make-layout-leaf p0) (make-layout-leaf p1) 1/2)))
        (cl-tmux/model::layout-assign tree 0 0 81 24)
        ;; 81 cols - 1 separator = 80, split 50/50 → 40 each
        (check-table (list (list (pane-x p0) 0 "p0 at column 0")
                           (list (pane-width p0) 40 "p0 width 40")
                           (list (pane-x p1) 41 "p1 starts one past separator")
                           (list (pane-width p1) 40 "p1 width 40")
                           (list (pane-height p0) 24 "p0 full height")
                           (list (pane-height p1) 24 "p1 full height"))))))

  ;; ── layout-split-axis-extent direct tests ─────────────────────────────────

  ;; For a :h split, axis-extent along :h = total width of both panes + separator.
  (it "layout-split-axis-extent-h-split"
    (let* ((l0   (tl-leaf 1 1 1))
           (l1   (tl-leaf 2 1 1))
           (tree (make-layout-split :h l0 l1)))
      ;; Lay out first so pane x/y/w/h are set.
      (cl-tmux/model::layout-assign tree 0 0 81 24)
      ;; The :h extent should be 81 (total width of the bounding box of all leaves).
      (expect (= 81 (cl-tmux/model::layout-split-axis-extent tree :h)))
      ;; The :v extent should be 24.
      (expect (= 24 (cl-tmux/model::layout-split-axis-extent tree :v)))))

  ;; ── resize-find-split direct tests ──────────────────────────────────────────

  ;; resize-find-split returns the nearest :h ancestor and the leaf's side.
  (it "resize-find-split-finds-nearest-ancestor"
    (let* ((l0   (tl-leaf 1 1 1))
           (l1   (tl-leaf 2 1 1))
           (tree (make-layout-split :h l0 l1)))
      ;; l0 is :first child of the :h split
      (multiple-value-bind (split side)
          (cl-tmux/model::resize-find-split tree l0 :h)
        (expect (eq tree split))
        (expect (eq :first side)))
      ;; l1 is :second child
      (multiple-value-bind (split side)
          (cl-tmux/model::resize-find-split tree l1 :h)
        (expect (eq tree split))
        (expect (eq :second side)))))

  ;; No :v split exists in a pure :h tree — returns NIL.
  (it "resize-find-split-returns-nil-for-wrong-orientation"
    (let* ((l0   (tl-leaf 1 1 1))
           (l1   (tl-leaf 2 1 1))
           (tree (make-layout-split :h l0 l1)))
      (multiple-value-bind (split side)
          (cl-tmux/model::resize-find-split tree l0 :v)
        (expect (null split))
        (expect (null side)))))

  ;; ── resize-direction-orientation mapping ─────────────────────────────────

  ;; :left/:right map to :h; :up/:down map to :v.
  (it "resize-direction-orientation-mapping"
    (dolist (c '((:left :h) (:right :h) (:up :v) (:down :v)))
      (destructuring-bind (dir expected) c
        (expect (eq expected (cl-tmux/model::resize-direction-orientation dir))))))

  ;; ── pane-neighbor tests ──────────────────────────────────────────────────────
  ;;
  ;; Shared fixture: with-h-split-window and with-v-split-window from helpers-layout-fixtures.lisp
  ;; replace the previously triplicated inline 81x24 two-pane window construction.

  ;; In a side-by-side split: right neighbor of left pane is right; left neighbor of right pane is left.
  (it "pane-neighbor-h-split"
    (with-h-split-window (win p0 p1)
      (expect (eq p1 (pane-neighbor win p0 :right)))
      (window-select-pane win p1)
      (expect (eq p0 (pane-neighbor win p1 :left)))))

  ;; A single pane has no neighbors in any direction.
  (it "pane-neighbor-nil"
    (let* ((p0  (make-pane :id 1 :fd -1 :pid -1
                            :x 0 :y 0 :width 80 :height 24
                            :screen (make-screen 80 24)))
           (win (make-window :id 1 :name "w" :width 80 :height 24
                             :panes (list p0)
                             :tree (make-layout-leaf p0))))
      (window-select-pane win p0)
      (dolist (dir '(:right :left :up :down))
        (expect (null (pane-neighbor win p0 dir))))))

  ;; In a top/bottom split: down neighbor of top pane is bottom; up neighbor of bottom pane is top.
  (it "pane-neighbor-v-split"
    (with-v-split-window (win p0 p1)
      (expect (eq p1 (pane-neighbor win p0 :down)))
      (window-select-pane win p1)
      (expect (eq p0 (pane-neighbor win p1 :up)))))

  ;; ── pane-at-position tests ───────────────────────────────────────────────────
  ;;
  ;; All three tests share with-h-split-81-24 to avoid repeating the fixture.

  ;; pane-at-position finds left pane, right pane, or NIL (separator) by column in an h-split.
  (it "pane-at-position-h-split-table"
    (with-h-split-81-24 (p0 p1 win)
      (dolist (entry (list (list  0  0 p0  "origin → p0")
                           (list 39 23 p0  "bottom-right of p0 → p0")
                           (list 40  0 nil "separator col 40 → NIL")
                           (list 41  0 p1  "start of p1 → p1")
                           (list 80 23 p1  "bottom-right of p1 → p1")))
        (destructuring-bind (col row expected desc) entry
          (declare (ignore desc))
          (expect (equal expected (cl-tmux/model:pane-at-position win col row)))))))

  ;; pane-at-position with a single full-screen pane returns it for any in-bounds coord.
  (it "pane-at-position-single-pane"
    (let* ((p0  (make-pane :id 1 :fd -1 :pid -1
                            :x 0 :y 0 :width 80 :height 24
                            :screen (make-screen 80 24)))
           (win (make-window :id 1 :name "w" :width 80 :height 24
                             :panes (list p0)
                             :tree  (make-layout-leaf p0))))
      (dolist (entry (list (list  0  0 p0  "origin")
                           (list 79 23 p0  "max corner")
                           (list 80  0 nil "out-of-bounds col")))
        (destructuring-bind (col row expected desc) entry
          (declare (ignore desc))
          (expect (equal expected (cl-tmux/model:pane-at-position win col row)))))))

  ;; ── orient-case macro ────────────────────────────────────────────────────────

  ;; orient-case dispatches to the matching :h or :v branch.
  (it "orient-case-table"
    (dolist (row '((:h :horizontal 40 ":h orientation selects :h branch")
                   (:v :vertical   20 ":v orientation selects :v branch")))
      (destructuring-bind (orient expected-kw expected-num desc) row
        (declare (ignore desc))
        (expect (eq expected-kw
                    (cl-tmux/model::orient-case orient :h :horizontal :v :vertical)))
        (expect (= expected-num
                   (cl-tmux/model::orient-case orient :h 40 :v 20))))))

  ;; ── split-child-geometry tests ──────────────────────────────────────────────
  ;;
  ;; split-child-geometry returns provisional x,y,w,h for the NEW child pane.
  ;; The geometry is recomputed by window-relayout; this only seeds a sensible size.

  ;; Vertical split: new child gets roughly half the height below the divider.
  (it "split-child-geometry-vertical-split-dimensions"
    (let ((pane (make-pane :id 1 :fd -1 :pid -1
                            :x 0 :y 0 :width 80 :height 21
                            :screen (make-screen 80 21))))
      (multiple-value-bind (nx ny nw nh)
          (cl-tmux/model::split-child-geometry pane :v)
        ;; avail = 21 - 1 = 20; fh = floor(20/2) = 10 → child starts at y=11, h=10
        (check-table (list (list nx 0 ":v split: new child x must equal parent x")
                           (list ny 11 ":v split: new child y must be pane-y + fh + 1")
                           (list nw 80 ":v split: new child width must equal parent width")
                           (list nh 10 ":v split: new child height must be avail - fh"))))))

  ;; Horizontal split: new child gets roughly half the width to the right of the divider.
  (it "split-child-geometry-horizontal-split-dimensions"
    (let ((pane (make-pane :id 1 :fd -1 :pid -1
                            :x 0 :y 0 :width 81 :height 24
                            :screen (make-screen 81 24))))
      (multiple-value-bind (nx ny nw nh)
          (cl-tmux/model::split-child-geometry pane :h)
        ;; avail = 81 - 1 = 80; fw = floor(80/2) = 40 → child starts at x=41, w=40
        (check-table (list (list nx 41 ":h split: new child x must be pane-x + fw + 1")
                           (list ny 0 ":h split: new child y must equal parent y")
                           (list nw 40 ":h split: new child width must be avail - fw")
                           (list nh 24 ":h split: new child height must equal parent height"))))))

  ;; ── layout-min-extent tests ──────────────────────────────────────────────────

  ;; A single leaf's minimum extent equals the axis-specific pane-min constant.
  (it "layout-min-extent-single-leaf-table"
    (let* ((p    (make-pane :id 1 :fd -1 :pid -1 :width 40 :height 15
                            :screen (make-screen 40 15)))
           (leaf (make-layout-leaf p)))
      (check-table (list (list (cl-tmux/model::layout-min-extent leaf :v)
                               cl-tmux/model::+pane-min-height+ "leaf :v extent")
                         (list (cl-tmux/model::layout-min-extent leaf :h)
                               cl-tmux/model::+pane-min-width+ "leaf :h extent")))))

  ;; A :h split's minimum :h extent = sum of children's :h extents + 1 separator.
  (it "layout-min-extent-h-split-same-axis"
    (let* ((l0   (tl-leaf 1 1 1))
           (l1   (tl-leaf 2 1 1))
           (tree (make-layout-split :h l0 l1)))
      (let ((expected (+ cl-tmux/model::+pane-min-width+
                         1
                         cl-tmux/model::+pane-min-width+)))
        (expect (= expected (cl-tmux/model::layout-min-extent tree :h))))))

  ;; A :h split's minimum :v extent = max of children's :v extents (no separator).
  (it "layout-min-extent-h-split-cross-axis"
    (let* ((l0   (tl-leaf 1 1 1))
           (l1   (tl-leaf 2 1 1))
           (tree (make-layout-split :h l0 l1)))
      (expect (= cl-tmux/model::+pane-min-height+
                 (cl-tmux/model::layout-min-extent tree :v)))))

  ;; layout-min-extent on NIL returns 0.
  (it "layout-min-extent-nil-node"
    (expect (= 0 (cl-tmux/model::layout-min-extent nil :h)))
    (expect (= 0 (cl-tmux/model::layout-min-extent nil :v))))

  ;; ── layout-assign v-split geometry ──────────────────────────────────────────

  ;; A :v split divides height: top gets ratio share, bottom gets remainder, one separator.
  (it "layout-assign-v-split-divides-height"
    (with-two-1x1-panes (p0 p1)
      (let ((tree (make-layout-split :v (make-layout-leaf p0) (make-layout-leaf p1) 1/2)))
        (cl-tmux/model::layout-assign tree 0 0 80 21)
        ;; 21 rows - 1 separator = 20, split 50/50 → 10 each
        (expect (= 0  (pane-y p0)))
        (expect (= 10 (pane-height p0)))
        (expect (= 11 (pane-y p1)))
        (expect (= 10 (pane-height p1)))
        (expect (= 80 (pane-width p0)))
        (expect (= 80 (pane-width p1))))))

  ;; ── %assign-split boundary clamping ──────────────────────────────────────────
  ;;
  ;; %assign-split clamps first-extent to [1, available-cells-1] so neither child
  ;; vanishes when the ratio is extreme (near 0 or near 1).  These tests exercise
  ;; both extremes to confirm the clamping invariant holds.

  ;; %assign-split clamps extreme ratios so neither child vanishes (>= 1 cell each).
  (it "assign-split-extreme-ratio-clamping-table"
    (dolist (row '((1/100  "near-zero ratio: first child clamped to >= 1")
                   (99/100 "near-unity ratio: second child clamped to >= 1")))
      (destructuring-bind (ratio desc) row
        (declare (ignore desc))
        (with-two-1x1-panes (p0 p1)
          (let ((tree (make-layout-split :h (make-layout-leaf p0) (make-layout-leaf p1) ratio)))
            (cl-tmux/model::layout-assign tree 0 0 80 24)
            (expect (>= (pane-width p0) 1))
            (expect (>= (pane-width p1) 1))
            (expect (= 79 (+ (pane-width p0) (pane-width p1)))))))))

  ;; %assign-split with ratio=1/2 on an even avail gives equal children.
  (it "assign-split-exact-half-ratio-distributes-evenly"
    ;; 81 cols: avail = 80, first-extent = round(40) = 40, second-extent = 40.
    (with-two-1x1-panes (p0 p1)
      (let ((tree (make-layout-split :h (make-layout-leaf p0) (make-layout-leaf p1) 1/2)))
        (cl-tmux/model::layout-assign tree 0 0 81 24)
        (expect (= 40 (pane-width p0)))
        (expect (= 40 (pane-width p1))))))

  ;; ── %ranges-overlap-p direct tests ──────────────────────────────────────────
  ;;
  ;; %ranges-overlap-p is a pure predicate used by the neighbor-filter lambdas.
  )
