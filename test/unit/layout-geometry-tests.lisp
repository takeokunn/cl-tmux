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

(test pane-neighbor-right
  "Right neighbor of the left pane in a side-by-side split is the right pane."
  ;; Window 81 wide x 24 tall, split :h 50/50 → left pane x=0 w=40, right pane x=41 w=40
  (let* ((p0   (make-pane :id 1 :fd -1 :pid -1
                           :x 0 :y 0 :width 40 :height 24
                           :screen (make-screen 40 24)))
         (p1   (make-pane :id 2 :fd -1 :pid -1
                           :x 41 :y 0 :width 40 :height 24
                           :screen (make-screen 40 24)))
         (win  (make-window :id 1 :name "w" :width 81 :height 24
                            :panes (list p0 p1)
                            :tree (make-layout-split :h
                                    (make-layout-leaf p0)
                                    (make-layout-leaf p1)
                                    1/2))))
    (window-select-pane win p0)
    (is (eq p1 (pane-neighbor win p0 :right))
        "Right neighbor of p0 must be p1")))

(test pane-neighbor-left
  "Left neighbor of the right pane in a side-by-side split is the left pane."
  (let* ((p0   (make-pane :id 1 :fd -1 :pid -1
                           :x 0 :y 0 :width 40 :height 24
                           :screen (make-screen 40 24)))
         (p1   (make-pane :id 2 :fd -1 :pid -1
                           :x 41 :y 0 :width 40 :height 24
                           :screen (make-screen 40 24)))
         (win  (make-window :id 1 :name "w" :width 81 :height 24
                            :panes (list p0 p1)
                            :tree (make-layout-split :h
                                    (make-layout-leaf p0)
                                    (make-layout-leaf p1)
                                    1/2))))
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
