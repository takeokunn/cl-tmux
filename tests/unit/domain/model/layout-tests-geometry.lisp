(in-package #:cl-tmux/test)

;;;; Layout tree geometry tests -- layout-tree-suite core geometry and tree ops.

;;; ────────────────────────────────────────────────────────────────────────────
;;; SUITE: layout-tree-suite (binary split-tree layout)
;;; ────────────────────────────────────────────────────────────────────────────

;;; ── Helpers ──────────────────────────────────────────────────────────────
;;; tl-pane, tl-leaf, tl-window are defined in tests/helpers-layout-fixtures.lisp and available
;;; to all test files in the suite -- no local redefinitions needed here.

(describe "layout-tree-suite"

  ;;; ── layout-leaves / find helpers ─────────────────────────────────────────

  ;; layout-leaves returns every pane left/top-to-right/bottom.
  (it "layout-leaves-collects-in-order"
    (let* ((l0 (tl-leaf 1 10 10))
           (l1 (tl-leaf 2 10 10))
           (l2 (tl-leaf 3 10 10))
           ;; (a | (b / c))
           (tree (make-layout-split :h l0 (make-layout-split :v l1 l2))))
      (expect (equal (list (layout-leaf-pane l0)
                           (layout-leaf-pane l1)
                           (layout-leaf-pane l2))
                     (layout-leaves tree)))))

  ;; layout-find-parent locates the parent split and the child's side.
  (it "layout-find-parent-resolves-side"
    (let* ((l0 (tl-leaf 1 10 10))
           (l1 (tl-leaf 2 10 10))
           (split (make-layout-split :h l0 l1)))
      (multiple-value-bind (p w) (layout-find-parent split l1)
        (expect (eq split p))
        (expect (eq :second w)))))

  ;;; ── Split halves only the active pane ────────────────────────────────────

  ;; A single :h split fills the window: left + separator + right covers cols.
  (it "relayout-h-split-divides-only-into-two"
    (let* ((tree (make-layout-split :h (tl-leaf 1 1 1) (tl-leaf 2 1 1)))
           (win  (tl-window tree 24 81)))
      (destructuring-bind (p0 p1) (window-panes win)
        ;; 81 cols - 1 separator = 80 split 50/50 = 40/40.
        (check-table (list (list (pane-x p0) 0 "p0 at column 0")
                           (list (pane-width p0) 40 "p0 width 40")
                           (list (pane-x p1) 41 "right pane sits one column past the separator")
                           (list (pane-width p1) 40 "p1 width 40")
                           (list (pane-height p0) 24 "full height on a left/right split")
                           (list (pane-height p1) 24 "p1 height 24")
                           (list (- (pane-x p1) (+ (pane-x p0) (pane-width p0))) 1
                                 "exactly one separator column between them"))))))

  ;; A pane split top/bottom, its bottom half then split left/right, yields three
  ;; non-overlapping panes covering the window (no gaps/overlaps).
  (it "nested-mixed-layout-geometry"
    (let* ((top    (tl-leaf 1 1 1))
           (bl     (tl-leaf 2 1 1))
           (br     (tl-leaf 3 1 1))
           ;; (top / (bl | br))
           (tree   (make-layout-split :v top (make-layout-split :h bl br)))
           (win    (tl-window tree 25 80)))
      (destructuring-bind (ptop pbl pbr) (window-panes win)
        ;; Vertical (:v) split: 25 rows - 1 separator = 24, 12 top / 12 bottom.
        ;; Bottom is split left|right: 80 - 1 separator = 79, 40 left / 39 right
        ;; (banker's rounding of 39.5 -> 40 for the first child).
        (check-table (list (list (pane-y ptop) 0 "top pane at row 0")
                           (list (pane-height ptop) 12 "top pane height 12")
                           (list (pane-width ptop) 80 "top spans full width")
                           (list (pane-y pbl) 13 "bottom-left starts at row 13")
                           (list (pane-y pbr) 13 "bottom-right starts at row 13")
                           (list (pane-height pbl) 12 "bottom-left height 12")
                           (list (pane-height pbr) 12 "bottom-right height 12")
                           (list (pane-x pbl) 0 "bottom-left at column 0")
                           (list (pane-width pbl) 40 "bottom-left width 40")
                           (list (pane-x pbr) 41 "bottom-right at column 41")
                           (list (pane-width pbr) 39 "bottom-right width 39")))
        ;; No overlap between the two bottom panes.
        (expect (<= (+ (pane-x pbl) (pane-width pbl)) (pane-x pbr))))))

  ;;; ── Resize on each axis (tree) ───────────────────────────────────────────

  ;; On a left|right split, :right grows the active (left) pane and shrinks the
  ;; neighbour; the neighbour slides to stay one column past the new border.
  (it "resize-h-right-moves-border-and-reflows"
    (let* ((l0   (tl-leaf 1 1 1))
           (l1   (tl-leaf 2 1 1))
           (tree (make-layout-split :h l0 l1))
           (win  (tl-window tree 24 81 :active (layout-leaf-pane l0))))
      (destructuring-bind (p0 p1) (window-panes win)
        (expect (= 40 (pane-width p0)))
        (expect (eq p0 (window-resize-active win :right 5)))
        (expect (= 45 (pane-width p0)))
        (expect (= 35 (pane-width p1)))
        (expect (= 46 (pane-x p1))))))

  ;; On a top/bottom split, :down grows the active (top) pane and shrinks the
  ;; lower neighbour -- i.e. resize works on the vertical axis too (not gated on a
  ;; single global orientation).
  (it "resize-v-down-moves-border"
    (let* ((l0   (tl-leaf 1 1 1))
           (l1   (tl-leaf 2 1 1))
           (tree (make-layout-split :v l0 l1))
           (win  (tl-window tree 25 80 :active (layout-leaf-pane l0))))
      (destructuring-bind (p0 p1) (window-panes win)
        (expect (= 12 (pane-height p0)))
        (expect (eq p0 (window-resize-active win :down 3)))
        (expect (= 15 (pane-height p0)))
        (expect (= 9  (pane-height p1)))
        (expect (= 16 (pane-y p1))))))

  ;; In a nested layout the active pane can be resized along BOTH axes: the left
  ;; pane of (left | (top/bottom)) resizes :right against the outer :h split.
  (it "resize-orthogonal-axis-finds-ancestor"
    (let* ((left  (tl-leaf 1 1 1))
           (rt    (tl-leaf 2 1 1))
           (rb    (tl-leaf 3 1 1))
           (tree  (make-layout-split :h left (make-layout-split :v rt rb)))
           (win   (tl-window tree 25 81 :active (layout-leaf-pane left))))
      (destructuring-bind (pl prt prb) (window-panes win)
        (let ((w-before (pane-width pl)))
          (expect (eq pl (window-resize-active win :right 4)))
          (expect (= (+ w-before 4) (pane-width pl)))
          ;; Both right-column panes shrink and stay vertically stacked.
          (expect (= (pane-x prt) (pane-x prb)))
          (expect (< (pane-y prt) (pane-y prb)))))))

  ;; A direction with no neighbour returns NIL and changes nothing.
  (it "resize-no-neighbour-is-noop"
    (let* ((l0   (tl-leaf 1 1 1))
           (l1   (tl-leaf 2 1 1))
           (tree (make-layout-split :h l0 l1))
           (win  (tl-window tree 24 81 :active (layout-leaf-pane l0))))
      (let ((w0 (pane-width (first (window-panes win)))))
        ;; A pure left|right split has no top/bottom border to move.
        (expect (null (window-resize-active win :up 5)))
        (expect (= w0 (pane-width (first (window-panes win))))))))

  ;;; ── Minimum-size abort ───────────────────────────────────────────────────

  ;; window-split refuses (returns NIL, forks nothing) when the active pane is too
  ;; small to hold two panes plus a separator along the chosen axis.
  (it "split-too-small-aborts-without-forking"
    ;; A 3-wide active pane cannot split left/right (needs >= 2+1+2 = 5).
    (let* ((pane (tl-pane 1 3 24))
           (win  (make-window :id 1 :name "w" :width 3 :height 24
                              :tree (make-layout-leaf pane)
                              :panes (list pane) :active pane)))
      (expect (null (window-split nil win :h)))
      (expect (= 1 (length (window-panes win)))))
    ;; A 2-tall active pane cannot split top/bottom (needs >= 1+1+1 = 3).
    (let* ((pane (tl-pane 1 80 2))
           (win  (make-window :id 1 :name "w" :width 80 :height 2
                              :tree (make-layout-leaf pane)
                              :panes (list pane) :active pane)))
      (expect (null (window-split nil win :v)))
      (expect (= 1 (length (window-panes win))))))

  ;;; ── Removing a pane collapses its parent split ───────────────────────────

  ;; Removing one pane of a 2-pane :h split collapses the split so the sibling
  ;; reclaims the full window rectangle.
  (it "remove-pane-collapses-parent-sibling-takes-over"
    (let* ((l0   (tl-leaf 1 1 1))
           (l1   (tl-leaf 2 1 1))
           (tree (make-layout-split :h l0 l1))
           (win  (tl-window tree 24 81 :active (layout-leaf-pane l0))))
      (destructuring-bind (p0 p1) (window-panes win)
        (declare (ignore p0))
        (let ((survivor (window-remove-pane win (layout-leaf-pane l0))))
          (expect (eq p1 survivor))
          (expect (equal (list p1) (window-panes win)))
          (expect (= 0  (pane-x p1)))
          (expect (= 81 (pane-width p1)))
          (expect (= 24 (pane-height p1)))
          ;; The tree collapsed to a single leaf.
          (expect (cl-tmux/model::layout-leaf-p (window-tree win)))))))

  ;; Removing the sole pane leaves an empty window with a NIL tree.
  (it "remove-last-pane-empties-window"
    (let* ((leaf (tl-leaf 1 80 24))
           (win  (tl-window leaf 24 80 :active (layout-leaf-pane leaf))))
      (expect (null (window-remove-pane win (layout-leaf-pane leaf))))
      (expect (null (window-panes win)))
      (expect (null (window-tree win)))))

  ;; Removing one bottom pane of (top / (bl | br)) keeps top and the surviving
  ;; bottom pane, which expands to fill the bottom row.
  (it "remove-pane-in-nested-tree-keeps-others"
    (let* ((top  (tl-leaf 1 1 1))
           (bl   (tl-leaf 2 1 1))
           (br   (tl-leaf 3 1 1))
           (tree (make-layout-split :v top (make-layout-split :h bl br)))
           (win  (tl-window tree 25 80)))
      (destructuring-bind (ptop pbl pbr) (window-panes win)
        (declare (ignore pbr))
        (window-remove-pane win (layout-leaf-pane br))
        (expect (equal (list ptop pbl) (window-panes win)))
        ;; The surviving bottom pane now spans the full width of the bottom row.
        (expect (= 0  (pane-x pbl)))
        (expect (= 80 (pane-width pbl))))))

  ;;; ── layout-min-extent (pure recursion, no PTY) ─────────────────────────────

  ;; A leaf node requires exactly the minimum extent for its orientation.
  (it "layout-min-extent-leaf"
    (let* ((p    (make-pane :id 1 :fd -1 :pid -1 :width 10 :height 5
                            :screen (make-screen 10 5)))
           (leaf (make-layout-leaf p)))
      ;; For a leaf: min :v extent = +pane-min-height+ (1), :h extent = +pane-min-width+ (2)
      (expect (= cl-tmux/model::+pane-min-height+ (cl-tmux/model::layout-min-extent leaf :v)))
      (expect (= cl-tmux/model::+pane-min-width+  (cl-tmux/model::layout-min-extent leaf :h)))))

  ;; A split along :h adds both children's :h extents plus 1 for the separator.
  (it "layout-min-extent-same-axis-split"
    (let* ((l0 (tl-leaf 1 1 1))
           (l1 (tl-leaf 2 1 1))
           (split (make-layout-split :h l0 l1)))
      ;; min :h extent = pane-min-width + 1 + pane-min-width = 2+1+2 = 5
      (expect (= 5 (cl-tmux/model::layout-min-extent split :h)))
      ;; min :v extent = max(pane-min-height, pane-min-height) = 1
      (expect (= 1 (cl-tmux/model::layout-min-extent split :v)))))

  ;; A split along one axis takes the max of children's extents on the other axis.
  (it "layout-min-extent-cross-axis-split"
    (let* ((l0 (tl-leaf 1 1 1))
           (l1 (tl-leaf 2 1 1))
           (split (make-layout-split :v l0 l1)))
      ;; min :v extent = pane-min-height + 1 + pane-min-height = 1+1+1 = 3
      (expect (= 3 (cl-tmux/model::layout-min-extent split :v)))
      ;; min :h extent = max(pane-min-width, pane-min-width) = 2
      (expect (= 2 (cl-tmux/model::layout-min-extent split :h)))))

  ;;; ── layout-find-leaf direct ─────────────────────────────────────────────────

  ;; layout-find-leaf returns the leaf for a present pane, NIL for an absent one.
  (it "layout-find-leaf-table"
    (let* ((l0   (tl-leaf 1 1 1))
           (l1   (tl-leaf 2 1 1))
           (tree (make-layout-split :h l0 l1))
           (p0   (layout-leaf-pane l0))
           (pabs (make-pane :id 99 :fd -1 :pid -1 :screen (make-screen 1 1))))
      (check-table (list (list (layout-find-leaf tree p0) l0 "present pane -> its leaf node")
                         (list (layout-find-leaf tree pabs) nil "absent pane -> NIL"))
                   :test #'eq))))
