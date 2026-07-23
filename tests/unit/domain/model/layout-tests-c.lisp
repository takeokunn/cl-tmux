(in-package #:cl-tmux/test)

;;;; layout tests — part C: layout persistence internals (split-bounding-box,
;;;; node-to-string) and layout-find-parent deep-tree traversal.

(describe "layout-tree-suite"

  ;;; ── Layout persistence: internal helpers ─────────────────────────────────────

  ;; layout-node-bounding-box derives correct bounding box for both :h and :v splits.
  (it "split-bounding-box-table"
    (dolist (row '((:h  5  3  40 12   5  3  40 12 ":h split")
                   (:v  0  0  80 21   0  0  80 21 ":v split")))
      (destructuring-bind (orient x y w h exp-x exp-y exp-w exp-h desc) row
        (declare (ignore desc))
        (let* ((l0   (tl-leaf 1 1 1))
               (l1   (tl-leaf 2 1 1))
               (tree (make-layout-split orient l0 l1)))
          (cl-tmux/model::layout-assign tree x y w h)
          (multiple-value-bind (min-x min-y width height)
              (layout-node-bounding-box tree)
            (expect (= exp-x min-x))
            (expect (= exp-y min-y))
            (expect (= exp-w width))
            (expect (= exp-h height)))))))

  ;; layout-node-bounding-box on an inner split must aggregate the min/max of ITS OWN
  ;; leaves, not merely echo back the outer layout-assign() rectangle passed at
  ;; the root. Tree: (outer :h (l0) (inner :v (l1) (l2))), assigned at 0,0,81,21.
  ;; l0 occupies the left 40 cols; inner (l1 over l2) occupies the right 40 cols
  ;; starting at x=41 — so inner's own bounding box (x=41, w=40) genuinely differs
  ;; from the outer rectangle (x=0, w=81) that was passed to layout-assign.
  (it "split-bounding-box-aggregates-nested-subtree-not-top-level-assign"
    (let* ((l0    (tl-leaf 1 1 1))
           (l1    (tl-leaf 2 1 1))
           (l2    (tl-leaf 3 1 1))
           (inner (make-layout-split :v l1 l2))
           (outer (make-layout-split :h l0 inner)))
      (cl-tmux/model::layout-assign outer 0 0 81 21)
      ;; Sanity: outer's own bounding box does equal the top-level assign rectangle.
      (multiple-value-bind (ox oy ow oh) (layout-node-bounding-box outer)
        (expect (= 0  ox))
        (expect (= 0  oy))
        (expect (= 81 ow))
        (expect (= 21 oh)))
      ;; The real assertion: inner's bounding box is computed from its own two
      ;; leaves (l1, l2), which occupy only the right half of the outer rectangle —
      ;; a bug in min/max aggregation across children would not shrink this to
      ;; match l1/l2's actual x/width and would instead leak the outer values.
      (multiple-value-bind (ix iy iw ih) (layout-node-bounding-box inner)
        (expect (= 41 ix))
        (expect (= 0  iy))
        (expect (= 40 iw))
        (expect (= 21 ih)))))

  ;; %node->string formats a leaf's WxH,X,Y,id fragment and returns empty string for NIL.
  (it "node-to-string-leaf-and-nil"
    (let* ((p    (tl-pane 7 20 10))
           (leaf (make-layout-leaf p)))
      (cl-tmux/model::layout-assign leaf 3 5 20 10)
      (let ((s (cl-tmux/model::%node->string leaf)))
        (expect (stringp s))
        (expect (search "20x10" s))
        (expect (search ",3,5," s))
        (expect (search "7" s))))
    (expect (string= "" (cl-tmux/model::%node->string nil))))

  ;;; ── layout-find-parent deep tree ─────────────────────────────────────────────

  ;; layout-find-parent correctly climbs into a nested tree to find the parent.
  (it "layout-find-parent-in-nested-tree-finds-correct-parent"
    ;; Tree: (outer :h (l0) (inner :v (l1) (l2)))
    (let* ((l0    (tl-leaf 1 1 1))
           (l1    (tl-leaf 2 1 1))
           (l2    (tl-leaf 3 1 1))
           (inner (make-layout-split :v l1 l2))
           (outer (make-layout-split :h l0 inner)))
      ;; l1 is :first child of inner (not of outer).
      (multiple-value-bind (p s) (layout-find-parent outer l1)
        (expect (eq inner p))
        (expect (eq :first s)))
      ;; l2 is :second child of inner.
      (multiple-value-bind (p s) (layout-find-parent outer l2)
        (expect (eq inner p))
        (expect (eq :second s)))
      ;; inner itself is :second child of outer.
      (multiple-value-bind (p s) (layout-find-parent outer inner)
        (expect (eq outer p))
        (expect (eq :second s)))))

  ;; layout-find-parent on a bare leaf (no splits) always returns (NIL NIL).
  (it "layout-find-parent-leaf-node-returns-nil"
    (let* ((leaf   (tl-leaf 1 1 1))
           (target (tl-leaf 2 1 1)))
      (multiple-value-bind (p s) (layout-find-parent leaf target)
        (expect (null p))
        (expect (null s)))))

  ;;; ── define-axis-rules / %split-fits-p ────────────────────────────────────────

  ;; %split-fits-p: T when room exists to split on the axis, NIL when too small.
  ;; min-width=2 → h needs ≥5 cols; min-height=1 → v needs ≥3 rows.
  (it "split-fits-p-layout-tree-suite"
    (dolist (row '((t   5 24 :h "5-col pane fits h-split (needs 5)")
                   (nil 4 24 :h "4-col pane too narrow for h-split (needs 5)")
                   (t  80  3 :v "3-row pane fits v-split (needs 3)")
                   (nil 80  2 :v "2-row pane too short for v-split (needs 3)")))
      (destructuring-bind (expected width height orient desc) row
        (declare (ignore desc))
        (let ((pane (make-pane :id 1 :fd -1 :pid -1 :width width :height height
                               :screen (make-screen width height))))
          (expect (eq expected (cl-tmux/model::%split-fits-p pane orient)))))))

  ;;; ── Table-driven: %layout-checksum known-value tests ─────────────────────────

  ;; %layout-checksum produces the canonical 4-hex layout checksum.
  (it "layout-checksum-known-values-match-expected"
    (dolist (c (list (list ""           "0000" "empty string checksum must be 0000")
                     (list "a"         "0061" "single-char 'a' (97 decimal = 0x61)")
                     (let ((s "1x1,0,0,1"))
                       (list s (cl-tmux/model::%layout-checksum s) "checksum must be deterministic"))))
      (destructuring-bind (input expected desc) c
        (declare (ignore desc))
        (expect (string= expected (cl-tmux/model::%layout-checksum input))))))

  ; resize-direction-orientation-all-directions-table removed.
  ; The identical 4-case mapping is already tested in layout-geometry-tests.lisp
  ; as resize-direction-orientation-mapping; the duplicate was removed.

  ;;; ── main-horizontal / main-vertical honour main-pane size ────────────────────
  ;;;
  ;;; apply-named-layout takes the main pane's size (tmux's main-pane-width /
  ;;; main-pane-height options, read by the cl-tmux-layer caller).  The main (first)
  ;;; pane is sized to exactly that many cells along the outer axis; the rest share
  ;;; the remainder.

  ;;; %three-pane-window is defined in tests/helpers-layout-fixtures.lisp.

  ;; apply-named-layout :main-vertical sizes the main pane to main-pane-width;
  ;; :main-horizontal sizes the main pane to main-pane-height.
  (it "main-layout-honours-main-pane-size"
    (let ((win (%three-pane-window 100 30)))
      (cl-tmux/model:apply-named-layout win :main-vertical 60 24)
      (let ((p0 (first (window-panes win))))
        (expect (= 60 (pane-width p0)))
        (expect (< (pane-width (second (window-panes win))) 60))))
    (let ((win (%three-pane-window 100 40)))
      (cl-tmux/model:apply-named-layout win :main-horizontal 80 15)
      (let ((p0 (first (window-panes win)))
            (p1 (second (window-panes win))))
        (expect (= 15 (pane-height p0)))
        (expect (> (pane-y p1) 15)))))

  ;; Without explicit sizes, main-vertical defaults the main pane to 80 columns
  ;; (tmux's default), not a half-split.
  (it "main-layout-default-main-pane-size-is-tmux-default"
    (let ((win (make-window :id 1 :name "w" :width 200 :height 50
                            :panes (list (make-no-pty-pane 1 0 0 200 50)
                                         (make-no-pty-pane 2 0 0 200 50)))))
      (cl-tmux/model:apply-named-layout win :main-vertical)
      (expect (= 80 (pane-width (first (window-panes win)))))))

  ;;; ── other-pane-width / -height override main-pane-* when set ─────────────────
  ;;;
  ;;; A non-zero other-pane size (that leaves room for the main pane) sizes the
  ;;; OTHER region and gives the main pane the rest — tmux layout_set_main_h/_v.
  ;;; When it does not fit, main-pane-* wins.

  ;; :main-vertical / :main-horizontal with a fitting other-pane size sizes the
  ;; other panes to it and gives the main pane the remaining dimension.
  (it "main-layout-other-pane-size-overrides-main"
    ;; :main-vertical: available=119; other-pane-width 30 fits (89 >= main 80) → main 89.
    (let ((win (%three-pane-window 120 30)))
      (cl-tmux/model:apply-named-layout win :main-vertical 80 24 30 0)
      (expect (= 89 (pane-width (first (window-panes win)))))
      (expect (= 30 (pane-width (second (window-panes win))))))
    ;; :main-horizontal: available=49; other-pane-height 20 fits (29 >= main 24) → main 29.
    (let ((win (%three-pane-window 100 50)))
      (cl-tmux/model:apply-named-layout win :main-horizontal 80 24 0 20)
      (expect (= 29 (pane-height (first (window-panes win)))))
      (expect (= 20 (pane-height (second (window-panes win)))))))

  ;; An other-pane-width that does not leave room for the main pane is ignored;
  ;; main-pane-width applies instead.
  (it "main-vertical-other-pane-width-too-big-falls-back-to-main"
    (let ((win (%three-pane-window 120 30)))
      (cl-tmux/model:apply-named-layout win :main-vertical 80 24 200 0)
      (expect (= 80 (pane-width (first (window-panes win))))))))
