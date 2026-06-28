(in-package #:cl-tmux/test)

;;;; layout tests — part C: layout persistence internals (split-bounding-box,
;;;; node-to-string, read-digits, parse-layout-string, round-trips).

(in-suite layout-tree-suite)

;;; ── Layout persistence: internal helpers ─────────────────────────────────────

(test split-bounding-box-table
  "%split-bounding-box derives correct bounding box for both :h and :v splits."
  (dolist (row '((:h  5  3  40 12   5  3  40 12 ":h split")
                 (:v  0  0  80 21   0  0  80 21 ":v split")))
    (destructuring-bind (orient x y w h exp-x exp-y exp-w exp-h desc) row
      (let* ((l0   (tl-leaf 1 1 1))
             (l1   (tl-leaf 2 1 1))
             (tree (make-layout-split orient l0 l1)))
        (cl-tmux/model::layout-assign tree x y w h)
        (multiple-value-bind (min-x min-y width height)
            (cl-tmux/model::%split-bounding-box tree)
          (is (= exp-x min-x)  "~A: bounding-box x" desc)
          (is (= exp-y min-y)  "~A: bounding-box y" desc)
          (is (= exp-w width)  "~A: bounding-box width" desc)
          (is (= exp-h height) "~A: bounding-box height" desc))))))

(test node-to-string-leaf-and-nil
  "%node->string formats a leaf's WxH,X,Y,id fragment and returns empty string for NIL."
  (let* ((p    (tl-pane 7 20 10))
         (leaf (make-layout-leaf p)))
    (cl-tmux/model::layout-assign leaf 3 5 20 10)
    (let ((s (cl-tmux/model::%node->string leaf)))
      (is (stringp s) "%node->string must return a string")
      (is (search "20x10" s) "fragment must contain WxH")
      (is (search ",3,5," s) "fragment must contain X,Y coordinates")
      (is (search "7" s) "fragment must contain the pane id")))
  (is (string= "" (cl-tmux/model::%node->string nil))
      "%node->string on NIL must return empty string"))

(test read-digits-table
  "%read-digits parses decimal integers at given positions, stopping at end-of-string."
  (dolist (row '(("abc123xyz" 3  123 6 "offset into middle")
                 ("42rest"    0   42 2 "offset at beginning")
                 ("100"       0  100 3 "digits up to end of string")))
    (destructuring-bind (input pos expected-val expected-end desc) row
      (multiple-value-bind (val end)
          (cl-tmux/model::%read-digits input pos)
        (is (= expected-val val) "~A: value" desc)
        (is (= expected-end end) "~A: end pos" desc)))))

(test string-to-layout-round-trips-v-split
  "string->layout decodes a :v split tree encoded by layout->string."
  (let* ((l0  (tl-leaf 1 1 1))
         (l1  (tl-leaf 2 1 1))
         (win (tl-window (make-layout-split :v l0 l1) 21 80))
         (p0  (layout-leaf-pane l0))
         (p1  (layout-leaf-pane l1)))
    (let* ((s    (layout->string win))
           (tree (string->layout s (list p0 p1))))
      (is (not (null tree)) "decoded tree must be non-NIL")
      (is (cl-tmux/model::layout-split-p tree) "decoded node must be a layout-split")
      (is (eq :v (cl-tmux/model::layout-split-orientation tree))
          "decoded split must have :v orientation"))))

(test string-to-layout-round-trips-nested-tree
  "string->layout correctly decodes a nested (top / (bl | br)) tree."
  (let* ((top (tl-leaf 1 1 1))
         (bl  (tl-leaf 2 1 1))
         (br  (tl-leaf 3 1 1))
         (win (tl-window (make-layout-split :v top (make-layout-split :h bl br)) 25 80))
         (ptop (layout-leaf-pane top))
         (pbl  (layout-leaf-pane bl))
         (pbr  (layout-leaf-pane br)))
    (let* ((s    (layout->string win))
           (tree (string->layout s (list ptop pbl pbr))))
      (is (not (null tree)) "decoded nested tree must be non-NIL")
      (is (cl-tmux/model::layout-split-p tree)
          "root of decoded nested tree must be a split")
      (is (eq :v (cl-tmux/model::layout-split-orientation tree))
          "root split must be :v (top-level vertical)")
      ;; The second child must be an :h split (bl|br).
      (is (cl-tmux/model::layout-split-p
           (cl-tmux/model::layout-split-second tree))
          "second child of root must also be a split (the h-split)")
      (is (eq :h (cl-tmux/model::layout-split-orientation
                  (cl-tmux/model::layout-split-second tree)))
          "inner split must be :h"))))

(test string-to-layout-empty-panes-returns-nil
  "string->layout returns NIL when no pane matches the encoded pane id."
  (let* ((p   (tl-pane 42 20 10))
         (win (tl-window (make-layout-leaf p) 10 20 :active p))
         (s   (layout->string win)))
    ;; Pass an empty panes list — no match possible.
    (is (null (string->layout s nil))
        "string->layout with empty panes list must return NIL")))

(test string-to-layout-without-checksum-decodes-correctly
  "string->layout decodes a bare layout string (no checksum prefix)."
  (let* ((p   (tl-pane 5 30 12))
         (win (tl-window (make-layout-leaf p) 12 30 :active p))
         (full-str (layout->string win))
         ;; Strip the checksum+comma prefix manually.
         (bare-str (subseq full-str 5)))
    (let ((tree (string->layout bare-str (list p))))
      (is (not (null tree)) "decoded tree from bare string must be non-NIL")
      (is (cl-tmux/model::layout-leaf-p tree)
          "decoded bare-string leaf must be a layout-leaf")
      (is (eq p (layout-leaf-pane tree))
          "decoded leaf must reference the correct pane"))))

;;; ── layout-find-parent deep tree ─────────────────────────────────────────────

(test layout-find-parent-in-nested-tree-finds-correct-parent
  "layout-find-parent correctly climbs into a nested tree to find the parent."
  ;; Tree: (outer :h (l0) (inner :v (l1) (l2)))
  (let* ((l0    (tl-leaf 1 1 1))
         (l1    (tl-leaf 2 1 1))
         (l2    (tl-leaf 3 1 1))
         (inner (make-layout-split :v l1 l2))
         (outer (make-layout-split :h l0 inner)))
    ;; l1 is :first child of inner (not of outer).
    (multiple-value-bind (p s) (layout-find-parent outer l1)
      (is (eq inner p) "parent of l1 must be inner, not outer")
      (is (eq :first s) "l1 is the :first child of inner"))
    ;; l2 is :second child of inner.
    (multiple-value-bind (p s) (layout-find-parent outer l2)
      (is (eq inner p) "parent of l2 must be inner")
      (is (eq :second s) "l2 is the :second child of inner"))
    ;; inner itself is :second child of outer.
    (multiple-value-bind (p s) (layout-find-parent outer inner)
      (is (eq outer p) "parent of inner must be outer")
      (is (eq :second s) "inner is the :second child of outer"))))

(test layout-find-parent-leaf-node-returns-nil
  "layout-find-parent on a bare leaf (no splits) always returns (NIL NIL)."
  (let* ((leaf   (tl-leaf 1 1 1))
         (target (tl-leaf 2 1 1)))
    (multiple-value-bind (p s) (layout-find-parent leaf target)
      (is (null p) "bare leaf has no parent")
      (is (null s) "side must be NIL when no parent found"))))

;;; ── define-axis-rules / %split-fits-p ────────────────────────────────────────

(test split-fits-p-layout-tree-suite
  "%split-fits-p: T when room exists to split on the axis, NIL when too small.
   min-width=2 → h needs ≥5 cols; min-height=1 → v needs ≥3 rows."
  (dolist (row '((t   5 24 :h "5-col pane fits h-split (needs 5)")
                 (nil 4 24 :h "4-col pane too narrow for h-split (needs 5)")
                 (t  80  3 :v "3-row pane fits v-split (needs 3)")
                 (nil 80  2 :v "2-row pane too short for v-split (needs 3)")))
    (destructuring-bind (expected width height orient desc) row
      (let ((pane (make-pane :id 1 :fd -1 :pid -1 :width width :height height
                             :screen (make-screen width height))))
        (is (eq expected (cl-tmux/model::%split-fits-p pane orient)) "~A" desc)))))

;;; ── Table-driven: %layout-checksum known-value tests ─────────────────────────

(test layout-checksum-known-values-match-expected
  "%layout-checksum produces the correct tmux-compatible 4-hex checksum."
  (dolist (c (list (list ""           "0000" "empty string checksum must be 0000")
                   (list "a"         "0061" "single-char 'a' (97 decimal = 0x61)")
                   (let ((s "1x1,0,0,1"))
                     (list s (cl-tmux/model::%layout-checksum s) "checksum must be deterministic"))))
    (destructuring-bind (input expected desc) c
      (is (string= expected (cl-tmux/model::%layout-checksum input)) "~A" desc))))

; resize-direction-orientation-all-directions-table removed.
; The identical 4-case mapping is already tested in layout-geometry-tests.lisp
; as resize-direction-orientation-mapping; the duplicate was removed.

;;; ── main-horizontal / main-vertical honour main-pane size ────────────────────
;;;
;;; apply-named-layout takes the main pane's size (tmux's main-pane-width /
;;; main-pane-height options, read by the cl-tmux-layer caller).  The main (first)
;;; pane is sized to exactly that many cells along the outer axis; the rest share
;;; the remainder.

;;; %three-pane-window is defined in tests/helpers-b.lisp.

(test main-layout-honours-main-pane-size
  "apply-named-layout :main-vertical sizes the main pane to main-pane-width;
   :main-horizontal sizes the main pane to main-pane-height."
  (let ((win (%three-pane-window 100 30)))
    (cl-tmux/model:apply-named-layout win :main-vertical 60 24)
    (let ((p0 (first (window-panes win))))
      (is (= 60 (pane-width p0)) "main pane is main-pane-width (60) columns wide")
      (is (< (pane-width (second (window-panes win))) 60)
          "secondary panes share the narrower remainder")))
  (let ((win (%three-pane-window 100 40)))
    (cl-tmux/model:apply-named-layout win :main-horizontal 80 15)
    (let ((p0 (first (window-panes win)))
          (p1 (second (window-panes win))))
      (is (= 15 (pane-height p0)) "main pane is main-pane-height (15) rows tall")
      (is (> (pane-y p1) 15)
          "secondary panes sit below the main pane (in the remaining rows)"))))

(test main-layout-default-main-pane-size-is-tmux-default
  "Without explicit sizes, main-vertical defaults the main pane to 80 columns
   (tmux's default), not a half-split."
  (let ((win (make-window :id 1 :name "w" :width 200 :height 50
                          :panes (list (make-no-pty-pane 1 0 0 200 50)
                                       (make-no-pty-pane 2 0 0 200 50)))))
    (cl-tmux/model:apply-named-layout win :main-vertical)
    (is (= 80 (pane-width (first (window-panes win))))
        "default main-pane-width is 80, not w/2")))

;;; ── other-pane-width / -height override main-pane-* when set ─────────────────
;;;
;;; A non-zero other-pane size (that leaves room for the main pane) sizes the
;;; OTHER region and gives the main pane the rest — tmux layout_set_main_h/_v.
;;; When it does not fit, main-pane-* wins.

(test main-layout-other-pane-size-overrides-main
  ":main-vertical / :main-horizontal with a fitting other-pane size sizes the
   other panes to it and gives the main pane the remaining dimension."
  ;; :main-vertical: available=119; other-pane-width 30 fits (89 >= main 80) → main 89.
  (let ((win (%three-pane-window 120 30)))
    (cl-tmux/model:apply-named-layout win :main-vertical 80 24 30 0)
    (is (= 89 (pane-width (first (window-panes win))))
        ":main-vertical main pane width = available - other-pane-width")
    (is (= 30 (pane-width (second (window-panes win))))
        ":main-vertical other panes get other-pane-width (30)"))
  ;; :main-horizontal: available=49; other-pane-height 20 fits (29 >= main 24) → main 29.
  (let ((win (%three-pane-window 100 50)))
    (cl-tmux/model:apply-named-layout win :main-horizontal 80 24 0 20)
    (is (= 29 (pane-height (first (window-panes win))))
        ":main-horizontal main pane height = available - other-pane-height")
    (is (= 20 (pane-height (second (window-panes win))))
        ":main-horizontal other panes get other-pane-height (20)")))

(test main-vertical-other-pane-width-too-big-falls-back-to-main
  "An other-pane-width that does not leave room for the main pane is ignored;
   main-pane-width applies instead."
  (let ((win (%three-pane-window 120 30)))
    (cl-tmux/model:apply-named-layout win :main-vertical 80 24 200 0)
    (is (= 80 (pane-width (first (window-panes win))))
        "over-large other-pane-width is ignored → main-pane-width (80) used")))
