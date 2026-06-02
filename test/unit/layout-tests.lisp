(in-package #:cl-tmux/test)

;;;; Layout tree tests — binary split-tree layout invariants and operations.

;;; ────────────────────────────────────────────────────────────────────────────
;;; SUITE: layout-tree-suite (binary split-tree layout)
;;; ────────────────────────────────────────────────────────────────────────────

(def-suite layout-tree-suite :description "Binary split-tree layout")
(in-suite layout-tree-suite)

;;; ── Helpers ──────────────────────────────────────────────────────────────
;;; tl-pane, tl-leaf, tl-window are defined in test/helpers.lisp and available
;;; to all test files in the suite — no local redefinitions needed here.

;;; ── layout-leaves / find helpers ─────────────────────────────────────────

(test layout-leaves-collects-in-order
  "layout-leaves returns every pane left/top-to-right/bottom."
  (let* ((l0 (tl-leaf 1 10 10))
         (l1 (tl-leaf 2 10 10))
         (l2 (tl-leaf 3 10 10))
         ;; (a | (b / c))
         (tree (make-layout-split :h l0 (make-layout-split :v l1 l2))))
    (is (equal (list (layout-leaf-pane l0)
                     (layout-leaf-pane l1)
                     (layout-leaf-pane l2))
               (layout-leaves tree)))))

(test layout-find-parent-resolves-side
  "layout-find-parent locates the parent split and the child's side."
  (let* ((l0 (tl-leaf 1 10 10))
         (l1 (tl-leaf 2 10 10))
         (split (make-layout-split :h l0 l1)))
    (multiple-value-bind (p w) (layout-find-parent split l1)
      (is (eq split p))
      (is (eq :second w)))))

;;; ── Split halves only the active pane ────────────────────────────────────

(test relayout-h-split-divides-only-into-two
  "A single :h split fills the window: left + separator + right covers cols."
  (let* ((tree (make-layout-split :h (tl-leaf 1 1 1) (tl-leaf 2 1 1)))
         (win  (tl-window tree 24 81)))
    (destructuring-bind (p0 p1) (window-panes win)
      ;; 81 cols - 1 separator = 80 split 50/50 = 40/40.
      (is (= 0  (pane-x p0)))
      (is (= 40 (pane-width p0)))
      (is (= 41 (pane-x p1)) "right pane sits one column past the separator")
      (is (= 40 (pane-width p1)))
      (is (= 24 (pane-height p0)) "full height on a left/right split")
      (is (= 24 (pane-height p1)))
      ;; Exactly one separator column between them.
      (is (= 1 (- (pane-x p1) (+ (pane-x p0) (pane-width p0))))))))

(test nested-mixed-layout-geometry
  "A pane split top/bottom, its bottom half then split left/right, yields three
   non-overlapping panes covering the window (no gaps/overlaps)."
  (let* ((top    (tl-leaf 1 1 1))
         (bl     (tl-leaf 2 1 1))
         (br     (tl-leaf 3 1 1))
         ;; (top / (bl | br))
         (tree   (make-layout-split :v top (make-layout-split :h bl br)))
         (win    (tl-window tree 25 80)))
    (destructuring-bind (ptop pbl pbr) (window-panes win)
      ;; Vertical (:v) split: 25 rows - 1 separator = 24, 12 top / 12 bottom.
      (is (= 0  (pane-y ptop)))
      (is (= 12 (pane-height ptop)))
      (is (= 80 (pane-width  ptop)) "top spans full width")
      ;; Bottom row starts at y = 12 + 1 separator = 13, height 12.
      (is (= 13 (pane-y pbl)))
      (is (= 13 (pane-y pbr)))
      (is (= 12 (pane-height pbl)))
      (is (= 12 (pane-height pbr)))
      ;; Bottom is split left|right: 80 - 1 separator = 79, 40 left / 39 right
      ;; (banker's rounding of 39.5 → 40 for the first child).
      (is (= 0  (pane-x pbl)))
      (is (= 40 (pane-width pbl)))
      (is (= 41 (pane-x pbr)))
      (is (= 39 (pane-width pbr)))
      ;; No overlap between the two bottom panes.
      (is (<= (+ (pane-x pbl) (pane-width pbl)) (pane-x pbr))))))

;;; ── Resize on each axis (tree) ───────────────────────────────────────────

(test resize-h-right-moves-border-and-reflows
  "On a left|right split, :right grows the active (left) pane and shrinks the
   neighbour; the neighbour slides to stay one column past the new border."
  (let* ((l0   (tl-leaf 1 1 1))
         (l1   (tl-leaf 2 1 1))
         (tree (make-layout-split :h l0 l1))
         (win  (tl-window tree 24 81 :active (layout-leaf-pane l0))))
    (destructuring-bind (p0 p1) (window-panes win)
      (is (= 40 (pane-width p0)))
      (is (eq p0 (window-resize-active win :right 5)))
      (is (= 45 (pane-width p0)) "active pane grows by 5")
      (is (= 35 (pane-width p1)) "neighbour shrinks by 5")
      (is (= 46 (pane-x p1)) "neighbour x = active.x + active.width + 1"))))

(test resize-v-down-moves-border
  "On a top/bottom split, :down grows the active (top) pane and shrinks the
   lower neighbour — i.e. resize works on the vertical axis too (not gated on a
   single global orientation)."
  (let* ((l0   (tl-leaf 1 1 1))
         (l1   (tl-leaf 2 1 1))
         (tree (make-layout-split :v l0 l1))
         (win  (tl-window tree 25 80 :active (layout-leaf-pane l0))))
    (destructuring-bind (p0 p1) (window-panes win)
      (is (= 12 (pane-height p0)))
      (is (eq p0 (window-resize-active win :down 3)))
      (is (= 15 (pane-height p0)) "top pane grows by 3")
      (is (= 9  (pane-height p1)) "lower pane shrinks by 3")
      (is (= 16 (pane-y p1)) "lower pane y = top.y + top.height + 1"))))

(test resize-orthogonal-axis-finds-ancestor
  "In a nested layout the active pane can be resized along BOTH axes: the left
   pane of (left | (top/bottom)) resizes :right against the outer :h split."
  (let* ((left  (tl-leaf 1 1 1))
         (rt    (tl-leaf 2 1 1))
         (rb    (tl-leaf 3 1 1))
         (tree  (make-layout-split :h left (make-layout-split :v rt rb)))
         (win   (tl-window tree 25 81 :active (layout-leaf-pane left))))
    (destructuring-bind (pl prt prb) (window-panes win)
      (let ((w-before (pane-width pl)))
        (is (eq pl (window-resize-active win :right 4)))
        (is (= (+ w-before 4) (pane-width pl)) "left pane grows along the :h axis")
        ;; Both right-column panes shrink and stay vertically stacked.
        (is (= (pane-x prt) (pane-x prb)) "right column stays a single column")
        (is (< (pane-y prt) (pane-y prb)) "right column stays top/bottom")))))

(test resize-no-neighbour-is-noop
  "A direction with no neighbour returns NIL and changes nothing."
  (let* ((l0   (tl-leaf 1 1 1))
         (l1   (tl-leaf 2 1 1))
         (tree (make-layout-split :h l0 l1))
         (win  (tl-window tree 24 81 :active (layout-leaf-pane l0))))
    (let ((w0 (pane-width (first (window-panes win)))))
      ;; A pure left|right split has no top/bottom border to move.
      (is (null (window-resize-active win :up 5)) "no :v ancestor → NIL")
      (is (= w0 (pane-width (first (window-panes win)))) "geometry untouched"))))

;;; ── Minimum-size abort ───────────────────────────────────────────────────

(test split-too-small-aborts-without-forking
  "window-split refuses (returns NIL, forks nothing) when the active pane is too
   small to hold two panes plus a separator along the chosen axis."
  ;; A 3-wide active pane cannot split left/right (needs >= 2+1+2 = 5).
  (let* ((pane (tl-pane 1 3 24))
         (win  (make-window :id 1 :name "w" :width 3 :height 24
                            :tree (make-layout-leaf pane)
                            :panes (list pane) :active pane)))
    (is (null (window-split win :h)) "too-narrow split is refused")
    (is (= 1 (length (window-panes win))) "no pane was added"))
  ;; A 2-tall active pane cannot split top/bottom (needs >= 1+1+1 = 3).
  (let* ((pane (tl-pane 1 80 2))
         (win  (make-window :id 1 :name "w" :width 80 :height 2
                            :tree (make-layout-leaf pane)
                            :panes (list pane) :active pane)))
    (is (null (window-split win :v)) "too-short split is refused")
    (is (= 1 (length (window-panes win))) "no pane was added")))

;;; ── Removing a pane collapses its parent split ───────────────────────────

(test remove-pane-collapses-parent-sibling-takes-over
  "Removing one pane of a 2-pane :h split collapses the split so the sibling
   reclaims the full window rectangle."
  (let* ((l0   (tl-leaf 1 1 1))
         (l1   (tl-leaf 2 1 1))
         (tree (make-layout-split :h l0 l1))
         (win  (tl-window tree 24 81 :active (layout-leaf-pane l0))))
    (destructuring-bind (p0 p1) (window-panes win)
      (declare (ignore p0))
      (let ((survivor (window-remove-pane win (layout-leaf-pane l0))))
        (is (eq p1 survivor) "the sibling survives")
        (is (equal (list p1) (window-panes win)) "only the sibling remains")
        (is (= 0  (pane-x p1)) "sibling reclaims the full rectangle: x")
        (is (= 81 (pane-width p1)) "sibling reclaims the full rectangle: width")
        (is (= 24 (pane-height p1)))
        ;; The tree collapsed to a single leaf.
        (is (cl-tmux/model::layout-leaf-p (window-tree win))
            "tree collapses to a lone leaf after the split is removed")))))

(test remove-last-pane-empties-window
  "Removing the sole pane leaves an empty window with a NIL tree."
  (let* ((leaf (tl-leaf 1 80 24))
         (win  (tl-window leaf 24 80 :active (layout-leaf-pane leaf))))
    (is (null (window-remove-pane win (layout-leaf-pane leaf))))
    (is (null (window-panes win)) "no panes remain")
    (is (null (window-tree win)) "tree is cleared")))

(test remove-pane-in-nested-tree-keeps-others
  "Removing one bottom pane of (top / (bl | br)) keeps top and the surviving
   bottom pane, which expands to fill the bottom row."
  (let* ((top  (tl-leaf 1 1 1))
         (bl   (tl-leaf 2 1 1))
         (br   (tl-leaf 3 1 1))
         (tree (make-layout-split :v top (make-layout-split :h bl br)))
         (win  (tl-window tree 25 80)))
    (destructuring-bind (ptop pbl pbr) (window-panes win)
      (declare (ignore pbr))
      (window-remove-pane win (layout-leaf-pane br))
      (is (equal (list ptop pbl) (window-panes win))
          "top and the surviving bottom pane remain")
      ;; The surviving bottom pane now spans the full width of the bottom row.
      (is (= 0  (pane-x pbl)))
      (is (= 80 (pane-width pbl)) "survivor reclaims the full bottom width"))))

;;; ── layout-min-extent (pure recursion, no PTY) ─────────────────────────────

(test layout-min-extent-leaf
  "A leaf node requires exactly the minimum extent for its orientation."
  (let* ((p    (make-pane :id 1 :fd -1 :pid -1 :width 10 :height 5
                          :screen (make-screen 10 5)))
         (leaf (make-layout-leaf p)))
    ;; For a leaf: min :v extent = +pane-min-height+ (1), :h extent = +pane-min-width+ (2)
    (is (= cl-tmux/model::+pane-min-height+ (cl-tmux/model::layout-min-extent leaf :v)))
    (is (= cl-tmux/model::+pane-min-width+  (cl-tmux/model::layout-min-extent leaf :h)))))

(test layout-min-extent-same-axis-split
  "A split along :h adds both children's :h extents plus 1 for the separator."
  (let* ((l0 (tl-leaf 1 1 1))
         (l1 (tl-leaf 2 1 1))
         (split (make-layout-split :h l0 l1)))
    ;; min :h extent = pane-min-width + 1 + pane-min-width = 2+1+2 = 5
    (is (= 5 (cl-tmux/model::layout-min-extent split :h)))
    ;; min :v extent = max(pane-min-height, pane-min-height) = 1
    (is (= 1 (cl-tmux/model::layout-min-extent split :v)))))

(test layout-min-extent-cross-axis-split
  "A split along one axis takes the max of children's extents on the other axis."
  (let* ((l0 (tl-leaf 1 1 1))
         (l1 (tl-leaf 2 1 1))
         (split (make-layout-split :v l0 l1)))
    ;; min :v extent = pane-min-height + 1 + pane-min-height = 1+1+1 = 3
    (is (= 3 (cl-tmux/model::layout-min-extent split :v)))
    ;; min :h extent = max(pane-min-width, pane-min-width) = 2
    (is (= 2 (cl-tmux/model::layout-min-extent split :h)))))

;;; ── layout-find-leaf direct ─────────────────────────────────────────────────

(test layout-find-leaf-finds-existing-pane
  "layout-find-leaf returns the leaf node wrapping the given pane."
  (let* ((l0 (tl-leaf 1 1 1))
         (l1 (tl-leaf 2 1 1))
         (tree (make-layout-split :h l0 l1))
         (p0  (layout-leaf-pane l0)))
    (is (eq l0 (layout-find-leaf tree p0))
        "must return the leaf that holds p0")))

(test layout-find-leaf-returns-nil-for-absent-pane
  "layout-find-leaf returns NIL when the pane is not in the tree."
  (let* ((l0 (tl-leaf 1 1 1))
         (l1 (tl-leaf 2 1 1))
         (tree (make-layout-split :h l0 l1))
         (other (make-pane :id 99 :fd -1 :pid -1 :screen (make-screen 1 1))))
    (is (null (layout-find-leaf tree other))
        "absent pane must return NIL")))

;;; ── layout-leaves and define-layout-visitor edge cases ──────────────────────

(test layout-leaves-nil-tree-returns-empty-list
  "layout-leaves on a NIL node (empty window tree) returns NIL."
  (is (null (layout-leaves nil))
      "layout-leaves NIL must return NIL"))

(test layout-leaves-single-leaf-returns-one-pane
  "layout-leaves on a bare leaf returns a list of exactly that leaf's pane."
  (let* ((leaf (tl-leaf 1 10 5))
         (pane (layout-leaf-pane leaf)))
    (is (equal (list pane) (layout-leaves leaf))
        "single leaf must yield (list pane)")))

;; axis-floor is tested in layout-geometry-tests.lisp (axis-floor-returns-correct-minimum).

(test direct-child-side-identifies-first-and-second
  "%direct-child-side returns (split :first) or (split :second) for direct children
   and (NIL NIL) for unrelated nodes."
  (let* ((l0    (tl-leaf 1 1 1))
         (l1    (tl-leaf 2 1 1))
         (split (make-layout-split :h l0 l1)))
    (multiple-value-bind (p s) (cl-tmux/model::%direct-child-side split l0)
      (is (eq split p) "parent must be the split")
      (is (eq :first s) "first child → :first"))
    (multiple-value-bind (p s) (cl-tmux/model::%direct-child-side split l1)
      (is (eq split p))
      (is (eq :second s) "second child → :second"))
    (multiple-value-bind (p s) (cl-tmux/model::%direct-child-side split (tl-leaf 99 1 1))
      (is (null p) "non-child → NIL parent")
      (is (null s) "non-child → NIL side"))))

(test layout-find-parent-returns-nil-for-nil-node
  "layout-find-parent on a NIL node returns (values NIL NIL)."
  (multiple-value-bind (p s) (layout-find-parent nil (tl-leaf 1 1 1))
    (is (null p) "nil node must return nil parent")
    (is (null s) "nil node must return nil side")))

(test define-layout-visitor-macro-generates-correct-visitor
  "define-layout-visitor generates a function with the declared null/leaf/split clauses.
   The layout-leaves function (generated by the macro) covers all three cases."
  ;; Nil case
  (is (null (layout-leaves nil)) "nil → empty list")
  ;; Leaf case
  (let ((leaf (tl-leaf 42 1 1)))
    (is (equal (list (layout-leaf-pane leaf)) (layout-leaves leaf))
        "leaf → (list pane)"))
  ;; Split case — verify both subtrees are collected
  (let* ((l0   (tl-leaf 1 1 1))
         (l1   (tl-leaf 2 1 1))
         (tree (make-layout-split :h l0 l1)))
    (is (= 2 (length (layout-leaves tree)))
        "split → both children collected")))

(test define-layout-fold-macro-is-defined
  "define-layout-fold generates multi-argument recursive tree functions.
   Tested via layout-find-leaf and layout-min-extent, both generated by the macro."
  ;; layout-find-leaf: fold with extra pane arg
  (let* ((l0   (tl-leaf 1 1 1))
         (l1   (tl-leaf 2 1 1))
         (tree (make-layout-split :h l0 l1))
         (p0   (layout-leaf-pane l0)))
    (is (eq l0 (layout-find-leaf tree p0))  "fold finds pane in subtree")
    (is (null (layout-find-leaf tree (make-pane :id 99 :fd -1 :pid -1
                                                :screen (make-screen 1 1))))
        "fold returns NIL for pane not in tree"))
  ;; layout-min-extent: fold with extra orient arg — nil case returns 0
  (is (= 0 (cl-tmux/model::layout-min-extent nil :h))
      "nil node returns 0 extent")
  (is (= 0 (cl-tmux/model::layout-min-extent nil :v))
      "nil node returns 0 extent for :v"))

;;; ── Layout persistence: layout->string / string->layout ─────────────────────
;;;
;;; These tests cover the encode/decode round-trip, the checksum computation,
;;; the %skip-checksum helper, and the %build-flat-tree constructor.

(test layout-to-string-single-leaf
  "layout->string on a single-pane window returns a checksum,WxH,X,Y,pane-id string."
  (let* ((p    (tl-pane 7 20 10))
         (win  (tl-window (make-layout-leaf p) 10 20 :active p)))
    (let ((s (layout->string win)))
      (is (stringp s) "layout->string must return a string")
      ;; Format: 4-hex-checksum , WxH,X,Y,pane-id
      (is (>= (length s) 5) "string must be long enough to include checksum")
      (is (char= #\, (char s 4)) "checksum must be followed by a comma")
      (is (search "20x10" s) "string must contain width×height")
      (is (search "7" s) "string must contain the pane id"))))

(test layout-to-string-nil-tree-returns-nil
  "layout->string on a window with no tree returns NIL."
  (let ((win (make-window :id 1 :name "w" :width 80 :height 24 :tree nil)))
    (is (null (layout->string win))
        "layout->string on nil tree must return NIL")))

(test layout-to-string-h-split-uses-braces
  "layout->string serializes an :h split using {child1,child2} notation."
  (let* ((l0  (tl-leaf 1 1 1))
         (l1  (tl-leaf 2 1 1))
         (win (tl-window (make-layout-split :h l0 l1) 24 81)))
    (let ((s (layout->string win)))
      (is (find #\{ (coerce s 'list))
          "H-split serialization must use '{' brackets")
      (is (find #\} (coerce s 'list))
          "H-split serialization must use '}' brackets"))))

(test layout-to-string-v-split-uses-brackets
  "layout->string serializes a :v split using [child1,child2] notation."
  (let* ((l0  (tl-leaf 1 1 1))
         (l1  (tl-leaf 2 1 1))
         (win (tl-window (make-layout-split :v l0 l1) 24 80)))
    (let ((s (layout->string win)))
      (is (find #\[ (coerce s 'list))
          "V-split serialization must use '[' brackets")
      (is (find #\] (coerce s 'list))
          "V-split serialization must use ']' brackets"))))

(test string-to-layout-round-trips-single-leaf
  "string->layout decodes a layout->string-encoded string back to an equivalent tree."
  (let* ((p   (tl-pane 3 40 20))
         (win (tl-window (make-layout-leaf p) 20 40 :active p)))
    (let* ((s    (layout->string win))
           (tree (string->layout s (list p))))
      (is (not (null tree)) "string->layout must return a non-NIL tree")
      (is (cl-tmux/model::layout-leaf-p tree) "decoded single-leaf must be a layout-leaf")
      (is (eq p (layout-leaf-pane tree))
          "decoded leaf must reference the same pane object"))))

(test string-to-layout-round-trips-h-split
  "string->layout decodes an H-split tree encoded by layout->string."
  (let* ((l0  (tl-leaf 1 1 1))
         (l1  (tl-leaf 2 1 1))
         (win (tl-window (make-layout-split :h l0 l1) 24 81))
         (p0  (layout-leaf-pane l0))
         (p1  (layout-leaf-pane l1)))
    (let* ((s    (layout->string win))
           (tree (string->layout s (list p0 p1))))
      (is (not (null tree)) "decoded tree must be non-NIL")
      (is (cl-tmux/model::layout-split-p tree) "decoded node must be a layout-split")
      (is (eq :h (cl-tmux/model::layout-split-orientation tree))
          "decoded split must have :h orientation"))))

(test string-to-layout-nil-on-garbage
  "string->layout returns NIL for a garbage string."
  (is (null (string->layout "not-a-layout" nil))
      "string->layout must return NIL for unrecognizable input"))

(test layout-checksum-is-reproducible
  "%layout-checksum returns the same 4-char hex string for the same input."
  (let ((s "%layout-checksum determinism check"))
    (is (string= (cl-tmux/model::%layout-checksum s)
                 (cl-tmux/model::%layout-checksum s))
        "%layout-checksum must be deterministic")
    (is (= 4 (length (cl-tmux/model::%layout-checksum s)))
        "checksum must always be exactly 4 hex digits")))

(test layout-checksum-empty-string
  "%layout-checksum on the empty string returns a 4-digit hex string."
  (let ((cs (cl-tmux/model::%layout-checksum "")))
    (is (= 4 (length cs)) "empty string checksum must be 4 chars")
    (is (every (lambda (c) (digit-char-p c 16)) cs)
        "checksum must consist of hex digits")))

(test skip-checksum-strips-leading-checksum
  "%skip-checksum removes a well-formed 4-hex-digit comma prefix."
  (is (string= "rest" (cl-tmux/model::%skip-checksum "ABCD,rest"))
      "%skip-checksum must strip the 4+1 char checksum prefix")
  (is (string= "rest" (cl-tmux/model::%skip-checksum "0000,rest"))
      "%skip-checksum must work with all-zero checksum"))

(test skip-checksum-passthrough-without-checksum
  "%skip-checksum returns the input unchanged when there is no checksum prefix."
  (is (string= "no-checksum" (cl-tmux/model::%skip-checksum "no-checksum"))
      "%skip-checksum must not strip non-checksum input")
  (is (string= "ABCDE" (cl-tmux/model::%skip-checksum "ABCDE"))
      "%skip-checksum must leave strings without comma-at-5 unchanged"))

(test build-flat-tree-single-pane
  "%build-flat-tree with one pane returns a bare layout-leaf."
  (let* ((p    (tl-pane 1 10 5))
         (tree (cl-tmux/model::%build-flat-tree (list p) :h)))
    (is (cl-tmux/model::layout-leaf-p tree) "single pane must produce a layout-leaf")
    (is (eq p (layout-leaf-pane tree)) "leaf must hold the sole pane")))

(test build-flat-tree-two-panes
  "%build-flat-tree with two panes returns a layout-split."
  (let* ((p0   (tl-pane 1 10 5))
         (p1   (tl-pane 2 10 5))
         (tree (cl-tmux/model::%build-flat-tree (list p0 p1) :h)))
    (is (cl-tmux/model::layout-split-p tree) "two panes must produce a layout-split")
    (is (eq :h (cl-tmux/model::layout-split-orientation tree)) "orientation must match")
    (is (eq p0 (layout-leaf-pane (cl-tmux/model::layout-split-first tree)))
        "first child must hold p0")
    (is (cl-tmux/model::layout-leaf-p (cl-tmux/model::layout-split-second tree))
        "second child must be a leaf (for 2-pane flat tree)")))

(test build-flat-tree-three-panes-is-right-leaning
  "%build-flat-tree with three panes produces a right-leaning chain."
  (let* ((panes (loop for i from 1 to 3 collect (tl-pane i 10 5)))
         (tree  (cl-tmux/model::%build-flat-tree panes :v)))
    (is (cl-tmux/model::layout-split-p tree) "three panes must produce a split")
    (is (cl-tmux/model::layout-split-p (cl-tmux/model::layout-split-second tree))
        "right-leaning chain: second child is also a split")
    (is (cl-tmux/model::layout-leaf-p (cl-tmux/model::layout-split-first tree))
        "right-leaning chain: first child is a leaf")))

;;; ── Named-layout helpers: direct unit tests ──────────────────────────────────
;;;
;;; %layout-even-h, %layout-even-v, %layout-main, %layout-tiled, and
;;; %build-grid-tree were previously exercised only indirectly via
;;; apply-named-layout.  These tests call the helpers directly to verify edge
;;; cases (single pane, grid dimensions) that the high-level path does not reach.

(test layout-even-h-single-pane-fills-window
  "%layout-even-h with a single pane assigns the full window width."
  (let* ((pane (tl-pane 1 80 24))
         (win  (make-window :id 1 :name "w" :width 80 :height 24
                            :panes (list pane)
                            :tree  (make-layout-leaf pane))))
    (cl-tmux/model::%layout-even-h win (window-panes win) 1 80 24)
    (is (= 80 (pane-width  pane)) "single pane must span the full width")
    (is (= 24 (pane-height pane)) "single pane must span the full height")))

(test layout-even-h-two-panes-equal-columns
  "%layout-even-h with two panes gives equal columns with a 1-col separator."
  ;; 81 cols: avail = 80, each = 40.  Panes: x=0 w=40, x=41 w=40.
  (let* ((p0  (tl-pane 1 1 1))
         (p1  (tl-pane 2 1 1))
         (win (make-window :id 1 :name "w" :width 81 :height 24
                           :panes (list p0 p1)
                           :tree  (make-layout-split :h
                                     (make-layout-leaf p0)
                                     (make-layout-leaf p1)))))
    (cl-tmux/model::%layout-even-h win (list p0 p1) 2 81 24)
    (is (= 0  (pane-x p0)) "p0 must start at column 0")
    (is (= 40 (pane-width p0)) "p0 must have 40 cols")
    (is (= 41 (pane-x p1)) "p1 must start one column past the separator")
    (is (= 40 (pane-width p1)) "p1 must have 40 cols")))

(test layout-even-v-single-pane-fills-window
  "%layout-even-v with a single pane assigns the full window height."
  (let* ((pane (tl-pane 1 80 24))
         (win  (make-window :id 1 :name "w" :width 80 :height 24
                            :panes (list pane)
                            :tree  (make-layout-leaf pane))))
    (cl-tmux/model::%layout-even-v win (window-panes win) 1 80 24)
    (is (= 80 (pane-width  pane)) "single pane must span the full width")
    (is (= 24 (pane-height pane)) "single pane must span the full height")))

(test layout-even-v-two-panes-equal-rows
  "%layout-even-v with two panes gives equal rows with a 1-row separator."
  ;; 25 rows: avail = 24, each = 12.  Panes: y=0 h=12, y=13 h=12.
  (let* ((p0  (tl-pane 1 1 1))
         (p1  (tl-pane 2 1 1))
         (win (make-window :id 1 :name "w" :width 80 :height 25
                           :panes (list p0 p1)
                           :tree  (make-layout-split :v
                                     (make-layout-leaf p0)
                                     (make-layout-leaf p1)))))
    (cl-tmux/model::%layout-even-v win (list p0 p1) 2 80 25)
    (is (= 0  (pane-y p0)) "p0 must start at row 0")
    (is (= 12 (pane-height p0)) "p0 must have 12 rows")
    (is (= 13 (pane-y p1)) "p1 must start one row past the separator")
    (is (= 12 (pane-height p1)) "p1 must have 12 rows")))

(test layout-main-single-pane-is-leaf
  "%layout-main with only one pane builds a bare leaf and assigns the full rect."
  (let* ((pane (tl-pane 1 80 24))
         (win  (make-window :id 1 :name "w" :width 80 :height 24
                            :panes (list pane)
                            :tree  (make-layout-leaf pane))))
    (cl-tmux/model::%layout-main win (list pane) 80 24 :v :h)
    (is (cl-tmux/model::layout-leaf-p (window-tree win))
        "single-pane main layout must produce a bare leaf")
    (is (= 80 (pane-width  pane)) "sole pane must span full width")
    (is (= 24 (pane-height pane)) "sole pane must span full height")))

(test layout-main-two-panes-h-orientation
  "%layout-main with :v outer produces a :v split (main-horizontal semantics)."
  (let* ((p0  (tl-pane 1 1 1))
         (p1  (tl-pane 2 1 1))
         (win (make-window :id 1 :name "w" :width 80 :height 24
                           :panes (list p0 p1)
                           :tree  (make-layout-split :v
                                     (make-layout-leaf p0)
                                     (make-layout-leaf p1)))))
    (cl-tmux/model::%layout-main win (list p0 p1) 80 24 :v :h)
    (let ((tree (window-tree win)))
      (is (cl-tmux/model::layout-split-p tree) "result must be a split node")
      (is (eq :v (cl-tmux/model::layout-split-orientation tree))
          ":v outer-orient → :v split at root"))))

(test layout-tiled-two-panes-produces-h-split
  "%layout-tiled with 2 panes: ceil(sqrt 2)=2 cols, 1 row → a flat :h tree."
  (let* ((p0  (tl-pane 1 1 1))
         (p1  (tl-pane 2 1 1))
         (win (make-window :id 1 :name "w" :width 81 :height 24
                           :panes (list p0 p1)
                           :tree  (make-layout-split :h
                                     (make-layout-leaf p0)
                                     (make-layout-leaf p1)))))
    (cl-tmux/model::%layout-tiled win (list p0 p1) 2 81 24)
    (is (cl-tmux/model::layout-split-p (window-tree win))
        "2-pane tiled layout must produce a split")
    ;; Both panes must cover the window (non-zero dimensions).
    (is (> (pane-width p0) 0)  "p0 must have positive width")
    (is (> (pane-width p1) 0)  "p1 must have positive width")))

(test layout-tiled-four-panes-is-2x2-grid
  "%layout-tiled with 4 panes: ceil(sqrt 4)=2 cols, 2 rows → 2x2 grid."
  ;; 4 panes in an 81x25 window: 2 cols × 2 rows.
  ;; cols = ceil(sqrt 4) = 2, rows = ceil(4/2) = 2.
  (let* ((panes (loop for i from 1 to 4 collect (tl-pane i 1 1)))
         (win   (tl-window (cl-tmux/model::%build-flat-tree panes :h) 25 81)))
    (cl-tmux/model::%layout-tiled win panes 4 81 25)
    ;; All four panes must have positive dimensions.
    (dolist (p panes)
      (is (> (pane-width  p) 0) "each pane must have positive width")
      (is (> (pane-height p) 0) "each pane must have positive height"))
    ;; Tree must be a :v split of two horizontal rows.
    (let ((tree (window-tree win)))
      (is (cl-tmux/model::layout-split-p tree) "4-pane tiled tree must be a split")
      (is (eq :v (cl-tmux/model::layout-split-orientation tree))
          "outer split must be :v (two rows stacked)"))))

(test layout-tiled-three-panes-last-row-partial
  "%layout-tiled with 3 panes: ceil(sqrt 3)=2 cols, 2 rows, last row has 1 pane."
  ;; cols = 2, rows = 2, last row has only 1 pane → right cell is empty.
  (let* ((panes (loop for i from 1 to 3 collect (tl-pane i 1 1)))
         (win   (tl-window (cl-tmux/model::%build-flat-tree panes :h) 25 81)))
    (cl-tmux/model::%layout-tiled win panes 3 81 25)
    (dolist (p panes)
      (is (> (pane-width  p) 0) "each pane must have positive width")
      (is (> (pane-height p) 0) "each pane must have positive height"))))

(test build-grid-tree-single-row-returns-flat-h-tree
  "%build-grid-tree with a single row-group returns a flat :h chain."
  (let* ((panes (loop for i from 1 to 3 collect (tl-pane i 10 5)))
         (tree  (cl-tmux/model::%build-grid-tree (list panes))))
    ;; Single row → same as %build-flat-tree panes :h
    (is (cl-tmux/model::layout-split-p tree)
        "three panes in one row must produce a split")
    (is (eq :h (cl-tmux/model::layout-split-orientation tree))
        "single-row grid must have :h orientation at root")))

(test build-grid-tree-two-rows-returns-v-split-of-h-rows
  "%build-grid-tree with two rows builds a :v split of two :h rows."
  (let* ((row0 (loop for i from 1 to 2 collect (tl-pane i 10 5)))
         (row1 (loop for i from 3 to 4 collect (tl-pane i 10 5)))
         (tree (cl-tmux/model::%build-grid-tree (list row0 row1))))
    (is (cl-tmux/model::layout-split-p tree)
        "two rows must produce a split at root")
    (is (eq :v (cl-tmux/model::layout-split-orientation tree))
        "root of a 2-row grid must be a :v split")
    (is (cl-tmux/model::layout-split-p (cl-tmux/model::layout-split-first tree))
        "first child must itself be a split (the :h row)")
    (is (eq :h (cl-tmux/model::layout-split-orientation
                (cl-tmux/model::layout-split-first tree)))
        "first child must be an :h split (one row)")))

;;; ── apply-named-layout — high-level named layout dispatch ───────────────────

(test apply-named-layout-even-horizontal-fills-window
  "apply-named-layout :even-horizontal distributes panes into equal columns."
  (let* ((panes (loop for i from 1 to 3 collect (tl-pane i 1 1)))
         (win   (make-window :id 1 :name "w" :width 82 :height 24
                             :panes panes
                             :tree  (cl-tmux/model::%build-flat-tree panes :h))))
    (apply-named-layout win :even-horizontal)
    ;; All panes must have positive width and height.
    (dolist (p (window-panes win))
      (is (> (pane-width  p) 0) "each pane must have positive width")
      (is (> (pane-height p) 0) "each pane must have positive height"))
    ;; Panes must be ordered left to right (increasing x).
    (let ((xs (mapcar #'pane-x (window-panes win))))
      (is (equal xs (sort (copy-seq xs) #'<))
          "pane x positions must be in ascending order"))))

(test apply-named-layout-even-vertical-fills-window
  "apply-named-layout :even-vertical distributes panes into equal rows."
  (let* ((panes (loop for i from 1 to 3 collect (tl-pane i 1 1)))
         (win   (make-window :id 1 :name "w" :width 80 :height 25
                             :panes panes
                             :tree  (cl-tmux/model::%build-flat-tree panes :v))))
    (apply-named-layout win :even-vertical)
    (dolist (p (window-panes win))
      (is (> (pane-width  p) 0) "each pane must have positive width")
      (is (> (pane-height p) 0) "each pane must have positive height"))
    ;; Panes must be ordered top to bottom (increasing y).
    (let ((ys (mapcar #'pane-y (window-panes win))))
      (is (equal ys (sort (copy-seq ys) #'<))
          "pane y positions must be in ascending order"))))

(test apply-named-layout-main-horizontal-first-pane-on-top
  "apply-named-layout :main-horizontal puts the first pane in the top half."
  (let* ((panes (loop for i from 1 to 3 collect (tl-pane i 1 1)))
         (win   (make-window :id 1 :name "w" :width 80 :height 25
                             :panes panes
                             :tree  (cl-tmux/model::%build-flat-tree panes :v))))
    (apply-named-layout win :main-horizontal)
    ;; The first pane must be at y=0 (top).
    (is (= 0 (pane-y (first (window-panes win))))
        "first pane must start at row 0 in main-horizontal")
    ;; All secondary panes must have a larger y than the first pane.
    (let ((first-pane-bottom (+ (pane-y (first (window-panes win)))
                                (pane-height (first (window-panes win))))))
      (dolist (p (rest (window-panes win)))
        (is (> (pane-y p) first-pane-bottom)
            "secondary panes must be below the first pane's bottom edge")))))

(test apply-named-layout-main-vertical-first-pane-on-left
  "apply-named-layout :main-vertical puts the first pane in the left half."
  (let* ((panes (loop for i from 1 to 3 collect (tl-pane i 1 1)))
         (win   (make-window :id 1 :name "w" :width 81 :height 24
                             :panes panes
                             :tree  (cl-tmux/model::%build-flat-tree panes :h))))
    (apply-named-layout win :main-vertical)
    ;; The first pane must be at x=0 (leftmost).
    (is (= 0 (pane-x (first (window-panes win))))
        "first pane must start at column 0 in main-vertical")
    ;; All secondary panes must have a larger x than the first pane.
    (let ((first-pane-right (+ (pane-x (first (window-panes win)))
                               (pane-width (first (window-panes win))))))
      (dolist (p (rest (window-panes win)))
        (is (> (pane-x p) first-pane-right)
            "secondary panes must be to the right of the first pane's right edge")))))

(test apply-named-layout-tiled-four-panes
  "apply-named-layout :tiled with 4 panes produces a near-square 2x2 grid."
  (let* ((panes (loop for i from 1 to 4 collect (tl-pane i 1 1)))
         (win   (make-window :id 1 :name "w" :width 81 :height 25
                             :panes panes
                             :tree  (cl-tmux/model::%build-flat-tree panes :h))))
    (apply-named-layout win :tiled)
    (dolist (p (window-panes win))
      (is (> (pane-width  p) 0) "each pane must have positive width in tiled")
      (is (> (pane-height p) 0) "each pane must have positive height in tiled"))))

(test apply-named-layout-single-pane-any-layout
  "apply-named-layout with a single pane assigns the full window regardless of layout."
  (dolist (layout-name '(:even-horizontal :even-vertical
                         :main-horizontal :main-vertical :tiled))
    (let* ((pane (tl-pane 1 1 1))
           (win  (make-window :id 1 :name "w" :width 80 :height 24
                              :panes (list pane)
                              :tree  (make-layout-leaf pane))))
      (apply-named-layout win layout-name)
      (is (= 80 (pane-width  pane))
          "sole pane must span full width for layout ~A" layout-name)
      (is (= 24 (pane-height pane))
          "sole pane must span full height for layout ~A" layout-name))))

(test apply-named-layout-empty-window-returns-nil
  "apply-named-layout returns NIL immediately when the window has no panes."
  (let ((win (make-window :id 1 :name "w" :width 80 :height 24 :panes nil :tree nil)))
    (is (null (apply-named-layout win :even-horizontal))
        "apply-named-layout on empty window must return NIL")))

;;; ── Layout persistence: internal helpers ─────────────────────────────────────

(test split-bounding-box-h-split
  "%split-bounding-box derives correct bounding box for a laid-out :h split."
  (let* ((l0  (tl-leaf 1 1 1))
         (l1  (tl-leaf 2 1 1))
         (tree (make-layout-split :h l0 l1)))
    ;; Lay out so pane coordinates are concrete.
    (cl-tmux/model::layout-assign tree 5 3 40 12)
    (multiple-value-bind (min-x min-y width height)
        (cl-tmux/model::%split-bounding-box tree)
      (is (= 5    min-x)  "bounding-box x must be the leftmost pane-x")
      (is (= 3    min-y)  "bounding-box y must be the topmost pane-y")
      (is (= 40   width)  "bounding-box width must span both panes + separator")
      (is (= 12   height) "bounding-box height must equal pane height"))))

(test split-bounding-box-v-split
  "%split-bounding-box derives correct bounding box for a laid-out :v split."
  (let* ((l0  (tl-leaf 1 1 1))
         (l1  (tl-leaf 2 1 1))
         (tree (make-layout-split :v l0 l1)))
    (cl-tmux/model::layout-assign tree 0 0 80 21)
    (multiple-value-bind (min-x min-y width height)
        (cl-tmux/model::%split-bounding-box tree)
      (is (= 0    min-x)  "bounding-box x must be 0")
      (is (= 0    min-y)  "bounding-box y must be 0")
      (is (= 80   width)  "bounding-box width must equal window width")
      (is (= 21   height) "bounding-box height must span both panes + separator"))))

(test node-to-string-single-leaf
  "%node->string produces the correct WxH,X,Y,pane-id fragment for a leaf."
  (let* ((p    (tl-pane 7 20 10))
         (leaf (make-layout-leaf p)))
    ;; Assign explicit coordinates.
    (cl-tmux/model::layout-assign leaf 3 5 20 10)
    (let ((s (cl-tmux/model::%node->string leaf)))
      (is (stringp s) "%node->string must return a string")
      (is (search "20x10" s) "fragment must contain WxH")
      (is (search ",3,5," s) "fragment must contain X,Y coordinates")
      (is (search "7" s) "fragment must contain the pane id"))))

(test node-to-string-nil-returns-empty-string
  "%node->string returns the empty string for a NIL node."
  (is (string= "" (cl-tmux/model::%node->string nil))
      "%node->string on NIL must return empty string"))

(test read-digits-parses-integer-at-pos
  "%read-digits reads a decimal integer from a string starting at a given position."
  (multiple-value-bind (val end)
      (cl-tmux/model::%read-digits "abc123xyz" 3)
    (is (= 123 val) "%read-digits must return the integer value 123")
    (is (= 6 end) "%read-digits end position must be past the last digit"))
  ;; Position at beginning:
  (multiple-value-bind (val end)
      (cl-tmux/model::%read-digits "42rest" 0)
    (is (= 42 val))
    (is (= 2 end))))

(test read-digits-at-end-of-string-returns-digits-up-to-end
  "%read-digits stops at the end of string when all trailing chars are digits."
  ;; "100" has digits from position 0 to 3 (end of string).
  (multiple-value-bind (val end)
      (cl-tmux/model::%read-digits "100" 0)
    (is (= 100 val) "%read-digits on '100' must return 100")
    (is (= 3 end) "end position must be 3 (past all digits")))

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

(test split-fits-p-adequate-h-pane
  "%split-fits-p returns T when pane width ≥ 2×min-width + 1 separator."
  ;; min-width = 2, so need at least 5 columns.
  (let ((pane (make-pane :id 1 :fd -1 :pid -1 :width 5 :height 24
                         :screen (make-screen 5 24))))
    (is (cl-tmux/model::%split-fits-p pane :h)
        "a 5-column pane must be splittable horizontally")))

(test split-fits-p-too-narrow-h-pane-returns-nil
  "%split-fits-p returns NIL when pane width < 2×min-width + 1 separator."
  ;; 4 columns: 2+1+2=5 needed, 4 < 5 → cannot split.
  (let ((pane (make-pane :id 1 :fd -1 :pid -1 :width 4 :height 24
                         :screen (make-screen 4 24))))
    (is (null (cl-tmux/model::%split-fits-p pane :h))
        "a 4-column pane must NOT be splittable horizontally")))

(test split-fits-p-adequate-v-pane
  "%split-fits-p returns T when pane height ≥ 2×min-height + 1 separator."
  ;; min-height = 1, so need at least 3 rows.
  (let ((pane (make-pane :id 1 :fd -1 :pid -1 :width 80 :height 3
                         :screen (make-screen 80 3))))
    (is (cl-tmux/model::%split-fits-p pane :v)
        "a 3-row pane must be splittable vertically")))

(test split-fits-p-too-short-v-pane-returns-nil
  "%split-fits-p returns NIL when pane height < 2×min-height + 1 separator."
  ;; 2 rows: 1+1+1=3 needed, 2 < 3 → cannot split.
  (let ((pane (make-pane :id 1 :fd -1 :pid -1 :width 80 :height 2
                         :screen (make-screen 80 2))))
    (is (null (cl-tmux/model::%split-fits-p pane :v))
        "a 2-row pane must NOT be splittable vertically")))

;;; ── Table-driven: %layout-checksum known-value tests ─────────────────────────

(test layout-checksum-known-values-match-expected
  "%layout-checksum produces the correct tmux-compatible 4-hex checksum."
  ;; Computed by running the same algorithm in isolation.
  (let ((cases
         ;; (input expected-checksum)
         (list (list ""          "0000")   ; empty string → 0
               (list "a"        "0061")   ; "a" = 97 decimal = 0x61
               (list "1x1,0,0,1" (cl-tmux/model::%layout-checksum "1x1,0,0,1")))))
    ;; The first two are pinned; the third just checks determinism.
    (destructuring-bind ((input1 expected1)
                         (input2 expected2)
                         (input3 expected3))
        cases
      (is (string= expected1 (cl-tmux/model::%layout-checksum input1))
          "empty string checksum must be 0000")
      (is (string= expected2 (cl-tmux/model::%layout-checksum input2))
          "single-char 'a' checksum must be 0061")
      (is (string= expected3 (cl-tmux/model::%layout-checksum input3))
          "checksum of typical layout body must be deterministic"))))

;;; ── Table-driven: resize-direction-orientation mapping ───────────────────────
;;;
;;; The 4-case mapping is already tested in layout-geometry-tests.lisp;
;;; a table-driven form here validates the same facts via a single loop.

(test resize-direction-orientation-all-directions-table
  "Table-driven: all four directions map to the correct split orientation."
  (let ((cases '((:left  . :h)
                 (:right . :h)
                 (:up    . :v)
                 (:down  . :v))))
    (dolist (c cases)
      (is (eq (cdr c) (cl-tmux/model::resize-direction-orientation (car c)))
          "direction ~A must map to orientation ~A" (car c) (cdr c)))))
