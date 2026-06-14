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
      (dolist (c (list (list (pane-x      p0) 0  "p0 at column 0")
                       (list (pane-width  p0) 40 "p0 width 40")
                       (list (pane-x      p1) 41 "right pane sits one column past the separator")
                       (list (pane-width  p1) 40 "p1 width 40")
                       (list (pane-height p0) 24 "full height on a left/right split")
                       (list (pane-height p1) 24 "p1 height 24")
                       (list (- (pane-x p1) (+ (pane-x p0) (pane-width p0))) 1
                             "exactly one separator column between them")))
        (destructuring-bind (actual expected desc) c
          (is (= expected actual) "~A" desc))))))

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
      ;; Bottom is split left|right: 80 - 1 separator = 79, 40 left / 39 right
      ;; (banker's rounding of 39.5 → 40 for the first child).
      (dolist (c (list (list (pane-y      ptop)  0  "top pane at row 0")
                       (list (pane-height ptop) 12  "top pane height 12")
                       (list (pane-width  ptop) 80  "top spans full width")
                       (list (pane-y      pbl)  13  "bottom-left starts at row 13")
                       (list (pane-y      pbr)  13  "bottom-right starts at row 13")
                       (list (pane-height pbl)  12  "bottom-left height 12")
                       (list (pane-height pbr)  12  "bottom-right height 12")
                       (list (pane-x      pbl)   0  "bottom-left at column 0")
                       (list (pane-width  pbl)  40  "bottom-left width 40")
                       (list (pane-x      pbr)  41  "bottom-right at column 41")
                       (list (pane-width  pbr)  39  "bottom-right width 39")))
        (destructuring-bind (actual expected desc) c
          (is (= expected actual) "~A" desc)))
      ;; No overlap between the two bottom panes.
      (is (<= (+ (pane-x pbl) (pane-width pbl)) (pane-x pbr))
          "bottom panes must not overlap")))

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

(test layout-find-leaf-table
  "layout-find-leaf returns the leaf for a present pane, NIL for an absent one."
  (let* ((l0   (tl-leaf 1 1 1))
         (l1   (tl-leaf 2 1 1))
         (tree (make-layout-split :h l0 l1))
         (p0   (layout-leaf-pane l0))
         (pabs (make-pane :id 99 :fd -1 :pid -1 :screen (make-screen 1 1))))
    (dolist (row (list (list p0   l0  "present pane → its leaf node")
                       (list pabs nil "absent pane → NIL")))
      (destructuring-bind (pane expected desc) row
        (is (eq expected (layout-find-leaf tree pane)) "~A" desc)))))

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

(test layout-to-string-split-notation
  "layout->string uses {..} for :h splits and [..] for :v splits."
  (dolist (c '((:h #\{ #\} "H-split uses braces")
               (:v #\[ #\] "V-split uses brackets")))
    (destructuring-bind (orient open close label) c
      (let* ((l0  (tl-leaf 1 1 1))
             (l1  (tl-leaf 2 1 1))
             (win (tl-window (make-layout-split orient l0 l1) 24 80))
             (s   (layout->string win)))
        (is (find open  (coerce s 'list)) "~A: must use ~C" label open)
        (is (find close (coerce s 'list)) "~A: must use ~C" label close)))))

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

(test skip-checksum-table
  "%skip-checksum strips a 4-hex+comma prefix, or passes the string through unchanged."
  (dolist (c '(("ABCD,rest"    "rest"         "valid hex checksum stripped")
               ("0000,rest"    "rest"         "all-zero checksum stripped")
               ("no-checksum"  "no-checksum"  "no prefix — pass through")
               ("ABCDE"        "ABCDE"        "5-char without comma — pass through")))
    (destructuring-bind (input expected desc) c
      (is (string= expected (cl-tmux/model::%skip-checksum input))
          "~A: ~S → ~S" desc input expected))))

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

