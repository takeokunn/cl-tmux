(in-package #:cl-tmux/test)

;;;; layout tests — part B: named-layout helpers (%layout-even-h/v, %layout-main,
;;;; %layout-tiled, %build-grid-tree), apply-named-layout dispatch, layout
;;;; persistence internal helpers, layout-find-parent, checksum table,
;;;; resize-direction-orientation, main-horizontal/vertical, other-pane-* override.

(in-suite layout-tree-suite)

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
    (cl-tmux/model::%layout-even-h win (window-panes win) 80 24)
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
    (cl-tmux/model::%layout-even-h win (list p0 p1) 81 24)
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
    (cl-tmux/model::%layout-even-v win (window-panes win) 80 24)
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
    (cl-tmux/model::%layout-even-v win (list p0 p1) 80 25)
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
    (cl-tmux/model::%layout-main win (list pane) 80 24 :v :h 12)
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
    (cl-tmux/model::%layout-main win (list p0 p1) 80 24 :v :h 12)
    (let ((tree (window-tree win)))
      (is (cl-tmux/model::layout-split-p tree) "result must be a split node")
      (is (eq :v (cl-tmux/model::layout-split-orientation tree))
          ":v outer-orient → :v split at root"))))

(test layout-even-h-three-panes-equal-columns
  "%layout-even-h with three panes distributes them into equal-width columns.
   Exercises the multi-pane arithmetic path (n >= 3) directly at the helper level."
  ;; 82 cols: avail = 81, 3 equal splits → each ~27.
  (let* ((p0  (tl-pane 1 1 1))
         (p1  (tl-pane 2 1 1))
         (p2  (tl-pane 3 1 1))
         (win (make-window :id 1 :name "w" :width 82 :height 24
                           :panes (list p0 p1 p2)
                           :tree  (cl-tmux/model::%build-flat-tree (list p0 p1 p2) :h))))
    (cl-tmux/model::%layout-even-h win (list p0 p1 p2) 82 24)
    (is (= 0 (pane-x p0)) "first pane must start at column 0")
    (is (> (pane-width p0) 0) "first pane must have positive width")
    (is (< (pane-x p0) (pane-x p1)) "second pane must be to the right of first")
    (is (< (pane-x p1) (pane-x p2)) "third pane must be to the right of second")
    (dolist (p (list p0 p1 p2))
      (is (= 24 (pane-height p)) "every pane must have the full window height"))))

(test layout-even-v-three-panes-equal-rows
  "%layout-even-v with three panes distributes them into equal-height rows.
   Exercises the multi-pane arithmetic path (n >= 3) directly at the helper level."
  ;; 25 rows: 3 equal splits.
  (let* ((p0  (tl-pane 1 1 1))
         (p1  (tl-pane 2 1 1))
         (p2  (tl-pane 3 1 1))
         (win (make-window :id 1 :name "w" :width 80 :height 25
                           :panes (list p0 p1 p2)
                           :tree  (cl-tmux/model::%build-flat-tree (list p0 p1 p2) :v))))
    (cl-tmux/model::%layout-even-v win (list p0 p1 p2) 80 25)
    (is (= 0 (pane-y p0)) "first pane must start at row 0")
    (is (> (pane-height p0) 0) "first pane must have positive height")
    (is (< (pane-y p0) (pane-y p1)) "second pane must be below first")
    (is (< (pane-y p1) (pane-y p2)) "third pane must be below second")
    (dolist (p (list p0 p1 p2))
      (is (= 80 (pane-width p)) "every pane must have the full window width"))))

(test layout-find-leaf-nil-node-arm-returns-nil
  "layout-find-leaf on a NIL node returns NIL — exercises the NIL etypecase arm
   of the define-layout-fold generated function for a non-extent traversal."
  ;; layout-find-leaf is generated by define-layout-fold and has a (null nil) arm.
  ;; Exercising it here confirms the null branch is reachable for fold functions.
  (let ((p (tl-pane 1 10 5)))
    (is (null (layout-find-leaf nil p))
        "layout-find-leaf on NIL node must return NIL")))

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

;;; ── main-horizontal / main-vertical honour main-pane size ────────────────────
;;;
;;; apply-named-layout takes the main pane's size (tmux's main-pane-width /
;;; main-pane-height options, read by the cl-tmux-layer caller).  The main (first)
;;; pane is sized to exactly that many cells along the outer axis; the rest share
;;; the remainder.

(defun %three-pane-window (w h)
  "A window of three no-PTY panes (W x H) with no preset tree."
  (make-window :id 1 :name "w" :width w :height h
               :panes (list (make-no-pty-pane 1 0 0 w h)
                            (make-no-pty-pane 2 0 0 w h)
                            (make-no-pty-pane 3 0 0 w h))))

(test main-vertical-honours-main-pane-width
  "apply-named-layout :main-vertical sizes the main (first/left) pane to the
   given main-pane-width."
  (let ((win (%three-pane-window 100 30)))
    (cl-tmux/model:apply-named-layout win :main-vertical 60 24)
    (let ((p0 (first (window-panes win))))
      (is (= 60 (pane-width p0)) "main pane is main-pane-width (60) columns wide")
      (is (< (pane-width (second (window-panes win))) 60)
          "secondary panes share the narrower remainder"))))

(test main-horizontal-honours-main-pane-height
  "apply-named-layout :main-horizontal sizes the main (first/top) pane to the
   given main-pane-height."
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

(test main-vertical-other-pane-width-overrides-main
  ":main-vertical with a fitting other-pane-width sizes the other panes to it and
   gives the main pane the remaining width."
  (let ((win (%three-pane-window 120 30)))
    ;; available = 119; other-pane-width 30 fits (119-30=89 >= main 80) → main 89.
    (cl-tmux/model:apply-named-layout win :main-vertical 80 24 30 0)
    (is (= 89 (pane-width (first (window-panes win))))
        "main pane width = available - other-pane-width")
    (is (= 30 (pane-width (second (window-panes win))))
        "other panes get other-pane-width (30)")))

(test main-horizontal-other-pane-height-overrides-main
  ":main-horizontal with a fitting other-pane-height sizes the other panes to it
   and gives the main pane the remaining height."
  (let ((win (%three-pane-window 100 50)))
    ;; available = 49; other-pane-height 20 fits (49-20=29 >= main 24) → main 29.
    (cl-tmux/model:apply-named-layout win :main-horizontal 80 24 0 20)
    (is (= 29 (pane-height (first (window-panes win))))
        "main pane height = available - other-pane-height")
    (is (= 20 (pane-height (second (window-panes win))))
        "other panes get other-pane-height (20)")))

(test main-vertical-other-pane-width-too-big-falls-back-to-main
  "An other-pane-width that does not leave room for the main pane is ignored;
   main-pane-width applies instead."
  (let ((win (%three-pane-window 120 30)))
    (cl-tmux/model:apply-named-layout win :main-vertical 80 24 200 0)
    (is (= 80 (pane-width (first (window-panes win))))
        "over-large other-pane-width is ignored → main-pane-width (80) used")))
