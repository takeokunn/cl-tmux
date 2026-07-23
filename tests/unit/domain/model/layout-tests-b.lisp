(in-package #:cl-tmux/test)

;;;; layout tests — part B: named-layout helpers (%layout-even-h/v, %layout-main,
;;;; %layout-tiled, %build-grid-tree), apply-named-layout dispatch, layout
;;;; persistence internal helpers, layout-find-parent, checksum table,
;;;; resize-direction-orientation, main-horizontal/vertical, other-pane-* override.

(describe "layout-tree-suite"

  ;;; ── Named-layout helpers: direct unit tests ──────────────────────────────────
  ;;;
  ;;; %layout-even-h, %layout-even-v, %layout-main, %layout-tiled, and
  ;;; %build-grid-tree were previously exercised only indirectly via
  ;;; apply-named-layout.  These tests call the helpers directly to verify edge
  ;;; cases (single pane, grid dimensions) that the high-level path does not reach.

  ;; %layout-even-h and %layout-even-v with a single pane both assign the full window rect.
  (it "layout-even-single-pane-fills-window-table"
    (dolist (row (list (list #'cl-tmux/model::%layout-even-h "even-h")
                       (list #'cl-tmux/model::%layout-even-v "even-v")))
      (destructuring-bind (layout-fn desc) row
        (let* ((pane (tl-pane 1 80 24))
               (win  (make-window :id 1 :name "w" :width 80 :height 24
                                  :panes (list pane)
                                  :tree  (make-layout-leaf pane))))
          (funcall layout-fn win (window-panes win) 80 24)
          (check-table (list (list (pane-width  pane) 80
                                   (format nil "~A single pane must span the full width" desc))
                             (list (pane-height pane) 24
                                   (format nil "~A single pane must span the full height" desc)))
                       :test #'equal)))))

  ;; %layout-even-h with two panes gives equal columns with a 1-col separator.
  (it "layout-even-h-two-panes-equal-columns"
    ;; 81 cols: avail = 80, each = 40.  Panes: x=0 w=40, x=41 w=40.
    (let* ((p0  (tl-pane 1 1 1))
           (p1  (tl-pane 2 1 1))
           (win (make-window :id 1 :name "w" :width 81 :height 24
                             :panes (list p0 p1)
                             :tree  (make-layout-split :h
                                       (make-layout-leaf p0)
                                       (make-layout-leaf p1)))))
      (cl-tmux/model::%layout-even-h win (list p0 p1) 81 24)
      (check-table (list (list (pane-x p0)    0  "p0 must start at column 0")
                         (list (pane-width p0) 40 "p0 must have 40 cols")
                         (list (pane-x p1)    41 "p1 must start one column past the separator")
                         (list (pane-width p1) 40 "p1 must have 40 cols"))
                   :test #'equal)))

  ;; %layout-even-v with two panes gives equal rows with a 1-row separator.
  (it "layout-even-v-two-panes-equal-rows"
    ;; 25 rows: avail = 24, each = 12.  Panes: y=0 h=12, y=13 h=12.
    (let* ((p0  (tl-pane 1 1 1))
           (p1  (tl-pane 2 1 1))
           (win (make-window :id 1 :name "w" :width 80 :height 25
                             :panes (list p0 p1)
                             :tree  (make-layout-split :v
                                       (make-layout-leaf p0)
                                       (make-layout-leaf p1)))))
      (cl-tmux/model::%layout-even-v win (list p0 p1) 80 25)
      (check-table (list (list (pane-y p0)     0  "p0 must start at row 0")
                         (list (pane-height p0) 12 "p0 must have 12 rows")
                         (list (pane-y p1)     13 "p1 must start one row past the separator")
                         (list (pane-height p1) 12 "p1 must have 12 rows"))
                   :test #'equal)))

  ;; %layout-main with only one pane builds a bare leaf and assigns the full rect.
  (it "layout-main-single-pane-is-leaf"
    (let* ((pane (tl-pane 1 80 24))
           (win  (make-window :id 1 :name "w" :width 80 :height 24
                              :panes (list pane)
                              :tree  (make-layout-leaf pane))))
      (cl-tmux/model::%layout-main win (list pane) 80 24 :v :h 12)
      (expect (cl-tmux/model::layout-leaf-p (window-tree win)))
      (expect (= 80 (pane-width  pane)))
      (expect (= 24 (pane-height pane)))))

  ;; %layout-main with :v outer produces a :v split (main-horizontal semantics).
  (it "layout-main-two-panes-h-orientation"
    (let* ((p0  (tl-pane 1 1 1))
           (p1  (tl-pane 2 1 1))
           (win (make-window :id 1 :name "w" :width 80 :height 24
                             :panes (list p0 p1)
                             :tree  (make-layout-split :v
                                       (make-layout-leaf p0)
                                       (make-layout-leaf p1)))))
      (cl-tmux/model::%layout-main win (list p0 p1) 80 24 :v :h 12)
      (let ((tree (window-tree win)))
        (expect (cl-tmux/model::layout-split-p tree))
        (expect (eq :v (cl-tmux/model::layout-split-orientation tree))))))

  ;; %layout-even-h with three panes distributes them into equal-width columns.
  ;; Exercises the multi-pane arithmetic path (n >= 3) directly at the helper level.
  (it "layout-even-h-three-panes-equal-columns"
    ;; 82 cols: avail = 81, 3 equal splits → each ~27.
    (let* ((p0  (tl-pane 1 1 1))
           (p1  (tl-pane 2 1 1))
           (p2  (tl-pane 3 1 1))
           (win (make-window :id 1 :name "w" :width 82 :height 24
                             :panes (list p0 p1 p2)
                             :tree  (cl-tmux/model::%build-flat-tree (list p0 p1 p2) :h))))
      (cl-tmux/model::%layout-even-h win (list p0 p1 p2) 82 24)
      (expect (= 0 (pane-x p0)))
      (expect (> (pane-width p0) 0))
      (expect (< (pane-x p0) (pane-x p1)))
      (expect (< (pane-x p1) (pane-x p2)))
      (dolist (p (list p0 p1 p2))
        (expect (= 24 (pane-height p))))))

  ;; %layout-even-v with three panes distributes them into equal-height rows.
  ;; Exercises the multi-pane arithmetic path (n >= 3) directly at the helper level.
  (it "layout-even-v-three-panes-equal-rows"
    ;; 25 rows: 3 equal splits.
    (let* ((p0  (tl-pane 1 1 1))
           (p1  (tl-pane 2 1 1))
           (p2  (tl-pane 3 1 1))
           (win (make-window :id 1 :name "w" :width 80 :height 25
                             :panes (list p0 p1 p2)
                             :tree  (cl-tmux/model::%build-flat-tree (list p0 p1 p2) :v))))
      (cl-tmux/model::%layout-even-v win (list p0 p1 p2) 80 25)
      (expect (= 0 (pane-y p0)))
      (expect (> (pane-height p0) 0))
      (expect (< (pane-y p0) (pane-y p1)))
      (expect (< (pane-y p1) (pane-y p2)))
      (dolist (p (list p0 p1 p2))
        (expect (= 80 (pane-width p))))))

  ;; layout-find-leaf on a NIL node returns NIL — exercises the NIL etypecase arm
  ;; of the define-layout-fold generated function for a non-extent traversal.
  (it "layout-find-leaf-nil-node-arm-returns-nil"
    ;; layout-find-leaf is generated by define-layout-fold and has a (null nil) arm.
    ;; Exercising it here confirms the null branch is reachable for fold functions.
    (let ((p (tl-pane 1 10 5)))
      (expect (null (layout-find-leaf nil p)))))

  ;; %layout-tiled with 2 panes: ceil(sqrt 2)=2 cols, 1 row → a flat :h tree.
  (it "layout-tiled-two-panes-produces-h-split"
    (let* ((p0  (tl-pane 1 1 1))
           (p1  (tl-pane 2 1 1))
           (win (make-window :id 1 :name "w" :width 81 :height 24
                             :panes (list p0 p1)
                             :tree  (make-layout-split :h
                                       (make-layout-leaf p0)
                                       (make-layout-leaf p1)))))
      (cl-tmux/model::%layout-tiled win (list p0 p1) 2 81 24)
      (expect (cl-tmux/model::layout-split-p (window-tree win)))
      ;; Both panes must cover the window (non-zero dimensions).
      (expect (> (pane-width p0) 0))
      (expect (> (pane-width p1) 0))))

  ;; %layout-tiled with 4 panes: ceil(sqrt 4)=2 cols, 2 rows → 2x2 grid.
  (it "layout-tiled-four-panes-is-2x2-grid"
    ;; 4 panes in an 81x25 window: 2 cols × 2 rows.
    ;; cols = ceil(sqrt 4) = 2, rows = ceil(4/2) = 2.
    (let* ((panes (loop for i from 1 to 4 collect (tl-pane i 1 1)))
           (win   (tl-window (cl-tmux/model::%build-flat-tree panes :h) 25 81)))
      (cl-tmux/model::%layout-tiled win panes 4 81 25)
      ;; All four panes must have positive dimensions.
      (dolist (p panes)
        (expect (> (pane-width  p) 0))
        (expect (> (pane-height p) 0)))
      ;; Tree must be a :v split of two horizontal rows.
      (let ((tree (window-tree win)))
        (expect (cl-tmux/model::layout-split-p tree))
        (expect (eq :v (cl-tmux/model::layout-split-orientation tree))))))

  ;; %layout-tiled with 3 panes: ceil(sqrt 3)=2 cols, 2 rows, last row has 1 pane.
  (it "layout-tiled-three-panes-last-row-partial"
    ;; cols = 2, rows = 2, last row has only 1 pane → right cell is empty.
    (let* ((panes (loop for i from 1 to 3 collect (tl-pane i 1 1)))
           (win   (tl-window (cl-tmux/model::%build-flat-tree panes :h) 25 81)))
      (cl-tmux/model::%layout-tiled win panes 3 81 25)
      (dolist (p panes)
        (expect (> (pane-width  p) 0))
        (expect (> (pane-height p) 0)))))

  ;; %build-grid-tree with a single row-group returns a flat :h chain.
  (it "build-grid-tree-single-row-returns-flat-h-tree"
    (let* ((panes (loop for i from 1 to 3 collect (tl-pane i 10 5)))
           (tree  (cl-tmux/model::%build-grid-tree (list panes))))
      ;; Single row → same as %build-flat-tree panes :h
      (expect (cl-tmux/model::layout-split-p tree))
      (expect (eq :h (cl-tmux/model::layout-split-orientation tree)))))

  ;; %build-grid-tree with two rows builds a :v split of two :h rows.
  (it "build-grid-tree-two-rows-returns-v-split-of-h-rows"
    (let* ((row0 (loop for i from 1 to 2 collect (tl-pane i 10 5)))
           (row1 (loop for i from 3 to 4 collect (tl-pane i 10 5)))
           (tree (cl-tmux/model::%build-grid-tree (list row0 row1))))
      (expect (cl-tmux/model::layout-split-p tree))
      (expect (eq :v (cl-tmux/model::layout-split-orientation tree)))
      (expect (cl-tmux/model::layout-split-p (cl-tmux/model::layout-split-first tree)))
      (expect (eq :h (cl-tmux/model::layout-split-orientation
                      (cl-tmux/model::layout-split-first tree))))))

  ;;; ── apply-named-layout — high-level named layout dispatch ───────────────────

  ;; apply-named-layout :even-horizontal and :even-vertical each distribute 3 panes
  ;; with positive dimensions, ordered along their respective axis.
  (it "apply-named-layout-even-h-and-v-distribute-panes"
    (let* ((panes (loop for i from 1 to 3 collect (tl-pane i 1 1)))
           (win   (make-window :id 1 :name "w" :width 82 :height 24
                               :panes panes
                               :tree  (cl-tmux/model::%build-flat-tree panes :h))))
      (apply-named-layout win :even-horizontal)
      (dolist (p (window-panes win))
        (expect (> (pane-width  p) 0))
        (expect (> (pane-height p) 0)))
      (let ((xs (mapcar #'pane-x (window-panes win))))
        (expect (equal xs (sort (copy-seq xs) #'<)))))
    (let* ((panes (loop for i from 1 to 3 collect (tl-pane i 1 1)))
           (win   (make-window :id 1 :name "w" :width 80 :height 25
                               :panes panes
                               :tree  (cl-tmux/model::%build-flat-tree panes :v))))
      (apply-named-layout win :even-vertical)
      (dolist (p (window-panes win))
        (expect (> (pane-width  p) 0))
        (expect (> (pane-height p) 0)))
      (let ((ys (mapcar #'pane-y (window-panes win))))
        (expect (equal ys (sort (copy-seq ys) #'<))))))

  ;; apply-named-layout :main-horizontal puts the first pane at the top;
  ;; :main-vertical puts the first pane at the left.
  (it "apply-named-layout-main-h-and-v-first-pane-position"
    (let* ((panes (loop for i from 1 to 3 collect (tl-pane i 1 1)))
           (win   (make-window :id 1 :name "w" :width 80 :height 25
                               :panes panes
                               :tree  (cl-tmux/model::%build-flat-tree panes :v))))
      (apply-named-layout win :main-horizontal)
      (expect (= 0 (pane-y (first (window-panes win)))))
      (let ((first-pane-bottom (+ (pane-y (first (window-panes win)))
                                  (pane-height (first (window-panes win))))))
        (dolist (p (rest (window-panes win)))
          (expect (> (pane-y p) first-pane-bottom)))))
    (let* ((panes (loop for i from 1 to 3 collect (tl-pane i 1 1)))
           (win   (make-window :id 1 :name "w" :width 81 :height 24
                               :panes panes
                               :tree  (cl-tmux/model::%build-flat-tree panes :h))))
      (apply-named-layout win :main-vertical)
      (expect (= 0 (pane-x (first (window-panes win)))))
      (let ((first-pane-right (+ (pane-x (first (window-panes win)))
                                 (pane-width (first (window-panes win))))))
        (dolist (p (rest (window-panes win)))
          (expect (> (pane-x p) first-pane-right))))))

  ;; apply-named-layout with a single pane assigns the full window regardless of layout.
  (it "apply-named-layout-single-pane-any-layout"
    (dolist (layout-name '(:even-horizontal :even-vertical
                           :main-horizontal :main-vertical :tiled))
      (let* ((pane (tl-pane 1 1 1))
             (win  (make-window :id 1 :name "w" :width 80 :height 24
                                :panes (list pane)
                                :tree  (make-layout-leaf pane))))
        (apply-named-layout win layout-name)
        (expect (= 80 (pane-width  pane)))
        (expect (= 24 (pane-height pane))))))

  ;; apply-named-layout returns NIL immediately when the window has no panes.
  (it "apply-named-layout-empty-window-returns-nil"
    (let ((win (make-window :id 1 :name "w" :width 80 :height 24 :panes nil :tree nil)))
      (expect (null (apply-named-layout win :even-horizontal))))))
