(in-package #:cl-tmux/model)

;;;; Named layout application — reposition all panes in a window.
;;;;
;;;; Lives here (not layout.lisp) because apply-named-layout accesses WINDOW
;;;; struct slots.  %build-flat-tree (pure tree construction) lives in layout.lisp
;;;; and is called from here.  Loads after window.lisp in the system definition.

;;; ── Named layouts ───────────────────────────────────────────────────────────
;;;
;;; Five standard tmux named layouts expressed as a Prolog-like fact table.
;;; Each define-named-layout-rules clause is one layout rule:
;;;
;;;   layout(even_horizontal, Window) :- n_equal_columns(Window).
;;;   layout(even_vertical,   Window) :- n_equal_rows(Window).
;;;   layout(main_horizontal, Window) :- main_top_rest_bottom_equal(Window).
;;;   layout(main_vertical,   Window) :- main_left_rest_right_stacked(Window).
;;;   layout(tiled,           Window) :- near_square_grid(Window).
;;;
;;; After repositioning all panes, layout-assign syncs every rectangle
;;; through the rebuilt split tree.

;;; ── define-named-layout-rules — declarative layout dispatch table ────────────
;;;
;;; Analogous to define-csi-rules / define-command-handlers: each clause names
;;; a layout keyword and the helper function that implements it.  apply-named-layout
;;; dispatches via ecase generated from this table.
;;;
;;; Pattern (Prolog analogy):
;;;   layout_rule(:even-horizontal) :- %layout-even-h.
;;;   layout_rule(:even-vertical)   :- %layout-even-v.
;;;   ...

(defmacro define-named-layout-rules (&rest rules)
  "Build apply-named-layout from a declarative fact table of layout rules.
   Each RULE is (layout-keyword function-call-form).
   Generates an ecase dispatch that calls the appropriate helper."
  `(defun apply-named-layout (window layout-name)
     "Reposition all panes in WINDOW according to LAYOUT-NAME, one of:
        :even-horizontal  :even-vertical  :main-horizontal
        :main-vertical    :tiled
      Rebuilds the window tree and calls layout-assign to sync all rectangles."
     (let* ((panes (window-panes window))
            (n     (length panes))
            (w     (window-width  window))
            (h     (window-height window)))
       (when (zerop n) (return-from apply-named-layout nil))
       (ecase layout-name
         ,@(mapcar (lambda (rule)
                     (destructuring-bind (keyword call-form) rule
                       `(,keyword ,call-form)))
                   rules)))))

;;; ── %layout-even-h / %layout-even-v ────────────────────────────────────────
;;;
;;; Note: pane-reposition is NOT called in a preliminary loop here.
;;; layout-assign (called at the end) already traverses the entire tree and
;;; repositions every leaf's pane to the correct rectangle, so a pre-loop
;;; was redundant and fragile (side-effect order dependency).

(defun %layout-even-h (window panes n w h)
  "Apply the :even-horizontal layout: N equal-width columns separated by 1-col borders."
  (setf (window-tree window) (%build-flat-tree panes :h))
  (layout-assign (window-tree window) 0 0 w h))

(defun %layout-even-v (window panes n w h)
  "Apply the :even-vertical layout: N equal-height rows separated by 1-row borders."
  (setf (window-tree window) (%build-flat-tree panes :v))
  (layout-assign (window-tree window) 0 0 w h))

;;; ── %layout-main — unified main-horizontal / main-vertical ──────────────────
;;;
;;; main-horizontal: first pane top half, rest split bottom half equally (outer :v, inner :h).
;;; main-vertical:   first pane left half, rest split right half equally stacked (outer :h, inner :v).
;;; The two variants are structurally symmetric: outer-orient / inner-orient swap.
;;;
;;; A single %layout-main function with an orient argument unifies the two,
;;; making the symmetry explicit and eliminating 18 nearly-identical lines.

(defun %layout-main (window panes w h outer-orient inner-orient)
  "Apply a main-axis layout: the first pane takes half the OUTER-ORIENT extent;
   the rest are evenly distributed along INNER-ORIENT in the remaining half.
   OUTER-ORIENT :v → main-horizontal; OUTER-ORIENT :h → main-vertical."
  (let* ((rest-panes (rest panes))
         (tree       (if rest-panes
                         (make-layout-split outer-orient
                                            (make-layout-leaf (first panes))
                                            (%build-flat-tree rest-panes inner-orient))
                         (make-layout-leaf (first panes)))))
    (setf (window-tree window) tree)
    (layout-assign (window-tree window) 0 0 w h)))

;;; ── %build-grid-tree ────────────────────────────────────────────────────────
;;;
;;; Extracted from the body of %layout-tiled.  Builds a right-leaning vertical
;;; chain of horizontal rows: the "near-square grid" binary tree encoding.

(defun %build-grid-tree (row-pane-groups)
  "Build a binary layout tree for a near-square grid from ROW-PANE-GROUPS.
   Each group is a list of panes in one row (left to right).
   Returns a right-leaning chain of :v splits, one horizontal row per node."
  (if (null (rest row-pane-groups))
      (%build-flat-tree (first row-pane-groups) :h)
      (make-layout-split :v
        (%build-flat-tree (first row-pane-groups) :h)
        (%build-grid-tree (rest row-pane-groups)))))

(defun %layout-tiled (window panes n w h)
  "Apply the :tiled layout: near-square grid, cols = ceil(sqrt n), rows = ceil(n / cols)."
  (let* ((cols  (ceiling (sqrt n)))
         (rows  (ceiling n cols))
         (row-pane-groups
          (loop for row-index from 0 below rows
                collect (let ((start (* row-index cols))
                              (end   (min (* (1+ row-index) cols) n)))
                          (subseq panes start end)))))
    (setf (window-tree window) (%build-grid-tree row-pane-groups))
    (layout-assign (window-tree window) 0 0 w h)))

;;; ── apply-named-layout — declarative fact table ──────────────────────────────
;;;
;;; One clause per layout:
;;;   layout_rule(:even-horizontal) :- %layout-even-h(window, panes, n, w, h)
;;;   layout_rule(:even-vertical)   :- %layout-even-v(window, panes, n, w, h)
;;;   layout_rule(:main-horizontal) :- %layout-main(window, panes, w, h, :v, :h)
;;;   layout_rule(:main-vertical)   :- %layout-main(window, panes, w, h, :h, :v)
;;;   layout_rule(:tiled)           :- %layout-tiled(window, panes, n, w, h)

(define-named-layout-rules
  (:even-horizontal (%layout-even-h window panes n w h))
  (:even-vertical   (%layout-even-v window panes n w h))
  (:main-horizontal (%layout-main   window panes w h :v :h))
  (:main-vertical   (%layout-main   window panes w h :h :v))
  (:tiled           (%layout-tiled  window panes n w h)))
