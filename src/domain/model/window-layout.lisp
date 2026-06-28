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
   Generates an ecase dispatch that calls the appropriate helper.

   IMPLICIT FREE VARIABLES: the generated ecase body is evaluated in the
   lexical scope of apply-named-layout's let*, so every RULE's call-form
   may freely reference 'window', 'panes', 'n', 'w', and 'h' by name.
   This is intentional: the macro acts as a code template, not a closure,
   and the Prolog-fact reading of each rule relies on those names being in
   scope at the call site."
  `(defun apply-named-layout (window layout-name
                              &optional (main-pane-width 80) (main-pane-height 24)
                                        (other-pane-width 0) (other-pane-height 0))
     "Reposition all panes in WINDOW according to LAYOUT-NAME, one of:
        :even-horizontal  :even-vertical  :main-horizontal
        :main-vertical    :tiled
      MAIN-PANE-WIDTH / MAIN-PANE-HEIGHT size the main (first) pane in the
      main-vertical / main-horizontal layouts (tmux's main-pane-width/-height
      options); OTHER-PANE-WIDTH / OTHER-PANE-HEIGHT (other-pane-*; 0 = unset)
      override them when set and fitting.  The option values are read by the
      cl-tmux-layer caller and passed in as plain integers, since this model-layer
      code loads before options.  Rebuilds the window tree and calls layout-assign."
     (let* ((panes (window-panes window))
            (n     (length panes))
            (w     (window-width  window))
            (h     (window-height window)))
       (declare (ignorable main-pane-width main-pane-height
                           other-pane-width other-pane-height))
       (when (plusp n)
         (ecase layout-name
           ,@(mapcar (lambda (rule)
                       (destructuring-bind (keyword call-form) rule
                         `(,keyword ,call-form)))
                     rules))))))

;;; ── %layout-even-h / %layout-even-v ────────────────────────────────────────
;;;
;;; Note: pane-reposition is NOT called in a preliminary loop here.
;;; layout-assign (called at the end) already traverses the entire tree and
;;; repositions every leaf's pane to the correct rectangle, so a pre-loop
;;; was redundant and fragile (side-effect order dependency).

(defun %layout-even-h (window panes w h)
  "Apply the :even-horizontal layout: equal-width columns separated by 1-col borders.
   The pane count is derived from PANES; no separate count parameter is needed."
  (setf (window-tree window) (%build-flat-tree panes :h))
  (%assign-window-tree window w h))

(defun %layout-even-v (window panes w h)
  "Apply the :even-vertical layout: equal-height rows separated by 1-row borders.
   The pane count is derived from PANES; no separate count parameter is needed."
  (setf (window-tree window) (%build-flat-tree panes :v))
  (%assign-window-tree window w h))

;;; ── %layout-main — unified main-horizontal / main-vertical ──────────────────
;;;
;;; main-horizontal: first pane top half, rest split bottom half equally (outer :v, inner :h).
;;; main-vertical:   first pane left half, rest split right half equally stacked (outer :h, inner :v).
;;; The two variants are structurally symmetric: outer-orient / inner-orient swap.
;;;
;;; A single %layout-main function with an orient argument unifies the two,
;;; making the symmetry explicit and eliminating 18 nearly-identical lines.

(defun %main-pane-extent (available main-size other-size)
  "Resolve the main pane's extent (cells) along the layout's outer axis.
   AVAILABLE is the usable extent (window extent minus the separator); MAIN-SIZE is
   the main-pane-width/-height option; OTHER-SIZE is the other-pane-width/-height
   option (0 = unset).  Mirrors tmux layout_set_main_h/_v: MAIN-SIZE normally
   dictates the main pane, but a valid non-zero OTHER-SIZE that leaves room for the
   main pane overrides it — the other region takes OTHER-SIZE and the main pane the
   rest.  When MAIN-SIZE alone leaves no room, the main pane is capped so at least
   one cell remains for the others."
  (let ((min 1))                          ; cl-tmux pane minimum (%assign-split clamps too)
    (cond
      ((>= (+ main-size min) available) (max min (- available min)))
      ((and other-size (plusp other-size)
            (<= other-size available)
            (>= (- available other-size) main-size))
       (- available other-size))
      (t main-size))))

(defun %layout-main (window panes w h outer-orient inner-orient main-size
                     &optional (other-size 0))
  "Apply a main-axis layout: the first (main) pane takes MAIN-SIZE cells along the
   OUTER-ORIENT axis; the rest are evenly distributed along INNER-ORIENT in the
   remaining space.  OUTER-ORIENT :v → main-horizontal (MAIN-SIZE = rows, from
   main-pane-height); :h → main-vertical (MAIN-SIZE = columns, from main-pane-width).
   OTHER-SIZE (other-pane-width/-height, 0 = unset) overrides MAIN-SIZE when set and
   fitting — see %main-pane-extent.  The split ratio is derived from the resolved
   main extent against the available extent (one row/column reserved for the
   separator), clamped so both regions stay > 0."
  (let* ((rest-panes (rest panes))
         (extent     (ecase outer-orient (:v h) (:h w)))
         ;; available-cells in %assign-split is (1- extent); the main pane's
         ;; first-extent = round(available * ratio), so ratio = main-extent/available.
         (available  (max 1 (1- extent)))
         (main-extent (%main-pane-extent available main-size other-size))
         (ratio      (max 1/100 (min 99/100 (/ main-extent available))))
         (tree       (if rest-panes
                         (make-layout-split outer-orient
                                            (make-layout-leaf (first panes))
                                            (%build-flat-tree rest-panes inner-orient)
                                            ratio)
                         (make-layout-leaf (first panes)))))
    (setf (window-tree window) tree)
    (%assign-window-tree window w h)))

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
    (%assign-window-tree window w h)))

;;; ── apply-named-layout — declarative fact table ──────────────────────────────
;;;
;;; One clause per layout:
;;;   layout_rule(:even-horizontal) :- %layout-even-h(window, panes, w, h)
;;;   layout_rule(:even-vertical)   :- %layout-even-v(window, panes, w, h)
;;;   layout_rule(:main-horizontal) :- %layout-main(window, panes, w, h, :v, :h)
;;;   layout_rule(:main-vertical)   :- %layout-main(window, panes, w, h, :h, :v)
;;;   layout_rule(:tiled)           :- %layout-tiled(window, panes, n, w, h)

(define-named-layout-rules
  (:even-horizontal (%layout-even-h window panes w h))
  (:even-vertical   (%layout-even-v window panes w h))
  (:main-horizontal (%layout-main   window panes w h :v :h main-pane-height other-pane-height))
  (:main-vertical   (%layout-main   window panes w h :h :v main-pane-width  other-pane-width))
  (:tiled           (%layout-tiled  window panes n w h)))
