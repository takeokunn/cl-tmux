(in-package #:cl-tmux/model)

;;; ── Tree geometry: rectangle assignment ────────────────────────────────────

;;; orient-case is defined in layout.lisp (which loads before this file).
;;; It dispatches on :h/:v and is used extensively below.

(defun %assign-split (node x y width height)
  "Assign rectangles to the two children of layout-split NODE.
   The :h and :v cases are symmetric: :h divides WIDTH, :v divides HEIGHT."
  (let* ((orient          (layout-split-orientation node))
         (ratio           (layout-split-ratio node))
         ;; :h splits divide the width (cols); :v splits divide the height (rows).
         (available-cells (1- (orient-case orient :h width :v height)))
         (first-extent    (max 1 (min (1- available-cells) (round (* available-cells ratio)))))
         (second-extent   (- available-cells first-extent)))
    (orient-case orient
      :h (progn
           (layout-assign (layout-split-first  node)  x                      y  first-extent  height)
           (layout-assign (layout-split-second node) (+ x first-extent 1)    y  second-extent height))
      :v (progn
           (layout-assign (layout-split-first  node)  x  y                    width  first-extent)
           (layout-assign (layout-split-second node)  x (+ y first-extent 1)  width  second-extent)))))

(defun layout-assign (node x y width height)
  "Walk NODE, updating every leaf's pane geometry to fit the X,Y,WIDTH,HEIGHT rectangle.
   Reserves one row/column for the separator at each internal split node.
   This is an ORCHESTRATE-layer function: it calls %update-pane-geometry (a DATA
   slot mutation in pane-geometry.lisp) on every leaf, so callers such as window-relayout
   can drive the PTY/screen resize as a separate step after the full tree has been
   repositioned.  It is NOT a pure transform — it mutates pane slots in place."
  (etypecase node
    (layout-leaf  (%update-pane-geometry (layout-leaf-pane node) x y (max 1 width) (max 1 height)))
    (layout-split (%assign-split node x y width height))))

;;; ── Pane neighbor lookup and hit testing — see window-neighbor.lisp ─────────
;;;
;;; pane-neighbor, pane-at-position, and their helpers (%ranges-overlap-p,
;;; %pane-center-x/y) live in window-neighbor.lisp because they access WINDOW
;;; struct slots (window-panes), which are defined in window-core.lisp.
;;; Defining them here would forward-reference the WINDOW struct (loaded later).

;;; ── Resize helpers ─────────────────────────────────────────────────────────

(defun layout-split-axis-extent (split orient)
  "Span of SPLIT's bounding rectangle along ORIENT's axis (:v → rows, :h → cols),
   derived from its already-laid-out leaves.  This is the SPLIT's own extent, so
   ratio arithmetic is correct even for a deeply nested split that occupies only
   a sub-rectangle of the window."
  (let ((panes (layout-leaves split)))
    (orient-case orient
      ;; :v → measure rows: max(y + height) - min(y)
      :v (- (reduce #'max panes :key (lambda (p) (+ (pane-y p) (pane-height p))))
            (reduce #'min panes :key #'pane-y))
      ;; :h → measure cols: max(x + width) - min(x)
      :h (- (reduce #'max panes :key (lambda (p) (+ (pane-x p) (pane-width p))))
            (reduce #'min panes :key #'pane-x)))))

(defun %resize-find-split-climb (tree node orient)
  "Climb from NODE toward TREE's root until an ORIENT split is found."
  (multiple-value-bind (parent which) (layout-find-parent tree node)
    (cond ((null parent) (values nil nil))
          ((eq (layout-split-orientation parent) orient)
           (values parent which))
          (t (%resize-find-split-climb tree parent orient)))))

(defun resize-find-split (tree leaf orient)
  "Climb from LEAF toward the root of TREE; return (values SPLIT SIDE) for the
   nearest ancestor LAYOUT-SPLIT whose orientation is ORIENT, where SIDE
   (:first/:second) is the branch LEAF descends from.  NIL when none exists."
  (%resize-find-split-climb tree leaf orient))

(defun resize-direction-orientation (direction)
  "Tree split orientation a resize DIRECTION acts on:
   :left/:right move an :h (left/right) border; :up/:down move a :v one."
  (ecase direction
    ((:left :right) :h)
    ((:up   :down)  :v)))

(defun split-child-geometry (pane orient)
  "Provisional rectangle for the NEW child when PANE is split along ORIENT.
   The exact geometry is fixed by the subsequent WINDOW-RELAYOUT; this only
   seeds the new pane/screen with a sensible size."
  (orient-case orient
    :v (let* ((available-rows (- (pane-height pane) 1))
              (first-rows     (floor available-rows 2)))
         (values (pane-x pane) (+ (pane-y pane) first-rows 1)
                 (pane-width pane) (- available-rows first-rows)))
    :h (let* ((available-cols (- (pane-width pane) 1))
              (first-cols     (floor available-cols 2)))
         (values (+ (pane-x pane) first-cols 1) (pane-y pane)
                 (- available-cols first-cols) (pane-height pane)))))
