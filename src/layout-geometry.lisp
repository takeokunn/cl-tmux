(in-package #:cl-tmux/model)

;;; ── Tree geometry: rectangle assignment ────────────────────────────────────

;;; ── define-orient-dispatch — symmetric :h/:v dispatch table ────────────────
;;;
;;; Many helpers in this file have two symmetric ecase branches — one for :h
;;; (columns) and one for :v (rows).  define-orient-dispatch captures both
;;; branches as a Prolog-like fact table so each orient choice is declared
;;; exactly once and the two branches are visually aligned.
;;;
;;; Pattern (Prolog analogy):
;;;   orient_case(:h, H-form).
;;;   orient_case(:v, V-form).
;;;
;;; Expands to: (ecase ORIENT-VAR (:h H-FORM) (:v V-FORM))

(defmacro orient-case (orient-var &key h v)
  "Dispatch on ORIENT-VAR (:h or :v), evaluating H or V respectively.
   A concise replacement for repeated (ecase orient (:h ...) (:v ...))."
  `(ecase ,orient-var
     (:h ,h)
     (:v ,v)))

(defun %assign-split (node x y w h)
  "Assign rectangles to the two children of layout-split NODE.
   The :h and :v cases are symmetric: :h divides WIDTH, :v divides HEIGHT."
  (let* ((orient          (layout-split-orientation node))
         (ratio           (layout-split-ratio node))
         ;; :h splits divide the width (cols); :v splits divide the height (rows).
         (available-cells (1- (orient-case orient :h w :v h)))
         (first-extent    (max 1 (min (1- available-cells) (round (* available-cells ratio)))))
         (second-extent   (- available-cells first-extent)))
    (orient-case orient
      :h (progn
           (layout-assign (layout-split-first  node)  x                    y  first-extent h)
           (layout-assign (layout-split-second node) (+ x first-extent 1)  y  second-extent h))
      :v (progn
           (layout-assign (layout-split-first  node)  x  y                    w  first-extent)
           (layout-assign (layout-split-second node)  x (+ y first-extent 1)  w  second-extent)))))

(defun layout-assign (node x y w h)
  "Walk NODE, repositioning every leaf's pane to fill the X,Y,W,H rectangle.
   Reserves one row/column for the separator at each internal split node."
  (etypecase node
    (layout-leaf  (pane-reposition (layout-leaf-pane node) x y (max 1 w) (max 1 h)))
    (layout-split (%assign-split node x y w h))))

;;; ── Pane neighbor lookup and hit testing — see window-neighbor.lisp ─────────
;;;
;;; pane-neighbor, pane-at-position, and their helpers (%ranges-overlap-p,
;;; %pane-center-x/y) live in window-neighbor.lisp because they access WINDOW
;;; struct slots (window-panes), which are defined in window.lisp.
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

(defun resize-find-split (tree leaf orient)
  "Climb from LEAF toward the root of TREE; return (values SPLIT SIDE) for the
   nearest ancestor LAYOUT-SPLIT whose orientation is ORIENT, where SIDE
   (:first/:second) is the branch LEAF descends from.  NIL when none exists."
  (labels ((climb (node)
             (multiple-value-bind (parent which) (layout-find-parent tree node)
               (cond ((null parent) (values nil nil))
                     ((eq (layout-split-orientation parent) orient)
                      (values parent which))
                     (t (climb parent))))))
    (climb leaf)))

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
    :v (let* ((avail (- (pane-height pane) 1))
              (fh    (floor avail 2)))
         (values (pane-x pane) (+ (pane-y pane) fh 1)
                 (pane-width pane) (- avail fh)))
    :h (let* ((avail (- (pane-width pane) 1))
              (fw    (floor avail 2)))
         (values (+ (pane-x pane) fw 1) (pane-y pane)
                 (- avail fw) (pane-height pane)))))


