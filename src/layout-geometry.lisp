(in-package #:cl-tmux/model)

;;; ── Tree geometry: rectangle assignment ────────────────────────────────────

(defun %assign-split (node x y w h)
  "Assign rectangles to the two children of layout-split NODE.
   The :h and :v cases are symmetric: :h divides WIDTH, :v divides HEIGHT."
  (let* ((orient (layout-split-orientation node))
         (ratio  (layout-split-ratio node))
         ;; :h splits divide the width (cols); :v splits divide the height (rows).
         (avail  (1- (ecase orient (:h w) (:v h))))
         (fext   (max 1 (min (1- avail) (round (* avail ratio)))))
         (sext   (- avail fext)))
    (ecase orient
      (:h (layout-assign (layout-split-first  node)  x           y  fext h)
          (layout-assign (layout-split-second node) (+ x fext 1) y  sext h))
      (:v (layout-assign (layout-split-first  node)  x  y           w  fext)
          (layout-assign (layout-split-second node)  x (+ y fext 1) w  sext)))))

(defun layout-assign (node x y w h)
  "Walk NODE, repositioning every leaf's pane to fill the X,Y,W,H rectangle.
   Reserves one row/column for the separator at each internal split node."
  (etypecase node
    (layout-leaf  (pane-reposition (layout-leaf-pane node) x y (max 1 w) (max 1 h)))
    (layout-split (%assign-split node x y w h))))

;;; ── Pane neighbor lookup — see window.lisp ──────────────────────────────────
;;;
;;; pane-neighbor and its helpers (%ranges-overlap-p, %pane-center-x/y) live in
;;; window.lisp because they access WINDOW struct slots (window-panes).
;;; Defining them here would forward-reference the WINDOW struct (loaded later).

;;; ── Resize helpers ─────────────────────────────────────────────────────────

(defun layout-split-axis-extent (split orient)
  "Span of SPLIT's bounding rectangle along ORIENT's axis (:v → rows, :h → cols),
   derived from its already-laid-out leaves.  This is the SPLIT's own extent, so
   ratio arithmetic is correct even for a deeply nested split that occupies only
   a sub-rectangle of the window."
  (let ((panes (layout-leaves split)))
    (ecase orient
      ;; :v → measure rows: max(y + height) - min(y)
      (:v (- (reduce #'max panes :key (lambda (p) (+ (pane-y p) (pane-height p))))
             (reduce #'min panes :key #'pane-y)))
      ;; :h → measure cols: max(x + width) - min(x)
      (:h (- (reduce #'max panes :key (lambda (p) (+ (pane-x p) (pane-width p))))
             (reduce #'min panes :key #'pane-x))))))

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
  (ecase orient
    (:v (let* ((avail (- (pane-height pane) 1))
               (fh    (floor avail 2)))
          (values (pane-x pane) (+ (pane-y pane) fh 1)
                  (pane-width pane) (- avail fh))))
    (:h (let* ((avail (- (pane-width pane) 1))
               (fw    (floor avail 2)))
          (values (+ (pane-x pane) fw 1) (pane-y pane)
                  (- avail fw) (pane-height pane))))))

(defun pane-at-position (window col row)
  "Return the pane in WINDOW that contains column COL and row ROW (0-based screen coordinates).
   Returns NIL when no pane contains the position."
  (find-if (lambda (p)
             (and (<= (pane-x p) col) (< col (+ (pane-x p) (pane-width p)))
                  (<= (pane-y p) row) (< row (+ (pane-y p) (pane-height p)))))
           (window-panes window)))

