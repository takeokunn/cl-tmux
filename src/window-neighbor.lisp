(in-package #:cl-tmux/model)

;;;; Directional pane navigation — find the pane adjacent to a given pane.
;;;;
;;;; Lives in its own file (not layout-geometry.lisp) because it accesses
;;;; WINDOW struct slots (window-panes), which are defined in window.lisp.
;;;; Loads after window.lisp in the system definition.

;;; ── Pane neighbor lookup ─────────────────────────────────────────────────────
;;;
;;; Prolog analogy (ordered clauses):
;;;   neighbor(right, Pane, Cands) :- edge_touching_right(Pane, Cands).
;;;   neighbor(left,  Pane, Cands) :- edge_touching_left(Pane, Cands).
;;;   neighbor(down,  Pane, Cands) :- edge_touching_below(Pane, Cands).
;;;   neighbor(up,    Pane, Cands) :- edge_touching_above(Pane, Cands).
;;; Among candidates: pick the one whose center is closest perpendicularly.

(defun %ranges-overlap-p (start1 len1 start2 len2)
  "T when [START1, START1+LEN1) and [START2, START2+LEN2) share at least one integer."
  (and (< start1 (+ start2 len2))
       (< start2 (+ start1 len1))))

(defun %pane-center-x (pane) (+ (pane-x pane) (ash (pane-width  pane) -1)))
(defun %pane-center-y (pane) (+ (pane-y pane) (ash (pane-height pane) -1)))

;;; ── Neighbor filter table (data layer) ──────────────────────────────────────
;;;
;;; define-neighbor-finders generates an alist of (direction . predicate-fn)
;;; from a Prolog-like fact table.  Each fact specifies:
;;;   - the edge expression (positive = touching on that side)
;;;   - the overlap range along the perpendicular axis

(defmacro define-neighbor-finders (&rest specs)
  "Build the per-direction candidate-filter lambdas from a Prolog-like fact table.
   Each SPEC is (direction edge-expr overlap-start1 overlap-len1 overlap-start2 overlap-len2).
   Returns an alist of (direction . (lambda (p pane) ...))."
  `(list
     ,@(mapcar
        (lambda (spec)
          (destructuring-bind (dir edge-expr os1 ol1 os2 ol2) spec
            `(cons ,dir (lambda (p pane)
                          (and (<= (abs ,edge-expr) 2)
                               (%ranges-overlap-p ,os1 ,ol1 ,os2 ,ol2))))))
        specs)))

;;; Direction adjacency facts — one row per direction (Prolog-like):
;;;   right_neighbor(P, Pane) :- P.x ≈ Pane.x + Pane.w, y-ranges overlap.
;;;   left_neighbor(P, Pane)  :- P.x + P.w ≈ Pane.x,    y-ranges overlap.
;;;   down_neighbor(P, Pane)  :- P.y ≈ Pane.y + Pane.h,  x-ranges overlap.
;;;   up_neighbor(P, Pane)    :- P.y + P.h ≈ Pane.y,     x-ranges overlap.

(defparameter *neighbor-filters*
  (define-neighbor-finders
    (:right (- (pane-x p) (+ (pane-x pane) (pane-width  pane)))
            (pane-y pane) (pane-height pane) (pane-y p) (pane-height p))
    (:left  (- (+ (pane-x p) (pane-width p)) (pane-x pane))
            (pane-y pane) (pane-height pane) (pane-y p) (pane-height p))
    (:down  (- (pane-y p) (+ (pane-y pane) (pane-height pane)))
            (pane-x pane) (pane-width pane)  (pane-x p) (pane-width  p))
    (:up    (- (+ (pane-y p) (pane-height p)) (pane-y pane))
            (pane-x pane) (pane-width pane)  (pane-x p) (pane-width  p)))
  "Alist of (direction . filter-fn) built by define-neighbor-finders.
   defparameter (not defconstant) because the value contains lambdas,
   which are not EQL-comparable across compilations as required by defconstant.")

;;; ── Public: pane-neighbor (logic layer) ─────────────────────────────────────

(defun pane-neighbor (window pane direction)
  "Return the pane adjacent to PANE in DIRECTION (:left :right :up :down), or NIL.
   Among edge-touching candidates, returns the one whose center is closest to
   PANE's center along the perpendicular axis."
  (let* ((filter     (cdr (assoc direction *neighbor-filters*)))
         (candidates (remove pane (window-panes window)))
         (matching   (remove-if-not (lambda (p) (funcall filter p pane)) candidates)))
    (when matching
      (ecase direction
        ((:left :right)
         (reduce (lambda (a b)
                   (if (<= (abs (- (%pane-center-y a) (%pane-center-y pane)))
                           (abs (- (%pane-center-y b) (%pane-center-y pane))))
                       a b))
                 matching))
        ((:up :down)
         (reduce (lambda (a b)
                   (if (<= (abs (- (%pane-center-x a) (%pane-center-x pane)))
                           (abs (- (%pane-center-x b) (%pane-center-x pane))))
                       a b))
                 matching))))))
