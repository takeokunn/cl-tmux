(in-package #:cl-tmux)

;;; Layout hit-testing is pure pane geometry until a resize is explicitly
;;; applied.  The fact table keeps horizontal and vertical border rules aligned.

(defmacro define-border-hit-predicates (&rest specs)
  "Generate border-hit predicates from declarative geometry specs."
  `(progn
     ,@(mapcar (lambda (spec)
                 (destructuring-bind (name doc sep-key lo-key hi-key sep-coord span-coord) spec
                   `(defun ,name (first-leaves all-leaves col row)
                      ,doc
                      (let* ((sep (reduce #'max first-leaves :key ,sep-key))
                             (lo  (reduce #'min all-leaves   :key ,lo-key))
                             (hi  (reduce #'max all-leaves   :key ,hi-key)))
                        (and (= ,sep-coord sep) (<= lo ,span-coord) (< ,span-coord hi))))))
               specs)))

(define-border-hit-predicates
  (%h-border-hit-p
   "T when (COL, ROW) lands on the vertical separator of a :h split."
   (lambda (p) (+ (pane-x p) (pane-width p))) #'pane-y
   (lambda (p) (+ (pane-y p) (pane-height p)))
   col row)
  (%v-border-hit-p
   "T when (COL, ROW) lands on the horizontal separator of a :v split."
   (lambda (p) (+ (pane-y p) (pane-height p))) #'pane-x
   (lambda (p) (+ (pane-x p) (pane-width p)))
   row col))

(defun %border-check-node (col row node)
  "Return (values split orientation) when (COL,ROW) is on NODE's border."
  (etypecase node
    (layout-leaf (values nil nil))
    (layout-split
     (multiple-value-bind (split1 orientation1)
         (%border-check-node col row (layout-split-first node))
       (if split1
           (values split1 orientation1)
           (multiple-value-bind (split2 orientation2)
               (%border-check-node col row (layout-split-second node))
             (if split2
                 (values split2 orientation2)
                 (let* ((orient (layout-split-orientation node))
                        (first-leaves (layout-leaves (layout-split-first node)))
                        (all-leaves (layout-leaves node)))
                   (ecase orient
                     (:h (if (%h-border-hit-p first-leaves all-leaves col row)
                             (values node :h)
                             (values nil nil)))
                     (:v (if (%v-border-hit-p first-leaves all-leaves col row)
                             (values node :v)
                             (values nil nil))))))))))))

(defun %border-at-position (window col row)
  "Return (values layout-split orientation) when (COL,ROW) is on a border."
  (let ((tree (window-tree window)))
    (if tree
        (%border-check-node col row tree)
        (values nil nil))))

(defun %compute-split-ratio (all-panes split orientation pointer origin-key)
  "Compute the clamped layout-split ratio for a drag pointer."
  (let* ((origin (reduce #'min all-panes :key origin-key))
         (total (layout-split-axis-extent split orientation))
         (new-first (max 1 (min (1- total) (- pointer origin)))))
    (/ new-first (float (1- total)))))

(defun %apply-drag-resize (window split orientation col row)
  "Adjust SPLIT's ratio so the separator tracks (COL,ROW) within WINDOW."
  (let ((all-panes (layout-leaves split)))
    (setf (layout-split-ratio split)
          (ecase orientation
            (:h (%compute-split-ratio all-panes split :h col #'pane-x))
            (:v (%compute-split-ratio all-panes split :v row #'pane-y)))))
  (when (window-tree window)
    (%assign-window-tree window (window-width window) (window-height window))))
