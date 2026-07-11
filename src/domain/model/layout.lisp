(in-package #:cl-tmux/model)

;;; ── Layout tree ────────────────────────────────────────────────────────────
;;;
;;; A window's geometry is a BINARY SPLIT TREE.  Every leaf wraps exactly one
;;; pane; every internal node splits its rectangle into two children along one
;;; axis at a fractional ratio.  This lets a split halve ONLY the active pane's
;;; rectangle and supports arbitrary nested/mixed layouts (a pane split top/
;;; bottom, one half then split left/right, …), matching real tmux.
;;;
;;; Orientations use tmux's -v/-h naming so the keywords are not inverted:
;;;   :v  — top/bottom split  (children stacked vertically;  tmux split-window -v / C-b ")
;;;   :h  — left/right split   (children side by side;        tmux split-window -h / C-b %)

(defconstant +pane-min-width+  2
  "Smallest interior width (columns) a pane may occupy.")
(defconstant +pane-min-height+ 1
  "Smallest interior height (rows) a pane may occupy.")

(defstruct (layout-leaf (:constructor make-layout-leaf (pane)))
  "Tree leaf: owns one PANE."
  pane)

(defstruct (layout-split (:constructor make-layout-split (orientation first second
                                                          &optional (ratio 1/2))))
  "Internal node: split ORIENTATION (:v top/bottom, :h left/right) between two
   children FIRST and SECOND, giving FIRST the fraction RATIO of the split axis."
  orientation
  first
  second
  (ratio 1/2))

;;; ── Prolog-like tree visitor macro ─────────────────────────────────────────
;;;
;;; (define-layout-visitor NAME (NODE) null-form leaf-form split-form)
;;; Each form is a Prolog-like clause:
;;;   null-form  → process(nil)
;;;   leaf-form  → process(leaf(pane))      — PANE is bound
;;;   split-form → process(split(o,f,s,r))  — FIRST, SECOND, ORIENT, RATIO are bound
;;; Replaces the repeated (etypecase node (null ..) (layout-leaf ..) (layout-split ..))
;;; pattern with a single declarative definition.

;;; ── Layout tree visitor macros ─────────────────────────────────────────────
;;;
;;; Two symmetric macros cover the two visitor shapes:
;;;   define-layout-visitor  — NODE only (1-arg traversal)
;;;   define-layout-fold     — NODE + extra args (2+-arg traversal)
;;;
;;; Prolog analogy:
;;;   traverse(nil)          :- null_case.
;;;   traverse(leaf(Pane))   :- leaf_case(Pane).
;;;   traverse(split(O,F,S)) :- split_case(O, F, S).

(defmacro define-layout-visitor (name (node-var) &key on-null on-leaf on-split
                                                       docstring)
  "Define a single-argument recursive layout-tree visitor NAME.
   ON-NULL  — form evaluated when NODE is NIL.
   ON-LEAF  — form with PANE bound to (layout-leaf-pane node).
   ON-SPLIT — form with FIRST, SECOND, ORIENT, RATIO bound."
  `(defun ,name (,node-var)
     ,@(when docstring (list docstring))
     (etypecase ,node-var
       (null ,on-null)
       (layout-leaf
        (let ((pane (layout-leaf-pane ,node-var)))
          (declare (ignorable pane))
          ,on-leaf))
       (layout-split
        (let ((first  (layout-split-first       ,node-var))
              (second (layout-split-second      ,node-var))
              (orient (layout-split-orientation ,node-var))
              (ratio  (layout-split-ratio       ,node-var)))
          (declare (ignorable first second orient ratio))
          ,on-split)))))

(define-layout-visitor layout-leaves (node)
  :docstring "Collect every pane in NODE's subtree, left/top-to-right/bottom."
  :on-null  nil
  :on-leaf  (list pane)
  :on-split (append (layout-leaves first) (layout-leaves second)))

;;; ── define-layout-fold — multi-argument traversal ────────────────────────────
;;;
;;; Like define-layout-visitor but supports extra arguments (beyond NODE).
;;; Bindings in each clause use prefixed names to avoid shadowing caller args:
;;;   leaf-pane   = (layout-leaf-pane node)
;;;   split-first / split-second / split-orient / split-ratio

(defmacro define-layout-fold (name (node-var &rest extra-vars) &key on-null on-leaf on-split docstring)
  "Define a multi-argument recursive layout-tree function NAME.
   NODE-VAR is the tree node; EXTRA-VARS are additional parameters.
   ON-LEAF has LEAF-PANE bound; ON-SPLIT has SPLIT-FIRST, SPLIT-SECOND,
   SPLIT-ORIENT, SPLIT-RATIO bound."
  `(defun ,name (,node-var ,@extra-vars)
     ,@(when docstring (list docstring))
     (etypecase ,node-var
       (null ,on-null)
       (layout-leaf
        (let ((leaf-pane (layout-leaf-pane ,node-var)))
          (declare (ignorable leaf-pane))
          ,on-leaf))
       (layout-split
        (let ((split-first  (layout-split-first       ,node-var))
              (split-second (layout-split-second      ,node-var))
              (split-orient (layout-split-orientation ,node-var))
              (split-ratio  (layout-split-ratio       ,node-var)))
          (declare (ignorable split-first split-second split-orient split-ratio))
          ,on-split)))))

(define-layout-fold layout-find-leaf (node pane)
  :docstring "Return the LAYOUT-LEAF in NODE that holds PANE, or NIL."
  :on-null  nil
  :on-leaf  (when (eq leaf-pane pane) node)
  :on-split (or (layout-find-leaf split-first  pane)
                (layout-find-leaf split-second pane)))

(defun %direct-child-side (split child)
  "If CHILD is a direct child of SPLIT, return (values SPLIT :first or :second).
   Returns (values NIL NIL) when CHILD is not a direct child of SPLIT."
  (cond ((eq (layout-split-first  split) child) (values split :first))
        ((eq (layout-split-second split) child) (values split :second))
        (t (values nil nil))))

(defun layout-find-parent (node child)
  "Return (values PARENT WHICH) for CHILD's immediate parent LAYOUT-SPLIT,
   where WHICH is :first or :second.  Returns (values NIL NIL) when not found."
  (when (layout-split-p node)
    ;; Check direct children.  Note: OR cannot be used here — it only propagates
    ;; the primary value, discarding the secondary :first/:second.
    (multiple-value-bind (p s) (%direct-child-side node child)
      (if p
          (values p s)
          (multiple-value-bind (p2 s2) (layout-find-parent (layout-split-first node) child)
            (if p2 (values p2 s2)
                (layout-find-parent (layout-split-second node) child)))))))

;;; ── orient-case: concise :h/:v dispatch ────────────────────────────────────
;;;
;;; Defined here (layout.lisp, the earliest-loading layout file) so that every
;;; later file — layout-geometry.lisp, window-core.lisp, window-tree.lisp, window-layout.lisp —
;;; can use it without forward-reference issues.
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
;;; ── Tree geometry: assign rectangles ───────────────────────────────────────

;;; ── %axis-floor: pure data lookup ───────────────────────────────────────────
;;;
;;; A Prolog-like fact:
;;;   axis_floor(:v) :- +pane-min-height+.
;;;   axis_floor(:h) :- +pane-min-width+.

(defun %axis-floor (orient)
  "Minimum pane extent (cells) along ORIENT's split axis: rows for :v, cols for :h."
  (orient-case orient :h +pane-min-width+ :v +pane-min-height+))

(define-layout-fold layout-min-extent (node orient)
  :docstring "Minimum cells NODE requires along ORIENT's axis (:v → rows, :h → cols),
   including 1-cell separators at same-axis internal nodes."
  :on-null  0
  :on-leaf  (%axis-floor orient)
  :on-split (let ((first-extent  (layout-min-extent split-first  orient))
                  (second-extent (layout-min-extent split-second orient)))
               (if (eq split-orient orient)
                   (+ first-extent 1 second-extent) ; same-axis split: stack + 1-cell separator
                   (max first-extent second-extent))))

;;; ── Named layouts (tree builder only) ───────────────────────────────────────
;;;
;;; %build-flat-tree is a pure tree-construction helper that only needs
;;; layout types (make-layout-leaf, make-layout-split), so it belongs here.
;;; apply-named-layout uses WINDOW struct accessors so it lives in window-core.lisp
;;; (which loads after layout.lisp, avoiding a forward reference).

(defun %build-flat-tree (panes orientation)
  "Build a right-leaning binary split chain from PANES using ORIENTATION.
   Single pane: return a layout-leaf.  Two or more: first pane is the
   left/top leaf; the rest recurse as the right/bottom subtree."
  (if (null (rest panes))
      (make-layout-leaf (first panes))
      (make-layout-split orientation
                         (make-layout-leaf (first panes))
                         (%build-flat-tree (rest panes) orientation))))

;;; Layout persistence (serialization) lives in layout-persistence.lisp,
;;; which is loaded immediately after this file.  That file defines:
;;;   %layout-checksum, layout-node-bounding-box, %node->string, layout->string.
