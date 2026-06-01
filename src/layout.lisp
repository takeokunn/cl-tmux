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
  (unless (layout-split-p node) (return-from layout-find-parent (values nil nil)))
  ;; Check direct children.  Note: OR cannot be used here — it only propagates
  ;; the primary value, discarding the secondary :first/:second.
  (multiple-value-bind (p s) (%direct-child-side node child)
    (when p (return-from layout-find-parent (values p s))))
  (multiple-value-bind (p s) (layout-find-parent (layout-split-first node) child)
    (if p (values p s)
        (layout-find-parent (layout-split-second node) child))))

;;; ── Tree geometry: assign rectangles ───────────────────────────────────────

;;; ── %axis-floor: pure data lookup ───────────────────────────────────────────
;;;
;;; A Prolog-like fact:
;;;   axis_floor(:v) :- +pane-min-height+.
;;;   axis_floor(:h) :- +pane-min-width+.

(defun %axis-floor (orient)
  "Minimum pane extent (cells) along ORIENT's split axis: rows for :v, cols for :h."
  (ecase orient
    (:v +pane-min-height+)
    (:h +pane-min-width+)))

(define-layout-fold layout-min-extent (node orient)
  :docstring "Minimum cells NODE requires along ORIENT's axis (:v → rows, :h → cols),
   including 1-cell separators at same-axis internal nodes."
  :on-null  0
  :on-leaf  (%axis-floor orient)
  :on-split (let ((fe (layout-min-extent split-first  orient))
                  (se (layout-min-extent split-second orient)))
               (if (eq split-orient orient)
                   (+ fe 1 se)   ; same-axis split: stack + 1-cell separator
                   (max fe se))))

;;; ── Named layouts (tree builder only) ───────────────────────────────────────
;;;
;;; %build-flat-tree is a pure tree-construction helper that only needs
;;; layout types (make-layout-leaf, make-layout-split), so it belongs here.
;;; apply-named-layout uses WINDOW struct accessors so it lives in window.lisp
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

;;; ── Layout persistence (layout string serialization) ──────────────────────────
;;;
;;; Encode/decode the layout tree in tmux's WxH,X,Y format.
;;; Full tmux format: checksum,WxH,X,Y[node1,node2]  or  checksum,WxH,X,Y,pane-id
;;;
;;; For cl-tmux we use a simplified but compatible subset:
;;;   Leaf:  "WxH,X,Y,pane-id"
;;;   H-split: "WxH,X,Y{first,second}"
;;;   V-split: "WxH,X,Y[first,second]"
;;; The 4-hex-digit checksum prefix is computed from the string.

(defun %layout-checksum (str)
  "Compute the tmux-style 16-bit checksum of STR.
   Algorithm: rolling multiply-add on character codes.
   Returns a 4-hex-digit string."
  (let ((csum 0))
    (loop for ch across str
          do (setf csum (logand #xFFFF (+ (* csum 61) (char-code ch)))))
    (format nil "~4,'0X" csum)))

(defun %node->string (node)
  "Serialize a layout node (leaf or split) to a layout string fragment.
   Does not include the checksum prefix."
  (etypecase node
    (layout-leaf
     (let ((p (layout-leaf-pane node)))
       (format nil "~Dx~D,~D,~D,~D"
               (pane-width p) (pane-height p)
               (pane-x p) (pane-y p)
               (pane-id p))))
    (layout-split
     (let* ((first  (layout-split-first  node))
            (second (layout-split-second node))
            (orient (layout-split-orientation node))
            ;; Compute the bounding box from the leaves.
            (leaves (layout-leaves node))
            (min-x  (reduce #'min leaves :key #'pane-x))
            (min-y  (reduce #'min leaves :key #'pane-y))
            (max-rx (reduce #'max leaves :key (lambda (p) (+ (pane-x p) (pane-width p)))))
            (max-ry (reduce #'max leaves :key (lambda (p) (+ (pane-y p) (pane-height p)))))
            (w      (- max-rx min-x))
            (h      (- max-ry min-y))
            (open   (if (eq orient :v) #\[ #\{))
            (close  (if (eq orient :v) #\] #\})))
       (format nil "~Dx~D,~D,~D~C~A,~A~C"
               w h min-x min-y
               open
               (%node->string first)
               (%node->string second)
               close)))))

(defun layout->string (window)
  "Serialize WINDOW's layout tree to a tmux-format layout string with checksum.
   Returns NIL when the window has no tree."
  (let ((tree (window-tree window)))
    (unless tree (return-from layout->string nil))
    (let* ((body     (%node->string tree))
           (checksum (%layout-checksum body)))
      (format nil "~A,~A" checksum body))))

;;; ── String → layout decoder ──────────────────────────────────────────────────
;;;
;;; Parse a layout string (optionally with a leading checksum) back into
;;; a layout tree, matching existing panes by id from PANES-LIST.
;;;
;;; The encoded format produced by %node->string:
;;;   Leaf:    "WxH,X,Y,pane-id"
;;;   H-split: "WxH,X,Y{child1,child2}"
;;;   V-split: "WxH,X,Y[child1,child2]"

(defun %skip-checksum (str)
  "If STR starts with a 4-char hex checksum followed by a comma, skip it.
   Returns the remaining string."
  (if (and (>= (length str) 5)
           (char= (char str 4) #\,)
           (every (lambda (ch) (digit-char-p ch 16)) (subseq str 0 4)))
      (subseq str 5)
      str))

(defun %read-digits (str pos)
  "Read decimal digits from STR starting at POS.
   Returns (values integer end-pos) where end-pos is past the last digit."
  (let ((start pos))
    (loop while (and (< pos (length str))
                     (digit-char-p (char str pos)))
          do (incf pos))
    (values (parse-integer str :start start :end pos) pos)))

;;; %parse-node uses forward-reference to %parse-split-body.
;;; We declare it here so the compiler accepts the mutual recursion.
(declaim (ftype (function (string list fixnum) (values t fixnum)) %parse-node))

(defun %parse-split-body (str panes pos close-ch orient)
  "Parse two child nodes starting at POS, expecting CLOSE-CH (} or ]) after second.
   Returns (values split-node end-pos)."
  (multiple-value-bind (c1 p8)
      (%parse-node str panes pos)
    (let ((p9 (if (and (< p8 (length str)) (char= (char str p8) #\,))
                  (1+ p8)
                  p8)))
      (multiple-value-bind (c2 p10)
          (%parse-node str panes p9)
        (let ((p11 (if (and (< p10 (length str)) (char= (char str p10) close-ch))
                       (1+ p10)
                       p10)))
          (values (make-layout-split orient c1 c2) p11))))))

(defun %parse-node (str panes pos)
  "Parse one layout node starting at POS in STR.
   Returns (values node end-pos)."
  ;; Format: WxH,X,Y then one of: { (h-split), [ (v-split), , pane-id (leaf).
  ;; Scan past W digits and 'x'
  (let* ((xp  (or (position #\x str :start pos) (length str)))
         (c1p (or (position #\, str :start (1+ xp)) (length str)))
         (c2p (or (position #\, str :start (1+ c1p)) (length str)))
         ;; Y value ends at the first {, [, , or end of string
         (p7  (or (position-if (lambda (c) (or (char= c #\{) (char= c #\[) (char= c #\,)))
                               str :start (1+ c2p))
                  (length str))))
    (if (>= p7 (length str))
        (values nil p7)
        (let ((next (char str p7)))
          (cond
            ((char= next #\{) (%parse-split-body str panes (1+ p7) #\} :h))
            ((char= next #\[) (%parse-split-body str panes (1+ p7) #\] :v))
            ((char= next #\,)
             (multiple-value-bind (pid p8) (%read-digits str (1+ p7))
               (let ((found-pane (find pid panes :key #'pane-id)))
                 (values (when found-pane (make-layout-leaf found-pane)) p8))))
            (t (values nil p7)))))))

(defun string->layout (layout-string panes)
  "Decode LAYOUT-STRING (tmux format, checksum optional) and rebuild the layout
   tree.  PANES is a list of existing pane objects matched by pane-id.
   Returns the root layout node, or NIL on parse failure."
  (handler-case
      (let ((str (%skip-checksum layout-string)))
        (multiple-value-bind (node _end)
            (%parse-node str panes 0)
          (declare (ignore _end))
          node))
    (error () nil)))
