(in-package #:cl-tmux/model)

;;; ── Window ─────────────────────────────────────────────────────────────────

(defstruct window
  "A named collection of panes with one active (focused) pane.
   TREE is the binary split-tree layout. PANES is the derived flat list of leaves,
   kept in tree order via window-refresh-panes."
  (id   0  :type fixnum)
  (name "" :type string)
  (width  80 :type fixnum)
  (height 24 :type fixnum)
  (panes nil :type list)
  (active nil)
  (tree  nil)
  (zoom-p      nil :type boolean)  ; T when this window's active pane is zoomed
  (zoom-tree   nil)               ; saved layout tree before zooming, NIL when not zoomed
  (last-active nil)               ; previously active pane (for C-b ;)
  (last-active-time 0 :type integer)  ; universal-time when this window was last focused
  (automatic-rename-p t :type boolean) ; when T, OSC 0/2 title updates window-name
  (lock (make-lock "window") :read-only t))

(defun window-refresh-panes (window)
  "Recompute WINDOW's derived PANES list from its TREE (when present)."
  (when (window-tree window)
    (setf (window-panes window) (layout-leaves (window-tree window))))
  (window-panes window))

(defun window-active-pane (window)
  (or (window-active window)
      (first (window-panes window))))

(defun window-select-pane (window pane)
  (let ((current (window-active window)))
    (when (and current (not (eq current pane)))
      (setf (window-last-active window) current)))
  (setf (window-active window) pane)
  (setf (window-last-active-time window) (get-universal-time)))

;;; ── Orientation-aware pane extent ──────────────────────────────────────────
;;;
;;; The :v/:h naming is tmux-style:
;;;   :v split stacks children vertically → extent measured in ROWS (height)
;;;   :h split places children side-by-side → extent measured in COLS (width)
;;; Both helpers use ecase for exhaustive, readable dispatch (Prolog-style fact).

(defun %orient-pane-extent (pane orient)
  "Current extent of PANE along ORIENT's split axis."
  (ecase orient
    (:v (pane-height pane))
    (:h (pane-width  pane))))

(defun %split-fits-p (pane orient)
  "T when PANE is wide/tall enough to split along ORIENT (needs 2×min + 1 separator)."
  (let ((avail  (%orient-pane-extent pane orient))
        (floor* (%axis-floor orient)))
    (>= avail (+ floor* 1 floor*))))

;;; ── Window-level pane ID allocation ────────────────────────────────────────

(defun next-pane-id (window)
  "Smallest positive pane id not already used in WINDOW.
   Window-level concern: queries pane membership, not geometry."
  (let ((used (mapcar #'pane-id (window-panes window))))
    (loop for i from 1
          unless (member i used) return i)))

;;; ── Tree-link mutation macro ────────────────────────────────────────────────
;;;
;;; Both %replace-in-tree and %collapse-parent share the same pattern:
;;;   set_tree_link(Window, Node, New) :-
;;;     find_parent(Window.tree, Node, Parent, Side), !,
;;;     set_child(Parent, Side, New).
;;;   set_tree_link(Window, _Node, New) :-
;;;     Window.tree := New.
;;;
;;; define-tree-link-setter generates this Prolog-like dispatch once.

(defmacro %set-tree-link (window node new-value)
  "Replace NODE's position in WINDOW's tree with NEW-VALUE.
   If NODE has a parent, updates that parent's child link (preserving side);
   if NODE is the root, replaces the root directly."
  (let ((p  (gensym "PARENT"))
        (w  (gensym "WHICH")))
    `(multiple-value-bind (,p ,w)
         (layout-find-parent (window-tree ,window) ,node)
       (if ,p
           (ecase ,w
             (:first  (setf (layout-split-first  ,p) ,new-value))
             (:second (setf (layout-split-second ,p) ,new-value)))
           (setf (window-tree ,window) ,new-value)))))

(defun %replace-in-tree (window leaf replacement)
  "Splice REPLACEMENT in place of LEAF in WINDOW's split tree."
  (%set-tree-link window leaf replacement))

(defun %collapse-parent (window parent which)
  "Remove the WHICH child of PARENT, replacing PARENT with the surviving sibling.
   Returns the sibling node."
  (let ((sibling (if (eq which :first)
                     (layout-split-second parent)
                     (layout-split-first  parent))))
    (%set-tree-link window parent sibling)
    sibling))

(defun %ratio-from-size-hint (hint avail orient)
  "Convert a size HINT (integer cells or real percentage) to a split ratio for
   the new (second) child given AVAIL total cells and ORIENT.
   Returns a ratio in (0,1) clamped to leave at least MIN cells on each side."
  (let* ((floor* (%axis-floor orient))
         (cells  (cond
                   ((and (integerp hint) (> hint 0)) hint)
                   ((and (realp hint) (< 0.0 hint 1.0)) (round (* avail hint)))
                   (t (floor avail 2)))))
    ;; clamp so both halves stay above the minimum
    (let ((clamped (max floor* (min (- avail floor* 1) cells))))
      ;; ratio is the fraction for the FIRST child; new pane is second.
      (/ (- avail clamped 1) avail))))

(defun window-split (window direction &key no-focus size)
  "Split the active pane of WINDOW along DIRECTION (:h left/right, :v top/bottom).
   Returns the new pane, or NIL when the active pane is too small.
   NO-FOCUS T keeps the current active pane selected (the new pane is created
   but not focused).  SIZE is an integer (cells) or real (fraction 0..1) that
   controls the new pane's initial size along the split axis."
  (let ((active (window-active-pane window))
        (tree   (window-tree window)))
    (unless (and active tree) (return-from window-split nil))
    (let ((leaf (layout-find-leaf tree active)))
      (unless (and leaf (%split-fits-p active direction))
        (return-from window-split nil))
      (multiple-value-bind (px py pw ph) (split-child-geometry active direction)
        (let* ((new-pane (%fork-pane (next-pane-id window) px py pw ph))
               ;; Compute initial ratio; if SIZE given, derive from it.
               (avail    (1- (ecase direction
                               (:h (pane-width  active))
                               (:v (pane-height active)))))
               (ratio    (if size
                             (%ratio-from-size-hint size avail direction)
                             1/2))
               (split    (make-layout-split direction leaf
                                            (make-layout-leaf new-pane) ratio)))
          (%replace-in-tree window leaf split)
          (window-relayout window (window-height window) (window-width window))
          (unless no-focus
            (setf (window-active window) new-pane))
          new-pane)))))

(defun window-relayout (window rows cols)
  "Re-fit WINDOW's panes into ROWS x COLS using the binary split tree."
  (setf (window-width  window) cols
        (window-height window) rows)
  (when (window-tree window)
    (layout-assign (window-tree window) 0 0 cols rows)
    (window-refresh-panes window)))

(defun ensure-window-fits (window rows cols)
  "Relayout WINDOW only when its stored size differs from ROWS x COLS."
  (when (or (/= (window-width  window) cols)
            (/= (window-height window) rows))
    (window-relayout window rows cols)))

(defun window-remove-pane (window pane)
  "Remove PANE from WINDOW's tree, collapsing its parent so the sibling reclaims
   the freed rectangle, then relayout.  Returns the surviving sibling pane
   (for MRU-style reselection), or NIL when WINDOW becomes empty."
  (let* ((tree (window-tree window))
         (leaf (layout-find-leaf tree pane)))
    (unless leaf
      (return-from window-remove-pane (first (window-panes window))))
    (multiple-value-bind (parent which) (layout-find-parent tree leaf)
      (cond
        ;; LEAF was the sole root — window becomes empty.
        ((null parent)
         (setf (window-tree window) nil
               (window-panes window) nil)
         nil)
        ;; Normal case: collapse the parent split and relayout.
        (t
         (let ((sibling (%collapse-parent window parent which)))
           (window-relayout window (window-height window) (window-width window))
           (first (layout-leaves sibling))))))))

;;; ── Resize via the tree ──────────────────────────────────────────────────

(defun %new-split-ratio (orient avail cur-ratio delta grow-first)
  "Compute the ratio after moving the split border by DELTA cells.
   Returns the new ratio as a rational, or NIL when the move would violate
   the minimum pane size on either side."
  (let* ((floor*    (%axis-floor orient))
         (cur-first (round (* avail cur-ratio)))
         (sign      (if grow-first 1 -1))
         (new-first (+ cur-first (* sign delta))))
    (when (and (<= floor* new-first) (<= new-first (- avail floor*)))
      (/ new-first avail))))

(defun window-resize-active (window direction delta)
  "Move the split border between the active pane and its neighbour in DIRECTION
   by DELTA cells, then relayout.  Returns ACTIVE on success, NIL when there is
   no neighbour in DIRECTION or the move is too small."
  (let* ((tree   (window-tree window))
         (active (window-active-pane window))
         (orient (resize-direction-orientation direction)))
    (unless (and tree active) (return-from window-resize-active nil))
    (let ((leaf (layout-find-leaf tree active)))
      (unless leaf (return-from window-resize-active nil))
      (multiple-value-bind (split side) (resize-find-split tree leaf orient)
        (unless split (return-from window-resize-active nil))
        (let* ((avail      (max 1 (- (layout-split-axis-extent split orient) 1)))
               (grow-first (if (eq side :first)
                               (member direction '(:right :down))
                               (member direction '(:left :up))))
               (new-ratio  (%new-split-ratio orient avail
                                             (layout-split-ratio split)
                                             delta grow-first)))
          (when new-ratio
            (setf (layout-split-ratio split) new-ratio)
            (window-relayout window (window-height window) (window-width window))
            active))))))

;;; ── Rotate-window ────────────────────────────────────────────────────────────
;;;
;;; rotate_window(Window, :up)   :- move first pane to end of panes list, relayout.
;;; rotate_window(Window, :down) :- move last  pane to front of panes list, relayout.

(defun window-rotate (window &optional (direction :up))
  "Rotate pane ordering within WINDOW.
   :UP moves the first pane to the end (forward rotation, tmux default).
   :DOWN moves the last pane to the front (reverse rotation).
   After rotation, the tree is rebuilt from the new panes order and relayouted."
  (let ((panes (window-panes window)))
    (when (> (length panes) 1)
      (let ((new-panes
             (ecase direction
               (:up   (append (rest panes) (list (first panes))))
               (:down (cons (car (last panes))
                            (butlast panes))))))
        ;; Rebuild the split tree in the new panes order using equal splits.
        ;; Build a right-spine binary tree: each step pairs the next pane with
        ;; a sub-tree of the remaining panes, all at ratio 1/2.
        (let ((tree (labels ((build (ps)
                               (if (null (rest ps))
                                   (make-layout-leaf (first ps))
                                   (make-layout-split :h
                                      (make-layout-leaf (first ps))
                                      (build (rest ps))
                                      1/2))))
                      (build new-panes))))
          (setf (window-panes window) new-panes
                (window-tree  window) tree)
          (window-relayout window (window-height window) (window-width window)))))))

(defun window-zoom-toggle (window)
  "Toggle zoom on WINDOW's active pane. When zooming in, saves the current tree
   and replaces it with a single-leaf tree. When zooming out, restores the saved tree.
   All slot mutations are protected by the window lock to prevent renderer races."
  (with-lock-held ((window-lock window))
    (if (window-zoom-p window)
      ;; Zoom out: restore saved tree, then relayout canonically.
      ;; Guard against corrupted state where zoom-tree is nil.
      (when (window-zoom-tree window)
        (setf (window-tree window) (window-zoom-tree window)
              (window-zoom-tree window) nil
              (window-zoom-p window) nil)
        (window-relayout window (window-height window) (window-width window)))
      ;; Zoom in: save tree, replace with single leaf.
      ;; Guard against empty windows (no active pane).
      (let ((pane (window-active-pane window)))
        (when pane
          (let ((new-tree (make-layout-leaf pane)))
            (setf (window-zoom-tree window) (window-tree window)
                  (window-tree window) new-tree
                  (window-zoom-p window) t)
            (window-refresh-panes window)
            (pane-reposition pane 0 0 (window-width window) (window-height window))))))))

