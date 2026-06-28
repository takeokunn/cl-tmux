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
  (local-options (make-hash-table :test #'equal) :type hash-table) ; per-window option overrides
  (zoom-p      nil :type boolean)  ; T when this window's active pane is zoomed
  (zoom-tree   nil)               ; saved layout tree before zooming, NIL when not zoomed
  (last-active nil)               ; previously active pane (for C-b ;)
  (last-active-time 0 :type integer)  ; universal-time when this window was last focused
  (automatic-rename-p t :type boolean) ; when T, OSC 0/2 title updates window-name
  (layout-cycle-index 0 :type fixnum) ; index into the layouts cycle for C-b Space
  ;; Activity tracking for monitor-activity / #{window_activity_flag}:
  ;; set T when a non-active window receives PTY output and monitor-activity is on.
  ;; Cleared when the window is selected.
  (activity-flag nil :type boolean)
  ;; Silence tracking for monitor-silence: universal-time of last PTY output.
  ;; Updated by the reader thread; checked by the timer to detect long silences.
  (last-output-time 0 :type integer)
  ;; #{window_silence_flag}: set T when monitor-silence threshold is exceeded.
  ;; Cleared when the window is selected or receives new output.
  (silence-flag nil :type boolean)
  (lock (make-lock "window") :read-only t))

(defun window-refresh-panes (window)
  "Recompute WINDOW's derived PANES list from its TREE (when present)."
  (when (window-tree window)
    (setf (window-panes window) (layout-leaves (window-tree window))))
  (window-panes window))

(defun window-active-pane (window)
  "Return WINDOW's active pane, falling back to the first pane when active is NIL."
  (or (window-active window)
      (first (window-panes window))))

(defun window-select-pane (window pane)
  "Make PANE the active pane of WINDOW.
   Records the previously active pane in window-last-active and updates
   window-last-active-time."
  (let ((current (window-active window)))
    (when (and current (not (eq current pane)))
      (setf (window-last-active window) current)))
  (setf (window-active window) pane
        (window-last-active-time window) (get-universal-time)))

;;; ── Orientation-aware pane extent ──────────────────────────────────────────
;;;
;;; The :v/:h naming is tmux-style:
;;;   :v split stacks children vertically → extent measured in ROWS (height)
;;;   :h split places children side-by-side → extent measured in COLS (width)
;;;
;;; Axis fact table (Prolog-style):
;;;   axis_extent(:v, pane) :- pane-height.
;;;   axis_extent(:h, pane) :- pane-width.

(defun %orient-pane-extent (pane orient)
  "Current extent of PANE along ORIENT's split axis."
  (orient-case orient :v (pane-height pane) :h (pane-width pane)))

(defconstant +pane-separator-width+ 1
  "Width in cells of the separator drawn between panes in a split layout.")

(defun %split-axis-fits-p (extent orient)
  "T when EXTENT is large enough to split along ORIENT.
   Requires at least 2 * axis-floor + +pane-separator-width+ cells."
  (let ((axis-floor (%axis-floor orient)))
    (>= extent (+ axis-floor +pane-separator-width+ axis-floor))))

(defun %split-fits-p (pane orient)
  "T when PANE is wide/tall enough to split along ORIENT."
  (%split-axis-fits-p (%orient-pane-extent pane orient) orient))

;;; ── Window-level pane ID allocation ────────────────────────────────────────

(defun next-pane-id (window)
  "Smallest pane id >= pane-base-index not already used in WINDOW.
   Window-level concern: queries pane membership, not geometry."
  (let* ((base (or (cl-tmux/options:get-option "pane-base-index") 0))
         (used (mapcar #'pane-id (window-panes window))))
    (loop for i from base
          unless (member i used) return i)))

;;; ── Tree-link mutation ──────────────────────────────────────────────────────
;;;
;;; Both %replace-in-tree and %collapse-parent share the same pattern:
;;;   set_tree_link(Window, Node, New) :-
;;;     find_parent(Window.tree, Node, Parent, Side), !,
;;;     set_child(Parent, Side, New).
;;;   set_tree_link(Window, _Node, New) :-
;;;     Window.tree := New.

(defmacro %set-tree-link (window node new-value)
  "Replace NODE's position in WINDOW's tree with NEW-VALUE.
   If NODE has a parent, updates that parent's child link (preserving side);
   if NODE is the root, replaces the root directly."
  (let ((parent-node  (gensym "PARENT-NODE"))
        (side-keyword (gensym "SIDE-KEYWORD")))
    `(multiple-value-bind (,parent-node ,side-keyword)
         (layout-find-parent (window-tree ,window) ,node)
       (if ,parent-node
           (ecase ,side-keyword
             (:first  (setf (layout-split-first  ,parent-node) ,new-value))
             (:second (setf (layout-split-second ,parent-node) ,new-value)))
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

;;; ── Window-level axis extent ────────────────────────────────────────────────
;;;
;;; The axis of a split measured at the WINDOW level (used by window-split :full):
;;;   :h → window-width   (how many columns the whole window spans)
;;;   :v → window-height  (how many rows the whole window spans)

(defun %window-axis-extent (window direction)
  "Return the WINDOW dimension along DIRECTION's split axis.
   :h → window-width (columns); :v → window-height (rows)."
  (orient-case direction :h (window-width window) :v (window-height window)))

;;; ── Size-hint conversion ────────────────────────────────────────────────────
;;;
;;; Size-hint fact table (Prolog-style):
;;;   hint_rule(integer, positive) :- hint cells for the new pane.
;;;   hint_rule(real, 0<r<1)       :- proportional cells derived from avail.
;;;   hint_rule(_default_)         :- half the available space.

(defun %requested-cells-from-hint (hint avail orient)
  "Convert a split size HINT to a cell count within AVAIL along ORIENT.
   Returns an integer: the requested cell count for the new (second) child."
  (declare (ignorable orient))
  (typecase hint
    (integer (if (> hint 0) hint (floor avail 2)))
    (real    (if (< 0.0 hint 1.0) (round (* avail hint)) (floor avail 2)))
    (t       (floor avail 2))))

(defun %ratio-from-size-hint (hint avail orient)
  "Convert a size HINT (integer cells or real percentage) to a split ratio for
   the new (second) child given AVAIL total cells and ORIENT.
   Returns a ratio in (0,1) clamped to leave at least the axis floor on each side."
  (let* ((axis-floor   (%axis-floor orient))
         ;; Requested cells for the NEW (second) child.
         (requested    (%requested-cells-from-hint hint avail orient))
         ;; Upper bound: leave at least axis-floor cells for the FIRST child.
         (upper-bound  (- avail axis-floor))
         ;; Clamped size: both halves stay above axis-floor.
         (clamped-size (max axis-floor (min upper-bound requested))))
    (/ clamped-size avail)))

(defun window-split (session window direction
                     &key no-focus size start-dir before full input-only input-bytes)
  "Split the active pane of WINDOW along DIRECTION (:h left/right, :v top/bottom).
   Returns the new pane, or NIL when the active pane is too small.
   NO-FOCUS T keeps the current active pane selected (the new pane is created
   but not focused).  SIZE is an integer (cells) or real (fraction 0..1) that
   controls the new pane's initial size along the split axis.
   BEFORE T inserts the new pane before (left of / above) the active pane
   instead of after (right of / below) — matches split-window -b.
   FULL T makes the new pane span the FULL window dimension (split-window -f): the
   split is inserted at the tree ROOT, with the entire existing layout as one child
   and the new pane as the other, instead of subdividing only the active pane.
   INPUT-ONLY T creates a pane without a PTY and feeds INPUT-BYTES into its screen.
   START-DIR: when non-NIL, the new pane's shell starts in that directory."
  (let ((active (window-active-pane window))
        (tree   (window-tree window)))
    (when (and active tree)
      (let ((leaf (layout-find-leaf tree active)))
        ;; Fit check: a full split is measured against the WINDOW extent, a normal
        ;; split against the active pane.
        (when (and leaf
                   (if full
                       (%split-axis-fits-p (%window-axis-extent window direction)
                                           direction)
                       (%split-fits-p active direction)))
          (multiple-value-bind (px py pw ph) (split-child-geometry active direction)
            (let* ((new-pane (if input-only
                                 (%make-input-pane (next-pane-id window) px py pw ph)
                                 (%fork-pane session (next-pane-id window) px py pw ph
                                             :start-dir start-dir)))
                   ;; A full split's extent is the whole window along the split axis;
                   ;; a normal split's is the active pane's extent.
                   (avail    (1- (if full
                                     (%window-axis-extent window direction)
                                     (%orient-pane-extent active direction))))
                   (new-ratio (if size
                                  (%ratio-from-size-hint size avail direction)
                                  1/2))
                   ;; ANCHOR is the existing node that becomes the new pane's sibling:
                   ;; the whole TREE for a full split, else just the active pane's LEAF.
                   (anchor   (if full tree leaf))
                   ;; When BEFORE is T: new pane is the first child; existing is second.
                   ;; The ratio fraction refers to the FIRST child's share of the extent.
                   ;; With BEFORE: ratio = new-pane's share = new-ratio.
                   ;; Without BEFORE: first=existing, second=new; ratio = (1 - new-ratio).
                   (split    (if before
                                 (make-layout-split direction
                                                    (make-layout-leaf new-pane)
                                                    anchor
                                                    new-ratio)
                                 (make-layout-split direction anchor
                                                    (make-layout-leaf new-pane)
                                                    (- 1 new-ratio)))))
              (if full
                  ;; Full split: the new split becomes the tree root.
                  (setf (window-tree window) split)
                  (%replace-in-tree window leaf split))
              (setf (pane-window new-pane) window)
              (window-relayout-current window)
              (when input-bytes
                (pane-feed new-pane input-bytes))
              (unless no-focus
                (setf (window-active window) new-pane))
              new-pane)))))))

(defun %status-top-offset ()
  "Rows reserved at the TOP of the window for a top-positioned status bar:
   cl-tmux/config:*status-height* when the status is on AND status-position is
   \"top\", else 0.  Panes are laid out starting at this y so a top status bar
   never overlaps them (and a bottom bar leaves the top flush at y=0).  Reads the
   live status-height/option globals; safe because config loads before this file
   and the option symbol exists from package.lisp."
  (if (and (plusp cl-tmux/config:*status-height*)
           (string-equal (or (cl-tmux/options:get-option "status-position") "bottom")
                         "top"))
      cl-tmux/config:*status-height*
      0))

(defun %assign-window-tree (window w h &optional (top-offset 0))
  "Assign WINDOW's split tree into a W x H area, offset DOWN by TOP-OFFSET rows.
   TOP-OFFSET defaults to 0; callers pass the result of %status-top-offset when
   they want the top status-bar shift (e.g. window-relayout), or leave it at 0
   for pure geometry operations (named layouts, resize handlers) that already
   handle the offset themselves.  Separating offset-reading from layout-assign
   keeps this function pure — option reads happen at the orchestration call site."
  (when (window-tree window)
    (layout-assign (window-tree window) 0 top-offset w h)))

(defun window-relayout (window rows cols)
  "Re-fit WINDOW's panes into ROWS x COLS using the binary split tree.
   Reads the live status-position option here (orchestration boundary) and passes
   the computed offset to %assign-window-tree so that function stays pure.
   After assigning geometry via the tree, each pane's screen and PTY are
   notified via pane-reposition — completing the data/logic separation:
   layout-assign owns geometry, pane-reposition owns the I/O side effects."
  (setf (window-width  window) cols
        (window-height window) rows)
  (when (window-tree window)
    (%assign-window-tree window cols rows (%status-top-offset))
    (window-refresh-panes window)
    (dolist (pane (window-panes window))
      (pane-reposition pane
                       (pane-x pane) (pane-y pane)
                       (pane-width pane) (pane-height pane)))))

(defun ensure-window-fits (window rows cols)
  "Relayout WINDOW only when its stored size differs from ROWS x COLS."
  (when (or (/= (window-width  window) cols)
            (/= (window-height window) rows))
    (window-relayout window rows cols)))

(defun window-relayout-current (window)
  "Relayout WINDOW using its current stored height and width."
  (window-relayout window (window-height window) (window-width window)))

(defun window-remove-pane (window pane)
  "Remove PANE from WINDOW's tree, collapsing its parent so the sibling reclaims
   the freed rectangle, then relayout.  Returns the surviving sibling pane
   (for MRU-style reselection), or NIL when WINDOW becomes empty."
  (let* ((tree (window-tree window))
         (leaf (layout-find-leaf tree pane)))
    (if leaf
        (multiple-value-bind (parent which) (layout-find-parent tree leaf)
          (setf (pane-window pane) nil)
          (if parent
              ;; Normal case: collapse the parent split and relayout.
              (let ((sibling (%collapse-parent window parent which)))
                (window-relayout-current window)
                (first (layout-leaves sibling)))
              ;; LEAF was the sole root — window becomes empty.
              (setf (window-tree  window) nil
                    (window-panes window) nil)))
        (first (window-panes window)))))

;;; Resize, rotate, and zoom operations have been split into window-operations.lisp.
