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
  (ecase orient
    (:v (pane-height pane))
    (:h (pane-width  pane))))

(defun %split-axis-fits-p (extent orient)
  "T when EXTENT is large enough to split along ORIENT (needs 2*min + 1 separator)."
  (let ((axis-floor (%axis-floor orient)))
    (>= extent (+ axis-floor 1 axis-floor))))

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
  (if (eq direction :h)
      (window-width  window)
      (window-height window)))

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
  (cond
    ((and (integerp hint) (> hint 0))     hint)
    ((and (realp hint) (< 0.0 hint 1.0)) (round (* avail hint)))
    (t (floor avail 2))))

(defun %ratio-from-size-hint (hint avail orient)
  "Convert a size HINT (integer cells or real percentage) to a split ratio for
   the new (second) child given AVAIL total cells and ORIENT.
   Returns a ratio in (0,1) clamped to leave at least the axis floor on each side."
  (let* ((axis-floor   (%axis-floor orient))
         ;; Requested cells for the NEW (second) child.
         (requested    (%requested-cells-from-hint hint avail orient))
         ;; Upper bound: leave at least axis-floor cells for the FIRST child + 1 separator.
         (upper-bound  (- avail axis-floor 1))
         ;; Clamped size: both halves stay above axis-floor.
         (clamped-size (max axis-floor (min upper-bound requested)))
         ;; Ratio measures the FIRST child; new pane is the second.
         (first-size   (- avail clamped-size 1)))
    (/ first-size avail)))

(defun window-split (window direction &key no-focus size start-dir before full)
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
        (let* ((new-pane (%fork-pane (next-pane-id window) px py pw ph
                                     :start-dir start-dir))
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

(defun %assign-window-tree (window w h)
  "Assign WINDOW's split tree into a W×H area, offset DOWN by the top status
   reservation (%status-top-offset).  The single layout-assign chokepoint so a
   top status bar shifts every pane below it — used by window-relayout, the named
   layouts, and the resize handlers alike."
  (when (window-tree window)
    (layout-assign (window-tree window) 0 (%status-top-offset) w h)))

(defun window-relayout (window rows cols)
  "Re-fit WINDOW's panes into ROWS x COLS using the binary split tree.
   After assigning geometry via the tree, each pane's screen and PTY are
   notified via pane-reposition — completing the data/logic separation:
   layout-assign owns geometry, pane-reposition owns the I/O side effects."
  (setf (window-width  window) cols
        (window-height window) rows)
  (when (window-tree window)
    (%assign-window-tree window cols rows)
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

;;; ── Resize via the tree ──────────────────────────────────────────────────

(defun %new-split-ratio (orient avail cur-ratio delta grow-first)
  "Compute the ratio after moving the split border by DELTA cells.
   Returns the new ratio as a rational, or NIL when the move would violate
   the minimum pane size on either side."
  (let* ((axis-floor (%axis-floor orient))
         (cur-first  (round (* avail cur-ratio)))
         (sign       (if grow-first 1 -1))
         (new-first  (+ cur-first (* sign delta))))
    (when (and (<= axis-floor new-first) (<= new-first (- avail axis-floor)))
      (/ new-first avail))))

(defun %grow-first-p (side direction)
  "Return true when the first child of a split should grow given SIDE (:first/:second)
   and the resize DIRECTION (:left/:right/:up/:down)."
  (if (eq side :first)
      (member direction '(:right :down))
      (member direction '(:left :up))))

(defun window-resize-active (window direction delta)
  "Move the split border between the active pane and its neighbour in DIRECTION
   by DELTA cells, then relayout.  Returns ACTIVE on success, NIL when there is
   no neighbour in DIRECTION or the move is too small."
  (let* ((tree   (window-tree window))
         (active (window-active-pane window))
         (orient (resize-direction-orientation direction)))
    (when (and tree active)
      (let ((leaf (layout-find-leaf tree active)))
        (when leaf
          (multiple-value-bind (split side) (resize-find-split tree leaf orient)
            (when split
              (let* ((avail      (max 1 (- (layout-split-axis-extent split orient) 1)))
                     (grow-first (%grow-first-p side direction))
                     (new-ratio  (%new-split-ratio orient avail
                                                   (layout-split-ratio split)
                                                   delta grow-first)))
                (when new-ratio
                  (setf (layout-split-ratio split) new-ratio)
                  (window-relayout-current window)
                  active)))))))))

;;; ── Rotate-window ────────────────────────────────────────────────────────────
;;;
;;; rotate_window(Window, :up)   :- move first pane to end of panes list, relayout.
;;; rotate_window(Window, :down) :- move last  pane to front of panes list, relayout.

(defun %build-spine-tree (panes)
  "Build a right-spine binary tree from PANES using :h orientation and equal 1/2 ratios.
   Rotation resets the layout to a flat left-to-right arrangement so visual order
   matches the panes list.  Use apply-named-layout after rotating to restore a
   specific orientation."
  (if (null (rest panes))
      (make-layout-leaf (first panes))
      (make-layout-split :h
                         (make-layout-leaf (first panes))
                         (%build-spine-tree (rest panes))
                         1/2)))

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
        (setf (window-panes window) new-panes
              (window-tree  window) (%build-spine-tree new-panes))
        (window-relayout window (window-height window) (window-width window))))))

;;; ── Zoom helpers — pure tree transforms ─────────────────────────────────────
;;;
;;; Data/logic separation: the pure tree-slot mutations are isolated from the
;;; PTY resize side-effect (pane-reposition) so each concern is a named step.

(defun %zoom-in-geometry (window pane)
  "Save the current tree and replace it with a single-leaf tree for PANE.
   Sets window-zoom-p to T and refreshes the panes list.
   Does NOT call pane-reposition — caller handles the PTY resize."
  (setf (window-zoom-tree window) (window-tree window)
        (window-tree       window) (make-layout-leaf pane)
        (window-zoom-p     window) t)
  (window-refresh-panes window))

(defun %zoom-out-geometry (window)
  "Restore the saved tree from window-zoom-tree and clear zoom flags.
   Guards against corrupted state where zoom-tree is NIL.
   Returns T on success, NIL when the saved tree was missing."
  (when (window-zoom-tree window)
    (setf (window-tree      window) (window-zoom-tree window)
          (window-zoom-tree window) nil
          (window-zoom-p    window) nil)
    (window-relayout window (window-height window) (window-width window))
    t))

(defun window-zoom-toggle (window)
  "Toggle zoom on WINDOW's active pane.
   Zooming in saves the current tree, replaces it with a single-leaf tree, then
   calls pane-reposition to give the pane the full window rectangle.
   Zooming out restores the saved tree and relayouts canonically.
   All slot mutations are protected by the window lock to prevent renderer races."
  (with-lock-held ((window-lock window))
    (if (window-zoom-p window)
        ;; Zoom out: restore saved tree (guard against corrupted state).
        (%zoom-out-geometry window)
        ;; Zoom in: save tree, replace with single leaf, then resize PTY.
        (let ((pane (window-active-pane window)))
          (when pane
            (%zoom-in-geometry window pane)
            (pane-reposition pane 0 0 (window-width window) (window-height window)))))))
