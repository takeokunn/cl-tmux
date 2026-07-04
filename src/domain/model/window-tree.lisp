(in-package #:cl-tmux/model)

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
