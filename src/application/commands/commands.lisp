(in-package #:cl-tmux/commands)

;;; ── Pane operations ────────────────────────────────────────────────────────
;;;
;;; swap_pane(Window, Dir)   :- active(Window, AP), neighbor(AP, Dir, Other),
;;;                              swap_positions(AP, Other), swap_list_order(AP, Other).
;;; capture_pane(Pane, Opts) :- lock(screen(Pane)),
;;;                              (scrollback(Opts) -> emit_scrollback ; true),
;;;                              emit_visible_rows.

(defun %swap-pane-geometry (active-pane other)
  "Exchange the screen positions of two panes ACTIVE-PANE and OTHER in-place.
   Updates both pane structs so the renderer sees the swapped layout immediately."
  (let ((saved-x      (pane-x      active-pane))
        (saved-y      (pane-y      active-pane))
        (saved-width  (pane-width  active-pane))
        (saved-height (pane-height active-pane)))
    (pane-reposition active-pane
                     (pane-x other) (pane-y other)
                     (pane-width other) (pane-height other))
    (pane-reposition other saved-x saved-y saved-width saved-height)))

(defun swap-two-panes (window pane-a pane-b)
  "Swap PANE-A and PANE-B within WINDOW: exchange both their list positions and
   their screen geometry, so the renderer sees the swap immediately.  No-op
   (returns NIL) when either pane is missing, they are the same, or either is not
   in WINDOW.  Does NOT change which pane is active.  Returns PANE-A on success."
  (let* ((panes (window-panes window))
         (ia    (and pane-a (position pane-a panes)))
         (ib    (and pane-b (position pane-b panes))))
    (when (and ia ib (/= ia ib))
      (let ((new-panes (copy-list panes)))
        (setf (nth ia new-panes) pane-b
              (nth ib new-panes) pane-a
              (window-panes window) new-panes))
      (%swap-pane-geometry pane-a pane-b)
      pane-a)))

(defun swap-pane (window direction)
  "Swap the active pane with an adjacent pane in WINDOW.
   DIRECTION:
     :right — next in panes list (wraps around)
     :left  — previous in panes list (wraps around)
     :up    — spatially adjacent pane above (via pane-neighbor)
     :down  — spatially adjacent pane below (via pane-neighbor)
   Swaps both list order and screen geometry (via swap-two-panes)."
  (let* ((panes        (window-panes window))
         (ap           (window-active-pane window))
         (active-index (position ap panes))
         (n            (length panes)))
    (when (> n 1)
      (let ((other
             (ecase direction
               (:right
                (nth (mod (1+ active-index) n) panes))
               (:left
                (nth (mod (1- active-index) n) panes))
               (:up   (pane-neighbor window ap :up))
               (:down (pane-neighbor window ap :down)))))
        (when other
          (swap-two-panes window ap other))))))

;;; ── break-pane ─────────────────────────────────────────────────────────────
;;;
;;; break_pane(Session) :-
;;;   active_window(Session, Win),
;;;   active_pane(Win, Pane),
;;;   (sole_pane(Win) -> no_op ; true),
;;;   remove_pane(Win, Pane),
;;;   new_window(Session, NewWin),
;;;   set_sole_pane(NewWin, Pane),
;;;   select_window(Session, NewWin).

(defun %break-window-id-occupied-p (session id)
  (find id (session-windows session) :key #'window-id))

(defun %break-shuffle-window-ids-up (session dst)
  "Free up window id DST in SESSION for an incoming window by shifting ids up.
   Scans upward from DST to find the first unoccupied id (FREE), then shifts
   every window currently occupying an id in [DST, FREE) up by one.  The shift
   walks windows in DESCENDING id order so each window is moved into the slot
   just vacated by the window above it, never colliding with a not-yet-moved
   window.  Finally re-sorts SESSION's window list back into ascending id order."
  (let ((free dst))
    (loop while (%break-window-id-occupied-p session free) do (incf free))
    (dolist (win (sort (copy-list (session-windows session)) #'> :key #'window-id))
      (when (<= dst (window-id win) (1- free))
        (incf (window-id win)))))
  (setf (session-windows session)
        (sort (copy-list (session-windows session)) #'< :key #'window-id))
  (session-windows-changed session))

(defun %break-pane-target-id (target-window-id insert-after)
  (and target-window-id
       (+ target-window-id (if insert-after 1 0))))

(defun %break-pane-create-window (session src-win pane new-id name select)
  (let* ((rows    (window-height src-win))
         (cols    (window-width  src-win))
         (wname   (or name (cl-tmux/model::%shell-basename)))
         (new-win (make-window :id new-id :name wname :width cols :height rows)))
    ;; Install the pane as the sole leaf in the new window's tree.
    (setf (window-panes new-win) (list pane)
          (window-tree  new-win) (make-layout-leaf pane)
          (pane-window  pane)    new-win)
    (window-select-pane new-win pane)
    ;; Reposition the pane to fill the new window.
    (pane-reposition pane 0 0 cols rows)
    ;; Attach the new window to the session via the model-layer helper.
    (session-insert-window session new-win)
    (when select (session-select-window session new-win))
    (run-hooks +hook-after-new-window+ new-win)
    new-win))

(defun break-pane (session &key src-window pane name (select t)
                             target-window-id insert-after insert-before)
  "Remove PANE from SRC-WINDOW and place it as the sole pane of a new window.
   SRC-WINDOW defaults to the session's active window and PANE to that window's
   active pane (the no-argument interactive behaviour).  NAME, when given, names
   the new window (break-pane -n); otherwise the shell basename is used.  When
   SELECT is true (default) the session switches to the new window; NIL leaves the
   current window active (break-pane -d).  When the source window has only one
  pane, break-pane is a no-op.  Returns the new window, or NIL."
  (let* ((src-win (or src-window (session-active-window session)))
         (pane    (or pane (and src-win (window-active-pane src-win))))
         (target-id (%break-pane-target-id target-window-id insert-after)))
    (when (and target-id (not insert-after) (not insert-before)
               (%break-window-id-occupied-p session target-id))
      (return-from break-pane nil))
    (when (and src-win pane (>= (length (window-panes src-win)) 2))
      (when (and target-id (or insert-after insert-before))
        (%break-shuffle-window-ids-up session target-id))
      ;; Remove pane from its current window (collapses the tree).
      (window-remove-pane src-win pane)
      ;; After removal, re-select a pane in the source window.
      (when (window-panes src-win)
        (window-select-pane src-win (first (window-panes src-win))))
      ;; Create a new window with the pane as the sole full-screen occupant.
      ;; Without a target, use the lowest free window id like session-new-window.
      (%break-pane-create-window
       session src-win pane
       (or target-id
           (cl-tmux/model::%next-window-id
            session
            (or (cl-tmux/options:get-option "base-index") 0)))
       name select))))

;;; ── join-pane / move-pane ───────────────────────────────────────────────────
;;;
;;; join_pane(Session, SrcWin, SrcPane, DstWin, Dir) :-
;;;   remove_pane(SrcWin, SrcPane),
;;;   (empty(SrcWin) -> kill_window(Session, SrcWin) ; true),
;;;   insert_by_split(DstWin, SrcPane, Dir).

(defun %join-pane-kill-empty-src (session src-window)
  "Remove SRC-WINDOW from SESSION when it has no panes remaining.
   Switches the active window to the first surviving window if needed."
  (when (null (window-panes src-window))
    (let ((remaining (remove src-window (session-windows session))))
      (setf (session-windows session) remaining)
      (session-windows-changed session)
      (when (eq (session-active-window session) src-window)
        (session-select-window session (first remaining))))))

(defun %join-pane-active-leaf (dst-window)
  "Return (values active-pane tree active-leaf) for DST-WINDOW's current split
   tree, or NIL for ACTIVE-LEAF when the window has no active pane or tree."
  (let* ((active (window-active-pane dst-window))
         (tree   (window-tree dst-window)))
    (values active tree (and active tree (layout-find-leaf tree active)))))

(defun %join-pane-fits-p (dst-window active direction full)
  "True when a new DIRECTION split will fit in DST-WINDOW.
   FULL splits are checked against the whole window's axis extent; otherwise
   against the ACTIVE pane's own extent."
  (if full
      (cl-tmux/model::%split-axis-fits-p
       (cl-tmux/model::%window-axis-extent dst-window direction)
       direction)
      (cl-tmux/model::%split-fits-p active direction)))

(defun %join-pane-build-split-node (src-pane dst-window active tree active-leaf
                                    direction before full size)
  "Build the new layout-split node inserting a fresh leaf for SRC-PANE next to
   ACTIVE-LEAF (or replacing the whole TREE, when FULL) for a DIRECTION split.
   SIZE is an optional tmux `-l` size hint for the new pane's share; BEFORE
   swaps the child order so the new pane sits before the anchor.
   `make-layout-split` stores the ratio for the FIRST child, while
   `%ratio-from-size-hint` returns the desired share for the NEW pane, hence
   the ratio is inverted on the BEFORE-less branch."
  (let* ((available (1- (if full
                            (cl-tmux/model::%window-axis-extent dst-window direction)
                            (cl-tmux/model::%orient-pane-extent active direction))))
         (new-ratio (if size
                        (cl-tmux/model::%ratio-from-size-hint size available direction)
                        1/2))
         (anchor    (if full tree active-leaf))
         (new-node  (make-layout-leaf src-pane)))
    (if before
        (make-layout-split direction new-node anchor new-ratio)
        (make-layout-split direction anchor new-node (- 1 new-ratio)))))

(defun %join-pane-insert-into-dst (src-pane dst-window direction
                                   &key before full size)
  "Insert SRC-PANE into DST-WINDOW as a DIRECTION split.
   Returns SRC-PANE on success, NIL when the destination has no active leaf."
  (multiple-value-bind (active tree active-leaf) (%join-pane-active-leaf dst-window)
    (when (and active-leaf (%join-pane-fits-p dst-window active direction full))
      ;; Refresh the destination layout before deriving the split size.  Test
      ;; fixtures can carry stale pane dimensions even when the window/tree size
      ;; is current, and `-l` must be computed from the rendered pane extent.
      (cl-tmux/model:window-relayout-current dst-window)
      (multiple-value-bind (active tree active-leaf) (%join-pane-active-leaf dst-window)
        ;; Match split-window's layout rules: -f uses the full window extent,
        ;; -b swaps the child order, and -l supplies the target size hint.
        (multiple-value-bind (px py pw ph) (split-child-geometry active direction)
          (pane-reposition src-pane px py pw ph)
          (let ((new-split (%join-pane-build-split-node
                             src-pane dst-window active tree active-leaf
                             direction before full size)))
            (if full
                (setf (window-tree dst-window) new-split)
                (cl-tmux/model::%replace-in-tree dst-window active-leaf new-split))
            (setf (window-panes dst-window) (layout-leaves (window-tree dst-window))
                  (pane-window src-pane) dst-window)
            (window-relayout dst-window (window-height dst-window) (window-width dst-window))
            src-pane))))))

(defun join-pane (session src-window src-pane dst-window direction
                  &key before full size)
  "Move SRC-PANE from SRC-WINDOW into DST-WINDOW as a split in DIRECTION.
   DIRECTION is :h (left/right) or :v (top/bottom).
   If SRC-WINDOW becomes empty after removal, it is killed.
   Returns SRC-PANE on success, NIL on failure."
  (when (and src-window src-pane dst-window)
    (window-remove-pane src-window src-pane)
    (%join-pane-kill-empty-src session src-window)
    (%join-pane-insert-into-dst src-pane dst-window direction
                                :before before
                                :full full
                                :size size)))
