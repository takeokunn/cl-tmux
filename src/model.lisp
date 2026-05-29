(in-package #:cl-tmux/model)

;;; ── Pane ───────────────────────────────────────────────────────────────────

(defstruct pane
  "One terminal pane: a PTY fd + virtual screen + position within its window."
  (id     0   :type fixnum)
  (x      0   :type fixnum)
  (y      0   :type fixnum)
  (width  80  :type fixnum)
  (height 24  :type fixnum)
  (fd     -1  :type fixnum)
  (pid    -1  :type fixnum)
  (screen nil))

(defun pane-feed (pane bytes)
  "Feed raw PTY bytes into PANE's screen, holding the screen lock."
  (let ((screen (pane-screen pane)))
    (with-lock-held ((screen-lock screen))
      (screen-process-bytes screen bytes))))

(defun pane-reposition (pane x y width height)
  "Move and resize PANE to X,Y with WIDTH x HEIGHT.
   Resizes the underlying PTY and virtual screen."
  (setf (pane-x pane)      x
        (pane-y pane)      y
        (pane-width  pane) width
        (pane-height pane) height)
  (set-pty-size (pane-fd pane) height width)
  (let ((screen (pane-screen pane)))
    (with-lock-held ((screen-lock screen))
      (screen-resize screen width height))))

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

(defconstant +pane-min-width+  2
  "Smallest interior width (columns) a pane may occupy.")
(defconstant +pane-min-height+ 1
  "Smallest interior height (rows) a pane may occupy.")

(defun layout-leaves (node)
  "Collect every pane in NODE's subtree, left/top-to-right/bottom, as a list."
  (etypecase node
    (null            nil)
    (layout-leaf     (list (layout-leaf-pane node)))
    (layout-split    (append (layout-leaves (layout-split-first node))
                             (layout-leaves (layout-split-second node))))))

(defun layout-find-leaf (node pane)
  "Return the LAYOUT-LEAF in NODE that holds PANE, or NIL."
  (etypecase node
    (null         nil)
    (layout-leaf  (when (eq (layout-leaf-pane node) pane) node))
    (layout-split (or (layout-find-leaf (layout-split-first  node) pane)
                      (layout-find-leaf (layout-split-second node) pane)))))

(defun layout-find-parent (node child)
  "Return (values PARENT WHICH) where PARENT is the LAYOUT-SPLIT whose FIRST or
   SECOND child is (eq) CHILD, and WHICH is :first or :second.  NIL if none."
  (when (layout-split-p node)
    (cond ((eq (layout-split-first  node) child) (values node :first))
          ((eq (layout-split-second node) child) (values node :second))
          (t (multiple-value-bind (p w)
                 (layout-find-parent (layout-split-first node) child)
               (if p (values p w)
                   (layout-find-parent (layout-split-second node) child)))))))

;;; ── Tree geometry: assign rectangles ───────────────────────────────────────

(defun layout-min-extent (node orientation)
  "Minimum cells NODE needs along ORIENTATION's split axis (:v → rows, :h →
   cols), accounting for the 1-cell separator reserved at every internal node
   whose orientation matches."
  (let ((min (if (eq orientation :v) +pane-min-height+ +pane-min-width+)))
    (etypecase node
      (layout-leaf min)
      (layout-split
       (if (eq (layout-split-orientation node) orientation)
           ;; Same axis: children stack along it; +1 for the separator.
           (+ (layout-min-extent (layout-split-first  node) orientation)
              1
              (layout-min-extent (layout-split-second node) orientation))
           ;; Cross axis: children share the extent; need the larger minimum.
           (max (layout-min-extent (layout-split-first  node) orientation)
                (layout-min-extent (layout-split-second node) orientation)))))))

(defun layout-assign (node x y w h)
  "Walk NODE, repositioning every leaf's pane to fill the X,Y,W,H rectangle.
   Reserves one row/column for the separator at each internal node."
  (etypecase node
    (layout-leaf
     (pane-reposition (layout-leaf-pane node) x y (max 1 w) (max 1 h)))
    (layout-split
     (let ((ratio (layout-split-ratio node)))
       (ecase (layout-split-orientation node)
         (:h                            ; left | right, separator column between
          (let* ((avail (- w 1))
                 (fw     (max 1 (min (- avail 1) (round (* avail ratio)))))
                 (sw     (- avail fw)))
            (layout-assign (layout-split-first  node) x y fw h)
            (layout-assign (layout-split-second node) (+ x fw 1) y sw h)))
         (:v                            ; top / bottom, separator row between
          (let* ((avail (- h 1))
                 (fh     (max 1 (min (- avail 1) (round (* avail ratio)))))
                 (sh     (- avail fh)))
            (layout-assign (layout-split-first  node) x y w fh)
            (layout-assign (layout-split-second node) x (+ y fh 1) w sh))))))))

;;; ── Window ─────────────────────────────────────────────────────────────────

(defstruct window
  "A named collection of panes with one active (focused) pane.

   TREE is the authoritative binary split-tree layout (NIL for legacy fixtures
   built directly with :PANES).  PANES is the derived flat list of leaves, kept
   in tree order; LAYOUT is the legacy scalar orientation, retained only so
   old fixtures and the flat fallback paths keep working."
  (id      0   :type fixnum)
  (name    ""  :type string)
  (width   80  :type fixnum)
  (height  24  :type fixnum)
  (panes   nil :type list)
  (active  nil)
  (layout  nil)
  (tree    nil))

(defun window-refresh-panes (window)
  "Recompute WINDOW's derived PANES list from its TREE (when present)."
  (when (window-tree window)
    (setf (window-panes window) (layout-leaves (window-tree window))))
  (window-panes window))

(defun window-active-pane (window)
  (or (window-active window)
      (first (window-panes window))))

(defun window-select-pane (window pane)
  (setf (window-active window) pane))

(defun split-orientation (direction)
  "Translate a legacy split DIRECTION into a tree orientation keyword.
   :vertical means side-by-side (left/right → :h); :horizontal means stacked
   (top/bottom → :v).  Tree orientation keywords (:h/:v) pass through unchanged."
  (ecase direction
    ((:vertical   :h) :h)
    ((:horizontal :v) :v)))

(defun window-split (window direction)
  "Split the ACTIVE pane of WINDOW in two along DIRECTION, leaving every other
   pane untouched.  DIRECTION may be a legacy keyword (:vertical = left/right,
   :horizontal = top/bottom) or a tree orientation (:h = left/right,
   :v = top/bottom).

   Returns the new pane, or NIL when the active pane is too small to split
   (mirroring tmux's \"create pane failed: pane too small\")."
  (let* ((orient (split-orientation direction))
         (active (window-active-pane window)))
    (when active
      ;; Build the tree lazily: legacy single-pane windows have none yet.
      (unless (window-tree window)
        (setf (window-tree window) (make-layout-leaf active)))
      (let ((leaf (layout-find-leaf (window-tree window) active)))
        (when leaf
          ;; Minimum-size precondition: refuse a split that cannot fit two panes
          ;; plus the separator along the chosen axis.
          (let* ((avail (if (eq orient :v) (pane-height active) (pane-width active)))
                 (floor* (if (eq orient :v) +pane-min-height+ +pane-min-width+)))
            (when (< avail (+ floor* 1 floor*))
              (return-from window-split nil)))
          (multiple-value-bind (px py pw ph) (split-child-geometry active orient)
            (multiple-value-bind (fd pid) (forkpty-with-shell ph pw)
              (let* ((new-pane (make-pane :id (next-pane-id window)
                                          :x px :y py :width pw :height ph
                                          :fd fd :pid pid
                                          :screen (make-screen pw ph)))
                     (new-leaf (make-layout-leaf new-pane))
                     (split    (make-layout-split orient leaf new-leaf 1/2)))
                ;; Graft SPLIT in place of LEAF in the tree.
                (multiple-value-bind (parent which)
                    (layout-find-parent (window-tree window) leaf)
                  (if parent
                      (ecase which
                        (:first  (setf (layout-split-first  parent) split))
                        (:second (setf (layout-split-second parent) split)))
                      (setf (window-tree window) split)))
                ;; Reflow the whole window so the new geometry is exact.
                (window-relayout window (window-height window) (window-width window))
                (setf (window-active window) new-pane)
                new-pane))))))))

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

(defun next-pane-id (window)
  "Smallest positive pane id not already used in WINDOW."
  (let ((used (mapcar #'pane-id (window-panes window))))
    (loop for i from 1
          unless (member i used) return i)))

(defun window-relayout (window rows cols)
  "Re-fit WINDOW's panes into ROWS x COLS.

   With a TREE the rectangles are walked from the tree (nested/mixed layouts
   supported).  Without a TREE the legacy flat DIVIDE-WINDOW behaviour is used
   so old fixtures keep working."
  (setf (window-width  window) cols
        (window-height window) rows)
  (if (window-tree window)
      (progn
        (layout-assign (window-tree window) 0 0 cols rows)
        (window-refresh-panes window))
      (let* ((panes   (window-panes window))
             (n       (length panes))
             (layouts (if (window-layout window)
                          (divide-window (window-layout window) n rows cols)
                          (list (list 0 0 cols rows)))))
        (loop for pane in panes
              for layout in layouts
              do (destructuring-bind (px py pw ph) layout
                   (pane-reposition pane px py pw ph))))))

(defun ensure-window-fits (window rows cols)
  "Relayout WINDOW only when its stored size differs from ROWS x COLS."
  (when (or (/= (window-width  window) cols)
            (/= (window-height window) rows))
    (window-relayout window rows cols)))

(defun window-remove-pane (window pane)
  "Remove PANE from WINDOW's tree, collapsing its parent split so the sibling
   takes over the freed rectangle, then relayout.  Returns the surviving sibling
   pane closest to PANE (for MRU-style reselection), or NIL when WINDOW becomes
   empty.  Falls back to a flat removal when WINDOW has no tree."
  (let ((tree (window-tree window)))
    (if (null tree)
        ;; Legacy flat path: drop the pane and reflow the survivors.
        (progn
          (setf (window-panes window) (remove pane (window-panes window)))
          (when (window-panes window)
            (window-relayout window (window-height window) (window-width window)))
          (first (window-panes window)))
        (let ((leaf (layout-find-leaf tree pane)))
          (if (null leaf)
              (first (window-panes window))
              (multiple-value-bind (parent which) (layout-find-parent tree leaf)
                (if (null parent)
                    ;; The leaf was the whole tree: window becomes empty.
                    (progn (setf (window-tree window) nil
                                 (window-panes window) nil)
                           nil)
                    (let ((sibling (if (eq which :first)
                                       (layout-split-second parent)
                                       (layout-split-first  parent))))
                      ;; Replace PARENT with SIBLING in the grandparent.
                      (multiple-value-bind (gp gw) (layout-find-parent tree parent)
                        (if gp
                            (ecase gw
                              (:first  (setf (layout-split-first  gp) sibling))
                              (:second (setf (layout-split-second gp) sibling)))
                            (setf (window-tree window) sibling)))
                      (window-relayout window (window-height window)
                                       (window-width window))
                      (first (layout-leaves sibling))))))))))

(defun divide-window (direction n rows cols)
  "Divide ROWS x COLS into N layout slots for DIRECTION (:vertical/:horizontal).
   Reserves one row/column between adjacent panes for a separator.
   Returns a list of (x y width height).

   Retained for the pure-geometry test-suite and the legacy flat relayout path."
  (case direction
    (:vertical
     (let* ((avail (- cols (1- n)))
            (w     (max 1 (floor avail n))))
       (loop for i below n
             for x = (* i (1+ w))
             collect (list x 0
                           (if (= i (1- n)) (max 1 (- cols x)) w)
                           rows))))
    (:horizontal
     (let* ((avail (- rows (1- n)))
            (h     (max 1 (floor avail n))))
       (loop for i below n
             for y = (* i (1+ h))
             collect (list 0 y cols
                           (if (= i (1- n)) (max 1 (- rows y)) h)))))
    (otherwise
     (list (list 0 0 cols rows)))))

;;; ── Resize via the tree ──────────────────────────────────────────────────

(defun resize-direction-orientation (direction)
  "Tree split orientation a resize DIRECTION acts on:
   :left/:right move an :h (left/right) border; :up/:down move a :v one."
  (ecase direction
    ((:left :right) :h)
    ((:up   :down)  :v)))

(defun window-resize-active (window direction delta)
  "Move, via WINDOW's TREE, the border between the active pane and the neighbour
   in DIRECTION, adjusting the nearest ancestor split's ratio by DELTA cells, then
   relayout so all affected panes reflow.  Returns the active pane on success,
   NIL when there is no neighbour in that direction or the move is too small.

   Requires a TREE; the flat fallback is handled by COMMANDS:RESIZE-PANE."
  (let* ((tree   (window-tree window))
         (active (window-active-pane window))
         (orient (resize-direction-orientation direction)))
    (when (and tree active)
      (let ((leaf (layout-find-leaf tree active)))
        (when leaf
          ;; Climb to the nearest ancestor split on the right axis; remember the
          ;; side the active pane lives on so we know which border we touch.
          (multiple-value-bind (split active-side)
              (resize-find-split tree leaf orient)
            (when split
              (let* ((axis-extent (layout-split-axis-extent split orient))
                     (avail       (max 1 (- axis-extent 1)))
                     ;; Growing the active side: :right/:down enlarge FIRST when
                     ;; active is FIRST; otherwise shrink it.  A move "toward" the
                     ;; neighbour grows the active pane.
                     (grow-first  (if (eq active-side :first)
                                      (member direction '(:right :down))
                                      (member direction '(:left :up))))
                     (sign        (if grow-first 1 -1))
                     (ratio       (layout-split-ratio split))
                     (cur-first   (round (* avail ratio)))
                     (new-first   (+ cur-first (* sign delta)))
                     ;; Keep both sides at least the minimum.
                     (floor*      (if (eq orient :v)
                                      +pane-min-height+ +pane-min-width+))
                     (lo          floor*)
                     (hi          (- avail floor*)))
                (when (and (<= lo new-first) (<= new-first hi))
                  (setf (layout-split-ratio split) (/ new-first avail))
                  (window-relayout window (window-height window)
                                   (window-width window))
                  active)))))))))

(defun layout-split-axis-extent (split orient)
  "Span of SPLIT's bounding rectangle along ORIENT's axis (:v → rows, :h → cols),
   derived from its already-laid-out leaves.  This is the SPLIT's own extent, so
   ratio arithmetic is correct even for a deeply nested split that occupies only
   a sub-rectangle of the window."
  (let ((panes (layout-leaves split)))
    (if (eq orient :v)
        (- (reduce #'max panes :key (lambda (p) (+ (pane-y p) (pane-height p))))
           (reduce #'min panes :key #'pane-y))
        (- (reduce #'max panes :key (lambda (p) (+ (pane-x p) (pane-width p))))
           (reduce #'min panes :key #'pane-x)))))

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

;;; ── Session ────────────────────────────────────────────────────────────────

(defstruct session
  "Top-level container: a named set of windows with one active."
  (id      0   :type fixnum)
  (name    ""  :type string)
  (windows nil :type list)
  (active  nil))

(defun session-active-window (session)
  (or (session-active session)
      (first (session-windows session))))

(defun session-select-window (session window)
  (setf (session-active session) window))

(defun session-active-pane (session)
  (let ((w (session-active-window session)))
    (when w (window-active-pane w))))

(defun session-new-window (session name rows cols)
  "Create a new window with one full-size pane and add it to SESSION."
  (let* ((id  (1+ (length (session-windows session))))
         (win (make-window :id id :name name
                           :width cols :height rows)))
    (multiple-value-bind (fd pid)
        (forkpty-with-shell rows cols)
      (let ((pane (make-pane :id 1 :x 0 :y 0 :width cols :height rows
                             :fd fd :pid pid
                             :screen (make-screen cols rows))))
        (setf (window-panes  win) (list pane)
              (window-active win) pane
              (window-tree   win) (make-layout-leaf pane))))
    (setf (session-windows session)
          (append (session-windows session) (list win)))
    (setf (session-active session) win)
    win))

;;; ── Global state & initialisation ─────────────────────────────────────────

(defun create-initial-session (rows cols)
  "Bootstrap: one session, one window, one full-screen pane."
  (let ((session   (make-session :id 1 :name "0"))
        (pane-rows (- rows *status-height*)))
    (session-new-window session "1" pane-rows cols)
    session))

(defun all-panes (session)
  "Flat list of every pane across all windows of SESSION."
  (loop for w in (session-windows session)
        nconc (copy-list (window-panes w))))
