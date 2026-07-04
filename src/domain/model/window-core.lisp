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
  ;; select-layout -o: the layout tree in effect before the last layout change.
  (last-layout-tree nil)
  ;; Activity tracking for monitor-activity / #{window_activity_flag}:
  ;; set T when a non-active window receives PTY output and monitor-activity is on.
  ;; Cleared when the window is selected.
  (activity-flag nil :type boolean)
  ;; Bell tracking for monitor-bell / #{window_bell_flag} (tmux WINLINK_BELL):
  ;; set T when a non-current window's pane rings BEL and monitor-bell is on.
  ;; Sticky until the window is selected (unlike the transient per-screen
  ;; bell-pending flag, which only drives the audible relay).
  (bell-flag nil :type boolean)
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

(defun %split-fit-p (window active direction full)
  "T when a split along DIRECTION would fit: a full split (FULL T) is measured
   against WINDOW's own extent; a normal split is measured against ACTIVE pane's
   extent."
  (if full
      (%split-axis-fits-p (%window-axis-extent window direction) direction)
      (%split-fits-p active direction)))

(defstruct (%split-spec (:constructor %make-split-spec))
  "The configuration of a single window-split call, gathered into one value so
   %compute-new-pane-split and %splice-split-into-tree take one argument
   instead of threading five-plus keywords by hand.
   NO-FOCUS: keep the current active pane selected instead of focusing the new one.
   SIZE: integer (cells) or real (fraction 0..1) sizing the new pane, or NIL for 1/2.
   START-DIR: non-NIL working directory for the new pane's shell.
   BEFORE: T inserts the new pane before (left of / above) the active pane.
   FULL: T spans the split across the whole window (split-window -f), splitting
   the tree root instead of just the active pane's leaf.
   INPUT-ONLY: T creates a screen-only pane (no PTY) fed via INPUT-BYTES."
  (no-focus    nil)
  (size        nil)
  (start-dir   nil)
  (before      nil)
  (full        nil)
  (input-only  nil)
  (input-bytes nil))

(defun %new-split-pane (session window direction active spec)
  "Construct the new pane created by a split of ACTIVE along DIRECTION.
   Returns the new pane, either PTY-backed (via %fork-pane) or, when SPEC's
   INPUT-ONLY is T, a screen-only pane with a blank screen (later fed via
   pane-feed)."
  (multiple-value-bind (px py pw ph) (split-child-geometry active direction)
    (if (%split-spec-input-only spec)
        (%make-input-pane (next-pane-id window) px py pw ph)
        (%fork-pane session (next-pane-id window) px py pw ph
                    :start-dir (%split-spec-start-dir spec)))))

(defun %split-ratio (window active direction spec)
  "Return the split ratio for the new (second) child of a split along DIRECTION.
   AVAIL is the whole window extent for a full split, else the active pane's
   extent; SPEC's SIZE is the caller's size hint (or NIL for an even 1/2 split)."
  (let ((avail (1- (if (%split-spec-full spec)
                       (%window-axis-extent window direction)
                       (%orient-pane-extent active direction))))
        (size  (%split-spec-size spec)))
    (if size
        (%ratio-from-size-hint size avail direction)
        1/2)))

(defun %compute-new-pane-split (session window direction leaf active spec)
  "Build the new pane and its layout-split node for a window-split per SPEC.
   ANCHOR is the existing node that becomes the new pane's sibling: the whole
   tree for a full split, else just the active pane's LEAF.  SPEC's BEFORE T
   inserts the new pane as the first child, existing as second; otherwise the
   reverse.  Returns (values new-pane split-node)."
  (let* ((new-pane  (%new-split-pane session window direction active spec))
         (new-ratio (%split-ratio window active direction spec))
         (anchor    (if (%split-spec-full spec) (window-tree window) leaf))
         (split     (if (%split-spec-before spec)
                        (make-layout-split direction
                                           (make-layout-leaf new-pane)
                                           anchor
                                           new-ratio)
                        (make-layout-split direction anchor
                                           (make-layout-leaf new-pane)
                                           (- 1 new-ratio)))))
    (when (%split-spec-input-bytes spec)
      (pane-feed new-pane (%split-spec-input-bytes spec)))
    (values new-pane split)))

(defun %splice-split-into-tree (window leaf split spec)
  "Splice SPLIT into WINDOW's tree, replacing LEAF (normal split) or becoming
   the new tree root (SPEC's FULL split)."
  (if (%split-spec-full spec)
      (setf (window-tree window) split)
      (%replace-in-tree window leaf split)))

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
        (tree   (window-tree window))
        (spec   (%make-split-spec :no-focus no-focus :size size :start-dir start-dir
                                  :before before :full full :input-only input-only
                                  :input-bytes input-bytes)))
    (when (and active tree)
      (let ((leaf (layout-find-leaf tree active)))
        (when (and leaf (%split-fit-p window active direction full))
          (multiple-value-bind (new-pane split)
              (%compute-new-pane-split session window direction leaf active spec)
            (%splice-split-into-tree window leaf split spec)
            (setf (pane-window new-pane) window)
            (window-relayout-current window)
            (unless no-focus
              (setf (window-active window) new-pane))
            new-pane))))))

;;; Tree-link mutation, relayout, and removal moved to window-tree.lisp.
