(in-package #:cl-tmux)

;;; -- Mouse event dispatch ----------------------------------------------------
;;;
;;; X10 and SGR mouse encoding, passthrough, drag-resize, click-count,
;;; border detection, mouse-key naming, and %dispatch-mouse-event.
;;; Mouse-only responsibilities stay here; overlay pager ESC handling lives in
;;; src/events-overlay-pager.lisp.

;;; ── Mouse event dispatch ─────────────────────────────────────────────────────
;;;
;;; X10 mouse encoding: ESC [ M <btn+32> <col+33> <row+33> (1-based coords).
;;; SGR mouse encoding: ESC [ < N ; COL ; ROW M (or m for release).
;;;
;;; %DISPATCH-MOUSE-EVENT handles scroll-wheel (btns 64/65), left-button
;;; press (btn 0) to focus pane or begin selection, status bar clicks, and
;;; wheel-scroll enter/exit of copy-mode.
;;; All mouse handling is gated behind the "mouse" session option.
;;;
;;; MOUSE PASSTHROUGH: when a pane app has enabled mouse tracking (?1000h /
;;; ?1002h / ?1003h / ?1006h), mouse events are translated to pane-local
;;; coordinates and forwarded to the pane's PTY *before* any tmux-UI handling.

(defun %encode-mouse-for-pane (pane screen btn col row release-p)
  "Encode a mouse event in the format the pane app requested and write to PTY.
   COL/ROW are 0-based screen coordinates; translated to pane-local 1-based.
   Returns T if the event was forwarded."
  (let* ((pane-col (1+ (- col (pane-x pane))))   ; 1-based pane-local column
         (pane-row (1+ (- row (pane-y pane))))    ; 1-based pane-local row
         (encoded
          (if (screen-mouse-sgr-mode screen)
              ;; SGR: ESC [ < btn ; col ; row M|m  (btn is raw, final M=press m=release)
              (format nil "~C[<~D;~D;~D~C"
                      #\Escape btn pane-col pane-row
                      (if release-p #\m #\M))
              ;; X10: ESC [ M <btn+32> <col+32> <row+32>  (1-based coords)
              ;; Release events use btn=3 (X10 release marker byte = 35 = 3+32).
              (let ((enc-btn (if release-p 35 (+ btn 32)))
                    (enc-col (min 255 (+ pane-col 32)))
                    (enc-row (min 255 (+ pane-row 32))))
                (format nil "~C[M~C~C~C"
                        #\Escape
                        (code-char enc-btn)
                        (code-char enc-col)
                        (code-char enc-row))))))
    (when (and encoded (cl-tmux/model:pane-live-p pane) (not *client-read-only*))
      (pty-write (pane-fd pane) encoded)
      t)))

(defun %try-mouse-passthrough (active-window active-pane btn col row release-p)
  "When the pane under COL/ROW (or ACTIVE-PANE for motion events) has enabled
   mouse tracking, translate the event to pane-local coordinates and forward it.
   Returns T if the event was consumed by a pane, NIL if tmux should handle it."
  (let* ((target-pane (or (and active-window
                               (pane-at-position active-window col row))
                          active-pane))
         (target-screen (and target-pane (pane-screen target-pane)))
         (mode (and target-screen (screen-mouse-mode target-screen))))
    (when (and target-pane target-screen (plusp (or mode 0)))
      (let ((should-forward
             (case mode
               ;; mode 1 (X10 / normal): button press only, not release
               (1 (not release-p))
               ;; mode 2 (button-event): press, release, button-motion (btn = +mouse-btn-motion+)
               (2 t)
               ;; mode 3 (any-event): all including pure motion
               (3 t)
               (otherwise nil))))
        (when should-forward
          (%encode-mouse-for-pane target-pane target-screen btn col row release-p))))))

(defun %forward-current-mouse-event-to-pane (pane)
  "Forward the currently bound mouse event to PANE.  Returns T on success, NIL
   when there is no current mouse event or the event could not be encoded."
  (when *current-mouse-event*
    (destructuring-bind (&key btn col row release-p)
        *current-mouse-event*
      (let ((screen (pane-screen pane)))
        (and screen
             (%encode-mouse-for-pane pane screen btn col row release-p))))))

(defun %pane-local-coordinates (pane col row)
  "Return COL and ROW translated into PANE-local coordinates."
  (values (- col (pane-x pane))
          (- row (pane-y pane))))

;;; ── Drag-resize state ────────────────────────────────────────────────────────

(defvar *mouse-drag-state* nil
  "Drag state for border-resize: NIL or (split orientation).
   Set on button-1 press on a border; cleared on button-1 release.
   The press coordinates (col, row) are not stored because they are not
   needed after the initial hit-test — only the split node and orientation
   matter for subsequent motion events.")

(defvar *last-mouse-click* nil
  "Double/triple-click detection state: (list time-ms row col count) of the most
   recent left mouse press, or NIL.  Reset per-test by WITH-LOOP-STATE so click
   counts do not leak between tests.")

(defvar *current-mouse-event* nil
  "Dynamically bound mouse event context for nested commands such as send-keys -M.
   Stored as a plist with :BTN, :COL, :ROW and :RELEASE-P keys.")

(defun %now-ms ()
  "Current monotonic time in milliseconds, from GET-INTERNAL-REAL-TIME."
  (floor (* 1000 (get-internal-real-time)) internal-time-units-per-second))

(defun %mouse-click-count (last now-ms row col threshold-ms)
  "Compute the click count for a left press at (ROW,COL) at NOW-MS.  LAST is the
   previous (time row col count) record or NIL.  A press within THRESHOLD-MS of
   the previous press AT THE SAME cell increments the count (1→2 double, 2→3
   triple); otherwise the count resets to 1.  Pure — testable without a clock."
  (if (and last
           (<= (- now-ms (first last)) threshold-ms)
           (= row (second last))
           (= col (third last)))
      (1+ (fourth last))
      1))

;;; Prolog-style fact table: (name doc sep-key lo-key hi-key sep-coord span-coord)
;;; The two border predicates are axis mirrors: swap x↔y and col↔row.
;;; define-border-hit-predicates makes the symmetry explicit data.

(defmacro define-border-hit-predicates (&rest specs)
  "Generate border-hit predicates from a declarative (name doc sep-key lo-key hi-key sep-coord span-coord) table.
   SEP-KEY  : function (pane → number) for the primary-axis separator edge.
   LO-KEY   : function (pane → number) for the low end of the secondary span.
   HI-KEY   : function (pane → number) for the high end of the secondary span.
   SEP-COORD: which of COL/ROW is the separator coordinate.
   SPAN-COORD: which of COL/ROW is checked against [LO, HI)."
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
   (lambda (p) (+ (pane-x p) (pane-width p)))   #'pane-y
   (lambda (p) (+ (pane-y p) (pane-height p)))
   col row)
  (%v-border-hit-p
   "T when (COL, ROW) lands on the horizontal separator of a :v split."
   (lambda (p) (+ (pane-y p) (pane-height p)))   #'pane-x
   (lambda (p) (+ (pane-x p) (pane-width p)))
   row col))

(defun %border-check-node (col row node)
  "Internal helper for %border-at-position: walk NODE and return
   (values split orientation) if (COL, ROW) is on a border, else (values nil nil)."
  (etypecase node
    (layout-leaf (values nil nil))
    (layout-split
     ;; Check children first, then this split's own border.
     (multiple-value-bind (split1 orientation1)
         (%border-check-node col row (layout-split-first node))
       (if split1
           (values split1 orientation1)
           (multiple-value-bind (split2 orientation2)
               (%border-check-node col row (layout-split-second node))
             (if split2
                 (values split2 orientation2)
                 (let* ((orient       (layout-split-orientation node))
                        (first-leaves (layout-leaves (layout-split-first node)))
                        (all-leaves   (layout-leaves node)))
                   (ecase orient
                     (:h (if (%h-border-hit-p first-leaves all-leaves col row)
                             (values node :h) (values nil nil)))
                     (:v (if (%v-border-hit-p first-leaves all-leaves col row)
                             (values node :v) (values nil nil))))))))))))

(defun %border-at-position (window col row)
  "Return (values layout-split orientation) when (COL, ROW) is on a pane separator,
   or (values NIL NIL) when it is not on any border."
  (let ((tree (window-tree window)))
    (if tree
        (%border-check-node col row tree)
        (values nil nil))))

(defun %compute-split-ratio (all-panes split orientation pointer origin-key)
  "Compute the new layout-split ratio when the drag pointer is at POINTER.
   ORIENTATION is the split axis (:h or :v).  ORIGIN-KEY extracts the primary
   start coordinate from a pane (e.g. #'pane-x for :h, #'pane-y for :v).
   The ratio is clamped so each half retains at least 1 cell."
  (let* ((origin    (reduce #'min all-panes :key origin-key))
         (total     (layout-split-axis-extent split orientation))
         (new-first (max 1 (min (1- total) (- pointer origin)))))
    (/ new-first (float (1- total)))))

(defun %apply-drag-resize (window split orientation col row)
  "Adjust SPLIT's ratio so the separator tracks (COL, ROW) within WINDOW.
   ORIENTATION is :h (moves the vertical separator) or :v (moves the horizontal one).
   Recomputes all pane geometries via layout-assign after updating the ratio."
  (let ((all-panes (layout-leaves split)))
    (setf (layout-split-ratio split)
          (ecase orientation
            (:h (%compute-split-ratio all-panes split :h col #'pane-x))
            (:v (%compute-split-ratio all-panes split :v row #'pane-y)))))
  (let ((tree (window-tree window)))
    (when tree
      (%assign-window-tree window (window-width window) (window-height window)))))

(defun %mouse-location-name (location)
  "Return the human-readable suffix used in tmux mouse key names."
  (ecase location
    (:status "Status")
    (:border "Border")
    (:pane "Pane")))

(defun %mouse-key-name (btn release-p location)
  "Build the tmux mouse key name for a mouse event, e.g. \"WheelUpPane\",
   \"MouseDown1Pane\", \"MouseUp3Status\".  BTN is the X10 button code, RELEASE-P
   selects MouseUp vs MouseDown, and LOCATION is one of :PANE/:STATUS/:BORDER.
   Returns NIL for events with no standard binding name (motion/drag, unknown
   buttons), so the caller falls back to the built-in mouse behaviour.

   These names are exactly what %parse-key-token stores for `bind -n WheelUpPane`
   / `bind -n MouseDown1Pane` (multi-char tokens are kept as strings), so the
   result doubles as a root key-table lookup key."
  (let ((button (cond
                  ((= btn +mouse-btn-left+)        "1")
                  ((= btn +mouse-btn-middle+)      "2")
                  ((= btn 2)                       "3")   ; right button (no named constant)
                  (t nil)))
        (location-name (%mouse-location-name location)))
    (cond
      ((= btn +mouse-btn-scroll-up+)   (concatenate 'string "WheelUp" location-name))
      ((= btn +mouse-btn-scroll-down+) (concatenate 'string "WheelDown" location-name))
      (button (concatenate 'string (if release-p "MouseUp" "MouseDown")
                           button location-name))
      (t nil))))

(defun %mouse-event-action (btn release-p location)
  "Classify a mouse event into a symbolic built-in action."
  (cond
    ((and (eq location :status) (not release-p) (= btn +mouse-btn-left+))
     :status-click)
    ((= btn +mouse-btn-scroll-up+)
     :scroll-up)
    ((= btn +mouse-btn-scroll-down+)
     :scroll-down)
    ((and (= btn +mouse-btn-left+) (not release-p) (not (eq location :status)))
     :left-press)
    ((and (= btn +mouse-btn-left+) release-p)
     :left-release)
    ((and (= btn +mouse-btn-middle+) (not release-p) (not (eq location :status)))
     :middle-press)
    ((= btn +mouse-btn-motion+)
     :motion)
    (t nil)))

(defun %mouse-hit-location (active-window col row)
  "Return the mouse location as (values location split orientation).
   LOCATION is one of :STATUS, :BORDER, or :PANE."
  (let ((status-row (1- *term-rows*)))
    (cond
      ((= row status-row)
       (values :status nil nil))
      (active-window
       (multiple-value-bind (split orient)
           (%border-at-position active-window col row)
         (if split
             (values :border split orient)
             (values :pane nil nil))))
      (t
       (values :pane nil nil)))))

(defun %mouse-binding-consumed-p (session in-copy copy-table mouse-key)
  "Return T when a user mouse binding handled the event."
  (or (and in-copy (%try-bound-string-key session copy-table mouse-key))
      (%try-bound-string-key session +table-root+ mouse-key)))

(defun %mouse-event-context (session)
  "Return the active mouse dispatch context for SESSION."
  (let* ((active-window (session-active-window session))
         (active-pane   (session-active-pane session))
         (active-screen (and active-pane (pane-screen active-pane))))
    (values active-window
            active-pane
            active-screen
            (and active-screen (screen-copy-mode-p active-screen))
            (%active-copy-mode-table))))

(defun %dispatch-mouse-event-with-context (session active-window active-pane active-screen
                                          in-copy copy-table btn col row release-p)
  "Dispatch a mouse event after the active context has been resolved."
  (multiple-value-bind (location border-split border-orient)
      (%mouse-hit-location active-window col row)
    (let ((mouse-key (%mouse-key-name btn release-p location)))
      (unless (%mouse-binding-consumed-p session in-copy copy-table mouse-key)
        (%handle-mouse-built-in-action session active-window active-pane active-screen
                                       btn col row release-p location
                                       border-split border-orient)))))

(defun %handle-mouse-built-in-action (session active-window active-pane active-screen
                                      btn col row release-p location
                                      border-split border-orient)
  "Run the built-in mouse action after key-table bindings have had a chance."
  (case (%mouse-event-action btn release-p location)
    (:status-click
     (%mouse-status-bar-click session col))
    (:scroll-up
     (%mouse-handle-scroll-up active-screen))
    (:scroll-down
     (%mouse-handle-scroll-down active-screen))
    (:left-press
     (let* ((now (%now-ms))
            (count (%mouse-click-count *last-mouse-click*
                                       now row col
                                       (or (cl-tmux/options:get-option "double-click-time")
                                           500))))
       (%mouse-handle-left-press active-window col row now count
                                 border-split border-orient)))
    (:left-release
     (%mouse-handle-left-release active-window active-pane))
    (:middle-press
     (%mouse-handle-middle-press active-window col row))
    (:motion
     (%mouse-handle-motion active-window active-pane col row))
    (t nil)))

(defun %mouse-handle-scroll-up (active-screen)
  "Enter copy mode if needed, then scroll back."
  (when active-screen
    (%mouse-enter-copy-mode-if-needed active-screen)
    (copy-mode-scroll active-screen 3)))

(defun %mouse-handle-scroll-down (active-screen)
  "Scroll forward, leaving copy mode at the bottom."
  (when active-screen
    (copy-mode-scroll active-screen -3)
    (when (and (screen-copy-mode-p active-screen)
               (zerop (screen-copy-offset active-screen)))
      (copy-mode-exit active-screen))))

(defun %mouse-handle-left-press (active-window col row now count border-split border-orient)
  "Handle a left-button press in pane space."
  (setf *last-mouse-click*
        (list now row col count))
  (when active-window
    (if border-split
        (setf *mouse-drag-state* (list border-split border-orient))
        (let ((target-pane (pane-at-position active-window col row)))
          (when target-pane
            ;; Clicking a pane should behave like a keyboard focus change.
            (%select-pane-with-focus active-window target-pane)
            (let ((screen (pane-screen target-pane)))
              (%mouse-enter-copy-mode-if-needed screen)
              (multiple-value-bind (pane-col pane-row)
                  (%pane-local-coordinates target-pane col row)
                (copy-mode-set-cursor screen pane-row pane-col)
                (cond
                  ((= count 2)  (copy-mode-select-word screen))
                  ((>= count 3) (copy-mode-begin-line-selection screen))
                  (t            (copy-mode-begin-selection screen))))))))))

(defun %mouse-handle-left-release (active-window active-pane)
  "End a border drag or yank a selection if one is active."
  (if *mouse-drag-state*
      (setf *mouse-drag-state* nil)
      (when (and active-window active-pane)
        (let ((screen (pane-screen active-pane)))
          (when (and (screen-copy-mode-p screen)
                     (screen-copy-selecting screen))
            (copy-mode-yank screen))))))

(defun %mouse-handle-middle-press (active-window col row)
  "Focus the clicked pane and paste the top paste buffer."
  (when active-window
    (let ((target-pane (pane-at-position active-window col row)))
      (when target-pane
        (%select-pane-with-focus active-window target-pane)
        (let ((text (cl-tmux/buffer:get-paste-buffer 0)))
          (when text
            (%paste-to-pane target-pane text)))))))

(defun %mouse-enter-copy-mode-if-needed (screen)
  "Enter copy mode and mark the session when SCREEN is not already in copy mode."
  (when screen
    (unless (screen-copy-mode-p screen)
      (copy-mode-enter screen)
      (setf (screen-copy-mode-entered-by-mouse-p screen) t))))

(defun %mouse-handle-motion (active-window active-pane col row)
  "Resize the active border drag or extend copy-mode selection."
  (if *mouse-drag-state*
      (destructuring-bind (split orient) *mouse-drag-state*
        (when active-window
          (%apply-drag-resize active-window split orient col row)))
      (when (and active-window active-pane)
        (let* ((target-pane  (pane-at-position active-window col row))
               (screen       (and target-pane (pane-screen target-pane))))
          (when (and screen (screen-copy-mode-p screen) (screen-copy-selecting screen))
            (multiple-value-bind (pane-col pane-row)
                (%pane-local-coordinates target-pane col row)
              (copy-mode-set-cursor screen pane-row pane-col)))))))

(defun %dispatch-mouse-event (session btn col row release-p)
  "Handle a parsed mouse event. BTN is the button number (X10 encoded minus 32),
   COL/ROW are 0-based screen coordinates, RELEASE-P is T for release events.
   All handling is gated on the global 'mouse' option.

   A user mouse binding in the root key table (e.g. `bind -n WheelUpPane
   copy-mode`) takes precedence over the built-in behaviour; only when the
   reconstructed mouse key name is unbound do we fall through to the hardcoded
   scroll/click/drag handling below."
  (let ((*current-mouse-event* (list :btn btn :col col :row row :release-p release-p)))
    (unwind-protect
         (multiple-value-bind (active-window active-pane active-screen in-copy copy-table)
             (%mouse-event-context session)
            (cond
              ((not (cl-tmux/options:get-option "mouse"))
               nil)
              ((%try-mouse-passthrough active-window active-pane btn col row release-p)
               nil)
               (t
                (%dispatch-mouse-event-with-context session active-window active-pane active-screen
                                                    in-copy copy-table btn col row release-p)))
         (setf *dirty* t)))))
