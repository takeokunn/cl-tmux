(in-package #:cl-tmux)

;;; -- Mouse event dispatch + overlay pager escape-sequence handler -----------
;;;
;;; X10 and SGR mouse encoding, passthrough, drag-resize, click-count,
;;; border detection, mouse-key naming, and %dispatch-mouse-event.
;;; Also: %overlay-escape-second-byte / %overlay-escape-final for pager mode.

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
    (when (and encoded (> (pane-fd pane) 0) (not *client-read-only*))
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
             (cond
               ;; mode 1 (X10 / normal): button press only, not release
               ((= mode 1) (not release-p))
               ;; mode 2 (button-event): press, release, button-motion (btn = +mouse-btn-motion+)
               ((= mode 2) (or (not release-p) (= btn +mouse-btn-motion+)))
               ;; mode 3 (any-event): all including pure motion
               ((= mode 3) t)
               (t nil))))
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

;;; ── Status bar column → window index mapping ─────────────────────────────────

(defun %status-col-to-window (session col)
  "Return the window at column COL of the status bar, or NIL.
   Mirrors the layout produced by %status-window-list-styled, including the
   per-window format, separator, and inline style blocks."
  (labels ((window-entry-width (window)
             (let* ((active-p (eq window (session-active-window session)))
                    (context  (cl-tmux/format:format-context-from-window session window))
                    (fmt      (cl-tmux/options:get-option-for-context
                               (if active-p "window-status-current-format"
                                   "window-status-format")
                               :window window))
                    (label    (cl-tmux/format:expand-format fmt context))
                    (style    (cl-tmux/renderer::%window-status-style session window active-p))
                    (sgr-code (when (and style (plusp (length style)))
                                (cl-tmux/renderer::%status-sgr-from-style style)))
                    (expanded (cl-tmux/renderer::%status-expand-style-blocks
                               label
                               (or sgr-code +sgr-default-status+))))
               (cl-tmux/renderer::%visible-length expanded))))
    (let ((current-col (+ 1 (length (session-name session))))
          (separator-width (cl-tmux/renderer::%visible-length
                            (cl-tmux/options:get-option "window-status-separator" " ")))
          (first-p t))
      (loop for window in (session-windows session)
            do (unless first-p
                 (incf current-col separator-width))
               (setf first-p nil)
               (let ((entry-len (window-entry-width window)))
                 (when (and (>= col current-col)
                            (< col (+ current-col entry-len)))
                   (return window))
                 (incf current-col entry-len))))))

(defun %mouse-status-bar-click (session col)
  "Handle a click at COL on the status bar row: select the clicked window."
  (let ((window (%status-col-to-window session col)))
    (when window
      (%with-window-focus-transition (session)
        (session-select-window session window)))))

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

(defun %mouse-key-name (btn release-p location)
  "Build the tmux mouse key name for a mouse event, e.g. \"WheelUpPane\",
   \"MouseDown1Pane\", \"MouseUp3Status\".  BTN is the X10 button code, RELEASE-P
   selects MouseUp vs MouseDown, and LOCATION is \"Pane\"/\"Status\"/\"Border\".
   Returns NIL for events with no standard binding name (motion/drag, unknown
   buttons), so the caller falls back to the built-in mouse behaviour.

   These names are exactly what %parse-key-token stores for `bind -n WheelUpPane`
   / `bind -n MouseDown1Pane` (multi-char tokens are kept as strings), so the
   result doubles as a root key-table lookup key."
  (let ((button (cond
                  ((= btn +mouse-btn-left+)        "1")
                  ((= btn +mouse-btn-middle+)      "2")
                  ((= btn 2)                       "3")   ; right button (no named constant)
                  (t nil))))
    (cond
      ((= btn +mouse-btn-scroll-up+)   (concatenate 'string "WheelUp" location))
      ((= btn +mouse-btn-scroll-down+) (concatenate 'string "WheelDown" location))
      (button (concatenate 'string (if release-p "MouseUp" "MouseDown")
                           button location))
      (t nil))))

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
         (let* ((active-window (session-active-window session))
                (active-pane   (session-active-pane session)))
           (cond
             ;; Mouse option disabled - mark dirty (redraws are gated on it) and bail.
             ((not (cl-tmux/options:get-option "mouse"))
              nil)
             ;; When the pane under the pointer has requested mouse tracking, translate
             ;; and forward the event to the pane's PTY. This takes priority over all
             ;; tmux-UI mouse handling (copy-mode, resize, pane-select).
             ((%try-mouse-passthrough active-window active-pane btn col row release-p)
              nil)
             ;; Built-in / user-binding handling.
             (t
              (let* ((status-row    (1- *term-rows*))
                     (in-status     (= row status-row))
                     (location      (cond (in-status "Status")
                                          ((and active-window
                                                (%border-at-position active-window col row))
                                           "Border")
                                          (t "Pane")))
                     (mouse-key     (%mouse-key-name btn release-p location))
                     ;; When the active pane is in copy mode, a copy-mode-table mouse binding
                     ;; (e.g. `bind -T copy-mode-vi WheelUpPane send -X halfpage-up`) takes
                     ;; precedence over both the root binding and the built-in handling.
                     (active-screen (and active-pane (pane-screen active-pane)))
                     (in-copy       (and active-screen (screen-copy-mode-p active-screen)))
                     (copy-table    (%active-copy-mode-table)))
                ;; User mouse binding wins over the built-in handling: copy-mode table first
                ;; (when in copy mode), then the root table.
                (unless (or (and in-copy (%try-bound-string-key session copy-table mouse-key))
                            (%try-bound-string-key session +table-root+ mouse-key))
                  (cond
                    ;; ── Status bar click ────────────────────────────────────────────────────
                    ((and in-status (not release-p) (= btn +mouse-btn-left+))
                     (%mouse-status-bar-click session col))

                    ;; ── Scroll wheel up: enter copy-mode + scroll back ───────────────────
                    ((= btn +mouse-btn-scroll-up+)
                     (when active-screen
                       (unless (screen-copy-mode-p active-screen)
                         (copy-mode-enter active-screen))
                       (copy-mode-scroll active-screen 3)))

                    ;; ── Scroll wheel down: scroll forward, exit copy-mode at bottom ──────
                    ((= btn +mouse-btn-scroll-down+)
                     (when active-screen
                       (copy-mode-scroll active-screen -3)
                       (when (and (screen-copy-mode-p active-screen)
                                  (zerop (screen-copy-offset active-screen)))
                         (copy-mode-exit active-screen))))

                    ;; ── Left button press ─────────────────────────────────────────────────
                    ((and (= btn +mouse-btn-left+) (not release-p) (not in-status))
                     ;; Double/triple-click detection: a click at the same cell within
                     ;; double-click-time of the previous one selects a word (2) or line (3+),
                     ;; matching tmux's default DoubleClick1Pane / TripleClick1Pane bindings.
                     (let* ((now   (%now-ms))
                            (count (%mouse-click-count *last-mouse-click* now row col
                                                       (or (cl-tmux/options:get-option "double-click-time")
                                                           500))))
                       (setf *last-mouse-click* (list now row col count))
                       (when active-window
                         ;; Check for border drag
                         (multiple-value-bind (split orient)
                             (%border-at-position active-window col row)
                           (if split
                               ;; Press on border: begin drag (store only what motion events need)
                               (setf *mouse-drag-state* (list split orient))
                               ;; Press in pane: focus pane and begin/extend the copy selection
                               (let ((target-pane (pane-at-position active-window col row)))
                                 (when target-pane
                                   ;; %select-pane-with-focus so clicking a pane delivers ?1004
                                   ;; focus events, consistent with keyboard pane switches.
                                   (%select-pane-with-focus active-window target-pane)
                                   (let* ((screen    (pane-screen target-pane))
                                          (pane-col  (- col (pane-x target-pane)))
                                          (pane-row  (- row (pane-y target-pane))))
                                     (unless (screen-copy-mode-p screen)
                                       (copy-mode-enter screen))
                                     ;; Route cursor mutation through the commands layer.
                                     (copy-mode-set-cursor screen pane-row pane-col)
                                     (cond
                                       ((= count 2)  (copy-mode-select-word screen))
                                       ((>= count 3) (copy-mode-begin-line-selection screen))
                                       (t            (copy-mode-begin-selection screen)))))))))))

                    ;; ── Left button release: finalize selection or end drag ───────────────
                    ((and (= btn +mouse-btn-left+) release-p)
                     (if *mouse-drag-state*
                         (setf *mouse-drag-state* nil)
                         (when (and active-window active-pane)
                           (let ((screen (pane-screen active-pane)))
                             (when (and (screen-copy-mode-p screen)
                                        (screen-copy-selecting screen))
                               (copy-mode-yank screen))))))

                    ;; ── Middle button press: paste the top paste-buffer into the pane ─────
                    ;; xterm-style middle-click paste. Focuses the pane under the pointer and
                    ;; writes the most recent paste-buffer (honouring bracketed-paste mode).
                    ((and (= btn +mouse-btn-middle+) (not release-p) (not in-status))
                     (when active-window
                       (let ((target-pane (pane-at-position active-window col row)))
                         (when target-pane
                           (%select-pane-with-focus active-window target-pane)
                           (let ((text (cl-tmux/buffer:get-paste-buffer 0)))
                             (when text
                               (%paste-to-pane target-pane text)))))))

                    ;; ── Mouse motion with button 1 (btn 32): drag selection or resize ─────
                    ((= btn +mouse-btn-motion+)
                     (if *mouse-drag-state*
                         ;; Border drag in progress - only split and orientation are stored
                         (destructuring-bind (split orient) *mouse-drag-state*
                           (when active-window
                             (%apply-drag-resize active-window split orient col row)))
                         ;; Motion in pane: update copy selection cursor
                         (when (and active-window active-pane)
                           (let* ((target-pane  (pane-at-position active-window col row))
                                  (screen       (and target-pane (pane-screen target-pane))))
                             (when (and screen (screen-copy-mode-p screen) (screen-copy-selecting screen))
                               (let ((pane-col (- col (pane-x target-pane)))
                                     (pane-row (- row (pane-y target-pane))))
                                 ;; Route cursor mutation through the commands layer.
                                 (copy-mode-set-cursor screen pane-row pane-col)))))))

                    (t nil))))))
      (setf *dirty* t)))))

;;; ── Overlay pager escape-sequence handler ────────────────────────────────────
;;;
;;; When the overlay pager is active and ESC is received, we accumulate the byte
;;; sequence.  ESC [ A (Up) scrolls -1 and ESC [ B (Down) scrolls +1.  Any other
;;; sequence (including bare ESC) dismisses the overlay.
;;;
;;; The overlay escape handler uses two named continuation functions so each
;;; protocol state is explicit and independently readable.

(defun %overlay-escape-second-byte (buffer)
  "CPS state: received ESC, now reading the second byte.
   If the second byte is '[' we continue to %overlay-escape-final; otherwise dismiss."
  (lambda (_ignored-session byte)
    (declare (ignore _ignored-session))
    (vector-push-extend byte buffer)
    (if (= byte +byte-csi-bracket+)
        (values nil (%overlay-escape-final buffer))
        (progn
          (clear-overlay)
          (setf *dirty* t)
          (values nil #'%ground-input-state)))))

(defun %overlay-escape-final (buffer)
  "CPS state: received ESC '[', now reading the final byte.
   Up arrow scrolls -1; Down arrow scrolls +1; anything else dismisses."
  (lambda (_ignored-session byte)
    (declare (ignore _ignored-session))
    (vector-push-extend byte buffer)
    (cond
      ;; ESC [ A — Up arrow: scroll overlay up
      ((= byte +byte-arrow-up+)
       (overlay-scroll -1)
       (setf *dirty* t)
       (values nil #'%ground-input-state))
      ;; ESC [ B — Down arrow: scroll overlay down
      ((= byte +byte-arrow-down+)
       (overlay-scroll 1)
       (setf *dirty* t)
       (values nil #'%ground-input-state))
      ;; Unrecognised final byte: dismiss the overlay
      (t
       (clear-overlay)
       (setf *dirty* t)
       (values nil #'%ground-input-state)))))
