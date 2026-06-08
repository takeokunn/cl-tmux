(in-package #:cl-tmux/renderer)

;;;; Session-frame compositing for the cl-tmux renderer.
;;;;
;;;; This file owns the full-frame pipeline: lock-screen overlay, pane/border
;;;; rendering, overlay dispatch, mouse sequences, bell emission, cursor
;;;; restoration, and the render-session / render-session-to-string entry points.
;;;;
;;;; Status-bar composition lives in renderer-statusbar.lisp (loaded just before
;;;; this file).
;;;;
;;;; Load order: renderer-format → renderer-style → renderer-pane
;;;;             → renderer-overlay → renderer-statusbar → renderer-compose

;;; ── Lock-screen overlay ─────────────────────────────────────────────────────

(defun render-lock-screen (stream terminal-rows terminal-cols)
  "Render a full-screen lock overlay.  Fills the screen with a solid colour
   and centres a 'Session locked' message."
  (reset-attrs stream)
  (format stream "~C[~Am" +esc+ +sgr-default-status+)
  ;; Fill all rows with spaces.
  (let ((blank-row (make-string terminal-cols :initial-element #\Space)))
    (loop for row below (1- terminal-rows)
          do (move-to stream row 0)
             (write-string blank-row stream)))
  ;; Centre the lock message.
  (let* ((msg     "Session locked — press any key to unlock")
         (mlen    (min (length msg) terminal-cols))
         (mid-row (floor terminal-rows 2))
         (mid-col (max 0 (floor (- terminal-cols mlen) 2))))
    (move-to stream mid-row mid-col)
    (write-string (subseq msg 0 mlen) stream))
  (reset-attrs stream))

;;; ── Overlay (list-keys help) ────────────────────────────────────────────────

(defun render-overlay (stream cols)
  "Draw the active overlay's lines over the top rows of the screen.
   Applies the message-style option (or message-command-style when a prompt is
   active) so overlays respect the user's colour scheme."
  (let* ((style-opt (if (prompt-active-p)
                        (cl-tmux/options:get-option "message-command-style" "")
                        (cl-tmux/options:get-option "message-style" "")))
         (sgr-code  (when (and style-opt (plusp (length style-opt)))
                      (%status-sgr-from-style style-opt))))
    (if sgr-code
        (format stream "~C[~Am" +esc+ sgr-code)
        (reset-attrs stream)))
  (loop for line in (overlay-lines)
        for row from 0
        do (move-to stream row 0)
           (write-string (subseq line 0 (min (length line) cols)) stream)))

;;; ── Mouse-mode DEC private mode dispatch table ──────────────────────────────
;;;
;;; define-mouse-mode-sequence maps a screen-mouse-mode integer to the
;;; DEC private mode number to enable:
;;;   mouse_mode(1) → ?1000h  (X10: press only)
;;;   mouse_mode(2) → ?1002h  (button-event: press + release + held motion)
;;;   mouse_mode(3) → ?1003h  (any-event: all mouse motion, and default fallback)
;;;
;;; Pattern matches define-csi-rules style: one declarative rule per mode.

(defmacro define-mouse-mode-sequence (&rest rules)
  "Build %MOUSE-MODE-DEC-NUMBER from a declarative (mode-integer dec-mode-number) table.
   The last entry's dec-mode-number is the default for any unmatched mode > 0."
  (let ((default-dec (second (car (last rules))))
        (explicit-rules (butlast rules)))
    `(defun %mouse-mode-dec-number (mode-integer)
       "Return the DEC private mode number for the given SCREEN-MOUSE-MODE integer."
       (cond
         ,@(mapcar (lambda (rule)
                     (destructuring-bind (mode-val dec-num) rule
                       `((= mode-integer ,mode-val) ,dec-num)))
                   explicit-rules)
         (t ,default-dec)))))

(define-mouse-mode-sequence
  (1 1000)   ; X10 basic mouse tracking (press only)
  (2 1002)   ; button-event tracking (press + release + motion while held)
  (3 1003))  ; any-event tracking (all motion) — also default fallback

(defun %render-mouse-sequences (stream active-pane)
  "Emit mouse-tracking mode sequences according to session and pane settings.
   When the session 'mouse' option is enabled, emit SGR + button-event sequences.
   Otherwise honour ACTIVE-PANE's screen-mouse-mode (X10/button-event/any-event)."
  (let ((session-mouse (cl-tmux/options:get-option "mouse")))
    (if session-mouse
        (progn
          (format stream "~C[?1006h" +esc+)
          (format stream "~C[?1002h" +esc+))
        (when active-pane
          (let* ((screen     (pane-screen active-pane))
                 (mouse-mode (screen-mouse-mode screen))
                 (sgr-mode   (screen-mouse-sgr-mode screen)))
            (when (> mouse-mode 0)
              (format stream "~C[?~Dh" +esc+ (%mouse-mode-dec-number mouse-mode))
              (when sgr-mode (format stream "~C[?1006h" +esc+))))))))

;;; ── Full-session render ────────────────────────────────────────────────────

(defun %render-panes-and-borders (buffer window panes active-pane terminal-cols)
  "Render all panes and split-tree borders for WINDOW into BUFFER.
   Snapshots zoom state under the window lock to avoid a race with
   window-zoom-toggle running on the main thread."
  (let ((zoomed nil) (tree nil))
    (when window
      (with-lock-held ((window-lock window))
        (setf zoomed (window-zoom-p window)
              tree   (window-tree   window))))
    (dolist (pane panes) (render-pane buffer pane))
    (when (and tree (not zoomed))
      (render-tree-borders buffer tree active-pane terminal-cols))))

(defun %render-overlay-layer (buffer active-pane terminal-rows terminal-cols)
  "Render the active overlay layer (popup > menu > overlay > cursor) into BUFFER."
  (cond
    (*active-popup*
     (render-popup buffer *active-popup* terminal-rows terminal-cols))
    (*active-menu*
     (render-menu buffer *active-menu* terminal-rows terminal-cols))
    ((overlay-active-p)
     (render-overlay buffer terminal-cols))
    (t
     (when active-pane
       (let ((screen (pane-screen active-pane)))
         (with-lock-held ((screen-lock screen))
           (move-to buffer
                    (+ (pane-y active-pane) (screen-cursor-y screen))
                    (+ (pane-x active-pane) (screen-cursor-x screen)))))))))

(defun %render-bell-and-cursor (buffer active-pane)
  "Emit a pending BEL from ACTIVE-PANE (if any) and restore cursor visibility.
   Bell consumption is gated on the 'bell-action' option:
   - 'any' (default): relay BEL from any pane
   - 'current': relay only from active pane (already filtered here since active-pane is the active one)
   - 'none': swallow all BELs silently
   - 'other': relay from non-active panes (not applicable here — active-pane IS active)
   Fires the alert-bell hook when a BEL is consumed."
  (when active-pane
    (let* ((bell-pending (screen-consume-bell (pane-screen active-pane)))
           (bell-action  (or (cl-tmux/options:get-option "bell-action") "any"))
           ;; Check if visual-bell is on — emit reverse-video flash instead of BEL
           (visual-bell  (cl-tmux/options:get-option "visual-bell"))
           (relay-bell   (and bell-pending
                              (not (string= bell-action "none")))))
      (when relay-bell
        (if visual-bell
            ;; Visual bell: brief reverse-video flash (SGR 7 + SGR 0)
            (format buffer "~C[7m~C[0m" +esc+ +esc+)
            ;; Audible bell: BEL character
            (write-char (code-char 7) buffer))
        (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-alert-bell+))))
  (when (or (null active-pane)
            (screen-cursor-visible (pane-screen active-pane)))
    (cursor-visible buffer)
    (when active-pane
      (set-cursor-shape buffer (screen-cursor-shape (pane-screen active-pane))))))

(defun %render-passthrough (buffer panes)
  "Drain each pane's passthrough-queue and emit the inner sequences to BUFFER
   (the outer terminal), oldest-first.  Gated on the allow-passthrough option:
   when 'off' (default) the queue is cleared without emitting; when 'on'/'all'
   the sequences are written through so tmux-in-tmux and iTerm2/kitty inline
   images reach the real terminal.  Drains under the screen lock since reader
   threads push to the queue concurrently."
  (let* ((mode  (or (cl-tmux/options:get-option "allow-passthrough" "off") "off"))
         (emit  (member mode '("on" "all") :test #'string=)))
    (dolist (pane panes)
      (let ((screen (pane-screen pane)))
        (when screen
          (with-lock-held ((screen-lock screen))
            (let ((queued (nreverse (screen-passthrough-queue screen))))
              (setf (screen-passthrough-queue screen) nil)
              (when emit
                (dolist (seq queued)
                  (write-string seq buffer))))))))))

(defun %render-clipboard (buffer panes)
  "Drain each pane's clipboard-queue and emit the OSC 52 sequences to BUFFER (the
   outer terminal) so a copy-mode yank reaches the host's system clipboard.  Gated
   on set-clipboard: 'off' clears the queue without emitting; 'on'/'external' write
   the sequences through.  Drains under the screen lock."
  (let* ((mode (or (cl-tmux/options:get-option "set-clipboard" "on") "on"))
         (emit (member mode '("on" "external") :test #'string=)))
    (dolist (pane panes)
      (let ((screen (pane-screen pane)))
        (when screen
          (with-lock-held ((screen-lock screen))
            (let ((queued (nreverse (screen-clipboard-queue screen))))
              (setf (screen-clipboard-queue screen) nil)
              (when emit
                (dolist (seq queued)
                  (write-string seq buffer))))))))))

(defun render-session-to-string (session terminal-rows terminal-cols)
  "Compose a full frame for SESSION as an escape-sequence string.
   Does not touch *standard-output*; suitable for unit-testing without a TTY."
  (let* ((buffer      (make-string-output-stream))
         (window      (session-active-window session))
         (panes       (when window (window-panes window)))
         (active-pane (session-active-pane session))
         ;; Status row count from the `status` option (0..5).  The pane layout
         ;; reserves the matching count via cl-tmux/config:*status-height*, kept
         ;; in sync by the `status` option's side-effect — so the bar and the
         ;; pane area stay in lockstep in normal use.
         (status-lines (status-line-count))
         (status-on   (> status-lines 0))
         (status-pos  (cl-tmux/options:get-option "status-position" "bottom")))
    (cursor-invisible buffer)
    (when (session-locked-p session)
      (render-lock-screen buffer terminal-rows terminal-cols)
      (return-from render-session-to-string (get-output-stream-string buffer)))
    (%render-panes-and-borders buffer window panes active-pane terminal-cols)
    ;; pane-border-status title lines (drawn after borders so they overwrite border cells)
    (when (and window panes
               (not (string= (cl-tmux/options:get-option "pane-border-status" "off") "off")))
      (dolist (pane panes)
        (%render-pane-border-status buffer pane session window)))
    ;; display-panes (C-b q): big per-pane numbers while the display-panes overlay
    ;; is active, coloured by display-panes-(active-)colour.  Drawn after borders so
    ;; the numbers overlay the pane content, before the top overlay layer.
    (when (and cl-tmux/prompt:*display-panes-active* (overlay-active-p) window panes)
      (dolist (pane panes)
        (%draw-pane-number-to-screen buffer (pane-x pane) (pane-y pane)
                                     (pane-width pane) (pane-height pane)
                                     (pane-id pane) (eq pane active-pane))))
    (%render-overlay-layer buffer active-pane terminal-rows terminal-cols)
    (when status-on
      (render-status-region buffer session terminal-rows terminal-cols
                            status-lines status-pos))
    (%render-mouse-sequences buffer active-pane)
    ;; allow-passthrough: emit any DCS-passthrough sequences (images, nested tmux).
    (when panes (%render-passthrough buffer panes))
    (when panes (%render-clipboard buffer panes))
    (%render-bell-and-cursor buffer active-pane)
    ;; set-titles: emit OSC 0 to set the outer terminal window title.
    (when (cl-tmux/options:get-option "set-titles")
      (let* ((title-fmt (cl-tmux/options:get-option "set-titles-string" "#W"))
             (win        (session-active-window session))
             (pane       (session-active-pane session))
             (ctx        (cl-tmux/format:format-context-from-session session win pane))
             (title      (cl-tmux/format:expand-format title-fmt ctx)))
        (format buffer "~C]0;~A~C" +esc+ title (code-char 7))))
    (get-output-stream-string buffer)))

(defun render-session (session terminal-rows terminal-cols)
  "Repaint all panes and the status bar; flush to *standard-output* in one write."
  (write-string (render-session-to-string session terminal-rows terminal-cols))
  (force-output))

(defun clear-display ()
  "Erase the entire terminal and move cursor home."
  (format t "~C[2J~C[H" +esc+ +esc+)
  (force-output))

;;; ── Mouse reporting control ────────────────────────────────────────────────
;;;
;;; enable-mouse-reporting emits the three DEC private mode sequences that
;;; instruct the outer terminal to send mouse events to cl-tmux's stdin:
;;;   ?1000h — X10 basic mouse tracking (press only)
;;;   ?1002h — button-event tracking (press + release + motion with button held)
;;;   ?1006h — SGR extended coordinate encoding (supports terminals > 223 cols)
;;;
;;; disable-mouse-reporting reverses all three with the corresponding ?Nh → ?Nl.
;;;
;;; Call enable-mouse-reporting once at startup when (get-option "mouse") is
;;; true.  The render pipeline also re-emits these sequences on each repaint
;;; via %render-mouse-sequences, so these helpers are primarily for explicit
;;; startup/shutdown use.

(defun enable-mouse-reporting ()
  "Emit DEC private mode sequences to enable mouse reporting on the outer terminal.
   Enables X10 tracking (?1000h), button-event tracking (?1002h), and SGR
   extended encoding (?1006h).  Flushes stdout immediately."
  (format t "~C[?1000h~C[?1002h~C[?1006h" +esc+ +esc+ +esc+)
  (force-output))

(defun disable-mouse-reporting ()
  "Emit DEC private mode sequences to disable mouse reporting on the outer terminal.
   Disables SGR encoding (?1006l), button-event tracking (?1002l), and X10
   tracking (?1000l).  Flushes stdout immediately."
  (format t "~C[?1006l~C[?1002l~C[?1000l" +esc+ +esc+ +esc+)
  (force-output))
