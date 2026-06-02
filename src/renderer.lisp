(in-package #:cl-tmux/renderer)

;;;; Terminal renderer: composites all pane screens onto the real terminal.
;;;;
;;;; Uses raw ANSI/VT100 escape sequences only — no curses dependency.
;;;; Each render call does a full repaint, buffered in a string stream and
;;;; flushed in one write to minimise flicker.
;;;;
;;;; This file contains the status bar and session compositing logic.
;;;; ANSI escape-code primitives live in renderer-format.lisp.
;;;; Pane and border rendering live in renderer-pane.lisp.

;;; ── Status bar data formatters (pure) ─────────────────────────────────────

(defun %status-current-time ()
  "HH:MM string from the system clock."
  (multiple-value-bind (sec min hour) (get-decoded-time)
    (declare (ignore sec))
    (format nil "~2,'0D:~2,'0D" hour min)))

(defun %status-pane-indicator (ap)
  "Pane-number string for the status bar, or empty string."
  (if ap (format nil " #~D" (pane-id ap)) ""))

(defun %status-copy-indicator (ap)
  "Copy-mode scroll offset string, or empty string."
  (if (and ap
           (screen-copy-mode-p (pane-screen ap))
           (> (screen-copy-offset (pane-screen ap)) 0))
      (format nil " [COPY +~D]" (screen-copy-offset (pane-screen ap)))
      ""))

(defun %status-window-list (session active-win)
  "Window-tab string: active window in brackets, others plain.
   Uses window-status-format, window-status-current-format, and
   window-status-separator options when available."
  (let ((fmt-normal  (cl-tmux/options:get-option "window-status-format"
                                                 " #{window_index}:#{window_name} "))
        (fmt-current (cl-tmux/options:get-option "window-status-current-format"
                                                 " #{window_index}:#{window_name}* "))
        (separator   (cl-tmux/options:get-option "window-status-separator" " ")))
    (with-output-to-string (ws)
      (let ((first-p t))
        (dolist (w (session-windows session))
          (unless first-p (write-string separator ws))
          (setf first-p nil)
          (let* ((ctx  (cl-tmux/format:format-context-from-window session w))
                 (active-p (eq w active-win))
                 (label (cl-tmux/format:expand-format
                         (if active-p fmt-current fmt-normal)
                         ctx)))
            (write-string label ws)))))))

(defun %status-left-text (session active-win ap)
  "Left portion of the status bar: prompt text or session/window/pane info."
  (if (prompt-active-p)
      (prompt-text)
      (format nil " ~A~A~A~A"
              (session-name session)
              (%status-window-list session active-win)
              (%status-pane-indicator ap)
              (%status-copy-indicator ap))))

(defun %status-bar-line (left time-str terminal-cols)
  "Assemble the full status bar string: left text, gap, time, truncated to TERMINAL-COLS."
  (let* ((gap  (max 0 (- terminal-cols (length left) (length time-str) 1)))
         (line (format nil "~A~A ~A" left (make-string gap :initial-element #\Space) time-str)))
    (subseq line 0 (min (length line) terminal-cols))))

(defun %status-format-or-default (opt-name ctx default-fn)
  "Return the expanded format string for OPT-NAME if the option has been
   set to a non-nil value that differs from the registered default;
   otherwise call DEFAULT-FN."
  (let* ((spec    (gethash opt-name cl-tmux/options:*option-registry*))
         (default (when spec (cl-tmux/options:option-spec-default spec)))
         (current (cl-tmux/options:get-option opt-name nil)))
    (if (and current (not (equal current default)))
        (cl-tmux/format:expand-format current ctx)
        (funcall default-fn))))

(defun %status-window-list-styled (session active-win)
  "Window-tab string with current-style applied to the active window entry.
   Uses window-status-format, window-status-current-format, window-status-separator,
   window-status-current-style, and window-status-style options."
  (let ((fmt-normal  (cl-tmux/options:get-option "window-status-format"
                                                 " #{window_index}:#{window_name} "))
        (fmt-current (cl-tmux/options:get-option "window-status-current-format"
                                                 " #{window_index}:#{window_name}* "))
        (separator   (cl-tmux/options:get-option "window-status-separator" " "))
        (current-style (cl-tmux/options:get-option "window-status-current-style" ""))
        (normal-style  (cl-tmux/options:get-option "window-status-style" "")))
    (with-output-to-string (ws)
      (let ((first-p t))
        (dolist (w (session-windows session))
          (unless first-p (write-string separator ws))
          (setf first-p nil)
          (let* ((ctx  (cl-tmux/format:format-context-from-window session w))
                 (active-p (eq w active-win))
                 (style    (if active-p current-style normal-style))
                 (label    (cl-tmux/format:expand-format
                            (if active-p fmt-current fmt-normal)
                            ctx)))
            ;; Apply per-window style using %status-sgr-from-style for proper SGR conversion.
            (let ((sgr (when (and style (plusp (length style)))
                         (%status-sgr-from-style style))))
              (when sgr
                (format ws "~C[~Am" +esc+ sgr))
              (write-string label ws)
              (when sgr
                (reset-attrs ws)))))))))

(defun %status-justify-line (left right-str cols justify)
  "Assemble the status bar according to JUSTIFY (:left :centre :right).
   COLS is the terminal width; result is truncated to COLS."
  (let* ((llen (length left))
         (rlen (length right-str)))
    (case (intern (string-upcase justify) :keyword)
      (:right
       ;; Right-justify the window list: place right text at far right
       (let* ((gap  (max 0 (- cols llen rlen 1)))
              (line (format nil "~A~A ~A" left
                            (make-string gap :initial-element #\Space)
                            right-str)))
         (subseq line 0 (min (length line) cols))))
      (:centre
       ;; Centre: pad left of window list
       (let* ((total (+ llen rlen))
              (pad-l (max 0 (floor (- cols total) 2)))
              (gap   (max 0 (- cols llen pad-l rlen)))
              (line  (format nil "~A~A~A ~A"
                             (make-string pad-l :initial-element #\Space)
                             left
                             (make-string gap :initial-element #\Space)
                             right-str)))
         (subseq line 0 (min (length line) cols))))
      (otherwise
       ;; Left (default) — same as %status-bar-line
       (%status-bar-line left right-str cols)))))

(defun render-status-bar (stream session terminal-rows terminal-cols
                          &key (status-row (1- terminal-rows)))
  "Draw the status bar at STATUS-ROW with dynamic format string expansion.
   STATUS-ROW defaults to (1- TERMINAL-ROWS), i.e. the bottom row.
   Respects status-style, status-justify, status-left-length, status-right-length,
   and window-status-current-style options."
  (let* ((active-win (session-active-window session))
         (ap         (session-active-pane session))
         (ctx        (cl-tmux/format:format-context-from-session session active-win ap))
         (raw-left   (if (prompt-active-p)
                         (prompt-text)
                         (%status-format-or-default
                          "status-left" ctx
                          (lambda () (%status-left-text session active-win ap)))))
         (raw-right  (%status-format-or-default
                      "status-right" ctx #'%status-current-time))
         ;; Enforce length limits
         (lmax       (cl-tmux/options:get-option "status-left-length" 40))
         (rmax       (cl-tmux/options:get-option "status-right-length" 40))
         (left       (if (> (length raw-left) lmax)
                         (subseq raw-left 0 lmax)
                         raw-left))
         (right-str  (if (> (length raw-right) rmax)
                         (subseq raw-right 0 rmax)
                         raw-right))
         (justify    (cl-tmux/options:get-option "status-justify" "left"))
         (line       (%status-justify-line left right-str terminal-cols justify))
         (sgr        (%status-sgr-from-style
                      (cl-tmux/options:get-option "status-style" ""))))
    (move-to stream status-row 0)
    (format stream "~C[~Am" +esc+ sgr)
    (write-string line stream)
    (reset-attrs stream)))

;;; ── Lock-screen overlay ─────────────────────────────────────────────────────

(defun render-lock-screen (stream terminal-rows terminal-cols)
  "Render a full-screen lock overlay.  Fills the screen with a solid colour
   and centres a 'Session locked' message."
  (reset-attrs stream)
  (format stream "~C[44;97m" +esc+)  ; blue background, bright white text
  ;; Fill all rows with spaces.
  (let ((blank-row (make-string terminal-cols :initial-element #\Space)))
    (loop for row below (1- terminal-rows)
          do (move-to stream row 0)
             (write-string blank-row stream)))
  ;; Centre the lock message.
  (let* ((msg      "Session locked — press any key to unlock")
         (mlen     (min (length msg) terminal-cols))
         (mid-row  (floor terminal-rows 2))
         (mid-col  (max 0 (floor (- terminal-cols mlen) 2))))
    (move-to stream mid-row mid-col)
    (write-string (subseq msg 0 mlen) stream))
  (reset-attrs stream))

;;; ── Overlay (list-keys help) ────────────────────────────────────────────────

(defun render-overlay (stream cols)
  "Draw the active overlay's lines over the top rows of the screen, each
   truncated to COLS columns, on default attributes."
  (reset-attrs stream)
  (loop for line in (overlay-lines)
        for row from 0
        do (move-to stream row 0)
           (write-string (subseq line 0 (min (length line) cols)) stream)))

(defun %render-mouse-sequences (stream ap)
  "Emit mouse-tracking mode sequences according to session and pane settings.
   When the session 'mouse' option is enabled, emit SGR + button-event sequences.
   Otherwise honour the active pane's screen-mouse-mode (X10/button-event/any-event)."
  (let ((session-mouse (cl-tmux/options:get-option "mouse")))
    (if session-mouse
        (progn
          (format stream "~C[?1006h" +esc+)
          (format stream "~C[?1002h" +esc+))
        (when ap
          (let* ((screen (pane-screen ap))
                 (mm     (screen-mouse-mode screen))
                 (sgr    (screen-mouse-sgr-mode screen)))
            (when (> mm 0)
              (format stream "~C[?~Dh" +esc+ (case mm (1 1000) (2 1002) (t 1003)))
              (when sgr (format stream "~C[?1006h" +esc+))))))))

;;; ── Full-session render ────────────────────────────────────────────────────

(defun render-session-to-string (session terminal-rows terminal-cols)
  "Compose a full frame for SESSION as an escape-sequence string.
   Does not touch *standard-output*; suitable for unit-testing without a TTY."
  (let* ((buf   (make-string-output-stream))
         (win   (session-active-window session))
         (panes (when win (window-panes win)))
         (ap    (session-active-pane session))
         ;; Check status bar options
         (status-on  (cl-tmux/options:get-option "status" t))
         (status-pos (cl-tmux/options:get-option "status-position" "bottom"))
         (status-row (if (equal status-pos "top") 0 (1- terminal-rows))))
    (cursor-invisible buf)
    ;; When the session is locked, render only the lock screen overlay.
    (when (session-locked-p session)
      (render-lock-screen buf terminal-rows terminal-cols)
      (return-from render-session-to-string (get-output-stream-string buf)))
    ;; Snapshot zoom state under the window lock to avoid a race with
    ;; window-zoom-toggle running on the main thread.
    (let ((zoomed nil) (tree nil))
      (when win
        (with-lock-held ((window-lock win))
          (setf zoomed (window-zoom-p win)
                tree   (window-tree   win))))
      (dolist (p panes) (render-pane buf p))
      (when (and tree (not zoomed))
        (render-tree-borders buf tree ap terminal-cols)))
    ;; Render popup overlay if active (takes priority over menu + overlay)
    (cond
      (*active-popup*
       (render-popup buf *active-popup* terminal-rows terminal-cols))
      (*active-menu*
       (render-menu buf *active-menu* terminal-rows terminal-cols))
      ((overlay-active-p)
       (render-overlay buf terminal-cols))
      (t
       (when ap
         (let ((screen (pane-screen ap)))
           (with-lock-held ((screen-lock screen))
             (move-to buf
                      (+ (pane-y ap) (screen-cursor-y screen))
                      (+ (pane-x ap) (screen-cursor-x screen))))))))
    ;; Render status bar when enabled
    (when status-on
      (render-status-bar buf session terminal-rows terminal-cols
                         :status-row status-row))
    (%render-mouse-sequences buf ap)
    ;; Emit and clear a pending BEL from the active pane.
    (when ap
      (let ((screen (pane-screen ap)))
        (when (screen-bell-pending screen)
          (write-char (code-char 7) buf)
          (setf (screen-bell-pending screen) nil))))
    ;; Restore cursor visibility according to the active pane's DECTCEM state.
    (when (or (null ap) (screen-cursor-visible (pane-screen ap)))
      (cursor-visible buf)
      (when ap
        (set-cursor-shape buf (screen-cursor-shape (pane-screen ap)))))
    (get-output-stream-string buf)))

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
;;; disable-mouse-reporting reverses all three with the corresponding ?Nh
;;; → ?Nl sequences.
;;;
;;; Call enable-mouse-reporting once at startup when (get-option "mouse") is
;;; true.  The render pipeline also re-emits these sequences on each repaint
;;; (render-session-to-string lines 461-465), so these helpers are primarily
;;; for explicit startup/shutdown use.

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
