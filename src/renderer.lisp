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
  "Window-tab string: active window in brackets, others plain."
  (with-output-to-string (ws)
    (dolist (w (session-windows session))
      (if (eq w active-win)
          (format ws " [~A]" (window-name w))
          (format ws "  ~A " (window-name w))))))

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

(defun %status-sgr-from-style (style-str)
  "Return a partial SGR string for STYLE-STR (e.g. \"fg=colour2,bg=colour4\").
   Returns the default blue-on-white SGR \"44;97\" when style-str is empty/nil."
  ;; Simple pass-through: if the user set something non-empty, use it raw.
  ;; A full color-string parser is in cl-tmux/format but would add circular deps.
  ;; For now, return the default if style-str is empty.
  (if (and style-str (not (string= style-str "")))
      style-str
      "44;97"))

(defun %status-window-list-styled (session active-win)
  "Window-tab string with current-style applied to the active window entry.
   Uses window-status-current-style option for the active window."
  (let ((current-style (cl-tmux/options:get-option "window-status-current-style" "")))
    (with-output-to-string (ws)
      (dolist (w (session-windows session))
        (if (eq w active-win)
            (progn
              ;; Apply current-style if set
              (when (and current-style (not (string= current-style "")))
                (format ws "~C[~Am" +esc+ current-style))
              (format ws " [~A]" (window-name w))
              (when (and current-style (not (string= current-style "")))
                (format ws "~C[0m" +esc+)))
            (format ws "  ~A " (window-name w)))))))

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
   By default STATUS-ROW is the bottom row (terminal-rows - 1).
   Respects status-style, status-justify, and window-status-current-style options."
  (declare (ignore terminal-rows))
  (let* ((active-win (session-active-window session))
         (ap         (session-active-pane session))
         (ctx        (cl-tmux/format:format-context-from-session session active-win ap))
         (left       (if (prompt-active-p)
                         (prompt-text)
                         (%status-format-or-default
                          "status-left" ctx
                          (lambda () (%status-left-text session active-win ap)))))
         (right-str  (%status-format-or-default
                      "status-right" ctx #'%status-current-time))
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

;;; ── Popup rendering ─────────────────────────────────────────────────────────

(defun render-popup (stream popup terminal-rows terminal-cols)
  "Draw the POPUP overlay box centered on the terminal.
   When the popup has a live pane, render it inside the box.
   Otherwise render an empty box with the popup title."
  (declare (ignore terminal-rows))
  (let* ((pw     (min (popup-width  popup) terminal-cols))
         (ph     (popup-height popup))
         (ox     (max 0 (floor (- terminal-cols pw) 2)))
         (oy     (max 0 (floor (- (1- terminal-rows) ph) 2)))
         (title  (popup-title popup)))
    (reset-attrs stream)
    ;; Top border: ┌─ title ─┐
    (move-to stream oy ox)
    (write-char #\┌ stream)
    (let* ((inner (- pw 2))
           (tlabel (format nil " ~A " title))
           (tlen   (min (length tlabel) inner))
           (fill   (max 0 (- inner tlen))))
      (write-string (subseq tlabel 0 tlen) stream)
      (loop repeat fill do (write-char #\─ stream)))
    (write-char #\┐ stream)
    ;; Middle rows: │ content │
    (if (popup-pane popup)
        (let ((sc (popup-screen popup)))
          (when sc
            (loop for row below (min ph (popup-height popup)) do
              (move-to stream (+ oy 1 row) ox)
              (write-char #\│ stream)
              (loop for col below (- pw 2)
                    for cell = (screen-display-cell sc col row)
                    do (write-char (cell-char cell) stream))
              (write-char #\│ stream))))
        ;; Empty popup — just draw side bars
        (loop for row below (- ph 2) do
          (move-to stream (+ oy 1 row) ox)
          (write-char #\│ stream)
          (loop repeat (- pw 2) do (write-char #\Space stream))
          (write-char #\│ stream)))
    ;; Bottom border: └──┘
    (move-to stream (+ oy ph -1) ox)
    (write-char #\└ stream)
    (loop repeat (- pw 2) do (write-char #\─ stream))
    (write-char #\┘ stream)))

;;; ── Menu rendering ──────────────────────────────────────────────────────────

(defun render-menu (stream menu terminal-rows terminal-cols)
  "Draw the MENU overlay box centered on the terminal."
  (let* ((items  (menu-items menu))
         (n      (length items))
         (title  (menu-title menu))
         (pw     (min 40 terminal-cols))
         (ph     (+ n 2))           ; title row + n items + border rows
         (ox     (max 0 (floor (- terminal-cols pw) 2)))
         (oy     (max 0 (floor (- terminal-rows ph) 2)))
         (sel    (menu-selected-index menu)))
    (reset-attrs stream)
    ;; Top border
    (move-to stream oy ox)
    (write-char #\┌ stream)
    (let* ((inner (- pw 2))
           (tlabel (format nil " ~A " title))
           (tlen   (min (length tlabel) inner))
           (fill   (max 0 (- inner tlen))))
      (write-string (subseq tlabel 0 tlen) stream)
      (loop repeat fill do (write-char #\─ stream)))
    (write-char #\┐ stream)
    ;; Item rows
    (loop for (label . _cmd) in items
          for i from 0
          do (move-to stream (+ oy 1 i) ox)
             (write-char #\│ stream)
             (write-char (if (= i sel) #\▶ #\Space) stream)
             (let* ((inner (- pw 3))
                    (llen  (min (length label) inner))
                    (fill  (max 0 (- inner llen))))
               (write-string (subseq label 0 llen) stream)
               (loop repeat fill do (write-char #\Space stream)))
             (write-char #\│ stream))
    ;; Bottom border
    (move-to stream (+ oy n 1) ox)
    (write-char #\└ stream)
    (loop repeat (- pw 2) do (write-char #\─ stream))
    (write-char #\┘ stream)))

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
    ;; Enable/disable mouse reporting on the outer terminal.
    ;; When the session "mouse" option is T, enable SGR + button-event tracking.
    ;; When false, disable basic mouse tracking on the outer terminal.
    ;; Per-pane screen-mouse-mode is also honoured for applications that set their
    ;; own mouse mode (e.g. vim's mouse=a); the session option gates cl-tmux's
    ;; own mouse handling.
    (let ((session-mouse (cl-tmux/options:get-option "mouse")))
      (if session-mouse
          (progn
            (format buf "~C[?1006h" +esc+)   ; SGR extended encoding
            (format buf "~C[?1002h" +esc+))  ; button-event tracking
          (when ap
            (let* ((sc  (pane-screen ap))
                   (mm  (screen-mouse-mode sc))
                   (sgr (screen-mouse-sgr-mode sc)))
              (when (> mm 0)
                (format buf "~C[?~Dh" +esc+ (case mm (1 1000) (2 1002) (t 1003)))
                (when sgr (format buf "~C[?1006h" +esc+)))))))
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
