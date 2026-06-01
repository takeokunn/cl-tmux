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

(defun render-status-bar (stream session terminal-rows terminal-cols)
  "Draw the bottom status bar with dynamic format string expansion."
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
         (line       (%status-bar-line left right-str terminal-cols)))
    (move-to stream (1- terminal-rows) 0)
    (format stream "~C[44;97m" +esc+)
    (write-string line stream)
    (reset-attrs stream)))

;;; ── Overlay (list-keys help) ────────────────────────────────────────────────

(defun render-overlay (stream cols)
  "Draw the active overlay's lines over the top rows of the screen, each
   truncated to COLS columns, on default attributes."
  (reset-attrs stream)
  (loop for line in (overlay-lines)
        for row from 0
        do (move-to stream row 0)
           (write-string (subseq line 0 (min (length line) cols)) stream)))

;;; ── Full-session render ────────────────────────────────────────────────────

(defun render-session-to-string (session terminal-rows terminal-cols)
  "Compose a full frame for SESSION as an escape-sequence string.
   Does not touch *standard-output*; suitable for unit-testing without a TTY."
  (let* ((buf   (make-string-output-stream))
         (win   (session-active-window session))
         (panes (when win (window-panes win)))
         (ap    (session-active-pane session)))
    (cursor-invisible buf)
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
    (if (overlay-active-p)
        (render-overlay buf terminal-cols)
        (when ap
          (let ((screen (pane-screen ap)))
            (with-lock-held ((screen-lock screen))
              (move-to buf
                       (+ (pane-y ap) (screen-cursor-y screen))
                       (+ (pane-x ap) (screen-cursor-x screen)))))))
    (render-status-bar buf session terminal-rows terminal-cols)
    ;; Enable/disable mouse reporting on the outer terminal to match the active pane.
    ;; This causes the outer terminal to forward mouse events as escape sequences.
    (when ap
      (let* ((sc  (pane-screen ap))
             (mm  (screen-mouse-mode sc))
             (sgr (screen-mouse-sgr-mode sc)))
        (when (> mm 0)
          (format buf "~C[?~Dh" +esc+ (case mm (1 1000) (2 1002) (t 1003)))
          (when sgr (format buf "~C[?1006h" +esc+)))))
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
