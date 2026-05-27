(in-package #:cl-tmux/renderer)

;;;; Terminal renderer: composites all pane screens onto the real terminal.
;;;;
;;;; Uses raw ANSI/VT100 escape sequences only — no curses dependency.
;;;; Each render call does a full repaint, buffered in a string stream and
;;;; flushed in one write to minimise flicker.

(defconstant +esc+ #\Escape)

;;; ── Escape-code helpers ────────────────────────────────────────────────────

(defun move-to (stream row col)
  "ESC[row;colH — cursor absolute position, 1-based."
  (format stream "~C[~D;~DH" +esc+ (1+ row) (1+ col)))

(defun set-attrs (stream fg bg attrs)
  "Emit SGR (Select Graphic Rendition) codes for the given cell attributes."
  (format stream "~C[0" +esc+)                           ; reset first
  (when (logbitp 0 attrs) (write-string ";1" stream))    ; bold
  (when (logbitp 1 attrs) (write-string ";2" stream))    ; dim
  (when (logbitp 2 attrs) (write-string ";7" stream))    ; reverse video
  (cond ((<= 0 fg  7) (format stream ";~D" (+ 30 fg)))
        ((<= 8 fg 15) (format stream ";~D" (+ 82 fg))))  ; 90..97 → 82+fg
  (cond ((<= 0 bg  7) (format stream ";~D" (+ 40 bg)))
        ((<= 8 bg 15) (format stream ";~D" (+ 92 bg))))  ; 100..107 → 92+bg
  (write-char #\m stream))

(defun cursor-invisible (stream)
  (format stream "~C[?25l" +esc+))

(defun cursor-visible (stream)
  (format stream "~C[?25h" +esc+))

(defun reset-attrs (stream)
  (format stream "~C[0m" +esc+))

;;; ── Pane ───────────────────────────────────────────────────────────────────

(defun render-pane (stream pane)
  "Draw the pane's screen into the real terminal at the pane's (x, y) offset."
  (let* ((screen (pane-screen pane))
         (pw     (pane-width   pane))
         (ph     (pane-height  pane))
         (ox     (pane-x      pane))
         (oy     (pane-y      pane)))
    (with-lock-held ((screen-lock screen))
      (let ((prev-fg -1) (prev-bg -1) (prev-attrs -1))
        (loop for row below ph do
          (move-to stream (+ oy row) ox)
          (loop for col below pw
                for cell  = (screen-cell screen col row)
                for fg    = (cell-fg    cell)
                for bg    = (cell-bg    cell)
                for attrs = (cell-attrs cell)
                do (unless (and (= fg prev-fg) (= bg prev-bg) (= attrs prev-attrs))
                     (set-attrs stream fg bg attrs)
                     (setf prev-fg fg prev-bg bg prev-attrs attrs))
                   (write-char (cell-char cell) stream))))
      (screen-clear-dirty screen))))

(defun render-vertical-border (stream pane)
  "Draw a vertical separator bar in the reserved column to PANE's right."
  (reset-attrs stream)
  (let ((border-col (+ (pane-x pane) (pane-width pane)))
        (oy         (pane-y    pane))
        (ph         (pane-height pane)))
    (loop for row below ph do
      (move-to stream (+ oy row) border-col)
      (write-char #\│ stream))))

(defun render-horizontal-border (stream pane terminal-cols)
  "Draw a horizontal separator bar in the reserved row below PANE."
  (reset-attrs stream)
  (let ((border-row (+ (pane-y pane) (pane-height pane)))
        (ox         (pane-x pane))
        (pw         (min (pane-width pane) (- terminal-cols (pane-x pane)))))
    (move-to stream border-row ox)
    (loop repeat pw do (write-char #\─ stream))))

;;; ── Status bar ─────────────────────────────────────────────────────────────

(defun render-status-bar (stream session terminal-rows terminal-cols)
  "Draw the bottom status bar showing session name, window list, and time."
  (let* ((active-win (session-active-window session))
         (time-str
          (multiple-value-bind (s m h) (get-decoded-time)
            (declare (ignore s))
            (format nil "~2,'0D:~2,'0D" h m)))
         (win-list
          (with-output-to-string (ws)
            (dolist (w (session-windows session))
              (if (eq w active-win)
                  (format ws " [~A]" (window-name w))
                  (format ws "  ~A " (window-name w)))))))
    (move-to stream (1- terminal-rows) 0)
    (format stream "~C[44;97m" +esc+)          ; bright white on blue
    (let* ((left  (format nil " ~A~A" (session-name session) win-list))
           (gap   (max 0 (- terminal-cols (length left) (length time-str) 1)))
           (line  (format nil "~A~A ~A"
                          left
                          (make-string gap :initial-element #\Space)
                          time-str)))
      (write-string (subseq line 0 (min (length line) terminal-cols)) stream))
    (reset-attrs stream)))

;;; ── Full-session render ────────────────────────────────────────────────────

(defun render-session (session terminal-rows terminal-cols)
  "Repaint all panes and the status bar; flush to *standard-output* in one write."
  (let ((buf (make-string-output-stream)))
    (cursor-invisible buf)

    (let* ((win   (session-active-window session))
           (panes (when win (window-panes win))))
      ;; Render pane contents
      (dolist (p panes)
        (render-pane buf p))
      ;; Separators between adjacent panes (direction set by the split).
      (when (> (length panes) 1)
        (ecase (window-layout win)
          (:vertical
           (loop for p in (butlast panes)
                 when (< (+ (pane-x p) (pane-width p)) terminal-cols)
                   do (render-vertical-border buf p)))
          (:horizontal
           (loop for p in (butlast panes)
                 do (render-horizontal-border buf p terminal-cols)))
          ((nil) nil)))
      ;; Move cursor to the active pane's cursor position
      (let ((ap (session-active-pane session)))
        (when ap
          (let ((screen (pane-screen ap)))
            (with-lock-held ((screen-lock screen))
              (move-to buf
                       (+ (pane-y ap) (screen-cursor-y screen))
                       (+ (pane-x ap) (screen-cursor-x screen))))))))

    (render-status-bar buf session terminal-rows terminal-cols)
    (cursor-visible buf)

    ;; Single atomic write keeps flicker minimal
    (write-string (get-output-stream-string buf))
    (force-output)))

(defun clear-display ()
  "Erase the entire terminal and move cursor home."
  (format t "~C[2J~C[H" +esc+ +esc+)
  (force-output))
