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

(defun render-cell-attrs (stream fg bg attrs)
  "Emit SGR (Select Graphic Rendition) codes for the given cell attributes.
   Writes only SGR escape codes to STREAM; performs no other side effects."
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
                for cell  = (screen-display-cell screen col row)
                ;; A continuation cell (width 0) is the right half of a
                ;; double-width glyph the terminal already drew — emit nothing.
                unless (zerop (cell-width cell))
                  do (let ((fg    (cell-fg    cell))
                           (bg    (cell-bg    cell))
                           (attrs (cell-attrs cell)))
                       (unless (and (= fg prev-fg) (= bg prev-bg) (= attrs prev-attrs))
                         (render-cell-attrs stream fg bg attrs)
                         (setf prev-fg fg prev-bg bg prev-attrs attrs))
                       (write-char (cell-char cell) stream)))))
      (screen-clear-dirty screen))))

(defun render-vertical-border (stream pane activep)
  "Draw a vertical separator bar in the reserved column to PANE's right.
   When ACTIVEP is true, highlight the border in bright green (ESC[32m);
   otherwise use the default attribute reset."
  (let ((border-col (+ (pane-x pane) (pane-width pane)))
        (oy         (pane-y    pane))
        (ph         (pane-height pane)))
    (if activep
        (format stream "~C[32m" +esc+)   ; bright green for active pane border
        (reset-attrs stream))
    (loop for row below ph do
      (move-to stream (+ oy row) border-col)
      (write-char #\│ stream))
    (reset-attrs stream)))

(defun render-horizontal-border (stream pane terminal-cols)
  "Draw a horizontal separator bar in the reserved row below PANE."
  (reset-attrs stream)
  (let ((border-row (+ (pane-y pane) (pane-height pane)))
        (ox         (pane-x pane))
        (pw         (min (pane-width pane) (- terminal-cols (pane-x pane)))))
    (move-to stream border-row ox)
    (loop repeat pw do (write-char #\─ stream))))

;;; ── Split-tree separators ───────────────────────────────────────────────────

(defun render-tree-borders (stream node active-pane terminal-cols)
  "Walk the split-tree NODE, drawing one separator per internal node at the
   boundary between its two children.  A vertical (:h) split draws a │ column
   just right of its first subtree; a horizontal (:v) split draws a ─ row just
   below its first subtree.  The border directly adjacent to ACTIVE-PANE is
   highlighted (green for the vertical bar, matching RENDER-VERTICAL-BORDER)."
  (when (layout-split-p node)
    (let ((a (layout-split-first  node))
          (b (layout-split-second node)))
      (ecase (layout-split-orientation node)
        (:h                             ; left | right → vertical bar
         (let* ((rect (layout-subtree-rect a))
                (border-col (+ (getf rect :x) (getf rect :w)))
                ;; Highlight when either side abutting the bar is the active pane.
                (activep (or (subtree-contains-p a active-pane)
                             (subtree-contains-p b active-pane))))
           (when (< border-col terminal-cols)
             (if activep
                 (format stream "~C[32m" +esc+)
                 (reset-attrs stream))
             (loop for row from (getf rect :y) below (+ (getf rect :y) (getf rect :h))
                   do (move-to stream row border-col)
                      (write-char #\│ stream))
             (reset-attrs stream))))
        (:v                             ; top / bottom → horizontal bar
         (let* ((rect (layout-subtree-rect a))
                (border-row (+ (getf rect :y) (getf rect :h)))
                (x (getf rect :x))
                (w (min (getf rect :w) (- terminal-cols x))))
           (reset-attrs stream)
           (move-to stream border-row x)
           (loop repeat (max 0 w) do (write-char #\─ stream)))))
      (render-tree-borders stream a active-pane terminal-cols)
      (render-tree-borders stream b active-pane terminal-cols))))

(defun layout-subtree-rect (node)
  "Bounding rectangle of NODE's leaves as a plist (:x :y :w :h), derived from the
   already-laid-out pane geometry."
  (let ((panes (layout-leaves node)))
    (let ((min-x (reduce #'min panes :key #'pane-x))
          (min-y (reduce #'min panes :key #'pane-y))
          (max-x (reduce #'max panes :key (lambda (p) (+ (pane-x p) (pane-width p)))))
          (max-y (reduce #'max panes :key (lambda (p) (+ (pane-y p) (pane-height p))))))
      (list :x min-x :y min-y :w (- max-x min-x) :h (- max-y min-y)))))

(defun subtree-contains-p (node pane)
  "True when PANE is a leaf of NODE's subtree."
  (and pane (member pane (layout-leaves node))))

;;; ── Status bar ─────────────────────────────────────────────────────────────

(defun render-status-bar (stream session terminal-rows terminal-cols)
  "Draw the bottom status bar showing session name, window list, active pane
   number, copy-mode indicator, and time."
  (let* ((active-win (session-active-window session))
         (ap         (session-active-pane session))
         (time-str
          (multiple-value-bind (sec min hour) (get-decoded-time)
            (declare (ignore sec))
            (format nil "~2,'0D:~2,'0D" hour min))))
    (move-to stream (1- terminal-rows) 0)
    (format stream "~C[44;97m" +esc+)          ; bright white on blue
    (let* ((pane-indicator
            (if ap (format nil " #~D" (pane-id ap)) ""))
           (copy-indicator
            (if (and ap
                     (screen-copy-mode-p (pane-screen ap))
                     (> (screen-copy-offset (pane-screen ap)) 0))
                (format nil " [COPY +~D]" (screen-copy-offset (pane-screen ap)))
                ""))
           (left  (if (prompt-active-p)
                      (prompt-text)
                      (let ((win-list
                             (with-output-to-string (ws)
                               (dolist (w (session-windows session))
                                 (if (eq w active-win)
                                     (format ws " [~A]" (window-name w))
                                     (format ws "  ~A " (window-name w)))))))
                        (format nil " ~A~A~A~A"
                                (session-name session)
                                win-list
                                pane-indicator
                                copy-indicator))))
           (gap   (max 0 (- terminal-cols (length left) (length time-str) 1)))
           (line  (format nil "~A~A ~A"
                          left
                          (make-string gap :initial-element #\Space)
                          time-str)))
      (write-string (subseq line 0 (min (length line) terminal-cols)) stream))
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

   Does not touch *standard-output*; it only reads pane screens (under their
   locks) and returns the string that RENDER-SESSION writes.  Exposed so the
   renderer can be exercised without a real terminal."
  (let ((buf (make-string-output-stream)))
    (cursor-invisible buf)

    (let* ((win    (session-active-window session))
           (panes  (when win (window-panes win)))
           (ap     (session-active-pane session)))
      ;; Render pane contents
      (dolist (p panes)
        (render-pane buf p))
      ;; Separators.  With a split tree, draw one separator per internal node,
      ;; placed at the boundary between its two children.  Without a tree, fall
      ;; back to the legacy flat scheme (one separator per non-last pane).
      (cond
        ((and win (window-tree win))
         (render-tree-borders buf (window-tree win) ap terminal-cols))
        ((> (length panes) 1)
         (ecase (window-layout win)
           (:vertical
            (loop for p in (butlast panes)
                  ;; Highlight the border to the right of the active pane.
                  for activep = (eq p ap)
                  when (< (+ (pane-x p) (pane-width p)) terminal-cols)
                    do (render-vertical-border buf p activep)))
           (:horizontal
            (loop for p in (butlast panes)
                  do (render-horizontal-border buf p terminal-cols)))
           ((nil) nil))))
      ;; A help overlay covers the top rows; otherwise move the cursor to the
      ;; active pane's cursor position (visible only for the active pane).
      (if (overlay-active-p)
          (render-overlay buf terminal-cols)
          (when ap
            (let ((screen (pane-screen ap)))
              (with-lock-held ((screen-lock screen))
                (move-to buf
                         (+ (pane-y ap) (screen-cursor-y screen))
                         (+ (pane-x ap) (screen-cursor-x screen))))))))

    (render-status-bar buf session terminal-rows terminal-cols)
    (cursor-visible buf)
    (get-output-stream-string buf)))

(defun render-session (session terminal-rows terminal-cols)
  "Repaint all panes and the status bar; flush to *standard-output* in one write."
  ;; Single atomic write keeps flicker minimal.
  (write-string (render-session-to-string session terminal-rows terminal-cols))
  (force-output))

(defun clear-display ()
  "Erase the entire terminal and move cursor home."
  (format t "~C[2J~C[H" +esc+ +esc+)
  (force-output))
