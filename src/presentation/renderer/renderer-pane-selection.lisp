(in-package #:cl-tmux/renderer)

;;; Selection bounds and hit-testing for pane rendering.

(defun in-selection-p (row col sel-start-r sel-end-r sel-start-c sel-end-c &optional rect-p)
  "Return T when (ROW, COL) falls within the selection.
   RECT-P non-nil: rectangle mode — any cell in [start-r..end-r] x [start-c..end-c).
   Default (character mode): the standard stream-of-characters selection logic."
  (if rect-p
      (and (<= sel-start-r row sel-end-r)
           (<= sel-start-c col)
           (< col sel-end-c))
      (cond
        ((= sel-start-r sel-end-r row)
         (and (<= sel-start-c col) (< col sel-end-c)))
        ((= row sel-start-r) (>= col sel-start-c))
        ((= row sel-end-r)   (< col sel-end-c))
        (t (and (> row sel-start-r) (< row sel-end-r))))))

(defun %compute-selection-bounds (screen)
  "Compute normalised selection boundary coordinates for SCREEN's copy-mode selection.
   Returns (values sel-active sel-start-row sel-end-row sel-start-col sel-end-col
                  sel-rect-p sel-mark-row sel-mark-col).
   sel-active is NIL when the selection prerequisites (selecting flag, mark, cursor)
   are not all present.  Rows are VIEWPORT rows (0..height-1) so in-selection-p and
   screen-display-cell work directly.  The computation uses virtual rows internally so
   a selection started before scrolling highlights the correct cells after scrolling.
   sel-mark-row is the clamped viewport row used for selection rendering.
   sel-rect-p is T when rectangle-select mode is active (screen-copy-rect-select-p)."
  (if (and (screen-copy-selecting screen)
           (consp (screen-copy-mark screen))
           (consp (screen-copy-cursor screen)))
      (let* ((mark        (screen-copy-mark screen))
             (cursor      (screen-copy-cursor screen))
             (mark-col    (cdr mark))
             (cursor-col  (cdr cursor))
             (sb-n        (length (screen-scrollback screen)))
             (mark-offset (screen-copy-mark-offset screen))
             (cur-offset  (screen-copy-offset screen))
             (h           (screen-height screen))
             (rect-p      (screen-copy-rect-select-p screen))
             ;; Convert viewport rows to virtual rows so selection stays stable
             ;; across scrollback changes.
             (mark-vrow   (+ sb-n (car mark) (- mark-offset)))
             (cur-vrow    (+ sb-n (car cursor) (- cur-offset)))
             (start-vrow  (min mark-vrow cur-vrow))
             (end-vrow    (max mark-vrow cur-vrow))
             (col-min     (min mark-col cursor-col))
             (col-max     (max mark-col cursor-col)))
        (flet ((viewport-row (vrow)
                 (max 0 (min (1- h) (+ vrow cur-offset (- sb-n)))))
               (selection-start-col ()
                 (cond
                   (rect-p col-min)
                   ((< mark-vrow cur-vrow) mark-col)
                   ((> mark-vrow cur-vrow) cursor-col)
                   (t col-min)))
               (selection-end-col ()
                 (cond
                   (rect-p (1+ col-max))
                   ((< mark-vrow cur-vrow) cursor-col)
                   ((> mark-vrow cur-vrow) mark-col)
                   (t col-max))))
          (values t
                  (viewport-row start-vrow)
                  (viewport-row end-vrow)
                  (selection-start-col)
                  (selection-end-col)
                  rect-p
                  (viewport-row mark-vrow)
                  mark-col)))
      (values nil 0 0 0 0 nil 0 0)))
