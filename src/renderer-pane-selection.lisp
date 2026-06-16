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

(defun %selection-column (rect-p mark-is-start mark-is-end rect-col start-col end-col tie)
  "Choose the selection column boundary for one end of the current selection."
  (if rect-p
      rect-col
      (if mark-is-start
          start-col
          (if mark-is-end end-col tie))))

(defun %compute-selection-bounds (screen)
  "Compute normalised selection boundary coordinates for SCREEN's copy-mode selection.
   Returns (values sel-active sel-start-row sel-end-row sel-start-col sel-end-col sel-rect-p).
   sel-active is NIL when the selection prerequisites (selecting flag, mark, cursor)
   are not all present.  Rows are VIEWPORT rows (0..height-1) so in-selection-p and
   screen-display-cell work directly.  The computation uses virtual rows internally so
   a selection started before scrolling highlights the correct cells after scrolling.
   sel-rect-p is T when rectangle-select mode is active (screen-copy-rect-select-p)."
  (if (and (screen-copy-selecting screen)
           (consp (screen-copy-mark   screen))
           (consp (screen-copy-cursor screen)))
      (let* ((mark        (screen-copy-mark   screen))
             (cursor      (screen-copy-cursor screen))
             (mark-col    (cdr mark))
             (cursor-col  (cdr cursor))
             (sb-n        (length (screen-scrollback screen)))
             (mark-offset (screen-copy-mark-offset screen))
             (cur-offset  (screen-copy-offset screen))
             (h           (screen-height screen))
             ;; Convert viewport rows (stored at their respective offsets) to virtual rows.
             (mark-vrow   (+ sb-n (car mark)   (- mark-offset)))
             (cur-vrow    (+ sb-n (car cursor) (- cur-offset)))
             ;; Convert virtual rows back to viewport rows at the CURRENT offset,
             ;; clamped so that off-screen anchors highlight the nearest edge.
             (start-vrow  (min mark-vrow cur-vrow))
             (end-vrow    (max mark-vrow cur-vrow))
             (start-vp    (max 0 (min (1- h) (+ start-vrow cur-offset (- sb-n)))))
             (end-vp      (max 0 (min (1- h) (+ end-vrow   cur-offset (- sb-n)))))
             (rect-p      (screen-copy-rect-select-p screen))
             ;; Column orientation: which virtual end is topmost?
             (mark-is-start (< mark-vrow cur-vrow))
             (mark-is-end   (> mark-vrow cur-vrow)))
        (let ((col-min (min mark-col cursor-col))
              (col-max (max mark-col cursor-col)))
          (values t
                  start-vp
                  end-vp
                  (%selection-column rect-p mark-is-start mark-is-end
                                     col-min mark-col cursor-col col-min)
                  (%selection-column rect-p mark-is-start mark-is-end
                                     (1+ col-max) cursor-col mark-col col-max)
                  rect-p)))
      (values nil 0 0 0 0 nil)))
