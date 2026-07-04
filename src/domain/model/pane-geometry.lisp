(in-package #:cl-tmux/model)

;;; ── pane-reposition ──────────────────────────────────────────────────────────
;;;
;;; Data/logic separation mirrors the zoom helpers in window-core.lisp:
;;;   %update-pane-geometry — pure slot mutation (data)
;;;   pane-reposition       — geometry update then PTY/screen resize (effects)

(defun %update-pane-geometry (pane x y width height)
  "Update PANE's position and dimension slots to X, Y, WIDTH, HEIGHT.
   Pure data mutation — no I/O side effects."
  (setf (pane-x pane)      x
        (pane-y pane)      y
        (pane-width  pane) width
        (pane-height pane) height))

(defun %pane-border-status-reservation (status height)
  "Return (values CONTENT-Y-OFFSET CONTENT-HEIGHT) for a pane allocated HEIGHT rows,
   given the STATUS string from the pane-border-status option.
   When STATUS is \"top\" or \"bottom\" and HEIGHT > 1, one row is reserved for
   the border-status title line:
     \"top\"    → offset 1 (title on the allocated top row), content height-1
     \"bottom\" → offset 0 (title on the allocated bottom row), content height-1
     \"off\" / \"\" / too short → offset 0, full height (no reservation).
   PURE function: STATUS is passed in rather than read from the option store,
   enforcing data/logic separation.  The pane's geometry becomes the CONTENT
   rectangle; the title row is drawn by %render-pane-border-status."
  (if (and (not (member status '("off" "") :test #'string=)) (> height 1))
      (values (if (string= status "top") 1 0) (1- height))
      (values 0 height)))

(defun pane-reposition (pane x y width height)
  "Move and resize PANE to X,Y with WIDTH x HEIGHT.
   Updates the geometry slots, then resizes the underlying PTY and virtual screen.
   When pane-border-status is on, one row of the allocation is reserved for the
   title line, so the pane's CONTENT geometry (and the app's PTY/screen) is one
   row shorter — the title no longer overwrites pane content."
  (let ((status (cl-tmux/options:get-option "pane-border-status" "off")))
    (multiple-value-bind (content-y-offset content-height)
        (%pane-border-status-reservation status height)
      (%update-pane-geometry pane x (+ y content-y-offset) width content-height)
      (when (> (pane-fd pane) 0)
        (resize-pty (pane-fd pane) content-height width))
      (let ((screen (pane-screen pane)))
        (with-lock-held ((screen-lock screen))
          (screen-resize screen width content-height))))))
