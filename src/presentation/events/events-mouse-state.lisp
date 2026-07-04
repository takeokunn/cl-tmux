(in-package #:cl-tmux)

;;; Mouse dispatch state is intentionally tiny and explicit: parser state,
;;; pane routing, and copy-mode logic live in separate files.

(defvar *mouse-drag-state* nil
  "Drag state for border-resize: NIL or (split orientation).")

(defvar *last-mouse-click* nil
  "Double/triple-click detection state: (list time-ms row col count), or NIL.")

(defvar *current-mouse-event* nil
  "Dynamically bound mouse event plist for commands such as send-keys -M.")

(defun %now-ms ()
  "Current monotonic time in milliseconds."
  (floor (* 1000 (get-internal-real-time)) internal-time-units-per-second))

(defun %pane-local-coordinates (pane col row)
  "Return COL and ROW translated into PANE-local coordinates."
  (values (- col (pane-x pane))
          (- row (pane-y pane))))

(defun %mouse-click-count (last now-ms row col threshold-ms)
  "Compute the click count for a left press at (ROW,COL) at NOW-MS."
  (if (and last
           (<= (- now-ms (first last)) threshold-ms)
           (= row (second last))
           (= col (third last)))
      (1+ (fourth last))
      1))
