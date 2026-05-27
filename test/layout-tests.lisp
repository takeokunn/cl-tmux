(in-package #:cl-tmux/test)

;;;; Pane layout tests for divide-window.  The key invariants:
;;;;   • panes never overlap (a divider row/column sits between them),
;;;;   • panes stay within the window bounds,
;;;;   • the whole area minus dividers is covered (last pane absorbs slack).

(def-suite layout-suite :description "Pane layout geometry")
(in-suite layout-suite)

(defun slots-overlap-p (a b direction)
  "Do layout slots A and B overlap along the split axis?"
  (destructuring-bind (ax ay aw ah) a
    (destructuring-bind (bx by bw bh) b
      (ecase direction
        (:vertical    (and (< ax (+ bx bw)) (< bx (+ ax aw))))   ; x-extents
        (:horizontal  (and (< ay (+ by bh)) (< by (+ ay ah)))))))) ; y-extents

(test vertical-split-two-no-overlap
  (let ((slots (divide-window :vertical 2 24 80)))
    (is (= 2 (length slots)))
    (destructuring-bind (s0 s1) slots
      (destructuring-bind (x0 y0 w0 h0) s0
        (declare (ignore y0))
        (destructuring-bind (x1 y1 w1 h1) s1
          (declare (ignore y1))
          ;; A divider column sits between the two panes.
          (is (< (+ x0 w0) x1) "left pane's right edge precedes right pane")
          ;; Both panes span the full height.
          (is (= 24 h0))
          (is (= 24 h1))
          ;; Right pane reaches the right edge.
          (is (= 80 (+ x1 w1)))
          (is (plusp w0)))))))

(test horizontal-split-two-no-overlap
  (let ((slots (divide-window :horizontal 2 24 80)))
    (is (= 2 (length slots)))
    (destructuring-bind (s0 s1) slots
      (destructuring-bind (x0 y0 w0 h0) s0
        (declare (ignore x0))
        (destructuring-bind (x1 y1 w1 h1) s1
          (declare (ignore x1))
          ;; A divider row sits between the two panes.
          (is (< (+ y0 h0) y1) "top pane's bottom edge precedes bottom pane")
          ;; Both panes span the full width.
          (is (= 80 w0))
          (is (= 80 w1))
          ;; Bottom pane reaches the bottom edge.
          (is (= 24 (+ y1 h1)))
          (is (plusp h0)))))))

(test no-pair-overlaps-vertical
  (let ((slots (divide-window :vertical 3 24 80)))
    (loop for (a . rest) on slots
          do (dolist (b rest)
               (is (not (slots-overlap-p a b :vertical))
                   "panes ~A and ~A overlap" a b)))))

(test no-pair-overlaps-horizontal
  (let ((slots (divide-window :horizontal 3 24 80)))
    (loop for (a . rest) on slots
          do (dolist (b rest)
               (is (not (slots-overlap-p a b :horizontal))
                   "panes ~A and ~A overlap" a b)))))

(test all-slots-stay-in-bounds
  (dolist (dir '(:vertical :horizontal))
    (dolist (n '(2 3 4))
      (dolist (slot (divide-window dir n 24 80))
        (destructuring-bind (x y w h) slot
          (is (>= x 0))
          (is (>= y 0))
          (is (>= w 1) "width must be positive")
          (is (>= h 1) "height must be positive")
          (is (<= (+ x w) 80) "slot ~A exceeds width" slot)
          (is (<= (+ y h) 24) "slot ~A exceeds height" slot))))))

(test single-pane-fills-window
  (let ((slots (divide-window nil 1 24 80)))
    (is (equal '((0 0 80 24)) slots))))
