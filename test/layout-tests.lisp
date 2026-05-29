(in-package #:cl-tmux/test)

;;;; Pane layout tests for divide-window.  The key invariants:
;;;;   • panes never overlap (a divider row/column sits between them),
;;;;   • panes stay within the window bounds,
;;;;   • the whole area minus dividers is covered (last pane absorbs slack).

(def-suite layout-suite :description "Pane layout geometry")
(in-suite layout-suite)

;;; ── Focused two-pane tests ─────────────────────────────────────────────────

(test vertical-split-two
  "Vertical split into 2 panes: divider column exists, full height, right edge."
  (let ((slots (divide-window :vertical 2 24 80)))
    (is (= 2 (length slots)))
    (check-layout-invariants slots :vertical 24 80 :test-name "vertical-split-two")
    (destructuring-bind ((x0 y0 w0 h0) (x1 y1 w1 h1)) slots
      (declare (ignore y0 y1))
      ;; A divider column sits between the two panes.
      (is (< (+ x0 w0) x1)
          "left pane right edge ~A must precede right pane x ~A" (+ x0 w0) x1)
      ;; Both panes span the full height.
      (is (= 24 h0) "left pane height must equal rows")
      (is (= 24 h1) "right pane height must equal rows")
      ;; Right pane reaches the right edge.
      (is (= 80 (+ x1 w1)) "right pane must reach cols")
      ;; Both panes have positive width.
      (is (plusp w0) "left pane width must be positive")
      (is (plusp w1) "right pane width must be positive"))))

(test horizontal-split-two
  "Horizontal split into 2 panes: divider row exists, full width, bottom edge."
  (let ((slots (divide-window :horizontal 2 24 80)))
    (is (= 2 (length slots)))
    (check-layout-invariants slots :horizontal 24 80 :test-name "horizontal-split-two")
    (destructuring-bind ((x0 y0 w0 h0) (x1 y1 w1 h1)) slots
      (declare (ignore x0 x1))
      ;; A divider row sits between the two panes.
      (is (< (+ y0 h0) y1)
          "top pane bottom edge ~A must precede bottom pane y ~A" (+ y0 h0) y1)
      ;; Both panes span the full width.
      (is (= 80 w0) "top pane width must equal cols")
      (is (= 80 w1) "bottom pane width must equal cols")
      ;; Bottom pane reaches the bottom edge.
      (is (= 24 (+ y1 h1)) "bottom pane must reach rows")
      ;; Both panes have positive height.
      (is (plusp h0) "top pane height must be positive")
      (is (plusp h1) "bottom pane height must be positive"))))

;;; ── Parametric invariants over many shapes ─────────────────────────────────

(test parametric-invariants
  "Run check-layout-invariants over a variety of (direction n rows cols) tuples."
  (dolist (params '((:vertical   2 24  80)
                    (:vertical   3 24  80)
                    (:vertical   4 24  80)
                    (:horizontal 2 24  80)
                    (:horizontal 3 24  80)
                    (:horizontal 4 24  80)
                    (:vertical   2 10  40)
                    (:horizontal 3 30 120)
                    (:vertical   5 24  80)
                    (:horizontal 5 24  80)))
    (destructuring-bind (direction n rows cols) params
      (let ((slots (divide-window direction n rows cols)))
        (is (= n (length slots))
            "expected ~A slots for ~A n=~A, got ~A" n direction n (length slots))
        (check-layout-invariants slots direction rows cols
                                 :test-name (format nil "~A n=~A ~Ax~A"
                                                    direction n cols rows))))))

;;; ── Single-pane degenerate case ────────────────────────────────────────────

(test single-pane
  "divide-window with n=1 (any direction) returns exactly '((0 0 cols rows))."
  (is (equal '((0 0 80 24)) (divide-window nil        1 24 80)))
  (is (equal '((0 0 80 24)) (divide-window :vertical  1 24 80)))
  (is (equal '((0 0 80 24)) (divide-window :horizontal 1 24 80))))

;;; ── Minimum-size stress ────────────────────────────────────────────────────

(test minimum-size
  "Even in a tiny terminal each pane must have positive dimensions."
  (dolist (dir '(:vertical :horizontal))
    (let ((slots (divide-window dir 2 4 10)))
      (dolist (slot slots)
        (destructuring-bind (x y w h) slot
          (declare (ignore x y))
          (is (>= w 1) "width ~A must be >= 1 in minimum-size ~A" w dir)
          (is (>= h 1) "height ~A must be >= 1 in minimum-size ~A" h dir))))))

;;; ── Last-slot absorbs remainder ────────────────────────────────────────────

(test last-slot-absorbs-remainder
  "For n=3 vertical, sum of pane widths + 2 dividers = cols (80)."
  (let ((slots (divide-window :vertical 3 24 80)))
    (is (= 3 (length slots)))
    ;; There are (n-1) = 2 divider columns, each 1 column wide.
    ;; Total coverage: sum-of-widths + 2 = 80.
    (let ((total-width (reduce #'+ slots :key #'third)))
      (is (= 80 (+ total-width 2))
          "sum of widths ~A + 2 dividers must equal 80" total-width))
    ;; Verify via check-layout-invariants as well.
    (check-layout-invariants slots :vertical 24 80
                             :test-name "last-slot-absorbs-remainder")))
