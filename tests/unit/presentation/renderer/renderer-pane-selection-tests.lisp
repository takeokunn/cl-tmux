(in-package #:cl-tmux/test)

;;;; Direct unit tests for renderer-pane-selection.lisp's %compute-selection-bounds,
;;;; which previously had no dedicated coverage (only exercised transitively, if at
;;;; all, through full-screen render integration tests).

(describe "renderer-suite/pane-selection"

  ;; %compute-selection-bounds reports SEL-ACTIVE nil (and all-zero defaults) when
  ;; the copy-mode selecting flag, mark, or cursor prerequisites are not all present.
  (it "compute-selection-bounds-inactive-without-selecting-flag"
    (let ((s (copy-mode-screen :mark (cons 1 0) :cursor (cons 3 0))))
      (multiple-value-bind (active start-row end-row start-col end-col rect-p mark-row mark-col)
          (cl-tmux/renderer::%compute-selection-bounds s)
        (expect active :to-be-falsy)
        (expect (= 0 start-row)) (expect (= 0 end-row))
        (expect (= 0 start-col)) (expect (= 0 end-col))
        (expect rect-p :to-be-falsy)
        (expect (= 0 mark-row)) (expect (= 0 mark-col)))))

  ;; A forward selection (mark above cursor) reports the mark's column as the
  ;; start column and the cursor's column as the end column.
  (it "compute-selection-bounds-forward-selection"
    (let ((s (copy-mode-screen :mark (cons 1 2) :cursor (cons 3 8) :selecting t)))
      (multiple-value-bind (active start-row end-row start-col end-col rect-p mark-row mark-col)
          (cl-tmux/renderer::%compute-selection-bounds s)
        (expect active :to-be-truthy)
        (expect (= 1 start-row))
        (expect (= 3 end-row))
        (expect (= 2 start-col))
        (expect (= 8 end-col))
        (expect rect-p :to-be-falsy)
        (expect (= 1 mark-row))
        (expect (= 2 mark-col)))))

  ;; A backward selection (cursor above mark) still normalizes start-row/end-row
  ;; by MIN/MAX, but swaps which column is "start" vs "end" to match reading order.
  (it "compute-selection-bounds-backward-selection"
    (let ((s (copy-mode-screen :mark (cons 3 8) :cursor (cons 1 2) :selecting t)))
      (multiple-value-bind (active start-row end-row start-col end-col)
          (cl-tmux/renderer::%compute-selection-bounds s)
        (expect active :to-be-truthy)
        (expect (= 1 start-row))
        (expect (= 3 end-row))
        (expect (= 2 start-col))
        (expect (= 8 end-col)))))

  ;; A single-row selection (mark and cursor on the same row) uses the column
  ;; MIN/MAX directly, regardless of which of mark/cursor came first.
  (it "compute-selection-bounds-same-row-selection"
    (let ((s (copy-mode-screen :mark (cons 2 8) :cursor (cons 2 3) :selecting t)))
      (multiple-value-bind (active start-row end-row start-col end-col)
          (cl-tmux/renderer::%compute-selection-bounds s)
        (declare (ignore active))
        (expect (= 2 start-row))
        (expect (= 2 end-row))
        (expect (= 3 start-col))
        (expect (= 8 end-col)))))

  ;; Rectangle-select mode always reports column MIN/MAX+1 (an exclusive-end
  ;; column range), independent of mark/cursor row order.
  (it "compute-selection-bounds-rectangle-selection"
    (let ((s (copy-mode-screen :mark (cons 3 8) :cursor (cons 1 2) :selecting t)))
      (setf (cl-tmux/terminal/types:screen-copy-rect-select-p s) t)
      (multiple-value-bind (active start-row end-row start-col end-col rect-p)
          (cl-tmux/renderer::%compute-selection-bounds s)
        (declare (ignore active start-row end-row))
        (expect rect-p :to-be-truthy)
        (expect (= 2 start-col))
        (expect (= 9 end-col))))))
