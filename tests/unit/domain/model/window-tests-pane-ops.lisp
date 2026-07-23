(in-package #:cl-tmux/test)

;;;; Window-level tests: pane selection and resize behavior.

(describe "model-suite"

  ;;; ── Splitting and selecting panes ─────────────────────────────────────────

  ;; After a split the first pane can be re-selected as active.
  (it "window-select-pane"
    (unless (pty-available-p)
      (skip "no PTY available (sandboxed environment)"))
    (with-session (session 24 80)
      (let* ((win        (session-active-window session))
             (first-pane (window-active-pane win)))
        ;; Split left/right (:h) → two panes; active switches to the new one.
        (window-split session win :h)
        (expect (= 2 (length (window-panes win))))
        (expect (not (eq first-pane (window-active-pane win))))
        ;; Select the first pane back.
        (window-select-pane win first-pane)
        (expect (eq first-pane (window-active-pane win))))))

  ;;; ── Resizing a pane ─────────────────────────────────────────────────────────

  ;; resize-pane adjusts the active pane and its neighbor by delta in the given axis.
  ;; Each row: (split-orient resize-dir delta dimension-accessor grow-desc shrink-desc).
  (it "resize-pane-table"
    (dolist (row (list (list :h :right 5 #'pane-width
                             "active (left) pane should grow by 5"
                             "right neighbour should shrink by 5")
                       (list :v :down  3 #'pane-height
                             "active (top) pane should grow by 3"
                             "lower neighbour should shrink by 3")))
      (destructuring-bind (split-orient resize-dir delta measure grow-desc shrink-desc) row
        (declare (ignore grow-desc shrink-desc))
        (unless (pty-available-p)
          (skip "no PTY available (sandboxed environment)"))
        (with-session (session 24 80)
          (let* ((win (session-active-window session))
                 (p0  (window-active-pane win)))
            (window-split session win split-orient)
            (window-select-pane win p0)
            (let* ((p1        (second (window-panes win)))
                   (a-before  (funcall measure p0))
                   (b-before  (funcall measure p1)))
              (resize-pane win resize-dir delta)
              (expect (= (+ a-before delta) (funcall measure p0)))
              (expect (= (- b-before delta) (funcall measure p1)))))))))

  ;; A :up/:down resize on a vertical split leaves pane widths unchanged.
  (it "resize-pane-wrong-axis-is-noop"
    (unless (pty-available-p)
      (skip "no PTY available (sandboxed environment)"))
    (with-session (session 24 80)
      (let* ((win (session-active-window session))
             (p0  (window-active-pane win)))
        (window-split session win :h)               ; p0 | p1 side-by-side
        (window-select-pane win p0)
        (let ((w0-before (pane-width p0)))
          (resize-pane win :up 5)            ; wrong axis for an :h split
          (expect (= w0-before (pane-width p0))))))))
