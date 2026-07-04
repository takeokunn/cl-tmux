(in-package #:cl-tmux/test)

;;;; Window-level tests: pane selection and resize behavior.

(in-suite model-suite)

;;; ── Splitting and selecting panes ─────────────────────────────────────────

(test window-select-pane
  "After a split the first pane can be re-selected as active."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (with-session (session 24 80)
    (let* ((win        (session-active-window session))
           (first-pane (window-active-pane win)))
      ;; Split left/right (:h) → two panes; active switches to the new one.
      (window-split session win :h)
      (is (= 2 (length (window-panes win))) "must have 2 panes after split")
      (is (not (eq first-pane (window-active-pane win)))
          "active pane must be the new (second) pane after split")
      ;; Select the first pane back.
      (window-select-pane win first-pane)
      (is (eq first-pane (window-active-pane win))
          "window-active-pane must return the pane passed to window-select-pane"))))

;;; ── Resizing a pane ─────────────────────────────────────────────────────────

(test resize-pane-table
  "resize-pane adjusts the active pane and its neighbor by delta in the given axis.
   Each row: (split-orient resize-dir delta dimension-accessor grow-desc shrink-desc)."
  (dolist (row (list (list :h :right 5 #'pane-width
                           "active (left) pane should grow by 5"
                           "right neighbour should shrink by 5")
                     (list :v :down  3 #'pane-height
                           "active (top) pane should grow by 3"
                           "lower neighbour should shrink by 3")))
    (destructuring-bind (split-orient resize-dir delta measure grow-desc shrink-desc) row
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
            (is (= (+ a-before delta) (funcall measure p0))
                "~A: ~D → ~D" grow-desc a-before (funcall measure p0))
            (is (= (- b-before delta) (funcall measure p1))
                "~A: ~D → ~D" shrink-desc b-before (funcall measure p1))))))))

(test resize-pane-wrong-axis-is-noop
  "A :up/:down resize on a vertical split leaves pane widths unchanged."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (with-session (session 24 80)
    (let* ((win (session-active-window session))
           (p0  (window-active-pane win)))
      (window-split session win :h)               ; p0 | p1 side-by-side
      (window-select-pane win p0)
      (let ((w0-before (pane-width p0)))
        (resize-pane win :up 5)            ; wrong axis for an :h split
        (is (= w0-before (pane-width p0))
            ":h split must ignore a :up resize (wrong axis)")))))
