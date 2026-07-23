(in-package #:cl-tmux/test)

(describe "model-suite"

  ;;; ── apply-named-layout ───────────────────────────────────────────────────────

  ;; even-horizontal places n panes side by side with equal width.
  (it "apply-named-layout-even-horizontal-positions-panes"
    (let* ((p0  (make-no-pty-pane 1 0 0 1 1))
           (p1  (make-no-pty-pane 2 0 0 1 1))
           (win (make-window :id 1 :name "w" :width 81 :height 24
                             :panes (list p0 p1)
                             :tree (make-layout-leaf p0))))
      (apply-named-layout win :even-horizontal)
      ;; 81 cols - 1 separator = 80 usable, floor(80/2) = 40 each.
      (check-table (list (list (pane-x      p0) 0  "p0 must start at column 0")
                         (list (pane-width  p0) 40 "p0 width must be 40")
                         (list (pane-x      p1) 41 "p1 must start at column 41")
                         (list (pane-width  p1) 40 "p1 width must be 40")
                         (list (pane-height p0) 24 "p0 height unchanged")
                         (list (pane-height p1) 24 "p1 height unchanged")))))

  ;; even-vertical places n panes stacked with equal height.
  (it "apply-named-layout-even-vertical-positions-panes"
    (let* ((p0  (make-no-pty-pane 1 0 0 1 1))
           (p1  (make-no-pty-pane 2 0 0 1 1))
           (win (make-window :id 1 :name "w" :width 80 :height 25
                             :panes (list p0 p1)
                             :tree (make-layout-leaf p0))))
      (apply-named-layout win :even-vertical)
      ;; 25 rows - 1 separator = 24, floor(24/2) = 12 each.
      (check-table (list (list (pane-y      p0) 0  "p0 must start at row 0")
                         (list (pane-height p0) 12 "p0 height must be 12")
                         (list (pane-y      p1) 13 "p1 must start at row 13")
                         (list (pane-height p1) 12 "p1 height must be 12")))))

  ;;; ── window-zoom-toggle ───────────────────────────────────────────────────────

  ;; Zooming a 2-pane window replaces the tree with a single-leaf tree so the
  ;; active pane receives the full window dimensions, and window-zoom-p becomes T.
  (it "window-zoom-toggle-zoom-in-fills-window"
    (let* ((p0  (make-no-pty-pane 1  0 0 40 24))
           (p1  (make-no-pty-pane 2 41 0 40 24))
           (win (make-window :id 1 :name "w" :width 81 :height 24
                             :panes (list p0 p1)
                             :tree (make-layout-split :h
                                      (make-layout-leaf p0)
                                      (make-layout-leaf p1)
                                      1/2))))
      (window-select-pane win p0)
      (cl-tmux/model:window-zoom-toggle win)
      (expect (cl-tmux/model:window-zoom-p win) :to-be-truthy)
      (expect (equal (list p0) (window-panes win)))
      (expect (= 81 (pane-width  p0)))
      (expect (= 24 (pane-height p0)))
      (expect (= 0  (pane-x p0)))
      (expect (= 0  (pane-y p0)))))

  ;; Unzooming restores the saved tree, the panes list, and window-zoom-p goes back to NIL.
  (it "window-zoom-toggle-zoom-out-restores-layout"
    (let* ((p0  (make-no-pty-pane 1  0 0 40 24))
           (p1  (make-no-pty-pane 2 41 0 40 24))
           (win (make-window :id 1 :name "w" :width 81 :height 24
                             :panes (list p0 p1)
                             :tree (make-layout-split :h
                                      (make-layout-leaf p0)
                                      (make-layout-leaf p1)
                                      1/2))))
      (window-select-pane win p0)
      (let ((w0-before (pane-width p0))
            (w1-before (pane-width p1)))
        (cl-tmux/model:window-zoom-toggle win)
        (cl-tmux/model:window-zoom-toggle win)
        (expect (cl-tmux/model:window-zoom-p win) :to-be-falsy)
        (expect (= 2 (length (window-panes win))))
        (expect (= w0-before (pane-width p0)))
        (expect (= w1-before (pane-width p1))))))

  ;; Zoom on a single-pane window succeeds: the pane fills the window, then
  ;; unzoom restores it (same geometry since there was only one pane).
  (it "window-zoom-toggle-single-pane-zooms-and-unzooms"
    (let* ((p0  (make-no-pty-pane 1 0 0 80 24))
           (win (make-window :id 1 :name "w" :width 80 :height 24
                             :panes (list p0)
                             :tree (make-layout-leaf p0))))
      (window-select-pane win p0)
      (cl-tmux/model:window-zoom-toggle win)
      (expect (cl-tmux/model:window-zoom-p win) :to-be-truthy)
      (expect (= 1 (length (window-panes win))))
      (expect (= 80 (pane-width  p0)))
      (expect (= 24 (pane-height p0)))
      (cl-tmux/model:window-zoom-toggle win)
      (expect (cl-tmux/model:window-zoom-p win) :to-be-falsy)
      (expect (= 1 (length (window-panes win))))))

  ;;; ── window-lock slot ─────────────────────────────────────────────────────────

  ;; window-lock returns the lock object created at make-window time (not NIL and
  ;; not an error).
  (it "window-lock-slot-accessible"
    (let ((win (make-window :id 1 :name "w")))
      (expect (window-lock win) :to-be-truthy))))
