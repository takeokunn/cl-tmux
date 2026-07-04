(in-package #:cl-tmux/test)

(in-suite model-suite)

;;; ── apply-named-layout ───────────────────────────────────────────────────────

(test apply-named-layout-even-horizontal-positions-panes
  "even-horizontal places n panes side by side with equal width."
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

(test apply-named-layout-even-vertical-positions-panes
  "even-vertical places n panes stacked with equal height."
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

(test window-zoom-toggle-zoom-in-fills-window
  "Zooming a 2-pane window replaces the tree with a single-leaf tree so the
   active pane receives the full window dimensions, and window-zoom-p becomes T."
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
    (is-true  (cl-tmux/model:window-zoom-p win) "window-zoom-p must be T after zoom-in")
    (is (equal (list p0) (window-panes win))
        "zoomed window must have only the active pane in its panes list")
    (is (= 81 (pane-width  p0)) "zoomed pane must fill the full window width")
    (is (= 24 (pane-height p0)) "zoomed pane must fill the full window height")
    (is (= 0  (pane-x p0)) "zoomed pane must start at column 0")
    (is (= 0  (pane-y p0)) "zoomed pane must start at row 0")))

(test window-zoom-toggle-zoom-out-restores-layout
  "Unzooming restores the saved tree, the panes list, and window-zoom-p goes back to NIL."
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
      (is-false (cl-tmux/model:window-zoom-p win) "window-zoom-p must be NIL after zoom-out")
      (is (= 2 (length (window-panes win))) "both panes must be back after zoom-out")
      (is (= w0-before (pane-width p0)) "p0 width must be restored to pre-zoom value")
      (is (= w1-before (pane-width p1)) "p1 width must be restored to pre-zoom value"))))

(test window-zoom-toggle-single-pane-zooms-and-unzooms
  "Zoom on a single-pane window succeeds: the pane fills the window, then
   unzoom restores it (same geometry since there was only one pane)."
  (let* ((p0  (make-no-pty-pane 1 0 0 80 24))
         (win (make-window :id 1 :name "w" :width 80 :height 24
                           :panes (list p0)
                           :tree (make-layout-leaf p0))))
    (window-select-pane win p0)
    (cl-tmux/model:window-zoom-toggle win)
    (is-true  (cl-tmux/model:window-zoom-p win) "single-pane window must accept zoom-in")
    (is (= 1 (length (window-panes win))) "single-pane list unchanged after zoom-in")
    (is (= 80 (pane-width  p0)) "single pane must still fill full window width")
    (is (= 24 (pane-height p0)) "single pane must still fill full window height")
    (cl-tmux/model:window-zoom-toggle win)
    (is-false (cl-tmux/model:window-zoom-p win) "single-pane window must accept zoom-out")
    (is (= 1 (length (window-panes win))) "single-pane list still 1 after zoom-out")))

;;; ── window-lock slot ─────────────────────────────────────────────────────────

(test window-lock-slot-accessible
  "window-lock returns the lock object created at make-window time (not NIL and
   not an error)."
  (let ((win (make-window :id 1 :name "w")))
    (is-true (window-lock win)
             "window-lock must return a non-NIL lock object")))
