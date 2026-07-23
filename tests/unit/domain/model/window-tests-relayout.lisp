(in-package #:cl-tmux/test)

;;;; Window-level tests: relayout and window size reconciliation.
;;;;
;;;; NOTE: NO-PTY relayout tests build panes by hand with :fd -1 :pid -1 (no
;;;; real PTY) and a directly constructed virtual screen.  pane-reposition /
;;;; window-relayout call set-pty-size on fd -1, which is a tolerated EBADF
;;;; no-op (ioctl returns -1 without signalling a Lisp condition), so the only
;;;; observable effect is the pane geometry update and the screen-resize.  They
;;;; therefore run real assertions in the sandbox without gating on pty-available-p.

(describe "model-suite"

  ;;; ── window-relayout preserves a vertical split (no PTY) ────────────────────

  ;; window-relayout reflows a vertical 2-pane tree window into the new geometry,
  ;; updating both pane rectangles and the underlying screen dimensions.
  (it "window-relayout-vertical-preserves-split-no-pty"
    (let* ((start-rows 24) (start-cols 80)
           (p0 (make-no-pty-pane 1 0 0 39 start-rows))
           (p1 (make-no-pty-pane 2 40 0 40 start-rows))
           (win (make-window :id 1 :name "w"
                             :width start-cols :height start-rows
                             :tree (make-layout-split :h
                                      (make-layout-leaf p0)
                                      (make-layout-leaf p1))
                             :panes (list p0 p1) :active p1)))
      ;; Relayout into a new size.
      (let* ((new-rows 40) (new-cols 100))
        (window-relayout win new-rows new-cols)
        ;; Window stored size updated.
        (expect (= new-cols (window-width  win)))
        (expect (= new-rows (window-height win)))
        ;; Both panes have correct height and positive width.
        (dolist (pair (list (list p0 "pane 1") (list p1 "pane 2")))
          (destructuring-bind (p lbl) pair
            (declare (ignore lbl))
            (expect (= new-rows (pane-height p)))
            (expect (plusp (pane-width p)))))
        ;; Total coverage: two pane widths + 1 separator = new-cols.
        (expect (= new-cols (+ (pane-width p0) 1 (pane-width p1))))
        ;; Screens match pane dimensions.
        (check-table (list (list (screen-width  (pane-screen p0)) (pane-width p0)  "pane 1 screen-width")
                           (list (screen-height (pane-screen p0)) (pane-height p0) "pane 1 screen-height")
                           (list (screen-width  (pane-screen p1)) (pane-width p1)  "pane 2 screen-width")
                           (list (screen-height (pane-screen p1)) (pane-height p1) "pane 2 screen-height"))
                     :test #'equal)
        ;; Sanity: separator gap is one column between panes.
        (expect (= 1 (- (pane-x p1) (+ (pane-x p0) (pane-width p0))))))))

  ;;; ── window-relayout with NIL layout ────────────────────────────────────────

  ;; A single-pane window (tree = one leaf) relayouts to give the pane the full
  ;; (0 0 cols rows) rectangle.
  (it "window-relayout-single-pane-tree-fullscreen"
    (let* ((pane (make-no-pty-pane 1 5 5 10 10))
           (win  (make-window :id 1 :name "w"
                              :width 10 :height 10
                              :tree (make-layout-leaf pane)
                              :panes (list pane) :active pane)))
      (window-relayout win 30 90)
      (check-table (list (list (pane-x      pane)              0  "pane x")
                         (list (pane-y      pane)              0  "pane y")
                         (list (pane-width  pane)              90 "pane width")
                         (list (pane-height pane)              30 "pane height")
                         (list (screen-width  (pane-screen pane)) 90 "screen width")
                         (list (screen-height (pane-screen pane)) 30 "screen height"))
                   :test #'equal)))

  ;; With a NIL tree, window-relayout updates the stored dimensions but does not
  ;; reposition any panes (no tree to walk).
  (it "window-relayout-no-tree-is-noop-for-panes"
    (let* ((p0 (make-no-pty-pane 1 5 5 10 10))
           (p1 (make-no-pty-pane 2 7 7 12 12))
           (win (make-window :id 1 :name "w"
                             :width 10 :height 10
                             :panes (list p0 p1) :active p0)))
      (window-relayout win 30 90)
      (check-table (list (list (window-width  win) 90 "window width updated")
                         (list (window-height win) 30 "window height updated")
                         (list (pane-x      p0) 5  "p0 x unchanged: no tree")
                         (list (pane-y      p0) 5  "p0 y unchanged: no tree")
                         (list (pane-width  p0) 10 "p0 width unchanged: no tree")
                         (list (pane-height p0) 10 "p0 height unchanged: no tree")
                         (list (pane-x      p1) 7  "p1 x unchanged: no tree")
                         (list (pane-y      p1) 7  "p1 y unchanged: no tree")
                         (list (pane-width  p1) 12 "p1 width unchanged: no tree")
                         (list (pane-height p1) 12 "p1 height unchanged: no tree"))
                   :test #'equal)))

  ;;; ── window-relayout-current ─────────────────────────────────────────────────

  ;; window-relayout-current relayouts WINDOW using its own stored width/height
  ;; (equivalent to (window-relayout window (window-height w) (window-width w))).
  (it "window-relayout-current-uses-stored-dimensions"
    (let* ((p0  (make-no-pty-pane 1 0 0 1 1))
           (win (make-window :id 1 :name "w" :width 90 :height 30
                             :tree (make-layout-leaf p0)
                             :panes (list p0) :active p0)))
      (window-relayout-current win)
      (check-table (list (list (window-width  win)              90 "window width unchanged")
                         (list (window-height win)              30 "window height unchanged")
                         (list (pane-width  p0)                 90 "pane width fills stored window width")
                         (list (pane-height p0)                 30 "pane height fills stored window height")
                         (list (screen-width  (pane-screen p0)) 90 "screen width matches stored window width")
                         (list (screen-height (pane-screen p0)) 30 "screen height matches stored window height")))))

  ;;; ── ensure-window-fits ───────────────────────────────────────────────────────

  ;; ensure-window-fits relayouts when the requested size differs from the
  ;; window's stored size, fixing a pane left with mismatched geometry.
  (it "ensure-window-fits-relayouts-on-size-change"
    (let* ((pane (make-no-pty-pane 1 0 0 80 24))
           (win  (make-window :id 1 :name "w"
                              :width 80 :height 24
                              :tree (make-layout-leaf pane)
                              :panes (list pane) :active pane)))
      ;; Deliberately leave the window's stored size inconsistent with the
      ;; requested target so ensure-window-fits must act.
      (cl-tmux/model::ensure-window-fits win 30 100)
      (check-table (list (list (window-width  win)              100 "window width")
                         (list (window-height win)              30  "window height")
                         (list (pane-width  pane)               100 "pane width")
                         (list (pane-height pane)               30  "pane height")
                         (list (screen-width  (pane-screen pane)) 100 "screen width")
                         (list (screen-height (pane-screen pane)) 30  "screen height")))))

  ;; ensure-window-fits is a no-op when the window already matches the requested
  ;; size: geometry is left untouched and the same screen object is retained
  ;; (no screen-resize, so EQ holds).
  (it "ensure-window-fits-noop-when-size-matches"
    (let* ((screen (make-screen 80 24))
           (pane   (make-pane :id 1 :x 0 :y 0 :width 80 :height 24
                              :fd -1 :pid -1 :screen screen))
           (win    (make-window :id 1 :name "w"
                                :width 80 :height 24
                                :tree (make-layout-leaf pane)
                                :panes (list pane) :active pane)))
      ;; Same size as stored → nothing should happen.
      (cl-tmux/model::ensure-window-fits win 24 80)
      (check-table (list (list (window-width  win)              80 "window width")
                         (list (window-height win)              24 "window height")
                         (list (pane-x      pane)              0  "pane x")
                         (list (pane-y      pane)              0  "pane y")
                         (list (pane-width  pane)              80 "pane width")
                         (list (pane-height pane)              24 "pane height")
                         (list (screen-width  (pane-screen pane)) 80 "screen width")
                         (list (screen-height (pane-screen pane)) 24 "screen height")))
      (expect (eq screen (pane-screen pane))))))
