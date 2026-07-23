(in-package #:cl-tmux/test)

;;;; window tests — part B: apply-named-layout (main-horizontal/vertical/tiled),
;;;; last-window by recency, move-window, swap-window, rotate-window,
;;;; find-window-by-name, list-windows-format, auto-rename-from-osc,
;;;; window-remove-pane, window-last-active-time, window-layout-cycle-index.

(describe "model-suite"

  ;;; ── apply-named-layout — remaining three named layouts ─────────────────────
  ;;;
  ;;; :main-horizontal, :main-vertical, and :tiled all contain substantively
  ;;; different geometry logic (floor-halving, secondary-pane subdivision,
  ;;; sqrt-based grid).  The tests below exercise each of these branches
  ;;; using make-no-pty-pane from helpers-pane-fixtures.lisp and make-fake-window from helpers-session-fixtures.lisp.

  ;; :main-horizontal with 2 panes: the main pane spans the top main-pane-height
  ;; rows (tmux default 24); the second fills the rest below it.
  (it "apply-named-layout-main-horizontal-two-panes"
    ;; Window taller than the default main-pane-height so the layout is
    ;; non-degenerate: h=50 → main 24, rest = 50 - 24 - 1 = 25.
    (with-blank-window (win p0 p1) (:width 80 :height 50)
      (apply-named-layout win :main-horizontal)
      (check-table (list (list (pane-x      p0)  0 "main pane starts at column 0")
                         (list (pane-y      p0)  0 "main pane starts at row 0")
                         (list (pane-width  p0) 80 "main pane spans full width")
                         (list (pane-height p0) 24 "main pane height = main-pane-height (24)")
                         (list (pane-x      p1)  0 "secondary pane starts at column 0")
                         (list (pane-y      p1) 25 "secondary pane y = main-h + 1 separator")
                         (list (pane-width  p1) 80 "secondary pane spans full width (1 pane below)")
                         (list (pane-height p1) 25 "secondary pane height = h - main-h - 1"))
                   :test #'equal)))

  ;; :main-horizontal with 3 panes: main spans the top main-pane-height rows; two
  ;; panes share the bottom region side by side with equal widths.
  (it "apply-named-layout-main-horizontal-three-panes"
    (with-blank-window (win p0 p1 p2) (:width 81 :height 50)
      (apply-named-layout win :main-horizontal)
      ;; main-h = main-pane-height = 24; rest-h = 50 - 24 - 1 = 25
      ;; Two secondary panes in a row: 81 cols - 1 separator = 80, floor(80/2) = 40 each
      (check-table (list (list (pane-height p0) 24 "main pane height = main-pane-height (24)")
                         (list (pane-height p1) 25 "secondary panes fill the bottom region height")
                         (list (pane-height p2) 25 "secondary panes fill the bottom region height")
                         (list (pane-x      p1)  0 "left secondary pane starts at column 0")
                         (list (pane-width  p1) 40 "left secondary width = floor(avail/2)")
                         (list (pane-x      p2) 41 "right secondary pane starts at column 41")
                         (list (pane-width  p2) 40 "right secondary width = avail - left-w"))
                   :test #'equal)))

  ;; :main-vertical with 2 panes: the main pane spans the left main-pane-width
  ;; columns (tmux default 80); the second fills the right column.
  (it "apply-named-layout-main-vertical-two-panes"
    ;; Window wider than the default main-pane-width: w=120 → main 80,
    ;; rest = 120 - 80 - 1 = 39.
    (with-blank-window (win p0 p1) (:width 120 :height 24)
      (apply-named-layout win :main-vertical)
      ;; Secondary pane fills the right column.
      (check-table (list (list (pane-x      p0)  0 "main pane starts at column 0")
                         (list (pane-y      p0)  0 "main pane starts at row 0")
                         (list (pane-width  p0) 80 "main pane width = main-pane-width (80)")
                         (list (pane-height p0) 24 "main pane spans full height")
                         (list (pane-x      p1) 81 "secondary pane x = main-w + 1 separator")
                         (list (pane-y      p1)  0 "secondary pane starts at row 0")
                         (list (pane-width  p1) 39 "secondary pane width = w - main-w - 1")
                         (list (pane-height p1) 24 "secondary pane spans full height (1 pane in column)"))
                   :test #'equal)))

  ;; :main-vertical with 3 panes: main spans the left main-pane-width columns; two
  ;; panes share the right region stacked equally.
  (it "apply-named-layout-main-vertical-three-panes"
    (with-blank-window (win p0 p1 p2) (:width 120 :height 25)
      (apply-named-layout win :main-vertical)
      ;; main-w = main-pane-width = 80; rest-w = 120 - 80 - 1 = 39
      ;; Two secondary panes stacked: 25 rows - 1 separator = 24, floor(24/2) = 12 each
      (check-table (list (list (pane-width  p0) 80 "main pane width = main-pane-width (80)")
                         (list (pane-height p0) 25 "main pane spans full height")
                         (list (pane-x      p1) 81 "top secondary pane x = main-w + 1")
                         (list (pane-y      p1)  0 "top secondary pane starts at row 0")
                         (list (pane-height p1) 12 "top secondary height = floor(avail/2)")
                         (list (pane-x      p2) 81 "bottom secondary pane in the same column")
                         (list (pane-y      p2) 13 "bottom secondary pane y = top-h + 1 separator")
                         (list (pane-height p2) 12 "bottom secondary height = avail - top-h"))
                   :test #'equal)))

  ;; All named layouts with a single pane assign it the full window rectangle.
  (it "apply-named-layout-single-pane-fills-window-table"
    (dolist (layout '(:main-horizontal :main-vertical :tiled))
      (with-blank-window (win p0) ()
        (apply-named-layout win layout)
        (expect (= 0  (pane-x      p0)))
        (expect (= 0  (pane-y      p0)))
        (expect (= 80 (pane-width  p0)))
        (expect (= 24 (pane-height p0))))))

  ;; :tiled with 4 panes produces a 2×2 grid with equal cell sizes.
  (it "apply-named-layout-tiled-four-panes"
    (with-blank-window (win p0 p1 p2 p3) (:width 81 :height 25)
      (apply-named-layout win :tiled)
      ;; ceil(sqrt(4)) = 2 cols; ceil(4/2) = 2 rows
      ;; col-w = floor((81 - 1) / 2) = 40; row-h = floor((25 - 1) / 2) = 12
      (check-table (list (list (pane-x      p0)  0 "p0 col 0, row 0: x")
                         (list (pane-y      p0)  0 "p0 col 0, row 0: y")
                         (list (pane-width  p0) 40 "p0 width")
                         (list (pane-height p0) 12 "p0 height")
                         (list (pane-x      p1) 41 "p1 col 1, row 0: x = 1*(40+1)")
                         (list (pane-y      p1)  0 "p1 row 0: y")
                         (list (pane-width  p1) 40 "p1 width")
                         (list (pane-height p1) 12 "p1 height")
                         (list (pane-x      p2)  0 "p2 col 0, row 1: x")
                         (list (pane-y      p2) 13 "p2 row 1: y = 1*(12+1)")
                         (list (pane-width  p2) 40 "p2 width")
                         (list (pane-height p2) 12 "p2 height")
                         (list (pane-x      p3) 41 "p3 col 1, row 1: x")
                         (list (pane-y      p3) 13 "p3 row 1: y")
                         (list (pane-width  p3) 40 "p3 width")
                         (list (pane-height p3) 12 "p3 height"))
                   :test #'equal)))

  ;; :tiled with 3 panes produces a 2-column grid (2×2 with one empty cell).
  (it "apply-named-layout-tiled-three-panes"
    (with-blank-window (win p0 p1 p2) (:width 81 :height 25)
      (apply-named-layout win :tiled)
      ;; ceil(sqrt(3)) = 2 cols; ceil(3/2) = 2 rows
      ;; col-w = floor((81-1)/2) = 40; row-h = floor((25-1)/2) = 12
      ;; p0 at (col=0,row=0) p1 at (col=1,row=0) p2 at (col=0,row=1)
      (check-table (list (list (pane-x p0)  0 "p0 at col 0, row 0: x")
                         (list (pane-y p0)  0 "p0 at col 0, row 0: y")
                         (list (pane-x p1) 41 "p1 at col 1, row 0: x")
                         (list (pane-y p1)  0 "p1 at col 1, row 0: y")
                         (list (pane-x p2)  0 "p2 at col 0, row 1: x")
                         (list (pane-y p2) 13 "p2 placed in second row"))
                   :test #'equal)))

  ;;; ── last-window by recency ───────────────────────────────────────────────────

  ;; session-last-window returns the window with the second-highest last-active-time.
  (it "last-window-by-recency"
    (let* ((w0 (make-window :id 1 :name "0" :last-active-time 100))
           (w1 (make-window :id 2 :name "1" :last-active-time 200))
           (w2 (make-window :id 3 :name "2" :last-active-time 300))
           (sess (make-session :id 1 :name "s" :windows (list w0 w1 w2))))
      (session-select-window sess w2)
      ;; second-most-recent is w1 (time 200)
      (expect (eq w1 (session-last-window sess)))))

  ;; session-last-window returns NIL when there is only one window.
  (it "last-window-single-window-returns-nil"
    (let* ((w0   (make-window :id 1 :name "0" :last-active-time 100))
           (sess (make-session :id 1 :name "s" :windows (list w0))))
      (expect (null (session-last-window sess)))))

  ;;; ── move-window-reorders ─────────────────────────────────────────────────────

  ;; session-move-window moves a window to the requested position.
  (it "move-window-reorders"
    (let* ((w0 (make-window :id 1 :name "0"))
           (w1 (make-window :id 2 :name "1"))
           (w2 (make-window :id 3 :name "2"))
           (sess (make-session :id 1 :name "s" :windows (list w0 w1 w2))))
      ;; Move w0 (currently index 0) to index 2 (the end).
      (session-move-window sess w0 2)
      (expect (equal (list w1 w2 w0) (session-windows sess)))))

  ;; session-move-window clamps out-of-range target to last valid index.
  (it "move-window-clamps-to-last"
    (let* ((w0 (make-window :id 1 :name "0"))
           (w1 (make-window :id 2 :name "1"))
           (sess (make-session :id 1 :name "s" :windows (list w0 w1))))
      (session-move-window sess w0 99)
      (expect (equal (list w1 w0) (session-windows sess)))))

  ;; session-move-window clamps a negative target index to 0 (the first position).
  (it "move-window-clamps-negative-index-to-zero"
    (let* ((w0 (make-window :id 1 :name "0"))
           (w1 (make-window :id 2 :name "1"))
           (w2 (make-window :id 3 :name "2"))
           (sess (make-session :id 1 :name "s" :windows (list w0 w1 w2))))
      ;; Move w2 (currently index 2) to a negative target — clamps to 0.
      (session-move-window sess w2 -5)
      (expect (equal (list w2 w0 w1) (session-windows sess)))))

  ;;; ── swap-window-exchanges ────────────────────────────────────────────────────

  ;; session-swap-windows exchanges two windows at the given indices.
  (it "swap-window-exchanges"
    (let* ((w0 (make-window :id 1 :name "0"))
           (w1 (make-window :id 2 :name "1"))
           (w2 (make-window :id 3 :name "2"))
           (sess (make-session :id 1 :name "s" :windows (list w0 w1 w2))))
      (session-swap-windows sess 0 2)
      (expect (equal (list w2 w1 w0) (session-windows sess)))))

  ;; session-swap-windows with equal indices leaves the list unchanged.
  (it "swap-window-same-index-is-noop"
    (let* ((w0 (make-window :id 1 :name "0"))
           (w1 (make-window :id 2 :name "1"))
           (sess (make-session :id 1 :name "s" :windows (list w0 w1))))
      (session-swap-windows sess 0 0)
      (expect (equal (list w0 w1) (session-windows sess)))))

  ;; session-swap-windows with an out-of-range index leaves the list unchanged.
  (it "swap-window-out-of-range-is-noop"
    (let* ((w0 (make-window :id 1 :name "0"))
           (w1 (make-window :id 2 :name "1"))
           (sess (make-session :id 1 :name "s" :windows (list w0 w1))))
      (session-swap-windows sess 0 5)
      (expect (equal (list w0 w1) (session-windows sess)))))

  ;; session-move-window is a no-op when the window is not in the session.
  (it "move-window-returns-unchanged-when-window-not-found"
    (let* ((w0 (make-window :id 1 :name "0"))
           (w1 (make-window :id 2 :name "1"))
           (w-other (make-window :id 99 :name "other"))
           (sess (make-session :id 1 :name "s" :windows (list w0 w1))))
      (session-move-window sess w-other 0)
      (expect (equal (list w0 w1) (session-windows sess)))))

  ;;; ── rotate-window-shifts-panes ───────────────────────────────────────────────

  ;; window-rotate :up moves the first pane to the end of the panes list.
  (it "rotate-window-shifts-panes"
    (let* ((p0  (make-no-pty-pane 1 0 0 20 5))
           (p1  (make-no-pty-pane 2 0 0 20 5))
           (p2  (make-no-pty-pane 3 0 0 20 5))
           (win (make-window :id 1 :name "w" :width 62 :height 5
                             :panes (list p0 p1 p2)
                             :tree (make-layout-split
                                    :h (make-layout-leaf p0)
                                    (make-layout-split
                                     :h (make-layout-leaf p1)
                                     (make-layout-leaf p2) 1/2) 1/2))))
      (window-rotate win :up)
      ;; p0 should now be at the end
      (expect (eq p0 (third (window-panes win))))
      (expect (eq p1 (first (window-panes win))))))

  ;; window-rotate updates the saved zoom layout without changing the visible zoomed pane.
  (it "rotate-window-while-zoomed-preserves-zoom-state"
    (let* ((p0  (make-no-pty-pane 1 0 0 20 5))
           (p1  (make-no-pty-pane 2 0 0 20 5))
           (p2  (make-no-pty-pane 3 0 0 20 5))
           (win (make-window :id 1 :name "w" :width 62 :height 5
                             :panes (list p0 p1 p2)
                             :tree (make-layout-split
                                    :h (make-layout-leaf p0)
                                    (make-layout-split
                                     :h (make-layout-leaf p1)
                                     (make-layout-leaf p2) 1/2) 1/2))))
      (window-select-pane win p0)
      (cl-tmux/model:window-zoom-toggle win)
      (window-rotate win :up)
      (expect (cl-tmux/model::window-zoom-p win) :to-be-truthy)
      (expect (equal (list p0) (window-panes win)))
      (cl-tmux/model:window-zoom-toggle win)
      (expect (equal (list p1 p2 p0) (window-panes win)))))

  ;; window-rotate :down moves the last pane to the front of the panes list.
  (it "rotate-window-down-shifts-panes"
    (let* ((p0  (make-no-pty-pane 1 0 0 20 5))
           (p1  (make-no-pty-pane 2 0 0 20 5))
           (p2  (make-no-pty-pane 3 0 0 20 5))
           (win (make-window :id 1 :name "w" :width 62 :height 5
                             :panes (list p0 p1 p2)
                             :tree (make-layout-split
                                    :h (make-layout-leaf p0)
                                    (make-layout-split
                                     :h (make-layout-leaf p1)
                                     (make-layout-leaf p2) 1/2) 1/2))))
      (window-rotate win :down)
      (expect (eq p2 (first (window-panes win))))
      (expect (eq p0 (second (window-panes win)))))))

;;; ── find-window-by-name ──────────────────────────────────────────────────────
