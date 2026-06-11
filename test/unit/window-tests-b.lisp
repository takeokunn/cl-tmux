(in-package #:cl-tmux/test)

;;;; window tests — part B: apply-named-layout (main-horizontal/vertical/tiled),
;;;; last-window by recency, move-window, swap-window, rotate-window,
;;;; find-window-by-name, list-windows-format, auto-rename-from-osc,
;;;; window-remove-pane, window-last-active-time, window-layout-cycle-index.

(in-suite model-suite)

;;; ── apply-named-layout — remaining three named layouts ─────────────────────
;;;
;;; :main-horizontal, :main-vertical, and :tiled all contain substantively
;;; different geometry logic (floor-halving, secondary-pane subdivision,
;;; sqrt-based grid).  The tests below exercise each of these branches
;;; using make-no-pty-pane and make-fake-window from helpers.lisp.

(test apply-named-layout-main-horizontal-two-panes
  ":main-horizontal with 2 panes: the main pane spans the top main-pane-height
   rows (tmux default 24); the second fills the rest below it."
  (let* ((p0  (make-no-pty-pane 1 0 0 1 1))
         (p1  (make-no-pty-pane 2 0 0 1 1))
         ;; Window taller than the default main-pane-height so the layout is
         ;; non-degenerate: h=50 → main 24, rest = 50 - 24 - 1 = 25.
         (win (make-window :id 1 :name "w" :width 80 :height 50
                           :panes (list p0 p1)
                           :tree (make-layout-leaf p0))))
    (apply-named-layout win :main-horizontal)
    (is (= 0  (pane-x p0))      "main pane starts at column 0")
    (is (= 0  (pane-y p0))      "main pane starts at row 0")
    (is (= 80 (pane-width p0))  "main pane spans full width")
    (is (= 24 (pane-height p0)) "main pane height = main-pane-height (24)")
    ;; Secondary pane fills the bottom portion.
    (is (= 0  (pane-x p1))      "secondary pane starts at column 0")
    (is (= 25 (pane-y p1))      "secondary pane y = main-h + 1 separator")
    (is (= 80 (pane-width p1))  "secondary pane spans full width (1 pane below)")
    (is (= 25 (pane-height p1)) "secondary pane height = h - main-h - 1")))

(test apply-named-layout-main-horizontal-three-panes
  ":main-horizontal with 3 panes: main spans the top main-pane-height rows; two
   panes share the bottom region side by side with equal widths."
  (let* ((p0  (make-no-pty-pane 1 0 0 1 1))
         (p1  (make-no-pty-pane 2 0 0 1 1))
         (p2  (make-no-pty-pane 3 0 0 1 1))
         (win (make-window :id 1 :name "w" :width 81 :height 50
                           :panes (list p0 p1 p2)
                           :tree (make-layout-leaf p0))))
    (apply-named-layout win :main-horizontal)
    ;; main-h = main-pane-height = 24; rest-h = 50 - 24 - 1 = 25
    (is (= 24 (pane-height p0)) "main pane height = main-pane-height (24)")
    (is (= 25 (pane-height p1)) "secondary panes fill the bottom region height")
    (is (= 25 (pane-height p2)) "secondary panes fill the bottom region height")
    ;; Two secondary panes in a row: 81 cols - 1 separator = 80, floor(80/2) = 40 each
    (is (= 0  (pane-x p1))     "left secondary pane starts at column 0")
    (is (= 40 (pane-width p1)) "left secondary width = floor(avail/2)")
    (is (= 41 (pane-x p2))    "right secondary pane starts at column 41")
    (is (= 40 (pane-width p2)) "right secondary width = avail - left-w")))

(test apply-named-layout-main-horizontal-single-pane
  ":main-horizontal with a single pane: pane takes the full window rectangle."
  (let* ((p0  (make-no-pty-pane 1 0 0 1 1))
         (win (make-window :id 1 :name "w" :width 80 :height 24
                           :panes (list p0)
                           :tree (make-layout-leaf p0))))
    (apply-named-layout win :main-horizontal)
    (is (= 0  (pane-x p0)))
    (is (= 0  (pane-y p0)))
    (is (= 80 (pane-width  p0)))
    (is (= 24 (pane-height p0)))))

(test apply-named-layout-main-vertical-two-panes
  ":main-vertical with 2 panes: the main pane spans the left main-pane-width
   columns (tmux default 80); the second fills the right column."
  (let* ((p0  (make-no-pty-pane 1 0 0 1 1))
         (p1  (make-no-pty-pane 2 0 0 1 1))
         ;; Window wider than the default main-pane-width: w=120 → main 80,
         ;; rest = 120 - 80 - 1 = 39.
         (win (make-window :id 1 :name "w" :width 120 :height 24
                           :panes (list p0 p1)
                           :tree (make-layout-leaf p0))))
    (apply-named-layout win :main-vertical)
    (is (= 0  (pane-x p0))      "main pane starts at column 0")
    (is (= 0  (pane-y p0))      "main pane starts at row 0")
    (is (= 80 (pane-width p0))  "main pane width = main-pane-width (80)")
    (is (= 24 (pane-height p0)) "main pane spans full height")
    ;; Secondary pane fills the right column.
    (is (= 81 (pane-x p1))      "secondary pane x = main-w + 1 separator")
    (is (= 0  (pane-y p1))      "secondary pane starts at row 0")
    (is (= 39 (pane-width p1))  "secondary pane width = w - main-w - 1")
    (is (= 24 (pane-height p1)) "secondary pane spans full height (1 pane in column)")))

(test apply-named-layout-main-vertical-three-panes
  ":main-vertical with 3 panes: main spans the left main-pane-width columns; two
   panes share the right region stacked equally."
  (let* ((p0  (make-no-pty-pane 1 0 0 1 1))
         (p1  (make-no-pty-pane 2 0 0 1 1))
         (p2  (make-no-pty-pane 3 0 0 1 1))
         (win (make-window :id 1 :name "w" :width 120 :height 25
                           :panes (list p0 p1 p2)
                           :tree (make-layout-leaf p0))))
    (apply-named-layout win :main-vertical)
    ;; main-w = main-pane-width = 80; rest-w = 120 - 80 - 1 = 39
    (is (= 80 (pane-width p0))  "main pane width = main-pane-width (80)")
    (is (= 25 (pane-height p0)) "main pane spans full height")
    ;; Two secondary panes stacked: 25 rows - 1 separator = 24, floor(24/2) = 12 each
    (is (= 81 (pane-x p1))     "top secondary pane x = main-w + 1")
    (is (= 0  (pane-y p1))     "top secondary pane starts at row 0")
    (is (= 12 (pane-height p1)) "top secondary height = floor(avail/2)")
    (is (= 81 (pane-x p2))     "bottom secondary pane in the same column")
    (is (= 13 (pane-y p2))     "bottom secondary pane y = top-h + 1 separator")
    (is (= 12 (pane-height p2)) "bottom secondary height = avail - top-h")))

(test apply-named-layout-main-vertical-single-pane
  ":main-vertical with a single pane: pane takes the full window rectangle."
  (let* ((p0  (make-no-pty-pane 1 0 0 1 1))
         (win (make-window :id 1 :name "w" :width 80 :height 24
                           :panes (list p0)
                           :tree (make-layout-leaf p0))))
    (apply-named-layout win :main-vertical)
    (is (= 0  (pane-x p0)))
    (is (= 0  (pane-y p0)))
    (is (= 80 (pane-width  p0)))
    (is (= 24 (pane-height p0)))))

(test apply-named-layout-tiled-single-pane
  ":tiled with 1 pane fills the whole window (1×1 grid)."
  (let* ((p0  (make-no-pty-pane 1 0 0 1 1))
         (win (make-window :id 1 :name "w" :width 80 :height 24
                           :panes (list p0)
                           :tree (make-layout-leaf p0))))
    (apply-named-layout win :tiled)
    ;; ceil(sqrt(1)) = 1 col; ceil(1/1) = 1 row; col-w = 80; row-h = 24
    (is (= 0  (pane-x p0)))
    (is (= 0  (pane-y p0)))
    (is (= 80 (pane-width  p0)))
    (is (= 24 (pane-height p0)))))

(test apply-named-layout-tiled-four-panes
  ":tiled with 4 panes produces a 2×2 grid with equal cell sizes."
  (let* ((p0  (make-no-pty-pane 1 0 0 1 1))
         (p1  (make-no-pty-pane 2 0 0 1 1))
         (p2  (make-no-pty-pane 3 0 0 1 1))
         (p3  (make-no-pty-pane 4 0 0 1 1))
         (win (make-window :id 1 :name "w" :width 81 :height 25
                           :panes (list p0 p1 p2 p3)
                           :tree (make-layout-leaf p0))))
    (apply-named-layout win :tiled)
    ;; ceil(sqrt(4)) = 2 cols; ceil(4/2) = 2 rows
    ;; col-w = floor((81 - 1) / 2) = 40; row-h = floor((25 - 1) / 2) = 12
    (is (=  0 (pane-x p0)) "p0 col 0")
    (is (=  0 (pane-y p0)) "p0 row 0")
    (is (= 40 (pane-width  p0)))
    (is (= 12 (pane-height p0)))
    (is (= 41 (pane-x p1)) "p1 col 1: x = 1*(40+1)")
    (is (=  0 (pane-y p1)) "p1 row 0")
    (is (= 40 (pane-width  p1)))
    (is (= 12 (pane-height p1)))
    (is (=  0 (pane-x p2)) "p2 col 0")
    (is (= 13 (pane-y p2)) "p2 row 1: y = 1*(12+1)")
    (is (= 40 (pane-width  p2)))
    (is (= 12 (pane-height p2)))
    (is (= 41 (pane-x p3)) "p3 col 1")
    (is (= 13 (pane-y p3)) "p3 row 1")
    (is (= 40 (pane-width  p3)))
    (is (= 12 (pane-height p3)))))

(test apply-named-layout-tiled-three-panes
  ":tiled with 3 panes produces a 2-column grid (2×2 with one empty cell)."
  (let* ((p0  (make-no-pty-pane 1 0 0 1 1))
         (p1  (make-no-pty-pane 2 0 0 1 1))
         (p2  (make-no-pty-pane 3 0 0 1 1))
         (win (make-window :id 1 :name "w" :width 81 :height 25
                           :panes (list p0 p1 p2)
                           :tree (make-layout-leaf p0))))
    (apply-named-layout win :tiled)
    ;; ceil(sqrt(3)) = 2 cols; ceil(3/2) = 2 rows
    ;; col-w = floor((81-1)/2) = 40; row-h = floor((25-1)/2) = 12
    ;; p0 at (col=0,row=0) p1 at (col=1,row=0) p2 at (col=0,row=1)
    (is (=  0 (pane-x p0)))
    (is (=  0 (pane-y p0)))
    (is (= 41 (pane-x p1)))
    (is (=  0 (pane-y p1)))
    (is (=  0 (pane-x p2)))
    (is (= 13 (pane-y p2)) "p2 placed in second row")))

;;; ── last-window by recency ───────────────────────────────────────────────────

(test last-window-by-recency
  "session-last-window returns the window with the second-highest last-active-time."
  (let* ((w0 (make-window :id 1 :name "0" :last-active-time 100))
         (w1 (make-window :id 2 :name "1" :last-active-time 200))
         (w2 (make-window :id 3 :name "2" :last-active-time 300))
         (sess (make-session :id 1 :name "s" :windows (list w0 w1 w2))))
    (session-select-window sess w2)
    ;; second-most-recent is w1 (time 200)
    (is (eq w1 (session-last-window sess))
        "session-last-window must return the window with the second-highest last-active-time")))

(test last-window-single-window-returns-nil
  "session-last-window returns NIL when there is only one window."
  (let* ((w0   (make-window :id 1 :name "0" :last-active-time 100))
         (sess (make-session :id 1 :name "s" :windows (list w0))))
    (is (null (session-last-window sess))
        "session-last-window must return NIL for a single-window session")))

;;; ── move-window-reorders ─────────────────────────────────────────────────────

(test move-window-reorders
  "session-move-window moves a window to the requested position."
  (let* ((w0 (make-window :id 1 :name "0"))
         (w1 (make-window :id 2 :name "1"))
         (w2 (make-window :id 3 :name "2"))
         (sess (make-session :id 1 :name "s" :windows (list w0 w1 w2))))
    ;; Move w0 (currently index 0) to index 2 (the end).
    (session-move-window sess w0 2)
    (is (equal (list w1 w2 w0) (session-windows sess))
        "w0 must move to position 2")))

(test move-window-clamps-to-last
  "session-move-window clamps out-of-range target to last valid index."
  (let* ((w0 (make-window :id 1 :name "0"))
         (w1 (make-window :id 2 :name "1"))
         (sess (make-session :id 1 :name "s" :windows (list w0 w1))))
    (session-move-window sess w0 99)
    (is (equal (list w1 w0) (session-windows sess))
        "w0 must land at index 1 (clamped from 99)")))

;;; ── swap-window-exchanges ────────────────────────────────────────────────────

(test swap-window-exchanges
  "session-swap-windows exchanges two windows at the given indices."
  (let* ((w0 (make-window :id 1 :name "0"))
         (w1 (make-window :id 2 :name "1"))
         (w2 (make-window :id 3 :name "2"))
         (sess (make-session :id 1 :name "s" :windows (list w0 w1 w2))))
    (session-swap-windows sess 0 2)
    (is (equal (list w2 w1 w0) (session-windows sess))
        "indices 0 and 2 must be swapped")))

(test swap-window-same-index-is-noop
  "session-swap-windows with equal indices leaves the list unchanged."
  (let* ((w0 (make-window :id 1 :name "0"))
         (w1 (make-window :id 2 :name "1"))
         (sess (make-session :id 1 :name "s" :windows (list w0 w1))))
    (session-swap-windows sess 0 0)
    (is (equal (list w0 w1) (session-windows sess))
        "same-index swap must be a no-op")))

;;; ── rotate-window-shifts-panes ───────────────────────────────────────────────

(test rotate-window-shifts-panes
  "window-rotate :up moves the first pane to the end of the panes list."
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
    (is (eq p0 (third (window-panes win)))
        "first pane must move to end after :up rotate")
    (is (eq p1 (first (window-panes win)))
        "second pane must become first after :up rotate")))

(test rotate-window-down-shifts-panes
  "window-rotate :down moves the last pane to the front of the panes list."
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
    (is (eq p2 (first (window-panes win)))
        "last pane must become first after :down rotate")
    (is (eq p0 (second (window-panes win)))
        "original first pane must shift to position 1")))

;;; ── find-window-by-name ──────────────────────────────────────────────────────
