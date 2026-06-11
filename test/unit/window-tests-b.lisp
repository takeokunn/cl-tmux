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

(test find-window-by-name
  "%format-window-list includes matching window names."
  (let* ((w0 (make-window :id 1 :name "bash" :width 80 :height 24
                          :panes (list (make-no-pty-pane 1 0 0 80 24))))
         (w1 (make-window :id 2 :name "vim" :width 80 :height 24
                          :panes (list (make-no-pty-pane 2 0 0 80 24))))
         (sess (make-session :id 1 :name "s" :windows (list w0 w1))))
    (session-select-window sess w0)
    (let ((listing (cl-tmux::%format-window-list sess)))
      (is (search "bash" listing) "listing must contain window name 'bash'")
      (is (search "vim"  listing) "listing must contain window name 'vim'"))))

;;; ── list-windows-format ──────────────────────────────────────────────────────

(test list-windows-format
  "%format-window-list includes the window's stored id, name, dimensions, and active marker."
  (let* ((p0  (make-no-pty-pane 1 0 0 80 24))
         ;; Use id=0 so that the listing shows "0:" as the index prefix.
         (w0  (make-window :id 0 :name "main" :width 80 :height 24
                           :panes (list p0)))
         (sess (make-session :id 1 :name "s" :windows (list w0))))
    (session-select-window sess w0)
    (let ((listing (cl-tmux::%format-window-list sess)))
      (is (search "main"    listing) "listing must include window name")
      (is (search "80x24"   listing) "listing must include dimensions")
      (is (search "[active]" listing) "active window must be marked [active]")
      (is (search "0:"      listing) "listing must include the window-id (0) as prefix"))))

;;; ── auto-rename-from-osc ─────────────────────────────────────────────────────
;;;
;;; These tests call the production function cl-tmux::%maybe-rename-window-from-title
;;; directly, rather than duplicating the rename logic inline.  This ensures the
;;; tests verify the real code path and provide genuine coverage confidence.

(test auto-rename-from-osc
  "When window-automatic-rename-p is T, window-name is updated from OSC title."
  (with-loop-state
    (let* ((p0   (make-no-pty-pane 1 0 0 80 24))
           (w0   (make-window :id 1 :name "original"
                              :panes (list p0) :active p0))
           (sess (make-session :id 1 :name "s" :windows (list w0))))
      (session-select-window sess w0)
      ;; Simulate OSC 0 title update on the screen.
      (setf (screen-title (pane-screen p0)) "new-title")
      ;; Call the production rename function — not a copy of its logic.
      (cl-tmux::%maybe-rename-window-from-title sess)
      (is (string= "new-title" (window-name w0))
          "window-name must be updated from OSC title when automatic-rename is enabled"))))

(test auto-rename-disabled-ignores-osc
  "When window-automatic-rename-p is NIL, window-name is NOT updated from OSC title."
  (with-loop-state
    (let* ((p0   (make-no-pty-pane 1 0 0 80 24))
           (w0   (make-window :id 1 :name "kept"
                              :automatic-rename-p nil
                              :panes (list p0) :active p0))
           (sess (make-session :id 1 :name "s" :windows (list w0))))
      (session-select-window sess w0)
      (setf (screen-title (pane-screen p0)) "ignored-title")
      ;; Call the production rename function; automatic-rename-p nil must suppress it.
      (cl-tmux::%maybe-rename-window-from-title sess)
      (is (string= "kept" (window-name w0))
          "window-name must NOT change when automatic-rename is disabled"))))

;;; ── window-remove-pane (no PTY) ──────────────────────────────────────────────

(test window-remove-pane-empties-single-pane-window
  "window-remove-pane on a single-pane window returns NIL and clears the tree."
  (let* ((p0  (make-no-pty-pane 1 0 0 80 24))
         (win (make-window :id 1 :name "w" :width 80 :height 24
                           :panes (list p0)
                           :tree (make-layout-leaf p0))))
    (window-select-pane win p0)
    (let ((result (window-remove-pane win p0)))
      (is (null result)
          "window-remove-pane on a sole pane must return NIL (no survivor)")
      (is (null (window-panes win))
          "window panes list must be empty after removing the sole pane")
      (is (null (window-tree win))
          "window tree must be NIL after removing the sole pane"))))

(test window-remove-pane-returns-sibling
  "window-remove-pane returns the surviving sibling pane after removing one of two."
  (with-h-split-window (win p0 p1)
    (let ((survivor (window-remove-pane win p0)))
      (is (not (null survivor))
          "window-remove-pane must return the surviving pane")
      (is (= 1 (length (window-panes win)))
          "one pane must remain after removing one of two"))))

;;; ── window-last-active-time slot ─────────────────────────────────────────────

(test window-last-active-time-updated-on-select
  "window-select-pane updates window-last-active-time to a recent value."
  (let* ((p0  (make-no-pty-pane 1 0 0 20 5))
         (win (make-window :id 1 :name "w" :width 20 :height 5
                           :panes (list p0) :last-active-time 0)))
    (let ((before (get-universal-time)))
      (window-select-pane win p0)
      (is (>= (window-last-active-time win) before)
          "window-last-active-time must be updated when a pane is selected"))))

;;; ── window-layout-cycle-index slot ──────────────────────────────────────────

(test window-layout-cycle-index-defaults-zero
  "window-layout-cycle-index defaults to 0 for a freshly created window."
  (let ((win (make-window :id 1 :name "w")))
    (is (= 0 (window-layout-cycle-index win))
        "window-layout-cycle-index must default to 0")))

;;; ── ensure-window-fits with matching size ────────────────────────────────────
;;;
;;; This test is identical in structure to window-tests.lisp's existing
;;; ensure-window-fits-noop-when-size-matches but targets the update of
;;; window-width/height as the observable: if size differs, relayout runs;
;;; if same, dimensions stay untouched.

(test ensure-window-fits-does-not-mutate-on-matching-size
  "ensure-window-fits leaves pane geometry untouched when size already matches."
  (let* ((p0  (make-no-pty-pane 1 0 0 80 24))
         (win (make-window :id 1 :name "w" :width 80 :height 24
                           :tree (make-layout-leaf p0)
                           :panes (list p0) :active p0)))
    (let ((x0-before (pane-x p0))
          (y0-before (pane-y p0)))
      (cl-tmux/model::ensure-window-fits win 24 80)
      (is (= x0-before (pane-x p0))
          "pane-x must not change when size already matches")
      (is (= y0-before (pane-y p0))
          "pane-y must not change when size already matches"))))

;;; ── window struct default slots ─────────────────────────────────────────────

(test window-zoom-p-defaults-nil
  "window-zoom-p defaults to NIL for a freshly created window."
  (let ((win (make-window :id 1 :name "w")))
    (is (null (cl-tmux/model:window-zoom-p win))
        "window-zoom-p must default to NIL")))

(test window-zoom-tree-defaults-nil
  "window-zoom-tree defaults to NIL for a freshly created window."
  (let ((win (make-window :id 1 :name "w")))
    (is (null (cl-tmux/model:window-zoom-tree win))
        "window-zoom-tree must default to NIL")))

(test window-last-active-defaults-nil
  "window-last-active defaults to NIL for a freshly created window."
  (let ((win (make-window :id 1 :name "w")))
    (is (null (window-last-active win))
        "window-last-active must default to NIL")))

(test window-automatic-rename-p-defaults-true
  "window-automatic-rename-p defaults to T for a freshly created window."
  (let ((win (make-window :id 1 :name "w")))
    (is-true (window-automatic-rename-p win)
             "window-automatic-rename-p must default to T")))

(test window-automatic-rename-p-settable
  "window-automatic-rename-p can be set to NIL and read back."
  (let ((win (make-window :id 1 :name "w" :automatic-rename-p nil)))
    (is (null (window-automatic-rename-p win))
        "window-automatic-rename-p must reflect the value set at construction")))

;;; ── window-active-pane falls back to first pane ─────────────────────────────

(test window-active-pane-falls-back-to-first-pane
  "window-active-pane returns the first pane when active slot is NIL."
  (let* ((p0  (make-no-pty-pane 1  0 0 40 24))
         (p1  (make-no-pty-pane 2 41 0 40 24))
         ;; No active pane set.
         (win (make-window :id 1 :name "w" :panes (list p0 p1))))
    (is (eq p0 (window-active-pane win))
        "window-active-pane must fall back to the first pane when active is NIL")))

;;; ── window-select-pane records previous active as last-active ──────────────

(test window-select-pane-records-previous-as-last-active
  "window-select-pane records the previously active pane in window-last-active."
  (let* ((p0  (make-no-pty-pane 1  0 0 40 24))
         (p1  (make-no-pty-pane 2 41 0 40 24))
         (win (make-window :id 1 :name "w" :panes (list p0 p1))))
    (window-select-pane win p0)
    (is (null (window-last-active win))
        "last-active must be NIL after first select (no prior pane)")
    (window-select-pane win p1)
    (is (eq p0 (window-last-active win))
        "last-active must be the previously active pane after switching")))

;;; ── window-remove-pane: leaf not in tree ────────────────────────────────────

(test window-remove-pane-absent-pane-returns-first-pane
  "window-remove-pane returns the first pane when the target leaf is absent from the tree."
  (let* ((p0  (make-no-pty-pane 1 0 0 40 24))
         (p1  (make-no-pty-pane 2 41 0 40 24))
         ;; Build window with p0 in the tree; p1 is not in the tree.
         (win (make-window :id 1 :name "w" :width 81 :height 24
                           :panes (list p0 p1)
                           :tree (make-layout-leaf p0))))
    ;; Removing p1 which is absent from the tree should return the first pane.
    (let ((result (window-remove-pane win p1)))
      (is-true result "result must be non-NIL (the first pane)")
      ;; The tree should be unchanged.
      (is-true (window-tree win) "tree must remain non-NIL"))))

;;; ── Table-driven %new-split-ratio (additional boundary cases) ───────────────

(test new-split-ratio-additional-cases
  "Table-driven: %new-split-ratio handles boundary and asymmetric ratio cases."
  ;; Each entry: (orient avail cur-ratio delta grow-first expected description)
  ;; These cases extend beyond the single tests (basic-grow/shrink/blocked-by-floor).
  (dolist (entry
           '((:h 100 3/4 10 t   85/100 "grow :h from 3/4 ratio")
             (:v 40  1/4  5 t   15/40  "grow :v from 1/4 ratio")
             (:h 60  2/3  1 nil 39/60  "shrink :h from 2/3 ratio")))
    (destructuring-bind (orient avail cur-ratio delta grow-first expected desc) entry
      (let ((result (cl-tmux/model::%new-split-ratio orient avail cur-ratio delta grow-first)))
        (is (equal expected result) desc)))))

;;; ── window-rotate single-pane is noop ───────────────────────────────────────

(test window-rotate-single-pane-noop
  "window-rotate on a single-pane window changes nothing."
  (let* ((p0  (make-no-pty-pane 1 0 0 80 24))
         (win (make-window :id 1 :name "w" :width 80 :height 24
                           :panes (list p0)
                           :tree (make-layout-leaf p0))))
    (window-rotate win :up)
    (is (equal (list p0) (window-panes win))
        "single-pane window panes list unchanged after :up rotate")
    (window-rotate win :down)
    (is (equal (list p0) (window-panes win))
        "single-pane window panes list unchanged after :down rotate")))

;;; ── window-id and window-name accessors ─────────────────────────────────────

(test window-id-slot-accessible
  "window-id returns the id passed to make-window."
  (let ((win (make-window :id 42 :name "test")))
    (is (= 42 (window-id win))
        "window-id must return the id set at construction")))

(test window-name-slot-accessible
  "window-name returns the name passed to make-window."
  (let ((win (make-window :id 1 :name "mywin")))
    (is (string= "mywin" (window-name win))
        "window-name must return the name set at construction")))

;;; ── window-width and window-height accessors ────────────────────────────────

(test window-width-height-slot-accessible
  "window-width and window-height return the geometry set at construction."
  (let ((win (make-window :id 1 :name "w" :width 120 :height 40)))
    (is (= 120 (window-width  win)) "window-width must return 120")
    (is (= 40  (window-height win)) "window-height must return 40")))

;;; ── pane-window back-pointer wiring ──────────────────────────────────────────
;;;
;;; pane-window is set by window-split and %attach-full-screen-pane (production),
;;; and cleared by window-remove-pane.  The tests below verify the clear path
;;; without requiring a real PTY.  The split/attach set path is verified by the
;;; PTY-gated test below.

(test window-remove-pane-clears-pane-window-sole-pane
  "window-remove-pane on the sole pane sets pane-window to NIL."
  (let* ((p0  (make-no-pty-pane 1 0 0 80 24))
         (win (make-window :id 1 :name "w" :width 80 :height 24
                           :panes (list p0)
                           :tree (make-layout-leaf p0))))
    (setf (pane-window p0) win)
    (window-remove-pane win p0)
    (is (null (pane-window p0))
        "pane-window of the sole removed pane must be NIL after removal")))

(test window-remove-pane-clears-pane-window-preserves-survivor
  "window-remove-pane clears pane-window only for the removed pane."
  (with-h-split-window (win p0 p1)
    (setf (pane-window p0) win
          (pane-window p1) win)
    (window-remove-pane win p0)
    (is (null (pane-window p0))
        "pane-window of the removed pane must be NIL")
    (is (eq win (pane-window p1))
        "pane-window of the surviving pane must remain pointing to its window")))

(test window-split-sets-pane-window-back-pointer
  "window-split wires pane-window on the new pane to the parent window."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (with-session (session 24 80)
    (let* ((win   (session-active-window session))
           (p-new (window-split win :h)))
      (is (eq win (pane-window p-new))
          "new pane's pane-window must point to its window after split"))))
