(in-package #:cl-tmux/test)

;;;; Window-level tests: window struct, relayout, splits, resize.
;;;;
;;;; NOTE: NO-PTY relayout tests build panes by hand with :fd -1 :pid -1 (no
;;;; real PTY) and a directly constructed virtual screen.  pane-reposition /
;;;; window-relayout call set-pty-size on fd -1, which is a tolerated EBADF
;;;; no-op (ioctl returns -1 without signalling a Lisp condition), so the only
;;;; observable effect is the pane geometry update and the screen-resize.  They
;;;; therefore run real assertions in the sandbox without gating on pty-available-p.

(in-suite model-suite)

;;; ── window-relayout preserves a vertical split (no PTY) ────────────────────

(test window-relayout-vertical-preserves-split-no-pty
  "window-relayout reflows a vertical 2-pane tree window into the new geometry,
   updating both pane rectangles and the underlying screen dimensions."
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
      (is (= new-cols (window-width  win)))
      (is (= new-rows (window-height win)))
      ;; Both panes have correct height and positive width.
      (is (= new-rows (pane-height p0)) "pane 1 height")
      (is (= new-rows (pane-height p1)) "pane 2 height")
      (is (plusp (pane-width p0)) "pane 1 has positive width")
      (is (plusp (pane-width p1)) "pane 2 has positive width")
      ;; Total coverage: two pane widths + 1 separator = new-cols.
      (is (= new-cols (+ (pane-width p0) 1 (pane-width p1)))
          "pane widths + separator must equal ~D" new-cols)
      ;; Screens match pane dimensions.
      (is (= (pane-width p0) (screen-width (pane-screen p0))) "pane 1 screen-width")
      (is (= (pane-height p0) (screen-height (pane-screen p0))) "pane 1 screen-height")
      (is (= (pane-width p1) (screen-width (pane-screen p1))) "pane 2 screen-width")
      (is (= (pane-height p1) (screen-height (pane-screen p1))) "pane 2 screen-height")
      ;; Sanity: separator gap is one column between panes.
      (is (= 1 (- (pane-x p1) (+ (pane-x p0) (pane-width p0))))
          "exactly one separator column between vertical panes"))))

;;; ── window-relayout with NIL layout ────────────────────────────────────────

(test window-relayout-single-pane-tree-fullscreen
  "A single-pane window (tree = one leaf) relayouts to give the pane the full
   (0 0 cols rows) rectangle."
  (let* ((pane (make-no-pty-pane 1 5 5 10 10))
         (win  (make-window :id 1 :name "w"
                            :width 10 :height 10
                            :tree (make-layout-leaf pane)
                            :panes (list pane) :active pane)))
    (window-relayout win 30 90)
    (is (= 0  (pane-x      pane)))
    (is (= 0  (pane-y      pane)))
    (is (= 90 (pane-width  pane)))
    (is (= 30 (pane-height pane)))
    (is (= 90 (screen-width  (pane-screen pane))))
    (is (= 30 (screen-height (pane-screen pane))))))

(test window-relayout-no-tree-is-noop-for-panes
  "With a NIL tree, window-relayout updates the stored dimensions but does not
   reposition any panes (no tree to walk)."
  (let* ((p0 (make-no-pty-pane 1 5 5 10 10))
         (p1 (make-no-pty-pane 2 7 7 12 12))
         (win (make-window :id 1 :name "w"
                           :width 10 :height 10
                           :panes (list p0 p1) :active p0)))
    (window-relayout win 30 90)
    ;; Dimensions updated.
    (is (= 90 (window-width  win)))
    (is (= 30 (window-height win)))
    ;; Without a tree no pane is repositioned.
    (is (= 5  (pane-x      p0)) "p0 x unchanged: no tree")
    (is (= 5  (pane-y      p0)) "p0 y unchanged: no tree")
    (is (= 10 (pane-width  p0)) "p0 width unchanged: no tree")
    (is (= 10 (pane-height p0)) "p0 height unchanged: no tree")
    (is (= 7  (pane-x      p1)) "p1 x unchanged: no tree")
    (is (= 7  (pane-y      p1)) "p1 y unchanged: no tree")
    (is (= 12 (pane-width  p1)) "p1 width unchanged: no tree")
    (is (= 12 (pane-height p1)) "p1 height unchanged: no tree")))

;;; ── ensure-window-fits ───────────────────────────────────────────────────────

(test ensure-window-fits-relayouts-on-size-change
  "ensure-window-fits relayouts when the requested size differs from the
   window's stored size, fixing a pane left with stale geometry."
  (let* ((pane (make-no-pty-pane 1 0 0 80 24))
         (win  (make-window :id 1 :name "w"
                            :width 80 :height 24
                            :tree (make-layout-leaf pane)
                            :panes (list pane) :active pane)))
    ;; Deliberately leave the window's stored size inconsistent with the
    ;; requested target so ensure-window-fits must act.
    (cl-tmux/model::ensure-window-fits win 30 100)
    (is (= 100 (window-width  win)))
    (is (= 30  (window-height win)))
    ;; Single-pane tree: pane spans the whole window.
    (is (= 100 (pane-width  pane)))
    (is (= 30  (pane-height pane)))
    (is (= 100 (screen-width  (pane-screen pane))))
    (is (= 30  (screen-height (pane-screen pane))))))

(test ensure-window-fits-noop-when-size-matches
  "ensure-window-fits is a no-op when the window already matches the requested
   size: geometry is left untouched and the same screen object is retained
   (no screen-resize, so EQ holds)."
  (let* ((screen (make-screen 80 24))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 80 :height 24
                            :fd -1 :pid -1 :screen screen))
         (win    (make-window :id 1 :name "w"
                              :width 80 :height 24
                              :tree (make-layout-leaf pane)
                              :panes (list pane) :active pane)))
    ;; Same size as stored → nothing should happen.
    (cl-tmux/model::ensure-window-fits win 24 80)
    (is (= 80 (window-width  win)))
    (is (= 24 (window-height win)))
    (is (= 0  (pane-x      pane)))
    (is (= 0  (pane-y      pane)))
    (is (= 80 (pane-width  pane)))
    (is (= 24 (pane-height pane)))
    ;; The exact screen object must be preserved (relayout was skipped).
    (is (eq screen (pane-screen pane))
        "pane screen object must be unchanged when size already matches")
    (is (= 80 (screen-width  (pane-screen pane))))
    (is (= 24 (screen-height (pane-screen pane))))))

;;; ── Splitting and selecting panes ─────────────────────────────────────────

(test window-select-pane
  "After a split the first pane can be re-selected as active."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (with-session (session 24 80)
    (let* ((win        (session-active-window session))
           (first-pane (window-active-pane win)))
      ;; Split left/right (:h) → two panes; active switches to the new one.
      (window-split win :h)
      (is (= 2 (length (window-panes win))) "must have 2 panes after split")
      (is (not (eq first-pane (window-active-pane win)))
          "active pane must be the new (second) pane after split")
      ;; Select the first pane back.
      (window-select-pane win first-pane)
      (is (eq first-pane (window-active-pane win))
          "window-active-pane must return the pane passed to window-select-pane"))))

;;; ── Resizing a pane ─────────────────────────────────────────────────────────

(test resize-pane-vertical
  "resize-pane :right grows the active pane and shrinks its right neighbour."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (with-session (session 24 80)
    (let* ((win (session-active-window session))
           (p0  (window-active-pane win)))
      (window-split win :h)               ; p0 | p1 side-by-side, active becomes p1
      (window-select-pane win p0)         ; make the left pane active
      (let* ((p1        (second (window-panes win)))
             (w0-before (pane-width p0))
             (w1-before (pane-width p1)))
        (resize-pane win :right 5)
        (is (= (+ w0-before 5) (pane-width p0))
            "active (left) pane should grow by 5: ~D → ~D"
            w0-before (pane-width p0))
        (is (= (- w1-before 5) (pane-width p1))
            "right neighbour should shrink by 5: ~D → ~D"
            w1-before (pane-width p1))))))

(test resize-pane-horizontal
  "resize-pane :down grows the active pane and shrinks the pane below it."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (with-session (session 24 80)
    (let* ((win (session-active-window session))
           (p0  (window-active-pane win)))
      (window-split win :v)               ; p0 / p1 stacked top/bottom, active becomes p1
      (window-select-pane win p0)         ; make the top pane active
      (let* ((p1        (second (window-panes win)))
             (h0-before (pane-height p0))
             (h1-before (pane-height p1)))
        (resize-pane win :down 3)
        (is (= (+ h0-before 3) (pane-height p0))
            "active (top) pane should grow by 3: ~D → ~D"
            h0-before (pane-height p0))
        (is (= (- h1-before 3) (pane-height p1))
            "lower neighbour should shrink by 3: ~D → ~D"
            h1-before (pane-height p1))))))

(test resize-pane-wrong-axis-is-noop
  "A :up/:down resize on a vertical split leaves pane widths unchanged."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (with-session (session 24 80)
    (let* ((win (session-active-window session))
           (p0  (window-active-pane win)))
      (window-split win :h)               ; p0 | p1 side-by-side
      (window-select-pane win p0)
      (let ((w0-before (pane-width p0)))
        (resize-pane win :up 5)            ; wrong axis for an :h split
        (is (= w0-before (pane-width p0))
            ":h split must ignore a :up resize (wrong axis)")))))

;;; ── split-child-geometry direct tests (pure, no PTY) ─────────────────────

(test split-child-geometry-h-orient
  "split-child-geometry :h gives the right half of a pane."
  (let ((p (make-pane :id 1 :x 0 :y 0 :width 41 :height 20 :fd -1 :pid -1
                      :screen (make-screen 41 20))))
    (multiple-value-bind (px py pw ph)
        (cl-tmux/model::split-child-geometry p :h)
      ;; avail = 41 - 1 = 40; fw = floor(40/2) = 20; child at x = 20+1 = 21, w = 40-20 = 20
      (is (= 21 px) "child x = parent-x + fw + 1")
      (is (= 0  py))
      (is (= 20 pw))
      (is (= 20 ph)))))

(test split-child-geometry-v-orient
  "split-child-geometry :v gives the bottom half of a pane."
  (let ((p (make-pane :id 1 :x 0 :y 0 :width 80 :height 25 :fd -1 :pid -1
                      :screen (make-screen 80 25))))
    (multiple-value-bind (px py pw ph)
        (cl-tmux/model::split-child-geometry p :v)
      ;; avail = 25 - 1 = 24; fh = floor(24/2) = 12; child at y = 12+1 = 13, h = 24-12 = 12
      (is (= 0  px))
      (is (= 13 py) "child y = parent-y + fh + 1")
      (is (= 80 pw))
      (is (= 12 ph)))))

;;; ── %new-split-ratio direct tests (pure, no PTY) ─────────────────────────

(test new-split-ratio-basic-grow
  "%new-split-ratio returns a larger ratio when growing the first child."
  (let ((new-ratio (cl-tmux/model::%new-split-ratio :h 80 1/2 5 t)))
    ;; cur-first = round(80 * 1/2) = 40; new-first = 40 + 5 = 45; ratio = 45/80
    (is (= 45/80 new-ratio))))

(test new-split-ratio-blocked-by-floor
  "%new-split-ratio returns NIL when the move would shrink a side below minimum."
  ;; avail = 10, floor* = 2 (pane-min-width for :h); cur = 5; grow by 10 → new = 15 > hi (8)
  (is (null (cl-tmux/model::%new-split-ratio :h 10 1/2 10 t))))

(test new-split-ratio-shrink
  "%new-split-ratio shrinks first child when grow-first is NIL."
  (let ((new-ratio (cl-tmux/model::%new-split-ratio :v 20 1/2 3 nil)))
    ;; cur-first = 10; sign = -1; new-first = 7; ratio = 7/20
    (is (= 7/20 new-ratio))))

;;; ── window-refresh-panes ────────────────────────────────────────────────────

(test window-refresh-panes-derives-list-from-tree
  "window-refresh-panes recomputes (window-panes win) from the split tree."
  (let* ((p0   (make-pane :id 1 :fd -1 :pid -1 :width 40 :height 24
                          :screen (make-screen 40 24)))
         (p1   (make-pane :id 2 :fd -1 :pid -1 :width 40 :height 24
                          :screen (make-screen 40 24)))
         (tree (make-layout-split :h (make-layout-leaf p0) (make-layout-leaf p1)))
         (win  (make-window :id 1 :name "w" :tree tree :panes nil)))
    (window-refresh-panes win)
    (is (equal (list p0 p1) (window-panes win)))))

;;; ── Private helper tests ────────────────────────────────────────────────────

(test split-fits-p-returns-t-when-large-enough
  "%split-fits-p returns T when the pane axis is >= 2*min + 1."
  (let ((p (make-pane :id 1 :fd -1 :pid -1 :width 5 :height 3
                      :screen (make-screen 5 3))))
    ;; :h needs >= 2*2+1 = 5 cols: exactly 5 → fits
    (is-true  (cl-tmux/model::%split-fits-p p :h))
    ;; :v needs >= 2*1+1 = 3 rows: exactly 3 → fits
    (is-true  (cl-tmux/model::%split-fits-p p :v))))

(test split-fits-p-returns-nil-when-too-small
  "%split-fits-p returns NIL when the pane is too small to split."
  (let ((p-narrow (make-pane :id 1 :fd -1 :pid -1 :width 4 :height 5
                              :screen (make-screen 4 5)))
        (p-short  (make-pane :id 1 :fd -1 :pid -1 :width 5 :height 2
                              :screen (make-screen 5 2))))
    ;; :h needs 5, only 4 → does not fit
    (is-false (cl-tmux/model::%split-fits-p p-narrow :h))
    ;; :v needs 3, only 2 → does not fit
    (is-false (cl-tmux/model::%split-fits-p p-short :v))))

(test replace-in-tree-updates-parent-link
  "%replace-in-tree splices a replacement in place of a leaf."
  (let* ((l0   (tl-leaf 1 1 1))
         (l1   (tl-leaf 2 1 1))
         (tree (make-layout-split :h l0 l1))
         (win  (make-window :id 1 :name "w" :tree tree :panes nil)))
    (let ((new-leaf (tl-leaf 3 1 1)))
      (cl-tmux/model::%replace-in-tree win l0 new-leaf)
      (is (eq new-leaf (cl-tmux/model::layout-split-first (window-tree win)))
          "first child must be new-leaf after %replace-in-tree"))))

(test collapse-parent-promotes-sibling
  "%collapse-parent replaces the parent split with the surviving sibling node."
  (let* ((l0   (tl-leaf 1 1 1))
         (l1   (tl-leaf 2 1 1))
         (tree (make-layout-split :h l0 l1))
         (win  (make-window :id 1 :name "w" :tree tree :panes nil)))
    ;; Collapse the :first child — l1 should become the new root
    (let ((sibling (cl-tmux/model::%collapse-parent win tree :first)))
      (is (eq l1 sibling)          "sibling of :first is :second (l1)")
      (is (eq l1 (window-tree win)) "tree root must be updated to the sibling")))
  ;; Collapse the :second child — l0 should become the new root
  (let* ((l0   (tl-leaf 1 1 1))
         (l1   (tl-leaf 2 1 1))
         (tree (make-layout-split :h l0 l1))
         (win  (make-window :id 1 :name "w" :tree tree :panes nil)))
    (let ((sibling (cl-tmux/model::%collapse-parent win tree :second)))
      (is (eq l0 sibling)          "sibling of :second is :first (l0)")
      (is (eq l0 (window-tree win)) "tree root must be l0 after collapsing :second"))))

;;; ── pane-neighbor (directional navigation) ───────────────────────────────────

(defun %two-pane-h-window ()
  "Build a laid-out :h split window: p0 (x=0 w=40) | p1 (x=41 w=40), h=24, w=81."
  (let* ((p0  (make-no-pty-pane 1  0 0 40 24))
         (p1  (make-no-pty-pane 2 41 0 40 24))
         (win (make-window :id 1 :name "w" :width 81 :height 24
                           :panes (list p0 p1)
                           :tree (make-layout-split :h (make-layout-leaf p0)
                                                       (make-layout-leaf p1) 1/2))))
    (window-select-pane win p0)
    (values win p0 p1)))

(test pane-neighbor-right-in-h-split
  "Right neighbor of the left pane is the right pane."
  (multiple-value-bind (win p0 p1) (%two-pane-h-window)
    (is (eq p1 (cl-tmux/model::pane-neighbor win p0 :right)))))

(test pane-neighbor-left-in-h-split
  "Left neighbor of the right pane is the left pane."
  (multiple-value-bind (win p0 p1) (%two-pane-h-window)
    (is (eq p0 (cl-tmux/model::pane-neighbor win p1 :left)))))

(test pane-neighbor-nil-for-single-pane
  "A single pane has no neighbor in any direction."
  (let* ((p0  (make-no-pty-pane 1 0 0 80 24))
         (win (make-window :id 1 :name "w" :width 80 :height 24
                           :panes (list p0)
                           :tree (make-layout-leaf p0))))
    (window-select-pane win p0)
    (is (null (cl-tmux/model::pane-neighbor win p0 :right)))
    (is (null (cl-tmux/model::pane-neighbor win p0 :left)))
    (is (null (cl-tmux/model::pane-neighbor win p0 :up)))
    (is (null (cl-tmux/model::pane-neighbor win p0 :down)))))

;;; ── apply-named-layout ───────────────────────────────────────────────────────

(test apply-named-layout-even-horizontal-positions-panes
  "even-horizontal places n panes side by side with equal width."
  (let* ((p0  (make-no-pty-pane 1 0 0 1 1))
         (p1  (make-no-pty-pane 2 0 0 1 1))
         (win (make-window :id 1 :name "w" :width 81 :height 24
                           :panes (list p0 p1)
                           :tree (make-layout-leaf p0))))
    (apply-named-layout win :even-horizontal)
    ;; 81 cols - 1 separator = 80 usable, floor(80/2) = 40 each
    (is (=  0 (pane-x p0)) "p0 must start at column 0")
    (is (= 40 (pane-width p0)) "p0 width must be 40")
    (is (= 41 (pane-x p1)) "p1 must start at column 41")
    (is (= 40 (pane-width p1)) "p1 width must be 40")
    (is (= 24 (pane-height p0)) "height unchanged")
    (is (= 24 (pane-height p1)) "height unchanged")))

(test apply-named-layout-even-vertical-positions-panes
  "even-vertical places n panes stacked with equal height."
  (let* ((p0  (make-no-pty-pane 1 0 0 1 1))
         (p1  (make-no-pty-pane 2 0 0 1 1))
         (win (make-window :id 1 :name "w" :width 80 :height 25
                           :panes (list p0 p1)
                           :tree (make-layout-leaf p0))))
    (apply-named-layout win :even-vertical)
    ;; 25 rows - 1 separator = 24, floor(24/2) = 12 each
    (is (=  0 (pane-y p0)) "p0 must start at row 0")
    (is (= 12 (pane-height p0)) "p0 height must be 12")
    (is (= 13 (pane-y p1)) "p1 must start at row 13")
    (is (= 12 (pane-height p1)) "p1 height must be 12")))
