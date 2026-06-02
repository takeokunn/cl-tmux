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
;;;
;;; Uses make-two-pane-h-window from test/helpers.lisp — the local duplicate
;;; %two-pane-h-window was removed to eliminate the 81×24 two-pane fixture
;;; defined in two places with identical construction logic.

(test pane-neighbor-right-in-h-split
  "Right neighbor of the left pane is the right pane."
  (multiple-value-bind (win p0 p1) (make-two-pane-h-window)
    (is (eq p1 (cl-tmux/model::pane-neighbor win p0 :right)))))

(test pane-neighbor-left-in-h-split
  "Left neighbor of the right pane is the left pane."
  (multiple-value-bind (win p0 p1) (make-two-pane-h-window)
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

(test pane-neighbor-down-in-v-split
  "Down neighbor of the top pane in a v-split is the bottom pane."
  (with-v-split-window (win p0 p1)
    (is (eq p1 (cl-tmux/model::pane-neighbor win p0 :down))
        "down neighbor of top pane must be the bottom pane")))

(test pane-neighbor-up-in-v-split
  "Up neighbor of the bottom pane in a v-split is the top pane."
  (with-v-split-window (win p0 p1)
    (is (eq p0 (cl-tmux/model::pane-neighbor win p1 :up))
        "up neighbor of bottom pane must be the top pane")))

(test pane-neighbor-nil-outside-split-axis
  "A pane in an h-split has no up or down neighbor."
  (multiple-value-bind (win p0 p1) (make-two-pane-h-window)
    (declare (ignore p1))
    (is (null (cl-tmux/model::pane-neighbor win p0 :up))
        "left pane in h-split must have no up neighbor")
    (is (null (cl-tmux/model::pane-neighbor win p0 :down))
        "left pane in h-split must have no down neighbor")))

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

;;; ── window-zoom-toggle ────────────────────────────────────────────────────────

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
    ;; Zoom flag is set.
    (is-true  (cl-tmux/model:window-zoom-p win) "window-zoom-p must be T after zoom-in")
    ;; Pane list now contains only the zoomed pane.
    (is (equal (list p0) (window-panes win))
        "zoomed window must have only the active pane in its panes list")
    ;; Active pane fills the full window dimensions.
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
    ;; Capture geometry before zoom
    (let ((w0-before (pane-width p0))
          (w1-before (pane-width p1)))
      ;; Zoom in then zoom out.
      (cl-tmux/model:window-zoom-toggle win)
      (cl-tmux/model:window-zoom-toggle win)
      ;; Zoom flag cleared.
      (is-false (cl-tmux/model:window-zoom-p win) "window-zoom-p must be NIL after zoom-out")
      ;; Both panes restored in the list.
      (is (= 2 (length (window-panes win))) "both panes must be back after zoom-out")
      ;; Original widths restored.
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
    ;; Zoom in on a single pane — must not error.
    (cl-tmux/model:window-zoom-toggle win)
    (is-true  (cl-tmux/model:window-zoom-p win) "single-pane window must accept zoom-in")
    (is (= 1 (length (window-panes win))) "single-pane list unchanged after zoom-in")
    (is (= 80 (pane-width  p0)) "single pane must still fill full window width")
    (is (= 24 (pane-height p0)) "single pane must still fill full window height")
    ;; Zoom out — zoom flag cleared, pane count unchanged.
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

;;; ── apply-named-layout — remaining three named layouts ─────────────────────
;;;
;;; :main-horizontal, :main-vertical, and :tiled all contain substantively
;;; different geometry logic (floor-halving, secondary-pane subdivision,
;;; sqrt-based grid).  The tests below exercise each of these branches
;;; using make-no-pty-pane and make-fake-window from helpers.lisp.

(test apply-named-layout-main-horizontal-two-panes
  ":main-horizontal with 2 panes places the first across the top half and
   the second across the bottom half."
  (let* ((p0  (make-no-pty-pane 1 0 0 1 1))
         (p1  (make-no-pty-pane 2 0 0 1 1))
         (win (make-window :id 1 :name "w" :width 80 :height 24
                           :panes (list p0 p1)
                           :tree (make-layout-leaf p0))))
    (apply-named-layout win :main-horizontal)
    ;; main-h = floor(24/2) = 12; rest-h = 24 - 12 - 1 = 11
    (is (= 0  (pane-x p0))      "main pane starts at column 0")
    (is (= 0  (pane-y p0))      "main pane starts at row 0")
    (is (= 80 (pane-width p0))  "main pane spans full width")
    (is (= 12 (pane-height p0)) "main pane height = floor(h/2)")
    ;; Secondary pane fills the bottom portion.
    (is (= 0  (pane-x p1))      "secondary pane starts at column 0")
    (is (= 13 (pane-y p1))      "secondary pane y = main-h + 1 separator")
    (is (= 80 (pane-width p1))  "secondary pane spans full width (1 pane below)")
    (is (= 11 (pane-height p1)) "secondary pane height = h - main-h - 1")))

(test apply-named-layout-main-horizontal-three-panes
  ":main-horizontal with 3 panes: main spans top half; two panes share the
   bottom half side by side with equal widths."
  (let* ((p0  (make-no-pty-pane 1 0 0 1 1))
         (p1  (make-no-pty-pane 2 0 0 1 1))
         (p2  (make-no-pty-pane 3 0 0 1 1))
         (win (make-window :id 1 :name "w" :width 81 :height 25
                           :panes (list p0 p1 p2)
                           :tree (make-layout-leaf p0))))
    (apply-named-layout win :main-horizontal)
    ;; main-h = floor(25/2) = 12; rest-h = 25 - 12 - 1 = 12
    (is (= 12 (pane-height p0)) "main pane height = floor(h/2)")
    (is (= 12 (pane-height p1)) "secondary panes share the rest row equally")
    (is (= 12 (pane-height p2)) "secondary panes share the rest row equally")
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
  ":main-vertical with 2 panes places the first in the left half and the
   second in the right half (stacked — but as a single pane it spans all rows)."
  (let* ((p0  (make-no-pty-pane 1 0 0 1 1))
         (p1  (make-no-pty-pane 2 0 0 1 1))
         (win (make-window :id 1 :name "w" :width 80 :height 24
                           :panes (list p0 p1)
                           :tree (make-layout-leaf p0))))
    (apply-named-layout win :main-vertical)
    ;; main-w = floor(80/2) = 40; rest-w = 80 - 40 - 1 = 39
    (is (= 0  (pane-x p0))      "main pane starts at column 0")
    (is (= 0  (pane-y p0))      "main pane starts at row 0")
    (is (= 40 (pane-width p0))  "main pane width = floor(w/2)")
    (is (= 24 (pane-height p0)) "main pane spans full height")
    ;; Secondary pane fills the right column.
    (is (= 41 (pane-x p1))      "secondary pane x = main-w + 1 separator")
    (is (= 0  (pane-y p1))      "secondary pane starts at row 0")
    (is (= 39 (pane-width p1))  "secondary pane width = w - main-w - 1")
    (is (= 24 (pane-height p1)) "secondary pane spans full height (1 pane in column)")))

(test apply-named-layout-main-vertical-three-panes
  ":main-vertical with 3 panes: main spans the left half; two panes share
   the right half stacked equally."
  (let* ((p0  (make-no-pty-pane 1 0 0 1 1))
         (p1  (make-no-pty-pane 2 0 0 1 1))
         (p2  (make-no-pty-pane 3 0 0 1 1))
         (win (make-window :id 1 :name "w" :width 81 :height 25
                           :panes (list p0 p1 p2)
                           :tree (make-layout-leaf p0))))
    (apply-named-layout win :main-vertical)
    ;; main-w = floor(81/2) = 40; rest-w = 81 - 40 - 1 = 40
    (is (= 40 (pane-width p0))  "main pane width = floor(w/2)")
    (is (= 25 (pane-height p0)) "main pane spans full height")
    ;; Two secondary panes stacked: 25 rows - 1 separator = 24, floor(24/2) = 12 each
    (is (= 41 (pane-x p1))     "top secondary pane x = main-w + 1")
    (is (= 0  (pane-y p1))     "top secondary pane starts at row 0")
    (is (= 12 (pane-height p1)) "top secondary height = floor(avail/2)")
    (is (= 41 (pane-x p2))     "bottom secondary pane in the same column")
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
