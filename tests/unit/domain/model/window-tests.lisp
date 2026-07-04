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
      (dolist (pair (list (list p0 "pane 1") (list p1 "pane 2")))
        (destructuring-bind (p lbl) pair
          (is (= new-rows (pane-height p)) "~A height" lbl)
          (is (plusp (pane-width p)) "~A has positive width" lbl)))
      ;; Total coverage: two pane widths + 1 separator = new-cols.
      (is (= new-cols (+ (pane-width p0) 1 (pane-width p1)))
          "pane widths + separator must equal ~D" new-cols)
      ;; Screens match pane dimensions.
      (check-table (list (list (screen-width  (pane-screen p0)) (pane-width p0)  "pane 1 screen-width")
                         (list (screen-height (pane-screen p0)) (pane-height p0) "pane 1 screen-height")
                         (list (screen-width  (pane-screen p1)) (pane-width p1)  "pane 2 screen-width")
                         (list (screen-height (pane-screen p1)) (pane-height p1) "pane 2 screen-height"))
                   :test #'equal)
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
    (check-table (list (list (pane-x      pane)              0  "pane x")
                       (list (pane-y      pane)              0  "pane y")
                       (list (pane-width  pane)              90 "pane width")
                       (list (pane-height pane)              30 "pane height")
                       (list (screen-width  (pane-screen pane)) 90 "screen width")
                       (list (screen-height (pane-screen pane)) 30 "screen height"))
                 :test #'equal)))

(test window-relayout-no-tree-is-noop-for-panes
  "With a NIL tree, window-relayout updates the stored dimensions but does not
   reposition any panes (no tree to walk)."
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

(test window-relayout-current-uses-stored-dimensions
  "window-relayout-current relayouts WINDOW using its own stored width/height
   (equivalent to (window-relayout window (window-height w) (window-width w)))."
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

(test ensure-window-fits-relayouts-on-size-change
  "ensure-window-fits relayouts when the requested size differs from the
   window's stored size, fixing a pane left with mismatched geometry."
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
    (check-table (list (list (window-width  win)              80 "window width")
                       (list (window-height win)              24 "window height")
                       (list (pane-x      pane)              0  "pane x")
                       (list (pane-y      pane)              0  "pane y")
                       (list (pane-width  pane)              80 "pane width")
                       (list (pane-height pane)              24 "pane height")
                       (list (screen-width  (pane-screen pane)) 80 "screen width")
                       (list (screen-height (pane-screen pane)) 24 "screen height")))
    (is (eq screen (pane-screen pane))
        "pane screen object must be unchanged when size already matches")))

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

;;; ── %grow-first-p direct tests (pure, no PTY) ────────────────────────────────

(test grow-first-p-table
  "%grow-first-p returns T when the given SIDE should grow for DIRECTION.
   :first grows on :right/:down; :second grows on :left/:up.
   Each row: (side direction expected description)."
  (dolist (row '((:first  :right t   ":first grows on :right")
                 (:first  :down  t   ":first grows on :down")
                 (:first  :left  nil ":first does not grow on :left")
                 (:first  :up    nil ":first does not grow on :up")
                 (:second :left  t   ":second grows on :left")
                 (:second :up    t   ":second grows on :up")
                 (:second :right nil ":second does not grow on :right")
                 (:second :down  nil ":second does not grow on :down")))
    (destructuring-bind (side direction expected desc) row
      (if expected
          (is-true  (cl-tmux/model::%grow-first-p side direction) desc)
          (is-false (cl-tmux/model::%grow-first-p side direction) desc)))))

;;; ── split-child-geometry direct tests (pure, no PTY) ─────────────────────

(test split-child-geometry-table
  "split-child-geometry returns the correct child position and size for :h and :v.
   For :h the child is the right half; for :v it is the bottom half.
   Each row: (orient pane-w pane-h expected-x expected-y expected-w expected-h)."
  (dolist (row '((:h 41 20 21 0  20 20)
                 (:v 80 25 0  13 80 12)))
    (destructuring-bind (orient w h ex ey ew eh) row
      (let ((p (make-pane :id 1 :x 0 :y 0 :width w :height h :fd -1 :pid -1
                          :screen (make-screen w h))))
        (multiple-value-bind (px py pw ph)
            (cl-tmux/model::split-child-geometry p orient)
          (is (eql ex px) "~A split: child x" orient)
          (is (eql ey py) "~A split: child y" orient)
          (is (eql ew pw) "~A split: child width" orient)
          (is (eql eh ph) "~A split: child height" orient))))))

;;; ── %new-split-ratio
;;; ── %new-split-ratio direct tests (pure, no PTY) ─────────────────────────

(test new-split-ratio-table
  "%new-split-ratio: positive delta grows, clamped case → NIL, negative delta shrinks."
  (dolist (row '((:h 80 1/2  5 t  45/80 "grow: cur=40, +5 → 45/80")
                 (:h 10 1/2 10 t  nil   "blocked: new=15 > max=8 → NIL")
                 (:v 20 1/2  3 nil 7/20  "shrink: cur=10, -3 → 7/20")))
    (destructuring-bind (orient avail ratio delta grow-first expected desc) row
      (is (equal expected
                 (cl-tmux/model::%new-split-ratio orient avail ratio delta grow-first))
          "~A" desc))))

;;; ── %requested-cells-from-hint direct tests (pure, no PTY) ───────────────────

(test requested-cells-from-hint-table
  "%requested-cells-from-hint converts a size HINT to a cell count within AVAIL.
   Integer hints > 0 pass through unchanged; non-positive integers fall back to
   half of AVAIL.  Real hints in (0,1) scale AVAIL; reals outside that range
   also fall back to half of AVAIL.
   Each row: (hint avail orient expected description)."
  (dolist (row '((20   80 :h 20 "positive integer hint passes through unchanged")
                 (0    80 :h 40 "zero integer hint falls back to half of avail")
                 (-5   80 :h 40 "negative integer hint falls back to half of avail")
                 (0.25 80 :h 20 "real hint in (0,1) scales avail proportionally")
                 (0.3  80 :h 24 "real hint 0.3 scales and rounds to nearest cell")
                 (1.0  80 :h 40 "real hint >= 1.0 falls back to half of avail")
                 (0.0  80 :h 40 "real hint <= 0.0 falls back to half of avail")
                 (nil  80 :h 40 "non-numeric hint falls back to half of avail")))
    (destructuring-bind (hint avail orient expected desc) row
      (is (eql expected
               (cl-tmux/model::%requested-cells-from-hint hint avail orient))
          "~A" desc))))

;;; ── %ratio-from-size-hint direct tests (pure, no PTY) ─────────────────────────

(test ratio-from-size-hint-clamps-to-axis-floor
  "%ratio-from-size-hint clamps the requested cell count so both the new pane
   and its sibling keep at least the axis floor (+pane-min-width+ for :h)."
  ;; avail=10, :h axis-floor=2; requesting 1 cell must clamp up to 2/10.
  (is (= 1/5 (cl-tmux/model::%ratio-from-size-hint 1 10 :h))
      "requested cell count below the axis floor clamps up to axis-floor/avail")
  ;; avail=10, :h axis-floor=2; requesting 9 cells must clamp down to leave
  ;; axis-floor=2 for the first child, i.e. (10-2)/10 = 8/10.
  (is (= 4/5 (cl-tmux/model::%ratio-from-size-hint 9 10 :h))
      "requested cell count above (avail - axis-floor) clamps down"))

(test ratio-from-size-hint-mid-range-passes-through
  "%ratio-from-size-hint returns the exact ratio for a hint safely within bounds."
  (is (= 1/4 (cl-tmux/model::%ratio-from-size-hint 20 80 :h))
      "hint well within [axis-floor, avail - axis-floor] passes through unclamped"))

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

(test split-fits-p-table
  "%split-fits-p returns T when the pane axis meets the minimum, NIL otherwise.
   :h needs width >= 5 (2*2+1); :v needs height >= 3 (2*1+1).
   Each row: (orient width height expected description)."
  (dolist (row '((:h 5  3  t   "h exactly-minimum width of 5 → fits")
                 (:v 5  3  t   "v exactly-minimum height of 3 → fits")
                 (:h 4  5  nil "h width 4 < 5 → does not fit")
                 (:v 5  2  nil "v height 2 < 3 → does not fit")))
    (destructuring-bind (orient w h expected desc) row
      (let ((p (make-pane :id 1 :fd -1 :pid -1 :width w :height h
                          :screen (make-screen w h))))
        (if expected
            (is-true  (cl-tmux/model::%split-fits-p p orient) desc)
            (is-false (cl-tmux/model::%split-fits-p p orient) desc))))))

(test window-split-full-obeys-axis-minimums
  "window-split :full refuses root splits that cannot leave both panes at min size."
  (with-session (session 24 80)
    (dolist (row '((:h 4 24 "full h-split needs at least 5 columns")
                   (:v 80 2 "full v-split needs at least 3 rows")))
      (destructuring-bind (direction width height desc) row
        (let* ((p0   (make-no-pty-pane 1 0 0 width height))
               (leaf (make-layout-leaf p0))
               (win  (make-window :id 1 :name "w" :width width :height height
                                  :panes (list p0)
                                  :tree leaf)))
          (window-select-pane win p0)
          (is (null (window-split session win direction :full t)) "~A" desc)
          (is (eq leaf (window-tree win)) "~A: tree unchanged" desc)
          (is (equal (list p0) (window-panes win)) "~A: pane list unchanged" desc)
          (is (eq p0 (window-active-pane win)) "~A: active pane unchanged" desc))))))

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
