(in-package #:cl-tmux/test)

;;;; layout tests — part D: %main-pane-extent boundary table, layout-split struct
;;;; defaults, checksum constants, zoomed-window pane-neighbor guard,
;;;; +neighbor-edge-tolerance+ constant, pane-neighbor up/down symmetry, and
;;;; layout-split type predicates.

(in-suite layout-tree-suite)

;;; ── %main-pane-extent — pure extent resolver ─────────────────────────────────
;;;
;;; Three code paths (Prolog-like rules):
;;;   rule1: main-size alone leaves no room → cap main to (available - 1)
;;;   rule2: other-size non-zero AND fits AND leaves room for main → other region wins
;;;   rule3: default → main-size as-is

(test main-pane-extent-normal-main-size
  "%main-pane-extent returns main-size when it fits and other-size is zero."
  ;; available=99, main-size=80, other-size=0 → 80
  (is (= 80 (cl-tmux/model::%main-pane-extent 99 80 0))
      "normal path: main-size must be returned as-is when other-size=0")
  ;; available=30, main-size=12, other-size=0 → 12
  (is (= 12 (cl-tmux/model::%main-pane-extent 30 12 0))
      "normal path: smaller main-size also returns as-is"))

(test main-pane-extent-main-size-too-large-clamped
  "%main-pane-extent caps main-size so at least 1 cell remains for others."
  ;; available=10, main-size=10 → (>= 10+1 10) → cap: max(1, 10-1) = 9
  (is (= 9 (cl-tmux/model::%main-pane-extent 10 10 0))
      "rule1: main-size that leaves no room must be capped to available-1")
  ;; available=5, main-size=100 → also capped to 4
  (is (= 4 (cl-tmux/model::%main-pane-extent 5 100 0))
      "rule1: extreme main-size must be capped to available-1"))

(test main-pane-extent-other-size-overrides-when-fitting
  "%main-pane-extent uses available-other-size when other-size fits and leaves room."
  ;; available=119, main-size=80, other-size=30 → (89 >= 80) → main = 119-30 = 89
  (is (= 89 (cl-tmux/model::%main-pane-extent 119 80 30))
      "rule2: fitting other-size → main gets the remaining cells")
  ;; available=49, main-size=24, other-size=20 → (29 >= 24) → main = 49-20 = 29
  (is (= 29 (cl-tmux/model::%main-pane-extent 49 24 20))
      "rule2: smaller available → correct main from other-size override"))

(test main-pane-extent-other-size-too-big-falls-back-to-main-size
  "%main-pane-extent ignores other-size when it does not leave room for main."
  ;; available=20, main-size=15, other-size=10 → 20-10=10 < 15 → falls back to 15
  (is (= 15 (cl-tmux/model::%main-pane-extent 20 15 10))
      "rule2 guard fails → main-size used as fallback")
  ;; available=100, main-size=80, other-size=200 → other-size > available → fails
  (is (= 80 (cl-tmux/model::%main-pane-extent 100 80 200))
      "other-size > available → treated as non-fitting → main-size returned"))

(test main-pane-extent-zero-other-size-ignored
  "%main-pane-extent treats other-size=0 as 'unset'; rule2 condition requires plusp."
  ;; other-size=0 is the 'unset' sentinel; rule2's (plusp 0) = NIL → skip to rule3
  (is (= 40 (cl-tmux/model::%main-pane-extent 80 40 0))
      "other-size=0 must not trigger the override rule"))

;;; ── layout-split struct defaults ─────────────────────────────────────────────

(test layout-split-default-ratio-is-one-half
  "make-layout-split with 2 args uses the default ratio of 1/2."
  (let* ((l0    (tl-leaf 1 1 1))
         (l1    (tl-leaf 2 1 1))
         (split (make-layout-split :h l0 l1)))
    (is (= 1/2 (cl-tmux/model::layout-split-ratio split))
        "default split ratio must be 1/2")))

(test layout-split-explicit-ratio-is-stored
  "make-layout-split with an explicit ratio stores it verbatim."
  (let* ((l0    (tl-leaf 1 1 1))
         (l1    (tl-leaf 2 1 1))
         (split (make-layout-split :h l0 l1 3/4)))
    (is (= 3/4 (cl-tmux/model::layout-split-ratio split))
        "explicit ratio must be stored exactly")))

(test layout-leaf-p-and-layout-split-p-predicates
  "layout-leaf-p and layout-split-p correctly identify node types."
  (let* ((leaf  (tl-leaf 1 1 1))
         (split (make-layout-split :h leaf (tl-leaf 2 1 1))))
    (is (cl-tmux/model::layout-leaf-p  leaf)  "leaf must satisfy layout-leaf-p")
    (is (not (cl-tmux/model::layout-split-p leaf)) "leaf must not satisfy layout-split-p")
    (is (cl-tmux/model::layout-split-p split)  "split must satisfy layout-split-p")
    (is (not (cl-tmux/model::layout-leaf-p  split)) "split must not satisfy layout-leaf-p")))

;;; ── Persistence: checksum constants ────────────────────────────────────────────

(test checksum-constants-values
  "Layout persistence constants have the correct tmux-compatible values."
  (is (= 61    cl-tmux/model::+checksum-multiplier+)
      "+checksum-multiplier+ must be 61 (from tmux layout.c)")
  (is (= #xFFFF cl-tmux/model::+checksum-mask+)
      "+checksum-mask+ must be #xFFFF (16-bit mask)"))

;;; ── pane-neighbor: zoomed window guard ──────────────────────────────────────────

(test pane-neighbor-returns-nil-in-zoomed-window
  "pane-neighbor returns NIL immediately when the window is zoomed."
  ;; Build a 2-pane window and toggle zoom; pane-neighbor must not find any neighbor.
  (with-h-split-window (win p0 p1)
    ;; Manually set the zoom flag (without a real PTY resize).
    (setf (cl-tmux/model::window-zoom-p win) t)
    (is (null (pane-neighbor win p0 :right))
        "right neighbor must be NIL in a zoomed window")
    (is (null (pane-neighbor win p1 :left))
        "left neighbor must be NIL in a zoomed window")
    ;; Cleanup: restore zoom flag so state does not leak.
    (setf (cl-tmux/model::window-zoom-p win) nil)))

;;; ── pane-neighbor: up/down symmetry ─────────────────────────────────────────

(test pane-neighbor-v-split-up-down-symmetry
  "pane-neighbor is symmetric: down neighbor of top is bottom, up neighbor of bottom is top."
  (with-v-split-window (win p0 p1)
    (is (eq p1 (pane-neighbor win p0 :down))
        "down neighbor of top pane must be bottom pane")
    (window-select-pane win p1)
    (is (eq p0 (pane-neighbor win p1 :up))
        "up neighbor of bottom pane must be top pane")))

;;; ── +neighbor-edge-tolerance+ constant ──────────────────────────────────────

(test neighbor-edge-tolerance-value
  "+neighbor-edge-tolerance+ must be 2 to account for the 1-cell separator."
  (is (= 2 cl-tmux/model::+neighbor-edge-tolerance+)
      "+neighbor-edge-tolerance+ must be exactly 2"))

;;; ── define-named-layout-rules macro coverage ─────────────────────────────────

(test define-named-layout-rules-generates-tiled-dispatch
  "apply-named-layout dispatches :tiled to %layout-tiled, placing all panes in a grid."
  ;; 4 panes: ceil(sqrt 4) = 2 cols, ceil(4/2) = 2 rows.
  (let* ((panes (loop for i from 1 to 4 collect (tl-pane i 1 1)))
         (win   (make-window :id 1 :name "w" :width 81 :height 25
                             :panes panes
                             :tree  (cl-tmux/model::%build-flat-tree panes :h))))
    (apply-named-layout win :tiled)
    ;; All panes must have positive geometry after tiled layout.
    (dolist (p (window-panes win))
      (is (> (pane-width  p) 0) "pane must have positive width after tiled")
      (is (> (pane-height p) 0) "pane must have positive height after tiled"))
    ;; The root must be a :v split (two rows stacked).
    (is (eq :v (cl-tmux/model::layout-split-orientation (window-tree win)))
        "tiled 4-pane root must be a :v split")))

;;; ── layout-split-axis-extent: nested tree ────────────────────────────────────

(test layout-split-axis-extent-nested-tree-h-outer
  "layout-split-axis-extent on an outer :h tree covers the full window width."
  ;; (left | (top / bot)): outer :h, inner :v.  Total :h extent = full window width.
  (let* ((left (tl-leaf 1 1 1))
         (top  (tl-leaf 2 1 1))
         (bot  (tl-leaf 3 1 1))
         (outer (make-layout-split :h left (make-layout-split :v top bot))))
    (cl-tmux/model::layout-assign outer 0 0 81 25)
    ;; :h extent of the outer split = full width = 81.
    (is (= 81 (cl-tmux/model::layout-split-axis-extent outer :h))
        "nested h-outer: :h extent must equal the full window width")
    ;; :v extent = full height = 25.
    (is (= 25 (cl-tmux/model::layout-split-axis-extent outer :v))
        "nested h-outer: :v extent must equal the full window height")))
