(in-package #:cl-tmux/test)

;;;; layout tests — part D: %main-pane-extent boundary table, layout-split struct
;;;; defaults, checksum constants, zoomed-window pane-neighbor guard,
;;;; +neighbor-edge-tolerance+ constant, pane-neighbor up/down symmetry, and
;;;; layout-split type predicates.

(describe "layout-tree-suite"

  ;;; ── %main-pane-extent — pure extent resolver ─────────────────────────────────
  ;;;
  ;;; Three code paths (Prolog-like rules):
  ;;;   rule1: main-size alone leaves no room → cap main to (available - 1)
  ;;;   rule2: other-size non-zero AND fits AND leaves room for main → other region wins
  ;;;   rule3: default → main-size as-is

  ;; %main-pane-extent returns main-size when it fits and other-size is zero.
  (it "main-pane-extent-normal-main-size"
    ;; available=99, main-size=80, other-size=0 → 80
    (expect (= 80 (cl-tmux/model::%main-pane-extent 99 80 0)))
    ;; available=30, main-size=12, other-size=0 → 12
    (expect (= 12 (cl-tmux/model::%main-pane-extent 30 12 0))))

  ;; %main-pane-extent caps main-size so at least 1 cell remains for others.
  (it "main-pane-extent-main-size-too-large-clamped"
    ;; available=10, main-size=10 → (>= 10+1 10) → cap: max(1, 10-1) = 9
    (expect (= 9 (cl-tmux/model::%main-pane-extent 10 10 0)))
    ;; available=5, main-size=100 → also capped to 4
    (expect (= 4 (cl-tmux/model::%main-pane-extent 5 100 0))))

  ;; %main-pane-extent uses available-other-size when other-size fits and leaves room.
  (it "main-pane-extent-other-size-overrides-when-fitting"
    ;; available=119, main-size=80, other-size=30 → (89 >= 80) → main = 119-30 = 89
    (expect (= 89 (cl-tmux/model::%main-pane-extent 119 80 30)))
    ;; available=49, main-size=24, other-size=20 → (29 >= 24) → main = 49-20 = 29
    (expect (= 29 (cl-tmux/model::%main-pane-extent 49 24 20))))

  ;; %main-pane-extent ignores other-size when it does not leave room for main.
  (it "main-pane-extent-other-size-too-big-falls-back-to-main-size"
    ;; available=20, main-size=15, other-size=10 → 20-10=10 < 15 → falls back to 15
    (expect (= 15 (cl-tmux/model::%main-pane-extent 20 15 10)))
    ;; available=100, main-size=80, other-size=200 → other-size > available → fails
    (expect (= 80 (cl-tmux/model::%main-pane-extent 100 80 200))))

  ;; %main-pane-extent treats other-size=0 as 'unset'; rule2 condition requires plusp.
  (it "main-pane-extent-zero-other-size-ignored"
    ;; other-size=0 is the 'unset' sentinel; rule2's (plusp 0) = NIL → skip to rule3
    (expect (= 40 (cl-tmux/model::%main-pane-extent 80 40 0))))

  ;;; ── layout-split struct defaults ─────────────────────────────────────────────

  ;; make-layout-split with 2 args uses the default ratio of 1/2.
  (it "layout-split-default-ratio-is-one-half"
    (let* ((l0    (tl-leaf 1 1 1))
           (l1    (tl-leaf 2 1 1))
           (split (make-layout-split :h l0 l1)))
      (expect (= 1/2 (cl-tmux/model::layout-split-ratio split)))))

  ;; make-layout-split with an explicit ratio stores it verbatim.
  (it "layout-split-explicit-ratio-is-stored"
    (let* ((l0    (tl-leaf 1 1 1))
           (l1    (tl-leaf 2 1 1))
           (split (make-layout-split :h l0 l1 3/4)))
      (expect (= 3/4 (cl-tmux/model::layout-split-ratio split)))))

  ;; layout-leaf-p and layout-split-p correctly identify node types.
  (it "layout-leaf-p-and-layout-split-p-predicates"
    (let* ((leaf  (tl-leaf 1 1 1))
           (split (make-layout-split :h leaf (tl-leaf 2 1 1))))
      (expect (cl-tmux/model::layout-leaf-p  leaf))
      (expect (not (cl-tmux/model::layout-split-p leaf)))
      (expect (cl-tmux/model::layout-split-p split))
      (expect (not (cl-tmux/model::layout-leaf-p  split)))))

  ;;; ── Persistence: checksum constants ────────────────────────────────────────────

  ;; Layout persistence constants have the canonical checksum values.
  (it "checksum-constants-values"
    (expect (= 61    cl-tmux/model::+checksum-multiplier+))
    (expect (= #xFFFF cl-tmux/model::+checksum-mask+)))

  ;;; ── pane-neighbor: zoomed window guard ──────────────────────────────────────────

  ;; pane-neighbor returns NIL immediately when the window is zoomed.
  (it "pane-neighbor-returns-nil-in-zoomed-window"
    ;; Build a 2-pane window and toggle zoom; pane-neighbor must not find any neighbor.
    (with-h-split-window (win p0 p1)
      ;; Manually set the zoom flag (without a real PTY resize).
      (setf (cl-tmux/model::window-zoom-p win) t)
      (expect (null (pane-neighbor win p0 :right)))
      (expect (null (pane-neighbor win p1 :left)))
      ;; Cleanup: restore zoom flag so state does not leak.
      (setf (cl-tmux/model::window-zoom-p win) nil)))

  ;;; ── pane-neighbor: up/down symmetry ─────────────────────────────────────────

  ;; pane-neighbor is symmetric: down neighbor of top is bottom, up neighbor of bottom is top.
  (it "pane-neighbor-v-split-up-down-symmetry"
    (with-v-split-window (win p0 p1)
      (expect (eq p1 (pane-neighbor win p0 :down)))
      (window-select-pane win p1)
      (expect (eq p0 (pane-neighbor win p1 :up)))))

  ;;; ── +neighbor-edge-tolerance+ constant ──────────────────────────────────────

  ;; +neighbor-edge-tolerance+ must be 2 to account for the 1-cell separator.
  (it "neighbor-edge-tolerance-value"
    (expect (= 2 cl-tmux/model::+neighbor-edge-tolerance+)))

  ;;; ── define-named-layout-rules macro coverage ─────────────────────────────────

  ;; apply-named-layout dispatches :tiled to %layout-tiled, placing all panes in a grid.
  (it "define-named-layout-rules-generates-tiled-dispatch"
    ;; 4 panes: ceil(sqrt 4) = 2 cols, ceil(4/2) = 2 rows.
    (let* ((panes (loop for i from 1 to 4 collect (tl-pane i 1 1)))
           (win   (make-window :id 1 :name "w" :width 81 :height 25
                               :panes panes
                               :tree  (cl-tmux/model::%build-flat-tree panes :h))))
      (apply-named-layout win :tiled)
      ;; All panes must have positive geometry after tiled layout.
      (dolist (p (window-panes win))
        (expect (> (pane-width  p) 0))
        (expect (> (pane-height p) 0)))
      ;; The root must be a :v split (two rows stacked).
      (expect (eq :v (cl-tmux/model::layout-split-orientation (window-tree win))))))

  ;;; ── layout-split-axis-extent: nested tree ────────────────────────────────────

  ;; layout-split-axis-extent on an outer :h tree covers the full window width.
  (it "layout-split-axis-extent-nested-tree-h-outer"
    ;; (left | (top / bot)): outer :h, inner :v.  Total :h extent = full window width.
    (let* ((left (tl-leaf 1 1 1))
           (top  (tl-leaf 2 1 1))
           (bot  (tl-leaf 3 1 1))
           (outer (make-layout-split :h left (make-layout-split :v top bot))))
      (cl-tmux/model::layout-assign outer 0 0 81 25)
      ;; :h extent of the outer split = full width = 81.
      (expect (= 81 (cl-tmux/model::layout-split-axis-extent outer :h)))
      ;; :v extent = full height = 25.
      (expect (= 25 (cl-tmux/model::layout-split-axis-extent outer :v))))))
