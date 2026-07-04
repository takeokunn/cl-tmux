(in-package #:cl-tmux/test)

;;;; Window-level tests: tree refresh and tree surgery helpers.

(in-suite model-suite)

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
