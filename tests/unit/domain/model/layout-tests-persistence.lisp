(in-package #:cl-tmux/test)

(in-suite layout-tree-suite)

;;; ── Layout persistence: layout->string ────────────────────────────────────
;;;
;;; These tests cover the checksum computation and the %build-flat-tree
;;; constructor.

(test layout-to-string-single-leaf
  "layout->string on a single-pane window returns a checksum,WxH,X,Y,pane-id string."
  (let* ((p    (tl-pane 7 20 10))
         (win  (tl-window (make-layout-leaf p) 10 20 :active p)))
    (let ((s (layout->string win)))
      (is (stringp s) "layout->string must return a string")
      ;; Format: 4-hex-checksum , WxH,X,Y,pane-id
      (is (>= (length s) 5) "string must be long enough to include checksum")
      (is (char= #\, (char s 4)) "checksum must be followed by a comma")
      (is (search "20x10" s) "string must contain widthxheight")
      (is (search "7" s) "string must contain the pane id"))))

(test layout-to-string-nil-tree-returns-nil
  "layout->string on a window with no tree returns NIL."
  (let ((win (make-window :id 1 :name "w" :width 80 :height 24 :tree nil)))
    (is (null (layout->string win))
        "layout->string on nil tree must return NIL")))

(test layout-to-string-split-notation
  "layout->string uses {..} for :h splits and [..] for :v splits."
  (dolist (c '((:h #\{ #\} "H-split uses braces")
               (:v #\[ #\] "V-split uses brackets")))
    (destructuring-bind (orient open close label) c
      (let* ((l0  (tl-leaf 1 1 1))
             (l1  (tl-leaf 2 1 1))
             (win (tl-window (make-layout-split orient l0 l1) 24 80))
             (s   (layout->string win)))
        (is (find open  (coerce s 'list)) "~A: must use ~C" label open)
        (is (find close (coerce s 'list)) "~A: must use ~C" label close)))))

(test layout-checksum-is-reproducible
  "%layout-checksum returns the same 4-char hex string for the same input."
  (let ((s "%layout-checksum determinism check"))
    (is (string= (cl-tmux/model::%layout-checksum s)
                 (cl-tmux/model::%layout-checksum s))
        "%layout-checksum must be deterministic")
    (is (= 4 (length (cl-tmux/model::%layout-checksum s)))
        "checksum must always be exactly 4 hex digits")))

(test layout-checksum-empty-string
  "%layout-checksum on the empty string returns a 4-digit hex string."
  (let ((cs (cl-tmux/model::%layout-checksum "")))
    (is (= 4 (length cs)) "empty string checksum must be 4 chars")
    (is (every (lambda (c) (digit-char-p c 16)) cs)
        "checksum must consist of hex digits")))

(test build-flat-tree-single-pane
  "%build-flat-tree with one pane returns a bare layout-leaf."
  (let* ((p    (tl-pane 1 10 5))
         (tree (cl-tmux/model::%build-flat-tree (list p) :h)))
    (is (cl-tmux/model::layout-leaf-p tree) "single pane must produce a layout-leaf")
    (is (eq p (layout-leaf-pane tree)) "leaf must hold the sole pane")))

(test build-flat-tree-two-panes
  "%build-flat-tree with two panes returns a layout-split."
  (let* ((p0   (tl-pane 1 10 5))
         (p1   (tl-pane 2 10 5))
         (tree (cl-tmux/model::%build-flat-tree (list p0 p1) :h)))
    (is (cl-tmux/model::layout-split-p tree) "two panes must produce a layout-split")
    (is (eq :h (cl-tmux/model::layout-split-orientation tree)) "orientation must match")
    (is (eq p0 (layout-leaf-pane (cl-tmux/model::layout-split-first tree)))
        "first child must hold p0")
    (is (cl-tmux/model::layout-leaf-p (cl-tmux/model::layout-split-second tree))
        "second child must be a leaf (for 2-pane flat tree)")))

(test build-flat-tree-three-panes-is-right-leaning
  "%build-flat-tree with three panes produces a right-leaning chain."
  (let* ((panes (loop for i from 1 to 3 collect (tl-pane i 10 5)))
         (tree  (cl-tmux/model::%build-flat-tree panes :v)))
    (is (cl-tmux/model::layout-split-p tree) "three panes must produce a split")
    (is (cl-tmux/model::layout-split-p (cl-tmux/model::layout-split-second tree))
        "right-leaning chain: second child is also a split")
    (is (cl-tmux/model::layout-leaf-p (cl-tmux/model::layout-split-first tree))
        "right-leaning chain: first child is a leaf")))

