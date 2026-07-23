(in-package #:cl-tmux/test)

(describe "layout-tree-suite"

  ;;; ── Layout persistence: layout->string ────────────────────────────────────
  ;;;
  ;;; These tests cover the checksum computation and the %build-flat-tree
  ;;; constructor.

  ;; layout->string on a single-pane window returns a checksum,WxH,X,Y,pane-id string.
  (it "layout-to-string-single-leaf"
    (let* ((p    (tl-pane 7 20 10))
           (win  (tl-window (make-layout-leaf p) 10 20 :active p)))
      (let ((s (layout->string win)))
        (expect (stringp s))
        ;; Format: 4-hex-checksum , WxH,X,Y,pane-id
        (expect (>= (length s) 5))
        (expect (char= #\, (char s 4)))
        (expect (search "20x10" s))
        (expect (search "7" s)))))

  ;; layout->string on a window with no tree returns NIL.
  (it "layout-to-string-nil-tree-returns-nil"
    (let ((win (make-window :id 1 :name "w" :width 80 :height 24 :tree nil)))
      (expect (null (layout->string win)))))

  ;; layout->string uses {..} for :h splits and [..] for :v splits.
  (it "layout-to-string-split-notation"
    (dolist (c '((:h #\{ #\} "H-split uses braces")
                 (:v #\[ #\] "V-split uses brackets")))
      (destructuring-bind (orient open close label) c
        (declare (ignore label))
        (let* ((l0  (tl-leaf 1 1 1))
               (l1  (tl-leaf 2 1 1))
               (win (tl-window (make-layout-split orient l0 l1) 24 80))
               (s   (layout->string win)))
          (expect (find open  (coerce s 'list)))
          (expect (find close (coerce s 'list)))))))

  ;; %layout-checksum returns the same 4-char hex string for the same input.
  (it "layout-checksum-is-reproducible"
    (let ((s "%layout-checksum determinism check"))
      (expect (string= (cl-tmux/model::%layout-checksum s)
                       (cl-tmux/model::%layout-checksum s)))
      (expect (= 4 (length (cl-tmux/model::%layout-checksum s))))))

  ;; %layout-checksum on the empty string returns a 4-digit hex string.
  (it "layout-checksum-empty-string"
    (let ((cs (cl-tmux/model::%layout-checksum "")))
      (expect (= 4 (length cs)))
      (expect (every (lambda (c) (digit-char-p c 16)) cs))))

  ;; %build-flat-tree with one pane returns a bare layout-leaf.
  (it "build-flat-tree-single-pane"
    (let* ((p    (tl-pane 1 10 5))
           (tree (cl-tmux/model::%build-flat-tree (list p) :h)))
      (expect (cl-tmux/model::layout-leaf-p tree))
      (expect (eq p (layout-leaf-pane tree)))))

  ;; %build-flat-tree with two panes returns a layout-split.
  (it "build-flat-tree-two-panes"
    (let* ((p0   (tl-pane 1 10 5))
           (p1   (tl-pane 2 10 5))
           (tree (cl-tmux/model::%build-flat-tree (list p0 p1) :h)))
      (expect (cl-tmux/model::layout-split-p tree))
      (expect (eq :h (cl-tmux/model::layout-split-orientation tree)))
      (expect (eq p0 (layout-leaf-pane (cl-tmux/model::layout-split-first tree))))
      (expect (cl-tmux/model::layout-leaf-p (cl-tmux/model::layout-split-second tree)))))

  ;; %build-flat-tree with three panes produces a right-leaning chain.
  (it "build-flat-tree-three-panes-is-right-leaning"
    (let* ((panes (loop for i from 1 to 3 collect (tl-pane i 10 5)))
           (tree  (cl-tmux/model::%build-flat-tree panes :v)))
      (expect (cl-tmux/model::layout-split-p tree))
      (expect (cl-tmux/model::layout-split-p (cl-tmux/model::layout-split-second tree)))
      (expect (cl-tmux/model::layout-leaf-p (cl-tmux/model::layout-split-first tree))))))
