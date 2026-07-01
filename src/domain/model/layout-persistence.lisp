(in-package #:cl-tmux/model)

;;; -- Layout persistence (layout string serialization) --------------------------
;;;
;;; Encode the layout tree in tmux's WxH,X,Y format.
;;; Full tmux format: checksum,WxH,X,Y[node1,node2]  or  checksum,WxH,X,Y,pane-id
;;;
;;; For cl-tmux we use a simplified subset:
;;;   Leaf:  "WxH,X,Y,pane-id"
;;;   H-split: "WxH,X,Y{first,second}"
;;;   V-split: "WxH,X,Y[first,second]"
;;; The 4-hex-digit checksum prefix is computed from the string.

;;; tmux rolling checksum constants -- match the algorithm in tmux's layout.c.
(defconstant +checksum-multiplier+ 61
  "Multiplier for the tmux rolling 16-bit layout checksum (from tmux layout.c).")
(defconstant +checksum-mask+ #xFFFF
  "16-bit mask applied at each step of the tmux rolling layout checksum.")

(defun %layout-checksum (str)
  "Compute the tmux-style 16-bit checksum of STR.
   Algorithm: rolling multiply-add on character codes (multiplier = +checksum-multiplier+).
   Returns a 4-hex-digit string."
  (format nil "~4,'0X"
          (reduce (lambda (accumulator ch)
                    (logand +checksum-mask+
                            (+ (* accumulator +checksum-multiplier+) (char-code ch))))
                  str
                  :initial-value 0)))

(defun %split-bounding-box (node)
  "Derive (values min-x min-y width height) for a LAYOUT-SPLIT node from its leaves.
   The bounding box is re-derived from the already-laid-out pane coordinates."
  (let* ((leaves (layout-leaves node))
         (min-x  (reduce #'min leaves :key #'pane-x))
         (min-y  (reduce #'min leaves :key #'pane-y))
         (max-rx (reduce #'max leaves :key (lambda (p) (+ (pane-x p) (pane-width p)))))
         (max-ry (reduce #'max leaves :key (lambda (p) (+ (pane-y p) (pane-height p))))))
    (values min-x min-y (- max-rx min-x) (- max-ry min-y))))

;;; %node->string uses define-layout-fold to dispatch over tree node types,
;;; eliminating the manual etypecase branch.  In the split branch, the bounding
;;; box is derived from already-laid-out leaf coordinates via %split-bounding-box.
;;;
;;; orient-case dispatches on :h/:v (defined in layout.lisp).

(define-layout-fold %node->string (node)
  :docstring "Serialize a layout node (leaf or split) to a layout string fragment.
   Does not include the checksum prefix."
  :on-null  ""
  :on-leaf  (format nil "~Dx~D,~D,~D,~D"
                    (pane-width leaf-pane) (pane-height leaf-pane)
                    (pane-x leaf-pane) (pane-y leaf-pane)
                    (pane-id leaf-pane))
  :on-split (let ((open-bracket  (orient-case split-orient :h #\{ :v #\[))
                  (close-bracket (orient-case split-orient :h #\} :v #\])))
               (multiple-value-bind (min-x min-y width height) (%split-bounding-box node)
                 (format nil "~Dx~D,~D,~D~C~A,~A~C"
                         width height min-x min-y
                         open-bracket
                         (%node->string split-first)
                         (%node->string split-second)
                         close-bracket))))

(defun layout->string (window)
  "Serialize WINDOW's layout tree to a tmux-format layout string with checksum.
   Returns NIL when the window has no tree."
  (let ((tree (window-tree window)))
    (when tree
      (let* ((body     (%node->string tree))
             (checksum (%layout-checksum body)))
        (format nil "~A,~A" checksum body)))))
