(in-package #:cl-tmux/model)

;;; ── Layout persistence (layout string serialization) ──────────────────────────
;;;
;;; Encode/decode the layout tree in tmux's WxH,X,Y format.
;;; Full tmux format: checksum,WxH,X,Y[node1,node2]  or  checksum,WxH,X,Y,pane-id
;;;
;;; For cl-tmux we use a simplified but compatible subset:
;;;   Leaf:  "WxH,X,Y,pane-id"
;;;   H-split: "WxH,X,Y{first,second}"
;;;   V-split: "WxH,X,Y[first,second]"
;;; The 4-hex-digit checksum prefix is computed from the string.

;;; tmux rolling checksum multiplier — matches the algorithm in tmux's layout.c.
(defconstant +checksum-multiplier+ 61
  "Multiplier for the tmux rolling 16-bit layout checksum (from tmux layout.c).")

(defun %layout-checksum (str)
  "Compute the tmux-style 16-bit checksum of STR.
   Algorithm: rolling multiply-add on character codes (multiplier = +checksum-multiplier+).
   Returns a 4-hex-digit string."
  (let ((csum 0))
    (loop for ch across str
          do (setf csum (logand #xFFFF (+ (* csum +checksum-multiplier+) (char-code ch)))))
    (format nil "~4,'0X" csum)))

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

(define-layout-fold %node->string (node)
  :docstring "Serialize a layout node (leaf or split) to a layout string fragment.
   Does not include the checksum prefix."
  :on-null  ""
  :on-leaf  (format nil "~Dx~D,~D,~D,~D"
                    (pane-width leaf-pane) (pane-height leaf-pane)
                    (pane-x leaf-pane) (pane-y leaf-pane)
                    (pane-id leaf-pane))
  :on-split (let ((open-bracket  (if (eq split-orient :v) #\[ #\{))
                  (close-bracket (if (eq split-orient :v) #\] #\})))
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
    (unless tree (return-from layout->string nil))
    (let* ((body     (%node->string tree))
           (checksum (%layout-checksum body)))
      (format nil "~A,~A" checksum body))))

;;; ── String → layout decoder ──────────────────────────────────────────────────
;;;
;;; Parse a layout string (optionally with a leading checksum) back into
;;; a layout tree, matching existing panes by id from PANES-LIST.
;;;
;;; The encoded format produced by %node->string:
;;;   Leaf:    "WxH,X,Y,pane-id"
;;;   H-split: "WxH,X,Y{child1,child2}"
;;;   V-split: "WxH,X,Y[child1,child2]"

(defun %skip-checksum (str)
  "If STR starts with a 4-char hex checksum followed by a comma, skip it.
   Returns the remaining string."
  (if (and (>= (length str) 5)
           (char= (char str 4) #\,)
           (every (lambda (ch) (digit-char-p ch 16)) (subseq str 0 4)))
      (subseq str 5)
      str))

(defun %read-digits (str pos)
  "Read decimal digits from STR starting at POS.
   Returns (values integer end-pos) where end-pos is past the last digit."
  (let ((start pos))
    (loop while (and (< pos (length str))
                     (digit-char-p (char str pos)))
          do (incf pos))
    (values (parse-integer str :start start :end pos) pos)))

;;; %parse-node uses forward-reference to %parse-split-body.
;;; We declare it here so the compiler accepts the mutual recursion.
(declaim (ftype (function (string list fixnum) (values t fixnum)) %parse-node))

(defun %parse-split-body (str panes pos close-ch orient)
  "Parse two child nodes starting at POS, expecting CLOSE-CH (} or ]) after second.
   Returns (values split-node end-pos)."
  (multiple-value-bind (child1 child1-end)
      (%parse-node str panes pos)
    (let ((child2-start (if (and (< child1-end (length str))
                                 (char= (char str child1-end) #\,))
                            (1+ child1-end)
                            child1-end)))
      (multiple-value-bind (child2 child2-end)
          (%parse-node str panes child2-start)
        (let ((close-end (if (and (< child2-end (length str))
                                  (char= (char str child2-end) close-ch))
                             (1+ child2-end)
                             child2-end)))
          (values (make-layout-split orient child1 child2) close-end))))))

(defun %parse-node (str panes pos)
  "Parse one layout node starting at POS in STR.
   Returns (values node end-pos)."
  ;; Format: WxH,X,Y then one of: { (h-split), [ (v-split), , pane-id (leaf).
  ;; Scan past W digits, the 'x' separator, the H digits, then X and Y commas.
  (let* ((x-sep-pos    (or (position #\x str :start pos)  (length str)))
         (x-comma-pos  (or (position #\, str :start (1+ x-sep-pos)) (length str)))
         (y-comma-pos  (or (position #\, str :start (1+ x-comma-pos)) (length str)))
         ;; Y value ends at the first {, [, , or end of string
         (dispatch-pos (or (position-if (lambda (c) (or (char= c #\{) (char= c #\[) (char= c #\,)))
                                        str :start (1+ y-comma-pos))
                           (length str))))
    (if (>= dispatch-pos (length str))
        (values nil dispatch-pos)
        (let ((dispatch-char (char str dispatch-pos)))
          (cond
            ((char= dispatch-char #\{)
             (%parse-split-body str panes (1+ dispatch-pos) #\} :h))
            ((char= dispatch-char #\[)
             (%parse-split-body str panes (1+ dispatch-pos) #\] :v))
            ((char= dispatch-char #\,)
             (multiple-value-bind (pane-id pane-id-end)
                 (%read-digits str (1+ dispatch-pos))
               (let ((found-pane (find pane-id panes :key #'pane-id)))
                 (values (when found-pane (make-layout-leaf found-pane)) pane-id-end))))
            (t
             (warn "~A: unrecognized dispatch character ~S at position ~D; skipping."
                   '%parse-node dispatch-char dispatch-pos)
             (values nil dispatch-pos)))))))

(defun string->layout (layout-string panes)
  "Decode LAYOUT-STRING (tmux format, checksum optional) and rebuild the layout
   tree.  PANES is a list of existing pane objects matched by pane-id.
   Returns the root layout node, or NIL on parse failure."
  (handler-case
      (let ((str (%skip-checksum layout-string)))
        (multiple-value-bind (node _end)
            (%parse-node str panes 0)
          (declare (ignore _end))
          node))
    (error () nil)))
