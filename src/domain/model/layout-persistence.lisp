(in-package #:cl-tmux/model)

;;; -- Layout persistence (layout string serialization) --------------------------
;;;
;;; Encode/decode the layout tree in tmux's WxH,X,Y format.
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
(defconstant +checksum-hex-digits+ 4
  "Number of hex digits in a tmux layout checksum (e.g. \"A1B2\").")
(defconstant +checksum-prefix-length+ 5
  "Total prefix length to skip when a checksum is present: 4 hex digits + 1 comma.")

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

;;; -- String -> layout decoder --------------------------------------------------
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
  (if (and (>= (length str) +checksum-prefix-length+)
           (char= (char str +checksum-hex-digits+) #\,)
           (every (lambda (ch) (digit-char-p ch 16))
                  (subseq str 0 +checksum-hex-digits+)))
      (subseq str +checksum-prefix-length+)
      str))

(defun %read-digits (str pos)
  "Read decimal digits from STR starting at POS (pure, no mutation).
   Returns (values integer end-pos) where end-pos is past the last digit."
  (labels ((scan (current-pos)
             (if (and (< current-pos (length str))
                      (digit-char-p (char str current-pos)))
                 (scan (1+ current-pos))
                 current-pos)))
    (let ((end (scan pos)))
      (values (parse-integer str :start pos :end end) end))))

;;; %parse-node uses forward-reference to %parse-split-body.
;;; We declare it here so the compiler accepts the mutual recursion.
(declaim (ftype (function (string list fixnum) (values t fixnum)) %parse-node))

(defun %advance-if-char (str pos str-length ch)
  "Return (1+ POS) when STR[POS] == CH and POS < STR-LENGTH; POS otherwise."
  (if (and (< pos str-length) (char= (char str pos) ch))
      (1+ pos)
      pos))

(defun %parse-split-body (str panes pos close-ch orient)
  "Parse two child nodes starting at POS, expecting CLOSE-CH (} or ]) after second.
   Returns (values split-node end-pos)."
  (let ((str-length (length str)))
    (multiple-value-bind (child1 child1-end) (%parse-node str panes pos)
      (let ((child2-start (%advance-if-char str child1-end str-length #\,)))
        (multiple-value-bind (child2 child2-end) (%parse-node str panes child2-start)
          (values (make-layout-split orient child1 child2)
                  (%advance-if-char str child2-end str-length close-ch)))))))

(defun %parse-geometry-prefix (str pos)
  "Scan past the WxH,X,Y prefix in STR starting at POS.
   Returns the index of the dispatch character ({, [, or , for a leaf)."
  (let* ((x-sep-pos   (or (position #\x str :start pos)            (length str)))
         (x-comma-pos (or (position #\, str :start (1+ x-sep-pos)) (length str)))
         (y-comma-pos (or (position #\, str :start (1+ x-comma-pos)) (length str))))
    (or (position-if (lambda (c) (or (char= c #\{) (char= c #\[) (char= c #\,)))
                     str :start (1+ y-comma-pos))
        (length str))))

(defun %parse-node (str panes pos)
  "Parse one layout node starting at POS in STR.
   Returns (values node end-pos)."
  ;; Format: WxH,X,Y then one of: { (h-split), [ (v-split), , pane-id (leaf).
  (let ((dispatch-pos (%parse-geometry-prefix str pos)))
    (if (>= dispatch-pos (length str))
        (values nil dispatch-pos)
        (let ((dispatch-char (char str dispatch-pos)))
          (cond
            ((char= dispatch-char #\{) (%parse-split-body str panes (1+ dispatch-pos) #\} :h))
            ((char= dispatch-char #\[) (%parse-split-body str panes (1+ dispatch-pos) #\] :v))
            ((char= dispatch-char #\,)
             (multiple-value-bind (pane-id pane-id-end)
                 (%read-digits str (1+ dispatch-pos))
               (let ((found-pane (find pane-id panes :key #'pane-id)))
                 (values (when found-pane (make-layout-leaf found-pane)) pane-id-end))))
            (t (values nil dispatch-pos)))))))  ; pure: no warn side-effect

(defun string->layout (layout-string panes)
  "Decode LAYOUT-STRING (tmux format, checksum optional) and rebuild the layout
   tree.  PANES is a list of existing pane objects matched by pane-id.
   Returns the root layout node, or NIL on parse failure.

   TODO: Hook this up as infrastructure for session restore (persisting and
   replaying a window's layout without re-running the PTY diff).
   No production src/ caller exists yet."
  (handler-case
      (let ((str (%skip-checksum layout-string)))
        (multiple-value-bind (node end)
            (%parse-node str panes 0)
          (declare (ignore end))
          node))
    (error () nil)))
