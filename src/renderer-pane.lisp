(in-package #:cl-tmux/renderer)

;;;; Pane and border rendering.
;;;;
;;;; Depends on the ANSI escape-code primitives from renderer-format.lisp
;;;; (loaded first in the same package) and the layout/model structures from
;;;; cl-tmux/model.

;;; ── Pane ────────────────────────────────────────────────────────────────────

(defun render-pane (stream pane)
  "Draw the pane's screen into the real terminal at the pane's (x, y) offset."
  (let* ((screen (pane-screen pane))
         (pw     (pane-width   pane))
         (ph     (pane-height  pane))
         (ox     (pane-x      pane))
         (oy     (pane-y      pane)))
    (with-lock-held ((screen-lock screen))
      ;; Hoist selection boundary computation outside the cell loop so it is
      ;; computed once per frame instead of once per cell (~1920 times).
      (let* ((sel-active (and (screen-copy-selecting screen)
                              (consp (screen-copy-mark   screen))
                              (consp (screen-copy-cursor screen))))
             (sel-start-r 0) (sel-end-r 0) (sel-start-c 0) (sel-end-c 0))
        (when sel-active
          (let* ((mark   (screen-copy-mark   screen))
                 (cursor (screen-copy-cursor screen))
                 (mr (car mark))   (mc (cdr mark))
                 (cr (car cursor)) (cc (cdr cursor))
                 ;; mark/cursor are live-grid rows (0..height-1).
                 ;; Viewport row = live-grid row + copy-offset, so add the offset
                 ;; here so that the in-sel check below uses viewport coordinates,
                 ;; matching the row variable in the render loop.
                 (offset (screen-copy-offset screen)))
            (setf sel-start-r (+ (min mr cr) offset)
                  sel-end-r   (+ (max mr cr) offset)
                  sel-start-c (if (< mr cr) mc (if (> mr cr) cc (min mc cc)))
                  sel-end-c   (if (< mr cr) cc (if (> mr cr) mc (max mc cc))))))
        (let ((prev-fg -1) (prev-bg -1) (prev-attrs -1))
          (loop for row below ph do
            (move-to stream (+ oy row) ox)
            (loop for col below pw
                  for cell  = (screen-display-cell screen col row)
                  ;; A continuation cell (width 0) is the right half of a
                  ;; double-width glyph the terminal already drew — emit nothing.
                  unless (zerop (cell-width cell))
                    do (let* ((fg    (cell-fg    cell))
                              (bg    (cell-bg    cell))
                              (in-sel (and sel-active
                                           (cond
                                             ((= sel-start-r sel-end-r row)
                                              (and (<= sel-start-c col) (< col sel-end-c)))
                                             ((= row sel-start-r) (>= col sel-start-c))
                                             ((= row sel-end-r)   (< col sel-end-c))
                                             (t (and (> row sel-start-r)
                                                     (< row sel-end-r))))))
                              (attrs (if in-sel
                                         (logxor (cell-attrs cell) cl-tmux/terminal/types:+attr-reverse+)
                                         (cell-attrs cell))))
                         (unless (and (= fg prev-fg) (= bg prev-bg) (= attrs prev-attrs))
                           (render-cell-attrs stream fg bg attrs)
                           (setf prev-fg fg prev-bg bg prev-attrs attrs))
                         (write-char (cell-char cell) stream))))))
      (screen-clear-dirty screen))))

;;; ── Split-tree separators ───────────────────────────────────────────────────

(defun layout-subtree-rect (node)
  "Bounding rectangle of NODE's leaves as a plist (:x :y :w :h), derived from the
   already-laid-out pane geometry."
  (let ((panes (layout-leaves node)))
    (let ((min-x (reduce #'min panes :key #'pane-x))
          (min-y (reduce #'min panes :key #'pane-y))
          (max-x (reduce #'max panes :key (lambda (p) (+ (pane-x p) (pane-width p)))))
          (max-y (reduce #'max panes :key (lambda (p) (+ (pane-y p) (pane-height p))))))
      (list :x min-x :y min-y :w (- max-x min-x) :h (- max-y min-y)))))

(defun subtree-contains-p (node pane)
  "True when PANE is a leaf of NODE's subtree."
  (and pane (member pane (layout-leaves node))))

;;; ── Separator renderers (data layer — what each orientation draws) ──────────

(defun %render-h-separator (stream node active-pane terminal-cols)
  "Draw the │ column between the left and right children of an :h split.
   Highlights green when either neighbouring pane is ACTIVE-PANE."
  (let* ((a          (layout-split-first  node))
         (b          (layout-split-second node))
         (rect       (layout-subtree-rect a))
         (border-col (+ (getf rect :x) (getf rect :w)))
         (activep    (or (subtree-contains-p a active-pane)
                         (subtree-contains-p b active-pane))))
    (when (< border-col terminal-cols)
      (if activep
          (format stream "~C[32m" +esc+)
          (reset-attrs stream))
      (loop for row from (getf rect :y) below (+ (getf rect :y) (getf rect :h))
            do (move-to stream row border-col)
               (write-char #\│ stream))
      (reset-attrs stream))))

(defun %render-v-separator (stream node terminal-cols)
  "Draw the ─ row between the top and bottom children of a :v split."
  (let* ((rect       (layout-subtree-rect (layout-split-first node)))
         (border-row (+ (getf rect :y) (getf rect :h)))
         (x          (getf rect :x))
         (w          (min (getf rect :w) (- terminal-cols x))))
    (reset-attrs stream)
    (move-to stream border-row x)
    (loop repeat (max 0 w) do (write-char #\─ stream))))

;;; ── Tree border walk (logic layer) ──────────────────────────────────────────

(defun render-tree-borders (stream node active-pane terminal-cols)
  "Walk the split-tree NODE, drawing one separator per internal split node.
   :h (left|right) splits draw │ bars; :v (top/bottom) splits draw ─ bars.
   Recurses into both children after drawing the parent separator."
  (when (layout-split-p node)
    (ecase (layout-split-orientation node)
      (:h (%render-h-separator stream node active-pane terminal-cols))
      (:v (%render-v-separator stream node terminal-cols)))
    (render-tree-borders stream (layout-split-first  node) active-pane terminal-cols)
    (render-tree-borders stream (layout-split-second node) active-pane terminal-cols)))
