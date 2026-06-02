(in-package #:cl-tmux/model)

;;;; Named layout application — reposition all panes in a window.
;;;;
;;;; Lives here (not layout.lisp) because apply-named-layout accesses WINDOW
;;;; struct slots.  %build-flat-tree (pure tree construction) lives in layout.lisp
;;;; and is called from here.  Loads after window.lisp in the system definition.

;;; ── Named layouts ───────────────────────────────────────────────────────────
;;;
;;; Five standard tmux named layouts expressed as a Prolog-like fact table.
;;; Each ecase clause is one layout rule:
;;;
;;;   layout(even_horizontal, Window) :- n_equal_columns(Window).
;;;   layout(even_vertical,   Window) :- n_equal_rows(Window).
;;;   layout(main_horizontal, Window) :- main_top_rest_bottom_equal(Window).
;;;   layout(main_vertical,   Window) :- main_left_rest_right_stacked(Window).
;;;   layout(tiled,           Window) :- near_square_grid(Window).
;;;
;;; After repositioning all panes, layout-assign syncs every rectangle
;;; through the rebuilt split tree.

(defun %layout-even-h (window panes n w h)
  "Apply the :even-horizontal layout: N equal-width columns separated by 1-col borders."
  (let* ((avail-w (- w (1- n)))
         (each-w  (floor avail-w n)))
    (loop for pane in panes for i from 0
          do (pane-reposition pane (* i (1+ each-w)) 0 each-w h))
    (setf (window-tree window) (%build-flat-tree panes :h))
    (layout-assign (window-tree window) 0 0 w h)))

(defun %layout-even-v (window panes n w h)
  "Apply the :even-vertical layout: N equal-height rows separated by 1-row borders."
  (let* ((avail-h (- h (1- n)))
         (each-h  (floor avail-h n)))
    (loop for pane in panes for i from 0
          do (pane-reposition pane 0 (* i (1+ each-h)) w each-h))
    (setf (window-tree window) (%build-flat-tree panes :v))
    (layout-assign (window-tree window) 0 0 w h)))

(defun %layout-main-h (window panes w h)
  "Apply the :main-horizontal layout: first pane top half, rest split bottom half equally."
  (let* ((main-h  (floor h 2))
         (rest-h  (- h main-h 1))
         (rest-ps (rest panes))
         (m       (length rest-ps)))
    (pane-reposition (first panes) 0 0 w main-h)
    (when (plusp m)
      (let* ((avail-w (- w (1- m)))
             (each-w  (floor avail-w m)))
        (loop for pane in rest-ps for i from 0
              do (pane-reposition pane (* i (1+ each-w)) (1+ main-h) each-w rest-h))))
    (setf (window-tree window)
          (if rest-ps
              (make-layout-split :v (make-layout-leaf (first panes))
                                    (%build-flat-tree rest-ps :h))
              (make-layout-leaf (first panes))))
    (layout-assign (window-tree window) 0 0 w h)))

(defun %layout-main-v (window panes w h)
  "Apply the :main-vertical layout: first pane left half, rest split right half equally stacked."
  (let* ((main-w  (floor w 2))
         (rest-ps (rest panes))
         (m       (length rest-ps)))
    (pane-reposition (first panes) 0 0 main-w h)
    (when (plusp m)
      (let* ((avail-h (- h (1- m)))
             (each-h  (floor avail-h m)))
        (loop for pane in rest-ps for i from 0
              do (pane-reposition pane (1+ main-w) (* i (1+ each-h)) (- w main-w 1) each-h))))
    (setf (window-tree window)
          (if rest-ps
              (make-layout-split :h (make-layout-leaf (first panes))
                                    (%build-flat-tree rest-ps :v))
              (make-layout-leaf (first panes))))
    (layout-assign (window-tree window) 0 0 w h)))

(defun %layout-tiled (window panes n w h)
  "Apply the :tiled layout: near-square grid, cols = ceil(sqrt n), rows = ceil(n / cols)."
  (let* ((cols  (ceiling (sqrt n)))
         (rows  (ceiling n cols))
         (col-w (floor (- w (1- cols)) cols))
         (row-h (floor (- h (1- rows)) rows)))
    (loop for pane in panes for i from 0
          for col = (mod i cols) for row = (floor i cols)
          do (pane-reposition pane (* col (1+ col-w)) (* row (1+ row-h)) col-w row-h))
    ;; Build a tree that encodes the grid: vertical chain of horizontal rows.
    ;; This ensures layout-assign reproduces the same geometry when called later.
    (let* ((row-pane-groups
            (loop for row-idx from 0 below rows
                  collect (let ((start (* row-idx cols))
                                (end   (min (* (1+ row-idx) cols) n)))
                            (subseq panes start end))))
           (grid-tree
            (labels ((build-rows (groups)
                       (if (null (rest groups))
                           (%build-flat-tree (first groups) :h)
                           (make-layout-split :v
                             (%build-flat-tree (first groups) :h)
                             (build-rows (rest groups))))))
              (build-rows row-pane-groups))))
      (setf (window-tree window) grid-tree)
      (layout-assign (window-tree window) 0 0 w h))))

(defun apply-named-layout (window layout-name)
  "Reposition all panes in WINDOW according to LAYOUT-NAME, one of:
     :even-horizontal  :even-vertical  :main-horizontal
     :main-vertical    :tiled
   Rebuilds the window tree and calls layout-assign to sync all rectangles."
  (let* ((panes (window-panes window))
         (n     (length panes))
         (w     (window-width  window))
         (h     (window-height window)))
    (when (zerop n) (return-from apply-named-layout nil))
    (ecase layout-name
      (:even-horizontal (%layout-even-h window panes n w h))
      (:even-vertical   (%layout-even-v window panes n w h))
      (:main-horizontal (%layout-main-h window panes w h))
      (:main-vertical   (%layout-main-v window panes w h))
      (:tiled           (%layout-tiled  window panes n w h)))))
