(in-package #:cl-tmux/test)

;;;; Pane and border rendering tests.
;;;;
;;;; Covers: render-pane, layout-subtree-rect, subtree-contains-p,
;;;;         render-tree-borders, %apply-border-style, draw-clock-to-screen
;;;;         from src/renderer-pane.lisp.
;;;;
;;;; renderer-suite is declared in renderer-format-tests.lisp (loaded first).

(in-suite renderer-suite)

;;; -- Local fixture ----------------------------------------------------------
;;;
;;; make-renderer-test-session (defined in test/helpers.lisp) is the canonical
;;; shared fixture.  The old local %make-pane-test-session has been removed in
;;; favour of the shared version.

;;; -- render-pane (content + positioning) ------------------------------------

(test render-pane-content-and-positioning
  (let* ((sess (make-renderer-test-session 5 2 :content "hi"))
         (pane (first (window-panes (session-active-window sess))))
         (out  (with-output-to-string (s)
                 (cl-tmux/renderer::render-pane s pane))))
    (is (find #\h out) "render-pane should emit the h glyph (got ~S)" out)
    (is (find #\i out) "render-pane should emit the i glyph (got ~S)" out)
    (is (search (format nil "~C[1;1H" #\Escape) out)
        "render-pane should position row 0 with ESC[1;1H (got ~S)" out)))

;;; -- double-width glyphs are not double-printed ------------------------------

(test render-pane-double-width-not-duplicated
  (let* ((screen (make-screen 5 2))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 5 :height 2 :fd -1 :screen screen)))
    (cl-tmux/test::utf8-feed screen "あ")
    (let ((out (with-output-to-string (s)
                 (cl-tmux/renderer::render-pane s pane))))
      (is (= 1 (count #\あ out))
          "exactly one wide glyph should be printed (got ~D in ~S)"
          (count #\あ out) out))))

;;; -- layout-subtree-rect and subtree-contains-p ------------------------------

(test layout-subtree-rect-bounding-box
  "layout-subtree-rect returns the tight bounding box of all leaves."
  (let* ((l0   (tl-leaf 1 1 1))
         (l1   (tl-leaf 2 1 1))
         (tree (make-layout-split :h l0 l1)))
    (cl-tmux/model::layout-assign tree 0 0 81 24)
    (let ((rect (cl-tmux/renderer::layout-subtree-rect tree)))
      (is (= 0  (getf rect :x)))
      (is (= 0  (getf rect :y)))
      (is (= 81 (getf rect :w)))
      (is (= 24 (getf rect :h))))))

(test subtree-contains-p-detects-membership
  "subtree-contains-p returns T for panes in the subtree and NIL otherwise."
  (let* ((l0 (tl-leaf 1 1 1))
         (l1 (tl-leaf 2 1 1))
         (tree (make-layout-split :h l0 l1))
         (p0  (layout-leaf-pane l0))
         (p1  (layout-leaf-pane l1))
         (p-other (make-pane :id 99 :fd -1 :pid -1 :screen (make-screen 1 1))))
    (is-true  (cl-tmux/renderer::subtree-contains-p tree p0))
    (is-true  (cl-tmux/renderer::subtree-contains-p tree p1))
    (is-false (cl-tmux/renderer::subtree-contains-p tree p-other))
    (is-false (cl-tmux/renderer::subtree-contains-p tree nil))))

;;; -- render-tree-borders -----------------------------------------------------

(test render-tree-borders-draws-vertical-bar
  "render-tree-borders draws vertical-bar separators for a :h split."
  (let* ((l0   (tl-leaf 1 1 1))
         (l1   (tl-leaf 2 1 1))
         (tree (make-layout-split :h l0 l1)))
    (cl-tmux/model::layout-assign tree 0 0 81 24)
    (let* ((ap  (layout-leaf-pane l0))
           (buf (make-string-output-stream)))
      (cl-tmux/renderer::render-tree-borders buf tree ap 81)
      (let ((out (get-output-stream-string buf)))
        (is (plusp (length out)) "render-tree-borders must produce output")
        (is (find #\│ out) "vertical bar character must be present")))))

;;; -- %apply-border-style branch coverage -------------------------------------
;;;
;;; %apply-border-style (stream style-string) has four reachable branches:
;;;   1. NIL style        -> reset-attrs (no colour code)
;;;   2. "default" style  -> reset-attrs (no colour code)
;;;   3. "fg=COLOR" style -> reset-attrs then ESC[Nm for the named colour
;;;   4. t (fallback)     -> reset-attrs (no colour code)

(defun %border-style-output (style)
  "Return the string emitted by %apply-border-style for STYLE."
  (with-output-to-string (s)
    (cl-tmux/renderer::%apply-border-style s style)))

(test apply-border-style-nil-resets
  "NIL style emits only reset attributes (ESC[0m)."
  (let ((out (%border-style-output nil)))
    (is (search (format nil "~C[0m" #\Escape) out)
        "nil style must emit ESC[0m (got ~S)" out)
    (is (= 1 (count #\m out))
        "nil style must emit exactly one m terminator (got ~S)" out)))

(test apply-border-style-default-resets
  "\"default\" style emits only reset attributes (ESC[0m)."
  (let ((out (%border-style-output "default")))
    (is (search (format nil "~C[0m" #\Escape) out)
        "\"default\" style must emit ESC[0m (got ~S)" out)))

(test apply-border-style-fg-green
  "\"fg=green\" style emits reset then ESC[32m (SGR 32 = green foreground)."
  (let ((out (%border-style-output "fg=green")))
    (is (search (format nil "~C[32m" #\Escape) out)
        "fg=green must emit ESC[32m (got ~S)" out)))

(test apply-border-style-fg-red
  "\"fg=red\" style emits reset then ESC[31m (SGR 31 = red foreground)."
  (let ((out (%border-style-output "fg=red")))
    (is (search (format nil "~C[31m" #\Escape) out)
        "fg=red must emit ESC[31m (got ~S)" out)))

(test apply-border-style-fg-blue
  "\"fg=blue\" style emits reset then ESC[34m (SGR 34 = blue foreground)."
  (let ((out (%border-style-output "fg=blue")))
    (is (search (format nil "~C[34m" #\Escape) out)
        "fg=blue must emit ESC[34m (got ~S)" out)))

(test apply-border-style-fg-yellow
  "\"fg=yellow\" style emits reset then ESC[33m (SGR 33 = yellow foreground)."
  (let ((out (%border-style-output "fg=yellow")))
    (is (search (format nil "~C[33m" #\Escape) out)
        "fg=yellow must emit ESC[33m (got ~S)" out)))

(test apply-border-style-unknown-falls-back-to-reset
  "An unrecognised non-fg= style falls through to the reset-attrs fallback."
  (let ((out (%border-style-output "bold")))
    (is (search (format nil "~C[0m" #\Escape) out)
        "unknown style token must fall back to ESC[0m (got ~S)" out)))

;;; -- draw-clock-to-screen branch ---------------------------------------------

(test draw-clock-to-screen-emits-digits
  "draw-clock-to-screen produces output containing block characters for a
   pane that is wide and tall enough."
  (let ((out (with-output-to-string (s)
               (cl-tmux/renderer::draw-clock-to-screen s 0 0 20 6))))
    (is (plusp (length out))
        "draw-clock-to-screen must produce non-empty output for 20x6 pane")
    (is (find #\█ out)
        "draw-clock-to-screen must emit block-element characters for digits (got ~S)" out)))

(test draw-clock-to-screen-too-small-emits-nothing
  "draw-clock-to-screen produces no output when the pane is too narrow (< 13 cols)."
  (let ((out (with-output-to-string (s)
               (cl-tmux/renderer::draw-clock-to-screen s 0 0 5 3))))
    (is (string= "" out)
        "draw-clock-to-screen must not render in a 5-wide pane (got ~S)" out)))

(test render-pane-clock-mode-overlay
  "When *clock-mode-pane-id* matches the pane id, render-pane draws the clock overlay."
  (let* ((screen (make-screen 20 6))
         (pane   (make-pane :id 42 :x 0 :y 0 :width 20 :height 6 :fd -1 :screen screen))
         (cl-tmux::*clock-mode-pane-id* 42))
    (let ((out (with-output-to-string (s)
                 (cl-tmux/renderer::render-pane s pane))))
      (is (find #\█ out)
          "render-pane in clock mode must emit block-element digits (got ~S)" out))))

(test render-pane-no-clock-when-id-mismatch
  "When *clock-mode-pane-id* does not match the pane id, the clock overlay is suppressed."
  (let* ((screen (make-screen 20 6))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 20 :height 6 :fd -1 :screen screen))
         (cl-tmux::*clock-mode-pane-id* 99))
    (let ((out (with-output-to-string (s)
                 (cl-tmux/renderer::render-pane s pane))))
      (is (null (find #\█ out))
          "render-pane without matching clock-mode id must not emit clock digits (got ~S)"
          out))))

;;; -- in-sel branch coverage via render-pane ----------------------------------

(defun %make-selecting-pane (w h content mark-row mark-col cursor-row cursor-col)
  "Return a pane whose screen is in copy-mode with an active selection."
  (let* ((screen (make-screen w h))
         (pane   (make-pane :id 1 :x 0 :y 0 :width w :height h
                            :fd -1 :screen screen)))
    (feed screen content)
    (setf (screen-copy-mode-p       screen) t
          (screen-copy-selecting    screen) t
          (screen-copy-offset       screen) 0
          (screen-copy-mark         screen) (cons mark-row   mark-col)
          (screen-copy-cursor       screen) (cons cursor-row cursor-col))
    pane))

(defun %render-pane-string (pane)
  "Return the string produced by render-pane for PANE."
  (with-output-to-string (s)
    (cl-tmux/renderer::render-pane s pane)))

(defun %reverse-video-p (out)
  "True when OUT contains the SGR reverse-video code (;7)."
  (not (null (search ";7" out))))

(test in-sel-branch-not-selecting
  "When copy-selecting is NIL the sel-active gate is false."
  (let* ((screen (make-screen 8 4))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 8 :height 4
                            :fd -1 :screen screen)))
    (feed screen "ABCDEFGH")
    (setf (screen-copy-mode-p    screen) t
          (screen-copy-selecting screen) nil
          (screen-copy-mark      screen) nil
          (screen-copy-cursor    screen) nil)
    (let ((out (%render-pane-string pane)))
      (is (null (%reverse-video-p out))
          "no reverse-video SGR should appear when copy-selecting is NIL (got ~S)"
          out))))

(test in-sel-branch-single-row
  "Single-row selection: only cells in [sel-start-c, sel-end-c) are highlighted."
  (let* ((pane (%make-selecting-pane 8 4
                                     "ABCDEFGHIJKLMNOPQRSTUVWXYZ01234567"
                                     0 2 0 5)))
    (let ((out (%render-pane-string pane)))
      (is (%reverse-video-p out)
          "single-row selection: reverse-video SGR must appear (got ~S)" out))))

(test in-sel-branch-first-row
  "First row of a multi-row selection: cols >= sel-start-c are highlighted."
  (let* ((pane (%make-selecting-pane 8 4
                                     "ABCDEFGHIJKLMNOPQRSTUVWXYZ01234567"
                                     0 3 2 0)))
    (let ((out (%render-pane-string pane)))
      (is (%reverse-video-p out)
          "first-row branch: reverse-video SGR must appear (got ~S)" out))))

(test in-sel-branch-last-row
  "Last row of a multi-row selection: cols < sel-end-c are highlighted."
  (let* ((pane (%make-selecting-pane 8 4
                                     "ABCDEFGHIJKLMNOPQRSTUVWXYZ01234567"
                                     0 0 2 5)))
    (let ((out (%render-pane-string pane)))
      (is (%reverse-video-p out)
          "last-row branch: reverse-video SGR must appear (got ~S)" out))))

(test in-sel-branch-middle-row
  "Middle rows of a multi-row selection are fully highlighted."
  (let* ((pane (%make-selecting-pane 8 4
                                     "ABCDEFGHIJKLMNOPQRSTUVWXYZ01234567"
                                     0 0 3 0)))
    (let ((out (%render-pane-string pane)))
      (is (%reverse-video-p out)
          "middle-row branch: reverse-video SGR must appear (got ~S)" out))))

(test in-sel-branch-selecting-but-no-mark
  "When copy-selecting is T but mark is NIL, sel-active is false."
  (let* ((screen (make-screen 8 4))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 8 :height 4
                            :fd -1 :screen screen)))
    (feed screen "ABCDEFGH")
    (setf (screen-copy-mode-p    screen) t
          (screen-copy-selecting screen) t
          (screen-copy-mark      screen) nil
          (screen-copy-cursor    screen) (cons 0 3))
    (let ((out (%render-pane-string pane)))
      (is (null (%reverse-video-p out))
          "nil mark must suppress reverse-video (got ~S)" out))))

;;; -- %clock-digit-rows -------------------------------------------------------

(test clock-digit-rows-zero
  "%clock-digit-rows returns 3 row strings for digit 0."
  (let ((rows (cl-tmux/renderer::%clock-digit-rows 0)))
    (is (= 3 (length rows))
        "%clock-digit-rows 0 must return 3 rows (got ~D)" (length rows))
    (is (every #'stringp rows)
        "%clock-digit-rows 0 must return strings")))

(test clock-digit-rows-nine
  "%clock-digit-rows returns non-empty strings for digit 9."
  (let ((rows (cl-tmux/renderer::%clock-digit-rows 9)))
    (is (= 3 (length rows))
        "%clock-digit-rows 9 must return 3 rows (got ~D)" (length rows))
    (is (every (lambda (r) (plusp (length r))) rows)
        "%clock-digit-rows 9 rows must be non-empty")))

(test clock-digit-rows-all-digits-present
  "*clock-digits* has entries for all 10 digits (0..9)."
  (is (= 10 (length cl-tmux/renderer::*clock-digits*))
      "*clock-digits* must contain exactly 10 entries (got ~D)"
      (length cl-tmux/renderer::*clock-digits*)))

;;; -- %render-v-separator branch coverage ------------------------------------

(test render-v-separator-draws-horizontal-bar
  "%render-v-separator draws ─ characters between top and bottom children."
  (let* ((l0   (tl-leaf 1 1 1))
         (l1   (tl-leaf 2 1 1))
         (tree (make-layout-split :v l0 l1)))
    (cl-tmux/model::layout-assign tree 0 0 80 21)
    (let ((buf (make-string-output-stream)))
      (cl-tmux/renderer::%render-v-separator buf tree 80)
      (let ((out (get-output-stream-string buf)))
        (is (plusp (length out)) "%render-v-separator must produce output")
        (is (find #\─ out)
            "horizontal separator must contain ─ character")))))

;;; -- render-tree-borders with :v split --------------------------------------

(test render-tree-borders-draws-horizontal-bar-for-v-split
  "render-tree-borders draws ─ separators for a :v split."
  (let* ((l0   (tl-leaf 1 1 1))
         (l1   (tl-leaf 2 1 1))
         (tree (make-layout-split :v l0 l1)))
    (cl-tmux/model::layout-assign tree 0 0 80 21)
    (let* ((ap  (layout-leaf-pane l0))
           (buf (make-string-output-stream)))
      (cl-tmux/renderer::render-tree-borders buf tree ap 80)
      (let ((out (get-output-stream-string buf)))
        (is (plusp (length out)) "render-tree-borders must produce output for :v split")
        (is (find #\─ out) "horizontal bar character must be present for :v split")))))

;;; -- layout-subtree-rect single-leaf edge case ------------------------------

(test layout-subtree-rect-single-leaf
  "layout-subtree-rect on a single leaf returns the leaf pane geometry."
  (let* ((pane (tl-pane 7 40 20))
         (leaf (make-layout-leaf pane)))
    (cl-tmux/model::layout-assign leaf 5 3 40 20)
    (let ((rect (cl-tmux/renderer::layout-subtree-rect leaf)))
      (is (= 5  (getf rect :x)) ":x must match pane-x (got ~D)" (getf rect :x))
      (is (= 3  (getf rect :y)) ":y must match pane-y (got ~D)" (getf rect :y))
      (is (= 40 (getf rect :w)) ":w must match pane-width (got ~D)" (getf rect :w))
      (is (= 20 (getf rect :h)) ":h must match pane-height (got ~D)" (getf rect :h)))))

;;; -- subtree-contains-p nil pane corner case --------------------------------

(test subtree-contains-p-leaf-node-with-matching-pane
  "subtree-contains-p returns T when the subtree is a leaf containing the pane."
  (let* ((p    (tl-pane 1 10 5))
         (leaf (make-layout-leaf p)))
    (is-true (cl-tmux/renderer::subtree-contains-p leaf p)
             "subtree-contains-p must return T for matching leaf pane")))

(test subtree-contains-p-leaf-node-with-nonmatching-pane
  "subtree-contains-p returns NIL when the subtree is a leaf for a different pane."
  (let* ((p1   (tl-pane 1 10 5))
         (p2   (tl-pane 2 10 5))
         (leaf (make-layout-leaf p1)))
    (is-false (cl-tmux/renderer::subtree-contains-p leaf p2)
              "subtree-contains-p must return NIL for non-member pane")))

;;; -- in-selection-p direct unit tests ----------------------------------------
;;;
;;; in-selection-p is the innermost hot path: test all 4 cond branches directly.

(defun %in-sel (row col sr er sc ec)
  "Call in-selection-p with positional args in a more readable order."
  (cl-tmux/renderer::in-selection-p row col sr er sc ec))

(test in-selection-p-single-row-inside-range
  "Single-row selection: cell within [sel-start-c, sel-end-c) is included."
  (is-true (%in-sel 2 3 2 2 1 5)
           "row=2 col=3 in single-row selection [1,5) must be T"))

(test in-selection-p-single-row-left-boundary
  "Single-row selection: cell at sel-start-c is included (inclusive lower bound)."
  (is-true (%in-sel 2 1 2 2 1 5)
           "col at sel-start-c must be included"))

(test in-selection-p-single-row-right-boundary-exclusive
  "Single-row selection: cell at sel-end-c is excluded (exclusive upper bound)."
  (is-false (%in-sel 2 5 2 2 1 5)
            "col at sel-end-c must be excluded"))

(test in-selection-p-single-row-outside-left
  "Single-row selection: cell before sel-start-c is excluded."
  (is-false (%in-sel 2 0 2 2 1 5)
            "col before sel-start-c must be excluded"))

(test in-selection-p-first-row-of-multirow
  "First row of multi-row selection: cols >= sel-start-c are included."
  (is-true  (%in-sel 0 3 0 2 2 4) "col >= sel-start-c on first row must be T")
  (is-false (%in-sel 0 1 0 2 2 4) "col < sel-start-c on first row must be F"))

(test in-selection-p-last-row-of-multirow
  "Last row of multi-row selection: cols < sel-end-c are included."
  (is-true  (%in-sel 2 3 0 2 2 4) "col < sel-end-c on last row must be T")
  (is-false (%in-sel 2 4 0 2 2 4) "col = sel-end-c on last row must be F (exclusive)"))

(test in-selection-p-middle-row-of-multirow
  "Middle rows of multi-row selection: all cells are included."
  (is-true (%in-sel 1 0 0 2 2 4) "col 0 in middle row must be T (full row)")
  (is-true (%in-sel 1 7 0 2 2 4) "col 7 in middle row must be T (full row)"))

(test in-selection-p-row-before-selection-excluded
  "Row before selection start is not included."
  (is-false (%in-sel 0 0 1 3 0 5)
            "row before sel-start-r must be excluded"))

(test in-selection-p-row-after-selection-excluded
  "Row after selection end is not included."
  (is-false (%in-sel 4 0 1 3 0 5)
            "row after sel-end-r must be excluded"))

;;; -- %compute-selection-bounds unit tests ------------------------------------

(test compute-selection-bounds-active-selection
  "%compute-selection-bounds returns sel-active=T when all prerequisites are present."
  (let ((screen (make-screen 10 5)))
    (setf (cl-tmux/terminal/types:screen-copy-selecting screen) t
          (cl-tmux/terminal/types:screen-copy-mark     screen) (cons 1 2)
          (cl-tmux/terminal/types:screen-copy-cursor   screen) (cons 3 4)
          (cl-tmux/terminal/types:screen-copy-offset   screen) 0)
    (multiple-value-bind (active sr er sc ec)
        (cl-tmux/renderer::%compute-selection-bounds screen)
      (is-true active "sel-active must be T when all prerequisites present")
      (is (= 1 sr) "start row must be min(mark-row, cursor-row)")
      (is (= 3 er) "end row must be max(mark-row, cursor-row)")
      (is (= 2 sc) "start col: mark-col when mark-row < cursor-row")
      (is (= 4 ec) "end col: cursor-col when mark-row < cursor-row"))))

(test compute-selection-bounds-no-selecting
  "%compute-selection-bounds returns sel-active=NIL when copy-selecting is NIL."
  (let ((screen (make-screen 10 5)))
    (setf (cl-tmux/terminal/types:screen-copy-selecting screen) nil
          (cl-tmux/terminal/types:screen-copy-mark     screen) (cons 0 0)
          (cl-tmux/terminal/types:screen-copy-cursor   screen) (cons 1 1))
    (multiple-value-bind (active sr er sc ec)
        (cl-tmux/renderer::%compute-selection-bounds screen)
      (declare (ignore sr er sc ec))
      (is-false active "sel-active must be NIL when copy-selecting is NIL"))))

(test compute-selection-bounds-nil-mark
  "%compute-selection-bounds returns sel-active=NIL when mark is NIL."
  (let ((screen (make-screen 10 5)))
    (setf (cl-tmux/terminal/types:screen-copy-selecting screen) t
          (cl-tmux/terminal/types:screen-copy-mark     screen) nil
          (cl-tmux/terminal/types:screen-copy-cursor   screen) (cons 1 1))
    (multiple-value-bind (active sr er sc ec)
        (cl-tmux/renderer::%compute-selection-bounds screen)
      (declare (ignore sr er sc ec))
      (is-false active "sel-active must be NIL when mark is NIL"))))

(test compute-selection-bounds-reversed-rows-normalised
  "%compute-selection-bounds normalises row order so start <= end."
  (let ((screen (make-screen 10 5)))
    ;; cursor above mark — rows should be swapped in the output
    (setf (cl-tmux/terminal/types:screen-copy-selecting screen) t
          (cl-tmux/terminal/types:screen-copy-mark     screen) (cons 3 5)
          (cl-tmux/terminal/types:screen-copy-cursor   screen) (cons 1 2)
          (cl-tmux/terminal/types:screen-copy-offset   screen) 0)
    (multiple-value-bind (active sr er sc ec)
        (cl-tmux/renderer::%compute-selection-bounds screen)
      (is-true active "sel-active must be T")
      (is (<= sr er) "start row (~D) must be <= end row (~D)" sr er)
      (is (= 1 sr) "start row must be min(mark-row=3, cursor-row=1)=1")
      (is (= 3 er) "end row must be max(mark-row=3, cursor-row=1)=3")
      ;; cursor-row < mark-row: start-col = cursor-col, end-col = mark-col
      (is (= 2 sc) "start col = cursor-col when cursor-row < mark-row")
      (is (= 5 ec) "end col = mark-col when cursor-row < mark-row"))))

(test compute-selection-bounds-same-row-cols-normalised
  "%compute-selection-bounds normalises col order for same-row selections."
  (let ((screen (make-screen 10 5)))
    (setf (cl-tmux/terminal/types:screen-copy-selecting screen) t
          (cl-tmux/terminal/types:screen-copy-mark     screen) (cons 2 7)
          (cl-tmux/terminal/types:screen-copy-cursor   screen) (cons 2 3)
          (cl-tmux/terminal/types:screen-copy-offset   screen) 0)
    (multiple-value-bind (active sr er sc ec)
        (cl-tmux/renderer::%compute-selection-bounds screen)
      (is-true active "sel-active must be T")
      (is (= 2 sr) "both rows are 2")
      (is (= 2 er) "both rows are 2")
      (is (= 3 sc) "start col = min(mark-col=7, cursor-col=3)=3")
      (is (= 7 ec) "end col = max(mark-col=7, cursor-col=3)=7"))))

(test compute-selection-bounds-copy-offset-applied
  "%compute-selection-bounds adds copy-offset to both row values."
  (let ((screen (make-screen 10 5)))
    (setf (cl-tmux/terminal/types:screen-copy-selecting screen) t
          (cl-tmux/terminal/types:screen-copy-mark     screen) (cons 0 0)
          (cl-tmux/terminal/types:screen-copy-cursor   screen) (cons 1 0)
          (cl-tmux/terminal/types:screen-copy-offset   screen) 5)
    (multiple-value-bind (active sr er sc ec)
        (cl-tmux/renderer::%compute-selection-bounds screen)
      (declare (ignore sc ec))
      (is-true active "sel-active must be T")
      (is (= 5 sr) "start row must be min(0,1) + offset(5) = 5")
      (is (= 6 er) "end row must be max(0,1) + offset(5) = 6"))))
