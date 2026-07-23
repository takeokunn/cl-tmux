(in-package #:cl-tmux/test)

;;;; renderer-pane tests — part B: %clock-digit-rows, %render-v-separator,
;;;; render-tree-borders with :v split, layout-subtree-rect single-leaf,
;;;; subtree-contains-p nil-pane corner case, additional in-sel/pane/border coverage.

(defun %in-sel (row col sr er sc ec &optional rect-p)
  "Call in-selection-p with positional args in a more readable order."
  (cl-tmux/renderer::in-selection-p row col sr er sc ec rect-p))

(defun %border-status-output (pane session win status-val fmt-val)
  "Run %render-pane-border-status with STATUS-VAL and FMT-VAL options and return output."
  (with-isolated-options ("pane-border-status" status-val
                          "pane-border-format"  fmt-val)
    (with-output-to-string (s)
      (cl-tmux/renderer::%render-pane-border-status s pane session win))))

(describe "renderer-suite"

  ;;; -- %clock-digit-rows -------------------------------------------------------

  ;; %clock-digit-rows returns 3 non-empty strings for representative digits.
  (it "clock-digit-rows-table"
    (dolist (digit '(0 9))
      (let ((rows (cl-tmux/renderer::%clock-digit-rows digit)))
        (expect (= 3 (length rows)))
        (expect (every (lambda (r) (and (stringp r) (plusp (length r)))) rows)))))

  ;; *clock-digits* has entries for all 10 digits (0..9).
  (it "clock-digit-rows-all-digits-present"
    (expect (= 10 (length cl-tmux/renderer::*clock-digits*))))

  ;;; -- %render-v-separator branch coverage ------------------------------------

  ;; %render-v-separator draws ─ characters between top and bottom children.
  (it "render-v-separator-draws-horizontal-bar"
    (let* ((l0   (tl-leaf 1 1 1))
           (l1   (tl-leaf 2 1 1))
           (tree (make-layout-split :v l0 l1)))
      (cl-tmux/model::layout-assign tree 0 0 80 21)
      (let ((buf (make-string-output-stream)))
        (cl-tmux/renderer::%render-v-separator buf tree nil 80)
        (let ((out (get-output-stream-string buf)))
          (expect (plusp (length out)))
          (expect (find #\─ out))))))

  ;;; -- render-tree-borders with :v split --------------------------------------

  ;; render-tree-borders draws ─ separators for a :v split.
  (it "render-tree-borders-draws-horizontal-bar-for-v-split"
    (let* ((l0   (tl-leaf 1 1 1))
           (l1   (tl-leaf 2 1 1))
           (tree (make-layout-split :v l0 l1)))
      (cl-tmux/model::layout-assign tree 0 0 80 21)
      (let ((out (render-tree-borders-output tree (layout-leaf-pane l0) 80)))
        (expect (plusp (length out)))
        (expect (find #\─ out)))))

  ;;; -- layout-subtree-rect single-leaf edge case ------------------------------

  ;; layout-subtree-rect on a single leaf returns the leaf pane geometry.
  (it "layout-subtree-rect-single-leaf"
    (let* ((pane (tl-pane 7 40 20))
           (leaf (make-layout-leaf pane)))
      (cl-tmux/model::layout-assign leaf 5 3 40 20)
      (let ((rect (cl-tmux/renderer::layout-subtree-rect leaf)))
        (check-table (list (list (getf rect :x) 5 ":x must match pane-x")
                           (list (getf rect :y) 3 ":y must match pane-y")
                           (list (getf rect :w) 40 ":w must match pane-width")
                           (list (getf rect :h) 20 ":h must match pane-height"))))))

  ;;; -- subtree-contains-p nil pane corner case --------------------------------

  ;; subtree-contains-p returns T when the subtree is a leaf containing the pane.
  (it "subtree-contains-p-leaf-node-with-matching-pane"
    (let* ((p    (tl-pane 1 10 5))
           (leaf (make-layout-leaf p)))
      (expect (cl-tmux/renderer::subtree-contains-p leaf p) :to-be-truthy)))

  ;; subtree-contains-p returns NIL when the subtree is a leaf for a different pane.
  (it "subtree-contains-p-leaf-node-with-nonmatching-pane"
    (let* ((p1   (tl-pane 1 10 5))
           (p2   (tl-pane 2 10 5))
           (leaf (make-layout-leaf p1)))
      (expect (cl-tmux/renderer::subtree-contains-p leaf p2) :to-be-falsy)))

  ;;; -- in-selection-p direct unit tests ----------------------------------------
  ;;;
  ;;; in-selection-p is the innermost hot path: test all 4 cond branches directly.

  ;; in-selection-p covers all four cond branches (single-row, first/last/mid row, rect mode).
  ;; Each row is (expected row col sr er sc ec rect-p description).
  (it "in-selection-p-table"
    (dolist (c '(;; single-row selection (sr = er = 2, sc=1, ec=5)
                 (t   2 3 2 2 1 5 nil "single-row inside [1,5)")
                 (t   2 1 2 2 1 5 nil "single-row at left boundary (inclusive)")
                 (nil 2 5 2 2 1 5 nil "single-row at right boundary (exclusive)")
                 (nil 2 0 2 2 1 5 nil "single-row before sc")
                 ;; multi-row: sr=0, er=2, sc=2, ec=4
                 (t   0 3 0 2 2 4 nil "first row, col >= sc")
                 (nil 0 1 0 2 2 4 nil "first row, col < sc")
                 (t   2 3 0 2 2 4 nil "last row, col < ec")
                 (nil 2 4 0 2 2 4 nil "last row, col = ec (exclusive)")
                 (t   1 0 0 2 2 4 nil "middle row, col 0 (full row)")
                 (t   1 7 0 2 2 4 nil "middle row, col 7 (full row)")
                 (nil 0 0 1 3 0 5 nil "row before sr")
                 (nil 4 0 1 3 0 5 nil "row after er")
                 ;; rectangle mode: sr=1, er=4, sc=2, ec=6
                 (t   2 3 1 4 2 6 t   "rect inside box")
                 (t   2 2 1 4 2 6 t   "rect col at sc (inclusive)")
                 (nil 2 6 1 4 2 6 t   "rect col at ec (exclusive)")
                 (t   1 3 1 4 2 6 t   "rect start row included")
                 (t   4 3 1 4 2 6 t   "rect end row included")
                 (nil 2 1 1 4 2 6 t   "rect col before sc")
                 (nil 2 7 1 4 2 6 t   "rect col after ec")
                 (nil 0 3 1 4 2 6 t   "rect row before sr")
                 (nil 5 3 1 4 2 6 t   "rect row after er")
                 (t   2 4 1 4 2 6 t   "rect middle row, col in range")
                 (nil 2 0 1 4 2 6 t   "rect middle row, col out of range")))
      (destructuring-bind (expected row col sr er sc ec rect-p desc) c
        (declare (ignore desc))
        (if expected
            (expect (%in-sel row col sr er sc ec rect-p) :to-be-truthy)
            (expect (%in-sel row col sr er sc ec rect-p) :to-be-falsy)))))

  ;;; -- %compute-selection-bounds unit tests ------------------------------------

  ;; %compute-selection-bounds returns sel-active=T when all prerequisites are present.
  (it "compute-selection-bounds-active-selection"
    (let ((screen (make-selecting-screen 10 5 1 2 3 4)))
      (multiple-value-bind (active sr er sc ec rect-p mark-row mark-col)
          (cl-tmux/renderer::%compute-selection-bounds screen)
        (declare (ignore mark-row mark-col))
        (expect active :to-be-truthy)
        (expect rect-p :to-be-falsy)
        (check-table (list (list sr 1 "start row must be min(mark-row, cursor-row)")
                           (list er 3 "end row must be max(mark-row, cursor-row)")
                           (list sc 2 "start col: mark-col when mark-row < cursor-row")
                           (list ec 4 "end col: cursor-col when mark-row < cursor-row"))))))

  ;; %compute-selection-bounds returns sel-active=NIL when copy-selecting is NIL.
  (it "compute-selection-bounds-no-selecting"
    (let ((screen (make-screen 10 5)))
      (setf (cl-tmux/terminal/types:screen-copy-selecting screen) nil
            (cl-tmux/terminal/types:screen-copy-mark      screen) (cons 0 0)
            (cl-tmux/terminal/types:screen-copy-cursor    screen) (cons 1 1))
      (multiple-value-bind (active sr er sc ec rect-p mark-row mark-col)
          (cl-tmux/renderer::%compute-selection-bounds screen)
        (declare (ignore sr er sc ec rect-p mark-row mark-col))
        (expect active :to-be-falsy))))

  ;; %compute-selection-bounds returns sel-active=NIL when mark is NIL.
  (it "compute-selection-bounds-nil-mark"
    (let ((screen (make-screen 10 5)))
      (setf (cl-tmux/terminal/types:screen-copy-selecting screen) t
            (cl-tmux/terminal/types:screen-copy-mark      screen) nil
            (cl-tmux/terminal/types:screen-copy-cursor    screen) (cons 1 1))
      (multiple-value-bind (active sr er sc ec rect-p mark-row mark-col)
          (cl-tmux/renderer::%compute-selection-bounds screen)
        (declare (ignore sr er sc ec rect-p mark-row mark-col))
        (expect active :to-be-falsy))))

  ;; %compute-selection-bounds normalises row order so start <= end.
  (it "compute-selection-bounds-reversed-rows-normalised"
    ;; cursor above mark — rows should be swapped in the output
    (let ((screen (make-selecting-screen 10 5 3 5 1 2)))
      (multiple-value-bind (active sr er sc ec rect-p mark-row mark-col)
          (cl-tmux/renderer::%compute-selection-bounds screen)
        (declare (ignore rect-p mark-row mark-col))
        (expect active :to-be-truthy)
        (expect (<= sr er))
        ;; cursor-row < mark-row: start-col = cursor-col, end-col = mark-col
        (check-table (list (list sr 1 "start row must be min(mark-row=3, cursor-row=1)=1")
                           (list er 3 "end row must be max(mark-row=3, cursor-row=1)=3")
                           (list sc 2 "start col = cursor-col when cursor-row < mark-row")
                           (list ec 5 "end col = mark-col when cursor-row < mark-row"))))))

  ;; %compute-selection-bounds normalises col order for same-row selections.
  (it "compute-selection-bounds-same-row-cols-normalised"
    (let ((screen (make-selecting-screen 10 5 2 7 2 3)))
      (multiple-value-bind (active sr er sc ec rect-p mark-row mark-col)
          (cl-tmux/renderer::%compute-selection-bounds screen)
        (declare (ignore rect-p mark-row mark-col))
        (expect active :to-be-truthy)
        (check-table (list (list sr 2 "both rows are 2: start")
                           (list er 2 "both rows are 2: end")
                           (list sc 3 "start col = min(mark-col=7, cursor-col=3)=3")
                           (list ec 7 "end col = max(mark-col=7, cursor-col=3)=7"))))))

  ;; %compute-selection-bounds maps virtual rows to viewport rows using the CURRENT offset.
  ;; The returned mark row is the clamped viewport row even when the virtual mark row
  ;; falls outside the visible pane.
  (it "compute-selection-bounds-copy-offset-applied"
    ;; No scrollback.  mark=(4,0) was set when offset=0 (mark-offset=0 by default).
    ;; Current offset=2, cursor=(2,0).
    (let ((screen (make-selecting-screen 10 5 4 0 2 0 :offset 2)))
      (multiple-value-bind (active sr er sc ec rect-p mark-row mark-col)
          (cl-tmux/renderer::%compute-selection-bounds screen)
        (declare (ignore sc ec rect-p mark-col))
        (expect active :to-be-truthy)
        (expect (= 2 sr))
        (expect (= 4 er))
        (expect (= 4 mark-row)))))

  ;; %compute-selection-bounds uses min/max column symmetrically in rectangle mode.
  (it "compute-selection-bounds-rect-columns-symmetric"
    ;; mark at (row=1, col=6), cursor at (row=4, col=2): rect cols [2,7) inclusive
    (let ((screen (make-selecting-screen 10 6 1 6 4 2 :rect t)))
      (multiple-value-bind (active sr er sc ec rect-p mark-row mark-col)
          (cl-tmux/renderer::%compute-selection-bounds screen)
        (declare (ignore mark-row mark-col))
        (expect active :to-be-truthy)
        (expect rect-p :to-be-truthy)
        (check-table (list (list sr 1 "start row = min(1,4) = 1")
                           (list er 4 "end row = max(1,4) = 4")
                           (list sc 2 "start col = min(mark-col=6, cursor-col=2) = 2")
                           (list ec 7 "end col = 1+max(6,2) = 7 (exclusive)"))
                     :test #'equal))))

  ;; %compute-selection-bounds swaps columns correctly when cursor-col > mark-col in rect mode.
  (it "compute-selection-bounds-rect-columns-reversed"
    ;; mark at (row=2, col=3), cursor at (row=5, col=8)
    (let ((screen (make-selecting-screen 10 8 2 3 5 8 :rect t)))
      (multiple-value-bind (active sr er sc ec rect-p mark-row mark-col)
          (cl-tmux/renderer::%compute-selection-bounds screen)
        (declare (ignore sr er mark-row mark-col))
        (expect active :to-be-truthy)
        (expect rect-p :to-be-truthy)
        (expect (= 3 sc))
        (expect (= 9 ec)))))

  ;;; -- make-test-pane and make-selecting-screen fixture helpers -------------------

  ;; make-test-pane returns a pane with the requested width, height, id, and origin.
  (it "make-test-pane-creates-correct-geometry"
    (let ((pane (make-test-pane 20 5 :id 7 :x 3 :y 2)))
      (check-table (list (list (pane-width  pane) 20 "pane width must be 20")
                         (list (pane-height pane)  5 "pane height must be 5")
                         (list (pane-id     pane)  7 "pane id must be 7")
                         (list (pane-x      pane)  3 "pane x must be 3")
                         (list (pane-y      pane)  2 "pane y must be 2"))
                   :test #'equal)
      (expect (screen-p (pane-screen pane)))))

  ;; make-test-pane feeds :content into the pane screen.
  (it "make-test-pane-feeds-content"
    (let* ((pane   (make-test-pane 10 5 :content "AB"))
           (screen (pane-screen pane)))
      (expect (char= #\A (cell-char (screen-cell screen 0 0))))
      (expect (char= #\B (cell-char (screen-cell screen 1 0))))))

  ;; make-selecting-screen returns a screen with copy-selecting T and the given mark/cursor.
  (it "make-selecting-screen-sets-selection-state"
    (let ((screen (make-selecting-screen 10 5 1 2 3 4)))
      (expect (cl-tmux/terminal/types:screen-copy-selecting screen) :to-be-truthy)
      (expect (equal (cons 1 2) (cl-tmux/terminal/types:screen-copy-mark screen)))
      (expect (equal (cons 3 4) (cl-tmux/terminal/types:screen-copy-cursor screen)))
      (expect (= 0 (cl-tmux/terminal/types:screen-copy-offset screen)))))

  ;; make-selecting-screen respects the :offset keyword.
  (it "make-selecting-screen-custom-offset"
    (let ((screen (make-selecting-screen 10 5 0 0 1 0 :offset 7)))
      (expect (= 7 (cl-tmux/terminal/types:screen-copy-offset screen)))))

  ;;; -- %render-pane-border-status coverage ------------------------------------
  ;;;
  ;;; %render-pane-border-status (~line 250-271 in renderer-pane.lisp) is only
  ;;; reachable when pane-border-status is not "off".  These tests exercise the
  ;;; top/bottom row placement branches and the format expansion path.

  ;; %render-pane-border-status does nothing when pane-border-status is "off".
  (it "render-pane-border-status-off-produces-nothing"
    (let* ((pane (make-test-pane 20 5 :id 1))
           (sess (make-fake-session :nwindows 1))
           (win  (first (cl-tmux/model:session-windows sess)))
           (out  (%border-status-output pane sess win "off" " #{pane_index} ")))
      (expect (string= "" out))))

  ;; %render-pane-border-status with status=top places the label on the RESERVED row
  ;; just above the content (pane-y - 1), so it never overwrites pane content.
  (it "render-pane-border-status-top-positions-above-content"
    (let* ((pane (make-test-pane 20 5 :id 1 :y 3))
           (sess (make-fake-session :nwindows 1))
           (win  (first (cl-tmux/model:session-windows sess)))
           (out  (%border-status-output pane sess win "top" "TITLE")))
      ;; Reserved row = pane-y - 1 = 2 → ESC[3;1H (1-based: 2+1=3)
      (expect (search (format nil "~C[3;" #\Escape) out))
      (expect (search "TITLE" out))))

  ;; %render-pane-border-status with status=bottom places the label on the RESERVED
  ;; row just below the content (pane-y + pane-height).
  (it "render-pane-border-status-bottom-positions-below-content"
    (let* ((pane (make-test-pane 20 5 :id 1 :y 0))
           (sess (make-fake-session :nwindows 1))
           (win  (first (cl-tmux/model:session-windows sess)))
           (out  (%border-status-output pane sess win "bottom" "BOT")))
      ;; Reserved row = pane-y + pane-height = 0 + 5 = 5 → ESC[6;1H
      (expect (search (format nil "~C[6;" #\Escape) out))
      (expect (search "BOT" out))))

  ;; %render-pane-border-status truncates the label to pane-width characters.
  (it "render-pane-border-status-truncates-to-pane-width"
    (let* ((pane (make-test-pane 5 3 :id 1))
           (sess (make-fake-session :nwindows 1))
           (win  (first (cl-tmux/model:session-windows sess)))
           (out  (%border-status-output pane sess win "top" "ABCDEFGHIJ")))
      ;; Only the first 5 visible chars should appear (pane-width=5).
      ;; The status text "ABCDEFGHIJ" should be truncated to "ABCDE".
      (expect (search "ABCDE" out))
      (expect (null (search "ABCDEF" out)))))

  ;;; -- copy-mode search-match highlighting -------------------------------------

  ;; %all-match-ranges returns every match span; regex with literal fallback.
  (it "all-match-ranges-literal-and-regex"
    (expect (equal '((0 . 3) (8 . 11))
               (cl-tmux/renderer::%all-match-ranges "abc" "abc def abc")))
    (expect (equal '((4 . 7))
               (cl-tmux/renderer::%all-match-ranges "[0-9]+" "abc 123 xyz")))
    (expect (equal '((2 . 3))
               (cl-tmux/renderer::%all-match-ranges "(" "a ( b"))))

  ;; When copy mode has a search term, render-session-to-string overdraws matches in
  ;; copy-mode-match-style.
  (it "copy-mode-search-matches-highlighted-in-frame"
    (with-fake-session (s)
      (feed (active-screen s) "hello world hello")
      (cl-tmux/commands::copy-mode-enter (active-screen s))
      (setf (cl-tmux/terminal/types:screen-copy-search-term (active-screen s)) "hello")
      (let* ((expected (cl-tmux/renderer:style-to-sgr
                        (cl-tmux/renderer:parse-style-string "bg=green")))
             (frame    (cl-tmux/renderer:render-session-to-string s 24 81)))
        (expect frame :to-contain-sgr expected))))

  ;; With copy mode active but no search term, no match-style SGR is emitted.
  (it "copy-mode-no-search-term-no-highlight"
    (with-fake-session (s)
      (feed (active-screen s) "hello world")
      (cl-tmux/commands::copy-mode-enter (active-screen s))
      (setf (cl-tmux/terminal/types:screen-copy-search-term (active-screen s)) nil)
      (let* ((match-sgr (cl-tmux/renderer:style-to-sgr
                         (cl-tmux/renderer:parse-style-string "bg=green")))
             (frame     (cl-tmux/renderer:render-session-to-string s 24 81)))
        (expect frame :not :to-contain-sgr match-sgr))))

  ;;; -- +min-clock-width+ constant -----------------------------------------------

  ;; +min-clock-width+ equals 13 — the documented minimum column count for the clock.
  (it "min-clock-width-constant-is-13"
    (expect (= 13 cl-tmux/renderer::+min-clock-width+)))

  ;; draw-clock-to-screen renders when the pane is exactly +min-clock-width+ wide.
  (it "draw-clock-at-min-width-renders"
    (let ((out (with-output-to-string (s)
                 (cl-tmux/renderer::draw-clock-to-screen
                  s 0 0 cl-tmux/renderer::+min-clock-width+ 3))))
      (expect (plusp (length out)))))

  ;; draw-clock-to-screen emits nothing when pane width is +min-clock-width+ - 1.
  (it "draw-clock-below-min-width-suppressed"
    (let ((out (with-output-to-string (s)
                 (cl-tmux/renderer::draw-clock-to-screen
                  s 0 0 (1- cl-tmux/renderer::+min-clock-width+) 3))))
      (expect (string= "" out))))

  ;;; -- %dispatch-pane-border-chars table ----------------------------------------

  ;; %dispatch-pane-border-chars with unknown style falls back to single-line glyphs.
  (it "dispatch-pane-border-chars-single-is-default"
    (multiple-value-bind (v h)
        (cl-tmux/renderer::%dispatch-pane-border-chars "unknown-style")
      (expect (char= #\│ v))
      (expect (char= #\─ h))))

  ;; %dispatch-pane-border-chars returns the expected glyphs for each named style.
  (it "dispatch-pane-border-chars-all-styles"
    (flet ((chars (style) (multiple-value-list
                           (cl-tmux/renderer::%dispatch-pane-border-chars style))))
      (dolist (c '(("single" #\│ #\─ "single fallback")
                   ("double" #\║ #\═ "double: ║ ═")
                   ("heavy"  #\┃ #\━ "heavy: ┃ ━")
                   ("simple" #\| #\- "simple: | -")))
        (destructuring-bind (style ev eh desc) c
          (declare (ignore desc))
          (expect (equal (list ev eh) (chars style))))))))
