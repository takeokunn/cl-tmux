(in-package #:cl-tmux/test)

;;;; renderer-pane tests — part B: %clock-digit-rows, %render-v-separator,
;;;; render-tree-borders with :v split, layout-subtree-rect single-leaf,
;;;; subtree-contains-p nil-pane corner case, additional in-sel/pane/border coverage.

(in-suite renderer-suite)

;;; -- %clock-digit-rows -------------------------------------------------------

(test clock-digit-rows-table
  "%clock-digit-rows returns 3 non-empty strings for representative digits."
  (dolist (digit '(0 9))
    (let ((rows (cl-tmux/renderer::%clock-digit-rows digit)))
      (is (= 3 (length rows))
          "digit ~D: must return 3 rows (got ~D)" digit (length rows))
      (is (every (lambda (r) (and (stringp r) (plusp (length r)))) rows)
          "digit ~D: all rows must be non-empty strings" digit))))

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

(defun %in-sel (row col sr er sc ec &optional rect-p)
  "Call in-selection-p with positional args in a more readable order."
  (cl-tmux/renderer::in-selection-p row col sr er sc ec rect-p))

(test in-selection-p-table
  "in-selection-p covers all four cond branches (single-row, first/last/mid row, rect mode).
   Each row is (expected row col sr er sc ec rect-p description)."
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
      (if expected
          (is-true  (%in-sel row col sr er sc ec rect-p) "~A must be T"   desc)
          (is-false (%in-sel row col sr er sc ec rect-p) "~A must be NIL" desc)))))

;;; -- %compute-selection-bounds unit tests ------------------------------------

(test compute-selection-bounds-active-selection
  "%compute-selection-bounds returns sel-active=T when all prerequisites are present."
  (let ((screen (make-selecting-screen 10 5 1 2 3 4)))
    (multiple-value-bind (active sr er sc ec rect-p)
        (cl-tmux/renderer::%compute-selection-bounds screen)
      (is-true  active  "sel-active must be T when all prerequisites present")
      (is-false rect-p  "rect-p must be NIL for non-rectangle selection")
      (is (= 1 sr) "start row must be min(mark-row, cursor-row)")
      (is (= 3 er) "end row must be max(mark-row, cursor-row)")
      (is (= 2 sc) "start col: mark-col when mark-row < cursor-row")
      (is (= 4 ec) "end col: cursor-col when mark-row < cursor-row"))))

(test compute-selection-bounds-no-selecting
  "%compute-selection-bounds returns sel-active=NIL when copy-selecting is NIL."
  (let ((screen (make-screen 10 5)))
    (setf (cl-tmux/terminal/types:screen-copy-selecting screen) nil
          (cl-tmux/terminal/types:screen-copy-mark      screen) (cons 0 0)
          (cl-tmux/terminal/types:screen-copy-cursor    screen) (cons 1 1))
    (multiple-value-bind (active sr er sc ec rect-p)
        (cl-tmux/renderer::%compute-selection-bounds screen)
      (declare (ignore sr er sc ec rect-p))
      (is-false active "sel-active must be NIL when copy-selecting is NIL"))))

(test compute-selection-bounds-nil-mark
  "%compute-selection-bounds returns sel-active=NIL when mark is NIL."
  (let ((screen (make-screen 10 5)))
    (setf (cl-tmux/terminal/types:screen-copy-selecting screen) t
          (cl-tmux/terminal/types:screen-copy-mark      screen) nil
          (cl-tmux/terminal/types:screen-copy-cursor    screen) (cons 1 1))
    (multiple-value-bind (active sr er sc ec rect-p)
        (cl-tmux/renderer::%compute-selection-bounds screen)
      (declare (ignore sr er sc ec rect-p))
      (is-false active "sel-active must be NIL when mark is NIL"))))

(test compute-selection-bounds-reversed-rows-normalised
  "%compute-selection-bounds normalises row order so start <= end."
  ;; cursor above mark — rows should be swapped in the output
  (let ((screen (make-selecting-screen 10 5 3 5 1 2)))
    (multiple-value-bind (active sr er sc ec rect-p)
        (cl-tmux/renderer::%compute-selection-bounds screen)
      (declare (ignore rect-p))
      (is-true active "sel-active must be T")
      (is (<= sr er) "start row (~D) must be <= end row (~D)" sr er)
      (is (= 1 sr) "start row must be min(mark-row=3, cursor-row=1)=1")
      (is (= 3 er) "end row must be max(mark-row=3, cursor-row=1)=3")
      ;; cursor-row < mark-row: start-col = cursor-col, end-col = mark-col
      (is (= 2 sc) "start col = cursor-col when cursor-row < mark-row")
      (is (= 5 ec) "end col = mark-col when cursor-row < mark-row"))))

(test compute-selection-bounds-same-row-cols-normalised
  "%compute-selection-bounds normalises col order for same-row selections."
  (let ((screen (make-selecting-screen 10 5 2 7 2 3)))
    (multiple-value-bind (active sr er sc ec rect-p)
        (cl-tmux/renderer::%compute-selection-bounds screen)
      (declare (ignore rect-p))
      (is-true active "sel-active must be T")
      (is (= 2 sr) "both rows are 2")
      (is (= 2 er) "both rows are 2")
      (is (= 3 sc) "start col = min(mark-col=7, cursor-col=3)=3")
      (is (= 7 ec) "end col = max(mark-col=7, cursor-col=3)=7"))))

(test compute-selection-bounds-copy-offset-applied
  "%compute-selection-bounds maps virtual rows to viewport rows using the CURRENT offset.
   When the mark was placed at offset=0 (mark-offset=0, the default) and the viewport
   has since scrolled to offset=2, the mark at viewport row 4 now resolves to viewport
   row 4+2=6 (off-screen, clamped to height-1=4).  The cursor at viewport row 2 with
   offset=2 resolves to virtual row 0, which at offset=2 is viewport row 2.
   Selection: viewport rows 2-4 (mark is clamped to the bottom edge)."
  ;; No scrollback.  mark=(4,0) was set when offset=0 (mark-offset=0 by default).
  ;; Current offset=2, cursor=(2,0).
  (let ((screen (make-selecting-screen 10 5 4 0 2 0 :offset 2)))
    (multiple-value-bind (active sr er sc ec rect-p)
        (cl-tmux/renderer::%compute-selection-bounds screen)
      (declare (ignore sc ec rect-p))
      (is-true active "sel-active must be T")
      (is (= 2 sr) "start row: cursor vrow=0 → viewport 0-0+2=2")
      (is (= 4 er) "end row: mark vrow=4 → viewport 4-0+2=6, clamped to height-1=4"))))

(test compute-selection-bounds-rect-columns-symmetric
  "%compute-selection-bounds uses min/max column symmetrically in rectangle mode."
  ;; mark at (row=1, col=6), cursor at (row=4, col=2): rect cols [2,7) inclusive
  (let ((screen (make-selecting-screen 10 6 1 6 4 2 :rect t)))
    (multiple-value-bind (active sr er sc ec rect-p)
        (cl-tmux/renderer::%compute-selection-bounds screen)
      (is-true  active  "sel-active must be T")
      (is-true  rect-p  "rect-p must be T when :rect t")
      (is (= 1 sr) "start row = min(1,4) = 1")
      (is (= 4 er) "end row = max(1,4) = 4")
      (is (= 2 sc) "start col = min(mark-col=6, cursor-col=2) = 2")
      (is (= 7 ec) "end col = 1+max(6,2) = 7 (exclusive)"))))

(test compute-selection-bounds-rect-columns-reversed
  "%compute-selection-bounds swaps columns correctly when cursor-col > mark-col in rect mode."
  ;; mark at (row=2, col=3), cursor at (row=5, col=8)
  (let ((screen (make-selecting-screen 10 8 2 3 5 8 :rect t)))
    (multiple-value-bind (active sr er sc ec rect-p)
        (cl-tmux/renderer::%compute-selection-bounds screen)
      (declare (ignore sr er))
      (is-true  rect-p  "rect-p must be T")
      (is (= 3 sc) "start col = min(3,8) = 3")
      (is (= 9 ec) "end col = 1+max(3,8) = 9"))))

;;; -- make-test-pane and make-selecting-screen fixture helpers -------------------

(test make-test-pane-creates-correct-geometry
  "make-test-pane returns a pane with the requested width, height, id, and origin."
  (let ((pane (make-test-pane 20 5 :id 7 :x 3 :y 2)))
    (is (= 20 (pane-width  pane)) "pane width must be 20")
    (is (= 5  (pane-height pane)) "pane height must be 5")
    (is (= 7  (pane-id     pane)) "pane id must be 7")
    (is (= 3  (pane-x      pane)) "pane x must be 3")
    (is (= 2  (pane-y      pane)) "pane y must be 2")
    (is (screen-p (pane-screen pane)) "pane screen must be a screen struct")))

(test make-test-pane-feeds-content
  "make-test-pane feeds :content into the pane screen."
  (let* ((pane   (make-test-pane 10 5 :content "AB"))
         (screen (pane-screen pane)))
    (is (char= #\A (cell-char (screen-cell screen 0 0)))
        "first char must be A")
    (is (char= #\B (cell-char (screen-cell screen 1 0)))
        "second char must be B")))

(test make-selecting-screen-sets-selection-state
  "make-selecting-screen returns a screen with copy-selecting T and the given mark/cursor."
  (let ((screen (make-selecting-screen 10 5 1 2 3 4)))
    (is-true (cl-tmux/terminal/types:screen-copy-selecting screen)
             "copy-selecting must be T")
    (is (equal (cons 1 2) (cl-tmux/terminal/types:screen-copy-mark screen))
        "mark must be (1 . 2)")
    (is (equal (cons 3 4) (cl-tmux/terminal/types:screen-copy-cursor screen))
        "cursor must be (3 . 4)")
    (is (= 0 (cl-tmux/terminal/types:screen-copy-offset screen))
        "default offset must be 0")))

(test make-selecting-screen-custom-offset
  "make-selecting-screen respects the :offset keyword."
  (let ((screen (make-selecting-screen 10 5 0 0 1 0 :offset 7)))
    (is (= 7 (cl-tmux/terminal/types:screen-copy-offset screen))
        "copy-offset must be 7")))

;;; -- %render-pane-border-status coverage ------------------------------------
;;;
;;; %render-pane-border-status (~line 250-271 in renderer-pane.lisp) is only
;;; reachable when pane-border-status is not "off".  These tests exercise the
;;; top/bottom row placement branches and the format expansion path.

(defun %border-status-output (pane session win status-val fmt-val)
  "Run %render-pane-border-status with STATUS-VAL and FMT-VAL options and return output."
  (with-isolated-options ("pane-border-status" status-val
                          "pane-border-format"  fmt-val)
    (with-output-to-string (s)
      (cl-tmux/renderer::%render-pane-border-status s pane session win))))

(test render-pane-border-status-off-produces-nothing
  "%render-pane-border-status does nothing when pane-border-status is \"off\"."
  (let* ((pane (make-test-pane 20 5 :id 1))
         (sess (make-fake-session :nwindows 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (out  (%border-status-output pane sess win "off" " #{pane_index} ")))
    (is (string= "" out)
        "pane-border-status=off must produce no output (got ~S)" out)))

(test render-pane-border-status-top-positions-above-content
  "%render-pane-border-status with status=top places the label on the RESERVED row
   just above the content (pane-y - 1), so it never overwrites pane content."
  (let* ((pane (make-test-pane 20 5 :id 1 :y 3))
         (sess (make-fake-session :nwindows 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (out  (%border-status-output pane sess win "top" "TITLE")))
    ;; Reserved row = pane-y - 1 = 2 → ESC[3;1H (1-based: 2+1=3)
    (is (search (format nil "~C[3;" #\Escape) out)
        "top status must position at the row above content (pane-y-1=2 → ESC[3;...H) (got ~S)" out)
    (is (search "TITLE" out)
        "top status must emit the format text (got ~S)" out)))

(test render-pane-border-status-bottom-positions-below-content
  "%render-pane-border-status with status=bottom places the label on the RESERVED
   row just below the content (pane-y + pane-height)."
  (let* ((pane (make-test-pane 20 5 :id 1 :y 0))
         (sess (make-fake-session :nwindows 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (out  (%border-status-output pane sess win "bottom" "BOT")))
    ;; Reserved row = pane-y + pane-height = 0 + 5 = 5 → ESC[6;1H
    (is (search (format nil "~C[6;" #\Escape) out)
        "bottom status must position at the row below content (5 → ESC[6;...H) (got ~S)" out)
    (is (search "BOT" out)
        "bottom status must emit the format text (got ~S)" out)))

(test render-pane-border-status-truncates-to-pane-width
  "%render-pane-border-status truncates the label to pane-width characters."
  (let* ((pane (make-test-pane 5 3 :id 1))
         (sess (make-fake-session :nwindows 1))
         (win  (first (cl-tmux/model:session-windows sess)))
         (out  (%border-status-output pane sess win "top" "ABCDEFGHIJ")))
    ;; Only the first 5 visible chars should appear (pane-width=5).
    ;; The status text "ABCDEFGHIJ" should be truncated to "ABCDE".
    (is (search "ABCDE" out)
        "border status must emit first 5 chars for a 5-wide pane (got ~S)" out)
    (is (null (search "ABCDEF" out))
        "border status must not emit more than pane-width chars (got ~S)" out)))

;;; -- copy-mode search-match highlighting -------------------------------------

(test all-match-ranges-literal-and-regex
  "%all-match-ranges returns every match span; regex with literal fallback."
  (is (equal '((0 . 3) (8 . 11))
             (cl-tmux/renderer::%all-match-ranges "abc" "abc def abc"))
      "two literal matches")
  (is (equal '((4 . 7))
             (cl-tmux/renderer::%all-match-ranges "[0-9]+" "abc 123 xyz"))
      "regex digit run")
  (is (equal '((2 . 3))
             (cl-tmux/renderer::%all-match-ranges "(" "a ( b"))
      "invalid regex falls back to literal substring"))

(test copy-mode-search-matches-highlighted-in-frame
  "When copy mode has a search term, render-session-to-string overdraws matches in
   copy-mode-match-style."
  (with-fake-session (s)
    (feed (active-screen s) "hello world hello")
    (cl-tmux/commands::copy-mode-enter (active-screen s))
    (setf (cl-tmux/terminal/types:screen-copy-search-term (active-screen s)) "hello")
    (let* ((expected (cl-tmux/renderer:style-to-sgr
                      (cl-tmux/renderer:parse-style-string "bg=green")))
           (frame    (cl-tmux/renderer:render-session-to-string s 24 81)))
      (is (search (format nil "~C[~Am" #\Escape expected) frame)
          "matches must be drawn in copy-mode-match-style (~S)" expected))))

(test copy-mode-no-search-term-no-highlight
  "With copy mode active but no search term, no match-style SGR is emitted."
  (with-fake-session (s)
    (feed (active-screen s) "hello world")
    (cl-tmux/commands::copy-mode-enter (active-screen s))
    (setf (cl-tmux/terminal/types:screen-copy-search-term (active-screen s)) nil)
    (let* ((match-sgr (cl-tmux/renderer:style-to-sgr
                       (cl-tmux/renderer:parse-style-string "bg=green")))
           (frame     (cl-tmux/renderer:render-session-to-string s 24 81)))
      (is (null (search (format nil "~C[~Am" #\Escape match-sgr) frame))
          "no search term → no match highlighting"))))

;;; -- +min-clock-width+ constant -----------------------------------------------

(test min-clock-width-constant-is-13
  "+min-clock-width+ equals 13 — the documented minimum column count for the clock."
  (is (= 13 cl-tmux/renderer::+min-clock-width+)
      "+min-clock-width+ must be 13 (got ~D)" cl-tmux/renderer::+min-clock-width+))

(test draw-clock-at-min-width-renders
  "draw-clock-to-screen renders when the pane is exactly +min-clock-width+ wide."
  (let ((out (with-output-to-string (s)
               (cl-tmux/renderer::draw-clock-to-screen
                s 0 0 cl-tmux/renderer::+min-clock-width+ 3))))
    (is (plusp (length out))
        "clock must render at exactly +min-clock-width+ columns (got empty string)")))

(test draw-clock-below-min-width-suppressed
  "draw-clock-to-screen emits nothing when pane width is +min-clock-width+ - 1."
  (let ((out (with-output-to-string (s)
               (cl-tmux/renderer::draw-clock-to-screen
                s 0 0 (1- cl-tmux/renderer::+min-clock-width+) 3))))
    (is (string= "" out)
        "clock must NOT render when width is +min-clock-width+ - 1 (got ~S)" out)))

;;; -- %dispatch-pane-border-chars table ----------------------------------------

(test dispatch-pane-border-chars-single-is-default
  "%dispatch-pane-border-chars with unknown style falls back to single-line glyphs."
  (multiple-value-bind (v h)
      (cl-tmux/renderer::%dispatch-pane-border-chars "unknown-style")
    (is (char= #\│ v) "unknown style v must fall back to │")
    (is (char= #\─ h) "unknown style h must fall back to ─")))

(test dispatch-pane-border-chars-all-styles
  "%dispatch-pane-border-chars returns the expected glyphs for each named style."
  (flet ((chars (style) (multiple-value-list
                         (cl-tmux/renderer::%dispatch-pane-border-chars style))))
    (dolist (c '(("single" #\│ #\─ "single fallback")
                 ("double" #\║ #\═ "double: ║ ═")
                 ("heavy"  #\┃ #\━ "heavy: ┃ ━")
                 ("simple" #\| #\- "simple: | -")))
      (destructuring-bind (style ev eh desc) c
        (is (equal (list ev eh) (chars style)) "~A" desc)))))
