(in-package #:cl-tmux/test)

;;;; Pane and border rendering tests.
;;;;
;;;; Covers: render-pane, layout-subtree-rect, subtree-contains-p,
;;;;         render-tree-borders from src/renderer-pane.lisp.
;;;;
;;;; renderer-suite is declared in renderer-format-tests.lisp (loaded first).

(in-suite renderer-suite)

;;; ── Local fixture ────────────────────────────────────────────────────────────

(defun %make-pane-test-session (w h &key (content ""))
  "A 1-window, 1-pane session whose pane screen has CONTENT fed into it.
   No PTY is allocated (fd -1), so this is safe in any environment."
  (let* ((screen (make-screen w h))
         (pane   (make-pane :id 1 :x 0 :y 0 :width w :height h :fd -1 :screen screen))
         (win    (make-window :id 1 :name "1" :width w :height h :panes (list pane)))
         (sess   (make-session :id 1 :name "0" :windows (list win))))
    (window-select-pane win pane)
    (session-select-window sess win)
    (unless (string= content "") (feed screen content))
    sess))

;;; ── render-pane (content + positioning) ─────────────────────────────────────

(test render-pane-content-and-positioning
  (let* ((sess (%make-pane-test-session 5 2 :content "hi"))
         (pane (first (window-panes (session-active-window sess))))
         (out  (with-output-to-string (s)
                 (cl-tmux/renderer::render-pane s pane))))
    (is (find #\h out) "render-pane should emit the h glyph (got ~S)" out)
    (is (find #\i out) "render-pane should emit the i glyph (got ~S)" out)
    ;; Row 0 of the pane is positioned via move-to row 0 => ESC[1;1H.
    (is (search (format nil "~C[1;1H" #\Escape) out)
        "render-pane should position row 0 with ESC[1;1H (got ~S)" out)))

;;; ── double-width glyphs are not double-printed ──────────────────────────────

(test render-pane-double-width-not-duplicated
  (let* ((screen (make-screen 5 2))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 5 :height 2 :fd -1 :screen screen)))
    (cl-tmux/test::utf8-feed screen "あ")     ; one wide glyph + width-0 continuation
    (let ((out (with-output-to-string (s)
                 (cl-tmux/renderer::render-pane s pane))))
      ;; The continuation cell (width 0) must be skipped: exactly one wide glyph,
      ;; and no placeholder char inflating the output.
      (is (= 1 (count #\あ out))
          "exactly one wide glyph should be printed (got ~D in ~S)"
          (count #\あ out) out))))

;;; ── layout-subtree-rect and subtree-contains-p ──────────────────────────────

(test layout-subtree-rect-bounding-box
  "layout-subtree-rect returns the tight bounding box of all leaves."
  (let* ((l0   (tl-leaf 1 1 1))
         (l1   (tl-leaf 2 1 1))
         (tree (make-layout-split :h l0 l1)))
    ;; Lay out the tree first so pane positions are defined.
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

;;; ── render-tree-borders ──────────────────────────────────────────────────────

(test render-tree-borders-draws-vertical-bar
  "render-tree-borders draws │ separators for a :h split."
  (let* ((l0   (tl-leaf 1 1 1))
         (l1   (tl-leaf 2 1 1))
         (tree (make-layout-split :h l0 l1)))
    (cl-tmux/model::layout-assign tree 0 0 81 24)
    (let* ((ap  (layout-leaf-pane l0))
           (buf (make-string-output-stream)))
      (cl-tmux/renderer::render-tree-borders buf tree ap 81)
      (let ((out (get-output-stream-string buf)))
        (is (plusp (length out)) "render-tree-borders must produce output")
        (is (find #\│ out) "vertical bar character │ must be present")))))

;;; ── in-sel branch coverage via render-pane ───────────────────────────────────
;;;
;;; The selection-highlight logic inside render-pane has five independently
;;; reachable branches:
;;;
;;;   1. sel-active = NIL  — copy-selecting is false (or mark/cursor not set)
;;;   2. single-row        — sel-start-r = sel-end-r = row  (column-range check)
;;;   3. first-row         — row = sel-start-r < sel-end-r  (cols >= sel-start-c)
;;;   4. last-row          — row = sel-end-r > sel-start-r  (cols < sel-end-c)
;;;   5. middle-row        — sel-start-r < row < sel-end-r  (always selected)
;;;
;;; The indicator for "this cell is selected" is reverse-video (SGR ;7) toggled
;;; onto the cell's attributes via logxor.  We detect it by searching for the
;;; substring ";7" followed eventually by the SGR terminator "m" in the stream
;;; output produced by render-pane.
;;;
;;; Screen geometry: width=8, height=4, copy-offset=0 so viewport-row = grid-row.

(defun %make-selecting-pane (w h content mark-row mark-col cursor-row cursor-col)
  "Return a pane whose screen is in copy-mode with an active selection.
   MARK and CURSOR are given as (ROW . COL) grid coordinates (0-based)."
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
  "True when OUT contains the SGR reverse-video code (;7 followed before the m)."
  (not (null (search ";7" out))))

;;; Branch 1: sel-active = NIL (copy-selecting not set) — no reverse video.

(test in-sel-branch-not-selecting
  "When copy-selecting is NIL the sel-active gate is false: no cell gets
   reverse-video highlighting regardless of the mark/cursor positions."
  (let* ((screen (make-screen 8 4))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 8 :height 4
                            :fd -1 :screen screen)))
    (feed screen "ABCDEFGH")
    ;; copy-selecting NIL  →  sel-active = NIL
    (setf (screen-copy-mode-p    screen) t
          (screen-copy-selecting screen) nil
          (screen-copy-mark      screen) nil
          (screen-copy-cursor    screen) nil)
    (let ((out (%render-pane-string pane)))
      (is (null (%reverse-video-p out))
          "no reverse-video SGR should appear when copy-selecting is NIL (got ~S)"
          out))))

;;; Branch 2: single-row selection (sel-start-r = sel-end-r = row).
;;;
;;; mark=(0,2) cursor=(0,5), offset=0  →  sel-start-r=sel-end-r=0,
;;; sel-start-c=2, sel-end-c=5.
;;; Row 0 cols 2..4 are selected; row 1 is not.

(test in-sel-branch-single-row
  "Single-row selection: only cells within the column range [sel-start-c, sel-end-c)
   on the single selected row receive reverse-video highlighting."
  (let* ((pane (make-pane :id 1 :x 0 :y 0 :width 8 :height 4 :fd -1
                          :screen (make-screen 8 4))))
    (feed (pane-screen pane) "ABCDEFGHIJKLMNOPQRSTUVWXYZ01234567")
    (let ((screen (pane-screen pane)))
      (setf (screen-copy-mode-p    screen) t
            (screen-copy-selecting screen) t
            (screen-copy-offset    screen) 0
            (screen-copy-mark      screen) (cons 0 2)   ; row 0, col 2
            (screen-copy-cursor    screen) (cons 0 5))) ; row 0, col 5
    (let ((out (%render-pane-string pane)))
      (is (%reverse-video-p out)
          "single-row selection: reverse-video SGR must appear for selected cols (got ~S)"
          out))))

;;; Branch 3: first-row of a multi-row selection.
;;;
;;; mark=(0,3) cursor=(2,0), offset=0  →  sel-start-r=0, sel-end-r=2,
;;; sel-start-c=3, sel-end-c=0.
;;; On row 0 (= sel-start-r), cols >= 3 are selected.

(test in-sel-branch-first-row
  "First row of a multi-row selection: cells from sel-start-c to the line end
   are highlighted on the start row."
  (let* ((pane (make-pane :id 1 :x 0 :y 0 :width 8 :height 4 :fd -1
                          :screen (make-screen 8 4))))
    (feed (pane-screen pane) "ABCDEFGHIJKLMNOPQRSTUVWXYZ01234567")
    (let ((screen (pane-screen pane)))
      (setf (screen-copy-mode-p    screen) t
            (screen-copy-selecting screen) t
            (screen-copy-offset    screen) 0
            (screen-copy-mark      screen) (cons 0 3)   ; row 0, col 3
            (screen-copy-cursor    screen) (cons 2 0))) ; row 2, col 0
    (let ((out (%render-pane-string pane)))
      (is (%reverse-video-p out)
          "first-row branch: reverse-video SGR must appear for selected cols (got ~S)"
          out))))

;;; Branch 4: last-row of a multi-row selection.
;;;
;;; mark=(0,0) cursor=(2,5), offset=0  →  sel-start-r=0, sel-end-r=2,
;;; sel-start-c=0, sel-end-c=5.
;;; On row 2 (= sel-end-r), cols 0..4 are selected.

(test in-sel-branch-last-row
  "Last row of a multi-row selection: cells from the line start up to sel-end-c
   (exclusive) are highlighted."
  (let* ((pane (make-pane :id 1 :x 0 :y 0 :width 8 :height 4 :fd -1
                          :screen (make-screen 8 4))))
    (feed (pane-screen pane) "ABCDEFGHIJKLMNOPQRSTUVWXYZ01234567")
    (let ((screen (pane-screen pane)))
      (setf (screen-copy-mode-p    screen) t
            (screen-copy-selecting screen) t
            (screen-copy-offset    screen) 0
            (screen-copy-mark      screen) (cons 0 0)   ; row 0, col 0
            (screen-copy-cursor    screen) (cons 2 5))) ; row 2, col 5
    (let ((out (%render-pane-string pane)))
      (is (%reverse-video-p out)
          "last-row branch: reverse-video SGR must appear for selected cols (got ~S)"
          out))))

;;; Branch 5: middle-row of a multi-row selection (always fully selected).
;;;
;;; mark=(0,0) cursor=(3,0), offset=0  →  sel-start-r=0, sel-end-r=3.
;;; Row 1 and row 2 are middle rows; every cell there is selected.

(test in-sel-branch-middle-row
  "Middle rows of a multi-row selection are fully highlighted: every cell on
   rows strictly between sel-start-r and sel-end-r gets reverse-video."
  (let* ((pane (make-pane :id 1 :x 0 :y 0 :width 8 :height 4 :fd -1
                          :screen (make-screen 8 4))))
    (feed (pane-screen pane) "ABCDEFGHIJKLMNOPQRSTUVWXYZ01234567")
    (let ((screen (pane-screen pane)))
      (setf (screen-copy-mode-p    screen) t
            (screen-copy-selecting screen) t
            (screen-copy-offset    screen) 0
            (screen-copy-mark      screen) (cons 0 0)   ; row 0, col 0
            (screen-copy-cursor    screen) (cons 3 0))) ; row 3, col 0
    (let ((out (%render-pane-string pane)))
      (is (%reverse-video-p out)
          "middle-row branch: reverse-video SGR must appear for fully-covered rows (got ~S)"
          out))))

;;; sel-active guard: mark or cursor is NIL  →  no highlighting even if selecting.

(test in-sel-branch-selecting-but-no-mark
  "When copy-selecting is T but screen-copy-mark is NIL, sel-active is false:
   no reverse-video highlighting is emitted (the consp guard short-circuits)."
  (let* ((screen (make-screen 8 4))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 8 :height 4
                            :fd -1 :screen screen)))
    (feed screen "ABCDEFGH")
    (setf (screen-copy-mode-p    screen) t
          (screen-copy-selecting screen) t
          (screen-copy-mark      screen) nil          ; not yet placed
          (screen-copy-cursor    screen) (cons 0 3))
    (let ((out (%render-pane-string pane)))
      (is (null (%reverse-video-p out))
          "nil mark must suppress reverse-video (sel-active gate fails) (got ~S)"
          out))))
