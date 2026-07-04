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
;;; make-renderer-test-session (defined in tests/helpers-renderer-fixtures.lisp) is the canonical
;;; shared fixture.  The old local %make-pane-test-session has been removed in
;;; favour of the shared version.

(defun %snippet-around (text needle &optional (radius 24))
  (let ((pos (position needle text)))
    (and pos
         (subseq text pos (min (length text) (+ pos radius))))))

;;; -- render-pane (content + positioning) ------------------------------------

(test render-pane-content-and-positioning
  "render-pane emits the pane's cell glyphs preceded by a cursor-position sequence for row 0."
  (let* ((sess (make-renderer-test-session 5 2 :content "hi"))
         (pane (first (window-panes (session-active-window sess))))
         (out  (render-pane-output sess pane)))
    (is (find #\h out) "render-pane should emit the h glyph (got ~S)" out)
    (is (find #\i out) "render-pane should emit the i glyph (got ~S)" out)
    (is (search (format nil "~C[1;1H" #\Escape) out)
        "render-pane should position row 0 with ESC[1;1H (got ~S)" out)))

(test render-pane-decscnm-reverses-output
  "With DECSCNM (reverse-screen) on, render-pane emits the reverse attribute (SGR 7)
   globally; the rendered output differs from the non-reversed render."
  (with-isolated-options ("window-style" ""
                          "window-active-style" "")
    (let* ((sess   (make-renderer-test-session 2 1))
           (pane   (first (window-panes (session-active-window sess))))
           (screen (pane-screen pane)))
      (setf (screen-cell screen 0 0)
            (cl-tmux/terminal/types:make-cell :char #\A))
      (setf (cl-tmux/terminal/types:screen-reverse-screen screen) nil)
      (let ((normal (render-pane-output sess pane)))
        (setf (cl-tmux/terminal/types:screen-reverse-screen screen) t)
        (let ((reversed (render-pane-output sess pane)))
          (let ((normal-snippet (%snippet-around normal #\A))
                (reversed-snippet (%snippet-around reversed #\A)))
            (is (not (string= normal reversed))
                "reverse-screen on must change the rendered output")
            (is (and reversed-snippet (search ";7" reversed-snippet))
                "reversed render must carry the reverse SGR ;7 near the cell (got ~S)"
                reversed-snippet)
            (is (and normal-snippet (null (search ";7" normal-snippet)))
                "non-reversed render must not emit the reverse SGR ;7 near the cell (got ~S)"
                normal-snippet)))))))

;;; -- double-width glyphs are not double-printed ------------------------------

(test render-pane-double-width-not-duplicated
  "A double-width glyph occupying two cells is printed exactly once, not twice."
  (let* ((sess   (make-renderer-test-session 5 2))
         (pane   (first (window-panes (session-active-window sess))))
         (screen (pane-screen pane)))
    (cl-tmux/test::utf8-feed screen "あ")
    (let ((out (render-pane-output sess pane)))
      (is (= 1 (count #\あ out))
          "exactly one wide glyph should be printed (got ~D in ~S)"
          (count #\あ out) out))))

;;; -- OSC 8 hyperlinks re-emitted around their cell span ----------------------

(test render-pane-emits-osc-8-hyperlink
  "A cell written under OSC 8 is re-emitted with its hyperlink (set before the
   cell, cleared after) so the outer terminal makes it clickable."
  (let* ((sess   (make-renderer-test-session 10 2))
         (pane   (first (window-panes (session-active-window sess))))
         (screen (pane-screen pane)))
    (feed screen (format nil "~C]8;;https://x~C\\X" #\Escape #\Escape))
    (let ((out (render-pane-output sess pane)))
      (is (search (format nil "~C]8;;https://x~C\\" #\Escape #\Escape) out)
          "render-pane must emit OSC 8 set for the hyperlinked cell (got ~S)" out)
      (is (search (format nil "~C]8;;~C\\" #\Escape #\Escape) out)
          "render-pane must emit an OSC 8 clear after the link span (got ~S)" out))))

(test render-pane-no-osc-8-without-hyperlink
  "Plain content (no OSC 8) emits no OSC 8 sequence — existing render output is
   unchanged for the common no-hyperlink case."
  (let* ((sess   (make-renderer-test-session 10 2))
         (pane   (first (window-panes (session-active-window sess))))
         (screen (pane-screen pane)))
    (feed screen "plain")
    (let ((out (render-pane-output sess pane)))
      (is (null (search (format nil "~C]8;" #\Escape) out))
          "no OSC 8 must be emitted when no cell has a hyperlink (got ~S)" out))))

;;; -- window-style / window-active-style (pane background recolour) -----------

(test color-name-to-cell-color-maps-names-palette-and-truecolor
  "%color-name-to-cell-color converts to the cell colour encoding; default/empty
   yields NIL (no override)."
  (dolist (c '(("red" 1) ("brightred" 9) ("colour235" 235)
               ("default" nil) ("" nil) (nil nil)))
    (destructuring-bind (name expected) c
      (is (eql expected (cl-tmux/renderer::%color-name-to-cell-color name))
          "~S should map to ~S (got ~S)" name expected
          (cl-tmux/renderer::%color-name-to-cell-color name))))
  (is (= (logior #x1000000 #xff8800)
         (cl-tmux/renderer::%color-name-to-cell-color "#ff8800"))
      "truecolor hex names should map to the 0x1000000-tagged RGB encoding"))

(test window-style-default-colors-extracts-fg-bg
  "%window-style-default-colors returns the fg/bg cell numbers a style sets, NIL
   for ones it omits, and (NIL NIL) for an empty style."
  (multiple-value-bind (fg bg)
      (cl-tmux/renderer::%window-style-default-colors "fg=red,bg=colour235")
    (is (= 1 fg)) (is (= 235 bg)))
  (multiple-value-bind (fg bg)
      (cl-tmux/renderer::%window-style-default-colors "bg=colour52")
    (is (null fg)) (is (= 52 bg)))
  (multiple-value-bind (fg bg)
      (cl-tmux/renderer::%window-style-default-colors "")
    (is (null fg)) (is (null bg))))

(test render-pane-applies-window-style-to-default-cells
  "With window-style set, a pane's default-bg (0) cells render with the style's
   background SGR; unset, they do not — verifying the opt-in recolour."
  (with-isolated-config
    (let* ((sess (make-renderer-test-session 5 2 :content "hi"))
           (pane (first (window-panes (session-active-window sess)))))
      ;; Baseline: no window-style → no colour-52 background emitted.
      (let ((out (render-pane-output sess pane)))
        (is (not (search "48;5;52" out))
            "default render must not contain the window-style bg (got ~S)" out))
      ;; Opt in: window-style recolours the default-bg cells.
      (cl-tmux/options:set-option "window-style" "bg=colour52")
      (let ((out (render-pane-output sess pane)))
        (is (search "48;5;52" out)
            "pane must emit bg colour52 (48;5;52) for default cells (got ~S)" out)))))

(test pane-cell-base-colors-preserves-explicit-background
  "%pane-cell-base-colors only substitutes the pane defaults: an explicit
   non-default background survives unchanged."
  (let ((cell (cl-tmux/terminal/types:make-cell :char #\X :fg cl-tmux/terminal/types:+default-color+ :bg 200)))
    (multiple-value-bind (fg bg)
        (cl-tmux/renderer::%pane-cell-base-colors cell 31 52)
      (is (= 31 fg) "default fg should be replaced by the pane default")
      (is (= 200 bg) "an explicit bg must survive window-style recolour"))))

(test pane-cell-base-colors-recolours-only-default-sentinel
  "%pane-cell-base-colors substitutes the pane defaults ONLY for cells whose
   colour is the +default-color+ sentinel; explicit white(7)/black(0) survive."
  ;; Default-sentinel cell: both fg and bg get the pane defaults.
  (let ((cell (cl-tmux/terminal/types:make-cell
               :char #\X
               :fg cl-tmux/terminal/types:+default-color+
               :bg cl-tmux/terminal/types:+default-color+)))
    (multiple-value-bind (fg bg)
        (cl-tmux/renderer::%pane-cell-base-colors cell 31 52)
      (is (= 31 fg) "default-sentinel fg must take the pane default")
      (is (= 52 bg) "default-sentinel bg must take the pane default")))
  ;; Explicit white(7)/black(0): NOT recoloured (this is the gap fix).
  (let ((cell (cl-tmux/terminal/types:make-cell :char #\X :fg 7 :bg 0)))
    (multiple-value-bind (fg bg)
        (cl-tmux/renderer::%pane-cell-base-colors cell 31 52)
      (is (= 7 fg) "explicit white fg(7) must survive window-style recolour")
      (is (= 0 bg) "explicit black bg(0) must survive window-style recolour"))))

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
    (let ((out (render-tree-borders-output tree (layout-leaf-pane l0) 81)))
      (is (plusp (length out)) "render-tree-borders must produce output")
      (is (find #\│ out) "vertical bar character must be present"))))

(test border-indicators-colour-p-honours-option
  "%border-indicators-colour-p follows tmux: colour for colour/both; arrows is
   arrows-only (no colour); off disables everything."
  (with-isolated-options ("pane-border-indicators" "off")
    (is (not (cl-tmux/renderer::%border-indicators-colour-p)) "off → no colour")
    (is (not (cl-tmux/renderer::%border-indicators-arrows-p)) "off → no arrows"))
  (with-isolated-options ("pane-border-indicators" "colour")
    (is-true (cl-tmux/renderer::%border-indicators-colour-p) "colour → colour")
    (is (not (cl-tmux/renderer::%border-indicators-arrows-p)) "colour → no arrows"))
  (with-isolated-options ("pane-border-indicators" "both")
    (is-true (cl-tmux/renderer::%border-indicators-colour-p) "both → colour")
    (is-true (cl-tmux/renderer::%border-indicators-arrows-p) "both → arrows"))
  (with-isolated-options ("pane-border-indicators" "arrows")
    (is (not (cl-tmux/renderer::%border-indicators-colour-p))
        "arrows → arrows only, no colour (tmux)")
    (is-true (cl-tmux/renderer::%border-indicators-arrows-p) "arrows → arrows")))

(test border-arrows-drawn-pointing-at-active-pane
  "pane-border-indicators arrows/both draw an arrow glyph on the separator
   pointing at the active pane; colour-only does not."
  (let* ((l0   (tl-leaf 1 1 1))
         (l1   (tl-leaf 2 1 1))
         (tree (make-layout-split :h l0 l1)))
    (cl-tmux/model::layout-assign tree 0 0 81 24)
    (let ((ap (layout-leaf-pane l0)))
      (with-isolated-options ("pane-border-indicators" "arrows")
        (is (find #\← (render-tree-borders-output tree ap 81))
            "active pane on the LEFT must draw a left-pointing arrow"))
      (with-isolated-options ("pane-border-indicators" "both")
        (is (find #\← (render-tree-borders-output tree ap 81))
            "both must also draw the arrow"))
      (with-isolated-options ("pane-border-indicators" "colour")
        (is (not (find #\← (render-tree-borders-output tree ap 81)))
            "colour-only must not draw arrows"))
      (let ((ap2 (layout-leaf-pane l1)))
        (with-isolated-options ("pane-border-indicators" "arrows")
          (is (find #\→ (render-tree-borders-output tree ap2 81))
              "active pane on the RIGHT must draw a right-pointing arrow"))))))

(test border-lines-padded-and-number
  "pane-border-lines padded draws blank borders; number writes the adjacent
   pane's number into the border."
  (let* ((l0   (tl-leaf 1 1 1))
         (l1   (tl-leaf 2 1 1))
         (tree (make-layout-split :h l0 l1)))
    (cl-tmux/model::layout-assign tree 0 0 81 24)
    (let ((ap (layout-leaf-pane l0)))
      (with-isolated-options ("pane-border-lines" "padded")
        (is (not (find #\│ (render-tree-borders-output tree ap 81)))
            "padded borders must not contain line glyphs"))
      (with-isolated-options ("pane-border-lines" "number")
        (let ((out (render-tree-borders-output tree ap 81)))
          (is (find (char (format nil "~D" (cl-tmux/model:pane-id
                                            (layout-leaf-pane l0)))
                          0)
                    out)
              "number borders must contain the adjacent pane's number"))))))

(test pane-border-indicators-off-suppresses-active-colour
  "pane-border-indicators \"off\" suppresses the active-pane border colour; the
   default (\"colour\") keeps it (pane-active-border-style fg=green → SGR 32)."
  (let* ((l0   (tl-leaf 1 1 1))
         (l1   (tl-leaf 2 1 1))
         (tree (make-layout-split :h l0 l1)))
    (cl-tmux/model::layout-assign tree 0 0 81 24)
    (let ((ap (layout-leaf-pane l0)))
      (with-isolated-options ("pane-border-indicators" "colour"
                              "pane-active-border-style" "fg=green")
        (is (search (format nil "~C[32m" #\Escape)
                    (render-tree-borders-output tree ap 81))
            "default indicators must colour the active border green (32)"))
      (with-isolated-options ("pane-border-indicators" "off"
                              "pane-active-border-style" "fg=green")
        (is (null (search (format nil "~C[32m" #\Escape)
                          (render-tree-borders-output tree ap 81)))
            "indicators off must NOT colour the active border")))))

(test pane-border-chars-follow-pane-border-lines
  "%pane-border-chars selects glyph pairs by pane-border-lines; unknown/number
   fall back to single."
  (with-isolated-config
    (flet ((chars () (multiple-value-list (cl-tmux/renderer::%pane-border-chars))))
      (dolist (c '(("single" (#\│ #\─))
                   ("double" (#\║ #\═))
                   ("heavy"  (#\┃ #\━))
                   ("simple" (#\| #\-))
                   ("number" (#\│ #\─))))
        (destructuring-bind (style expected) c
          (cl-tmux/options:set-option "pane-border-lines" style)
          (is (equal expected (chars))
              "pane-border-lines ~S should select chars ~S (got ~S)"
              style expected (chars)))))))

(test render-tree-borders-honours-pane-border-lines-double
  "With pane-border-lines double, the vertical separator uses ║ not │."
  (with-isolated-config
    (cl-tmux/options:set-option "pane-border-lines" "double")
    (let* ((l0   (tl-leaf 1 1 1))
           (l1   (tl-leaf 2 1 1))
           (tree (make-layout-split :h l0 l1)))
      (cl-tmux/model::layout-assign tree 0 0 81 24)
      (let ((out (render-tree-borders-output tree (layout-leaf-pane l0) 81)))
        (is (find #\║ out) "double border must draw ║ (got ~S)" out)
        (is (not (find #\│ out)) "double border must not draw the single-line │")))))
