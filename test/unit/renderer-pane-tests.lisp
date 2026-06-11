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

(test render-pane-decscnm-reverses-output
  "With DECSCNM (reverse-screen) on, render-pane emits the reverse attribute (SGR 7)
   globally; the rendered output differs from the non-reversed render."
  (let* ((sess   (make-renderer-test-session 5 2 :content "hi"))
         (pane   (first (window-panes (session-active-window sess))))
         (screen (pane-screen pane)))
    (flet ((render () (with-output-to-string (s) (cl-tmux/renderer::render-pane s pane))))
      (setf (cl-tmux/terminal/types:screen-reverse-screen screen) nil)
      (let ((normal (render)))
        (setf (cl-tmux/terminal/types:screen-reverse-screen screen) t)
        (let ((reversed (render)))
          (is (not (string= normal reversed))
              "reverse-screen on must change the rendered output")
          ;; render-cell-attrs emits the reverse attribute as the SGR token ";7"
          ;; (default fg=7 emits ";37", which does not contain ";7").
          (is (search ";7" reversed)
              "reversed render must carry the reverse SGR ;7 (got ~S)" reversed)
          (is (null (search ";7" normal))
              "non-reversed render must not emit the reverse SGR ;7 (got ~S)" normal))))))

;;; -- double-width glyphs are not double-printed ------------------------------

(test render-pane-double-width-not-duplicated
  (let* ((pane   (make-test-pane 5 2))
         (screen (pane-screen pane)))
    (cl-tmux/test::utf8-feed screen "あ")
    (let ((out (with-output-to-string (s)
                 (cl-tmux/renderer::render-pane s pane))))
      (is (= 1 (count #\あ out))
          "exactly one wide glyph should be printed (got ~D in ~S)"
          (count #\あ out) out))))

;;; -- OSC 8 hyperlinks re-emitted around their cell span ----------------------

(test render-pane-emits-osc-8-hyperlink
  "A cell written under OSC 8 is re-emitted with its hyperlink (set before the
   cell, cleared after) so the outer terminal makes it clickable."
  (let* ((pane   (make-test-pane 10 2))
         (screen (pane-screen pane)))
    (feed screen (format nil "~C]8;;https://x~C\\X" #\Escape #\Escape))
    (let ((out (with-output-to-string (s) (cl-tmux/renderer::render-pane s pane))))
      (is (search (format nil "~C]8;;https://x~C\\" #\Escape #\Escape) out)
          "render-pane must emit OSC 8 set for the hyperlinked cell (got ~S)" out)
      (is (search (format nil "~C]8;;~C\\" #\Escape #\Escape) out)
          "render-pane must emit an OSC 8 clear after the link span (got ~S)" out))))

(test render-pane-no-osc-8-without-hyperlink
  "Plain content (no OSC 8) emits no OSC 8 sequence — existing render output is
   unchanged for the common no-hyperlink case."
  (let* ((pane   (make-test-pane 10 2))
         (screen (pane-screen pane)))
    (feed screen "plain")
    (let ((out (with-output-to-string (s) (cl-tmux/renderer::render-pane s pane))))
      (is (null (search (format nil "~C]8;" #\Escape) out))
          "no OSC 8 must be emitted when no cell has a hyperlink (got ~S)" out))))

;;; -- window-style / window-active-style (pane background recolour) -----------

(test color-name-to-cell-color-maps-names-palette-and-truecolor
  "%color-name-to-cell-color converts to the cell colour encoding; default/empty
   yields NIL (no override)."
  (is (= 1   (cl-tmux/renderer::%color-name-to-cell-color "red")))
  (is (= 9   (cl-tmux/renderer::%color-name-to-cell-color "brightred")))
  (is (= 235 (cl-tmux/renderer::%color-name-to-cell-color "colour235")))
  (is (= (logior #x1000000 #xff8800)
         (cl-tmux/renderer::%color-name-to-cell-color "#ff8800")))
  (is (null (cl-tmux/renderer::%color-name-to-cell-color "default")))
  (is (null (cl-tmux/renderer::%color-name-to-cell-color "")))
  (is (null (cl-tmux/renderer::%color-name-to-cell-color nil))))

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
      (let ((out (with-output-to-string (s) (cl-tmux/renderer::render-pane s pane))))
        (is (not (search "48;5;52" out))
            "default render must not contain the window-style bg (got ~S)" out))
      ;; Opt in: window-style recolours the default-bg cells.
      (cl-tmux/options:set-option "window-style" "bg=colour52")
      (let ((out (with-output-to-string (s) (cl-tmux/renderer::render-pane s pane))))
        (is (search "48;5;52" out)
            "pane must emit bg colour52 (48;5;52) for default cells (got ~S)" out)))))

(test render-pane-window-style-preserves-explicit-colors
  "window-style recolours only default cells: a cell with an explicit non-default
   background keeps it (the bg=0 → style substitution is guarded on the default)."
  (with-isolated-config
    (let* ((sess (make-renderer-test-session 4 1))
           (pane (first (window-panes (session-active-window sess))))
           (screen (pane-screen pane)))
      ;; Paint one cell with an explicit bg=colour200 (SGR 48;5;200) then text.
      (feed screen (esc "[48;5;200mX"))
      (cl-tmux/options:set-option "window-style" "bg=colour52")
      (let ((out (with-output-to-string (s) (cl-tmux/renderer::render-pane s pane))))
        (is (search "48;5;200" out)
            "an explicit bg must survive window-style recolour (got ~S)" out)))))

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

(test border-indicators-colour-p-honours-option
  "%border-indicators-colour-p is true unless pane-border-indicators is \"off\"."
  (with-isolated-options ("pane-border-indicators" "off")
    (is (not (cl-tmux/renderer::%border-indicators-colour-p)) "off → no colour"))
  (with-isolated-options ("pane-border-indicators" "colour")
    (is-true (cl-tmux/renderer::%border-indicators-colour-p) "colour → colour"))
  (with-isolated-options ("pane-border-indicators" "both")
    (is-true (cl-tmux/renderer::%border-indicators-colour-p) "both → colour"))
  (with-isolated-options ("pane-border-indicators" "arrows")
    (is-true (cl-tmux/renderer::%border-indicators-colour-p)
             "arrows → colour (glyphs not drawn, degrades to colour)")))

(test pane-border-indicators-off-suppresses-active-colour
  "pane-border-indicators \"off\" suppresses the active-pane border colour; the
   default (\"colour\") keeps it (pane-active-border-style fg=green → SGR 32)."
  (let* ((l0   (tl-leaf 1 1 1))
         (l1   (tl-leaf 2 1 1))
         (tree (make-layout-split :h l0 l1)))
    (cl-tmux/model::layout-assign tree 0 0 81 24)
    (let ((ap (layout-leaf-pane l0)))
      (flet ((render ()
               (with-output-to-string (buf)
                 (cl-tmux/renderer::render-tree-borders buf tree ap 81))))
        (with-isolated-options ("pane-border-indicators" "colour"
                                "pane-active-border-style" "fg=green")
          (is (search (format nil "~C[32m" #\Escape) (render))
              "default indicators must colour the active border green (32)"))
        (with-isolated-options ("pane-border-indicators" "off"
                                "pane-active-border-style" "fg=green")
          (is (null (search (format nil "~C[32m" #\Escape) (render)))
              "indicators off must NOT colour the active border"))))))

(test pane-border-chars-follow-pane-border-lines
  "%pane-border-chars selects glyph pairs by pane-border-lines; unknown/number
   fall back to single."
  (with-isolated-config
    (flet ((chars () (multiple-value-list (cl-tmux/renderer::%pane-border-chars))))
      (cl-tmux/options:set-option "pane-border-lines" "single")
      (is (equal '(#\│ #\─) (chars)))
      (cl-tmux/options:set-option "pane-border-lines" "double")
      (is (equal '(#\║ #\═) (chars)))
      (cl-tmux/options:set-option "pane-border-lines" "heavy")
      (is (equal '(#\┃ #\━) (chars)))
      (cl-tmux/options:set-option "pane-border-lines" "simple")
      (is (equal '(#\| #\-) (chars)))
      (cl-tmux/options:set-option "pane-border-lines" "number")
      (is (equal '(#\│ #\─) (chars)) "number/unknown falls back to single"))))

(test render-tree-borders-honours-pane-border-lines-double
  "With pane-border-lines double, the vertical separator uses ║ not │."
  (with-isolated-config
    (cl-tmux/options:set-option "pane-border-lines" "double")
    (let* ((l0   (tl-leaf 1 1 1))
           (l1   (tl-leaf 2 1 1))
           (tree (make-layout-split :h l0 l1)))
      (cl-tmux/model::layout-assign tree 0 0 81 24)
      (let* ((ap  (layout-leaf-pane l0))
             (buf (make-string-output-stream)))
        (cl-tmux/renderer::render-tree-borders buf tree ap 81)
        (let ((out (get-output-stream-string buf)))
          (is (find #\║ out) "double border must draw ║ (got ~S)" out)
          (is (not (find #\│ out)) "double border must not draw the single-line │"))))))
