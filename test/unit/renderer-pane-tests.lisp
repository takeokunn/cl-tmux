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

(test pane-border-style-applied-directly
  "pane-border-style and pane-active-border-style are read directly from global options."
  (with-isolated-options ("pane-border-style" "fg=red"
                          "pane-active-border-style" "fg=green,bg=black")
    (let ((normal (cl-tmux/options:get-option "pane-border-style" ""))
          (active (cl-tmux/options:get-option "pane-active-border-style" "")))
      (is (search "fg=red" normal)
          "pane-border-style fg=red (got ~S)" normal)
      (is (search "fg=green" active) "pane-active-border-style fg=green (got ~S)" active)
      (is (search "bg=black" active) "pane-active-border-style bg=black (got ~S)" active))))

(test mode-style-applied-directly
  "mode-style is read directly from the global option (no deprecated-option fold-in)."
  (with-isolated-options ("mode-style" "fg=black,bg=yellow,bold")
    (let ((eff (cl-tmux/options:get-option "mode-style" "")))
      (is (search "fg=black" eff)  "fg=black in mode-style (got ~S)" eff)
      (is (search "bg=yellow" eff) "bg=yellow in mode-style (got ~S)" eff)
      (is (search "bold" eff)      "bold in mode-style (got ~S)" eff))))

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
  (let* ((pane   (make-test-pane 20 6 :id 42))
         (cl-tmux::*clock-mode-pane-id* 42))
    (let ((out (with-output-to-string (s)
                 (cl-tmux/renderer::render-pane s pane))))
      (is (find #\█ out)
          "render-pane in clock mode must emit block-element digits (got ~S)" out))))

(test render-pane-no-clock-when-id-mismatch
  "When *clock-mode-pane-id* does not match the pane id, the clock overlay is suppressed."
  (let* ((pane   (make-test-pane 20 6 :id 1))
         (cl-tmux::*clock-mode-pane-id* 99))
    (let ((out (with-output-to-string (s)
                 (cl-tmux/renderer::render-pane s pane))))
      (is (null (find #\█ out))
          "render-pane without matching clock-mode id must not emit clock digits (got ~S)"
          out))))

;;; -- clock-mode-style (12/24h) and clock-mode-colour -------------------------

(test clock-display-hour-24-hour-default
  "clock-mode-style 24 (the default) leaves the hour unchanged."
  (with-isolated-options ()
    (is (= 13 (cl-tmux/renderer::%clock-display-hour 13)) "13:00 stays 13 in 24h")
    (is (= 0  (cl-tmux/renderer::%clock-display-hour 0))  "midnight stays 0 in 24h")))

(test clock-display-hour-12-hour
  "clock-mode-style 12 converts to a 12-hour clock (0→12, 13→1, 12→12, 23→11)."
  (with-isolated-options ("clock-mode-style" 12)
    (is (= 12 (cl-tmux/renderer::%clock-display-hour 0))  "midnight → 12")
    (is (= 1  (cl-tmux/renderer::%clock-display-hour 13)) "13:00 → 1")
    (is (= 12 (cl-tmux/renderer::%clock-display-hour 12)) "noon → 12")
    (is (= 11 (cl-tmux/renderer::%clock-display-hour 23)) "23:00 → 11")))

(test clock-face-sgr-from-colour-option
  "clock-mode-colour maps to its foreground SGR code; an unknown name falls back
   to bright cyan (96)."
  (with-isolated-options ("clock-mode-colour" "red")
    (is (string= "31" (cl-tmux/renderer::%clock-face-sgr)) "red → 31"))
  (with-isolated-options ("clock-mode-colour" "green")
    (is (string= "32" (cl-tmux/renderer::%clock-face-sgr)) "green → 32"))
  (with-isolated-options ("clock-mode-colour" "bogus-colour")
    (is (string= "96" (cl-tmux/renderer::%clock-face-sgr)) "unknown → 96 fallback")))

;;; -- display-panes per-pane big numbers (C-b q) ------------------------------

(test draw-pane-number-emits-big-digits
  "%draw-pane-number-to-screen emits block-element digits for a pane number."
  (let ((out (with-output-to-string (s)
               (cl-tmux/renderer::%draw-pane-number-to-screen s 0 0 20 6 7 nil))))
    (is (find #\█ out) "must emit big-digit block glyphs (got ~S)" out)))

(test draw-pane-number-active-vs-inactive-colour
  "%draw-pane-number-to-screen colours the active pane with display-panes-active-
   colour and others with display-panes-colour."
  (with-isolated-options ("display-panes-colour" "green"
                          "display-panes-active-colour" "red")
    (let ((inactive (with-output-to-string (s)
                      (cl-tmux/renderer::%draw-pane-number-to-screen s 0 0 20 6 1 nil)))
          (active   (with-output-to-string (s)
                      (cl-tmux/renderer::%draw-pane-number-to-screen s 0 0 20 6 1 t))))
      (is (search (format nil "~C[32m" #\Escape) inactive)
          "inactive pane number uses display-panes-colour green (32)")
      (is (search (format nil "~C[31m" #\Escape) active)
          "active pane number uses display-panes-active-colour red (31)"))))

(test draw-pane-number-too-small-emits-nothing
  "%draw-pane-number-to-screen renders nothing in a pane smaller than 3x3."
  (is (string= "" (with-output-to-string (s)
                    (cl-tmux/renderer::%draw-pane-number-to-screen s 0 0 2 2 1 nil)))
      "a 2x2 pane is too small for a big digit"))

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
  (let* ((pane   (make-test-pane 8 4 :content "ABCDEFGH"))
         (screen (pane-screen pane)))
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

(test mode-style-default-reverse-keeps-reverse-video-selection
  "With the default mode-style (reverse), a selection is still drawn with the
   reverse-video SGR — the colour path stays opt-in."
  (with-isolated-config
    (let ((pane (%make-selecting-pane 8 4 "ABCDEFGHIJKLMNOP" 0 2 0 5)))
      (cl-tmux/options:set-option "mode-style" "reverse")
      (let ((out (%render-pane-string pane)))
        (is (%reverse-video-p out)
            "default mode-style must keep reverse-video selection (got ~S)" out)))))

(test mode-style-colour-recolours-selection-without-reverse
  "A colour-based mode-style highlights the selection with its bg instead of
   reverse-video: bg=colour172 → 48;5;172 appears, the ;7 reverse code does not."
  (with-isolated-config
    (let ((pane (%make-selecting-pane 8 4 "ABCDEFGHIJKLMNOP" 0 2 0 5)))
      (cl-tmux/options:set-option "mode-style" "bg=colour172")
      (let ((out (%render-pane-string pane)))
        (is (search "48;5;172" out)
            "colour mode-style must emit bg colour172 on the selection (got ~S)" out)
        (is (null (%reverse-video-p out))
            "colour mode-style must NOT also reverse-video the selection (got ~S)" out)))))

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
  (let* ((pane   (make-test-pane 8 4 :content "ABCDEFGH"))
         (screen (pane-screen pane)))
    (setf (screen-copy-mode-p    screen) t
          (screen-copy-selecting screen) t
          (screen-copy-mark      screen) nil
          (screen-copy-cursor    screen) (cons 0 3))
    (let ((out (%render-pane-string pane)))
      (is (null (%reverse-video-p out))
          "nil mark must suppress reverse-video (got ~S)" out))))

