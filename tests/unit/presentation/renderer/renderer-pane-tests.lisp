(in-package #:cl-tmux/test)

;;;; Pane and border rendering tests.
;;;;
;;;; Covers: render-pane, layout-subtree-rect, subtree-contains-p,
;;;;         render-tree-borders, %apply-border-style, draw-clock-to-screen
;;;;         from src/renderer-pane.lisp.
;;;;
;;;; renderer-suite is declared in renderer-format-tests.lisp (loaded first).

;;; -- Local fixture ----------------------------------------------------------
;;;
;;; make-renderer-test-session (defined in tests/helpers-renderer-fixtures.lisp) is the canonical
;;; shared fixture.  The old local %make-pane-test-session has been removed in
;;; favour of the shared version.

(defun %snippet-around (text needle &optional (radius 24))
  (let ((pos (position needle text)))
    (and pos
         (subseq text pos (min (length text) (+ pos radius))))))

(describe "renderer-suite"

  ;; -- render-pane (content + positioning) ------------------------------------

  ;; render-pane emits the pane's cell glyphs preceded by a cursor-position sequence for row 0.
  (it "render-pane-content-and-positioning"
    (let* ((sess (make-renderer-test-session 5 2 :content "hi"))
           (pane (first (window-panes (session-active-window sess))))
           (out  (render-pane-output sess pane)))
      (expect (find #\h out))
      (expect (find #\i out))
      (expect (search (format nil "~C[1;1H" #\Escape) out))))

  ;; With DECSCNM (reverse-screen) on, render-pane emits the reverse attribute (SGR 7)
  ;; globally; the rendered output differs from the non-reversed render.
  (it "render-pane-decscnm-reverses-output"
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
              (expect (not (string= normal reversed)))
              (expect (and reversed-snippet (search ";7" reversed-snippet)))
              (expect (and normal-snippet (null (search ";7" normal-snippet))))))))))

  ;; -- double-width glyphs are not double-printed ------------------------------

  ;; A double-width glyph occupying two cells is printed exactly once, not twice.
  (it "render-pane-double-width-not-duplicated"
    (let* ((sess   (make-renderer-test-session 5 2))
           (pane   (first (window-panes (session-active-window sess))))
           (screen (pane-screen pane)))
      (cl-tmux/test::utf8-feed screen "あ")
      (let ((out (render-pane-output sess pane)))
        (expect (= 1 (count #\あ out))))))

  ;; -- OSC 8 hyperlinks re-emitted around their cell span ----------------------

  ;; A cell written under OSC 8 is re-emitted with its hyperlink (set before the
  ;; cell, cleared after) so the outer terminal makes it clickable.
  (it "render-pane-emits-osc-8-hyperlink"
    (let* ((sess   (make-renderer-test-session 10 2))
           (pane   (first (window-panes (session-active-window sess))))
           (screen (pane-screen pane)))
      (feed screen (format nil "~C]8;;https://x~C\\X" #\Escape #\Escape))
      (let ((out (render-pane-output sess pane)))
        (expect (search (format nil "~C]8;;https://x~C\\" #\Escape #\Escape) out))
        (expect (search (format nil "~C]8;;~C\\" #\Escape #\Escape) out)))))

  ;; Plain content (no OSC 8) emits no OSC 8 sequence — existing render output is
  ;; unchanged for the common no-hyperlink case.
  (it "render-pane-no-osc-8-without-hyperlink"
    (let* ((sess   (make-renderer-test-session 10 2))
           (pane   (first (window-panes (session-active-window sess))))
           (screen (pane-screen pane)))
      (feed screen "plain")
      (let ((out (render-pane-output sess pane)))
        (expect (null (search (format nil "~C]8;" #\Escape) out))))))

  ;; -- window-style / window-active-style (pane background recolour) -----------

  ;; %color-name-to-cell-color converts to the cell colour encoding; default/empty
  ;; yields NIL (no override).
  (it "color-name-to-cell-color-maps-names-palette-and-truecolor"
    (dolist (c '(("red" 1) ("brightred" 9) ("colour235" 235)
                 ("default" nil) ("" nil) (nil nil)))
      (destructuring-bind (name expected) c
        (expect (eql expected (cl-tmux/renderer::%color-name-to-cell-color name)))))
    (expect (= (logior #x1000000 #xff8800)
               (cl-tmux/renderer::%color-name-to-cell-color "#ff8800"))))

  ;; %window-style-default-colors returns the fg/bg cell numbers a style sets, NIL
  ;; for ones it omits, and (NIL NIL) for an empty style.
  (it "window-style-default-colors-extracts-fg-bg"
    (multiple-value-bind (fg bg)
        (cl-tmux/renderer::%window-style-default-colors "fg=red,bg=colour235")
      (expect (= 1 fg)) (expect (= 235 bg)))
    (multiple-value-bind (fg bg)
        (cl-tmux/renderer::%window-style-default-colors "bg=colour52")
      (expect (null fg)) (expect (= 52 bg)))
    (multiple-value-bind (fg bg)
        (cl-tmux/renderer::%window-style-default-colors "")
      (expect (null fg)) (expect (null bg))))

  ;; With window-style set, a pane's default-bg (0) cells render with the style's
  ;; background SGR; unset, they do not — verifying the opt-in recolour.
  (it "render-pane-applies-window-style-to-default-cells"
    (with-isolated-config
      (let* ((sess (make-renderer-test-session 5 2 :content "hi"))
             (pane (first (window-panes (session-active-window sess)))))
        ;; Baseline: no window-style → no colour-52 background emitted.
        (let ((out (render-pane-output sess pane)))
          (expect (not (search "48;5;52" out))))
        ;; Opt in: window-style recolours the default-bg cells.
        (cl-tmux/options:set-option "window-style" "bg=colour52")
        (let ((out (render-pane-output sess pane)))
          (expect (search "48;5;52" out))))))

  ;; %pane-cell-base-colors only substitutes the pane defaults: an explicit
  ;; non-default background survives unchanged.
  (it "pane-cell-base-colors-preserves-explicit-background"
    (let ((cell (cl-tmux/terminal/types:make-cell :char #\X :fg cl-tmux/terminal/types:+default-color+ :bg 200)))
      (multiple-value-bind (fg bg)
          (cl-tmux/renderer::%pane-cell-base-colors cell 31 52)
        (expect (= 31 fg))
        (expect (= 200 bg)))))

  ;; %pane-cell-base-colors substitutes the pane defaults ONLY for cells whose
  ;; colour is the +default-color+ sentinel; explicit white(7)/black(0) survive.
  (it "pane-cell-base-colors-recolours-only-default-sentinel"
    ;; Default-sentinel cell: both fg and bg get the pane defaults.
    (let ((cell (cl-tmux/terminal/types:make-cell
                 :char #\X
                 :fg cl-tmux/terminal/types:+default-color+
                 :bg cl-tmux/terminal/types:+default-color+)))
      (multiple-value-bind (fg bg)
          (cl-tmux/renderer::%pane-cell-base-colors cell 31 52)
        (expect (= 31 fg))
        (expect (= 52 bg))))
    ;; Explicit white(7)/black(0): NOT recoloured (this is the gap fix).
    (let ((cell (cl-tmux/terminal/types:make-cell :char #\X :fg 7 :bg 0)))
      (multiple-value-bind (fg bg)
          (cl-tmux/renderer::%pane-cell-base-colors cell 31 52)
        (expect (= 7 fg))
        (expect (= 0 bg)))))

  ;; -- layout-subtree-rect and subtree-contains-p ------------------------------

  ;; layout-subtree-rect returns the tight bounding box of all leaves.
  (it "layout-subtree-rect-bounding-box"
    (let* ((l0   (tl-leaf 1 1 1))
           (l1   (tl-leaf 2 1 1))
           (tree (make-layout-split :h l0 l1)))
      (cl-tmux/model::layout-assign tree 0 0 81 24)
      (let ((rect (cl-tmux/renderer::layout-subtree-rect tree)))
        (expect (= 0  (getf rect :x)))
        (expect (= 0  (getf rect :y)))
        (expect (= 81 (getf rect :w)))
        (expect (= 24 (getf rect :h))))))

  ;; subtree-contains-p returns T for panes in the subtree and NIL otherwise.
  (it "subtree-contains-p-detects-membership"
    (let* ((l0 (tl-leaf 1 1 1))
           (l1 (tl-leaf 2 1 1))
           (tree (make-layout-split :h l0 l1))
           (p0  (layout-leaf-pane l0))
           (p1  (layout-leaf-pane l1))
           (p-other (make-pane :id 99 :fd -1 :pid -1 :screen (make-screen 1 1))))
      (expect (cl-tmux/renderer::subtree-contains-p tree p0) :to-be-truthy)
      (expect (cl-tmux/renderer::subtree-contains-p tree p1) :to-be-truthy)
      (expect (cl-tmux/renderer::subtree-contains-p tree p-other) :to-be-falsy)
      (expect (cl-tmux/renderer::subtree-contains-p tree nil) :to-be-falsy)))

  ;; -- render-tree-borders -----------------------------------------------------

  ;; render-tree-borders draws vertical-bar separators for a :h split.
  (it "render-tree-borders-draws-vertical-bar"
    (let* ((l0   (tl-leaf 1 1 1))
           (l1   (tl-leaf 2 1 1))
           (tree (make-layout-split :h l0 l1)))
      (cl-tmux/model::layout-assign tree 0 0 81 24)
      (let ((out (render-tree-borders-output tree (layout-leaf-pane l0) 81)))
        (expect (plusp (length out)))
        (expect (find #\│ out)))))

  ;; %border-indicators-colour-p follows tmux: colour for colour/both; arrows is
  ;; arrows-only (no colour); off disables everything.
  (it "border-indicators-colour-p-honours-option"
    (with-isolated-options ("pane-border-indicators" "off")
      (expect (not (cl-tmux/renderer::%border-indicators-colour-p)))
      (expect (not (cl-tmux/renderer::%border-indicators-arrows-p))))
    (with-isolated-options ("pane-border-indicators" "colour")
      (expect (cl-tmux/renderer::%border-indicators-colour-p) :to-be-truthy)
      (expect (not (cl-tmux/renderer::%border-indicators-arrows-p))))
    (with-isolated-options ("pane-border-indicators" "both")
      (expect (cl-tmux/renderer::%border-indicators-colour-p) :to-be-truthy)
      (expect (cl-tmux/renderer::%border-indicators-arrows-p) :to-be-truthy))
    (with-isolated-options ("pane-border-indicators" "arrows")
      (expect (not (cl-tmux/renderer::%border-indicators-colour-p)))
      (expect (cl-tmux/renderer::%border-indicators-arrows-p) :to-be-truthy)))

  ;; pane-border-indicators arrows/both draw an arrow glyph on the separator
  ;; pointing at the active pane; colour-only does not.
  (it "border-arrows-drawn-pointing-at-active-pane"
    (let* ((l0   (tl-leaf 1 1 1))
           (l1   (tl-leaf 2 1 1))
           (tree (make-layout-split :h l0 l1)))
      (cl-tmux/model::layout-assign tree 0 0 81 24)
      (let ((ap (layout-leaf-pane l0)))
        (with-isolated-options ("pane-border-indicators" "arrows")
          (expect (find #\← (render-tree-borders-output tree ap 81))))
        (with-isolated-options ("pane-border-indicators" "both")
          (expect (find #\← (render-tree-borders-output tree ap 81))))
        (with-isolated-options ("pane-border-indicators" "colour")
          (expect (not (find #\← (render-tree-borders-output tree ap 81)))))
        (let ((ap2 (layout-leaf-pane l1)))
          (with-isolated-options ("pane-border-indicators" "arrows")
            (expect (find #\→ (render-tree-borders-output tree ap2 81))))))))

  ;; pane-border-lines padded draws blank borders; number writes the adjacent
  ;; pane's number into the border.
  (it "border-lines-padded-and-number"
    (let* ((l0   (tl-leaf 1 1 1))
           (l1   (tl-leaf 2 1 1))
           (tree (make-layout-split :h l0 l1)))
      (cl-tmux/model::layout-assign tree 0 0 81 24)
      (let ((ap (layout-leaf-pane l0)))
        (with-isolated-options ("pane-border-lines" "padded")
          (expect (not (find #\│ (render-tree-borders-output tree ap 81)))))
        (with-isolated-options ("pane-border-lines" "number")
          (let ((out (render-tree-borders-output tree ap 81)))
            (expect (find (char (format nil "~D" (cl-tmux/model:pane-id
                                              (layout-leaf-pane l0)))
                            0)
                      out)))))))

  ;; pane-border-indicators "off" suppresses the active-pane border colour; the
  ;; default ("colour") keeps it (pane-active-border-style fg=green → SGR 32).
  (it "pane-border-indicators-off-suppresses-active-colour"
    (let* ((l0   (tl-leaf 1 1 1))
           (l1   (tl-leaf 2 1 1))
           (tree (make-layout-split :h l0 l1)))
      (cl-tmux/model::layout-assign tree 0 0 81 24)
      (let ((ap (layout-leaf-pane l0)))
        (with-isolated-options ("pane-border-indicators" "colour"
                                "pane-active-border-style" "fg=green")
          (expect (search (format nil "~C[32m" #\Escape)
                          (render-tree-borders-output tree ap 81))))
        (with-isolated-options ("pane-border-indicators" "off"
                                "pane-active-border-style" "fg=green")
          (expect (null (search (format nil "~C[32m" #\Escape)
                                (render-tree-borders-output tree ap 81))))))))

  ;; %pane-border-chars selects glyph pairs by pane-border-lines; unknown/number
  ;; fall back to single.
  (it "pane-border-chars-follow-pane-border-lines"
    (with-isolated-config
      (flet ((chars () (multiple-value-list (cl-tmux/renderer::%pane-border-chars))))
        (dolist (c '(("single" (#\│ #\─))
                     ("double" (#\║ #\═))
                     ("heavy"  (#\┃ #\━))
                     ("simple" (#\| #\-))
                     ("number" (#\│ #\─))))
          (destructuring-bind (style expected) c
            (cl-tmux/options:set-option "pane-border-lines" style)
            (expect (equal expected (chars))))))))

  ;; With pane-border-lines double, the vertical separator uses ║ not │.
  (it "render-tree-borders-honours-pane-border-lines-double"
    (with-isolated-config
      (cl-tmux/options:set-option "pane-border-lines" "double")
      (let* ((l0   (tl-leaf 1 1 1))
             (l1   (tl-leaf 2 1 1))
             (tree (make-layout-split :h l0 l1)))
        (cl-tmux/model::layout-assign tree 0 0 81 24)
        (let ((out (render-tree-borders-output tree (layout-leaf-pane l0) 81)))
          (expect (find #\║ out))
          (expect (not (find #\│ out))))))))
