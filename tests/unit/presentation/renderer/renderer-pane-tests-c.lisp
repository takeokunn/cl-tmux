(in-package #:cl-tmux/test)

;;;; renderer-pane tests — part C: %apply-border-style branch coverage,
;;;; draw-clock, render-pane-clock-mode, draw-pane-number, in-sel-branch.

(describe "renderer-suite"

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

  ;; NIL and "default" styles emit only the reset SGR (ESC[0m).
  (it "apply-border-style-resets-table"
    (dolist (c '((nil       "nil style")
                 ("default" "\"default\" style")))
      (destructuring-bind (style desc) c
        (declare (ignore desc))
        (let ((out (%border-style-output style)))
          (expect (search (format nil "~C[0m" #\Escape) out))))))

  ;; pane-border-style and pane-active-border-style are read directly from global options.
  (it "pane-border-style-applied-directly"
    (with-isolated-options ("pane-border-style" "fg=red"
                            "pane-active-border-style" "fg=green,bg=black")
      (let ((normal (cl-tmux/options:get-option "pane-border-style" ""))
            (active (cl-tmux/options:get-option "pane-active-border-style" "")))
        (expect (search "fg=red" normal))
        (expect (search "fg=green" active))
        (expect (search "bg=black" active)))))

  ;; mode-style is read directly from the global option (no deprecated-option fold-in).
  (it "mode-style-applied-directly"
    (with-isolated-options ("mode-style" "fg=black,bg=yellow,bold")
      (let ((eff (cl-tmux/options:get-option "mode-style" "")))
        (expect (search "fg=black" eff))
        (expect (search "bg=yellow" eff))
        (expect (search "bold" eff)))))

  ;; fg=COLOR style emits the named colour's SGR code.
  (it "apply-border-style-fg-table"
    (dolist (c '(("fg=green"  "~C[32m" "green → ESC[32m")
                 ("fg=red"    "~C[31m" "red → ESC[31m")
                 ("fg=blue"   "~C[34m" "blue → ESC[34m")
                 ("fg=yellow" "~C[33m" "yellow → ESC[33m")))
      (destructuring-bind (style fmt desc) c
        (declare (ignore desc))
        (let ((out (%border-style-output style)))
          (expect (search (format nil fmt #\Escape) out))))))

  ;; An unrecognised non-fg= style falls through to the reset-attrs fallback.
  (it "apply-border-style-unknown-falls-back-to-reset"
    (let ((out (%border-style-output "bold")))
      (expect (search (format nil "~C[0m" #\Escape) out))))

  ;;; -- draw-clock-to-screen branch ---------------------------------------------

  ;; draw-clock-to-screen produces output containing block characters for a
  ;; pane that is wide and tall enough.
  (it "draw-clock-to-screen-emits-digits"
    (let ((out (with-output-to-string (s)
                 (cl-tmux/renderer::draw-clock-to-screen s 0 0 20 6))))
      (expect (plusp (length out)))
      (expect (find #\█ out))))

  ;; draw-clock-to-screen produces no output when the pane is too narrow (< 13 cols).
  (it "draw-clock-to-screen-too-small-emits-nothing"
    (let ((out (with-output-to-string (s)
                 (cl-tmux/renderer::draw-clock-to-screen s 0 0 5 3))))
      (expect (string= "" out))))

  ;; When *clock-mode-pane-id* matches the pane id, render-pane draws the clock overlay.
  (it "render-pane-clock-mode-overlay"
    (with-copy-mode-render-fixture (sess pane screen 20 6)
      (declare (ignore screen))
      (let ((cl-tmux::*clock-mode-pane-id* (pane-id pane)))
        (let ((out (render-pane-output sess pane)))
          (expect (find #\█ out))))))

  ;; When *clock-mode-pane-id* does not match the pane id, the clock overlay is suppressed.
  (it "render-pane-no-clock-when-id-mismatch"
    (with-copy-mode-render-fixture (sess pane screen 20 6)
      (declare (ignore screen))
      (let ((cl-tmux::*clock-mode-pane-id* 99))
        (let ((out (render-pane-output sess pane)))
          (expect (null (find #\█ out)))))))

  ;; When copy mode is active, render-pane draws the copy-mode position banner.
  (it "render-pane-copy-mode-position-overlay"
    (with-copy-mode-render-fixture (sess pane screen 20 6
                                    :position-format "COPY-BANNER")
      (setf (screen-copy-mode-p screen) t)
      (let ((out (render-pane-output sess pane)))
        (expect (search "COPY-BANNER" out)))))

  ;; copy-mode -H (screen-copy-hide-position) suppresses the position banner
  ;; even with a non-empty copy-mode-position-format — previously only
  ;; tested at the dispatch layer (does -H set the flag), never at the
  ;; renderer layer (does the renderer actually honour it).
  (it "render-pane-copy-mode-position-overlay-suppressed-when-hidden"
    (with-copy-mode-render-fixture (sess pane screen 20 6
                                    :position-format "COPY-BANNER")
      (setf (screen-copy-mode-p screen) t
            (cl-tmux/terminal/types:screen-copy-hide-position screen) t)
      (let ((out (render-pane-output sess pane)))
        (expect (null (search "COPY-BANNER" out))))))

  ;;; -- copy-mode line numbers --------------------------------------------------

  (defun %strip-csi-sequences (out)
    "Remove CSI escape sequences from OUT so the visible pane text can be compared."
    (cl-ppcre:regex-replace-all (format nil "~C\\[[0-9;?]*[A-Za-z]" #\Escape)
                                out
                                ""))

  ;; copy-mode-line-numbers off leaves the pane content unchanged.
  (it "copy-mode-line-numbers-off-suppresses-gutter"
    (with-copy-mode-render-fixture (sess pane screen 4 2
                                    :content "ABCDEFGH"
                                    :options '("copy-mode-line-numbers" "off"))
      (setf (screen-copy-mode-p screen) t
            (screen-copy-cursor screen) (cons 1 0))
      (let ((vis (%strip-csi-sequences (render-pane-output sess pane))))
        (expect (string= "ABCDEFGH" vis)))))

  ;; relative copy-mode line numbers render a gutter with the cursor row at 0.
  (it "copy-mode-line-numbers-relative-renders-gutter"
    (with-copy-mode-render-fixture (sess pane screen 4 2
                                    :content "ABCDEFGH"
                                    :options '("copy-mode-line-numbers" "relative"))
      (setf (screen-copy-mode-p screen) t
            (screen-copy-cursor screen) (cons 1 0))
      (let ((vis (%strip-csi-sequences (render-pane-output sess pane))))
        (expect (string= " 1AB 0EF" vis)))))

  ;; default copy-mode line numbers use the viewport row index.
  (it "copy-mode-line-numbers-default-renders-zero-based-gutter"
    (with-copy-mode-render-fixture (sess pane screen 4 2
                                    :content "ABCDEFGH"
                                    :options '("copy-mode-line-numbers" "default"))
      (setf (screen-copy-mode-p screen) t
            (screen-copy-cursor screen) (cons 0 0))
      (let ((vis (%strip-csi-sequences (render-pane-output sess pane))))
        (expect (string= " 0AB 1EF" vis)))))

  ;; absolute copy-mode line numbers use the absolute pane history index.
  (it "copy-mode-line-numbers-absolute-renders-one-based-gutter"
    (with-copy-mode-render-fixture (sess pane screen 4 2
                                    :content "ABCDEFGH"
                                    :options '("copy-mode-line-numbers" "absolute"))
      (setf (screen-copy-mode-p screen) t
            (screen-copy-cursor screen) (cons 0 0))
      (let ((vis (%strip-csi-sequences (render-pane-output sess pane))))
        (expect (string= " 1AB 2EF" vis)))))

  ;; hybrid copy-mode line numbers render absolute numbering on the cursor row and relative elsewhere.
  (it "copy-mode-line-numbers-hybrid-renders-absolute-on-cursor-row"
    (with-copy-mode-render-fixture (sess pane screen 4 2
                                    :content "ABCDEFGH"
                                    :options '("copy-mode-line-numbers" "hybrid"))
      (setf (screen-copy-mode-p screen) t
            (screen-copy-cursor screen) (cons 0 0))
      (let ((vis (%strip-csi-sequences (render-pane-output sess pane))))
        (expect (string= " 1AB 1EF" vis)))))

  ;; copy-mode-entered-by-mouse-p suppresses line numbers, matching tmux mouse enter behaviour.
  (it "copy-mode-line-numbers-mouse-enter-suppresses-gutter"
    (with-copy-mode-render-fixture (sess pane screen 4 2
                                    :content "ABCDEFGH"
                                    :options '("copy-mode-line-numbers" "absolute"))
      (setf (screen-copy-mode-p screen) t
            (cl-tmux/terminal/types:screen-copy-mode-entered-by-mouse-p screen) t)
      (let ((vis (%strip-csi-sequences (render-pane-output sess pane))))
        (expect (string= "ABCDEFGH" vis)))))

  ;; copy-mode-current-line-number-style overrides the base line-number style on the cursor row.
  (it "copy-mode-current-line-number-style-applies-to-cursor-row"
    (with-copy-mode-render-fixture (sess pane screen 4 2
                                    :content "ABCDEFGH"
                                    :options '("copy-mode-line-numbers" "absolute"
                                               "copy-mode-line-number-style" "fg=green"
                                               "copy-mode-current-line-number-style" "fg=red"))
      (setf (screen-copy-mode-p screen) t
            (screen-copy-cursor screen) (cons 1 0))
      (let ((out (render-pane-output sess pane)))
        (expect (= 1 (%count-substring (format nil "~C[32m" #\Escape) out)))
        (expect (= 1 (%count-substring (format nil "~C[31m" #\Escape) out))))))

  ;;; -- clock-mode-style (12/24h) and clock-mode-colour -------------------------

  ;; clock-mode-style 24 (the default) leaves the hour unchanged.
  (it "clock-display-hour-24-hour-default"
    (with-isolated-options ()
      (expect (= 13 (cl-tmux/renderer::%clock-display-hour 13)))
      (expect (= 0  (cl-tmux/renderer::%clock-display-hour 0)))))

  ;; clock-mode-style 12 converts to a 12-hour clock (0→12, 13→1, 12→12, 23→11).
  (it "clock-display-hour-12-hour"
    (with-isolated-options ("clock-mode-style" 12)
      (expect (= 12 (cl-tmux/renderer::%clock-display-hour 0)))
      (expect (= 1  (cl-tmux/renderer::%clock-display-hour 13)))
      (expect (= 12 (cl-tmux/renderer::%clock-display-hour 12)))
      (expect (= 11 (cl-tmux/renderer::%clock-display-hour 23)))))

  ;; clock-mode-colour maps to its foreground SGR code; an unknown name falls back
  ;; to bright cyan (96).
  (it "clock-face-sgr-from-colour-option"
    (dolist (c '(("red"          "31" "red -> 31")
                 ("green"        "32" "green -> 32")
                 ("bogus-colour" "96" "unknown -> 96 fallback")))
      (destructuring-bind (colour expected desc) c
        (declare (ignore desc))
        (with-isolated-options ("clock-mode-colour" colour)
          (expect (string= expected (cl-tmux/renderer::%clock-face-sgr)))))))

  ;;; -- display-panes per-pane big numbers (C-b q) ------------------------------

  ;; %draw-pane-number-to-screen emits block-element digits for a pane number.
  (it "draw-pane-number-emits-big-digits"
    (let ((out (with-output-to-string (s)
                 (cl-tmux/renderer::%draw-pane-number-to-screen s 0 0 20 6 7 nil))))
      (expect (find #\█ out))))

  ;; %draw-pane-number-to-screen colours the active pane with display-panes-active-
  ;; colour and others with display-panes-colour.
  (it "draw-pane-number-active-vs-inactive-colour"
    (with-isolated-options ("display-panes-colour" "green"
                            "display-panes-active-colour" "red")
      (let ((inactive (with-output-to-string (s)
                        (cl-tmux/renderer::%draw-pane-number-to-screen s 0 0 20 6 1 nil)))
            (active   (with-output-to-string (s)
                        (cl-tmux/renderer::%draw-pane-number-to-screen s 0 0 20 6 1 t))))
        (expect (search (format nil "~C[32m" #\Escape) inactive))
        (expect (search (format nil "~C[31m" #\Escape) active)))))

  ;; %draw-pane-number-to-screen renders nothing in a pane smaller than 3x3.
  (it "draw-pane-number-too-small-emits-nothing"
    (expect (string= "" (with-output-to-string (s)
                      (cl-tmux/renderer::%draw-pane-number-to-screen s 0 0 2 2 1 nil)))))

  ;;; -- in-sel branch coverage via render-pane ----------------------------------

  (defun %reverse-video-p (out)
    "True when OUT contains the SGR reverse-video code (;7)."
    (not (null (search ";7" out))))

  (defun %count-substring (needle haystack)
    "Count non-overlapping occurrences of NEEDLE in HAYSTACK."
    (loop with start = 0
          for pos = (search needle haystack :start2 start)
          while pos
          do (setf start (+ pos (length needle)))
          count 1))

  ;; When copy-selecting is NIL the sel-active gate is false.
  (it "in-sel-branch-not-selecting"
    (with-isolated-options ("copy-mode-position-style" "default"
                            "copy-mode-position-format" "")
      (with-copy-mode-selection-fixture (sess pane screen 8 4
                                            :content "ABCDEFGH"
                                            :copy-mode-p nil
                                            :selecting-p nil)
        (let ((baseline (render-pane-output sess pane)))
          (setf (screen-copy-selecting screen) nil
                (screen-copy-mark screen) nil
                (screen-copy-cursor screen) nil)
          (let ((out (render-pane-output sess pane)))
            (expect (string= baseline out)))))))

  ;; Single-row selection: only cells in [sel-start-c, sel-end-c) are highlighted.
  (it "in-sel-branch-single-row"
    (with-copy-mode-selection-fixture (sess pane screen 8 4
                                          :content "ABCDEFGHIJKLMNOPQRSTUVWXYZ01234567"
                                          :mark-row 0
                                          :mark-col 2
                                          :cursor-row 0
                                          :cursor-col 5)
      (let ((out (render-pane-output sess pane)))
        (expect (%reverse-video-p out)))))

  ;; copy-mode-selection-style recolours selected cells, and mode-style no longer feeds selection highlighting.
  (it "copy-mode-selection-style-drives-selection-colour"
    (with-isolated-config
      (with-isolated-options ("mode-style" "bg=colour99"
                              "copy-mode-selection-style" "bg=colour172"
                              "copy-mode-position-style" "default"
                              "copy-mode-position-format" ""
                              "copy-mode-mark-style" "default")
        (with-copy-mode-selection-fixture (sess pane screen 8 4
                                              :content "ABCDEFGHIJKLMNOP"
                                              :mark-row 0
                                              :mark-col 2
                                              :cursor-row 0
                                              :cursor-col 5)
          (let ((out (render-pane-output sess pane)))
            (expect (search "48;5;172" out))
            (expect (null (search "48;5;99" out))))))))

  ;; copy-mode-mark-style recolours the marked cell only, and the marked endpoint still flips to reverse-video.
  (it "copy-mode-mark-style-applies-mark-endpoint-style"
    (with-isolated-config
      (with-isolated-options ("copy-mode-position-style" "default"
                              "copy-mode-position-format" ""
                              "copy-mode-mark-style" "fg=colour88,bg=colour172")
        (with-copy-mode-selection-fixture (sess pane screen 8 4
                                              :content "ABCDEFGHIJKLMNOP"
                                              :cursor-row 1
                                              :cursor-col 3
                                              :selecting-p nil)
          (cl-tmux/commands::copy-mode-set-mark screen)
          (let ((out (render-pane-output sess pane)))
            (expect (= 1 (%count-substring "38;5;88" out)))
            (expect (= 1 (%count-substring "48;5;172" out)))
            (expect (%reverse-video-p out)))))))

  ;; First row of a multi-row selection: cols >= sel-start-c are highlighted.
  (it "in-sel-branch-first-row"
    (with-copy-mode-selection-fixture (sess pane screen 8 4
                                          :content "ABCDEFGHIJKLMNOPQRSTUVWXYZ01234567"
                                          :mark-row 0
                                          :mark-col 3
                                          :cursor-row 2
                                          :cursor-col 0)
      (let ((out (render-pane-output sess pane)))
        (expect (%reverse-video-p out)))))

  ;; Last row of a multi-row selection: cols < sel-end-c are highlighted.
  (it "in-sel-branch-last-row"
    (with-copy-mode-selection-fixture (sess pane screen 8 4
                                          :content "ABCDEFGHIJKLMNOPQRSTUVWXYZ01234567"
                                          :mark-row 0
                                          :mark-col 0
                                          :cursor-row 2
                                          :cursor-col 5)
      (let ((out (render-pane-output sess pane)))
        (expect (%reverse-video-p out)))))

  ;; Middle rows of a multi-row selection are fully highlighted.
  (it "in-sel-branch-middle-row"
    (with-copy-mode-selection-fixture (sess pane screen 8 4
                                          :content "ABCDEFGHIJKLMNOPQRSTUVWXYZ01234567"
                                          :mark-row 0
                                          :mark-col 0
                                          :cursor-row 3
                                          :cursor-col 0)
      (let ((out (render-pane-output sess pane)))
        (expect (%reverse-video-p out)))))

  ;; When copy-selecting is T but mark is NIL, sel-active is false.
  (it "in-sel-branch-selecting-but-no-mark"
    (with-isolated-options ("copy-mode-position-style" "default"
                            "copy-mode-position-format" "")
      (let* ((sess   (make-renderer-test-session 8 4 :content "ABCDEFGH"))
             (pane   (first (window-panes (session-active-window sess))))
             (screen (pane-screen pane)))
        (let ((baseline (render-pane-output sess pane)))
        (setf (screen-copy-mode-p    screen) t
              (screen-copy-selecting screen) t
              (screen-copy-mark      screen) nil
              (screen-copy-cursor    screen) (cons 0 3))
          (let ((out (render-pane-output sess pane)))
            (expect (string= baseline out))))))))
