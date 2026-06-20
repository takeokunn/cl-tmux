(in-package #:cl-tmux/test)

;;;; renderer-pane tests — part C: %apply-border-style branch coverage,
;;;; draw-clock, render-pane-clock-mode, draw-pane-number, in-sel-branch.

(in-suite renderer-suite)

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

(test apply-border-style-resets-table
  "NIL and \"default\" styles emit only the reset SGR (ESC[0m)."
  (dolist (c '((nil       "nil style")
               ("default" "\"default\" style")))
    (destructuring-bind (style desc) c
      (let ((out (%border-style-output style)))
        (is (search (format nil "~C[0m" #\Escape) out)
            "~A must emit ESC[0m (got ~S)" desc out)))))


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

(test apply-border-style-fg-table
  "fg=COLOR style emits the named colour's SGR code."
  (dolist (c '(("fg=green"  "~C[32m" "green → ESC[32m")
               ("fg=red"    "~C[31m" "red → ESC[31m")
               ("fg=blue"   "~C[34m" "blue → ESC[34m")
               ("fg=yellow" "~C[33m" "yellow → ESC[33m")))
    (destructuring-bind (style fmt desc) c
      (let ((out (%border-style-output style)))
        (is (search (format nil fmt #\Escape) out) "~A (got ~S)" desc out)))))

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
  (with-copy-mode-render-fixture (sess pane screen 20 6)
    (let ((cl-tmux::*clock-mode-pane-id* (pane-id pane)))
      (let ((out (render-pane-output sess pane)))
        (is (find #\█ out)
            "render-pane in clock mode must emit block-element digits (got ~S)" out)))))

(test render-pane-no-clock-when-id-mismatch
  "When *clock-mode-pane-id* does not match the pane id, the clock overlay is suppressed."
  (with-copy-mode-render-fixture (sess pane screen 20 6)
    (let ((cl-tmux::*clock-mode-pane-id* 99))
      (let ((out (render-pane-output sess pane)))
        (is (null (find #\█ out))
            "render-pane without matching clock-mode id must not emit clock digits (got ~S)"
            out)))))

(test render-pane-copy-mode-position-overlay
  "When copy mode is active, render-pane draws the copy-mode position banner."
  (with-copy-mode-render-fixture (sess pane screen 20 6
                                  :position-format "COPY-BANNER")
    (setf (screen-copy-mode-p screen) t)
    (let ((out (render-pane-output sess pane)))
      (is (search "COPY-BANNER" out)
          "render-pane in copy mode must emit the copy-mode position banner (got ~S)"
          out))))

;;; -- copy-mode line numbers --------------------------------------------------

(defun %strip-csi-sequences (out)
  "Remove CSI escape sequences from OUT so the visible pane text can be compared."
  (cl-ppcre:regex-replace-all (format nil "~C\\[[0-9;?]*[A-Za-z]" #\Escape)
                              out
                              ""))

(test copy-mode-line-numbers-off-suppresses-gutter
  "copy-mode-line-numbers off leaves the pane content unchanged."
  (with-copy-mode-render-fixture (sess pane screen 4 2
                                  :content "ABCDEFGH"
                                  :options '("copy-mode-line-numbers" "off"))
    (setf (screen-copy-mode-p screen) t
          (screen-copy-cursor screen) (cons 1 0))
    (let ((vis (%strip-csi-sequences (render-pane-output sess pane))))
      (is (string= "ABCDEFGH" vis)
          "copy-mode-line-numbers off must not reserve a gutter (got ~S)" vis))))

(test copy-mode-line-numbers-relative-renders-gutter
  "relative copy-mode line numbers render a gutter with the cursor row at 0."
  (with-copy-mode-render-fixture (sess pane screen 4 2
                                  :content "ABCDEFGH"
                                  :options '("copy-mode-line-numbers" "relative"))
    (setf (screen-copy-mode-p screen) t
          (screen-copy-cursor screen) (cons 1 0))
    (let ((vis (%strip-csi-sequences (render-pane-output sess pane))))
      (is (string= " 1AB 0EF" vis)
          "relative numbering must render the expected gutter labels (got ~S)" vis))))

(test copy-mode-line-numbers-default-renders-zero-based-gutter
  "default copy-mode line numbers use the viewport row index."
  (with-copy-mode-render-fixture (sess pane screen 4 2
                                  :content "ABCDEFGH"
                                  :options '("copy-mode-line-numbers" "default"))
    (setf (screen-copy-mode-p screen) t
          (screen-copy-cursor screen) (cons 0 0))
    (let ((vis (%strip-csi-sequences (render-pane-output sess pane))))
      (is (string= " 0AB 1EF" vis)
          "default numbering must render zero-based viewport rows (got ~S)" vis))))

(test copy-mode-line-numbers-absolute-renders-one-based-gutter
  "absolute copy-mode line numbers use the absolute pane history index."
  (with-copy-mode-render-fixture (sess pane screen 4 2
                                  :content "ABCDEFGH"
                                  :options '("copy-mode-line-numbers" "absolute"))
    (setf (screen-copy-mode-p screen) t
          (screen-copy-cursor screen) (cons 0 0))
    (let ((vis (%strip-csi-sequences (render-pane-output sess pane))))
      (is (string= " 1AB 2EF" vis)
          "absolute numbering must render one-based history rows (got ~S)" vis))))

(test copy-mode-line-numbers-hybrid-renders-absolute-on-cursor-row
  "hybrid copy-mode line numbers render absolute numbering on the cursor row and relative elsewhere."
  (with-copy-mode-render-fixture (sess pane screen 4 2
                                  :content "ABCDEFGH"
                                  :options '("copy-mode-line-numbers" "hybrid"))
    (setf (screen-copy-mode-p screen) t
          (screen-copy-cursor screen) (cons 0 0))
    (let ((vis (%strip-csi-sequences (render-pane-output sess pane))))
      (is (string= " 1AB 1EF" vis)
          "hybrid numbering must switch the cursor row to absolute numbering (got ~S)"
          vis))))

(test copy-mode-line-numbers-mouse-enter-suppresses-gutter
  "copy-mode-entered-by-mouse-p suppresses line numbers, matching tmux mouse enter behaviour."
  (with-copy-mode-render-fixture (sess pane screen 4 2
                                  :content "ABCDEFGH"
                                  :options '("copy-mode-line-numbers" "absolute"))
    (setf (screen-copy-mode-p screen) t
          (cl-tmux/terminal/types:screen-copy-mode-entered-by-mouse-p screen) t)
    (let ((vis (%strip-csi-sequences (render-pane-output sess pane))))
      (is (string= "ABCDEFGH" vis)
          "mouse-entered copy mode must suppress the gutter (got ~S)" vis))))

(test copy-mode-current-line-number-style-applies-to-cursor-row
  "copy-mode-current-line-number-style overrides the base line-number style on the cursor row."
  (with-copy-mode-render-fixture (sess pane screen 4 2
                                  :content "ABCDEFGH"
                                  :options '("copy-mode-line-numbers" "absolute"
                                             "copy-mode-line-number-style" "fg=green"
                                             "copy-mode-current-line-number-style" "fg=red"))
    (setf (screen-copy-mode-p screen) t
          (screen-copy-cursor screen) (cons 1 0))
    (let ((out (render-pane-output sess pane)))
      (is (= 1 (%count-substring (format nil "~C[32m" #\Escape) out))
          "base line-number style must be emitted once for the non-cursor row (got ~S)" out)
      (is (= 1 (%count-substring (format nil "~C[31m" #\Escape) out))
          "current line-number style must be emitted once for the cursor row (got ~S)" out))))

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
  (dolist (c '(("red"          "31" "red -> 31")
               ("green"        "32" "green -> 32")
               ("bogus-colour" "96" "unknown -> 96 fallback")))
    (destructuring-bind (colour expected desc) c
      (with-isolated-options ("clock-mode-colour" colour)
        (is (string= expected (cl-tmux/renderer::%clock-face-sgr)) "~A" desc)))))

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

(test in-sel-branch-not-selecting
  "When copy-selecting is NIL the sel-active gate is false."
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
          (is (string= baseline out)
              "copy-selecting NIL must not change the rendered output (got ~S)"
              out))))))

(test in-sel-branch-single-row
  "Single-row selection: only cells in [sel-start-c, sel-end-c) are highlighted."
  (with-copy-mode-selection-fixture (sess pane screen 8 4
                                        :content "ABCDEFGHIJKLMNOPQRSTUVWXYZ01234567"
                                        :mark-row 0
                                        :mark-col 2
                                        :cursor-row 0
                                        :cursor-col 5)
    (let ((out (render-pane-output sess pane)))
      (is (%reverse-video-p out)
          "single-row selection: reverse-video SGR must appear (got ~S)" out))))

(test copy-mode-selection-style-drives-selection-colour
  "copy-mode-selection-style recolours selected cells, and mode-style no longer feeds selection highlighting."
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
          (is (search "48;5;172" out)
              "copy-mode-selection-style must emit bg colour172 on the selection (got ~S)" out)
          (is (null (search "48;5;99" out))
              "mode-style must not leak into copy-mode selection rendering (got ~S)" out))))))

(test copy-mode-mark-style-applies-mark-endpoint-style
  "copy-mode-mark-style recolours the marked cell only, and the marked endpoint still flips to reverse-video."
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
          (is (= 1 (%count-substring "38;5;88" out))
              "copy-mode-mark-style must emit fg colour88 once for the marked cell (got ~S)" out)
          (is (= 1 (%count-substring "48;5;172" out))
              "copy-mode-mark-style must emit bg colour172 once for the marked cell (got ~S)" out)
          (is (%reverse-video-p out)
              "the marked cell must still be rendered with reverse-video (got ~S)" out))))))

(test in-sel-branch-first-row
  "First row of a multi-row selection: cols >= sel-start-c are highlighted."
  (with-copy-mode-selection-fixture (sess pane screen 8 4
                                        :content "ABCDEFGHIJKLMNOPQRSTUVWXYZ01234567"
                                        :mark-row 0
                                        :mark-col 3
                                        :cursor-row 2
                                        :cursor-col 0)
    (let ((out (render-pane-output sess pane)))
      (is (%reverse-video-p out)
          "first-row branch: reverse-video SGR must appear (got ~S)" out))))

(test in-sel-branch-last-row
  "Last row of a multi-row selection: cols < sel-end-c are highlighted."
  (with-copy-mode-selection-fixture (sess pane screen 8 4
                                        :content "ABCDEFGHIJKLMNOPQRSTUVWXYZ01234567"
                                        :mark-row 0
                                        :mark-col 0
                                        :cursor-row 2
                                        :cursor-col 5)
    (let ((out (render-pane-output sess pane)))
      (is (%reverse-video-p out)
          "last-row branch: reverse-video SGR must appear (got ~S)" out))))

(test in-sel-branch-middle-row
  "Middle rows of a multi-row selection are fully highlighted."
  (with-copy-mode-selection-fixture (sess pane screen 8 4
                                        :content "ABCDEFGHIJKLMNOPQRSTUVWXYZ01234567"
                                        :mark-row 0
                                        :mark-col 0
                                        :cursor-row 3
                                        :cursor-col 0)
    (let ((out (render-pane-output sess pane)))
      (is (%reverse-video-p out)
          "middle-row branch: reverse-video SGR must appear (got ~S)" out))))

(test in-sel-branch-selecting-but-no-mark
  "When copy-selecting is T but mark is NIL, sel-active is false."
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
          (is (string= baseline out)
              "nil mark must suppress selection rendering changes (got ~S)" out))))))
