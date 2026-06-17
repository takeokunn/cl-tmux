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
  (let* ((pane   (make-test-pane 20 6 :id 42))
         (cl-tmux::*clock-mode-pane-id* 42))
    (let ((out (render-pane-output pane)))
      (is (find #\█ out)
          "render-pane in clock mode must emit block-element digits (got ~S)" out))))

(test render-pane-no-clock-when-id-mismatch
  "When *clock-mode-pane-id* does not match the pane id, the clock overlay is suppressed."
  (let* ((pane   (make-test-pane 20 6 :id 1))
         (cl-tmux::*clock-mode-pane-id* 99))
    (let ((out (render-pane-output pane)))
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
    (let ((out (render-pane-output pane)))
      (is (null (%reverse-video-p out))
          "no reverse-video SGR should appear when copy-selecting is NIL (got ~S)"
          out))))

(test in-sel-branch-single-row
  "Single-row selection: only cells in [sel-start-c, sel-end-c) are highlighted."
  (let* ((pane (%make-selecting-pane 8 4
                                     "ABCDEFGHIJKLMNOPQRSTUVWXYZ01234567"
                                     0 2 0 5)))
    (let ((out (render-pane-output pane)))
      (is (%reverse-video-p out)
          "single-row selection: reverse-video SGR must appear (got ~S)" out))))

(test mode-style-default-reverse-keeps-reverse-video-selection
  "With the default mode-style (reverse), a selection is still drawn with the
   reverse-video SGR — the colour path stays opt-in."
  (with-isolated-config
    (let ((pane (%make-selecting-pane 8 4 "ABCDEFGHIJKLMNOP" 0 2 0 5)))
      (cl-tmux/options:set-option "mode-style" "reverse")
      (let ((out (render-pane-output pane)))
        (is (%reverse-video-p out)
            "default mode-style must keep reverse-video selection (got ~S)" out)))))

(test mode-style-colour-recolours-selection-without-reverse
  "A colour-based mode-style highlights the selection with its bg instead of
   reverse-video: bg=colour172 → 48;5;172 appears, the ;7 reverse code does not."
  (with-isolated-config
    (let ((pane (%make-selecting-pane 8 4 "ABCDEFGHIJKLMNOP" 0 2 0 5)))
      (cl-tmux/options:set-option "mode-style" "bg=colour172")
      (let ((out (render-pane-output pane)))
        (is (search "48;5;172" out)
            "colour mode-style must emit bg colour172 on the selection (got ~S)" out)
        (is (null (%reverse-video-p out))
            "colour mode-style must NOT also reverse-video the selection (got ~S)" out)))))

(test in-sel-branch-first-row
  "First row of a multi-row selection: cols >= sel-start-c are highlighted."
  (let* ((pane (%make-selecting-pane 8 4
                                     "ABCDEFGHIJKLMNOPQRSTUVWXYZ01234567"
                                     0 3 2 0)))
    (let ((out (render-pane-output pane)))
      (is (%reverse-video-p out)
          "first-row branch: reverse-video SGR must appear (got ~S)" out))))

(test in-sel-branch-last-row
  "Last row of a multi-row selection: cols < sel-end-c are highlighted."
  (let* ((pane (%make-selecting-pane 8 4
                                     "ABCDEFGHIJKLMNOPQRSTUVWXYZ01234567"
                                     0 0 2 5)))
    (let ((out (render-pane-output pane)))
      (is (%reverse-video-p out)
          "last-row branch: reverse-video SGR must appear (got ~S)" out))))

(test in-sel-branch-middle-row
  "Middle rows of a multi-row selection are fully highlighted."
  (let* ((pane (%make-selecting-pane 8 4
                                     "ABCDEFGHIJKLMNOPQRSTUVWXYZ01234567"
                                     0 0 3 0)))
    (let ((out (render-pane-output pane)))
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
    (let ((out (render-pane-output pane)))
      (is (null (%reverse-video-p out))
          "nil mark must suppress reverse-video (got ~S)" out))))
