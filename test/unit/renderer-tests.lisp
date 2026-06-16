(in-package #:cl-tmux/test)

;;;; render-status-bar, render-session, clear-display, status-pane/copy indicators, window-list styled — part I

(in-suite renderer-suite)

;;; ── Test fixtures ───────────────────────────────────────────────────────────
;;;
;;; make-renderer-test-session and make-test-session are defined in test/helpers.lisp
;;; and shared across renderer-tests.lisp, renderer-pane-tests.lisp, and prompt-tests.lisp.

(defun make-split-session (w h orient)
  "A 1-window session split into two panes (fd -1, no PTY).
   ORIENT is :h (side-by-side left|right) or :v (stacked top/bottom).
   The FIRST pane is active."
  (let* ((s0 (make-screen w h))
         (s1 (make-screen w h))
         (p0 (make-pane :id 1 :x 0 :y 0 :width w :height h :fd -1 :screen s0))
         (p1 (if (eq orient :h)
                 (make-pane :id 2 :x (1+ w) :y 0     :width w :height h :fd -1 :screen s1)
                 (make-pane :id 2 :x 0       :y (1+ h) :width w :height h :fd -1 :screen s1)))
         (total-w (if (eq orient :h) (+ (* 2 w) 1) w))
         (total-h (if (eq orient :h) h (+ (* 2 h) 1)))
         (win (make-window :id 1 :name "1"
                           :width  total-w
                           :height total-h
                           :tree (make-layout-split orient
                                    (make-layout-leaf p0)
                                    (make-layout-leaf p1)
                                    1/2)
                           :panes (list p0 p1)))
         (sess (make-session :id 1 :name "0" :windows (list win))))
    (window-select-pane win p0)
    (session-select-window sess win)
    sess))

;;; ── Test helper macros ───────────────────────────────────────────────────────
;;;
(defmacro with-minimal-status-bar-options (&body body)
  "Run BODY with status-left and status-right cleared (the 'no decorations'
   baseline used by ~10 status-bar tests)."
  `(with-isolated-options ("status-left" nil "status-right" nil)
     ,@body))

;;; ── render-status-bar ───────────────────────────────────────────────────────

(test render-status-bar-shows-names
  (with-isolated-options ("status-left" nil "status-right" nil)
    (let* ((sess (make-test-session 40 10 :content ""))
           (out  (render-status-bar-output sess 10 40)))
      (is (search "0" out) "status bar should contain the session name 0 (got ~S)" out)
      ;; The active window is formatted using window-status-current-format:
      ;; " #{window_index}:#{window_name}* " → " 1:1* " for window named "1" at index 1.
      (is (search "1:1" out)
          "status bar should contain the active-window fragment 1:1 (got ~S)" out))))

(test compose-aligned-line-positions-regions
  "%compose-aligned-line places #[align=right] content flush-right and
   #[align=centre] content centred, filling to the requested width."
  (flet ((vis (s) (cl-ppcre:regex-replace-all
                   (format nil "~C\\[[0-9;]*m" #\Escape) s "")))
    (is (string= "AB      CD"
                 (vis (cl-tmux/renderer::%compose-aligned-line "AB#[align=right]CD" "" 10)))
        "left + right-aligned across width 10")
    (is (string= "    XX    "
                 (vis (cl-tmux/renderer::%compose-aligned-line "#[align=centre]XX" "" 10)))
        "centred across width 10")
    (is (= 10 (cl-tmux/renderer::%visible-length
               (cl-tmux/renderer::%compose-aligned-line "L#[align=centre]C#[align=right]R" "" 10)))
        "three regions fill exactly the width")))

(test render-status-bar-uses-status-format0-template
  "When status-format[0] is set, the bar renders from that template with
   #[align=right] honoured, instead of the procedural left/window-list/right path."
  (with-isolated-options ("status-format[0]" "LEFThere#[align=right]RIGHThere")
    (let* ((sess (make-test-session 40 10 :content ""))
           (out  (render-status-bar-output sess 10 40))
           ;; Strip ALL CSI sequences (the leading cursor-move ESC[10;1H and any SGR).
           (vis  (cl-ppcre:regex-replace-all
                  (format nil "~C\\[[0-9;?]*[A-Za-z]" #\Escape) out ""))
           (rpos (search "RIGHThere" vis)))
      (is (eql 0 (search "LEFThere" vis)) "left content starts at column 0 (got ~S)" vis)
      (is (and rpos (= (+ rpos (length "RIGHThere")) 40))
          "right content ends at the terminal width (got ~S)" vis))))

(test render-status-bar-status-format0-expands-W-window-list
  "status-format[0] expands #{W:...}, so the window list appears in the template."
  (with-isolated-options ("status-format[0]" "#{W:[#{window_index}]}")
    (let* ((sess (make-test-session 40 10 :content ""))
           (out  (render-status-bar-output sess 10 40))
           (vis  (cl-ppcre:regex-replace-all
                  (format nil "~C\\[[0-9;?]*[A-Za-z]" #\Escape) out "")))
      (is (search "[" vis) "the #{W:} window list renders the bracketed window entry (got ~S)" vis))))

(test status-bar-no-prompt-when-inactive
  "With *prompt* explicitly inactive, the status bar shows the normal status
   (window 1) and never the prompt text — pinning the active/inactive exclusion."
  (with-isolated-options ("status-left" nil "status-right" nil)
    (let ((cl-tmux/prompt:*prompt* nil))
      (let* ((sess (make-test-session 40 10 :content ""))
             (out  (render-status-bar-output sess 10 40)))
        ;; window-status-current-format renders active window as " 1:1* "
        (is (search "1:1" out)
            "inactive status bar should show the window fragment 1:1 (got ~S)" out)
        (is (null (search "rename-window:" out))
            "inactive status bar must NOT show the prompt text (got ~S)" out)))))

(test render-status-bar-copy-mode-indicator
  (with-isolated-options ("status-left" nil "status-right" nil)
    (let* ((sess   (make-test-session 60 10 :content ""))
           (ap     (session-active-pane sess))
           (screen (pane-screen ap)))
      (setf (screen-copy-mode-p screen) t
            (screen-copy-offset screen) 3)
      (let ((out (render-status-bar-output sess 10 60)))
        (is (search "COPY" out)
            "status bar should show COPY indicator in copy mode (got ~S)" out)
        (is (search "+3" out)
            "status bar should show the copy offset +3 (got ~S)" out)))))

(test render-status-bar-no-copy-indicator-live
  (let* ((sess (make-test-session 60 10 :content ""))
         (out  (render-status-bar-output sess 10 60)))
    (is (not (search "COPY" out))
        "live status bar should NOT show the COPY indicator (got ~S)" out)))

(test render-status-bar-truncates-long-line
  ;; A very narrow terminal forces the status line to be truncated via subseq.
  ;; The bar is: move-to, ESC[44;97m, <status content>, ESC[0m.  The visible
  ;; status content sits between the colour SGR and the trailing reset, and the
  ;; renderer guarantees it is no longer than the terminal width.
  (with-isolated-options ("status-left" nil "status-right" nil)
    (let* ((width  8)
           (sess   (make-test-session width 10 :content ""))
           (out    (render-status-bar-output sess 10 width))
           (color  (format nil "~C[44;97m" #\Escape))
           (reset  (format nil "~C[0m" #\Escape))
           (start  (+ (search color out) (length color)))
           (end    (search reset out :start2 start))
           (content (subseq out start end)))
      ;; Measure VISIBLE cells: the default window-status-current-style is
      ;; "reverse", so the active window is wrapped in a zero-width ESC[7m…
      ;; highlight.  The renderer now fills the full terminal width with visible
      ;; glyphs and preserves that SGR, so the raw length may exceed WIDTH while
      ;; the on-screen width does not.
      (is (<= (cl-tmux/renderer::%visible-length content) width)
          "narrow status content must fit in ~D visible cols (got ~D visible / ~D raw: ~S)"
          width (cl-tmux/renderer::%visible-length content) (length content) content)
      ;; The full line (left text + gap + time) is longer than the terminal, so
      ;; the HH:MM time string (right portion) is truncated off the visible content.
      ;; We verify this by checking the content is shorter than the full line would be.
      (is (< (length content) 20)
          "narrow status content should be truncated (got ~S)" content))))

(test render-status-bar-active-prompt-replaces-left-segment
  "An active *prompt* replaces the whole left status segment with its
   \"LABEL: BUFFER\" text — the prompt text appears and the normal
   window-list (1:1*) is absent."
  (let ((cl-tmux/prompt:*prompt* nil))
    (cl-tmux/prompt:prompt-start "rename-window" "abc" nil)
    (unwind-protect
         (let* ((sess (make-test-session 60 10 :content ""))
                (out  (render-status-bar-output sess 10 60)))
           ;; prompt-text formats as "LABEL: BUFFER".
           (is (search "rename-window: abc" out)
               "active-prompt status bar should show the prompt text (got ~S)" out)
           ;; When prompt is active, the window list (1:1*) is suppressed.
           (is (null (search "1:1*" out))
               "active-prompt status bar must NOT show the window-list 1:1* (got ~S)" out))
      (cl-tmux/prompt:prompt-clear))))

;;; ── render-session-to-string (full frame) ───────────────────────────────────

(test render-session-to-string-full-frame
  (with-isolated-options ("status-left" nil "status-right" nil)
    (let* ((sess (make-test-session 20 5 :content "hi"))
           (out  (render-session-to-string sess 6 20)))
      (is (find #\h out) "frame should contain h from content (got ~S)" out)
      (is (find #\i out) "frame should contain i from content (got ~S)" out)
      (is (search (format nil "~C[?25l" #\Escape) out)
          "frame should hide the cursor with ESC[?25l (got ~S)" out)
      (is (search (format nil "~C[?25h" #\Escape) out)
          "frame should show the cursor with ESC[?25h (got ~S)" out)
      ;; The active window is formatted with window-status-current-format
      ;; default: " #{window_index}:#{window_name}* " → " 1:1* "
      (is (search "1:1" out)
          "frame should contain the active-window fragment 1:1 (got ~S)" out))))

(test render-session-vertical-split-emits-separators
  (let* ((sess  (make-split-session 5 3 :h))
         (win   (session-active-window sess))
         (panes (window-panes win))
         (green (format nil "~C[32m" #\Escape)))
    (feed (pane-screen (first  panes)) "AAA")
    (feed (pane-screen (second panes)) "BBB")
    (let ((out (render-session-to-string sess 3 11)))   ; full width = 2*5+1
      (is (find (code-char #x2502) out)
          "vertical split frame should contain a vertical separator │ (got ~S)" out)
      ;; pane 0 is active and non-last, so its right border is highlighted.
      (is (search green out)
          "vertical split frame should highlight the active pane border (got ~S)" out)
      (is (find #\A out)
          "vertical split frame should contain pane 0 content A (got ~S)" out)
      (is (find #\B out)
          "vertical split frame should contain pane 1 content B (got ~S)" out))))

(test render-session-horizontal-split-emits-separators
  (let* ((sess  (make-split-session 5 3 :v))
         (win   (session-active-window sess))
         (panes (window-panes win)))
    (feed (pane-screen (first  panes)) "AAA")
    (feed (pane-screen (second panes)) "BBB")
    (let ((out (render-session-to-string sess 7 5)))    ; full height = 2*3+1
      (is (find (code-char #x2500) out)
          "horizontal split frame should contain a horizontal separator ─ (got ~S)" out)
      (is (find #\A out)
          "horizontal split frame should contain pane 0 content A (got ~S)" out)
      (is (find #\B out)
          "horizontal split frame should contain pane 1 content B (got ~S)" out))))

(test render-session-vertical-border-suppressed-at-edge
  "In render-session-to-string the vertical separator is drawn only when the
   border column is strictly inside the terminal width.  A split whose first
   pane's right edge lands exactly at terminal-cols suppresses the │ bar."
  (let* ((sess  (make-split-session 5 3 :h))
         (win   (session-active-window sess))
         (panes (window-panes win)))
    (feed (pane-screen (first  panes)) "AAA")
    (feed (pane-screen (second panes)) "BBB")
    ;; First pane is x=0 width=5, so its border column is 5.  Render with
    ;; terminal-cols=5 → (< 5 5) is false → the vertical border is suppressed.
    (let ((out (render-session-to-string sess 3 5)))
      (is (null (find (code-char #x2502) out))
          "vertical border at the terminal edge should be suppressed (got ~S)" out))))

(test render-session-writes-to-standard-output
  (let ((out (let ((*standard-output* (make-string-output-stream)))
               (render-session (make-test-session 10 4 :content "hi") 5 10)
               (get-output-stream-string *standard-output*))))
    (is (plusp (length out))
        "render-session should write a non-empty frame to *standard-output*")
    (is (find #\h out)
        "render-session output should contain content char h (got ~S)" out)))

;;; ── clear-display ───────────────────────────────────────────────────────────

(test clear-display-emits-clear-and-home
  (let ((out (let ((*standard-output* (make-string-output-stream)))
               (clear-display)
               (get-output-stream-string *standard-output*))))
    (is (search (format nil "~C[2J" #\Escape) out)
        "clear-display should emit ESC[2J (got ~S)" out)
    (is (search (format nil "~C[H" #\Escape) out)
        "clear-display should emit ESC[H (got ~S)" out)))

;;; ── %status-pane-indicator (pure) ───────────────────────────────────────────

(test status-pane-indicator-with-active-pane
  (let* ((screen (make-screen 10 5))
         (pane   (make-pane :id 7 :x 0 :y 0 :width 10 :height 5 :fd -1 :screen screen))
         (out    (cl-tmux/renderer::%status-pane-indicator pane)))
    (is (search "#7" out)
        "%status-pane-indicator should contain the pane id (got ~S)" out)))

(test status-pane-indicator-nil-returns-empty
  (is (string= "" (cl-tmux/renderer::%status-pane-indicator nil))
      "%status-pane-indicator with nil should return empty string"))

;;; ── %status-copy-indicator (pure) ───────────────────────────────────────────

(test status-copy-indicator-in-copy-mode
  (let* ((screen (make-screen 10 5))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 5 :fd -1 :screen screen)))
    (setf (screen-copy-mode-p screen) t
          (screen-copy-offset screen) 5)
    (let ((out (cl-tmux/renderer::%status-copy-indicator pane)))
      (is (search "COPY" out)
          "%status-copy-indicator in copy mode should contain COPY (got ~S)" out)
      (is (search "+5" out)
          "%status-copy-indicator should contain the offset +5 (got ~S)" out))))

(test status-copy-indicator-not-in-copy-mode
  (let* ((screen (make-screen 10 5))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 5 :fd -1 :screen screen)))
    (is (string= "" (cl-tmux/renderer::%status-copy-indicator pane))
        "%status-copy-indicator outside copy mode should return empty string")))

(test status-copy-indicator-nil-pane-returns-empty
  (is (string= "" (cl-tmux/renderer::%status-copy-indicator nil))
      "%status-copy-indicator with nil pane should return empty string"))

;;; ── %status-window-list-styled window-list behaviour (via styled variant) ───
;;;
;;; %status-window-list was dead code (never called from production) and has been
;;; removed.  These tests now call %status-window-list-styled with empty style
;;; options so the same window-list behaviour is exercised through the live path.

(test status-window-list-brackets-active-window
  "Active window appears with the * marker in the window list."
  (with-isolated-options ("window-status-current-style" ""
                          "window-status-style" "")
    (let* ((sess (make-test-session 20 5 :content ""))
           (win  (session-active-window sess))
           (out  (cl-tmux/renderer::%status-window-list-styled sess win)))
      ;; window-status-current-format default: " #{window_index}:#{window_name}* "
      ;; window named "1" at index 1 → " 1:1* "
      (is (search "1:1" out)
          "%status-window-list-styled should contain the active window 1:1 (got ~S)" out)
      (is (search "*" out)
          "%status-window-list-styled should contain * marker for active window (got ~S)" out))))

(test status-window-list-two-windows-formats-both
  "Both active and inactive windows appear with correct format strings."
  (with-isolated-options ("window-status-current-style" ""
                          "window-status-style" "")
    ;; Build a 2-window session manually; second window is active.
    (let* ((s0   (make-screen 10 5))
           (p0   (make-pane :id 1 :x 0 :y 0 :width 10 :height 5 :fd -1 :screen s0))
           (w0   (make-window :id 1 :name "alpha" :width 10 :height 5 :panes (list p0)))
           (s1   (make-screen 10 5))
           (p1   (make-pane :id 2 :x 0 :y 0 :width 10 :height 5 :fd -1 :screen s1))
           (w1   (make-window :id 2 :name "beta"  :width 10 :height 5 :panes (list p1)))
           (sess (make-session :id 1 :name "0" :windows (list w0 w1))))
      (window-select-pane w0 p0)
      (window-select-pane w1 p1)
      (session-select-window sess w1)
      ;; Active window is beta → rendered with window-status-current-format "index:name*"
      ;; Inactive window alpha → rendered with window-status-format "index:name"
      (let ((out (cl-tmux/renderer::%status-window-list-styled sess w1)))
        ;; Active window "beta" gets the current-format with "*"
        (is (search "beta*" out)
            "%status-window-list-styled should mark active window beta with * (got ~S)" out)
        ;; Inactive window "alpha" appears without "*"
        (is (search "alpha" out)
            "%status-window-list-styled should include the inactive window alpha (got ~S)" out)
        ;; alpha should NOT have the asterisk marker
        (is (null (search "alpha*" out))
            "%status-window-list-styled must NOT mark inactive window alpha with * (got ~S)" out)))))

;;; ── %status-window-list-styled ───────────────────────────────────────────────

(test status-window-list-styled-active-gets-sgr
  "When window-status-current-style is set, %status-window-list-styled wraps
   the active window label in the configured SGR codes."
  (with-isolated-options ("window-status-current-style" "bold"
                          "window-status-style" "")
    (let* ((sess (make-test-session 20 5 :content ""))
           (win  (session-active-window sess))
           (out  (cl-tmux/renderer::%status-window-list-styled sess win)))
      ;; bold SGR = "1"; the function wraps it as ESC[<sgr>m ... ESC[0m
      (is (search "1:1" out)
          "%status-window-list-styled must include the window label 1:1 (got ~S)" out)
      ;; The SGR reset ESC[0m must appear (closing the style wrapper).
      (is (search (format nil "~C[0m" #\Escape) out)
          "%status-window-list-styled must emit SGR reset (got ~S)" out))))

(test window-status-current-style-applied-directly
  "%window-status-style returns the window-status-current-style option directly
   for the active window."
  (with-isolated-options ("window-status-current-style" "bg=red")
    (let* ((sess  (make-test-session 20 5 :content ""))
           (win   (session-active-window sess))
           (style (cl-tmux/renderer::%window-status-style sess win t)))
      (is (search "bg=red" style)
          "active window style must be bg=red (got ~S)" style))))

(test window-status-style-applied-directly
  "%window-status-style returns the window-status-style option directly
   for a non-active window."
  (with-isolated-options ("window-status-style" "fg=green")
    (let* ((sess  (make-test-session 20 5 :content ""))
           (win   (session-active-window sess))
           (style (cl-tmux/renderer::%window-status-style sess win nil)))
      (is (search "fg=green" style)
          "non-active window style must be fg=green (got ~S)" style))))

(test status-window-list-styled-no-style-no-sgr
  "When both style options are empty, %status-window-list-styled emits plain
   labels with no SGR wrapping."
  (with-isolated-options ("window-status-current-style" ""
                          "window-status-style" "")
    (let* ((sess (make-test-session 20 5 :content ""))
           (win  (session-active-window sess))
           (out  (cl-tmux/renderer::%status-window-list-styled sess win)))
      (is (search "1:1" out)
          "%status-window-list-styled must include the window label 1:1 (got ~S)" out)
      ;; No SGR sequences should be emitted when styles are empty.
      (is (null (search (format nil "~C[" #\Escape) out))
          "%status-window-list-styled must NOT emit SGR when styles are empty (got ~S)" out))))

(test status-window-list-inline-style-block-in-current-format
  "Inline #[fg=red] in window-status-current-format expands to real SGR in the
   window list, even when the per-window style option is empty."
  (with-isolated-options ("window-status-current-style" ""
                          "window-status-style" ""
                          "window-status-current-format" "#[fg=red]#{window_name}#[default]")
    (let* ((sess (make-test-session 20 5 :content ""))
           (win  (session-active-window sess))
           (out  (cl-tmux/renderer::%status-window-list-styled sess win)))
      (is (search (format nil "~C[31m" #\Escape) out)
          "inline #[fg=red] must emit SGR 31 in the window list (got ~S)" out)
      (is (null (search "#[" out))
          "no literal #[ block may survive into the window list (got ~S)" out)
      (is (search "1" out)
          "the window name must still be present (got ~S)" out)
      ;; The trailing #[default] / wrapper guarantees a reset so colour does not
      ;; bleed past the window label.
      (is (search (format nil "~C[0" #\Escape) out)
          "a reset must close the inline style (got ~S)" out))))

(test status-window-list-inline-block-without-window-style-still-resets
  "A window label that injects SGR via #[...] is reset afterwards even when the
   window has no style option set (so the next window/separator is unstyled)."
  (with-isolated-options ("window-status-current-style" ""
                          "window-status-style" ""
                          "window-status-current-format" "#[fg=green]#{window_name}")
    (let* ((sess (make-test-session 20 5 :content ""))
           (win  (session-active-window sess))
           (out  (cl-tmux/renderer::%status-window-list-styled sess win)))
      (is (search (format nil "~C[32m" #\Escape) out)
          "inline #[fg=green] must emit SGR 32 (got ~S)" out)
      (is (search (format nil "~C[0m" #\Escape) out)
          "the injected style must be reset after the label (got ~S)" out))))

(test status-window-list-plain-format-unchanged-by-expansion
  "A window-status-current-format with no #[ block and no style option produces
   exactly the same plain label as before (no spurious SGR)."
  (with-isolated-options ("window-status-current-style" ""
                          "window-status-style" ""
                          "window-status-current-format" " #{window_index}:#{window_name} ")
    (let* ((sess (make-test-session 20 5 :content ""))
           (win  (session-active-window sess))
           (out  (cl-tmux/renderer::%status-window-list-styled sess win)))
      (is (search "1:1" out)
          "plain window label must be present (got ~S)" out)
      (is (null (search (format nil "~C[" #\Escape) out))
          "plain format with empty styles must emit NO SGR (got ~S)" out))))
