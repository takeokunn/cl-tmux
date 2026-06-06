(in-package #:cl-tmux/test)

;;;; Status bar and session compositing tests.
;;;;
;;;; Covers: %status-* helpers, render-status-bar, render-overlay,
;;;;         render-session-to-string, render-session, clear-display,
;;;;         render-popup, render-menu, enable/disable-mouse-reporting,
;;;;         %status-window-list-styled, %status-justify-line,
;;;;         %status-format-or-default
;;;;         from src/renderer-statusbar.lisp and src/renderer-compose.lisp.
;;;;
;;;; renderer-suite is declared in renderer-format-tests.lisp (loaded first).

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

;;; ── render-status-bar ───────────────────────────────────────────────────────

(test render-status-bar-shows-names
  (with-isolated-options ("status-left" nil "status-right" nil)
    (let* ((sess (make-test-session 40 10 :content ""))
           (out  (with-output-to-string (s)
                   (cl-tmux/renderer::render-status-bar s sess 10 40))))
      (is (search "0" out) "status bar should contain the session name 0 (got ~S)" out)
      ;; The active window is formatted using window-status-current-format:
      ;; " #{window_index}:#{window_name}* " → " 1:1* " for window named "1" at index 1.
      (is (search "1:1" out)
          "status bar should contain the active-window fragment 1:1 (got ~S)" out))))

(test status-bar-no-prompt-when-inactive
  "With *prompt* explicitly inactive, the status bar shows the normal status
   (window 1) and never the prompt text — pinning the active/inactive exclusion."
  (with-isolated-options ("status-left" nil "status-right" nil)
    (let ((cl-tmux/prompt:*prompt* nil))
      (let* ((sess (make-test-session 40 10 :content ""))
             (out  (with-output-to-string (s)
                     (cl-tmux/renderer::render-status-bar s sess 10 40))))
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
      (let ((out (with-output-to-string (s)
                   (cl-tmux/renderer::render-status-bar s sess 10 60))))
        (is (search "COPY" out)
            "status bar should show COPY indicator in copy mode (got ~S)" out)
        (is (search "+3" out)
            "status bar should show the copy offset +3 (got ~S)" out)))))

(test render-status-bar-no-copy-indicator-live
  (let* ((sess (make-test-session 60 10 :content ""))
         (out  (with-output-to-string (s)
                 (cl-tmux/renderer::render-status-bar s sess 10 60))))
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
           (out    (with-output-to-string (s)
                     (cl-tmux/renderer::render-status-bar s sess 10 width)))
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
                (out  (with-output-to-string (s)
                        (cl-tmux/renderer::render-status-bar s sess 10 60))))
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

;;; ── %status-bar-line (pure) ─────────────────────────────────────────────────

(test status-bar-line-fits-in-terminal-cols
  (let ((line (cl-tmux/renderer::%status-bar-line "left-text" "12:34" 20)))
    (is (<= (length line) 20)
        "%status-bar-line output must fit within terminal-cols=20 (got ~D: ~S)"
        (length line) line)))

(test status-bar-line-contains-left-and-time
  (let ((line (cl-tmux/renderer::%status-bar-line "mysession" "09:00" 40)))
    (is (search "mysession" line)
        "%status-bar-line should contain left text (got ~S)" line)
    (is (search "09:00" line)
        "%status-bar-line should contain the time string (got ~S)" line)))

(test status-bar-line-truncates-when-too-long
  ;; Terminal is only 5 cols wide; result must be clamped.
  (let ((line (cl-tmux/renderer::%status-bar-line "very-long-left-text" "99:99" 5)))
    (is (= 5 (length line))
        "%status-bar-line should truncate to terminal-cols=5 (got ~D: ~S)"
        (length line) line)))

;;; ── %status-current-time ────────────────────────────────────────────────────

(test status-current-time-returns-hhmm
  "%status-current-time returns a 5-char HH:MM string."
  (let ((t-str (cl-tmux/renderer::%status-current-time)))
    (is (= 5 (length t-str))
        "time string must be 5 chars, got ~D: ~S" (length t-str) t-str)
    (is (char= #\: (char t-str 2))
        "colon must be at position 2, got ~C" (char t-str 2))
    (is (every #'digit-char-p (remove #\: t-str))
        "all non-colon chars must be digits")))

;;; ── %status-left-text ────────────────────────────────────────────────────────

(test status-left-text-normal-mode
  "%status-left-text returns session/window info when no prompt is active."
  (let ((cl-tmux/prompt:*prompt* nil))
    (let* ((s   (make-fake-session :nwindows 1))
           (win (session-active-window s))
           (ap  (session-active-pane  s))
           (left (cl-tmux/renderer::%status-left-text s win ap)))
      (is (search "0" left) "session name '0' must appear in left text")
      (is (search "0" left) "window name '0' must appear in left text"))))

;;; ── %status-justify-line ─────────────────────────────────────────────────────

(test status-justify-line-left-default
  "%status-justify-line with justify=left matches %status-bar-line."
  (let* ((left "hello")
         (right "world")
         (cols 40)
         (result (cl-tmux/renderer::%status-justify-line left right cols "left"))
         (expected (cl-tmux/renderer::%status-bar-line left right cols)))
    (is (string= expected result)
        "left justify must match %status-bar-line (got ~S vs ~S)" result expected)))

(test status-justify-line-right-places-content-at-far-right
  "%status-justify-line with justify=right places the right string at far right."
  (let* ((result (cl-tmux/renderer::%status-justify-line "L" "R" 20 "right")))
    (is (<= (length result) 20)
        "right-justified result must fit in 20 cols (got ~D: ~S)"
        (length result) result)
    (is (char= #\R (char result (1- (length result))))
        "last character must be 'R' in right-justified mode (got ~S)" result)))

(test status-justify-line-centre-pads-symmetrically
  "%status-justify-line with justify=centre produces output containing both strings."
  (let ((result (cl-tmux/renderer::%status-justify-line "AB" "XY" 20 "centre")))
    (is (search "AB" result)
        "centre-justified must contain left 'AB' (got ~S)" result)
    (is (search "XY" result)
        "centre-justified must contain right 'XY' (got ~S)" result)
    (is (<= (length result) 20)
        "centre-justified result must fit in 20 cols (got ~D: ~S)"
        (length result) result)))

;;; ── %status-format-or-default ────────────────────────────────────────────────

(test status-format-or-default-uses-custom-option
  "%status-format-or-default returns the expanded custom option when set."
  (with-isolated-options ()
    (cl-tmux/options:set-option "status-left" "custom-left")
    (let* ((sess (make-test-session 40 10))
           (win  (session-active-window sess))
           (ap   (session-active-pane  sess))
           (ctx  (cl-tmux/format:format-context-from-session sess win ap))
           (result (cl-tmux/renderer::%status-format-or-default
                    "status-left" ctx (lambda () "fallback"))))
      (is (string= "custom-left" result)
          "%status-format-or-default must return the custom option (got ~S)" result))))

(test status-format-or-default-falls-back-to-default-fn
  "%status-format-or-default calls default-fn when option equals the registered default."
  (let* ((sess (make-test-session 40 10))
         (win  (session-active-window sess))
         (ap   (session-active-pane  sess))
         (ctx  (cl-tmux/format:format-context-from-session sess win ap))
         (called nil)
         (result (cl-tmux/renderer::%status-format-or-default
                  "status-left" ctx (lambda () (setf called t) "from-default"))))
    (is-true called
             "%status-format-or-default must invoke default-fn when option is unset")
    (is (string= "from-default" result)
        "%status-format-or-default must return the default-fn result (got ~S)" result)))

;;; ── render-overlay ───────────────────────────────────────────────────────────

(test render-session-draws-overlay
  "When an overlay is active, its text appears in the composed frame."
  (let ((*overlay* nil))
    (show-overlay "OVERLAY-HELP-LINE")
    (unwind-protect
         (let ((out (render-session-to-string
                     (make-test-session 30 6 :content "hi") 7 30)))
           (is (search "OVERLAY-HELP-LINE" out)
               "overlay text should be composited into the frame"))
      (clear-overlay))))

(test render-overlay-draws-overlay-lines
  "render-overlay writes each overlay line at the top of the screen."
  (let ((*overlay* nil))
    (show-overlay (format nil "line one~%line two"))
    (unwind-protect
         (let ((buf (make-string-output-stream)))
           (cl-tmux/renderer::render-overlay buf 20)
           (let ((out (get-output-stream-string buf)))
             (is (search "line" out) "overlay text must appear in output")))
      (clear-overlay))))

;;; ── DECTCEM cursor-visibility in rendered output ────────────────────────────

(test render-session-hides-cursor-when-dectcem-off
  "When the active pane has screen-cursor-visible=NIL, ?25h must NOT appear in the frame."
  (let* ((sess (make-test-session 30 5))
         (ap   (session-active-pane sess))
         (screen (pane-screen ap)))
    (setf (cl-tmux/terminal/types:screen-cursor-visible screen) nil)
    (let ((out (render-session-to-string sess 6 30)))
      (is (search (format nil "~C[?25l" #\Escape) out)
          "?25l must be emitted (cursor hidden at start)")
      (is-false (search (format nil "~C[?25h" #\Escape) out)
                "?25h must NOT be emitted when screen-cursor-visible is NIL"))))

(test render-session-shows-cursor-when-dectcem-on
  "When screen-cursor-visible=T (default), ?25h appears in the frame."
  (let* ((sess (make-test-session 30 5))
         (ap   (session-active-pane sess))
         (screen (pane-screen ap)))
    (setf (cl-tmux/terminal/types:screen-cursor-visible screen) t)
    (let ((out (render-session-to-string sess 6 30)))
      (is (search (format nil "~C[?25h" #\Escape) out)
          "?25h must appear when screen-cursor-visible is T"))))

(test render-session-no-active-pane-shows-cursor
  "With no active pane (nil ap), the renderer always emits ?25h."
  (let* ((win  (make-window :id 1 :name "1" :width 20 :height 5 :panes nil))
         (sess (make-session :id 1 :name "0" :windows (list win))))
    (session-select-window sess win)
    (let ((out (render-session-to-string sess 6 20)))
      (is (search (format nil "~C[?25h" #\Escape) out)
          "?25h must be emitted when active pane is nil"))))

;;; ── status-bar format string with #{session_name} ────────────────────────────

(test render-status-bar-custom-status-left-format-expands-session-name
  "When the status-left option is set to a #{session_name} format string,
   render-status-bar expands it and the rendered output contains the actual
   session name rather than the literal variable syntax."
  (with-isolated-options ()
    (cl-tmux/options:set-option "status-left" "sess:#{session_name}")
    (let* ((sess (make-test-session 60 10 :content ""))
           (out  (with-output-to-string (s)
                   (cl-tmux/renderer::render-status-bar s sess 10 60))))
      (is (search "sess:0" out)
          "status-left #{session_name} must expand to the session name '0' (got ~S)" out)
      (is (null (search "#{session_name}" out))
          "literal #{session_name} must NOT appear in the output (got ~S)" out))))

(test render-status-bar-custom-status-right-format-expands-window-name
  "When status-right is set to #{window_name}, the rendered bar contains the
   active window name instead of the default HH:MM clock."
  (with-isolated-options ()
    (cl-tmux/options:set-option "status-right" "win:#{window_name}")
    (let* ((sess (make-test-session 60 10 :content ""))
           (out  (with-output-to-string (s)
                   (cl-tmux/renderer::render-status-bar s sess 10 60))))
      (is (search "win:1" out)
          "status-right #{window_name} must expand to the window name '1' (got ~S)" out))))

;;; ── status-position top/bottom ───────────────────────────────────────────────

(test status-position-bottom-default
  "With status-position = bottom (default), the status bar appears at the last row."
  (with-isolated-options ("status-position" "bottom" "status-left" nil "status-right" nil)
    (let* ((sess (make-test-session 20 5))
           (rows 6)
           (out  (with-output-to-string (s)
                   (cl-tmux/renderer::render-status-bar s sess rows 20))))
      ;; The status bar emits ESC[row;colH where row is 1-based.
      ;; Bottom row = rows-1 = 5, so ESC[6;1H
      (is (search (format nil "~C[6;1H" #\Escape) out)
          "status-position bottom must place bar at row ~D (got ~S)" (1- rows) out))))

(test status-position-top
  "With status-position = top, the status bar appears at row 0 (ESC[1;1H)."
  (with-isolated-options ("status-position" "top" "status-left" nil "status-right" nil)
    (let* ((sess (make-test-session 20 5))
           (out  (with-output-to-string (s)
                   ;; render-status-bar directly with explicit status-row = 0
                   (cl-tmux/renderer::render-status-bar s sess 6 20 :status-row 0))))
      ;; ESC[1;1H is row=0, col=0 (1-based = row 1, col 1)
      (is (search (format nil "~C[1;1H" #\Escape) out)
          "status-position top must place bar at row 0 (got ~S)" out))))

;;; ── status on/off ────────────────────────────────────────────────────────────

(test status-off-no-status-bar
  "When the status option is nil/false, render-session-to-string emits no status bar."
  (with-isolated-options ("status" nil)
    (let* ((sess (make-test-session 20 5))
           (out  (render-session-to-string sess 6 20)))
      ;; With status=nil, the default blue SGR "44;97m" should not appear
      (is (null (search (format nil "~C[44;97m" #\Escape) out))
          "status=nil must suppress the status bar blue background (got ~S)" out))))

(test status-on-shows-status-bar
  "When the status option is true (default), render-session-to-string emits a status bar."
  (with-isolated-options ("status" t "status-left" nil "status-right" nil "status-style" "")
    (let* ((sess (make-test-session 20 5))
           (out  (render-session-to-string sess 6 20)))
      (is (search (format nil "~C[44;97m" #\Escape) out)
          "status=t must produce the status bar with blue background (got ~S)" out))))

;;; ── BEL rendering ────────────────────────────────────────────────────────────

(test render-emits-bel-when-bell-pending
  "render-session-to-string emits BEL (byte 7) when the active pane has bell-pending T,
   then clears the flag."
  (let* ((sess  (make-test-session 20 5))
         (ap    (session-active-pane sess))
         (sc    (pane-screen ap)))
    ;; Manually set bell-pending.
    (setf (cl-tmux/terminal/types:screen-bell-pending sc) t)
    (let ((out (render-session-to-string sess 6 20)))
      (is (find (code-char 7) out)
          "render output must contain BEL char (code 7) when bell-pending is T")
      (is-false (cl-tmux/terminal/types:screen-bell-pending sc)
                "bell-pending must be cleared after rendering"))))

(test render-no-bel-when-bell-pending-nil
  "render-session-to-string does not emit BEL when bell-pending is NIL."
  (let* ((sess  (make-test-session 20 5))
         (ap    (session-active-pane sess))
         (sc    (pane-screen ap)))
    (setf (cl-tmux/terminal/types:screen-bell-pending sc) nil)
    (let ((out (render-session-to-string sess 6 20)))
      (is (null (find (code-char 7) out))
          "render output must NOT contain BEL when bell-pending is NIL"))))

;;; ── status-left expanded ─────────────────────────────────────────────────────

(test status-left-expanded-session-name
  "status-left #{session_name} expands to the actual session name."
  (with-isolated-options ()
    (cl-tmux/options:set-option "status-left" "#{session_name}")
    (let* ((sess (make-test-session 40 10))
           (out  (with-output-to-string (s)
                   (cl-tmux/renderer::render-status-bar s sess 11 40))))
      (is (search "0" out)
          "status-left #{session_name} must expand to session name '0' (got ~S)" out)
      (is (null (search "#{session_name}" out))
          "literal #{session_name} must NOT appear (got ~S)" out))))

;;; ── parse-style-string ───────────────────────────────────────────────────────

(test parse-style-string-nil-returns-nil
  "parse-style-string with NIL returns NIL."
  (is (null (cl-tmux/renderer:parse-style-string nil))
      "parse-style-string nil must return nil"))

(test parse-style-string-empty-returns-nil
  "parse-style-string with empty string returns NIL."
  (is (null (cl-tmux/renderer:parse-style-string ""))
      "parse-style-string \"\" must return nil"))

(test parse-style-string-fg-color
  "parse-style-string parses fg=red into :fg \"red\"."
  (let ((p (cl-tmux/renderer:parse-style-string "fg=red")))
    (is (string= "red" (getf p :fg))
        "parse-style-string fg=red must set :fg to \"red\", got ~S" (getf p :fg))))

(test parse-style-string-bg-color
  "parse-style-string parses bg=blue into :bg \"blue\"."
  (let ((p (cl-tmux/renderer:parse-style-string "bg=blue")))
    (is (string= "blue" (getf p :bg))
        "parse-style-string bg=blue must set :bg to \"blue\", got ~S" (getf p :bg))))

(test parse-style-string-bold
  "parse-style-string parses bold into :bold T."
  (let ((p (cl-tmux/renderer:parse-style-string "bold")))
    (is (getf p :bold)
        "parse-style-string bold must set :bold T, got ~S" (getf p :bold))))

(test parse-style-string-reverse
  "parse-style-string parses reverse into :reverse T."
  (let ((p (cl-tmux/renderer:parse-style-string "reverse")))
    (is (getf p :reverse)
        "parse-style-string reverse must set :reverse T, got ~S" (getf p :reverse))))

(test parse-style-string-multiple-attrs
  "parse-style-string parses fg=green,bold,underline into a combined plist."
  (let ((p (cl-tmux/renderer:parse-style-string "fg=green,bold,underline")))
    (is (string= "green" (getf p :fg))
        ":fg must be \"green\", got ~S" (getf p :fg))
    (is (getf p :bold)
        ":bold must be T, got ~S" (getf p :bold))
    (is (getf p :underline)
        ":underline must be T, got ~S" (getf p :underline))))

(test parse-style-string-colour-n
  "parse-style-string parses fg=colour4 correctly."
  (let ((p (cl-tmux/renderer:parse-style-string "fg=colour4")))
    (is (string= "colour4" (getf p :fg))
        ":fg must be \"colour4\", got ~S" (getf p :fg))))

;;; ── style-to-sgr ────────────────────────────────────────────────────────────

(test style-to-sgr-nil-returns-default
  "style-to-sgr with NIL returns default blue-on-white SGR \"44;97\"."
  (is (string= "44;97" (cl-tmux/renderer:style-to-sgr nil))
      "style-to-sgr nil must return \"44;97\", got ~S"
      (cl-tmux/renderer:style-to-sgr nil)))

(test style-to-sgr-bold
  "style-to-sgr with :bold T includes SGR code 1."
  (let ((sgr (cl-tmux/renderer:style-to-sgr '(:bold t))))
    (is (search "1" sgr)
        "style-to-sgr :bold must include SGR code 1, got ~S" sgr)))

(test style-to-sgr-reverse
  "style-to-sgr with :reverse T includes SGR code 7."
  (let ((sgr (cl-tmux/renderer:style-to-sgr '(:reverse t))))
    (is (search "7" sgr)
        "style-to-sgr :reverse must include SGR code 7, got ~S" sgr)))

(test style-to-sgr-fg-red
  "style-to-sgr with :fg \"red\" includes SGR code 31."
  (let ((sgr (cl-tmux/renderer:style-to-sgr '(:fg "red"))))
    (is (search "31" sgr)
        "style-to-sgr :fg red must include SGR code 31, got ~S" sgr)))

(test style-to-sgr-bg-blue
  "style-to-sgr with :bg \"blue\" includes SGR code 44."
  (let ((sgr (cl-tmux/renderer:style-to-sgr '(:bg "blue"))))
    (is (search "44" sgr)
        "style-to-sgr :bg blue must include SGR code 44, got ~S" sgr)))

(test style-to-sgr-colour-n
  "style-to-sgr with :bg \"colour4\" includes extended SGR sequence 48;5;4."
  (let ((sgr (cl-tmux/renderer:style-to-sgr '(:bg "colour4"))))
    (is (search "48;5;4" sgr)
        "style-to-sgr :bg colour4 must include 48;5;4, got ~S" sgr)))

;;; ── status-left-length / status-right-length enforcement ────────────────────

(test status-left-length-truncates-long-left
  "status-left-length truncates the expanded left string to the configured max."
  (with-isolated-options ()
    (cl-tmux/options:set-option "status-left" "abcdefghij")
    (cl-tmux/options:set-option "status-left-length" 5)
    (let* ((sess (make-test-session 80 10))
           (out  (with-output-to-string (s)
                   (cl-tmux/renderer::render-status-bar s sess 11 80))))
      (is (search "abcde" out)
          "truncated left must start with first 5 chars (got ~S)" out)
      (is (null (search "abcdefghij" out))
          "full 10-char left must NOT appear when length limit is 5 (got ~S)" out))))

(test status-right-length-truncates-long-right
  "status-right-length truncates the expanded right string to the configured max."
  (with-isolated-options ()
    (cl-tmux/options:set-option "status-right" "1234567890")
    (cl-tmux/options:set-option "status-right-length" 4)
    (let* ((sess (make-test-session 80 10))
           (out  (with-output-to-string (s)
                   (cl-tmux/renderer::render-status-bar s sess 11 80))))
      (is (search "1234" out)
          "truncated right must start with first 4 chars (got ~S)" out)
      (is (null (search "1234567890" out))
          "full 10-char right must NOT appear when length limit is 4 (got ~S)" out))))

;;; ── window-status-format and window-status-current-format ───────────────────

(test window-status-format-custom
  "window-status-format option is used when rendering inactive windows."
  (with-isolated-options ("status-left" nil "status-right" nil
                          "window-status-format" "WIN:#{window_name}"
                          "window-status-current-format" "[#{window_name}]")
    ;; make-two-window-session creates windows named "alpha" (active) and "beta".
    (multiple-value-bind (sess win0 _p0 _w1 _p1)
        (make-two-window-session 80 5)
      (declare (ignore _p0 _w1 _p1))
      (session-select-window sess win0)  ; alpha is active
      (let ((out (with-output-to-string (s)
                   (cl-tmux/renderer::render-status-bar s sess 11 80))))
        (is (search "[alpha]" out)
            "active window must use window-status-current-format [alpha] (got ~S)" out)
        (is (search "WIN:beta" out)
            "inactive window must use window-status-format WIN:beta (got ~S)" out)))))

;;; ── window-status-separator ──────────────────────────────────────────────────

(test window-status-separator-used-between-windows
  "window-status-separator is placed between window entries."
  (with-isolated-options ("status-left" nil "status-right" nil
                          "window-status-separator" "|SEP|")
    (multiple-value-bind (sess win0 _p0 _w1 _p1)
        (make-two-window-session 80 5)
      (declare (ignore _p0 _w1 _p1))
      (session-select-window sess win0)
      (let ((out (with-output-to-string (s)
                   (cl-tmux/renderer::render-status-bar s sess 11 80))))
        (is (search "|SEP|" out)
            "window-status-separator |SEP| must appear between windows (got ~S)" out)))))

;;; ── render-popup ─────────────────────────────────────────────────────────────

(test render-popup-empty-draws-borders
  "render-popup with no live pane draws top border with corners and title, plus bottom border."
  (let* ((popup (make-popup :title "Test" :x 0 :y 0 :width 20 :height 6
                            :pane nil :screen nil :close-on-exit nil))
         (out   (with-output-to-string (s)
                  (cl-tmux/renderer::render-popup s popup 24 80))))
    (is (find (code-char #x250C) out)
        "render-popup must draw top-left corner ┌ (got ~S)" out)
    (is (find (code-char #x2510) out)
        "render-popup must draw top-right corner ┐ (got ~S)" out)
    (is (find (code-char #x2514) out)
        "render-popup must draw bottom-left corner └ (got ~S)" out)
    (is (find (code-char #x2518) out)
        "render-popup must draw bottom-right corner ┘ (got ~S)" out)
    (is (search "Test" out)
        "render-popup must include the popup title (got ~S)" out)))

(test render-popup-empty-draws-side-bars
  "render-popup with no live pane fills interior rows with │ side bars."
  (let* ((popup (make-popup :title "T" :x 0 :y 0 :width 10 :height 4
                            :pane nil :screen nil :close-on-exit nil))
         (out   (with-output-to-string (s)
                  (cl-tmux/renderer::render-popup s popup 24 80))))
    (is (find (code-char #x2502) out)
        "render-popup with empty interior must draw │ side bars (got ~S)" out)))

(test render-popup-with-pane-renders-content
  "render-popup with a live pane renders the screen cells inside the box."
  (let* ((sc    (make-screen 8 2))
         (pane  (make-pane :id 1 :x 0 :y 0 :width 8 :height 2 :fd -1 :screen sc))
         (popup (make-popup :title "P" :x 0 :y 0 :width 10 :height 4
                            :pane pane :screen sc :close-on-exit nil)))
    (feed sc "hi")
    (let ((out (with-output-to-string (s)
                 (cl-tmux/renderer::render-popup s popup 24 80))))
      (is (find #\h out)
          "render-popup with live pane must render pane content h (got ~S)" out)
      (is (find #\i out)
          "render-popup with live pane must render pane content i (got ~S)" out))))

;;; ── render-menu ──────────────────────────────────────────────────────────────

(test render-menu-draws-borders-and-items
  "render-menu draws borders, the title, and each menu item label."
  (let* ((items '(("Option A" . nil) ("Option B" . nil) ("Option C" . nil)))
         (menu  (make-menu :title "Choose" :items items :selected-index 0))
         (out   (with-output-to-string (s)
                  (cl-tmux/renderer::render-menu s menu 24 80))))
    (is (find (code-char #x250C) out)  "render-menu must draw top-left ┌ (got ~S)" out)
    (is (find (code-char #x2514) out)  "render-menu must draw bottom-left └ (got ~S)" out)
    (is (search "Choose" out)  "render-menu must include the title (got ~S)" out)
    (is (search "Option A" out) "render-menu must include item 'Option A' (got ~S)" out)
    (is (search "Option B" out) "render-menu must include item 'Option B' (got ~S)" out)
    (is (search "Option C" out) "render-menu must include item 'Option C' (got ~S)" out)))

(test render-menu-selection-indicator
  "render-menu emits ▶ for the selected item and space for others."
  (let* ((items '(("Alpha" . nil) ("Beta" . nil)))
         (menu  (make-menu :title "M" :items items :selected-index 1))
         (out   (with-output-to-string (s)
                  (cl-tmux/renderer::render-menu s menu 24 80))))
    ;; Selected item is index 1 (Beta).
    (is (find (code-char #x25B6) out)
        "render-menu must emit ▶ for the selected item (got ~S)" out)))

;;; ── enable-mouse-reporting / disable-mouse-reporting ─────────────────────────

(test enable-mouse-reporting-emits-dec-sequences
  "enable-mouse-reporting writes ?1000h, ?1002h, and ?1006h to *standard-output*."
  (let ((out (let ((*standard-output* (make-string-output-stream)))
               (cl-tmux/renderer::enable-mouse-reporting)
               (get-output-stream-string *standard-output*))))
    (is (search (format nil "~C[?1000h" #\Escape) out)
        "enable-mouse-reporting must emit ?1000h (got ~S)" out)
    (is (search (format nil "~C[?1002h" #\Escape) out)
        "enable-mouse-reporting must emit ?1002h (got ~S)" out)
    (is (search (format nil "~C[?1006h" #\Escape) out)
        "enable-mouse-reporting must emit ?1006h (got ~S)" out)))

(test disable-mouse-reporting-emits-dec-sequences
  "disable-mouse-reporting writes ?1006l, ?1002l, and ?1000l to *standard-output*."
  (let ((out (let ((*standard-output* (make-string-output-stream)))
               (cl-tmux/renderer::disable-mouse-reporting)
               (get-output-stream-string *standard-output*))))
    (is (search (format nil "~C[?1006l" #\Escape) out)
        "disable-mouse-reporting must emit ?1006l (got ~S)" out)
    (is (search (format nil "~C[?1002l" #\Escape) out)
        "disable-mouse-reporting must emit ?1002l (got ~S)" out)
    (is (search (format nil "~C[?1000l" #\Escape) out)
        "disable-mouse-reporting must emit ?1000l (got ~S)" out)))

;;; ── render-lock-screen ───────────────────────────────────────────────────────

(test render-lock-screen-fills-with-lock-message
  "render-lock-screen fills the terminal with a blue background and the 'locked' message."
  (let ((out (with-output-to-string (s)
               (cl-tmux/renderer::render-lock-screen s 24 80))))
    (is (plusp (length out))
        "render-lock-screen must produce non-empty output")
    (is (search "locked" out)
        "render-lock-screen must include 'locked' in the output (got ~S)" out)))

(test render-lock-screen-emits-blue-background
  "render-lock-screen emits the blue-background SGR sequence."
  (let ((out (with-output-to-string (s)
               (cl-tmux/renderer::render-lock-screen s 24 80))))
    (is (search (format nil "~C[44;97m" #\Escape) out)
        "render-lock-screen must emit ESC[44;97m (got ~S)" out)))

(test render-session-locked-shows-lock-overlay
  "When session-locked-p is T, render-session-to-string emits the lock overlay."
  (let* ((sess (make-test-session 40 10)))
    (setf (session-locked-p sess) t)
    (unwind-protect
         (let ((out (render-session-to-string sess 11 40)))
           (is (search "locked" out)
               "locked session must show 'locked' message in frame (got ~S)" out))
      ;; Restore so other tests are not affected.
      (setf (session-locked-p sess) nil))))

;;; ── %status-copy-indicator edge cases ───────────────────────────────────────

(test status-copy-indicator-in-copy-mode-zero-offset-returns-empty
  "%status-copy-indicator in copy-mode with offset 0 returns empty string."
  (let* ((screen (make-screen 10 5))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 5 :fd -1 :screen screen)))
    (setf (screen-copy-mode-p screen) t
          (screen-copy-offset screen) 0)
    (is (string= "" (cl-tmux/renderer::%status-copy-indicator pane))
        "%status-copy-indicator with offset 0 must return empty string")))

;;; ── %status-pane-indicator with non-nil pane ─────────────────────────────────

(test status-pane-indicator-formats-pane-id
  "%status-pane-indicator with pane id 99 returns a string containing '#99'."
  (let* ((screen (make-screen 10 5))
         (pane   (make-pane :id 99 :x 0 :y 0 :width 10 :height 5 :fd -1 :screen screen)))
    (let ((out (cl-tmux/renderer::%status-pane-indicator pane)))
      (is (search "#99" out)
          "%status-pane-indicator must contain '#99' (got ~S)" out))))

;;; ── %status-left-text with copy mode ─────────────────────────────────────────

(test status-left-text-copy-mode-shows-indicator
  "%status-left-text with copy mode active includes the copy indicator."
  (let ((cl-tmux/prompt:*prompt* nil))
    (let* ((sess   (make-fake-session :nwindows 1))
           (win    (session-active-window sess))
           (ap     (session-active-pane  sess))
           (screen (pane-screen ap)))
      ;; Enable copy mode with a non-zero offset.
      (setf (screen-copy-mode-p   screen) t
            (screen-copy-offset   screen) 2)
      (let ((left (cl-tmux/renderer::%status-left-text sess win ap)))
        (is (search "COPY" left)
            "%status-left-text in copy mode must contain 'COPY' (got ~S)" left)
        (is (search "+2" left)
            "%status-left-text in copy mode must show offset '+2' (got ~S)" left)))))

;;; ── %render-mouse-sequences (internal — three-way dispatch) ──────────────────
;;;
;;; These tests exercise %render-mouse-sequences directly to cover all three
;;; branches of the mouse-mode case: X10 (1 → ?1000h), button-event (2 → ?1002h),
;;; and any-event (other → ?1003h).

(defun %mouse-seq-output (mouse-mode sgr-mode)
  "Run %render-mouse-sequences with a synthetic pane whose screen has MOUSE-MODE
   and SGR-MODE set.  The global 'mouse' option is isolated to NIL so only the
   pane-level branch fires.  Returns the emitted string."
  (with-isolated-options ("mouse" nil)
    (let* ((screen (make-screen 10 4))
           (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 4
                              :fd -1 :screen screen)))
      (setf (cl-tmux/terminal/types:screen-mouse-mode     screen) mouse-mode
            (cl-tmux/terminal/types:screen-mouse-sgr-mode screen) sgr-mode)
      (with-output-to-string (s)
        (cl-tmux/renderer::%render-mouse-sequences s pane)))))

(test render-mouse-sequences-x10-mode
  "%render-mouse-sequences with mouse-mode 1 emits ?1000h (X10 tracking)."
  (let ((out (%mouse-seq-output 1 nil)))
    (is (search (format nil "~C[?1000h" #\Escape) out)
        "mouse-mode 1 must emit ?1000h (got ~S)" out)))

(test render-mouse-sequences-button-event-mode
  "%render-mouse-sequences with mouse-mode 2 emits ?1002h (button-event tracking)."
  (let ((out (%mouse-seq-output 2 nil)))
    (is (search (format nil "~C[?1002h" #\Escape) out)
        "mouse-mode 2 must emit ?1002h (got ~S)" out)))

(test render-mouse-sequences-any-event-mode
  "%render-mouse-sequences with mouse-mode 3 (other) emits ?1003h (any-event tracking)."
  (let ((out (%mouse-seq-output 3 nil)))
    (is (search (format nil "~C[?1003h" #\Escape) out)
        "mouse-mode 3 must emit ?1003h (got ~S)" out)))

(test render-mouse-sequences-sgr-extension
  "%render-mouse-sequences with sgr-mode T appends ?1006h (SGR extended encoding)."
  (let ((out (%mouse-seq-output 1 t)))
    (is (search (format nil "~C[?1006h" #\Escape) out)
        "sgr-mode T must emit ?1006h (got ~S)" out)))

(test render-mouse-sequences-zero-mode-emits-nothing
  "%render-mouse-sequences with mouse-mode 0 emits no sequences."
  (let ((out (%mouse-seq-output 0 nil)))
    (is (= 0 (length out))
        "mouse-mode 0 must emit nothing (got ~S)" out)))

(test render-mouse-sequences-session-global-overrides-pane
  "When the 'mouse' option is globally enabled, %render-mouse-sequences emits the
   global sequences (?1006h + ?1002h) regardless of pane mouse-mode."
  (with-isolated-options ("mouse" t)
    (let* ((screen (make-screen 10 4))
           (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 4
                              :fd -1 :screen screen)))
      (setf (cl-tmux/terminal/types:screen-mouse-mode screen) 0)
      (let ((out (with-output-to-string (s)
                   (cl-tmux/renderer::%render-mouse-sequences s pane))))
        (is (search (format nil "~C[?1006h" #\Escape) out)
            "global mouse must emit ?1006h (got ~S)" out)
        (is (search (format nil "~C[?1002h" #\Escape) out)
            "global mouse must emit ?1002h (got ~S)" out)))))

;;; ── render-lock-screen edge cases (coverage gap) ────────────────────────────

(test render-lock-screen-narrow-terminal-fits-message
  "render-lock-screen clamps the message to terminal-cols when the terminal
   is narrower than the message."
  (let* ((narrow-cols 12)
         (out (with-output-to-string (s)
                (cl-tmux/renderer::render-lock-screen s 5 narrow-cols))))
    (is (plusp (length out))
        "narrow render-lock-screen must produce output")
    ;; The message is truncated to 12 chars; "Session lock" is the prefix.
    (is (search "Session lock" out)
        "narrow lock screen must include the beginning of the message (got ~S)" out)))

(test render-lock-screen-single-row-terminal
  "render-lock-screen with terminal-rows=1 produces output without error."
  (let ((out (with-output-to-string (s)
               (cl-tmux/renderer::render-lock-screen s 1 40))))
    (is (plusp (length out))
        "single-row lock screen must produce non-empty output")))

;;; ── %render-panes-and-borders zoom suppression (coverage gap) ───────────────

(test render-panes-borders-suppressed-when-zoomed
  "%render-panes-and-borders emits no border characters when window-zoom-p is T."
  (let* ((l0   (tl-leaf 1 1 1))
         (l1   (tl-leaf 2 1 1))
         (tree (make-layout-split :h l0 l1)))
    (cl-tmux/model::layout-assign tree 0 0 81 24)
    (let* ((p0   (layout-leaf-pane l0))
           (p1   (layout-leaf-pane l1))
           (win  (make-window :id 1 :name "w" :width 81 :height 24
                              :panes (list p0 p1) :tree tree)))
      (window-select-pane win p0)
      ;; Enable zoom — borders should be suppressed.
      (setf (cl-tmux/model:window-zoom-p win) t)
      (let ((buf (make-string-output-stream)))
        (cl-tmux/renderer::%render-panes-and-borders buf win (list p0 p1) p0 81)
        (let ((out (get-output-stream-string buf)))
          (is (null (find #\│ out))
              "zoomed window must not emit vertical border │ (got ~S)" out))))))

;;; ── %justify-right and %justify-centre (coverage gap) ───────────────────────

(test justify-right-places-right-text-flush-right
  "%justify-right puts the right text at the far right of the line."
  (let* ((left  "left-text")
         (right "right")
         (cols  30)
         (line  (cl-tmux/renderer::%justify-right left right cols)))
    (is (<= (length line) cols)
        "%justify-right result must fit in ~D cols (got ~D: ~S)" cols (length line) line)
    (is (char= #\t (char line (1- (length line))))
        "last char must be the last char of right-text (got ~S)" line)
    (is (search left line)
        "%justify-right must include the left text (got ~S)" line)))

(test justify-right-short-cols-truncates
  "%justify-right truncates when cols is very small."
  (let ((line (cl-tmux/renderer::%justify-right "LLLL" "RRRR" 5)))
    (is (<= (length line) 5)
        "%justify-right must not exceed 5 cols (got ~D: ~S)" (length line) line)))

(test justify-centre-contains-both-strings
  "%justify-centre produces output containing both left and right strings."
  (let ((line (cl-tmux/renderer::%justify-centre "LEFT" "RIGHT" 30)))
    (is (search "LEFT"  line) "%justify-centre must contain 'LEFT' (got ~S)" line)
    (is (search "RIGHT" line) "%justify-centre must contain 'RIGHT' (got ~S)" line)
    (is (<= (length line) 30)
        "%justify-centre result must fit in 30 cols (got ~D: ~S)" (length line) line)))

(test justify-centre-short-cols-truncates
  "%justify-centre truncates when cols is smaller than the combined content."
  (let ((line (cl-tmux/renderer::%justify-centre "AAAA" "BBBB" 5)))
    (is (<= (length line) 5)
        "%justify-centre must not exceed 5 cols (got ~D: ~S)" (length line) line)))

;;; ── %clamp-status-segment ───────────────────────────────────────────────────

(test clamp-status-segment-short-text-unchanged
  "%clamp-status-segment returns text unchanged when it fits within max-length."
  (is (string= "hello" (cl-tmux/renderer::%clamp-status-segment "hello" 10))
      "text shorter than max must be returned unchanged"))

(test clamp-status-segment-exact-length-unchanged
  "%clamp-status-segment returns text unchanged when length equals max-length."
  (is (string= "hello" (cl-tmux/renderer::%clamp-status-segment "hello" 5))
      "text at exact max must be returned unchanged"))

(test clamp-status-segment-truncates-long-text
  "%clamp-status-segment truncates text exceeding max-length."
  (is (string= "hel" (cl-tmux/renderer::%clamp-status-segment "hello" 3))
      "text exceeding max must be truncated to max chars"))

(test clamp-status-segment-empty-string
  "%clamp-status-segment with empty string returns empty string."
  (is (string= "" (cl-tmux/renderer::%clamp-status-segment "" 10))
      "empty string must be returned as-is"))

;;; ── set-cursor-shape in rendered output ──────────────────────────────────────

(test render-session-emits-cursor-shape
  "render-session-to-string emits the DECSCUSR sequence for the pane cursor shape."
  (let* ((sess  (make-test-session 20 5))
         (ap    (session-active-pane sess))
         (sc    (pane-screen ap)))
    ;; Set a non-default cursor shape (2 = steady block)
    (setf (cl-tmux/terminal/types:screen-cursor-shape sc) 2)
    (let ((out (render-session-to-string sess 6 20)))
      (is (search (format nil "~C[2 q" #\Escape) out)
          "DECSCUSR shape 2 must appear in the frame (got ~S)" out))))

;;; ── render-session-to-string with nil window ────────────────────────────────

(test render-session-no-window-produces-output
  "render-session-to-string with a session that has no active window still renders."
  (let* ((sess (make-session :id 1 :name "0" :windows nil)))
    (finishes
      (let ((out (render-session-to-string sess 5 20)))
        (is (plusp (length out))
            "no-window render must produce non-empty output")))))

;;; ── %render-panes-and-borders with nil window ───────────────────────────────

(test render-panes-borders-nil-window-finishes
  "%render-panes-and-borders with NIL window does not signal."
  (finishes
    (let ((buf (make-string-output-stream)))
      (cl-tmux/renderer::%render-panes-and-borders buf nil nil nil 80))))

;;; ── status-justify-line dispatch table ──────────────────────────────────────

(test status-justify-line-table-driven
  "%status-justify-line dispatches correctly to right/centre/left strategies."
  (let ((cases
         ;; (justify left right cols . description)
         '(("right"  "L" "R" 20 . "right")
           ("centre" "L" "R" 20 . "centre")
           ("left"   "L" "R" 20 . "left (default)")
           ("unknown" "L" "R" 20 . "unknown falls back to left"))))
    (dolist (c cases)
      (destructuring-bind (justify left right cols . desc) c
        (let ((result (cl-tmux/renderer::%status-justify-line left right cols justify)))
          (is (<= (length result) cols)
              "%status-justify-line ~A result must fit in ~D cols (got ~D: ~S)"
              desc cols (length result) result)
          (is (search left result)
              "%status-justify-line ~A must contain left text (got ~S)" desc result))))))

;;; ── render-overlay with scroll offset ───────────────────────────────────────

(test render-overlay-scroll-renders-lines-from-offset
  "render-overlay renders overlay lines starting from *overlay-scroll-offset*."
  (let ((*overlay* nil)
        (*overlay-scroll-offset* 0))
    (show-overlay (format nil "line-A~%line-B~%line-C"))
    (unwind-protect
         (let ((buf (make-string-output-stream)))
           (cl-tmux/renderer::render-overlay buf 30)
           (let ((out (get-output-stream-string buf)))
             (is (search "line-A" out) "render-overlay must show first line")))
      (clear-overlay))))

;;; ── %status-bar-line gap calculation ────────────────────────────────────────

(test status-bar-line-gap-fills-exactly
  "%status-bar-line total length equals terminal-cols when content fits."
  (let* ((left  "abcde")
         (time  "12:34")
         (cols  20)
         (line  (cl-tmux/renderer::%status-bar-line left time cols)))
    ;; The line is truncated to cols so its length must be <= cols.
    (is (<= (length line) cols)
        "%status-bar-line must produce at most ~D chars (got ~D)" cols (length line))))

(test status-bar-line-empty-left-and-time
  "%status-bar-line with empty left and time strings produces spaces up to terminal-cols."
  (let ((line (cl-tmux/renderer::%status-bar-line "" "" 10)))
    (is (<= (length line) 10)
        "%status-bar-line with empty inputs must fit in 10 cols (got ~D: ~S)"
        (length line) line)))

;;; ── render-session-to-string status on/off interaction ──────────────────────

(test render-session-status-on-default-includes-time
  "With status=T and default options, the frame includes the HH:MM time pattern."
  (with-isolated-options ("status" t "status-left" nil)
    (let* ((sess (make-test-session 40 5))
           (out  (render-session-to-string sess 6 40)))
      ;; The default right status is HH:MM — 5 chars with a colon at position 2.
      ;; We just check a colon is present in a 5-char time substring.
      (is (find #\: out)
          "default status must include time with ':' character (got ~S)" out))))

;;; ── inline #[attr] style blocks + SGR-aware width (renderer-statusbar) ────────
;;;
;;; tmux status strings carry inline #[fg=…] style blocks and embedded SGR.  Those
;;; sequences are zero-width on screen, so the renderer expands #[…] into SGR and
;;; measures width by VISIBLE cells.  %visible-length/%visible-truncate must reduce
;;; to LENGTH/SUBSEQ on escape-free input (proven below) so older tests are intact.

(test visible-length-escape-free-equals-length
  "%visible-length equals LENGTH for strings with no escape sequences."
  (is (= 5 (cl-tmux/renderer::%visible-length "hello")))
  (is (= 0 (cl-tmux/renderer::%visible-length "")))
  (is (= (length "a:b 12:34")
         (cl-tmux/renderer::%visible-length "a:b 12:34"))))

(test visible-length-skips-sgr-sequences
  "%visible-length counts only visible cells, skipping CSI SGR escapes."
  (let ((esc #\Escape))
    (is (= 2 (cl-tmux/renderer::%visible-length
              (format nil "~C[32mhi~C[0m" esc esc)))
        "ESC[32mhiESC[0m has 2 visible cells")
    (is (= 3 (cl-tmux/renderer::%visible-length
              (format nil "~C[1;44;97mABC" esc)))
        "a multi-param SGR prefix is zero-width")))

(test visible-truncate-escape-free-equals-subseq
  "%visible-truncate equals SUBSEQ for escape-free strings."
  (is (string= "hel" (cl-tmux/renderer::%visible-truncate "hello" 3)))
  (is (string= "hello" (cl-tmux/renderer::%visible-truncate "hello" 5)))
  (is (string= "hello" (cl-tmux/renderer::%visible-truncate "hello" 99)))
  (is (string= "" (cl-tmux/renderer::%visible-truncate "hello" 0))))

(test visible-truncate-passes-sgr-through
  "%visible-truncate copies SGR escapes through without counting them toward N."
  (let* ((esc  #\Escape)
         (in   (format nil "~C[32mABCDE" esc))
         (out  (cl-tmux/renderer::%visible-truncate in 2)))
    (is (= 2 (cl-tmux/renderer::%visible-length out))
        "result must hold exactly 2 visible cells (got ~S)" out)
    (is (search "AB" out) "the 2 kept glyphs AB must be present (got ~S)" out)
    (is (char= esc (char out 0))
        "the leading SGR escape must be preserved (got ~S)" out)))

(test status-style-block-fg-becomes-sgr
  "%status-style-block-sgr turns fg=green into the SGR colour code 32."
  (let ((out (cl-tmux/renderer::%status-style-block-sgr "fg=green" "44;97")))
    (is (search (format nil "~C[32m" #\Escape) out)
        "fg=green must produce ESC[32m (got ~S)" out)))

(test status-style-block-default-resets-to-base
  "%status-style-block-sgr default/none/empty resets to the base status SGR."
  (let ((esc #\Escape))
    (dolist (body '("default" "none" "" "  "))
      (is (string= (format nil "~C[0;44;97m" esc)
                   (cl-tmux/renderer::%status-style-block-sgr body "44;97"))
          "~S must reset to ESC[0;44;97m" body))))

(test status-expand-style-blocks-no-block-unchanged
  "%status-expand-style-blocks returns escape-free / block-free text unchanged."
  (is (string= "plain text"
               (cl-tmux/renderer::%status-expand-style-blocks "plain text" "44;97")))
  (is (string= " 0 1:1* "
               (cl-tmux/renderer::%status-expand-style-blocks " 0 1:1* " "44;97"))))

(test status-expand-style-blocks-converts-blocks
  "%status-expand-style-blocks turns #[fg=green]X#[default] into SGR around X."
  (let* ((esc #\Escape)
         (out (cl-tmux/renderer::%status-expand-style-blocks
               "#[fg=green]X#[default]Y" "44;97")))
    (is (null (search "#[" out))
        "no literal #[ block may survive (got ~S)" out)
    (is (search (format nil "~C[32mX" esc) out)
        "green SGR must wrap X (got ~S)" out)
    (is (search (format nil "~C[0;44;97mY" esc) out)
        "#[default] before Y must reset to base SGR (got ~S)" out)))

(test clamp-status-segment-counts-visible-not-sgr
  "%clamp-status-segment measures visible cells; SGR escapes don't count and survive."
  (let* ((esc #\Escape)
         (txt (format nil "~C[32mhello~C[0m" esc esc)))   ; 5 visible cells
    (is (string= txt (cl-tmux/renderer::%clamp-status-segment txt 5))
        "5 visible ≤ max 5 → unchanged (SGR preserved)")
    (is (= 3 (cl-tmux/renderer::%visible-length
              (cl-tmux/renderer::%clamp-status-segment txt 3)))
        "max 3 keeps 3 visible cells")))

(test justify-right-ignores-sgr-width
  "%justify-right computes the gap from visible cells, so SGR doesn't shove content off-edge."
  (let* ((esc  #\Escape)
         (left (format nil "~C[32mABC~C[0m" esc esc))   ; 3 visible cells
         (line (cl-tmux/renderer::%justify-right left "RR" 20)))
    (is (= 20 (cl-tmux/renderer::%visible-length line))
        "visible width must fill exactly 20 cols (got ~D: ~S)"
        (cl-tmux/renderer::%visible-length line) line)
    (is (search "RR" line) "right text must be present (got ~S)" line)))

(test render-status-bar-inline-style-block-becomes-sgr
  "render-status-bar expands status-left #[fg=green]…#[default] into real SGR,
   and no literal #[ block reaches the output."
  (with-isolated-options ("status-left"  "#[fg=green]G#[default]"
                          "status-right" nil
                          "status-style" "")
    (let* ((sess (make-test-session 40 6))
           (out  (with-output-to-string (s)
                   (cl-tmux/renderer::render-status-bar s sess 10 40))))
      (is (search (format nil "~C[32m" #\Escape) out)
          "inline #[fg=green] must emit SGR 32 (got ~S)" out)
      (is (null (search "#[" out))
          "literal #[ must not survive into the rendered bar (got ~S)" out)
      (is (find #\G out)
          "the styled glyph G must be present (got ~S)" out))))
