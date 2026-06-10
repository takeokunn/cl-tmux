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
           (out  (with-output-to-string (s)
                   (cl-tmux/renderer::render-status-bar s sess 10 40)))
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
           (out  (with-output-to-string (s)
                   (cl-tmux/renderer::render-status-bar s sess 10 40)))
           (vis  (cl-ppcre:regex-replace-all
                  (format nil "~C\\[[0-9;?]*[A-Za-z]" #\Escape) out "")))
      (is (search "[" vis) "the #{W:} window list renders the bracketed window entry (got ~S)" vis))))

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

(test window-status-style-folds-deprecated-current-bg
  "%window-status-style folds the deprecated window-status-current-bg into the
   active window's style (old .tmux.conf compat)."
  (with-isolated-options ("window-status-current-style" ""
                          "window-status-current-bg" "red")
    (let* ((sess  (make-test-session 20 5 :content ""))
           (win   (session-active-window sess))
           (style (cl-tmux/renderer::%window-status-style sess win t)))
      (is (string= "bg=red" style)
          "active window style must fold to bg=red (got ~S)" style))))

(test window-status-style-folds-deprecated-normal-fg
  "%window-status-style folds the deprecated window-status-fg into a non-active
   window's normal style."
  (with-isolated-options ("window-status-style" ""
                          "window-status-fg" "green")
    (let* ((sess  (make-test-session 20 5 :content ""))
           (win   (session-active-window sess))
           (style (cl-tmux/renderer::%window-status-style sess win nil)))
      (is (string= "fg=green" style)
          "non-active window style must fold to fg=green (got ~S)" style))))

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

;;; ── %status-window-list-styled per-window option resolution ──────────────────
;;;
;;; window-status-format / window-status-current-format / window-status-style /
;;; window-status-current-style are now resolved PER WINDOW via
;;; get-option-for-context :window WINDOW inside the dolist.  A window-local
;;; override on one window must affect only that window's tab; windows without an
;;; override must still resolve to the global / registered value (so output is
;;; identical to the pre-change behaviour).  make-fake-session ids windows from 0
;;; (base-index), the FIRST window is active, so window 2 has #{window_index} = 1.

(test status-window-list-per-window-format-override
  "A window-local window-status-format on the (non-active) second window is used
   for that window's tab only; the active window keeps the default
   current-format.  Proves the format is read per-window, not from the global."
  (with-isolated-config
    (let* ((sess (make-fake-session :nwindows 2))
           (windows (cl-tmux/model:session-windows sess))
           (win2 (second windows))
           ;; make-fake-session selects the FIRST window, so win2 is inactive.
           (idx2 (cl-tmux/model:window-id win2)))
      ;; Distinctive literal so the expansion " W1X " is unmistakable in output.
      (cl-tmux/options:set-option-for-window
       "window-status-format" " W#{window_index}X " win2)
      (let ((out (cl-tmux/renderer::%status-window-list-styled
                  sess (cl-tmux/model:session-active-window sess))))
        ;; Window 2 (inactive, index 1) must use its per-window override.
        (is (search (format nil "W~DX" idx2) out)
            "non-active window 2 must use its per-window window-status-format ~
             (expected ~S in ~S)" (format nil "W~DX" idx2) out)
        ;; Window 1 (active, index 0) must still use the default current-format,
        ;; i.e. the normal "index:name" form ("0:0").  It must NOT have picked up
        ;; window 2's distinctive literal.
        (is (search "0:0" out)
            "active window 1 must still use the default current-format \"0:0\" ~
             (got ~S)" out)
        (is (null (search "W0X" out))
            "the per-window override on window 2 must NOT bleed onto window 1 ~
             (got ~S)" out)))))

(test status-window-list-per-window-style-override
  "A window-local window-status-style fg=red on the non-active window changes
   only that window's rendered output.  Comparing the 2-window list WITH the
   per-window override against the same list WITHOUT it proves the per-window
   style is resolved and applied (the strings must differ)."
  ;; Capture WITHOUT any override.
  (let ((without
          (with-isolated-config
            (let ((sess (make-fake-session :nwindows 2)))
              (cl-tmux/renderer::%status-window-list-styled
               sess (cl-tmux/model:session-active-window sess)))))
        ;; Capture WITH a window-local fg=red on window 2 (inactive).
        (with
          (with-isolated-config
            (let* ((sess (make-fake-session :nwindows 2))
                   (win2 (second (cl-tmux/model:session-windows sess))))
              (cl-tmux/options:set-option-for-window
               "window-status-style" "fg=red" win2)
              (cl-tmux/renderer::%status-window-list-styled
               sess (cl-tmux/model:session-active-window sess))))))
    ;; fg=red is SGR 31; the styled output must emit a CSI escape and "31".
    (is (search (format nil "~C[" #\Escape) with)
        "per-window fg=red must emit an SGR escape sequence (got ~S)" with)
    (is (search "31" with)
        "per-window fg=red must emit SGR code 31 for the window-2 tab (got ~S)"
        with)
    ;; Robust proof of per-window resolution: with vs without must differ.
    (is (not (string= with without))
        "per-window window-status-style override must change the output ~
         (with=~S without=~S)" with without)))

(test status-window-list-no-override-is-global
  "With NO per-window overrides, a GLOBAL window-status-style still applies to
   the window tabs.  Proves the fallback path (window -> global) is intact and
   behaviour is preserved for windows that carry no local override."
  (with-isolated-config
    ;; fg=green is SGR 32.  Set it GLOBALLY (no per-window override anywhere).
    (cl-tmux/options:set-option "window-status-style" "fg=green")
    (cl-tmux/options:set-option "window-status-current-style" "fg=green")
    (let* ((sess (make-fake-session :nwindows 2))
           (out  (cl-tmux/renderer::%status-window-list-styled
                  sess (cl-tmux/model:session-active-window sess))))
      (is (search (format nil "~C[" #\Escape) out)
          "global window-status-style must emit an SGR escape (got ~S)" out)
      (is (search "32" out)
          "global window-status-style fg=green must emit SGR code 32 for the ~
           window tabs (got ~S)" out))))

(test status-window-list-per-window-current-format-override
  "A window-local window-status-CURRENT-format on the ACTIVE (first) window is
   used for that window's tab only.  make-fake-session selects the FIRST window,
   so the override lands on the active-window branch (current-format), proving
   the current-format is read per-window and does not bleed onto the inactive
   window."
  (with-isolated-config
    (let* ((sess (make-fake-session :nwindows 2))
           (windows (cl-tmux/model:session-windows sess))
           (win1 (first windows)))   ; make-fake-session selects the FIRST window
      ;; Distinctive literal so the expansion " C0Y " is unmistakable in output.
      (cl-tmux/options:set-option-for-window
       "window-status-current-format" " C#{window_index}Y " win1)
      (let ((out (cl-tmux/renderer::%status-window-list-styled
                  sess (cl-tmux/model:session-active-window sess))))
        ;; Window 1 (active, index 0) must use its per-window current-format override.
        (is (search "C0Y" out)
            "active window 1 must use its per-window window-status-current-format ~
             (expected \"C0Y\" in ~S)" out)
        ;; The per-window override on the active window must NOT bleed onto the
        ;; inactive window 2 (index 1) — it keeps the default format.
        (is (null (search "C1Y" out))
            "the per-window current-format override on window 1 must NOT bleed ~
             onto window 2 (got ~S)" out)))))

(test status-window-list-per-window-current-style-override
  "A window-local window-status-CURRENT-style fg=red on the ACTIVE (first) window
   changes only that window's rendered output.  Comparing the 2-window list WITH
   the per-window override against the same list WITHOUT it proves the per-window
   current-style is resolved and applied to the active-window branch (the strings
   must differ)."
  ;; Capture WITHOUT any override.
  (let ((without
          (with-isolated-config
            (let ((sess (make-fake-session :nwindows 2)))
              (cl-tmux/renderer::%status-window-list-styled
               sess (cl-tmux/model:session-active-window sess)))))
        ;; Capture WITH a window-local fg=red on window 1 (active).
        (with
          (with-isolated-config
            (let* ((sess (make-fake-session :nwindows 2))
                   (win1 (first (cl-tmux/model:session-windows sess))))
              (cl-tmux/options:set-option-for-window
               "window-status-current-style" "fg=red" win1)
              (cl-tmux/renderer::%status-window-list-styled
               sess (cl-tmux/model:session-active-window sess))))))
    ;; fg=red is SGR 31; the styled output must emit a CSI escape and "31".
    (is (search (format nil "~C[" #\Escape) with)
        "per-window current-style fg=red must emit an SGR escape sequence (got ~S)" with)
    (is (search "31" with)
        "per-window current-style fg=red must emit SGR code 31 for the active ~
         window tab (got ~S)" with)
    ;; Robust proof of per-window resolution: with vs without must differ.
    (is (not (string= with without))
        "per-window window-status-current-style override must change the output ~
         (with=~S without=~S)" with without)))

;;; ── Alert-state window-tab styles (bell / activity / last) ───────────────────

(test status-window-list-bell-style-applied-to-window-with-pending-bell
  "A non-active window with a pane holding a pending BEL renders its tab with
   window-status-bell-style (fg=red → SGR 31), overriding the (empty) normal style."
  (with-isolated-config
    (cl-tmux/options:set-option "window-status-style" "")        ; normal: unstyled
    (cl-tmux/options:set-option "window-status-bell-style" "fg=red")
    (let* ((sess (make-fake-session :nwindows 2))
           (win2 (second (cl-tmux/model:session-windows sess)))
           (pane (first (cl-tmux/model:window-panes win2))))
      ;; Mark a pane in the inactive window 2 as having a pending bell.
      (setf (cl-tmux/terminal/types:screen-bell-pending (cl-tmux/model:pane-screen pane)) t)
      (let ((out (cl-tmux/renderer::%status-window-list-styled
                  sess (cl-tmux/model:session-active-window sess))))
        (is (search "31" out)
            "window with a pending bell must use window-status-bell-style (SGR 31): ~S" out)))))

(test status-window-list-activity-style-applied-to-window-with-activity
  "A non-active window with its activity-flag set renders its tab with
   window-status-activity-style (fg=blue → SGR 34)."
  (with-isolated-config
    (cl-tmux/options:set-option "window-status-style" "")
    (cl-tmux/options:set-option "window-status-activity-style" "fg=blue")
    (let* ((sess (make-fake-session :nwindows 2))
           (win2 (second (cl-tmux/model:session-windows sess))))
      (setf (cl-tmux/model:window-activity-flag win2) t)
      (let ((out (cl-tmux/renderer::%status-window-list-styled
                  sess (cl-tmux/model:session-active-window sess))))
        (is (search "34" out)
            "window with activity must use window-status-activity-style (SGR 34): ~S" out)))))

(test status-window-list-last-style-applied-to-last-window
  "The last (previously active) non-active window renders its tab with
   window-status-last-style (fg=magenta → SGR 35) when set."
  (with-isolated-config
    (cl-tmux/options:set-option "window-status-style" "")
    (cl-tmux/options:set-option "window-status-last-style" "fg=magenta")
    (let* ((sess (make-fake-session :nwindows 2)))
      ;; make-fake-session selects window 1 active, leaving window 2 as the
      ;; last (second-highest last-active-time) window.
      (is (eq (second (cl-tmux/model:session-windows sess))
              (cl-tmux/model:session-last-window sess))
          "precondition: window 2 is the last window")
      (let ((out (cl-tmux/renderer::%status-window-list-styled
                  sess (cl-tmux/model:session-active-window sess))))
        (is (search "35" out)
            "last window must use window-status-last-style (SGR 35): ~S" out)))))

(test status-window-list-bell-style-beats-activity-style
  "Alert-style precedence: a non-active window with BOTH a pending bell and the
   activity flag uses bell-style (fg=red, 31), not activity-style (fg=blue, 34)."
  (with-isolated-config
    (cl-tmux/options:set-option "window-status-style" "")
    (cl-tmux/options:set-option "window-status-bell-style" "fg=red")
    (cl-tmux/options:set-option "window-status-activity-style" "fg=blue")
    (let* ((sess (make-fake-session :nwindows 2))
           (win2 (second (cl-tmux/model:session-windows sess)))
           (pane (first (cl-tmux/model:window-panes win2))))
      (setf (cl-tmux/model:window-activity-flag win2) t)
      (setf (cl-tmux/terminal/types:screen-bell-pending (cl-tmux/model:pane-screen pane)) t)
      (let ((out (cl-tmux/renderer::%status-window-list-styled
                  sess (cl-tmux/model:session-active-window sess))))
        (is (search "31" out)
            "bell must win over activity (expect SGR 31): ~S" out)
        (is (not (search "34" out))
            "activity style (SGR 34) must NOT appear when bell takes priority: ~S" out)))))

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

(test message-style-folds-deprecated-fg-bg
  "The deprecated message-fg/message-bg/message-attr options fold into message-style
   via %fold-deprecated-style — the exact call render-overlay makes (old .tmux.conf
   compat)."
  (with-isolated-options ("message-style" "bold" "message-fg" "white" "message-bg" "blue")
    (let ((eff (cl-tmux/renderer::%fold-deprecated-style
                (cl-tmux/options:get-option "message-style" "")
                "message-fg" "message-bg" "message-attr")))
      (is (search "bold" eff)     "message-style base preserved (got ~S)" eff)
      (is (search "fg=white" eff) "message-fg folded in (got ~S)" eff)
      (is (search "bg=blue" eff)  "message-bg folded in (got ~S)" eff))))

(test render-overlay-wires-deprecated-message-bg
  "render-overlay actually applies the folded message-bg: the rendered overlay SGR
   differs from the unstyled overlay."
  (flet ((render ()
           (let ((*overlay* nil))
             (show-overlay "hello")
             (unwind-protect
                  (let ((buf (make-string-output-stream)))
                    (cl-tmux/renderer::render-overlay buf 20)
                    (get-output-stream-string buf))
               (clear-overlay)))))
    (let ((styled   (with-isolated-options ("message-style" "" "message-bg" "red")
                      (render)))
          (unstyled (with-isolated-options ("message-style" "" "message-bg" "")
                      (render))))
      (is (not (string= styled unstyled))
          "message-bg must change the overlay's rendered SGR (styled=~S unstyled=~S)"
          styled unstyled))))

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

(test status-position-top-offsets-panes-down
  "With status-position = top, window-relayout shifts the panes DOWN by the status
   height so they do not overlap the top status bar (the panes start at y=1, not 0)."
  (with-isolated-options ("status-position" "top")
    (let ((cl-tmux/config:*status-height* 1))
      (let* ((p0  (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                             :fd -1 :pid -1 :screen (make-screen 20 5)))
             (win (make-window :id 1 :name "w" :width 20 :height 6
                               :tree (make-layout-leaf p0) :panes (list p0))))
        (cl-tmux/model:window-relayout win 5 20)   ; content height 5
        (is (= 1 (pane-y p0))
            "top status must offset the pane to y=1, got ~D" (pane-y p0))
        (is (= 5 (pane-height p0)) "the pane keeps the full content height")))))

(test status-position-bottom-no-pane-offset
  "With status-position = bottom (default), panes start flush at y=0."
  (with-isolated-options ("status-position" "bottom")
    (let ((cl-tmux/config:*status-height* 1))
      (let* ((p0  (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                             :fd -1 :pid -1 :screen (make-screen 20 5)))
             (win (make-window :id 1 :name "w" :width 20 :height 6
                               :tree (make-layout-leaf p0) :panes (list p0))))
        (cl-tmux/model:window-relayout win 5 20)
        (is (= 0 (pane-y p0))
            "bottom status must leave the pane at y=0, got ~D" (pane-y p0))))))

;;; ── status on/off ────────────────────────────────────────────────────────────

(test status-segment-style-falls-back-to-base-when-unset
  "%status-segment-style-sgr returns the base SGR when the segment style is unset
   or \"default\"."
  (with-isolated-options ("status-left-style" "")
    (is (string= "44;97" (cl-tmux/renderer::%status-segment-style-sgr
                           "status-left-style" "44;97"))
        "empty status-left-style must fall back to the base SGR"))
  (with-isolated-options ("status-left-style" "default")
    (is (string= "44;97" (cl-tmux/renderer::%status-segment-style-sgr
                           "status-left-style" "44;97"))
        "\"default\" status-left-style must fall back to the base SGR")))

(test apply-segment-style-wraps-and-reverts
  "%apply-segment-style wraps TEXT in the segment SGR and reverts to the base."
  (let ((out (cl-tmux/renderer::%apply-segment-style "TEXT" "31" "44")))
    (is (search (format nil "~C[31m" #\Escape) out) "segment SGR (31) present")
    (is (search "TEXT" out) "text preserved")
    (is (search (format nil "~C[44m" #\Escape) out) "reverts to base SGR (44)")))

(test apply-segment-style-noop-when-equal-to-base
  "%apply-segment-style returns TEXT unchanged when the segment SGR equals the base."
  (is (string= "TEXT" (cl-tmux/renderer::%apply-segment-style "TEXT" "44" "44"))
      "no redundant SGR wrapping when segment style equals the base"))

(test status-left-style-applied-in-rendered-bar
  "status-left-style injects its SGR into the rendered status bar."
  (with-isolated-options ("status-left" "L" "status-right" nil "status-style" ""
                          "status-left-style" "fg=red")
    (let* ((expected (cl-tmux/renderer::%status-sgr-from-style "fg=red"))
           (sess     (make-test-session 20 5))
           (out      (with-output-to-string (s)
                       (cl-tmux/renderer::render-status-bar s sess 6 20))))
      (is (search (format nil "~C[~Am" #\Escape expected) out)
          "the rendered bar must include the status-left-style SGR (got ~S)" out))))

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

;;; ── multi-line status (status 2..5 + status-format[N]) ─────────────────────

(test status-line-count-parses-option
  "status-line-count maps the `status` option to a row count (0..5, tmux cap)."
  (flet ((n (v) (with-isolated-options ("status" v)
                  (cl-tmux/renderer::status-line-count))))
    (is (= 0 (n nil))   "nil → 0")
    (is (= 0 (n "off")) "off → 0")
    (is (= 0 (n "0"))   "0 → 0")
    (is (= 1 (n t))     "t → 1")
    (is (= 1 (n "on"))  "on → 1")
    (is (= 2 (n "2"))   "2 → 2")
    (is (= 5 (n "5"))   "5 → 5")
    (is (= 5 (n "9"))   "9 → 5 (capped at tmux maximum)")
    (is (= 2 (n 2))     "integer 2 → 2")))

(test render-session-multiline-status-shows-extra-line
  "With status=2 and status-format[1] set, render-session-to-string renders the
   extra status line's content; with status=1 it does not."
  (let ((sess (make-test-session 20 5)))
    (with-isolated-options ("status" "2" "status-format[1]" "EXTRALINE"
                            "status-left" nil "status-right" nil "status-style" "")
      (let ((out (render-session-to-string sess 8 20)))
        (is (search "EXTRALINE" out)
            "status=2 must render status-format[1] content (got ~S)" out)))
    (with-isolated-options ("status" "1" "status-format[1]" "EXTRALINE"
                            "status-left" nil "status-right" nil "status-style" "")
      (let ((out (render-session-to-string sess 8 20)))
        (is (null (search "EXTRALINE" out))
            "status=1 must NOT render the extra status line (got ~S)" out)))))

(test render-extra-status-line-blank-when-unset
  "An extra status line with no status-format[N] set renders a blank styled row
   (no crash, returns cleanly)."
  (let ((sess (make-test-session 20 5)))
    (with-isolated-options ("status" "3" "status-style" "")
      (let ((out (render-session-to-string sess 8 20)))
        ;; status=3 reserves three rows; rendering must succeed and produce output.
        (is (stringp out) "multi-line status with unset formats must still render")
        (is (plusp (length out)) "output must be non-empty")))))

(test render-extra-status-line-honours-align
  "An extra status row (status-format[N]) honours #[align=right] via the shared
   align composer — consistent with status-format[0]."
  (with-isolated-options ("status-format[1]" "L1#[align=right]R1")
    (let* ((sess (make-test-session 30 10 :content ""))
           (out  (with-output-to-string (s)
                   (cl-tmux/renderer::render-extra-status-line s sess 30 8 1)))
           (vis  (cl-ppcre:regex-replace-all
                  (format nil "~C\\[[0-9;?]*[A-Za-z]" #\Escape) out ""))
           (rpos (search "R1" vis)))
      (is (eql 0 (search "L1" vis)) "left content at column 0 (got ~S)" vis)
      (is (and rpos (= (+ rpos 2) 30))
          "right content ends at the row width (got ~S)" vis))))

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

(test render-popup-honours-border-lines-option
  "render-popup draws the box with the popup-border-lines characters (the whole
   box: corners and vertical sides), and not the single-line glyphs."
  (with-isolated-options ("popup-border-lines" "double")
    (let* ((popup (make-popup :title "T" :x 0 :y 0 :width 20 :height 6
                              :pane nil :screen nil :close-on-exit nil))
           (out   (with-output-to-string (s)
                    (cl-tmux/renderer::render-popup s popup 24 80))))
      (is (find #\╔ out) "double border draws ╔ top-left")
      (is (find #\╗ out) "double border draws ╗ top-right")
      (is (find #\╚ out) "double border draws ╚ bottom-left")
      (is (find #\╝ out) "double border draws ╝ bottom-right")
      (is (find #\║ out) "double border draws ║ vertical sides")
      (is (null (find #\┌ out)) "no single-line ┌ corner when double is set"))))

(test render-popup-honours-border-style-colour
  "render-popup wraps the popup border in the popup-border-style SGR."
  (with-isolated-options ("popup-border-style" "fg=red")
    (let* ((expected (cl-tmux/renderer:style-to-sgr
                      (cl-tmux/renderer:parse-style-string "fg=red")))
           (popup (make-popup :title "T" :x 0 :y 0 :width 20 :height 6
                              :pane nil :screen nil :close-on-exit nil))
           (out   (with-output-to-string (s)
                    (cl-tmux/renderer::render-popup s popup 24 80))))
      (is (search (format nil "~C[~Am" #\Escape expected) out)
          "the popup-border-style SGR (~S) must appear in the rendered border"
          expected))))

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

(test render-menu-applies-selected-and-item-styles
  "render-menu colours the selected item with menu-selected-style and the others
   with menu-style (when set)."
  (with-isolated-options ("menu-style" "fg=blue" "menu-selected-style" "bg=red")
    (let* ((items '(("Alpha" . nil) ("Beta" . nil)))
           (menu  (make-menu :title "M" :items items :selected-index 1))
           (out   (with-output-to-string (s)
                    (cl-tmux/renderer::render-menu s menu 24 80))))
      (is (search (format nil "~C[41m" #\Escape) out)
          "selected item must use menu-selected-style bg=red (SGR 41, got ~S)" out)
      (is (search (format nil "~C[34m" #\Escape) out)
          "non-selected items must use menu-style fg=blue (SGR 34, got ~S)" out))))

(test render-menu-no-style-emits-no-item-sgr
  "With menu-style/menu-selected-style empty (default), render-menu emits no item
   colour SGR — only the labels and box, preserving the plain appearance."
  (with-isolated-options ("menu-style" "" "menu-selected-style" "")
    (let* ((items '(("Alpha" . nil) ("Beta" . nil)))
           (menu  (make-menu :title "M" :items items :selected-index 1))
           (out   (with-output-to-string (s)
                    (cl-tmux/renderer::render-menu s menu 24 80))))
      (is (null (search (format nil "~C[41m" #\Escape) out))
          "no menu-selected-style means no bg SGR (got ~S)" out)
      (is (search "Alpha" out) "labels are still drawn (got ~S)" out))))

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

;;; ── enable/disable-extended-keys (CSI u / modifyOtherKeys) ───────────────────

(test extended-keys-level-mapping
  "extended-keys-level maps the option value to a modifyOtherKeys level or NIL."
  (is (= 1 (cl-tmux/renderer::extended-keys-level "on"))     "on → level 1")
  (is (= 2 (cl-tmux/renderer::extended-keys-level "always")) "always → level 2")
  (is (null (cl-tmux/renderer::extended-keys-level "off"))   "off → NIL")
  (is (null (cl-tmux/renderer::extended-keys-level nil))     "NIL → NIL"))

(test enable-extended-keys-on-emits-level-1
  "enable-extended-keys with \"on\" writes CSI > 4 ; 1 m and returns level 1."
  (let* ((level nil)
         (out (let ((*standard-output* (make-string-output-stream)))
                (setf level (cl-tmux/renderer::enable-extended-keys "on"))
                (get-output-stream-string *standard-output*))))
    (is (= 1 level) "returns the emitted level")
    (is (search (format nil "~C[>4;1m" #\Escape) out)
        "must emit CSI > 4 ; 1 m (got ~S)" out)))

(test enable-extended-keys-always-emits-level-2
  "enable-extended-keys with \"always\" writes CSI > 4 ; 2 m and returns level 2."
  (let* ((level nil)
         (out (let ((*standard-output* (make-string-output-stream)))
                (setf level (cl-tmux/renderer::enable-extended-keys "always"))
                (get-output-stream-string *standard-output*))))
    (is (= 2 level) "returns the emitted level")
    (is (search (format nil "~C[>4;2m" #\Escape) out)
        "must emit CSI > 4 ; 2 m (got ~S)" out)))

(test enable-extended-keys-off-emits-nothing
  "enable-extended-keys with \"off\" writes no bytes and returns NIL."
  (let* ((level :sentinel)
         (out (let ((*standard-output* (make-string-output-stream)))
                (setf level (cl-tmux/renderer::enable-extended-keys "off"))
                (get-output-stream-string *standard-output*))))
    (is (null level) "off → NIL (reporting stays off)")
    (is (string= "" out) "off must emit nothing (got ~S)" out)))

(test disable-extended-keys-emits-reset
  "disable-extended-keys writes CSI > 4 ; 0 m to reset the outer terminal."
  (let ((out (let ((*standard-output* (make-string-output-stream)))
               (cl-tmux/renderer::disable-extended-keys)
               (get-output-stream-string *standard-output*))))
    (is (search (format nil "~C[>4;0m" #\Escape) out)
        "disable-extended-keys must emit CSI > 4 ; 0 m (got ~S)" out)))

;;; ── enable/disable-focus-reporting (?1004) ───────────────────────────────────

(test enable-focus-reporting-emits-1004h
  "enable-focus-reporting writes ?1004h to enable focus events on the outer terminal."
  (let ((out (let ((*standard-output* (make-string-output-stream)))
               (cl-tmux/renderer::enable-focus-reporting)
               (get-output-stream-string *standard-output*))))
    (is (search (format nil "~C[?1004h" #\Escape) out)
        "enable-focus-reporting must emit ?1004h (got ~S)" out)))

(test disable-focus-reporting-emits-1004l
  "disable-focus-reporting writes ?1004l to disable focus events on the outer terminal."
  (let ((out (let ((*standard-output* (make-string-output-stream)))
               (cl-tmux/renderer::disable-focus-reporting)
               (get-output-stream-string *standard-output*))))
    (is (search (format nil "~C[?1004l" #\Escape) out)
        "disable-focus-reporting must emit ?1004l (got ~S)" out)))

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
