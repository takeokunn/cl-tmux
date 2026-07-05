(in-package #:cl-tmux/test)

;;;; render-status-bar, render-session, clear-display, and status-pane indicators

(in-suite renderer-suite)

;;; ── Test fixtures ───────────────────────────────────────────────────────────
;;;
;;; make-renderer-test-session is defined in tests/helpers-renderer-fixtures.lisp
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
  "render-status-bar shows the session name and the active window's index:name."
  (with-minimal-status-bar-options
    (let* ((sess (make-renderer-test-session 40 10 :content ""))
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
    (let* ((sess (make-renderer-test-session 40 10 :content ""))
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
    (let* ((sess (make-renderer-test-session 40 10 :content ""))
           (out  (render-status-bar-output sess 10 40))
           (vis  (cl-ppcre:regex-replace-all
                  (format nil "~C\\[[0-9;?]*[A-Za-z]" #\Escape) out "")))
      (is (search "[" vis) "the #{W:} window list renders the bracketed window entry (got ~S)" vis))))

(test status-bar-no-prompt-when-inactive
  "With *prompt* explicitly inactive, the status bar shows the normal status
   (window 1) and never the prompt text — pinning the active/inactive exclusion."
  (with-minimal-status-bar-options
    (let ((cl-tmux/prompt:*prompt* nil))
      (let* ((sess (make-renderer-test-session 40 10 :content ""))
             (out  (render-status-bar-output sess 10 40)))
        ;; window-status-current-format renders active window as " 1:1* "
        (is (search "1:1" out)
            "inactive status bar should show the window fragment 1:1 (got ~S)" out)
        (is (null (search "rename-window:" out))
            "inactive status bar must NOT show the prompt text (got ~S)" out)))))

(test render-status-bar-copy-mode-has-no-indicator
  "The status bar does not show a COPY/offset indicator when a pane is in copy mode."
  (with-minimal-status-bar-options
    (let* ((sess   (make-renderer-test-session 60 10 :content ""))
           (ap     (session-active-pane sess))
           (screen (pane-screen ap)))
      (setf (screen-copy-mode-p screen) t
            (screen-copy-offset screen) 3)
      (let ((out (render-status-bar-output sess 10 60)))
        (is (null (search "COPY" out))
            "status bar should not show the old COPY indicator in copy mode (got ~S)" out)
        (is (null (search "+3" out))
            "status bar should not show the old copy offset +3 in copy mode (got ~S)" out)))))

(test render-status-bar-no-copy-indicator-live
  "The status bar never shows a COPY indicator for a pane that is not in copy mode."
  (let* ((sess (make-renderer-test-session 60 10 :content ""))
         (out  (render-status-bar-output sess 10 60)))
    (is (not (search "COPY" out))
        "live status bar should NOT show the COPY indicator (got ~S)" out)))

(test render-status-bar-truncates-long-line
  "On a narrow terminal, the status bar's visible content is clamped to the terminal width."
  ;; A very narrow terminal forces the status line to be truncated via subseq.
  ;; The bar is: move-to, ESC[44;97m, <status content>, ESC[0m.  The visible
  ;; status content sits between the colour SGR and the trailing reset, and the
  ;; renderer guarantees it is no longer than the terminal width.
  (with-minimal-status-bar-options
    (let* ((width  8)
           (sess   (make-renderer-test-session width 10 :content ""))
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
         (let* ((sess (make-renderer-test-session 60 10 :content ""))
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
  "render-session-to-string emits pane content plus cursor-hide/show sequences and the status bar."
  (with-minimal-status-bar-options
    (let* ((sess (make-renderer-test-session 20 5 :content "hi"))
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
  "A side-by-side split renders a vertical separator, highlights the active pane's border, and shows both panes' content."
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
  "A stacked (top/bottom) split renders a horizontal separator and shows both panes' content."
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
  "render-session (unlike render-session-to-string) writes its frame directly to *standard-output*."
  (let ((out (let ((*standard-output* (make-string-output-stream)))
               (render-session (make-renderer-test-session 10 4 :content "hi") 5 10)
               (get-output-stream-string *standard-output*))))
    (is (plusp (length out))
        "render-session should write a non-empty frame to *standard-output*")
    (is (find #\h out)
        "render-session output should contain content char h (got ~S)" out)))

;;; ── clear-display ───────────────────────────────────────────────────────────

(test clear-display-emits-clear-and-home
  "clear-display writes the ANSI erase-screen (ESC[2J) and cursor-home (ESC[H) sequences."
  (let ((out (let ((*standard-output* (make-string-output-stream)))
               (clear-display)
               (get-output-stream-string *standard-output*))))
    (is (search (format nil "~C[2J" #\Escape) out)
        "clear-display should emit ESC[2J (got ~S)" out)
    (is (search (format nil "~C[H" #\Escape) out)
        "clear-display should emit ESC[H (got ~S)" out)))

;;; ── %status-pane-indicator (pure) ───────────────────────────────────────────

(test status-pane-indicator-with-active-pane
  "%status-pane-indicator formats a live pane as #<pane-id>."
  (let* ((screen (make-screen 10 5))
         (pane   (make-pane :id 7 :x 0 :y 0 :width 10 :height 5 :fd -1 :screen screen))
         (out    (cl-tmux/renderer::%status-pane-indicator pane)))
    (is (search "#7" out)
        "%status-pane-indicator should contain the pane id (got ~S)" out)))

(test status-pane-indicator-nil-returns-empty
  "%status-pane-indicator returns the empty string for a NIL pane."
  (is (string= "" (cl-tmux/renderer::%status-pane-indicator nil))
      "%status-pane-indicator with nil should return empty string"))
