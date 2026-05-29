(in-package #:cl-tmux/test)

;;;; Escape-code renderer tests.
;;;;
;;;; Exercises the renderer purely through string composition
;;;; (cl-tmux/renderer:render-session-to-string and the internal helpers)
;;;; so no real terminal or live PTY is required.  Panes are built with a
;;;; fake fd (-1); the screen is fed content directly with the helpers from
;;;; helpers.lisp / terminal-tests.lisp.

(def-suite renderer-suite :description "Escape-code renderer")
(in-suite renderer-suite)

;;; ── Test fixtures ───────────────────────────────────────────────────────────

(defun make-test-session (w h &key (content ""))
  "A 1-window, 1-pane session whose pane screen has CONTENT fed into it.
   No PTY is allocated (fd -1), so this is safe in any environment."
  (let* ((screen (make-screen w h))
         (pane   (make-pane :id 1 :x 0 :y 0 :width w :height h :fd -1 :screen screen))
         (win    (make-window :id 1 :name "1" :width w :height h :panes (list pane)))
         (sess   (make-session :id 1 :name "0" :windows (list win))))
    (window-select-pane win pane)
    (session-select-window sess win)
    (unless (string= content "") (feed screen content))
    sess))

(defun make-split-session (w h direction)
  "A 1-window session split into two side-by-side / stacked panes (fd -1, no PTY).
   DIRECTION is :vertical or :horizontal; window-layout is set so the renderer
   draws separators.  The FIRST pane is active."
  (let* ((s0 (make-screen w h))
         (s1 (make-screen w h))
         (p0 (make-pane :id 1 :x 0 :y 0 :width w :height h :fd -1 :screen s0))
         (p1 (if (eq direction :vertical)
                 (make-pane :id 2 :x (1+ w) :y 0 :width w :height h :fd -1 :screen s1)
                 (make-pane :id 2 :x 0 :y (1+ h) :width w :height h :fd -1 :screen s1)))
         (win (make-window :id 1 :name "1"
                           :width  (if (eq direction :vertical) (+ (* 2 w) 1) w)
                           :height (if (eq direction :vertical) h (+ (* 2 h) 1))
                           :panes (list p0 p1) :layout direction))
         (sess (make-session :id 1 :name "0" :windows (list win))))
    (window-select-pane win p0)
    (session-select-window sess win)
    sess))

;;; ── move-to (1-based conversion) ────────────────────────────────────────────

(test move-to-is-one-based
  (is (string= (format nil "~C[1;1H" #\Escape)
               (with-output-to-string (s)
                 (cl-tmux/renderer::move-to s 0 0)))
      "move-to 0,0 should emit ESC[1;1H")
  (is (string= (format nil "~C[3;5H" #\Escape)
               (with-output-to-string (s)
                 (cl-tmux/renderer::move-to s 2 4)))
      "move-to 2,4 should emit ESC[3;5H"))

;;; ── render-cell-attrs (SGR codes) ───────────────────────────────────────────

(defun cell-attrs-string (fg bg attrs)
  (with-output-to-string (s)
    (cl-tmux/renderer::render-cell-attrs s fg bg attrs)))

(test render-cell-attrs-foreground
  (let ((out (cell-attrs-string 1 0 0)))
    (is (search ";31" out) "fg 1 should emit ;31 (got ~S)" out)))

(test render-cell-attrs-background
  (let ((out (cell-attrs-string 0 2 0)))
    (is (search ";42" out) "bg 2 should emit ;42 (got ~S)" out)))

(test render-cell-attrs-bold
  (let ((out (cell-attrs-string 0 0 1)))      ; bit0 = bold
    (is (search ";1" out) "bold (attrs bit0) should emit ;1 (got ~S)" out)))

(test render-cell-attrs-reverse
  (let ((out (cell-attrs-string 0 0 4)))      ; bit2 = reverse video
    (is (search ";7" out) "reverse (attrs bit2) should emit ;7 (got ~S)" out)))

(test render-cell-attrs-bright-foreground
  (let ((out (cell-attrs-string 9 0 0)))      ; bright fg uses 82+fg => 91
    (is (search ";91" out) "bright fg 9 should emit ;91 (got ~S)" out)))

(test render-cell-attrs-frame
  (let ((out (cell-attrs-string 1 2 1)))
    (is (eql 0 (search (format nil "~C[0" #\Escape) out))
        "render-cell-attrs should start with ESC[0 (got ~S)" out)
    (is (char= #\m (char out (1- (length out))))
        "render-cell-attrs should end with m (got ~S)" out)))

;;; ── render-pane (content + positioning) ─────────────────────────────────────

(test render-pane-content-and-positioning
  (let* ((sess (make-test-session 5 2 :content "hi"))
         (pane (first (window-panes (session-active-window sess))))
         (out  (with-output-to-string (s)
                 (cl-tmux/renderer::render-pane s pane))))
    (is (find #\h out) "render-pane should emit the h glyph (got ~S)" out)
    (is (find #\i out) "render-pane should emit the i glyph (got ~S)" out)
    ;; Row 0 of the pane is positioned via move-to row 0 => ESC[1;1H.
    (is (search (format nil "~C[1;1H" #\Escape) out)
        "render-pane should position row 0 with ESC[1;1H (got ~S)" out)))

;;; ── double-width glyphs are not double-printed ──────────────────────────────

(test render-pane-double-width-not-duplicated
  (let* ((screen (make-screen 5 2))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 5 :height 2 :fd -1 :screen screen)))
    (cl-tmux/test::utf8-feed screen "あ")     ; one wide glyph + width-0 continuation
    (let ((out (with-output-to-string (s)
                 (cl-tmux/renderer::render-pane s pane))))
      ;; The continuation cell (width 0) must be skipped: exactly one wide glyph,
      ;; and no placeholder char inflating the output.
      (is (= 1 (count #\あ out))
          "exactly one wide glyph should be printed (got ~D in ~S)"
          (count #\あ out) out))))

;;; ── render-status-bar ───────────────────────────────────────────────────────

(test render-status-bar-shows-names
  (let* ((sess (make-test-session 40 10 :content ""))
         (out  (with-output-to-string (s)
                 (cl-tmux/renderer::render-status-bar s sess 10 40))))
    (is (search "0" out) "status bar should contain the session name 0 (got ~S)" out)
    ;; The active window is formatted as " [NAME]" — search for the bracketed
    ;; fragment so a stray digit inside an SGR code can't pass this check.
    (is (search "[1]" out)
        "status bar should contain the active-window fragment [1] (got ~S)" out)))

(test status-bar-no-prompt-when-inactive
  "With *prompt* explicitly inactive, the status bar shows the normal status
   ([1]) and never the prompt text — pinning the active/inactive exclusion."
  (let ((cl-tmux/prompt:*prompt* nil))
    (let* ((sess (make-test-session 40 10 :content ""))
           (out  (with-output-to-string (s)
                   (cl-tmux/renderer::render-status-bar s sess 10 40))))
      (is (search "[1]" out)
          "inactive status bar should show the normal active-window fragment [1] (got ~S)" out)
      (is (null (search "rename-window:" out))
          "inactive status bar must NOT show the prompt text (got ~S)" out))))

;;; ── render-session-to-string (full frame) ───────────────────────────────────

(test render-session-to-string-full-frame
  (let* ((sess (make-test-session 20 5 :content "hi"))
         (out  (render-session-to-string sess 6 20)))
    (is (find #\h out) "frame should contain h from content (got ~S)" out)
    (is (find #\i out) "frame should contain i from content (got ~S)" out)
    (is (search (format nil "~C[?25l" #\Escape) out)
        "frame should hide the cursor with ESC[?25l (got ~S)" out)
    (is (search (format nil "~C[?25h" #\Escape) out)
        "frame should show the cursor with ESC[?25h (got ~S)" out)
    ;; The active window is formatted as " [NAME]"; assert the bracketed
    ;; fragment instead of a bare digit (digits also occur in SGR codes).
    (is (search "[1]" out)
        "frame should contain the active-window fragment [1] (got ~S)" out)))

;;; ── clear-display ───────────────────────────────────────────────────────────

(test clear-display-emits-clear-and-home
  (let ((out (let ((*standard-output* (make-string-output-stream)))
               (clear-display)
               (get-output-stream-string *standard-output*))))
    (is (search (format nil "~C[2J" #\Escape) out)
        "clear-display should emit ESC[2J (got ~S)" out)
    (is (search (format nil "~C[H" #\Escape) out)
        "clear-display should emit ESC[H (got ~S)" out)))

;;; ── render-vertical-border ──────────────────────────────────────────────────

(test render-vertical-border-active-highlights-green
  (let* ((screen (make-screen 5 3))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 5 :height 3 :fd -1 :screen screen))
         (green  (format nil "~C[32m" #\Escape))
         (active (with-output-to-string (s)
                   (cl-tmux/renderer::render-vertical-border s pane t)))
         (inactive (with-output-to-string (s)
                     (cl-tmux/renderer::render-vertical-border s pane nil))))
    (is (search green active)
        "active vertical border should emit green SGR ESC[32m (got ~S)" active)
    (is (find #\│ active)
        "active vertical border should draw the bar char │ (got ~S)" active)
    (is (not (search green inactive))
        "inactive vertical border should NOT emit green SGR ESC[32m (got ~S)" inactive)
    (is (find #\│ inactive)
        "inactive vertical border should still draw the bar char │ (got ~S)" inactive)))

;;; ── render-horizontal-border ────────────────────────────────────────────────

(test render-horizontal-border-draws-bar
  (let* ((screen (make-screen 5 3))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 5 :height 3 :fd -1 :screen screen))
         (out    (with-output-to-string (s)
                   (cl-tmux/renderer::render-horizontal-border s pane 40))))
    (is (find #\─ out)
        "horizontal border should draw the bar char ─ (got ~S)" out)))

;;; ── render-session-to-string with splits ────────────────────────────────────

(test render-session-vertical-split-emits-separators
  (let* ((sess  (make-split-session 5 3 :vertical))
         (win   (session-active-window sess))
         (panes (window-panes win))
         (green (format nil "~C[32m" #\Escape)))
    (feed (pane-screen (first  panes)) "AAA")
    (feed (pane-screen (second panes)) "BBB")
    (let ((out (render-session-to-string sess 3 11)))   ; full width = 2*5+1
      (is (find #\│ out)
          "vertical split frame should contain a vertical separator │ (got ~S)" out)
      ;; pane 0 is active and non-last, so its right border is highlighted.
      (is (search green out)
          "vertical split frame should highlight the active pane border (got ~S)" out)
      (is (find #\A out)
          "vertical split frame should contain pane 0 content A (got ~S)" out)
      (is (find #\B out)
          "vertical split frame should contain pane 1 content B (got ~S)" out))))

(test render-session-horizontal-split-emits-separators
  (let* ((sess  (make-split-session 5 3 :horizontal))
         (win   (session-active-window sess))
         (panes (window-panes win)))
    (feed (pane-screen (first  panes)) "AAA")
    (feed (pane-screen (second panes)) "BBB")
    (let ((out (render-session-to-string sess 7 5)))    ; full height = 2*3+1
      (is (find #\─ out)
          "horizontal split frame should contain a horizontal separator ─ (got ~S)" out)
      (is (find #\A out)
          "horizontal split frame should contain pane 0 content A (got ~S)" out)
      (is (find #\B out)
          "horizontal split frame should contain pane 1 content B (got ~S)" out))))

;;; ── render-status-bar copy-mode indicator ───────────────────────────────────

(test render-status-bar-copy-mode-indicator
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
          "status bar should show the copy offset +3 (got ~S)" out))))

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
  (let* ((width  8)
         (sess   (make-test-session width 10 :content ""))
         (out    (with-output-to-string (s)
                   (cl-tmux/renderer::render-status-bar s sess 10 width)))
         (color  (format nil "~C[44;97m" #\Escape))
         (reset  (format nil "~C[0m" #\Escape))
         (start  (+ (search color out) (length color)))
         (end    (search reset out :start2 start))
         (content (subseq out start end)))
    (is (<= (length content) width)
        "narrow status content must fit in ~D cols (got ~D: ~S)"
        width (length content) content)
    ;; The full line (left text + gap + time) is longer than the terminal, so
    ;; the right-hand time string is truncated off the visible content.
    (is (not (search ":" content))
        "narrow status content should be truncated before the HH:MM time (got ~S)"
        content)))

;;; ── render-cell-attrs extra branches ────────────────────────────────────────

(test render-cell-attrs-dim
  (let ((out (cell-attrs-string 7 0 2)))      ; bit1 = dim
    (is (search ";2" out) "dim (attrs bit1) should emit ;2 (got ~S)" out)))

(test render-cell-attrs-default-color-omitted
  ;; fg/bg outside 0..15 emit no colour code — only the leading reset remains.
  (let ((out (cell-attrs-string -1 -1 0)))
    (is (string= (format nil "~C[0m" #\Escape) out)
        "out-of-range fg/bg should omit colour codes (got ~S)" out)))

;;; ── render-session writes to *standard-output* ──────────────────────────────

(test render-session-writes-to-standard-output
  (let ((out (let ((*standard-output* (make-string-output-stream)))
               (render-session (make-test-session 10 4 :content "hi") 5 10)
               (get-output-stream-string *standard-output*))))
    (is (plusp (length out))
        "render-session should write a non-empty frame to *standard-output*")
    (is (find #\h out)
        "render-session output should contain content char h (got ~S)" out)))

;;; ── list-keys overlay ───────────────────────────────────────────────────────

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

;;; ── status bar with an active input prompt ──────────────────────────────────

(test render-status-bar-active-prompt-replaces-left-segment
  "An active *prompt* replaces the whole left status segment with its
   \"LABEL: BUFFER\" text — the prompt text appears and the normal
   window-list fragment [1] is absent."
  (let ((cl-tmux/prompt:*prompt* nil))
    (cl-tmux/prompt:prompt-start "rename-window" "abc" nil)
    (unwind-protect
         (let* ((sess (make-test-session 60 10 :content ""))
                (out  (with-output-to-string (s)
                        (cl-tmux/renderer::render-status-bar s sess 10 60))))
           ;; prompt-text formats as "LABEL: BUFFER".
           (is (search "rename-window: abc" out)
               "active-prompt status bar should show the prompt text (got ~S)" out)
           (is (null (search "[1]" out))
               "active-prompt status bar must NOT show the window-list fragment [1] (got ~S)" out))
      (cl-tmux/prompt:prompt-clear))))

;;; ── border clamping at the terminal edges ───────────────────────────────────

(test render-horizontal-border-clamps-at-right-edge
  "render-horizontal-border clamps its bar width to (- terminal-cols pane-x):
   a pane wider than the space remaining to the right edge draws no char past
   the edge, and a pane flush against the edge draws nothing."
  ;; Pane at x=0 width 10, but terminal is only 6 cols wide → pw clamps to 6.
  (let* ((screen (make-screen 10 3))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 3 :fd -1 :screen screen))
         (out    (with-output-to-string (s)
                   (cl-tmux/renderer::render-horizontal-border s pane 6))))
    (is (= 6 (count #\─ out))
        "horizontal border should clamp to terminal-cols=6 chars (got ~D in ~S)"
        (count #\─ out) out))
  ;; Pane whose x equals terminal-cols: (- cols x) <= 0 → no bar char emitted.
  (let* ((screen (make-screen 5 3))
         (pane   (make-pane :id 1 :x 6 :y 0 :width 5 :height 3 :fd -1 :screen screen))
         (out    (with-output-to-string (s)
                   (cl-tmux/renderer::render-horizontal-border s pane 6))))
    (is (zerop (count #\─ out))
        "horizontal border flush against the right edge should draw no bar char (got ~S)" out)))

(test render-session-vertical-border-suppressed-at-edge
  "In render-session-to-string the vertical separator is drawn only when the
   border column is strictly inside the terminal width.  A split whose first
   pane's right edge lands exactly at terminal-cols suppresses the │ bar."
  (let* ((sess  (make-split-session 5 3 :vertical))
         (win   (session-active-window sess))
         (panes (window-panes win)))
    (feed (pane-screen (first  panes)) "AAA")
    (feed (pane-screen (second panes)) "BBB")
    ;; First pane is x=0 width=5, so its border column is 5.  Render with
    ;; terminal-cols=5 → (< 5 5) is false → the vertical border is suppressed.
    (let ((out (render-session-to-string sess 3 5)))
      (is (null (find #\│ out))
          "vertical border at the terminal edge should be suppressed (got ~S)" out))))
