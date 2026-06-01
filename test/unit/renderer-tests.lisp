(in-package #:cl-tmux/test)

;;;; Status bar and session compositing tests.
;;;;
;;;; Covers: %status-* helpers, render-status-bar, render-overlay,
;;;;         render-session-to-string, render-session, clear-display
;;;;         from src/renderer.lisp.
;;;;
;;;; renderer-suite is declared in renderer-format-tests.lisp (loaded first).

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
  (let ((cl-tmux/options:*global-options*
         (let ((ht (make-hash-table :test #'equal)))
           (maphash (lambda (k v) (setf (gethash k ht) v))
                    cl-tmux/options:*global-options*)
           ;; Reset format options to nil (default = not configured).
           (setf (gethash "status-left" ht) nil
                 (gethash "status-right" ht) nil)
           ht)))
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
  (let ((cl-tmux/options:*global-options*
         (let ((ht (make-hash-table :test #'equal)))
           (maphash (lambda (k v) (setf (gethash k ht) v))
                    cl-tmux/options:*global-options*)
           (setf (gethash "status-left" ht) nil
                 (gethash "status-right" ht) nil)
           ht))
        (cl-tmux/prompt:*prompt* nil))
    (let* ((sess (make-test-session 40 10 :content ""))
           (out  (with-output-to-string (s)
                   (cl-tmux/renderer::render-status-bar s sess 10 40))))
      ;; window-status-current-format renders active window as " 1:1* "
      (is (search "1:1" out)
          "inactive status bar should show the window fragment 1:1 (got ~S)" out)
      (is (null (search "rename-window:" out))
          "inactive status bar must NOT show the prompt text (got ~S)" out))))

(test render-status-bar-copy-mode-indicator
  (let ((cl-tmux/options:*global-options*
         (let ((ht (make-hash-table :test #'equal)))
           (maphash (lambda (k v) (setf (gethash k ht) v))
                    cl-tmux/options:*global-options*)
           (setf (gethash "status-left" ht) nil
                 (gethash "status-right" ht) nil)
           ht)))
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
  (let ((cl-tmux/options:*global-options*
         (let ((ht (make-hash-table :test #'equal)))
           (maphash (lambda (k v) (setf (gethash k ht) v))
                    cl-tmux/options:*global-options*)
           (setf (gethash "status-left" ht) nil
                 (gethash "status-right" ht) nil)
           ht)))
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
  (let ((cl-tmux/options:*global-options*
         (let ((ht (make-hash-table :test #'equal)))
           (maphash (lambda (k v) (setf (gethash k ht) v))
                    cl-tmux/options:*global-options*)
           (setf (gethash "status-left" ht) nil
                 (gethash "status-right" ht) nil)
           ht)))
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
  (let* ((sess  (make-split-session 5 3 :v))
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
      (is (null (find #\│ out))
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

;;; ── %status-window-list (pure) ──────────────────────────────────────────────

(test status-window-list-brackets-active-window
  (let* ((sess     (make-test-session 20 5 :content ""))
         (win      (session-active-window sess))
         (out      (cl-tmux/renderer::%status-window-list sess win)))
    ;; window-status-current-format default: " #{window_index}:#{window_name}* "
    ;; window named "1" at index 1 → " 1:1* "
    (is (search "1:1" out)
        "%status-window-list should contain the active window 1:1 (got ~S)" out)
    (is (search "*" out)
        "%status-window-list should contain * marker for active window (got ~S)" out)))

(test status-window-list-two-windows-formats-both
  ;; Build a 2-window session manually; second window is active.
  (let* ((s0  (make-screen 10 5))
         (p0  (make-pane :id 1 :x 0 :y 0 :width 10 :height 5 :fd -1 :screen s0))
         (w0  (make-window :id 1 :name "alpha" :width 10 :height 5 :panes (list p0)))
         (s1  (make-screen 10 5))
         (p1  (make-pane :id 2 :x 0 :y 0 :width 10 :height 5 :fd -1 :screen s1))
         (w1  (make-window :id 2 :name "beta"  :width 10 :height 5 :panes (list p1)))
         (sess (make-session :id 1 :name "0" :windows (list w0 w1))))
    (window-select-pane w0 p0)
    (window-select-pane w1 p1)
    (session-select-window sess w1)
    ;; Active window is beta → rendered with window-status-current-format "index:name*"
    ;; Inactive window alpha → rendered with window-status-format "index:name"
    (let ((out (cl-tmux/renderer::%status-window-list sess w1)))
      ;; Active window "beta" gets the current-format with "*"
      (is (search "beta*" out)
          "%status-window-list should mark active window beta with * (got ~S)" out)
      ;; Inactive window "alpha" appears without "*"
      (is (search "alpha" out)
          "%status-window-list should include the inactive window alpha (got ~S)" out)
      ;; alpha should NOT have the asterisk marker
      (is (null (search "alpha*" out))
          "%status-window-list must NOT mark inactive window alpha with * (got ~S)" out))))

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
      (is (search "1" left) "window name '1' must appear in left text"))))

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
  ;; Isolate the global options table so this test does not bleed into others.
  (let ((cl-tmux/options:*global-options*
         (let ((ht (make-hash-table :test #'equal)))
           (maphash (lambda (k v) (setf (gethash k ht) v))
                    cl-tmux/options:*global-options*)
           ht)))
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
  (let ((cl-tmux/options:*global-options*
         (let ((ht (make-hash-table :test #'equal)))
           (maphash (lambda (k v) (setf (gethash k ht) v))
                    cl-tmux/options:*global-options*)
           ht)))
    (cl-tmux/options:set-option "status-right" "win:#{window_name}")
    (let* ((sess (make-test-session 60 10 :content ""))
           (out  (with-output-to-string (s)
                   (cl-tmux/renderer::render-status-bar s sess 10 60))))
      (is (search "win:1" out)
          "status-right #{window_name} must expand to the window name '1' (got ~S)" out))))

;;; ── status-position top/bottom ───────────────────────────────────────────────

(test status-position-bottom-default
  "With status-position = bottom (default), the status bar appears at the last row."
  (let ((cl-tmux/options:*global-options*
         (let ((ht (make-hash-table :test #'equal)))
           (maphash (lambda (k v) (setf (gethash k ht) v))
                    cl-tmux/options:*global-options*)
           (setf (gethash "status-position" ht) "bottom"
                 (gethash "status-left" ht) nil
                 (gethash "status-right" ht) nil)
           ht)))
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
  (let ((cl-tmux/options:*global-options*
         (let ((ht (make-hash-table :test #'equal)))
           (maphash (lambda (k v) (setf (gethash k ht) v))
                    cl-tmux/options:*global-options*)
           (setf (gethash "status-position" ht) "top"
                 (gethash "status-left" ht) nil
                 (gethash "status-right" ht) nil)
           ht)))
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
  (let ((cl-tmux/options:*global-options*
         (let ((ht (make-hash-table :test #'equal)))
           (maphash (lambda (k v) (setf (gethash k ht) v))
                    cl-tmux/options:*global-options*)
           (setf (gethash "status" ht) nil)
           ht)))
    (let* ((sess (make-test-session 20 5))
           (out  (render-session-to-string sess 6 20)))
      ;; With status=nil, the default blue SGR "44;97m" should not appear
      (is (null (search (format nil "~C[44;97m" #\Escape) out))
          "status=nil must suppress the status bar blue background (got ~S)" out))))

(test status-on-shows-status-bar
  "When the status option is true (default), render-session-to-string emits a status bar."
  (let ((cl-tmux/options:*global-options*
         (let ((ht (make-hash-table :test #'equal)))
           (maphash (lambda (k v) (setf (gethash k ht) v))
                    cl-tmux/options:*global-options*)
           (setf (gethash "status" ht) t
                 (gethash "status-left" ht) nil
                 (gethash "status-right" ht) nil
                 (gethash "status-style" ht) "")
           ht)))
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
  (let ((cl-tmux/options:*global-options*
         (let ((ht (make-hash-table :test #'equal)))
           (maphash (lambda (k v) (setf (gethash k ht) v))
                    cl-tmux/options:*global-options*)
           ht)))
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
  (let ((cl-tmux/options:*global-options*
         (let ((ht (make-hash-table :test #'equal)))
           (maphash (lambda (k v) (setf (gethash k ht) v))
                    cl-tmux/options:*global-options*)
           ht)))
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
  (let ((cl-tmux/options:*global-options*
         (let ((ht (make-hash-table :test #'equal)))
           (maphash (lambda (k v) (setf (gethash k ht) v))
                    cl-tmux/options:*global-options*)
           ht)))
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
  (let ((cl-tmux/options:*global-options*
         (let ((ht (make-hash-table :test #'equal)))
           (maphash (lambda (k v) (setf (gethash k ht) v))
                    cl-tmux/options:*global-options*)
           ;; Use nil so status-left falls through to %status-left-text
           ;; which renders the window list via %status-window-list.
           (setf (gethash "status-left" ht) nil
                 (gethash "status-right" ht) nil
                 (gethash "window-status-format" ht) "WIN:#{window_name}"
                 (gethash "window-status-current-format" ht) "[#{window_name}]")
           ht)))
    ;; Build 2-window session; 2nd window is inactive
    (let* ((s0 (make-screen 80 5))
           (p0 (make-pane :id 1 :x 0 :y 0 :width 80 :height 5 :fd -1 :screen s0))
           (w0 (make-window :id 1 :name "alpha" :width 80 :height 5 :panes (list p0)))
           (s1 (make-screen 80 5))
           (p1 (make-pane :id 2 :x 0 :y 0 :width 80 :height 5 :fd -1 :screen s1))
           (w1 (make-window :id 2 :name "beta"  :width 80 :height 5 :panes (list p1)))
           (sess (make-session :id 1 :name "0" :windows (list w0 w1))))
      (window-select-pane w0 p0)
      (window-select-pane w1 p1)
      (session-select-window sess w0)      ; alpha is active
      (let ((out (with-output-to-string (s)
                   (cl-tmux/renderer::render-status-bar s sess 11 80))))
        (is (search "[alpha]" out)
            "active window must use window-status-current-format [alpha] (got ~S)" out)
        (is (search "WIN:beta" out)
            "inactive window must use window-status-format WIN:beta (got ~S)" out)))))

;;; ── window-status-separator ──────────────────────────────────────────────────

(test window-status-separator-used-between-windows
  "window-status-separator is placed between window entries."
  (let ((cl-tmux/options:*global-options*
         (let ((ht (make-hash-table :test #'equal)))
           (maphash (lambda (k v) (setf (gethash k ht) v))
                    cl-tmux/options:*global-options*)
           ;; Use nil so status-left falls through to %status-left-text
           ;; which renders the window list via %status-window-list.
           (setf (gethash "status-left" ht) nil
                 (gethash "status-right" ht) nil
                 (gethash "window-status-separator" ht) "|SEP|")
           ht)))
    (let* ((s0 (make-screen 80 5))
           (p0 (make-pane :id 1 :x 0 :y 0 :width 80 :height 5 :fd -1 :screen s0))
           (w0 (make-window :id 1 :name "a" :width 80 :height 5 :panes (list p0)))
           (s1 (make-screen 80 5))
           (p1 (make-pane :id 2 :x 0 :y 0 :width 80 :height 5 :fd -1 :screen s1))
           (w1 (make-window :id 2 :name "b" :width 80 :height 5 :panes (list p1)))
           (sess (make-session :id 1 :name "0" :windows (list w0 w1))))
      (window-select-pane w0 p0)
      (window-select-pane w1 p1)
      (session-select-window sess w0)
      (let ((out (with-output-to-string (s)
                   (cl-tmux/renderer::render-status-bar s sess 11 80))))
        (is (search "|SEP|" out)
            "window-status-separator |SEP| must appear between windows (got ~S)" out)))))

;;; ── *status-dirty* and start-status-timer ────────────────────────────────────

(test status-dirty-var-exists
  "*status-dirty* is bound in the cl-tmux package."
  (is (boundp 'cl-tmux::*status-dirty*)
      "*status-dirty* must be bound"))

(test status-timer-thread-var-exists
  "*status-timer-thread* is bound in the cl-tmux package."
  (is (boundp 'cl-tmux::*status-timer-thread*)
      "*status-timer-thread* must be bound"))

(test start-status-timer-is-fbound
  "start-status-timer is a defined function."
  (is (fboundp 'cl-tmux::start-status-timer)
      "start-status-timer must be fbound"))
