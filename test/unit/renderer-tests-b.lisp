(in-package #:cl-tmux/test)

;;;; status-bar format, position, on/off, multi-line, BEL, parse-style, render-popup/menu — part II

(in-suite renderer-suite)

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

(test status-position-pane-offset-table
  "window-relayout offsets pane y by 1 for status-position=top, leaves y=0 for bottom."
  (dolist (c '(("top"    1 "top → pane at y=1")
               ("bottom" 0 "bottom → pane at y=0")))
    (destructuring-bind (pos expected-y desc) c
      (with-isolated-options ("status-position" pos)
        (let ((cl-tmux/config:*status-height* 1))
          (let* ((p0  (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                                 :fd -1 :pid -1 :screen (make-screen 20 5)))
                 (win (make-window :id 1 :name "w" :width 20 :height 6
                                   :tree (make-layout-leaf p0) :panes (list p0))))
            (cl-tmux/model:window-relayout win 5 20)
            (is (= expected-y (pane-y p0)) "~A (got ~D)" desc (pane-y p0))
            (is (= 5 (pane-height p0)) "~A: height must remain 5" desc)))))))

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
