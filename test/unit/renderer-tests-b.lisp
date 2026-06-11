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

(test render-popup-style-colours-empty-body
  "popup-style colours the empty popup interior; with it unset the body has no SGR."
  (let ((popup (make-popup :title "T" :x 0 :y 0 :width 20 :height 6
                           :pane nil :screen nil :close-on-exit nil)))
    (flet ((render () (with-output-to-string (s)
                        (cl-tmux/renderer::render-popup s popup 24 80))))
      (with-isolated-options ("popup-style" "bg=blue")
        (is (search (format nil "~C[44m" #\Escape) (render))
            "popup-style bg=blue must colour the body (SGR 44)"))
      (with-isolated-options ("popup-style" "")
        (is (null (search (format nil "~C[44m" #\Escape) (render)))
            "no popup-style means no body bg SGR")))))

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

(test render-menu-border-lines-selects-glyphs
  "menu-border-lines \"double\" draws the menu box with double-line glyphs (the
   sides too); the default \"single\" uses ┌│└."
  (with-isolated-options ("menu-border-lines" "double")
    (let* ((items '(("Alpha" . nil) ("Beta" . nil)))
           (menu  (make-menu :title "M" :items items :selected-index 0))
           (out   (with-output-to-string (s)
                    (cl-tmux/renderer::render-menu s menu 24 80))))
      (is (find (code-char #x2554) out) "double → top-left ╔ (got ~S)" out)
      (is (find (code-char #x2551) out) "double → vertical side ║ (got ~S)" out)
      (is (null (find (code-char #x250C) out)) "no single ┌ when double (got ~S)" out))))

(test render-menu-border-style-colours-border
  "menu-border-style colours the menu box border SGR."
  (with-isolated-options ("menu-border-style" "fg=red")
    (let* ((items '(("Alpha" . nil)))
           (menu  (make-menu :title "M" :items items :selected-index 0))
           (out   (with-output-to-string (s)
                    (cl-tmux/renderer::render-menu s menu 24 80))))
      (is (search (format nil "~C[31m" #\Escape) out)
          "menu-border-style fg=red must emit SGR 31 (got ~S)" out))))
