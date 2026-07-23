(in-package #:cl-tmux/test)

;;;; status-bar format, position, on/off, multi-line, BEL, parse-style, render-popup/menu — part II

(describe "renderer-suite"

  ;;; ── status-bar format string with #{session_name} ────────────────────────────

  ;; When the status-left option is set to a #{session_name} format string,
  ;; render-status-bar expands it and the rendered output contains the actual
  ;; session name rather than the literal variable syntax.
  (it "render-status-bar-custom-status-left-format-expands-session-name"
    (with-isolated-options ()
      (cl-tmux/options:set-option "status-left" "sess:#{session_name}")
      (let* ((sess (make-renderer-test-session 60 10 :content ""))
             (out  (render-status-bar-output sess 10 60)))
        (expect (search "sess:0" out))
        (expect (null (search "#{session_name}" out))))))

  ;; When status-right is set to #{window_name}, the rendered bar contains the
  ;; active window name instead of the default HH:MM clock.
  (it "render-status-bar-custom-status-right-format-expands-window-name"
    (with-isolated-options ()
      (cl-tmux/options:set-option "status-right" "win:#{window_name}")
      (let* ((sess (make-renderer-test-session 60 10 :content ""))
             (out  (render-status-bar-output sess 10 60)))
        (expect (search "win:1" out)))))

  ;;; ── status-position top/bottom ───────────────────────────────────────────────

  ;; With status-position = bottom (default), the status bar appears at the last row.
  (it "status-position-bottom-default"
    (with-empty-status-bar-options ("status-position" "bottom")
      (let* ((sess (make-renderer-test-session 20 5))
             (rows 6)
             (out  (render-status-bar-output sess rows 20)))
        ;; The status bar emits ESC[row;colH where row is 1-based.
        ;; Bottom row = rows-1 = 5, so ESC[6;1H
        (expect (search (format nil "~C[6;1H" #\Escape) out)))))

  ;; With status-position = top, the status bar appears at row 0 (ESC[1;1H).
  (it "status-position-top"
    (with-empty-status-bar-options ("status-position" "top")
      (let* ((sess (make-renderer-test-session 20 5))
             (out  (render-status-bar-output sess 6 20 :status-row 0)))
        ;; ESC[1;1H is row=0, col=0 (1-based = row 1, col 1)
        (expect (search (format nil "~C[1;1H" #\Escape) out)))))

  ;; window-relayout offsets pane y by 1 for status-position=top, leaves y=0 for bottom.
  (it "status-position-pane-offset-table"
    (dolist (c '(("top"    1 "top → pane at y=1")
                 ("bottom" 0 "bottom → pane at y=0")))
      (destructuring-bind (pos expected-y desc) c
        (declare (ignore desc))
        (with-isolated-options ("status-position" pos)
          (let ((cl-tmux/config:*status-height* 1))
            (let* ((p0  (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                                   :fd -1 :pid -1 :screen (make-screen 20 5)))
                   (win (make-window :id 1 :name "w" :width 20 :height 6
                                     :tree (make-layout-leaf p0) :panes (list p0))))
              (cl-tmux/model:window-relayout win 5 20)
              (expect (= expected-y (pane-y p0)))
              (expect (= 5 (pane-height p0)))))))))

  ;;; ── status on/off ────────────────────────────────────────────────────────────

  ;; %status-segment-style-sgr returns the base SGR when the segment style is unset
  ;; or "default".
  (it "status-segment-style-falls-back-to-base-when-unset"
    (with-isolated-options ("status-left-style" "")
      (expect (string= "44;97" (cl-tmux/renderer::%status-segment-style-sgr
                                 "status-left-style" "44;97"))))
    (with-isolated-options ("status-left-style" "default")
      (expect (string= "44;97" (cl-tmux/renderer::%status-segment-style-sgr
                                 "status-left-style" "44;97")))))

  ;; %apply-segment-style wraps TEXT in the segment SGR and reverts to the base.
  (it "apply-segment-style-wraps-and-reverts"
    (let ((out (cl-tmux/renderer::%apply-segment-style "TEXT" "31" "44")))
      (expect (search (format nil "~C[31m" #\Escape) out))
      (expect (search "TEXT" out))
      (expect (search (format nil "~C[44m" #\Escape) out))))

  ;; %apply-segment-style returns TEXT unchanged when the segment SGR equals the base.
  (it "apply-segment-style-noop-when-equal-to-base"
    (expect (string= "TEXT" (cl-tmux/renderer::%apply-segment-style "TEXT" "44" "44"))))

  ;; status-left-style injects its SGR into the rendered status bar.
  (it "status-left-style-applied-in-rendered-bar"
    (with-isolated-options ("status-left" "L" "status-right" nil "status-style" ""
                            "status-left-style" "fg=red")
      (let* ((expected (cl-tmux/renderer::%status-sgr-from-style "fg=red"))
             (sess     (make-renderer-test-session 20 5))
             (out      (render-status-bar-output sess 6 20)))
        (expect out :to-contain-sgr expected))))

  ;; The status option controls whether render-session-to-string emits a blue status bar.
  (it "status-on-off-table"
    (dolist (row '((nil "status=nil: blue background absent")
                   (t   "status=t:   blue background present")))
      (destructuring-bind (status-val desc) row
        (declare (ignore desc))
        (with-empty-status-bar-options ("status" status-val
                                        "status-style" "")
          (let* ((sess (make-renderer-test-session 20 5))
                 (out  (render-session-to-string sess 6 20))
                 (hit  (search (format nil "~C[44;97m" #\Escape) out)))
            (expect (if status-val hit (null hit))))))))

  ;;; ── multi-line status (status 2..5 + status-format[N]) ─────────────────────

  ;; status-line-count maps the `status` option to a row count (0..5, tmux cap).
  (it "status-line-count-parses-option"
    (flet ((n (v) (with-isolated-options ("status" v)
                    (cl-tmux/renderer::status-line-count))))
      (expect (= 0 (n nil)))
      (expect (= 0 (n "off")))
      (expect (= 0 (n "0")))
      (expect (= 1 (n t)))
      (expect (= 1 (n "on")))
      (expect (= 1 (n "bogus")))
      (expect (= 2 (n "2")))
      (expect (= 0 (n "-3")))
      (expect (= 5 (n "5")))
      (expect (= 5 (n "9")))
      (expect (= 2 (n 2)))))

  ;; With status=2 and status-format[1] set, render-session-to-string renders the
  ;; extra status line's content; with status=1 it does not.
  (it "render-session-multiline-status-shows-extra-line"
    (let ((sess (make-renderer-test-session 20 5)))
      (with-empty-status-bar-options ("status" "2"
                                      "status-format[1]" "EXTRALINE"
                                      "status-style" "")
        (let ((out (render-session-to-string sess 8 20)))
          (expect (search "EXTRALINE" out))))
      (with-empty-status-bar-options ("status" "1"
                                      "status-format[1]" "EXTRALINE"
                                      "status-style" "")
        (let ((out (render-session-to-string sess 8 20)))
          (expect (null (search "EXTRALINE" out)))))))

  ;; An extra status line with no status-format[N] set renders a blank styled row
  ;; (no crash, returns cleanly).
  (it "render-extra-status-line-blank-when-unset"
    (let ((sess (make-renderer-test-session 20 5)))
      (with-isolated-options ("status" "3" "status-style" "")
        (let ((out (render-session-to-string sess 8 20)))
          ;; status=3 reserves three rows; rendering must succeed and produce output.
          (expect (stringp out))
          (expect (plusp (length out)))))))

  ;; An extra status row (status-format[N]) honours #[align=right] via the shared
  ;; align composer — consistent with status-format[0].
  (it "render-extra-status-line-honours-align"
    (with-isolated-options ("status-format[1]" "L1#[align=right]R1")
      (let* ((sess (make-renderer-test-session 30 10 :content ""))
             (out  (with-output-to-string (s)
                     (cl-tmux/renderer::render-extra-status-line s sess 30 8 1)))
             (vis  (cl-ppcre:regex-replace-all
                    (format nil "~C\\[[0-9;?]*[A-Za-z]" #\Escape) out ""))
             (rpos (search "R1" vis)))
        (expect (eql 0 (search "L1" vis)))
        (expect (and rpos (= (+ rpos 2) 30))))))

  ;;; ── BEL rendering ────────────────────────────────────────────────────────────

  ;; render-session-to-string emits BEL (byte 7) when bell-pending is T and clears
  ;; the flag; emits no BEL when bell-pending is NIL.
  (it "render-bel-table"
    (dolist (row '((t   "bell-pending T: BEL emitted and flag cleared")
                   (nil "bell-pending NIL: BEL absent")))
      (destructuring-bind (initial-pending desc) row
        (declare (ignore desc))
        (let* ((sess  (make-renderer-test-session 20 5))
               (ap    (session-active-pane sess))
               (sc    (pane-screen ap)))
          (setf (cl-tmux/terminal/types:screen-bell-pending sc) initial-pending)
          (let ((out (render-session-to-string sess 6 20)))
            (expect (if initial-pending
                        (find (code-char 7) out)
                        (null (find (code-char 7) out))))
            (when initial-pending
              (expect (cl-tmux/terminal/types:screen-bell-pending sc) :to-be-falsy)))))))

  ;;; ── status-left expanded ─────────────────────────────────────────────────────

  ;; status-left #{session_name} expands to the actual session name.
  (it "status-left-expanded-session-name"
    (with-isolated-options ()
      (cl-tmux/options:set-option "status-left" "#{session_name}")
      (let* ((sess (make-renderer-test-session 40 10))
             (out  (render-status-bar-output sess 11 40)))
        (expect (search "0" out))
        (expect (null (search "#{session_name}" out)))))))
