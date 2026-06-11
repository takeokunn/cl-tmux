(in-package #:cl-tmux/test)

;;;; Tests for cl-tmux/control: the control mode (-C) wire-protocol formatters.

(def-suite control-suite :description "Control mode (-C) protocol formatters")
(in-suite control-suite)

;;; ── %begin / %end / %error framing ──────────────────────────────────────────

(test control-begin-end-error-lines
  "%begin / %end / %error format as `%VERB <time> <number> <flags>`."
  (is (string= "%begin 5 7 1"
               (cl-tmux/control:control-begin 7 :time 5)))
  (is (string= "%end 5 7 1"
               (cl-tmux/control:control-end 7 :time 5)))
  (is (string= "%error 0 3 1"
               (cl-tmux/control:control-error 3))))

(test control-format-reply-success
  "A successful reply is %begin, the output lines, then %end."
  (let ((reply (cl-tmux/control:control-format-reply 9 (format nil "line1~%line2")
                                                     :time 100)))
    (is (string= (format nil "%begin 100 9 1~%line1~%line2~%%end 100 9 1") reply))))

(test control-format-reply-error
  "A failed reply closes with %error instead of %end."
  (let ((reply (cl-tmux/control:control-format-reply 4 "boom"
                                                     :success nil :time 2)))
    (is (string= (format nil "%begin 2 4 1~%boom~%%error 2 4 1") reply))))

(test control-format-reply-empty-output
  "Empty output yields just the %begin/%end pair (no blank middle line)."
  (let ((reply (cl-tmux/control:control-format-reply 1 "" :time 0)))
    (is (string= (format nil "%begin 0 1 1~%%end 0 1 1") reply))))

;;; ── %output escaping ────────────────────────────────────────────────────────

(test control-escape-output-octal
  "Non-printable bytes are escaped as 3-digit octal; printable ASCII passes through."
  (is (string= "abc" (cl-tmux/control:control-escape-output "abc")))
  (is (string= "a\\033b"
               (cl-tmux/control:control-escape-output
                (format nil "a~Cb" (code-char 27))))    ; ESC = 27 = octal 033
      "ESC escapes to \\033")
  (is (string= "x\\012"
               (cl-tmux/control:control-escape-output (format nil "x~C" #\Newline)))
      "newline (10 = octal 012) is escaped"))

(test control-output-notification
  "%output prefixes the pane id with % and escapes the data."
  (is (string= "%output %3 hello"
               (cl-tmux/control:control-output 3 "hello")))
  (is (string= "%output %12 a\\011b"
               (cl-tmux/control:control-output 12 (format nil "a~Cb" #\Tab)))
      "tab (9 = octal 011) escaped in %output"))

;;; ── State-change notifications (sigils $ @ %) ───────────────────────────────

(test control-session-and-window-notifications
  "Session/window notifications use the $ and @ id sigils."
  (is (string= "%session-changed $1 main"
               (cl-tmux/control:control-session-changed 1 "main")))
  (is (string= "%session-renamed $2 work"
               (cl-tmux/control:control-session-renamed 2 "work")))
  (is (string= "%window-add @4"   (cl-tmux/control:control-window-add 4)))
  (is (string= "%window-close @4" (cl-tmux/control:control-window-close 4)))
  (is (string= "%window-renamed @4 editor"
               (cl-tmux/control:control-window-renamed 4 "editor")))
  (is (string= "%unlinked-window-add @9"
               (cl-tmux/control:control-unlinked-window-add 9))))

(test control-active-change-notifications
  "%window-pane-changed and %session-window-changed use the @ / % / $ id sigils
   (tmux control_notify_window_pane_changed / _session_window_changed)."
  (is (string= "%window-pane-changed @4 %2"
               (cl-tmux/control:control-window-pane-changed 4 2)))
  (is (string= "%session-window-changed $1 @3"
               (cl-tmux/control:control-session-window-changed 1 3))))

(test control-layout-and-client-and-exit
  "layout-change, client-session-changed, and exit notifications."
  (is (string= "%layout-change @1 abcd,80x24,0,0 abcd,80x24,0,0 *"
               (cl-tmux/control:control-layout-change 1 "abcd,80x24,0,0"
                                                      "abcd,80x24,0,0" "*")))
  (is (string= "%client-session-changed client-1 $0 main"
               (cl-tmux/control:control-client-session-changed "client-1" 0 "main")))
  (is (string= "%exit" (cl-tmux/control:control-exit)))
  (is (string= "%exit server exited"
               (cl-tmux/control:control-exit "server exited"))))
