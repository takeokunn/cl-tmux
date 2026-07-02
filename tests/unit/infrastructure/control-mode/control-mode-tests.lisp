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
  (dolist (c `(("abc"                         "abc"     "plain ASCII passes through")
               (,(format nil "a~Cb" #\Esc)    "a\\033b" "ESC escapes to \\033")
               (,(format nil "x~C" #\Newline) "x\\012"  "newline (10 = octal 012) is escaped")))
    (destructuring-bind (input expected desc) c
      (is (string= expected (cl-tmux/control:control-escape-output input)) "~A" desc))))

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
  (dolist (c `(("%session-changed $1 main"
                ,(cl-tmux/control:control-session-changed 1 "main")
                "session-changed uses $ sigil")
               ("%session-renamed $2 work"
                ,(cl-tmux/control:control-session-renamed 2 "work")
                "session-renamed uses $ sigil")
               ("%window-add @4"
                ,(cl-tmux/control:control-window-add 4)
                "window-add uses @ sigil")
               ("%window-close @4"
                ,(cl-tmux/control:control-window-close 4)
                "window-close uses @ sigil")
               ("%window-renamed @4 editor"
                ,(cl-tmux/control:control-window-renamed 4 "editor")
                "window-renamed uses @ sigil")
               ("%unlinked-window-add @9"
                ,(cl-tmux/control:control-unlinked-window-add 9)
                "unlinked-window-add uses @ sigil")))
    (destructuring-bind (expected actual desc) c
      (is (string= expected actual) "~A" desc))))

(test control-active-change-notifications
  "%window-pane-changed and %session-window-changed use the @ / % / $ id sigils
   (tmux control_notify_window_pane_changed / _session_window_changed)."
  (is (string= "%window-pane-changed @4 %2"
               (cl-tmux/control:control-window-pane-changed 4 2)))
  (is (string= "%session-window-changed $1 @3"
               (cl-tmux/control:control-session-window-changed 1 3))))

(test control-layout-and-client-and-exit
  "layout-change, client-session-changed, and exit notifications."
  (dolist (c `(("%layout-change @1 abcd,80x24,0,0 abcd,80x24,0,0 *"
                ,(cl-tmux/control:control-layout-change 1 "abcd,80x24,0,0"
                                                          "abcd,80x24,0,0" "*")
                "layout-change lists window id then old and new layouts")
               ("%client-session-changed client-1 $0 main"
                ,(cl-tmux/control:control-client-session-changed "client-1" 0 "main")
                "client-session-changed uses $ sigil for the session id")
               ("%exit"
                ,(cl-tmux/control:control-exit)
                "bare exit has no reason")
               ("%exit server exited"
                ,(cl-tmux/control:control-exit "server exited")
                "exit with reason appends it after the sigil")))
    (destructuring-bind (expected actual desc) c
      (is (string= expected actual) "~A" desc))))
