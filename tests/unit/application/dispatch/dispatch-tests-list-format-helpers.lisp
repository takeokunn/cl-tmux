(in-package #:cl-tmux/test)

;;;; Window/session list formatting and small dispatch helper tests.

(in-suite dispatch-suite)

;;; ── %format-window-list helper ───────────────────────────────────────────────

(test format-window-list-includes-active-marker
  "%format-window-list includes an asterisk on the active window line and
   lists each window by id and name."
  (with-fake-session (s :nwindows 2)
    (let* ((text (cl-tmux::%format-window-list s))
           (aw   (session-active-window s)))
      (is (stringp text) "%format-window-list must return a string")
      (is (search (window-name aw) text)
          "output must mention the active window name")
      (is (search "*" text)
          "output must mark the active window with an asterisk"))))

(test format-window-list-shows-pane-count
  "%format-window-list includes the pane count for each window."
  (with-fake-two-pane-session (s)
    (let ((text (cl-tmux::%format-window-list s)))
      (is (search "pane" text)
          "output must include the word 'pane'"))))

;;; ── %format-session-list helper ──────────────────────────────────────────────

(test format-session-list-fallback-uses-session-name
  "%format-session-list with empty *server-sessions* falls back to the
   session-name one-line entry."
  (with-fake-session (s :nwindows 1)
    (let ((cl-tmux::*server-sessions* nil))
      (let ((text (cl-tmux::%format-session-list s)))
        (is (stringp text) "%format-session-list must return a string")
        (is (search (session-name s) text)
            "fallback output must contain the session name")))))

(test format-session-list-marks-current-session
  "%format-session-list with a populated *server-sessions* marks the current
   session with an asterisk."
  (with-fake-session (s :nwindows 1)
    (let* ((name (session-name s))
           (cl-tmux::*server-sessions* (list (cons name s))))
      (let ((text (cl-tmux::%format-session-list s)))
        (is (search "*" text) "current session must be marked with an asterisk")
        (is (search name text) "output must contain the session name")))))

;;; ── %copy-mode-call helper ────────────────────────────────────────────────────

(test copy-mode-call-invokes-fn-on-active-screen
  "%copy-mode-call invokes FN on the active screen when copy mode is on."
  (with-fake-session (s)
    (cl-tmux::dispatch-command s :copy-mode-enter nil)
    (let ((called-with nil))
      (cl-tmux::%copy-mode-call s (lambda (sc) (setf called-with sc)))
      (is (eq (active-screen s) called-with)
          "%copy-mode-call must pass the active screen to FN"))))

(test copy-mode-call-skips-when-no-session-has-no-screen
  "%copy-mode-call on a windowless session is a no-op (no error)."
  (with-fake-session (s :nwindows 0)
    (finishes (cl-tmux::%copy-mode-call s (lambda (screen) (declare (ignore screen)) nil))
              "%copy-mode-call must not error when there is no active screen")))

;;; ── %handle-kill-result helper ────────────────────────────────────────────────

(test handle-kill-result-table
  "%handle-kill-result: :quit clears *running* and returns :quit; NIL preserves it and returns NIL."
  (dolist (row '((:quit nil  ":quit must clear *running*")
                 (nil   t    "NIL must preserve *running*")))
    (destructuring-bind (result expected-running desc) row
      (with-loop-state
        (let ((ret (cl-tmux::%handle-kill-result result)))
          (is (equal result ret)            "~A: return value must equal input" desc)
          (is (eq expected-running cl-tmux::*running*) "~A: *running*" desc))))))
