(in-package #:cl-tmux/test)

;;;; Window/session list formatting and small dispatch helper tests.

(describe "dispatch-suite"

  ;;; ── %format-window-list helper ───────────────────────────────────────────────

  ;; %format-window-list includes an asterisk on the active window line and
  ;; lists each window by id and name.
  (it "format-window-list-includes-active-marker"
    (with-fake-session (s :nwindows 2)
      (let* ((text (cl-tmux::%format-window-list s))
             (aw   (session-active-window s)))
        (expect (stringp text))
        (expect (search (window-name aw) text))
        (expect (search "*" text)))))

  ;; %format-window-list includes the pane count for each window.
  (it "format-window-list-shows-pane-count"
    (with-fake-two-pane-session (s)
      (let ((text (cl-tmux::%format-window-list s)))
        (expect (search "pane" text)))))

  ;;; ── %format-session-list helper ──────────────────────────────────────────────

  ;; %format-session-list with empty *server-sessions* falls back to the
  ;; session-name one-line entry.
  (it "format-session-list-fallback-uses-session-name"
    (with-fake-session (s :nwindows 1)
      (let ((cl-tmux::*server-sessions* nil))
        (let ((text (cl-tmux::%format-session-list s)))
          (expect (stringp text))
          (expect (search (session-name s) text))))))

  ;; %format-session-list with a populated *server-sessions* marks the current
  ;; session with an asterisk.
  (it "format-session-list-marks-current-session"
    (with-fake-session (s :nwindows 1)
      (let* ((name (session-name s))
             (cl-tmux::*server-sessions* (list (cons name s))))
        (let ((text (cl-tmux::%format-session-list s)))
          (expect (search "*" text))
          (expect (search name text))))))

  ;;; ── %copy-mode-call helper ────────────────────────────────────────────────────

  ;; %copy-mode-call invokes FN on the active screen when copy mode is on.
  (it "copy-mode-call-invokes-fn-on-active-screen"
    (with-fake-session (s)
      (cl-tmux::dispatch-command s :copy-mode-enter nil)
      (let ((called-with nil))
        (cl-tmux::%copy-mode-call s (lambda (sc) (setf called-with sc)))
        (expect (eq (active-screen s) called-with)))))

  ;; %copy-mode-call on a windowless session is a no-op (no error).
  (it "copy-mode-call-skips-when-no-session-has-no-screen"
    (with-fake-session (s :nwindows 0)
      (finishes (cl-tmux::%copy-mode-call s (lambda (screen) (declare (ignore screen)) nil))
                "%copy-mode-call must not error when there is no active screen")))

  ;;; ── %handle-kill-result helper ────────────────────────────────────────────────

  ;; %handle-kill-result: :quit clears *running* and returns :quit; NIL preserves it and returns NIL.
  (it "handle-kill-result-table"
    (dolist (row '((:quit nil  ":quit must clear *running*")
                   (nil   t    "NIL must preserve *running*")))
      (destructuring-bind (result expected-running desc) row
        (declare (ignore desc))
        (with-loop-state
          (let ((ret (cl-tmux::%handle-kill-result result)))
            (expect (equal result ret))
            (expect (eq expected-running cl-tmux::*running*))))))))
