(in-package #:cl-tmux/test)

;;;; Session presence command dispatch tests.

(in-suite dispatch-suite)

;;; ── :has-session dispatch ────────────────────────────────────────────────────

(test dispatch-has-session-opens-prompt
  ":has-session opens a prompt for the session name."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :has-session nil)
      (is (prompt-active-p) ":has-session must open a prompt"))))

(test dispatch-has-session-found-shows-yes
  ":has-session on-submit shows 'yes' when the session is registered."
  (with-fake-session (s)
    (let ((name (session-name s)))
      (let ((*prompt* nil) (*overlay* nil)
            (cl-tmux::*server-sessions* (list (cons name s))))
        (cl-tmux::dispatch-command s :has-session nil)
        (is (prompt-active-p) "prompt must be open")
        (funcall (prompt-on-submit *prompt*) name)
        (assert-overlay-active "on-submit must open an overlay")
        (assert-overlay-contains "yes" (overlay-lines)
                                 "known session")))))

(test cmd-has-session-rejects-unsupported-arguments
  "has-session rejects unknown flags and positionals instead of silently checking all sessions."
  (dolist (command '("has-session extra"
                     "has-session -x"
                     "has-session -t 0 extra"))
    (with-fake-session (s)
      (let ((*overlay* nil)
            (cl-tmux::*server-sessions* (list (cons (session-name s) s))))
        (is (null (cl-tmux::%run-command-line s command))
            "~A must be rejected" command)
        (assert-overlay-active "~A must show an unsupported-argument overlay" command)
        (assert-overlay-contains "unsupported argument" (overlay-lines)
                                 command)
        (assert-overlay-not-contains "yes" (overlay-lines)
                                     command)))))
