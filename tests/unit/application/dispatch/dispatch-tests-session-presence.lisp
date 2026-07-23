(in-package #:cl-tmux/test)

;;;; Session presence command dispatch tests.

(describe "dispatch-suite"

  ;;; ── :has-session dispatch ────────────────────────────────────────────────────

  ;; :has-session opens a prompt for the session name.
  (it "dispatch-has-session-opens-prompt"
    (with-fake-session (s)
      (let ((*prompt* nil))
        (cl-tmux::dispatch-command s :has-session nil)
        (expect (prompt-active-p)))))

  ;; :has-session on-submit shows 'yes' when the session is registered.
  (it "dispatch-has-session-found-shows-yes"
    (with-fake-session (s)
      (let ((name (session-name s)))
        (let ((*prompt* nil) (*overlay* nil)
              (cl-tmux::*server-sessions* (list (cons name s))))
          (cl-tmux::dispatch-command s :has-session nil)
          (expect (prompt-active-p))
          (funcall (prompt-on-submit *prompt*) name)
          (assert-overlay-active "on-submit must open an overlay")
          (assert-overlay-contains "yes" (overlay-lines)
                                   "known session")))))

  ;; has-session rejects unknown flags and positionals instead of silently checking all sessions.
  (it "cmd-has-session-rejects-unsupported-arguments"
    (dolist (command '("has-session extra"
                       "has-session -x"
                       "has-session -t 0 extra"))
      (with-fake-session (s)
        (let ((*overlay* nil)
              (cl-tmux::*server-sessions* (list (cons (session-name s) s))))
          (expect (null (cl-tmux::%run-command-line s command)))
          (assert-overlay-active "~A must show an unsupported-argument overlay" command)
          (assert-overlay-contains "unsupported argument" (overlay-lines)
                                   command)
          (assert-overlay-not-contains "yes" (overlay-lines)
                                       command))))))
