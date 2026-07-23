(in-package #:cl-tmux/test)

;;;; Server command helpers.

(describe "server-suite"

  ;; new-session adds a session to the server registry.
  (it "new-session-command"
    (unless (pty-available-p) (skip "no PTY available (sandboxed environment)"))
    (with-empty-registry
      (let ((cl-tmux/model::*session-id-counter* 0))
        (let ((sess (cl-tmux::new-session "testsess" 24 80)))
          (expect sess :to-be-truthy)
          (expect (= 1 (length cl-tmux::*server-sessions*)))
          (let ((found (cl-tmux::server-find-session "testsess")))
            (expect (eq sess found)))
          (dolist (p (all-panes sess))
            (ignore-errors (pty-close (pane-fd p) (pane-pid p))))))))

  ;; After killing a session it is removed from the server registry.
  (it "kill-session-command"
    (with-empty-registry
      (let ((s1 (make-session :id 1 :name "alive"  :windows nil))
            (s2 (make-session :id 2 :name "doomed" :windows nil)))
        (cl-tmux::server-add-session s1)
        (cl-tmux::server-add-session s2)
        (cl-tmux::server-remove-session "doomed")
        (expect (= 1 (length (cl-tmux::server-all-sessions))))
        (expect (null (cl-tmux::server-find-session "doomed")))))))
