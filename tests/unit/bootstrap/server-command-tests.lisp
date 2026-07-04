(in-package #:cl-tmux/test)

;;;; Server command helpers.

(in-suite server-suite)

(test new-session-command
  :description "new-session adds a session to the server registry."
  (unless (pty-available-p) (skip "no PTY available (sandboxed environment)"))
  (with-empty-registry
    (let ((cl-tmux/model::*session-id-counter* 0))
      (let ((sess (cl-tmux::new-session "testsess" 24 80)))
        (is-true sess "new-session must return a session object")
        (is (= 1 (length cl-tmux::*server-sessions*))
            "after new-session, registry should contain 1 entry")
        (let ((found (cl-tmux::server-find-session "testsess")))
          (is (eq sess found)
              "server-find-session should find the newly created session"))
        (dolist (p (all-panes sess))
          (ignore-errors (pty-close (pane-fd p) (pane-pid p))))))))

(test kill-session-command
  :description "After killing a session it is removed from the server registry."
  (with-empty-registry
    (let ((s1 (make-session :id 1 :name "alive"  :windows nil))
          (s2 (make-session :id 2 :name "doomed" :windows nil)))
      (cl-tmux::server-add-session s1)
      (cl-tmux::server-add-session s2)
      (cl-tmux::server-remove-session "doomed")
      (is (= 1 (length (cl-tmux::server-all-sessions)))
          "registry should have 1 session after kill")
      (is (null (cl-tmux::server-find-session "doomed"))
          "killed session should not be findable"))))
