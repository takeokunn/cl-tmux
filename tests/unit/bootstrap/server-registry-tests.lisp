(in-package #:cl-tmux/test)

;;;; Session registry and lookup behavior.

(in-suite server-suite)

(test server-add-and-find-session
  :description "server-add-session registers a session; server-find-session retrieves it."
  (with-empty-registry
    (let ((sess (make-session :id 1 :name "alpha" :windows nil)))
      (cl-tmux::server-add-session sess)
      (let ((found (cl-tmux::server-find-session "alpha")))
        (is (eq sess found)
            "server-find-session should return the exact session object added")))))

(test server-remove-session
  :description "server-remove-session removes a previously added session from the registry."
  (with-empty-registry
    (let ((sess (make-session :id 1 :name "beta" :windows nil)))
      (cl-tmux::server-add-session sess)
      (cl-tmux::server-remove-session "beta")
      (is (null (cl-tmux::server-find-session "beta"))
          "after removal, server-find-session should return NIL"))))

(test server-all-sessions
  :description "server-all-sessions returns one entry per registered session."
  (with-empty-registry
    (let ((s1 (make-session :id 1 :name "one" :windows nil))
          (s2 (make-session :id 2 :name "two" :windows nil)))
      (cl-tmux::server-add-session s1)
      (cl-tmux::server-add-session s2)
      (let ((all (cl-tmux::server-all-sessions)))
        (is (= 2 (length all))
            "server-all-sessions should return 2 entries, got ~D" (length all))
        (is-true (member s1 all)
                 "session s1 should appear in server-all-sessions")
        (is-true (member s2 all)
                 "session s2 should appear in server-all-sessions")))))

(test server-all-sessions-empty-registry
  :description "server-all-sessions returns NIL (empty list) when no sessions are registered."
  (with-empty-registry
    (is (null (cl-tmux::server-all-sessions))
        "server-all-sessions on empty registry must return NIL")))

(test server-find-session-nil-inputs-table
  "server-find-session returns NIL for an unknown name, NIL, or an empty string."
  (dolist (row '(("no-such-session" "unknown name -> nil")
                 (nil               "nil input -> nil")
                 (""                "empty string -> nil")))
    (destructuring-bind (input desc) row
      (with-empty-registry
        (is (null (cl-tmux::server-find-session input)) "~A" desc)))))

(test multi-session-add-remove
  :description "Add 3 sessions, remove the middle one; exactly 2 sessions remain."
  (with-empty-registry
    (let ((s1 (make-session :id 1 :name "alpha" :windows nil))
          (s2 (make-session :id 2 :name "beta"  :windows nil))
          (s3 (make-session :id 3 :name "gamma" :windows nil)))
      (cl-tmux::server-add-session s1)
      (cl-tmux::server-add-session s2)
      (cl-tmux::server-add-session s3)
      (cl-tmux::server-remove-session "beta")
      (let ((all (cl-tmux::server-all-sessions)))
        (is (= 2 (length all))
            "after removing middle session, registry should hold 2, got ~D" (length all))
        (is-true (member s1 all) "s1 (alpha) must still be present")
        (is-true (member s3 all) "s3 (gamma) must still be present")
        (is-false (member s2 all) "s2 (beta) must have been removed")))))

(test server-add-session-replaces-existing-name
  :description "Adding a session whose name already exists replaces the old one."
  (with-empty-registry
    (let ((s1 (make-session :id 1 :name "same" :windows nil))
          (s2 (make-session :id 2 :name "same" :windows nil)))
      (cl-tmux::server-add-session s1)
      (cl-tmux::server-add-session s2)
      (is (= 1 (length (cl-tmux::server-all-sessions)))
          "duplicate-name add must replace, leaving only 1 entry")
      (is (eq s2 (cl-tmux::server-find-session "same"))
          "the second session (s2) must have replaced s1"))))

(test server-find-session-fuzzy
  :description "server-find-session with a name prefix 'my' finds the session named 'mysession'."
  (with-empty-registry
    (let ((sess (make-session :id 1 :name "mysession" :windows nil)))
      (cl-tmux::server-add-session sess)
      (let ((found (cl-tmux::server-find-session "my")))
        (is (eq sess found)
            "prefix 'my' should match session named 'mysession'")))))

(test server-find-session-by-id-table
  "server-find-session '$N' matches by id when present; returns NIL when absent.
   Each row: (session-id query expect-found description)."
  (dolist (row '((42 "$42"  t   "$42 should find the session with id 42")
                 (1  "$999" nil "$999 must return NIL when no session has id 999")))
    (destructuring-bind (id query expect-found desc) row
      (with-empty-registry
        (let ((sess (make-session :id id :name "s" :windows nil)))
          (cl-tmux::server-add-session sess)
          (let ((found (cl-tmux::server-find-session query)))
            (if expect-found
                (is (eq sess found) desc)
                (is (null found)    desc))))))))

(test server-current-session-by-last-active
  :description "server-current-session returns the session with the highest last-active time."
  (with-empty-registry
    (let ((s1 (make-session :id 1 :name "older"  :windows nil :last-active 100))
          (s2 (make-session :id 2 :name "newest" :windows nil :last-active 999))
          (s3 (make-session :id 3 :name "middle" :windows nil :last-active 500)))
      (cl-tmux::server-add-session s1)
      (cl-tmux::server-add-session s2)
      (cl-tmux::server-add-session s3)
      (let ((current (cl-tmux::server-current-session)))
        (is (eq s2 current)
            "server-current-session should return the session with highest last-active (s2)")))))

(test server-current-session-empty-registry-returns-nil
  :description "server-current-session returns NIL when no sessions are registered."
  (with-empty-registry
    (is (null (cl-tmux::server-current-session))
        "server-current-session on empty registry must return NIL")))
