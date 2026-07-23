(in-package #:cl-tmux/test)

;;;; Session registry and lookup behavior.

(describe "server-suite"

  ;; server-add-session registers a session; server-find-session retrieves it.
  (it "server-add-and-find-session"
    (with-empty-registry
      (let ((sess (make-session :id 1 :name "alpha" :windows nil)))
        (cl-tmux::server-add-session sess)
        (let ((found (cl-tmux::server-find-session "alpha")))
          (expect (eq sess found))))))

  ;; server-remove-session removes a previously added session from the registry.
  (it "server-remove-session"
    (with-empty-registry
      (let ((sess (make-session :id 1 :name "beta" :windows nil)))
        (cl-tmux::server-add-session sess)
        (cl-tmux::server-remove-session "beta")
        (expect (null (cl-tmux::server-find-session "beta"))))))

  ;; server-all-sessions returns one entry per registered session.
  (it "server-all-sessions"
    (with-empty-registry
      (let ((s1 (make-session :id 1 :name "one" :windows nil))
            (s2 (make-session :id 2 :name "two" :windows nil)))
        (cl-tmux::server-add-session s1)
        (cl-tmux::server-add-session s2)
        (let ((all (cl-tmux::server-all-sessions)))
          (expect (= 2 (length all)))
          (expect (member s1 all) :to-be-truthy)
          (expect (member s2 all) :to-be-truthy)))))

  ;; server-all-sessions returns NIL (empty list) when no sessions are registered.
  (it "server-all-sessions-empty-registry"
    (with-empty-registry
      (expect (null (cl-tmux::server-all-sessions)))))

  ;; server-find-session returns NIL for an unknown name, NIL, or an empty string.
  (it "server-find-session-nil-inputs-table"
    (dolist (row '(("no-such-session" "unknown name -> nil")
                   (nil               "nil input -> nil")
                   (""                "empty string -> nil")))
      (destructuring-bind (input desc) row
        (declare (ignore desc))
        (with-empty-registry
          (expect (null (cl-tmux::server-find-session input)))))))

  ;; Add 3 sessions, remove the middle one; exactly 2 sessions remain.
  (it "multi-session-add-remove"
    (with-empty-registry
      (let ((s1 (make-session :id 1 :name "alpha" :windows nil))
            (s2 (make-session :id 2 :name "beta"  :windows nil))
            (s3 (make-session :id 3 :name "gamma" :windows nil)))
        (cl-tmux::server-add-session s1)
        (cl-tmux::server-add-session s2)
        (cl-tmux::server-add-session s3)
        (cl-tmux::server-remove-session "beta")
        (let ((all (cl-tmux::server-all-sessions)))
          (expect (= 2 (length all)))
          (expect (member s1 all) :to-be-truthy)
          (expect (member s3 all) :to-be-truthy)
          (expect (member s2 all) :to-be-falsy)))))

  ;; Adding a session whose name already exists replaces the old one.
  (it "server-add-session-replaces-existing-name"
    (with-empty-registry
      (let ((s1 (make-session :id 1 :name "same" :windows nil))
            (s2 (make-session :id 2 :name "same" :windows nil)))
        (cl-tmux::server-add-session s1)
        (cl-tmux::server-add-session s2)
        (expect (= 1 (length (cl-tmux::server-all-sessions))))
        (expect (eq s2 (cl-tmux::server-find-session "same"))))))

  ;; server-find-session with a name prefix 'my' finds the session named 'mysession'.
  (it "server-find-session-fuzzy"
    (with-empty-registry
      (let ((sess (make-session :id 1 :name "mysession" :windows nil)))
        (cl-tmux::server-add-session sess)
        (let ((found (cl-tmux::server-find-session "my")))
          (expect (eq sess found))))))

  ;; server-find-session '$N' matches by id when present; returns NIL when absent.
  ;; Each row: (session-id query expect-found description).
  (it "server-find-session-by-id-table"
    (dolist (row '((42 "$42"  t   "$42 should find the session with id 42")
                   (1  "$999" nil "$999 must return NIL when no session has id 999")))
      (destructuring-bind (id query expect-found desc) row
        (declare (ignore desc))
        (with-empty-registry
          (let ((sess (make-session :id id :name "s" :windows nil)))
            (cl-tmux::server-add-session sess)
            (let ((found (cl-tmux::server-find-session query)))
              (if expect-found
                  (expect (eq sess found))
                  (expect (null found)))))))))

  ;; server-current-session returns the session with the highest last-active time.
  (it "server-current-session-by-last-active"
    (with-empty-registry
      (let ((s1 (make-session :id 1 :name "older"  :windows nil :last-active 100))
            (s2 (make-session :id 2 :name "newest" :windows nil :last-active 999))
            (s3 (make-session :id 3 :name "middle" :windows nil :last-active 500)))
        (cl-tmux::server-add-session s1)
        (cl-tmux::server-add-session s2)
        (cl-tmux::server-add-session s3)
        (let ((current (cl-tmux::server-current-session)))
          (expect (eq s2 current))))))

  ;; server-current-session returns NIL when no sessions are registered.
  (it "server-current-session-empty-registry-returns-nil"
    (with-empty-registry
      (expect (null (cl-tmux::server-current-session))))))
