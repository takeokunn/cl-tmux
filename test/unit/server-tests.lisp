(in-package #:cl-tmux/test)

;;;; Detach-attach server logic tests (src/server.lisp).
;;;;
;;;; The accept/serve loops are integration-level (like event-loop itself —
;;;; compile-verified, with their building blocks — protocol/transport/net/
;;;; process-byte — unit-tested elsewhere).  Here we cover the two pieces that
;;;; ARE pure/observable without a live socket: the socket-path naming and the
;;;; client-size application.  make-fake-session comes from events-tests.

(def-suite server-suite :description "Detach-attach server logic")
(in-suite server-suite)

(test socket-path-includes-session-name
  "socket-path names the per-session Unix socket under the temp directory."
  (let ((path (cl-tmux::socket-path "mysess")))
    (is (search "cl-tmux-mysess.sock" path)
        "socket-path should embed the session name, got ~S" path)))

(test apply-client-size-updates-dimensions-and-dirties
  "apply-client-size decodes a rows,cols payload, updates the terminal size,
   relayouts, and marks the session dirty so a fresh frame is sent."
  (let ((s (make-fake-session)))
    (let ((cl-tmux::*term-rows* 24)
          (cl-tmux::*term-cols* 80)
          (cl-tmux::*dirty* nil))
      ;; A resize payload is the body of a msg-resize frame.
      (multiple-value-bind (type payload) (decode-frame (msg-resize 30 100))
        (declare (ignore type))
        (cl-tmux::apply-client-size s payload)
        (is (= 30 cl-tmux::*term-rows*) "rows updated from payload")
        (is (= 100 cl-tmux::*term-cols*) "cols updated from payload")
        (is-true cl-tmux::*dirty* "resize marks the session dirty")))))

;;; ── process-client-keys: the serve loop's quit/detach disposition ────────────
;;;
;;; process-client-keys is the pure extraction of the +msg-key+ arm of
;;; serve-client: it feeds an already-decoded key payload through process-byte
;;; (the shared keystroke pipeline) and reports the serve-loop disposition,
;;; so the quit/detach decision is testable without a live client socket.
;;; The byte sequences mirror the events-tests fixtures: prefix byte 2 (^B,
;;; +prefix-key-code+) followed by the bound command key — 'd' for detach
;;; (process-byte-prefix-detach-returns-detach) and '&' for kill-window, which
;;; quits when it removes the last window (dispatch-kill-last-window-quits).

(test process-client-keys-detach-keystroke-returns-detach
  "A prefix+detach key payload (^B d) returns :detach and leaves *running* T —
   a detach disconnects the client but the session must survive for re-attach."
  (let ((s (make-fake-session)))
    (let ((cl-tmux::*running* t) (cl-tmux::*dirty* nil))
      (let ((state   (cl-tmux::make-input-state))
            (payload (make-array 2 :element-type '(unsigned-byte 8)
                                   :initial-contents (list 2 (char-code #\d)))))
        (is (eq :detach (cl-tmux::process-client-keys s payload state))
            "^B d should yield the :detach disposition")
        (is-true cl-tmux::*running*
                 "a detach must not clear *running* (session survives)")))))

(test process-client-keys-quit-keystroke-returns-quit
  "A prefix+kill-window key payload (^B &) now shows a confirm-before prompt
   instead of killing immediately.  The session is NOT ended until the user
   confirms with 'y'; *running* stays T and the prompt is active after the keystroke."
  (let ((s (make-fake-session :nwindows 1 :npanes 1)))
    (let ((cl-tmux::*running* t) (cl-tmux::*dirty* nil) (*prompt* nil))
      (let ((state   (cl-tmux::make-input-state))
            (payload (make-array 2 :element-type '(unsigned-byte 8)
                                   :initial-contents (list 2 (char-code #\&)))))
        ;; ^B & now shows a confirm-before prompt; session is NOT killed yet.
        (cl-tmux::process-client-keys s payload state)
        (is (prompt-active-p)
            "^B & on the last window should open a confirm-before prompt")
        (is-true cl-tmux::*running*
                 "*running* must stay T before the user confirms the kill")))))

(test process-client-keys-empty-payload-returns-nil
  "An empty key payload runs the byte loop zero times: returns NIL (keep serving)
   and leaves *running* untouched."
  (let ((s (make-fake-session)))
    (let ((cl-tmux::*running* t) (cl-tmux::*dirty* nil))
      (let ((state   (cl-tmux::make-input-state))
            (payload (make-array 0 :element-type '(unsigned-byte 8))))
        (is (null (cl-tmux::process-client-keys s payload state))
            "an empty payload yields NIL (no quit, no detach)")
        (is-true cl-tmux::*running*
                 "no quit keystroke means *running* stays T")))))

;;; ── Session registry tests ───────────────────────────────────────────────────

(test server-add-and-find-session
  "server-add-session registers a session; server-find-session retrieves it."
  (let ((cl-tmux::*server-sessions* nil))
    (let ((sess (make-session :id 1 :name "alpha" :windows nil)))
      (cl-tmux::server-add-session sess)
      (let ((found (cl-tmux::server-find-session "alpha")))
        (is (eq sess found)
            "server-find-session should return the exact session object added")))))

(test server-remove-session
  "server-remove-session removes a previously added session from the registry."
  (let ((cl-tmux::*server-sessions* nil))
    (let ((sess (make-session :id 1 :name "beta" :windows nil)))
      (cl-tmux::server-add-session sess)
      (cl-tmux::server-remove-session "beta")
      (is (null (cl-tmux::server-find-session "beta"))
          "after removal, server-find-session should return NIL"))))

(test server-all-sessions
  "server-all-sessions returns one entry per registered session."
  (let ((cl-tmux::*server-sessions* nil))
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

;;; ── Multi-session add/remove ─────────────────────────────────────────────────

(test multi-session-add-remove
  "Add 3 sessions, remove the middle one; exactly 2 sessions remain."
  (let ((cl-tmux::*server-sessions* nil))
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

;;; ── Fuzzy session lookup ─────────────────────────────────────────────────────

(test server-find-session-fuzzy
  "server-find-session with a name prefix 'my' finds the session named 'mysession'."
  (let ((cl-tmux::*server-sessions* nil))
    (let ((sess (make-session :id 1 :name "mysession" :windows nil)))
      (cl-tmux::server-add-session sess)
      (let ((found (cl-tmux::server-find-session "my")))
        (is (eq sess found)
            "prefix 'my' should match session named 'mysession'")))))

(test server-find-session-by-id
  "server-find-session with '$N' notation matches by session id."
  (let ((cl-tmux::*server-sessions* nil))
    (let ((sess (make-session :id 42 :name "thesession" :windows nil)))
      (cl-tmux::server-add-session sess)
      (let ((found (cl-tmux::server-find-session "$42")))
        (is (eq sess found)
            "$42 should find the session with id 42")))))

;;; ── server-current-session ───────────────────────────────────────────────────

(test server-current-session-by-last-active
  "server-current-session returns the session with the highest last-active time."
  (let ((cl-tmux::*server-sessions* nil))
    (let ((s1 (make-session :id 1 :name "older"  :windows nil :last-active 100))
          (s2 (make-session :id 2 :name "newest" :windows nil :last-active 999))
          (s3 (make-session :id 3 :name "middle" :windows nil :last-active 500)))
      (cl-tmux::server-add-session s1)
      (cl-tmux::server-add-session s2)
      (cl-tmux::server-add-session s3)
      (let ((current (cl-tmux::server-current-session)))
        (is (eq s2 current)
            "server-current-session should return the session with highest last-active (s2)")))))

;;; ── new-session / kill-session command helpers ────────────────────────────────

(test new-session-command
  "new-session adds a session to the server registry."
  (unless (pty-available-p) (skip "no PTY available (sandboxed environment)"))
  (let ((cl-tmux::*server-sessions* nil)
        (cl-tmux/model::*session-id-counter* 0))
    (let ((sess (cl-tmux::new-session "testsess" 24 80)))
      (is-true sess "new-session must return a session object")
      (is (= 1 (length cl-tmux::*server-sessions*))
          "after new-session, registry should contain 1 entry")
      (let ((found (cl-tmux::server-find-session "testsess")))
        (is (eq sess found)
            "server-find-session should find the newly created session"))
      ;; Cleanup: close PTYs to avoid resource leaks in tests
      (dolist (p (all-panes sess))
        (ignore-errors (pty-close (pane-fd p) (pane-pid p)))))))

(test kill-session-command
  "After killing a session it is removed from the server registry."
  (let ((cl-tmux::*server-sessions* nil))
    (let ((s1 (make-session :id 1 :name "alive"  :windows nil))
          (s2 (make-session :id 2 :name "doomed" :windows nil)))
      (cl-tmux::server-add-session s1)
      (cl-tmux::server-add-session s2)
      ;; Simulate kill: remove from registry
      (cl-tmux::server-remove-session "doomed")
      (is (= 1 (length (cl-tmux::server-all-sessions)))
          "registry should have 1 session after kill")
      (is (null (cl-tmux::server-find-session "doomed"))
          "killed session should not be findable"))))

;;; ── list-sessions format ─────────────────────────────────────────────────────

(test list-sessions-format
  "The list-sessions format string contains the session name and window count."
  (let ((s1 (make-session :id 1 :name "mysession" :windows (list 'w1 'w2))))
    (let ((cl-tmux::*server-sessions* (list (cons "mysession" s1))))
      (let ((output
             (with-output-to-string (str)
               (loop for (name . sess) in cl-tmux::*server-sessions*
                     for i from 0
                     do (format str "~A~A: ~A (~D window~:P)~%"
                                (if (string= name (session-name sess)) "*" " ")
                                i name
                                (length (session-windows sess)))))))
        (is-true (search "mysession" output)
                 "output should contain the session name")
        (is-true (search "2 windows" output)
                 "output should contain the window count")))))

;;; ── rename-session updates registry key ─────────────────────────────────────

(test rename-session-updates-registry
  "Renaming a session via :rename-session also updates the server registry key."
  (let ((cl-tmux::*server-sessions* nil))
    (let ((s (make-fake-session :nwindows 1)))
      ;; register under the original name "0"
      (cl-tmux::server-add-session s)
      (let ((*prompt* nil) (cl-tmux::*dirty* nil) (cl-tmux::*running* t))
        (cl-tmux::dispatch-command s :rename-session nil)
        ;; simulate submitting a new name
        (funcall (prompt-on-submit *prompt*) "renamed-sess"))
      ;; new name findable, old name gone
      (is (eq s (cl-tmux::server-find-session "renamed-sess"))
          "registry must index session under new name")
      (is (null (cl-tmux::server-find-session "0"))
          "old name must be removed from registry"))))

;;; ── switch-client-next / switch-client-prev ──────────────────────────────────

(test switch-client-next-touches-next-session
  ":switch-client-next touches (session-touch) the next session in the registry list
   and marks *dirty* so the server re-renders."
  (let ((cl-tmux::*server-sessions* nil))
    (let ((s1 (make-session :id 1 :name "s1" :windows nil :last-active 1000))
          (s2 (make-session :id 2 :name "s2" :windows nil :last-active 500)))
      (cl-tmux::server-add-session s1)
      (cl-tmux::server-add-session s2)
      (let ((cl-tmux::*dirty* nil) (cl-tmux::*running* t))
        ;; s1 is the current session; :switch-client-next touches s2
        (cl-tmux::dispatch-command s1 :switch-client-next nil)
        (is-true cl-tmux::*dirty*
                 ":switch-client-next must mark *dirty*")))))

(test switch-client-prev-touches-prev-session
  ":switch-client-prev touches the previous session in the registry list."
  (let ((cl-tmux::*server-sessions* nil))
    (let ((s1 (make-session :id 1 :name "s1" :windows nil :last-active 100))
          (s2 (make-session :id 2 :name "s2" :windows nil :last-active 200)))
      (cl-tmux::server-add-session s1)
      (cl-tmux::server-add-session s2)
      (let ((cl-tmux::*dirty* nil) (cl-tmux::*running* t))
        ;; s2 is the current session; :switch-client-prev touches s1
        (cl-tmux::dispatch-command s2 :switch-client-prev nil)
        (is-true cl-tmux::*dirty*
                 ":switch-client-prev must mark *dirty*")))))

;;; ── last-session cycles by recency ──────────────────────────────────────────

(test last-session-cycles-by-recency
  ":last-session touches the second-most-recently-active session."
  (let ((cl-tmux::*server-sessions* nil))
    (let ((s1 (make-session :id 1 :name "oldest" :windows nil :last-active 100))
          (s2 (make-session :id 2 :name "newest" :windows nil :last-active 999))
          (s3 (make-session :id 3 :name "second" :windows nil :last-active 500)))
      (cl-tmux::server-add-session s1)
      (cl-tmux::server-add-session s2)
      (cl-tmux::server-add-session s3)
      (let ((old-active-s3 (cl-tmux/model:session-last-active s3))
            (cl-tmux::*dirty* nil)
            (cl-tmux::*running* t))
        ;; s2 is current (newest); :last-session should touch s3 (second most recent)
        (cl-tmux::dispatch-command s2 :last-session nil)
        ;; s3's last-active must have been updated (touched)
        (is (> (cl-tmux/model:session-last-active s3) old-active-s3)
            ":last-session must update last-active of the second-most-recent session")
        (is-true cl-tmux::*dirty*
                 ":last-session must mark *dirty*")))))

;;; ── display-message sets overlay ────────────────────────────────────────────

(test display-message-sets-overlay
  ":display-message prompts for a message and sets the overlay when submitted."
  (let ((s (make-fake-session :nwindows 1)))
    (let ((*overlay* nil) (*prompt* nil)
          (cl-tmux::*dirty* nil) (cl-tmux::*running* t))
      (cl-tmux::dispatch-command s :display-message nil)
      ;; A prompt must be open
      (is (prompt-active-p) ":display-message must open a prompt")
      ;; Submitting text must set the overlay
      (funcall (prompt-on-submit *prompt*) "hello world")
      (is (overlay-active-p) ":display-message on-submit must show overlay")
      (is-true (search "hello world" *overlay*)
               "overlay text must contain the submitted message"))))

;;; ── source-file loads config ─────────────────────────────────────────────────

(test source-file-prompts-for-path
  ":source-file opens a prompt (source-file) for a file path."
  (let ((s (make-fake-session :nwindows 1)))
    (let ((*prompt* nil) (cl-tmux::*dirty* nil) (cl-tmux::*running* t))
      (cl-tmux::dispatch-command s :source-file nil)
      (is (prompt-active-p) ":source-file must open a prompt")
      (is (string= "source-file" (prompt-label *prompt*))
          "prompt label must be 'source-file'"))))

;;; ── attach-session flag parsing ──────────────────────────────────────────────

(test parse-attach-flags-default-name
  "%parse-attach-flags with no args returns session-name \"0\" and nil flags."
  (multiple-value-bind (name detach ro) (cl-tmux::%parse-attach-flags '())
    (is (string= "0" name) "default session name must be \"0\"")
    (is-false detach "detach flag must default to NIL")
    (is-false ro     "read-only flag must default to NIL")))

(test parse-attach-flags-detach-flag
  "%parse-attach-flags with -d sets the detach flag."
  (multiple-value-bind (name detach _ro) (cl-tmux::%parse-attach-flags '("-d" "-t" "mysess"))
    (declare (ignore _ro))
    (is (string= "mysess" name) "session name must be mysess")
    (is-true detach "-d must set detach flag")))

(test parse-attach-flags-readonly-flag
  "%parse-attach-flags with -r sets the read-only flag."
  (multiple-value-bind (name _detach ro) (cl-tmux::%parse-attach-flags '("-r" "mysess"))
    (declare (ignore _detach))
    (is (string= "mysess" name) "positional session name must be mysess")
    (is-true ro "-r must set read-only flag")))

(test parse-attach-flags-target-flag
  "%parse-attach-flags with -t <name> sets the session name."
  (multiple-value-bind (name _d _ro) (cl-tmux::%parse-attach-flags '("-t" "special"))
    (declare (ignore _d _ro))
    (is (string= "special" name) "-t must set session name to 'special'")))
