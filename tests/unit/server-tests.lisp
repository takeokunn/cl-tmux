(in-package #:cl-tmux/test)

;;;; socket-path, apply-client-size, dispatch-byte, process-client-keys, session-registry, link/unlink-window — part I

(def-suite server-suite :description "Detach-attach server logic")
(in-suite server-suite)

;;; ── socket-path naming ───────────────────────────────────────────────────────

(test socket-path-properties-table
  "socket-path embeds the session name in the filename and always ends with '.sock'."
  (dolist (row '(("mysess"  "cl-tmux-mysess.sock" "session name embedded in path")
                 ("anysess" ".sock"               "path always ends with .sock")))
    (destructuring-bind (sess expected desc) row
      (let ((path (cl-tmux::socket-path sess)))
        (is (search expected path) "~A: got ~S" desc path)))))

(test socket-path-distinct-for-different-names
  :description "socket-path returns distinct paths for distinct session names."
  (let ((p1 (cl-tmux::socket-path "alpha"))
        (p2 (cl-tmux::socket-path "beta")))
    (is (string/= p1 p2)
        "socket-path must be distinct for different session names")))

;;; ── apply-client-size ────────────────────────────────────────────────────────

(test apply-client-size-updates-dimensions
  :description "apply-client-size is a pure resize transform: it decodes a rows,cols payload,
updates *term-rows*/*term-cols*, and relayouts the active window.
It does NOT set *dirty* — that is the caller's responsibility (data/logic separation)."
  (with-fake-session (s)
    (let ((cl-tmux::*term-rows* 24)
          (cl-tmux::*term-cols* 80)
          (cl-tmux::*dirty* nil))
      (multiple-value-bind (type payload) (decode-frame (msg-resize 30 100))
        (declare (ignore type))
        (cl-tmux::apply-client-size s payload)
        (is (= 30 cl-tmux::*term-rows*) "rows updated from payload")
        (is (= 100 cl-tmux::*term-cols*) "cols updated from payload")
        (is-false cl-tmux::*dirty*
                  "apply-client-size must NOT set *dirty* (caller's responsibility)")))))

;;; ── %dispatch-byte-result (table-driven) ────────────────────────────────────
;;;
;;; %dispatch-byte-result is now a PURE predicate: it classifies a process-byte
;;; result as a disposition (:quit, :detach, NIL) without mutating *running*.
;;; *running* is cleared by %handle-client-message when it acts on :quit.
;;; The four cases differ only in input and expected disposition.

(test dispatch-byte-result-table
  :description "%dispatch-byte-result maps :quit→:quit, :detach→:detach,
NIL→NIL, and any other value→NIL.  It is a pure predicate and does NOT
mutate *running* — that is the caller's (%handle-client-message) responsibility."
  ;; Cases: (input expected-disposition)
  ;; *running* must remain T in every case — mutation is the caller's job.
  (let ((cases '((:quit    :quit)
                 (:detach  :detach)
                 (nil      nil)
                 (:other   nil))))
    (dolist (c cases)
      (destructuring-bind (input expected) c
        (with-loop-state
          (let ((disp (cl-tmux::%dispatch-byte-result input)))
            (is (eq expected disp)
                "input ~S: expected disposition ~S, got ~S"
                input expected disp)
            ;; Pure predicate contract: *running* is never touched by %dispatch-byte-result.
            (is-true cl-tmux::*running*
                     "input ~S: %dispatch-byte-result must not mutate *running*"
                     input)))))))

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
  :description "A prefix+detach key payload (^B d) returns :detach and leaves *running* T —
a detach disconnects the client but the session must survive for re-attach."
  ;; Isolate the key-tables so the default #\d → :detach binding is present.
  (with-fake-session (s)
    (with-isolated-config
      (let ((state   (cl-tmux::make-input-state))
            (payload (make-array 2 :element-type '(unsigned-byte 8)
                                   :initial-contents (list 2 (char-code #\d)))))
        (is (eq :detach (cl-tmux::process-client-keys s payload state))
            "^B d should yield the :detach disposition")
        (is-true cl-tmux::*running*
                 "a detach must not clear *running* (session survives)")))))

(test process-client-keys-quit-keystroke-returns-quit
  :description "A prefix+kill-window key payload (^B &) now shows a confirm-before prompt
instead of killing immediately.  The session is NOT ended until the user
confirms with 'y'; *running* stays T and the prompt is active after the keystroke."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (let ((*prompt* nil))
      (let ((state   (cl-tmux::make-input-state))
            (payload (make-array 2 :element-type '(unsigned-byte 8)
                                   :initial-contents (list 2 (char-code #\&)))))
        (cl-tmux::process-client-keys s payload state)
        (is (prompt-active-p)
            "^B & on the last window should open a confirm-before prompt")
        (is-true cl-tmux::*running*
                 "*running* must stay T before the user confirms the kill")))))

(test process-client-keys-empty-payload-returns-nil
  :description "An empty key payload runs the byte loop zero times: returns NIL (keep serving)
and leaves *running* untouched."
  (with-fake-session (s)
    (let ((state   (cl-tmux::make-input-state))
          (payload (make-array 0 :element-type '(unsigned-byte 8))))
      (is (null (cl-tmux::process-client-keys s payload state))
          "an empty payload yields NIL (no quit, no detach)")
      (is-true cl-tmux::*running*
               "no quit keystroke means *running* stays T"))))

;;; ── Session registry tests ───────────────────────────────────────────────────

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

;;; ── link-window / unlink-window (cross-session window sharing) ────────────────

(test window-session-count-counts-sessions-containing-window
  "%window-session-count returns the number of registered sessions holding a window."
  (with-empty-registry
    (let* ((alpha (make-fake-session :nwindows 1))
           (beta  (make-fake-session :nwindows 1))
           (win   (first (cl-tmux/model:session-windows alpha))))
      (setf (cl-tmux/model:session-name alpha) "alpha"
            (cl-tmux/model:session-name beta)  "beta")
      (cl-tmux::server-add-session alpha)
      (cl-tmux::server-add-session beta)
      (is (= 1 (cl-tmux::%window-session-count win))
          "window initially in 1 session")
      ;; Share alpha's window into beta (what link-window does).
      (cl-tmux/model:session-insert-window beta win)
      (is (= 2 (cl-tmux::%window-session-count win))
          "after sharing, window is in 2 sessions"))))

(test link-window-shares-window-into-destination
  "link-window -s src -t dst makes the source window appear in dst (no collision)."
  (with-empty-registry
    (let* ((alpha (make-fake-session :nwindows 1))
           (beta  (make-fake-session :nwindows 1))
           (alpha-win (first (cl-tmux/model:session-windows alpha))))
      (setf (cl-tmux/model:session-name alpha) "alpha"
            (cl-tmux/model:session-name beta)  "beta")
      ;; Give beta a distinct window id so alpha's window 0 links without a
      ;; collision — exercises the clean link path (no kill-window).
      (setf (cl-tmux/model:window-id (first (cl-tmux/model:session-windows beta))) 9)
      (cl-tmux::server-add-session alpha)
      (cl-tmux::server-add-session beta)
      ;; Bind *overlay* so the show-overlay status message does not leak into
      ;; later tests (an active overlay changes %ground-input-state dispatch).
      (let ((cl-tmux/prompt:*overlay* nil))
        (cl-tmux::%cmd-link-window alpha '("-s" "alpha:0" "-t" "beta")))
      (is-true (member alpha-win (cl-tmux/model:session-windows beta))
               "alpha's window must now appear in beta after link-window"))))

(test unlink-window-shared-removes-from-one-session-only
  "unlink-window on a window shared by 2 sessions removes it from the target only."
  (with-empty-registry
    (let* ((alpha (make-fake-session :nwindows 1))
           (beta  (make-fake-session :nwindows 1))
           (win   (first (cl-tmux/model:session-windows alpha))))
      (setf (cl-tmux/model:session-name alpha) "alpha"
            (cl-tmux/model:session-name beta)  "beta")
      (cl-tmux::server-add-session alpha)
      (cl-tmux::server-add-session beta)
      ;; Share win into beta, then make it beta's active window.
      (cl-tmux/model:session-insert-window beta win)
      (cl-tmux/model:session-select-window beta win)
      (let ((cl-tmux/prompt:*overlay* nil))
        (cl-tmux::%cmd-unlink-window beta nil))
      (is-false (member win (cl-tmux/model:session-windows beta))
                "window unlinked from beta")
      (is-true (member win (cl-tmux/model:session-windows alpha))
               "window still present in alpha (not orphaned)"))))

(test link-window-fires-window-linked-hook
  "link-window fires +hook-window-linked+ when a window is linked in."
  (with-empty-registry
    (with-isolated-hooks
      (let* ((alpha (make-fake-session :nwindows 1))
             (beta  (make-fake-session :nwindows 1))
             (fired nil))
        (setf (cl-tmux/model:session-name alpha) "alpha"
              (cl-tmux/model:session-name beta)  "beta")
        (setf (cl-tmux/model:window-id (first (cl-tmux/model:session-windows beta))) 9)
        (cl-tmux::server-add-session alpha)
        (cl-tmux::server-add-session beta)
        (cl-tmux/hooks:add-hook "window-linked"
                                (lambda (&rest _) (declare (ignore _)) (setf fired t)))
        (let ((cl-tmux/prompt:*overlay* nil))
          (cl-tmux::%cmd-link-window alpha '("-s" "alpha:0" "-t" "beta")))
        (is-true fired "window-linked hook must fire on link-window")))))

(test unlink-window-fires-window-unlinked-hook
  "unlink-window fires +hook-window-unlinked+ when a shared window is unlinked."
  (with-empty-registry
    (with-isolated-hooks
      (let* ((alpha (make-fake-session :nwindows 1))
             (beta  (make-fake-session :nwindows 1))
             (win   (first (cl-tmux/model:session-windows alpha)))
             (fired nil))
        (setf (cl-tmux/model:session-name alpha) "alpha"
              (cl-tmux/model:session-name beta)  "beta")
        (cl-tmux::server-add-session alpha)
        (cl-tmux::server-add-session beta)
        (cl-tmux/model:session-insert-window beta win)
        (cl-tmux/model:session-select-window beta win)
        (cl-tmux/hooks:add-hook "window-unlinked"
                                (lambda (&rest _) (declare (ignore _)) (setf fired t)))
        (let ((cl-tmux/prompt:*overlay* nil))
          (cl-tmux::%cmd-unlink-window beta nil))
        (is-true fired "window-unlinked hook must fire on unlink-window")))))

(test destroy-session-fires-session-closed-hook
  "%destroy-session removes the session AND fires +hook-session-closed+."
  (with-empty-registry
    (with-isolated-hooks
      (let ((fired nil))
        (with-fake-session (s :nwindows 1)
          (cl-tmux::server-add-session s)
          (cl-tmux/hooks:add-hook "session-closed"
                                  (lambda (&rest _) (declare (ignore _)) (setf fired t)))
          (cl-tmux::%destroy-session s)
          (is-true fired "session-closed hook must fire on destroy"))))))

;;; ── Reference-counted PTY teardown for session groups ────────────────────────
;;;
;;; Grouped sessions (new-session -t) SHARE the same window structs.  Destroying
;;; one grouped session must not close the shared windows' PTYs while another
;;; session still references them.  The fix iterates session-windows and only
;;; closes a window's PTYs when %window-session-count <= 1.  These tests spy on
;;; pty-close (its only observable effect is the call itself — it does not mutate
;;; pane-fd) by temporarily replacing its fdefinition.

(test destroy-grouped-session-keeps-shared-window-ptys-open
  "Destroying ONE session in a group must NOT close the PTYs of a window another
   grouped session still shares."
  (with-empty-registry
    (let ((target  (make-fake-session :nwindows 1))
          (grouped (make-fake-session :nwindows 1))
          (closed  0))
      (setf (cl-tmux::session-name target)  "base"
            (cl-tmux::session-name grouped) "clone"
            ;; grouped SHARES target's window list (same structs), like `new-session -t base`.
            (cl-tmux::session-windows grouped) (cl-tmux::session-windows target))
      (cl-tmux::server-add-session target)
      (cl-tmux::server-add-session grouped)
      (let ((orig (fdefinition 'cl-tmux/pty:pty-close)))
        (unwind-protect
             (progn
               (setf (fdefinition 'cl-tmux/pty:pty-close)
                     (lambda (fd pid) (declare (ignore fd pid)) (incf closed)))
               (cl-tmux::%destroy-session grouped))
          (setf (fdefinition 'cl-tmux/pty:pty-close) orig)))
      (is (zerop closed)
          "shared window's PTYs must NOT be closed while 'base' still references them")
      (is (null (cl-tmux::server-find-session "clone"))
          "the destroyed grouped session is removed from the registry")
      (is (not (null (cl-tmux::server-find-session "base")))
          "the surviving grouped session remains"))))

(test destroy-ungrouped-session-closes-its-ptys
  "Regression guard: an ungrouped (single-reference) session's PTYs ARE still
   closed on destroy — the reference-counted guard does not change the common case."
  (with-empty-registry
    (let ((sess   (make-fake-session :nwindows 1 :npanes 2))
          (closed  0))
      (setf (cl-tmux::session-name sess) "solo")
      (cl-tmux::server-add-session sess)
      (let ((orig (fdefinition 'cl-tmux/pty:pty-close)))
        (unwind-protect
             (progn
               (setf (fdefinition 'cl-tmux/pty:pty-close)
                     (lambda (fd pid) (declare (ignore fd pid)) (incf closed)))
               (cl-tmux::%destroy-session sess))
          (setf (fdefinition 'cl-tmux/pty:pty-close) orig)))
      (is (= 2 closed)
          "both panes of the unshared window are closed (window-session-count = 1)"))))

(test rename-session-does-not-fire-session-closed
  "rename-session removes+re-adds its registry entry but must NOT fire
   session-closed (only actual destruction does)."
  (with-empty-registry
    (with-isolated-hooks
      (let ((s (make-fake-session :nwindows 1))
            (fired nil))
        (cl-tmux::server-add-session s)
        (cl-tmux/hooks:add-hook "session-closed"
                                (lambda (&rest _) (declare (ignore _)) (setf fired t)))
        (cl-tmux::%cmd-rename-session s '("renamed"))
        (is-false fired "rename-session must NOT fire session-closed")))))

(test unlink-window-only-session-needs-k-flag
  "unlink-window on a window present in only one session is refused without -k."
  (with-empty-registry
    (let* ((alpha (make-fake-session :nwindows 2))
           (win   (first (cl-tmux/model:session-windows alpha))))
      (setf (cl-tmux/model:session-name alpha) "alpha")
      (cl-tmux::server-add-session alpha)
      (cl-tmux/model:session-select-window alpha win)
      ;; No -k: window must remain (only in this session).
      (let ((cl-tmux/prompt:*overlay* nil))
        (cl-tmux::%cmd-unlink-window alpha nil))
      (is-true (member win (cl-tmux/model:session-windows alpha))
               "window must remain without -k (would orphan otherwise)"))))

(test server-all-sessions-empty-registry
  :description "server-all-sessions returns NIL (empty list) when no sessions are registered."
  (with-empty-registry
    (is (null (cl-tmux::server-all-sessions))
        "server-all-sessions on empty registry must return NIL")))

(test server-find-session-nil-inputs-table
  "server-find-session returns NIL for an unknown name, NIL, or an empty string."
  (dolist (row '(("no-such-session" "unknown name → nil")
                 (nil               "nil input → nil")
                 (""                "empty string → nil")))
    (destructuring-bind (input desc) row
      (with-empty-registry
        (is (null (cl-tmux::server-find-session input)) "~A" desc)))))

;;; ── Multi-session add/remove ─────────────────────────────────────────────────

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

;;; ── Fuzzy session lookup ─────────────────────────────────────────────────────

(test server-find-session-fuzzy
  :description "server-find-session with a name prefix 'my' finds the session named 'mysession'."
  (with-empty-registry
    (let ((sess (make-session :id 1 :name "mysession" :windows nil)))
      (cl-tmux::server-add-session sess)
      (let ((found (cl-tmux::server-find-session "my")))
        (is (eq sess found)
            "prefix 'my' should match session named 'mysession'")))))

(test server-find-session-by-id
  :description "server-find-session with '$N' notation matches by session id."
  (with-empty-registry
    (let ((sess (make-session :id 42 :name "thesession" :windows nil)))
      (cl-tmux::server-add-session sess)
      (let ((found (cl-tmux::server-find-session "$42")))
        (is (eq sess found)
            "$42 should find the session with id 42")))))

(test server-find-session-by-id-not-found
  :description "server-find-session with '$N' returns NIL when no session has that id."
  (with-empty-registry
    (let ((sess (make-session :id 1 :name "sess" :windows nil)))
      (cl-tmux::server-add-session sess)
      (is (null (cl-tmux::server-find-session "$999"))
          "$999 must return NIL when no session has id 999"))))

;;; ── server-current-session ───────────────────────────────────────────────────

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

;;; ── new-session / kill-session command helpers ────────────────────────────────

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
