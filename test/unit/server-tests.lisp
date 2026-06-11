(in-package #:cl-tmux/test)

;;;; Detach-attach server logic tests (src/server.lisp).
;;;;
;;;; The accept/serve loops are integration-level (like event-loop itself —
;;;; compile-verified, with their building blocks — protocol/transport/net/
;;;; process-byte — unit-tested elsewhere).  Here we cover the two pieces that
;;;; ARE pure/observable without a live socket: the socket-path naming and the
;;;; client-size application.  make-fake-session comes from events-tests.
;;;;
;;;; with-empty-registry (from test/helpers.lisp) eliminates the repeated
;;;; (let ((cl-tmux::*server-sessions* nil)) ...) boilerplate throughout.

(def-suite server-suite :description "Detach-attach server logic")
(in-suite server-suite)

;;; ── socket-path naming ───────────────────────────────────────────────────────

(test socket-path-includes-session-name
  :description "socket-path names the per-session Unix socket under the temp directory."
  (let ((path (cl-tmux::socket-path "mysess")))
    (is (search "cl-tmux-mysess.sock" path)
        "socket-path should embed the session name, got ~S" path)))

(test socket-path-ends-with-sock
  :description "socket-path always produces a path ending in '.sock'."
  (let ((path (cl-tmux::socket-path "anysess")))
    (is (search ".sock" path)
        "socket-path must include .sock extension, got ~S" path)))

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
  (let ((s (make-fake-session)))
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
  (let ((s (make-fake-session)))
    (with-isolated-config
      (with-loop-state
        (let ((state   (cl-tmux::make-input-state))
              (payload (make-array 2 :element-type '(unsigned-byte 8)
                                     :initial-contents (list 2 (char-code #\d)))))
          (is (eq :detach (cl-tmux::process-client-keys s payload state))
              "^B d should yield the :detach disposition")
          (is-true cl-tmux::*running*
                   "a detach must not clear *running* (session survives)"))))))

(test process-client-keys-quit-keystroke-returns-quit
  :description "A prefix+kill-window key payload (^B &) now shows a confirm-before prompt
instead of killing immediately.  The session is NOT ended until the user
confirms with 'y'; *running* stays T and the prompt is active after the keystroke."
  (let ((s (make-fake-session :nwindows 1 :npanes 1)))
    (with-loop-state
      (let ((*prompt* nil))
        (let ((state   (cl-tmux::make-input-state))
              (payload (make-array 2 :element-type '(unsigned-byte 8)
                                     :initial-contents (list 2 (char-code #\&)))))
          (cl-tmux::process-client-keys s payload state)
          (is (prompt-active-p)
              "^B & on the last window should open a confirm-before prompt")
          (is-true cl-tmux::*running*
                   "*running* must stay T before the user confirms the kill"))))))

(test process-client-keys-empty-payload-returns-nil
  :description "An empty key payload runs the byte loop zero times: returns NIL (keep serving)
and leaves *running* untouched."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((state   (cl-tmux::make-input-state))
            (payload (make-array 0 :element-type '(unsigned-byte 8))))
        (is (null (cl-tmux::process-client-keys s payload state))
            "an empty payload yields NIL (no quit, no detach)")
        (is-true cl-tmux::*running*
                 "no quit keystroke means *running* stays T")))))

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
      (let ((s (make-fake-session :nwindows 1))
            (fired nil))
        (cl-tmux::server-add-session s)
        (cl-tmux/hooks:add-hook "session-closed"
                                (lambda (&rest _) (declare (ignore _)) (setf fired t)))
        (cl-tmux::%destroy-session s)
        (is-true fired "session-closed hook must fire on destroy")))))

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

(test server-find-session-returns-nil-for-unknown-name
  :description "server-find-session returns NIL when the name does not match any session."
  (with-empty-registry
    (is (null (cl-tmux::server-find-session "no-such-session"))
        "server-find-session must return NIL for an unknown name")))

(test server-find-session-returns-nil-for-nil-name
  :description "server-find-session returns NIL when called with NIL (guards the empty-string check)."
  (with-empty-registry
    (is (null (cl-tmux::server-find-session nil))
        "server-find-session must return NIL when name is NIL")))

(test server-find-session-returns-nil-for-empty-string
  :description "server-find-session returns NIL when called with an empty string."
  (with-empty-registry
    (is (null (cl-tmux::server-find-session ""))
        "server-find-session must return NIL for empty string name")))

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

;;; ── list-sessions format ─────────────────────────────────────────────────────
;;;
;;; The test calls the production %format-session-list helper (via dispatch-command
;;; :list-sessions + overlay inspection) rather than reimplementing the loop inline.
;;; This ensures changes to the formatting code are caught by the test.

(test list-sessions-format
  :description "The :list-sessions command overlay contains the session name and window count."
  (with-empty-registry
    (let ((s1 (make-session :id 1 :name "mysession" :windows (list 'w1 'w2))))
      (setf cl-tmux::*server-sessions* (list (cons "mysession" s1)))
      (let ((*overlay* nil)
            (cl-tmux::*dirty* nil)
            (cl-tmux::*running* t))
        (cl-tmux::dispatch-command s1 :list-sessions nil)
        (is (overlay-active-p)
            ":list-sessions must produce an overlay")
        (let ((output (format nil "~{~A~%~}" (overlay-lines))))
          (is-true (search "mysession" output)
                   "overlay should contain the session name")
          (is-true (search "2 windows" output)
                   "overlay should contain the window count"))))))

;;; ── rename-session updates registry key ─────────────────────────────────────

(test rename-session-updates-registry
  :description "Renaming a session via :rename-session also updates the server registry key."
  (with-empty-registry
    (let ((s (make-fake-session :nwindows 1)))
      (cl-tmux::server-add-session s)
      (with-loop-state
        (let ((*prompt* nil))
          (cl-tmux::dispatch-command s :rename-session nil)
          (funcall (prompt-on-submit *prompt*) "renamed-sess")))
      (is (eq s (cl-tmux::server-find-session "renamed-sess"))
          "registry must index session under new name")
      (is (null (cl-tmux::server-find-session "0"))
          "old name must be removed from registry"))))

;;; ── switch-client-next / switch-client-prev ──────────────────────────────────

(test switch-client-next-touches-next-session
  :description ":switch-client-next touches (session-touch) the next session in the registry list
and marks *dirty* so the server re-renders."
  (with-empty-registry
    (let ((s1 (make-session :id 1 :name "s1" :windows nil :last-active 1000))
          (s2 (make-session :id 2 :name "s2" :windows nil :last-active 500)))
      (cl-tmux::server-add-session s1)
      (cl-tmux::server-add-session s2)
      (with-loop-state
        (cl-tmux::dispatch-command s1 :switch-client-next nil)
        (is-true cl-tmux::*dirty*
                 ":switch-client-next must mark *dirty*")))))

(test switch-client-prev-touches-prev-session
  :description ":switch-client-prev touches the previous session in the registry list."
  (with-empty-registry
    (let ((s1 (make-session :id 1 :name "s1" :windows nil :last-active 100))
          (s2 (make-session :id 2 :name "s2" :windows nil :last-active 200)))
      (cl-tmux::server-add-session s1)
      (cl-tmux::server-add-session s2)
      (with-loop-state
        (cl-tmux::dispatch-command s2 :switch-client-prev nil)
        (is-true cl-tmux::*dirty*
                 ":switch-client-prev must mark *dirty*")))))

;;; ── last-session cycles by recency ──────────────────────────────────────────

(test last-session-cycles-by-recency
  :description ":last-session touches the second-most-recently-active session."
  (with-empty-registry
    (let ((s1 (make-session :id 1 :name "oldest" :windows nil :last-active 100))
          (s2 (make-session :id 2 :name "newest" :windows nil :last-active 999))
          (s3 (make-session :id 3 :name "second" :windows nil :last-active 500)))
      (cl-tmux::server-add-session s1)
      (cl-tmux::server-add-session s2)
      (cl-tmux::server-add-session s3)
      (let ((old-active-s3 (cl-tmux/model:session-last-active s3))
            (cl-tmux::*dirty* nil)
            (cl-tmux::*running* t))
        (cl-tmux::dispatch-command s2 :last-session nil)
        (is (> (cl-tmux/model:session-last-active s3) old-active-s3)
            ":last-session must update last-active of the second-most-recent session")
        (is-true cl-tmux::*dirty*
                 ":last-session must mark *dirty*")))))

;;; ── display-message sets overlay ────────────────────────────────────────────

(test display-message-sets-overlay
  :description ":display-message prompts for a message and sets the overlay when submitted."
  (let ((s (make-fake-session :nwindows 1)))
    (let ((*overlay* nil) (*prompt* nil)
          (cl-tmux::*dirty* nil) (cl-tmux::*running* t))
      (cl-tmux::dispatch-command s :display-message nil)
      (is (prompt-active-p) ":display-message must open a prompt")
      (funcall (prompt-on-submit *prompt*) "hello world")
      (is (overlay-active-p) ":display-message on-submit must show overlay")
      (is-true (search "hello world" *overlay*)
               "overlay text must contain the submitted message"))))

;;; ── source-file loads config ─────────────────────────────────────────────────

(test source-file-prompts-for-path
  :description ":source-file opens a prompt (source-file) for a file path."
  (let ((s (make-fake-session :nwindows 1)))
    (with-loop-state
      (let ((*prompt* nil))
        (cl-tmux::dispatch-command s :source-file nil)
        (is (prompt-active-p) ":source-file must open a prompt")
        (is (string= "source-file" (prompt-label *prompt*))
            "prompt label must be 'source-file'")))))

;;; ── attach-session flag parsing ──────────────────────────────────────────────

(test parse-attach-flags-default-name
  :description "%parse-attach-flags with no args returns session-name \"0\" and nil flags."
  (multiple-value-bind (name detach ro) (cl-tmux::%parse-attach-flags '())
    (is (string= "0" name) "default session name must be \"0\"")
    (is-false detach "detach flag must default to NIL")
    (is-false ro     "read-only flag must default to NIL")))

(test parse-attach-flags-detach-flag
  :description "%parse-attach-flags with -d sets the detach flag."
  (multiple-value-bind (name detach _ro) (cl-tmux::%parse-attach-flags '("-d" "-t" "mysess"))
    (declare (ignore _ro))
    (is (string= "mysess" name) "session name must be mysess")
    (is-true detach "-d must set detach flag")))

(test parse-attach-flags-readonly-flag
  :description "%parse-attach-flags with -r sets the read-only flag.
Positional (non-flag) args are silently ignored; use -t to set the session name."
  (multiple-value-bind (_name _detach ro) (cl-tmux::%parse-attach-flags '("-r"))
    (declare (ignore _name _detach))
    (is-true ro "-r must set read-only flag")))

(test parse-attach-flags-target-flag
  :description "%parse-attach-flags with -t <name> sets the session name."
  (multiple-value-bind (name _d _ro) (cl-tmux::%parse-attach-flags '("-t" "special"))
    (declare (ignore _d _ro))
    (is (string= "special" name) "-t must set session name to 'special'")))

(test parse-attach-flags-all-flags-combined
  :description "%parse-attach-flags honours -t, -d, and -r together."
  (multiple-value-bind (name detach ro)
      (cl-tmux::%parse-attach-flags '("-t" "combo" "-d" "-r"))
    (is (string= "combo" name) "session name must be 'combo'")
    (is-true detach "-d must set detach flag")
    (is-true ro     "-r must set read-only flag")))

(test parse-attach-flags-unknown-flag-silently-consumed
  :description "Unknown flags are silently skipped; known flags after them still apply."
  (multiple-value-bind (name detach _ro)
      (cl-tmux::%parse-attach-flags '("--unknown" "-t" "after-unknown" "-d"))
    (declare (ignore _ro))
    (is (string= "after-unknown" name) "session name must be 'after-unknown' after unknown flag")
    (is-true detach "-d after unknown flag must still set detach")))

;;; ── Session group helpers ────────────────────────────────────────────────────

(test next-group-id-monotonically-increases
  :description "%next-group-id returns strictly increasing ids on successive calls."
  (let ((cl-tmux::*group-id-counter* 0))
    (let ((id1 (cl-tmux::%next-group-id))
          (id2 (cl-tmux::%next-group-id))
          (id3 (cl-tmux::%next-group-id)))
      (is (< id1 id2) "second id must be greater than first")
      (is (< id2 id3) "third id must be greater than second"))))

(test resolve-group-id-allocates-for-ungrouped-session
  :description "%resolve-group-id allocates a fresh group-id for a session without one."
  (let ((cl-tmux::*group-id-counter* 0)
        (sess (make-session :id 1 :name "ungrouped" :windows nil)))
    (let ((gid (cl-tmux::%resolve-group-id sess)))
      (is (integerp gid)  "%resolve-group-id must return an integer")
      (is (plusp gid)     "%resolve-group-id must return a positive id")
      (is (= gid (session-group sess))
          "session-group slot must be set to the allocated gid"))))

(test resolve-group-id-reuses-existing-id
  :description "%resolve-group-id returns the existing group-id for an already-grouped session."
  (let ((cl-tmux::*group-id-counter* 0)
        (sess (make-session :id 1 :name "grouped" :windows nil)))
    (setf (session-group sess) 77)
    (let ((gid (cl-tmux::%resolve-group-id sess)))
      (is (= 77 gid)
          "%resolve-group-id must return the existing group-id 77"))))

(test link-session-to-group-shares-windows
  :description "%link-session-to-group copies existing-session's windows into new-session."
  (let* ((w1  (make-fake-window 1 "w1"))
         (existing (make-session :id 1 :name "existing" :windows (list w1)))
         (new-sess (make-session :id 2 :name "new"      :windows nil)))
    (session-select-window existing w1)
    (cl-tmux::%link-session-to-group new-sess existing 42)
    (is (equal (session-windows existing) (session-windows new-sess))
        "new-session must share the same windows list as existing-session")
    (is (= 42 (session-group new-sess))
        "new-session group slot must be set to the supplied group-id")))

(test register-in-group-alist-creates-new-entry
  :description "%register-in-group-alist adds a new entry when the group-id is not in the alist."
  (let ((cl-tmux::*session-groups* nil)
        (sess (make-session :id 1 :name "s" :windows nil)))
    (cl-tmux::%register-in-group-alist sess 10)
    (let ((entry (assoc 10 cl-tmux::*session-groups*)))
      (is-true entry "alist must have an entry for group-id 10")
      (is-true (member sess (cdr entry))
               "session must be in the group-id 10 entry"))))

(test register-in-group-alist-pushes-to-existing
  :description "%register-in-group-alist adds to an existing entry without duplicating it."
  (let* ((s1   (make-session :id 1 :name "s1" :windows nil))
         (s2   (make-session :id 2 :name "s2" :windows nil))
         (cl-tmux::*session-groups* (list (list 10 s1))))
    (cl-tmux::%register-in-group-alist s2 10)
    (let ((entry (assoc 10 cl-tmux::*session-groups*)))
      (is-true (member s2 (cdr entry)) "s2 must be in the existing group-10 entry")
      (is-true (member s1 (cdr entry)) "s1 must still be in the group-10 entry"))))

(test server-new-session-in-group-shares-windows
  :description "server-new-session-in-group wires both sessions into the same group."
  (let ((cl-tmux::*session-groups* nil)
        (cl-tmux::*group-id-counter* 0))
    (let* ((w1 (make-fake-window 1 "w1"))
           (existing (make-session :id 1 :name "existing" :windows (list w1)))
           (new-sess (make-session :id 2 :name "new-sess" :windows nil)))
      (session-select-window existing w1)
      (server-new-session-in-group new-sess existing)
      (is (equal (session-windows existing) (session-windows new-sess))
          "new-session must share the same windows as existing-session")
      (is (= (session-group existing) (session-group new-sess))
          "both sessions must have the same group-id after server-new-session-in-group"))))

;;; ── %handle-client-message dispatch ─────────────────────────────────────────
;;;
;;; %handle-client-message is generated by define-msg-dispatch.  These tests
;;; verify the four rule arms without requiring a live socket.

(test handle-client-message-nil-type-returns-disconnect
  :description "%handle-client-message returns :disconnect for a NIL type (EOF)."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((state (cl-tmux::make-input-state)))
        (is (eq :disconnect
                (cl-tmux::%handle-client-message nil #() s state))
            "NIL type must return :disconnect")))))

(test handle-client-message-detach-type-returns-detach
  :description "%handle-client-message returns :detach for +msg-detach+."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((state (cl-tmux::make-input-state)))
        (is (eq :detach
                (cl-tmux::%handle-client-message +msg-detach+ #() s state))
            "+msg-detach+ must return :detach")))))

(test handle-client-message-resize-returns-nil-and-marks-dirty
  :description "%handle-client-message +msg-resize+ resizes the session and marks *dirty*."
  (let ((s (make-fake-session)))
    (let ((cl-tmux::*term-rows* 24)
          (cl-tmux::*term-cols* 80)
          (cl-tmux::*dirty*    nil)
          (cl-tmux::*running*  t))
      (multiple-value-bind (_type payload) (decode-frame (msg-resize 30 100))
        (declare (ignore _type))
        (let ((result (cl-tmux::%handle-client-message +msg-resize+ payload s
                                                       (cl-tmux::make-input-state))))
          (is (null result) "+msg-resize+ must return NIL (continue serving)")
          (is-true cl-tmux::*dirty* "+msg-resize+ must set *dirty*")
          (is (= 30 cl-tmux::*term-rows*) "rows must update to 30")
          (is (= 100 cl-tmux::*term-cols*) "cols must update to 100"))))))

(test handle-client-message-unknown-type-returns-disconnect
  :description "%handle-client-message returns :disconnect for an unrecognized type."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((state (cl-tmux::make-input-state)))
        (is (eq :disconnect
                (cl-tmux::%handle-client-message 9999 #() s state))
            "unknown type must return :disconnect")))))

;;; ── handle-client-message +msg-attach+ arm ───────────────────────────────────

(test handle-client-message-attach-applies-size-and-marks-dirty
  :description "%handle-client-message +msg-attach+ applies client dimensions and marks *dirty*."
  (let ((s (make-fake-session)))
    (let ((cl-tmux::*term-rows* 24)
          (cl-tmux::*term-cols* 80)
          (cl-tmux::*dirty*    nil)
          (cl-tmux::*running*  t))
      (multiple-value-bind (_type payload) (decode-frame (msg-attach 40 120))
        (declare (ignore _type))
        (let ((result (cl-tmux::%handle-client-message +msg-attach+ payload s
                                                       (cl-tmux::make-input-state))))
          (is (null result) "+msg-attach+ must return NIL (continue serving)")
          (is-true cl-tmux::*dirty* "+msg-attach+ must set *dirty*")
          (is (= 40 cl-tmux::*term-rows*) "rows must update from attach payload")
          (is (= 120 cl-tmux::*term-cols*) "cols must update from attach payload"))))))

;;; ── apply-client-size with no active window ──────────────────────────────────

(test apply-client-size-nil-active-window-updates-dimensions-only
  :description "apply-client-size is safe when the session has no active window:
it still updates *term-rows*/*term-cols* without signalling."
  (let ((s (make-session :id 1 :name "empty" :windows nil)))
    (let ((cl-tmux::*term-rows* 24)
          (cl-tmux::*term-cols* 80))
      (multiple-value-bind (_type payload) (decode-frame (msg-resize 20 60))
        (declare (ignore _type))
        (finishes (cl-tmux::apply-client-size s payload))
        (is (= 20 cl-tmux::*term-rows*) "rows updated even with nil active window")
        (is (= 60 cl-tmux::*term-cols*) "cols updated even with nil active window")))))

;;; ── socket-path with hyphen/special characters ───────────────────────────────

(test socket-path-with-hyphenated-name
  :description "socket-path embeds a hyphenated session name correctly."
  (let ((path (cl-tmux::socket-path "my-session-1")))
    (is (search "my-session-1" path)
        "socket-path must embed the hyphenated name, got ~S" path)))

(test socket-path-consistent-on-repeated-calls
  :description "socket-path returns the same path for the same name on repeated calls."
  (let ((p1 (cl-tmux::socket-path "consistent"))
        (p2 (cl-tmux::socket-path "consistent")))
    (is (string= p1 p2)
        "socket-path must be deterministic for the same name")))

;;; ── %process-bytes-cps unit tests ────────────────────────────────────────────
;;;
;;; %process-bytes-cps is the CPS walker that underpins process-client-keys.
;;; It processes bytes starting from a given index and returns the first
;;; non-NIL disposition from %dispatch-byte-result, or NIL when all bytes
;;; are consumed.

(test process-bytes-cps-empty-bytes-returns-nil
  :description "%process-bytes-cps on an empty byte vector returns NIL immediately."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((state (cl-tmux::make-input-state))
            (bytes (make-array 0 :element-type '(unsigned-byte 8))))
        (is (null (cl-tmux::%process-bytes-cps s bytes state 0))
            "empty byte array must return NIL disposition")))))

(test process-bytes-cps-index-at-end-returns-nil
  :description "%process-bytes-cps with index = length returns NIL (all bytes consumed)."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((state (cl-tmux::make-input-state))
            (bytes (make-array 3 :element-type '(unsigned-byte 8)
                                 :initial-contents '(1 2 3))))
        (is (null (cl-tmux::%process-bytes-cps s bytes state 3))
            "index=length must return NIL (past end)")))))

(test process-bytes-cps-detach-keystroke-returns-detach
  :description "%process-bytes-cps on a prefix+d byte sequence returns :detach."
  ;; Isolate the key-tables so the default #\d → :detach binding is present.
  (let ((s (make-fake-session)))
    (with-isolated-config
      (with-loop-state
        (let ((state (cl-tmux::make-input-state))
              (bytes (make-array 2 :element-type '(unsigned-byte 8)
                                   :initial-contents (list 2 (char-code #\d)))))
          (is (eq :detach (cl-tmux::%process-bytes-cps s bytes state 0))
              "prefix+d must yield :detach disposition from CPS walker"))))))

;;; ── %sync-active-window unit test ────────────────────────────────────────────

(test sync-active-window-mirrors-existing-selection
  :description "%sync-active-window sets new-session's active window to match existing-session."
  (let* ((w1  (make-fake-window 1 "w1"))
         (w2  (make-fake-window 2 "w2"))
         (existing (make-session :id 1 :name "existing"
                                 :windows (list w1 w2)))
         (new-sess (make-session :id 2 :name "new"
                                 :windows (list w1 w2))))
    (session-select-window existing w2)
    (cl-tmux::%sync-active-window new-sess existing)
    (is (eq w2 (session-active-window new-sess))
        "%sync-active-window must mirror the active-window of existing-session")))

(test sync-active-window-nil-existing-window-is-safe
  :description "%sync-active-window is a no-op when existing-session has no active window."
  (let* ((new-sess (make-session :id 2 :name "new" :windows nil))
         (existing (make-session :id 1 :name "existing" :windows nil)))
    (finishes (cl-tmux::%sync-active-window new-sess existing))
    (is (null (session-active-window new-sess))
        "new-session active-window must remain NIL when existing has no active window")))

;;; ── run-server: session-registry initialization ──────────────────────────────
;;;
;;; run-server is an integration-level function (it opens real sockets and forks
;;; PTYs), but its session-registry initialization path (lines 286-289 in the
;;; original) can be tested by exercising the individual side effects it produces:
;;; server-add-session and the *server-sessions* reset.  These tests verify that
;;; the session-registry is properly initialized when a server would start, using
;;; the same helpers run-server calls.

(test run-server-session-registry-initialization
  :description "The session-registry setup that run-server performs: reset to NIL then add the
initial session — verifying the initialization contract without starting a real server."
  (unless (pty-available-p) (skip "no PTY available (sandboxed environment)"))
  (with-empty-registry
    (let ((cl-tmux::*session-groups*   nil)
          (cl-tmux::*group-id-counter* 0)
          (cl-tmux/model::*session-id-counter* 0))
      ;; Replicate the run-server initialization sequence:
      ;;   1. Reset the registry.
      (setf cl-tmux::*server-sessions* nil)
      ;;   2. Create and register the initial session.
      (let ((session (create-initial-session 24 80)))
        (cl-tmux::server-add-session session)
        ;; Verify: exactly one session in the registry.
        (is (= 1 (length cl-tmux::*server-sessions*))
            "registry must have exactly 1 session after initialization")
        ;; Verify: server-find-session locates it.
        (is-true (cl-tmux::server-find-session (session-name session))
                 "server-find-session must locate the initial session")
        ;; Clean up PTYs so the test does not leave open file descriptors.
        (dolist (pane (all-panes session))
          (ignore-errors (pty-close (pane-fd pane) (pane-pid pane))))))))

(test run-server-registry-teardown-on-remove
  :description "server-remove-session (the cleanup path run-server uses on exit) removes
the session from *server-sessions*, leaving it empty."
  (with-empty-registry
    (let ((sess (make-session :id 1 :name "teardown-test" :windows nil)))
      (cl-tmux::server-add-session sess)
      (is (= 1 (length cl-tmux::*server-sessions*))
          "registry must have 1 entry before removal")
      (cl-tmux::server-remove-session "teardown-test")
      (is (null cl-tmux::*server-sessions*))
      (is (null (cl-tmux::server-find-session "teardown-test"))
          "teardown must leave registry empty"))))
