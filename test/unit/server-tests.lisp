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
  "A prefix+kill-window key payload (^B &) on the last window returns :quit and
   clears *running* — the session itself ends."
  (let ((s (make-fake-session :nwindows 1 :npanes 1)))
    (let ((cl-tmux::*running* t) (cl-tmux::*dirty* nil))
      (let ((state   (cl-tmux::make-input-state))
            (payload (make-array 2 :element-type '(unsigned-byte 8)
                                   :initial-contents (list 2 (char-code #\&)))))
        (is (eq :quit (cl-tmux::process-client-keys s payload state))
            "^B & on the last window should yield the :quit disposition")
        (is-false cl-tmux::*running*
                  ":quit must clear *running* so the server stops")))))

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
