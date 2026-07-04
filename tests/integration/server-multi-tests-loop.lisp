(in-package #:cl-tmux/test)

(in-suite server-multi-suite)

;;;; Listener and readiness handling for the multi-client server loop.

;;; ── accept-pending-connection / dispatch-ready-clients ──────────────────────

(test accept-pending-connection-registers-client-when-listener-ready
  "%accept-pending-connection accepts and registers a new client when the
   listener fd appears in READY."
  (with-isolated-hooks
    (with-test-listener (listener path (%test-socket-path "accept-helper") :backlog 4)
      (let* ((listener-fd (cl-tmux/net:socket-fd listener))
             (cl-tmux::*clients* nil)
             (peer (cl-tmux/net:connect-to path)))
        (unwind-protect
             (progn
               ;; Give the connection a moment to become acceptable.
               (cl-tmux/pty:select-fds (list listener-fd) 1000000)
               (cl-tmux::%accept-pending-connection listener listener-fd (list listener-fd))
               (is (= 1 (length cl-tmux::*clients*))
                   "a ready listener fd must register exactly one new client"))
          (ignore-errors (cl-tmux/net:close-socket peer)))))))

(test accept-pending-connection-noop-when-listener-not-ready
  "%accept-pending-connection does nothing when the listener fd is absent from
   READY — no client is registered."
  (with-isolated-hooks
    (with-test-listener (listener path (%test-socket-path "accept-helper-noop") :backlog 4)
      (let ((listener-fd (cl-tmux/net:socket-fd listener))
            (cl-tmux::*clients* nil))
        (cl-tmux::%accept-pending-connection listener listener-fd nil)
        (is (null cl-tmux::*clients*)
            "an unready listener fd must not register any client")))))

(test dispatch-ready-clients-skips-clients-not-in-ready-set
  "%dispatch-ready-clients does not touch a client whose fd is absent from READY."
  (with-fake-session (s)
    (let* ((conn (%make-test-conn))
           (cl-tmux::*clients* (list conn)))
      (setf (cl-tmux::client-conn-fd conn) 4242)
      (is (null (cl-tmux::%dispatch-ready-clients s nil))
          "no ready fds → NIL, no client is dispatched")
      (is (equal (list conn) cl-tmux::*clients*)
          "an unready client must remain untouched in the registry"))))

(test dispatch-ready-clients-drops-client-on-eof
  "%dispatch-ready-clients drops a client whose stream yields EOF (a real closed
   socket), removing it from *clients*."
  (with-isolated-hooks
    (with-fake-session (s)
      (with-test-listener (listener path (%test-socket-path "dispatch-helper") :backlog 4)
        (let* ((client      (cl-tmux/net:connect-to path))
               (server-sock (cl-tmux/net:accept-connection listener))
               (cl-tmux::*clients* nil))
          (when server-sock
            (let ((conn (cl-tmux::%add-client server-sock)))
              ;; Client half-closes: server-side read now sees EOF.
              (cl-tmux/net:close-socket client)
              (let ((ready (list (cl-tmux::client-conn-fd conn))))
                (is (null (cl-tmux::%dispatch-ready-clients s ready))
                    "an EOF dispatch keeps serving (returns NIL, not :quit)")
                (is (null cl-tmux::*clients*)
                    "the EOF'd client must be dropped from the registry")))))))))
