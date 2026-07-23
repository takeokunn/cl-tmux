(in-package #:cl-tmux/test)

;;;; Listener and readiness handling for the multi-client server loop.

(describe "server-multi-suite"

  ;;; ── accept-pending-connection / dispatch-ready-clients ──────────────────────

  ;; %accept-pending-connection accepts and registers a new client when the
  ;; listener fd appears in READY.
  (it "accept-pending-connection-registers-client-when-listener-ready"
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
                 (expect (= 1 (length cl-tmux::*clients*))))
            (ignore-errors (cl-tmux/net:close-socket peer)))))))

  ;; %accept-pending-connection does nothing when the listener fd is absent from
  ;; READY — no client is registered.
  (it "accept-pending-connection-noop-when-listener-not-ready"
    (with-isolated-hooks
      (with-test-listener (listener path (%test-socket-path "accept-helper-noop") :backlog 4)
        (let ((listener-fd (cl-tmux/net:socket-fd listener))
              (cl-tmux::*clients* nil))
          (cl-tmux::%accept-pending-connection listener listener-fd nil)
          (expect (null cl-tmux::*clients*))))))

  ;; %dispatch-ready-clients does not touch a client whose fd is absent from READY.
  (it "dispatch-ready-clients-skips-clients-not-in-ready-set"
    (with-fake-session (s)
      (let* ((conn (%make-test-conn))
             (cl-tmux::*clients* (list conn)))
        (setf (cl-tmux::client-conn-fd conn) 4242)
        (expect (null (cl-tmux::%dispatch-ready-clients s nil)))
        (expect (equal (list conn) cl-tmux::*clients*)))))

  ;; %dispatch-ready-clients drops a client whose stream yields EOF (a real closed
  ;; socket), removing it from *clients*.
  (it "dispatch-ready-clients-drops-client-on-eof"
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
                  (expect (null (cl-tmux::%dispatch-ready-clients s ready)))
                  (expect (null cl-tmux::*clients*)))))))))))
