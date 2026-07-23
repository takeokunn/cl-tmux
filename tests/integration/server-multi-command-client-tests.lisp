(in-package #:cl-tmux/test)

;;;; Multi-client server integration tests: command-client, exit policy, broadcast

(describe "server-multi-suite"

  ;;; -- Command client: forwards a command to the server ------------------------

  ;; A forwarded display-message produces a +msg-reply+ carrying the command's output
  ;; text — the server side of the `cl-tmux display -p` stdout channel.
  (it "command-client-receives-output-reply"
    (with-isolated-hooks
      (with-fake-session (s)
        (with-test-listener (listener path (%test-socket-path "reply") :backlog 4)
          (let* ((client      (cl-tmux/net:connect-to path))
                 (server-sock (cl-tmux/net:accept-connection listener))
                 (cl-tmux::*clients* nil))
            (when server-sock
              (let ((conn    (cl-tmux::%add-client server-sock))
                    (payload (cl-tmux/protocol::encode-command-payload
                              :display-message :args '("hello"))))
                ;; Run the forwarded command; the server replies with its output.
                (cl-tmux::%handle-multi-client-message
                 cl-tmux::+msg-command+ payload s conn)
                (expect (string= "hello"
                                  (cdr (first (cl-tmux::client-conn-message-log conn)))))
                (let ((ready (cl-tmux/pty:select-fds
                              (list (cl-tmux/net:socket-fd client)) 1000000)))
                  (expect ready :to-be-truthy)
                  (when ready
                    (multiple-value-bind (type payload)
                        (cl-tmux::read-frame (cl-tmux/net:socket-stream client))
                      (expect (eql cl-tmux::+msg-reply+ type))
                      (expect (search "hello" (cl-tmux::decode-text payload)))))))))))))

  ;; run-command-client forwards a command to the server as a decodable
  ;; +msg-command+ frame (the `cl-tmux <command>` CLI path).  A -t target rides
  ;; along in the args for the server to parse.
  (it "command-client-sends-decodable-command-frame"
    (let ((name (format nil "cmdtest-~D" (get-universal-time)))
          (cl-tmux::*socket-path-override* (%test-socket-path "cmdtest")))
      (with-test-listener (listener path (cl-tmux::socket-path name) :backlog 4)
        (cl-tmux::run-command-client name '("next-window" "-t" "2"))
        (let ((server-sock (cl-tmux/net:accept-connection listener)))
          (expect server-sock :to-be-truthy)
          (when server-sock
            (let ((ready (cl-tmux/pty:select-fds
                          (list (cl-tmux/net:socket-fd server-sock)) 1000000)))
              (expect ready :to-be-truthy)
              (when ready
                (multiple-value-bind (type payload)
                    (cl-tmux::read-frame (cl-tmux/net:socket-stream server-sock))
                  (expect (eql cl-tmux::+msg-command+ type))
                  (multiple-value-bind (cmd target args)
                      (cl-tmux::decode-command-payload payload)
                    (declare (ignore target))
                    (expect (eq :next-window cmd))
                    (expect (equal '("-t" "2") args)))))))))))

  ;; run-command-client sends stdin after the split-window -I command frame.
  (it "command-client-split-window-I-forwards-stdin-frame"
    (let ((name (format nil "cmdinput-~D" (get-universal-time)))
          (cl-tmux::*socket-path-override* (%test-socket-path "cmdinput")))
      (with-test-listener (listener path (cl-tmux::socket-path name) :backlog 4)
        (with-input-from-string (*standard-input* "client stdin")
          (cl-tmux::run-command-client name '("split-window" "-I")))
        (let ((server-sock (cl-tmux/net:accept-connection listener)))
          (expect server-sock :to-be-truthy)
          (when server-sock
            (let ((ready (cl-tmux/pty:select-fds
                          (list (cl-tmux/net:socket-fd server-sock)) 1000000)))
              (expect ready :to-be-truthy)
              (when ready
                (multiple-value-bind (type payload)
                    (cl-tmux::read-frame (cl-tmux/net:socket-stream server-sock))
                  (expect (eql cl-tmux::+msg-command+ type))
                  (multiple-value-bind (cmd target args)
                      (cl-tmux::decode-command-payload payload)
                    (declare (ignore target))
                    (expect (eq :split-window cmd))
                    (expect (equal '("-I") args))))))
            (let ((ready (cl-tmux/pty:select-fds
                          (list (cl-tmux/net:socket-fd server-sock)) 1000000)))
              (expect ready :to-be-truthy)
              (when ready
                (multiple-value-bind (type payload)
                    (cl-tmux::read-frame (cl-tmux/net:socket-stream server-sock))
                  (expect (eql cl-tmux::+msg-key+ type))
                  (expect (string= "client stdin" (cl-tmux::decode-text payload)))))))))))

  ;;; -- exit-unattached: terminate when the last client detaches ----------------

  ;; %exit-after-last-detach-p is true only when NO clients remain AND exit-unattached
  ;; is on; default (off) keeps the session alive across detaches.
  (it "exit-after-last-detach-respects-option"
    (with-fresh-options
      (let ((cl-tmux::*clients* nil))
        (cl-tmux/options:set-option "exit-unattached" t)
        (expect (cl-tmux::%exit-after-last-detach-p) :to-be-truthy))
      (let ((cl-tmux::*clients* nil))
        (cl-tmux/options:set-option "exit-unattached" nil)
        (expect (cl-tmux::%exit-after-last-detach-p) :to-be-falsy))
      (let ((cl-tmux::*clients* (list (cl-tmux::%make-client-conn))))
        (cl-tmux/options:set-option "exit-unattached" t)
        (expect (cl-tmux::%exit-after-last-detach-p) :to-be-falsy))))

  ;; %exit-when-empty-p is true only when NO sessions remain AND exit-empty is on
  ;; (default); off keeps the server alive with zero sessions.
  (it "exit-when-empty-respects-option"
    (with-fresh-options
      (let ((cl-tmux::*server-sessions* nil))
        (cl-tmux/options:set-option "exit-empty" t)
        (expect (cl-tmux::%exit-when-empty-p) :to-be-truthy))
      (let ((cl-tmux::*server-sessions* nil))
        (cl-tmux/options:set-option "exit-empty" nil)
        (expect (cl-tmux::%exit-when-empty-p) :to-be-falsy))
      (let ((cl-tmux::*server-sessions* (list (cons "0" (make-fake-session)))))
        (cl-tmux/options:set-option "exit-empty" t)
        (expect (cl-tmux::%exit-when-empty-p) :to-be-falsy))))

  ;;; -- Integration: a broadcast frame reaches every attached client ------------

  ;; Two clients attached to the server both receive a broadcast frame — the core
  ;; multi-client property (one render fanned out to all).
  (it "multi-broadcast-reaches-all-clients"
    (with-isolated-hooks
      (with-fake-session (s)
        (with-test-listener (listener path (%test-socket-path "mtest") :backlog 4)
          (let* ((client1 (cl-tmux/net:connect-to path))
                 (server1 (cl-tmux/net:accept-connection listener))
                 (client2 (cl-tmux/net:connect-to path))
                 (server2 (cl-tmux/net:accept-connection listener))
                 (cl-tmux::*clients* nil))
            (when (and server1 server2)
              (cl-tmux::%add-client server1)
              (cl-tmux::%add-client server2)
              (setf cl-tmux::*dirty* t)
              (cl-tmux::%broadcast-frame s)
              ;; Both client sockets must now have a frame to read.  Gate the
              ;; reads on select so a missing frame fails fast (not hangs).
              (dolist (client (list client1 client2))
                (let ((ready (cl-tmux/pty:select-fds
                              (list (cl-tmux/net:socket-fd client)) 1000000)))
                  (expect ready :to-be-truthy)
                  (when ready
                    (multiple-value-bind (type payload)
                        (cl-tmux::read-frame (cl-tmux/net:socket-stream client))
                      (declare (ignore payload))
                      (expect (eql cl-tmux::+msg-frame+ type)))))))))))))
