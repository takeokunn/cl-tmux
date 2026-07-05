(in-package #:cl-tmux/test)

;;;; Multi-client server integration tests: command-client, exit policy, broadcast

(in-suite server-multi-suite)

;;; -- Command client: forwards a command to the server ------------------------

(test command-client-receives-output-reply
  "A forwarded display-message produces a +msg-reply+ carrying the command's output
   text — the server side of the `cl-tmux display -p` stdout channel."
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
              (is (string= "hello"
                           (cdr (first (cl-tmux::client-conn-message-log conn))))
                  "forwarded display-message must be logged for that client")
              (let ((ready (cl-tmux/pty:select-fds
                            (list (cl-tmux/net:socket-fd client)) 1000000)))
                (is-true ready "a command-output reply must arrive")
                (when ready
                  (multiple-value-bind (type payload)
                      (cl-tmux::read-frame (cl-tmux/net:socket-stream client))
                    (is (eql cl-tmux::+msg-reply+ type)
                        "the reply frame must be +msg-reply+")
                    (is (search "hello" (cl-tmux::decode-text payload))
                        "the reply must carry the display-message output")))))))))))

(test command-client-sends-decodable-command-frame
  "run-command-client forwards a command to the server as a decodable
   +msg-command+ frame (the `cl-tmux <command>` CLI path).  A -t target rides
   along in the args for the server to parse."
  (let ((name (format nil "cmdtest-~D" (get-universal-time)))
        (cl-tmux::*socket-path-override* (%test-socket-path "cmdtest")))
    (with-test-listener (listener path (cl-tmux::socket-path name) :backlog 4)
      (cl-tmux::run-command-client name '("next-window" "-t" "2"))
      (let ((server-sock (cl-tmux/net:accept-connection listener)))
        (is-true server-sock "server accepts the command-client connection")
        (when server-sock
          (let ((ready (cl-tmux/pty:select-fds
                        (list (cl-tmux/net:socket-fd server-sock)) 1000000)))
            (is-true ready "the command frame must arrive")
            (when ready
              (multiple-value-bind (type payload)
                  (cl-tmux::read-frame (cl-tmux/net:socket-stream server-sock))
                (is (eql cl-tmux::+msg-command+ type) "a +msg-command+ frame")
                (multiple-value-bind (cmd target args)
                    (cl-tmux::decode-command-payload payload)
                  (declare (ignore target))
                  (is (eq :next-window cmd) "command decodes to :next-window")
                  (is (equal '("-t" "2") args)
                      "args carry the -t target through to the server"))))))))))

(test command-client-split-window-I-forwards-stdin-frame
  "run-command-client sends stdin after the split-window -I command frame."
  (let ((name (format nil "cmdinput-~D" (get-universal-time)))
        (cl-tmux::*socket-path-override* (%test-socket-path "cmdinput")))
    (with-test-listener (listener path (cl-tmux::socket-path name) :backlog 4)
      (with-input-from-string (*standard-input* "client stdin")
        (cl-tmux::run-command-client name '("split-window" "-I")))
      (let ((server-sock (cl-tmux/net:accept-connection listener)))
        (is-true server-sock "server accepts the command-client connection")
        (when server-sock
          (let ((ready (cl-tmux/pty:select-fds
                        (list (cl-tmux/net:socket-fd server-sock)) 1000000)))
            (is-true ready "the split-window command frame must arrive")
            (when ready
              (multiple-value-bind (type payload)
                  (cl-tmux::read-frame (cl-tmux/net:socket-stream server-sock))
                (is (eql cl-tmux::+msg-command+ type)
                    "first frame is +msg-command+")
                (multiple-value-bind (cmd target args)
                    (cl-tmux::decode-command-payload payload)
                  (declare (ignore target))
                  (is (eq :split-window cmd)
                      "command decodes to :split-window")
                  (is (equal '("-I") args)
                      "args carry the input flag")))))
          (let ((ready (cl-tmux/pty:select-fds
                        (list (cl-tmux/net:socket-fd server-sock)) 1000000)))
            (is-true ready "the stdin key frame must arrive")
            (when ready
              (multiple-value-bind (type payload)
                  (cl-tmux::read-frame (cl-tmux/net:socket-stream server-sock))
                (is (eql cl-tmux::+msg-key+ type)
                    "second frame is +msg-key+")
                (is (string= "client stdin" (cl-tmux::decode-text payload))
                    "stdin payload round-trips as UTF-8 bytes")))))))))

;;; -- exit-unattached: terminate when the last client detaches ----------------

(test exit-after-last-detach-respects-option
  "%exit-after-last-detach-p is true only when NO clients remain AND exit-unattached
   is on; default (off) keeps the session alive across detaches."
  (with-fresh-options
    (let ((cl-tmux::*clients* nil))
      (cl-tmux/options:set-option "exit-unattached" t)
      (is-true (cl-tmux::%exit-after-last-detach-p)
               "no clients + exit-unattached on -> server should exit"))
    (let ((cl-tmux::*clients* nil))
      (cl-tmux/options:set-option "exit-unattached" nil)
      (is-false (cl-tmux::%exit-after-last-detach-p)
                "no clients + exit-unattached off (default) -> keep running"))
    (let ((cl-tmux::*clients* (list (cl-tmux::%make-client-conn))))
      (cl-tmux/options:set-option "exit-unattached" t)
      (is-false (cl-tmux::%exit-after-last-detach-p)
                "clients still attached -> keep running regardless of the option"))))

(test exit-when-empty-respects-option
  "%exit-when-empty-p is true only when NO sessions remain AND exit-empty is on
   (default); off keeps the server alive with zero sessions."
  (with-fresh-options
    (let ((cl-tmux::*server-sessions* nil))
      (cl-tmux/options:set-option "exit-empty" t)
      (is-true (cl-tmux::%exit-when-empty-p)
               "no sessions + exit-empty on (default) -> server should exit"))
    (let ((cl-tmux::*server-sessions* nil))
      (cl-tmux/options:set-option "exit-empty" nil)
      (is-false (cl-tmux::%exit-when-empty-p)
                "no sessions + exit-empty off -> keep running"))
    (let ((cl-tmux::*server-sessions* (list (cons "0" (make-fake-session)))))
      (cl-tmux/options:set-option "exit-empty" t)
      (is-false (cl-tmux::%exit-when-empty-p)
                "sessions still present -> keep running regardless of the option"))))

;;; -- Integration: a broadcast frame reaches every attached client ------------

(test multi-broadcast-reaches-all-clients
  "Two clients attached to the server both receive a broadcast frame — the core
   multi-client property (one render fanned out to all)."
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
                (is-true ready "a broadcast frame must reach the client")
                (when ready
                  (multiple-value-bind (type payload)
                      (cl-tmux::read-frame (cl-tmux/net:socket-stream client))
                    (declare (ignore payload))
                    (is (eql cl-tmux::+msg-frame+ type)
                        "the client must receive a +msg-frame+ message")))))))))))
