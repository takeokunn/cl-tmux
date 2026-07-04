(in-package #:cl-tmux/test)

(in-suite server-multi-suite)

;;;; Command forwarding and client-output tests for the multi-client server.

;;; ── Forwarded commands / reply helpers ───────────────────────────────────────

(test multi-handle-forwarded-command-runs-server-side
  "A general command message (e.g. next-window) is run server-side via
   %run-command-tokens — the CLI / control command-forwarding path."
  (with-fake-session (s :nwindows 2)
    (let ((conn (%make-test-conn))
          (payload (cl-tmux/protocol::encode-command-payload :next-window)))
      (is (null (cl-tmux::%handle-multi-client-message
                 cl-tmux::+msg-command+ payload s conn))
          "a forwarded command returns NIL (keep serving)")
      (is (eq (second (cl-tmux/model:session-windows s))
              (session-active-window s))
          "the forwarded next-window must advance the active window server-side")
      (is (null (cl-tmux::client-conn-message-log conn))
          "commands without display output must not add a client message log entry"))))

(test multi-handle-forwarded-command-with-arg-runs
  "A forwarded command carrying ARGUMENTS is reconstructed (<name> args...) and run
   server-side: `select-window -t 1` selects window-id 1 — verifying the arg path
   of command forwarding, not just the bare-command path above."
  (with-fake-session (s :nwindows 2)            ; window-ids 0,1
    (let ((payload (cl-tmux/protocol::encode-command-payload
                    :select-window :args '("-t" "1"))))
      (cl-tmux::%handle-multi-client-message
       cl-tmux::+msg-command+ payload s (%make-test-conn))
      (is (= 1 (cl-tmux/model:window-id (session-active-window s)))
          "forwarded `select-window -t 1` must select window-id 1 via the args"))))

(test multi-handle-forwarded-split-window-I-routes-stdin-to-new-pane
  "Forwarded split-window -I creates an input-only pane and routes later key bytes
   from the command client to that pane instead of the active interactive client."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (let* ((conn (%make-test-conn))
           (payload (cl-tmux/protocol::encode-command-payload
                     :split-window :args '("-I"))))
      (is (null (cl-tmux::%handle-multi-client-message
                 cl-tmux::+msg-command+ payload s conn))
          "the forwarded -I split keeps the server running")
      (let ((target (cl-tmux::client-conn-stdin-target conn)))
        (is-true (cl-tmux/model::pane-p target)
                 "the connection records the pane that receives stdin")
        (when target
          (is (= -1 (pane-fd target))
              "the forwarded -I pane is input-only and has no PTY fd")
          (cl-tmux::%handle-multi-client-message
           cl-tmux::+msg-key+
           (babel:string-to-octets "forwarded stdin" :encoding :utf-8)
           s conn)
          (is (search "forwarded stdin" (row-string (pane-screen target) 0))
              "subsequent key payload bytes are fed to the -I pane"))))))

(test multi-handle-forwarded-new-session-creates-session
  "A forwarded `new-session -d` command must run in the server process and add
   the new detached session to *server-sessions*."
  (with-isolated-hooks
    (with-fake-session (s)
      (let ((cl-tmux::*server-sessions* (list (cons "0" s)))
            (created nil))
        (unwind-protect
             (let ((payload (cl-tmux/protocol::encode-command-payload
                             :new-session :args '("-d" "-s" "beta" "-n" "two"))))
               (cl-tmux::%handle-multi-client-message
                cl-tmux::+msg-command+ payload s (%make-test-conn))
               (setf created (cl-tmux::server-find-session "beta"))
               (is (not (null created))
                   "forwarded detached new-session must register beta")
               (when created
                 (is (string= "two"
                              (cl-tmux::window-name
                               (cl-tmux::session-active-window created)))
                     "forwarded -n sets the initial window name")))
          (dolist (pane (and created (cl-tmux::all-panes created)))
            (ignore-errors (cl-tmux/pty:pty-close
                            (cl-tmux::pane-fd pane)
                            (cl-tmux::pane-pid pane)))))))))

(test multi-handle-forwarded-kill-server-quits-loop
  "A forwarded kill-server command must propagate :quit to the multi-client loop."
  (with-fake-session (s)
    (let ((payload (cl-tmux/protocol::encode-command-payload :kill-server))
          (cl-tmux::*running* t))
      (is (eq :quit (cl-tmux::%handle-multi-client-message
                     cl-tmux::+msg-command+ payload s (%make-test-conn)))
          "forwarded kill-server must return :quit")
      (is-false cl-tmux::*running*
                "kill-server command implementation clears *running*"))))

(test server-split-window-input-command-p-table
  "%server-split-window-input-command-p is true only for :split-window
   carrying a flag token that contains the character I."
  (dolist (row `((:split-window ("-I")   t   "split-window -I")
                 (:splitw       ("-I")   nil "splitw alias rejected")
                 (:split-window ("-Iv")  t   "-Iv combined flag contains I")
                 (:split-window ("-v")   nil "split-window without -I")
                 (:split-window ()       nil "split-window no flags")
                 (:new-window   ("-I")   nil "different command with -I")))
    (destructuring-bind (cmd args expected description) row
      (let ((got (if (cl-tmux::%server-split-window-input-command-p cmd args) t nil)))
        (is (eq expected got)
            "~A: expected ~S got ~S" description expected got)))))

(test forwarded-command-tokens-with-target-and-args
  "%forwarded-command-tokens reconstructs <name> -t <target> args... exactly as
   the interactive command-prompt would have typed it."
  (is (equal '("select-window" "-t" "beta" "-a" "-b")
             (cl-tmux::%forwarded-command-tokens :select-window "beta" '("-a" "-b")))
      "cmd/target/args must reconstruct in name, -t target, args order"))

(test forwarded-command-tokens-without-target
  "%forwarded-command-tokens omits the -t clause entirely when TARGET is NIL."
  (is (equal '("next-window")
             (cl-tmux::%forwarded-command-tokens :next-window nil nil))
      "a NIL target must produce no -t tokens, only the command name"))

(test reply-with-command-output-noop-for-socketless-conn
  "%reply-with-command-output is a safe no-op for a CLIENT-CONN with no live
   stream (the socket-less test conn) — it must not signal."
  (let ((conn (%make-test-conn)))
    (finishes (cl-tmux::%reply-with-command-output conn))))

(test reply-with-command-output-sends-overlay-text
  "%reply-with-command-output sends the current cl-tmux/prompt:*overlay* text as
   a +msg-reply+ frame on CONN's live stream."
  (with-test-listener (listener path (%test-socket-path "reply-helper") :backlog 4)
    (let* ((client      (cl-tmux/net:connect-to path))
           (server-sock (cl-tmux/net:accept-connection listener)))
      (when server-sock
        (let ((conn (cl-tmux::%make-client-conn :socket server-sock
                                                 :stream (cl-tmux/net:socket-stream server-sock)
                                                 :fd     (cl-tmux/net:socket-fd server-sock))))
          (let ((cl-tmux/prompt:*overlay* "reply-text"))
            (cl-tmux::%reply-with-command-output conn))
          (let ((ready (cl-tmux/pty:select-fds
                        (list (cl-tmux/net:socket-fd client)) 1000000)))
            (is-true ready "a reply frame must arrive")
            (when ready
              (multiple-value-bind (type payload)
                  (cl-tmux::read-frame (cl-tmux/net:socket-stream client))
                (is (eql cl-tmux::+msg-reply+ type) "the frame must be +msg-reply+")
                (is (string= "reply-text" (cl-tmux::decode-text payload))
                    "the reply payload must carry the overlay text")))))))))

(test multi-drop-client-removes-from-registry
  "%drop-client (no bye, no socket) removes the conn from *clients*."
  (with-isolated-hooks
    (let* ((a (%make-test-conn))
           (b (%make-test-conn))
           (cl-tmux::*clients* (list a b)))
      (cl-tmux::%drop-client a)
      (is (equal (list b) cl-tmux::*clients*) "dropped conn is removed; the other remains")
      ;; Idempotent: dropping again is a no-op.
      (cl-tmux::%drop-client a)
      (is (equal (list b) cl-tmux::*clients*) "double-drop is a safe no-op"))))
