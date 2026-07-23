(in-package #:cl-tmux/test)

;;;; Command forwarding and client-output tests for the multi-client server.

(describe "server-multi-suite"

  ;;; ── Forwarded commands / reply helpers ───────────────────────────────────────

  ;; A general command message (e.g. next-window) is run server-side via
  ;; %run-command-tokens — the CLI / control command-forwarding path.
  (it "multi-handle-forwarded-command-runs-server-side"
    (with-fake-session (s :nwindows 2)
      (let ((conn (%make-test-conn))
            (payload (cl-tmux/protocol::encode-command-payload :next-window)))
        (expect (null (cl-tmux::%handle-multi-client-message
                       cl-tmux::+msg-command+ payload s conn)))
        (expect (eq (second (cl-tmux/model:session-windows s))
                    (session-active-window s)))
        (expect (null (cl-tmux::client-conn-message-log conn))))))

  ;; A forwarded command carrying ARGUMENTS is reconstructed (<name> args...) and run
  ;; server-side: `select-window -t 1` selects window-id 1 — verifying the arg path
  ;; of command forwarding, not just the bare-command path above.
  (it "multi-handle-forwarded-command-with-arg-runs"
    (with-fake-session (s :nwindows 2)            ; window-ids 0,1
      (let ((payload (cl-tmux/protocol::encode-command-payload
                      :select-window :args '("-t" "1"))))
        (cl-tmux::%handle-multi-client-message
         cl-tmux::+msg-command+ payload s (%make-test-conn))
        (expect (= 1 (cl-tmux/model:window-id (session-active-window s)))))))

  ;; Forwarded split-window -I creates an input-only pane and routes later key bytes
  ;; from the command client to that pane instead of the active interactive client.
  (it "multi-handle-forwarded-split-window-I-routes-stdin-to-new-pane"
    (with-fake-session (s :nwindows 1 :npanes 1)
      (let* ((conn (%make-test-conn))
             (payload (cl-tmux/protocol::encode-command-payload
                       :split-window :args '("-I"))))
        (expect (null (cl-tmux::%handle-multi-client-message
                       cl-tmux::+msg-command+ payload s conn)))
        (let ((target (cl-tmux::client-conn-stdin-target conn)))
          (expect (cl-tmux/model::pane-p target) :to-be-truthy)
          (when target
            (expect (= -1 (pane-fd target)))
            (cl-tmux::%handle-multi-client-message
             cl-tmux::+msg-key+
             (babel:string-to-octets "forwarded stdin" :encoding :utf-8)
             s conn)
            (expect (search "forwarded stdin" (row-string (pane-screen target) 0))))))))

  ;; A forwarded `new-session -d` command must run in the server process and add
  ;; the new detached session to *server-sessions*.
  (it "multi-handle-forwarded-new-session-creates-session"
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
                 (expect (not (null created)))
                 (when created
                   (expect (string= "two"
                                    (cl-tmux::window-name
                                     (cl-tmux::session-active-window created))))))
            (dolist (pane (and created (cl-tmux::all-panes created)))
              (ignore-errors (cl-tmux/pty:pty-close
                              (cl-tmux::pane-fd pane)
                              (cl-tmux::pane-pid pane)))))))))

  ;; A forwarded kill-server command must propagate :quit to the multi-client loop.
  (it "multi-handle-forwarded-kill-server-quits-loop"
    (with-fake-session (s)
      (let ((payload (cl-tmux/protocol::encode-command-payload :kill-server))
            (cl-tmux::*running* t))
        (expect (eq :quit (cl-tmux::%handle-multi-client-message
                           cl-tmux::+msg-command+ payload s (%make-test-conn))))
        (expect cl-tmux::*running* :to-be-falsy))))

  ;; %server-split-window-input-command-p is true only for :split-window
  ;; carrying a flag token that contains the character I.
  (it "server-split-window-input-command-p-table"
    (dolist (row `((:split-window ("-I")   t   "split-window -I")
                   (:splitw       ("-I")   nil "splitw alias rejected")
                   (:split-window ("-Iv")  t   "-Iv combined flag contains I")
                   (:split-window ("-v")   nil "split-window without -I")
                   (:split-window ()       nil "split-window no flags")
                   (:new-window   ("-I")   nil "different command with -I")))
      (destructuring-bind (cmd args expected description) row
        (declare (ignore description))
        (let ((got (if (cl-tmux::%server-split-window-input-command-p cmd args) t nil)))
          (expect (eq expected got))))))

  ;; %forwarded-command-tokens reconstructs <name> -t <target> args... exactly as
  ;; the interactive command-prompt would have typed it.
  (it "forwarded-command-tokens-with-target-and-args"
    (expect (equal '("select-window" "-t" "beta" "-a" "-b")
                   (cl-tmux::%forwarded-command-tokens :select-window "beta" '("-a" "-b")))))

  ;; %forwarded-command-tokens omits the -t clause entirely when TARGET is NIL.
  (it "forwarded-command-tokens-without-target"
    (expect (equal '("next-window")
                   (cl-tmux::%forwarded-command-tokens :next-window nil nil))))

  ;; %reply-with-command-output is a safe no-op for a CLIENT-CONN with no live
  ;; stream (the socket-less test conn) — it must not signal.
  (it "reply-with-command-output-noop-for-socketless-conn"
    (let ((conn (%make-test-conn)))
      (finishes (cl-tmux::%reply-with-command-output conn))))

  ;; %reply-with-command-output sends the current cl-tmux/prompt:*overlay* text as
  ;; a +msg-reply+ frame on CONN's live stream.
  (it "reply-with-command-output-sends-overlay-text"
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
              (expect (not (null ready)))
              (when ready
                (multiple-value-bind (type payload)
                    (cl-tmux::read-frame (cl-tmux/net:socket-stream client))
                  (expect (eql cl-tmux::+msg-reply+ type))
                  (expect (string= "reply-text" (cl-tmux::decode-text payload)))))))))))

  ;; %drop-client (no bye, no socket) removes the conn from *clients*.
  (it "multi-drop-client-removes-from-registry"
    (with-isolated-hooks
      (let* ((a (%make-test-conn))
             (b (%make-test-conn))
             (cl-tmux::*clients* (list a b)))
        (cl-tmux::%drop-client a)
        (expect (equal (list b) cl-tmux::*clients*))
        ;; Idempotent: dropping again is a no-op.
        (cl-tmux::%drop-client a)
        (expect (equal (list b) cl-tmux::*clients*))))))
