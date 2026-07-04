(in-package #:cl-tmux/test)

;;;; Multi-client server tests (src/server-multi.lisp).
;;;;
;;;; The select-multiplexed serve loop is integration-level, but its building
;;;; blocks are pure/observable without a live socket: the smallest-client size
;;;; policy, per-client message dispatch, readiness handling, and socket
;;;; registration.

(def-suite server-multi-suite :description "Multi-client select-multiplexed server")
(in-suite server-multi-suite)

;;; ── %client-fds / %client-size-reduce: pure registry helpers ─────────────────

(test client-fds-returns-fd-of-every-attached-client
  "%client-fds returns the socket fd of every entry in *clients*, in order."
  (let ((cl-tmux::*clients*
          (list (cl-tmux::%make-client-conn :fd 11)
                (cl-tmux::%make-client-conn :fd 22)
                (cl-tmux::%make-client-conn :fd 33))))
    (is (equal '(11 22 33) (cl-tmux::%client-fds))
        "%client-fds must list the fds in *clients* order")))

(test client-fds-empty-when-no-clients
  "%client-fds returns NIL when no clients are attached."
  (let ((cl-tmux::*clients* nil))
    (is (null (cl-tmux::%client-fds))
        "%client-fds on an empty registry must return NIL")))

(test client-size-reduce-applies-fn-across-rows-and-cols
  "%client-size-reduce applies the given reducing FN independently across every
   attached client's rows and cols."
  (let ((cl-tmux::*clients*
          (list (cl-tmux::%make-client-conn :rows 50 :cols 80)
                (cl-tmux::%make-client-conn :rows 24 :cols 200)
                (cl-tmux::%make-client-conn :rows 40 :cols 120))))
    (multiple-value-bind (min-rows min-cols) (cl-tmux::%client-size-reduce #'min)
      (check-table (list (list min-rows 24  "min reduce → smallest rows")
                         (list min-cols 80  "min reduce → smallest cols"))))
    (multiple-value-bind (max-rows max-cols) (cl-tmux::%client-size-reduce #'max)
      (check-table (list (list max-rows 50  "max reduce → largest rows")
                         (list max-cols 200 "max reduce → largest cols"))))))

;;; ── %effective-client-size: smallest attached client ─────────────────────────

(test multi-effective-size-is-smallest-client
  "The session renders at the SMALLEST attached client's geometry so every client
   can display the shared broadcast frame."
  (let ((cl-tmux::*clients*
          (list (cl-tmux::%make-client-conn :rows 50 :cols 200)
                (cl-tmux::%make-client-conn :rows 24 :cols 80)
                (cl-tmux::%make-client-conn :rows 40 :cols 120))))
    (multiple-value-bind (rows cols) (cl-tmux::%effective-client-size)
       (check-table (list (list rows 24 "effective rows = smallest client rows")
                          (list cols 80 "effective cols = smallest client cols"))))))

(test multi-effective-size-no-clients-falls-back
  "With no clients attached, %effective-client-size falls back to *term-rows*/cols."
  (let ((cl-tmux::*clients* nil)
        (cl-tmux::*term-rows* 30)
        (cl-tmux::*term-cols* 100))
    (multiple-value-bind (rows cols) (cl-tmux::%effective-client-size)
       (check-table (list (list rows 30 "no clients → rows fallback to *term-rows*")
                          (list cols 100 "no clients → cols fallback to *term-cols*"))))))

(test multi-effective-size-largest-mode
  "window-size \"largest\" sizes to the biggest attached client."
  (with-fresh-options
    (cl-tmux/options:set-option "window-size" "largest")
    (let ((cl-tmux::*clients*
            (list (cl-tmux::%make-client-conn :rows 50 :cols 200)
                  (cl-tmux::%make-client-conn :rows 24 :cols 80))))
      (multiple-value-bind (rows cols) (cl-tmux::%effective-client-size)
        (check-table (list (list rows 50 "largest rows")
                           (list cols 200 "largest cols")))))))

(test multi-effective-size-latest-mode
  "window-size \"latest\" sizes to the most-recent client (front of *clients*)."
  (with-fresh-options
    (cl-tmux/options:set-option "window-size" "latest")
    (let ((cl-tmux::*clients*
            (list (cl-tmux::%make-client-conn :rows 40 :cols 120)   ; most recent
                  (cl-tmux::%make-client-conn :rows 24 :cols 80))))
      (multiple-value-bind (rows cols) (cl-tmux::%effective-client-size)
        (check-table (list (list rows 40 "latest rows")
                           (list cols 120 "latest cols")))))))

(test multi-effective-size-manual-mode-keeps-current
  "window-size \"manual\" ignores client sizes and keeps *term-rows*/cols."
  (with-fresh-options
    (cl-tmux/options:set-option "window-size" "manual")
    (let ((cl-tmux::*clients* (list (cl-tmux::%make-client-conn :rows 99 :cols 99)))
          (cl-tmux::*term-rows* 30)
          (cl-tmux::*term-cols* 100))
      (multiple-value-bind (rows cols) (cl-tmux::%effective-client-size)
        (check-table (list (list rows 30 "manual keeps current rows")
                           (list cols 100 "manual keeps current cols")))))))

;;; ── %handle-multi-client-message: per-client dispatch ────────────────────────

(defun %make-test-conn (&key (rows 24) (cols 80))
  "A socket-less CLIENT-CONN for dispatch tests (paths that never touch the socket)."
  (cl-tmux::%make-client-conn :state (cl-tmux::make-input-state)
                              :rows rows :cols cols))

(test multi-handle-resize-updates-conn-and-effective-size
  "A resize message updates the client's geometry and re-applies the effective size."
  (with-fake-session (s)
    (let* ((conn (%make-test-conn :rows 24 :cols 80))
           (cl-tmux::*clients* (list conn))
           (payload (cl-tmux/protocol::u16-octets-pair 40 100)))
      (cl-tmux::%handle-multi-client-message cl-tmux::+msg-resize+ payload s conn)
      ;; Single client → effective size equals that client's size.
      (check-table (list (list (cl-tmux::client-conn-rows conn) 40 "conn rows updated from the resize")
                         (list (cl-tmux::client-conn-cols conn) 100 "conn cols updated from the resize")
                         (list cl-tmux::*term-rows* 40 "effective rows applied to *term-rows*")
                         (list cl-tmux::*term-cols* 100 "effective cols applied to *term-cols*"))))))

(test multi-resize-marks-client-latest
  "A resize moves the client to the front of *clients* so window-size \"latest\"
   tracks the just-resized client."
  (with-fresh-options
    (cl-tmux/options:set-option "window-size" "latest")
    (with-fake-session (s)
      (let* ((a (%make-test-conn :rows 24 :cols 80))
             (b (%make-test-conn :rows 30 :cols 100))
             (cl-tmux::*clients* (list a b))   ; a is front initially
             (payload (cl-tmux/protocol::u16-octets-pair 50 150)))
        (cl-tmux::%handle-multi-client-message cl-tmux::+msg-resize+ payload s b)
        (is (eq b (first cl-tmux::*clients*)) "the resized client moves to the front")
        (multiple-value-bind (rows cols) (cl-tmux::%effective-client-size)
          (check-table (list (list rows 50 "latest tracks the just-resized client's new rows")
                             (list cols 150 "latest tracks the just-resized client's new cols"))))))))

(test multi-handle-key-detach-drops-client
  "A ^B d key message yields :drop (the client detaches; the session survives)."
  (with-fake-session (s)
    (with-isolated-config
      (let ((conn    (%make-test-conn))
            (payload (make-array 2 :element-type '(unsigned-byte 8)
                                   :initial-contents (list 2 (char-code #\d)))))
        (is (eq :drop (cl-tmux::%handle-multi-client-message
                       cl-tmux::+msg-key+ payload s conn))
            "^B d must produce :drop")
        (is-true cl-tmux::*running* "a detach must not end the session")))))

(test multi-attach-readonly-flag-sets-conn-slot
  "A +msg-attach+ frame whose flags byte sets +attach-flag-read-only+ marks the
   connection read-only; a plain (no-flag) attach leaves it NIL."
  (with-fake-session (s)
    (let* ((conn   (%make-test-conn))
           (cl-tmux::*clients* (list conn))
           (ro-payload (cl-tmux/protocol::to-octets
                        (concatenate 'list
                                     (cl-tmux/protocol::u16-octets-pair 30 100)
                                     (list cl-tmux/protocol:+attach-flag-read-only+)))))
      (cl-tmux::%handle-multi-client-message cl-tmux::+msg-attach+ ro-payload s conn)
      (is-true (cl-tmux::client-conn-read-only-p conn)
               "attach flags byte with the read-only bit must set conn read-only-p")
      ;; A subsequent plain attach (no flags byte) clears it again.
      (cl-tmux::%handle-multi-client-message
       cl-tmux::+msg-attach+ (cl-tmux/protocol::u16-octets-pair 30 100) s conn)
      (is-false (cl-tmux::client-conn-read-only-p conn)
                "a plain attach (no flags byte) must clear conn read-only-p"))))

(test multi-readonly-conn-suppresses-pane-input
  "When a connection is read-only, a printable key dispatched through
   %handle-multi-client-message must NOT reach the active pane (no pty-write)."
  (with-fake-session (s)
    (with-isolated-config
      (let* ((conn (%make-test-conn))
             (cl-tmux::*clients* (list conn))
             (writes nil))
        (setf (cl-tmux::client-conn-read-only-p conn) t)
        ;; Capture any pty-write the key would otherwise forward to the pane.
        (flet ((rec (fd bytes) (declare (ignore fd)) (push bytes writes)))
          (let ((orig (fdefinition 'cl-tmux::pty-write)))
            (unwind-protect
                 (progn
                   (setf (fdefinition 'cl-tmux::pty-write) #'rec)
                   (cl-tmux::%handle-multi-client-message
                    cl-tmux::+msg-key+
                    (make-array 1 :element-type '(unsigned-byte 8)
                                  :initial-contents (list (char-code #\a)))
                    s conn))
              (setf (fdefinition 'cl-tmux::pty-write) orig))))
        (is (null writes)
            "a read-only connection must not pty-write a printable key to the pane")))))

(test multi-handle-detach-message-drops-client
  "An explicit +msg-detach+ message yields :drop."
  (with-fake-session (s)
    (is (eq :drop (cl-tmux::%handle-multi-client-message
                   cl-tmux::+msg-detach+ #() s (%make-test-conn))))))

(test multi-handle-nil-and-unknown-type-drop
  "EOF (NIL type) and an unknown message type both yield :drop."
  (with-fake-session (s)
    (is (eq :drop (cl-tmux::%handle-multi-client-message nil #() s (%make-test-conn)))
        "NIL type (EOF) → :drop")
    (is (eq :drop (cl-tmux::%handle-multi-client-message 99 #() s (%make-test-conn)))
        "unknown type → :drop")))

(test multi-handle-detach-other-clients-command
  "A detach-other-clients command message yields :detach-others."
  (with-fake-session (s)
    (let ((payload (cl-tmux/protocol::encode-command-payload :detach-other-clients)))
      (is (eq :detach-others (cl-tmux::%handle-multi-client-message
                              cl-tmux::+msg-command+ payload s (%make-test-conn)))
          "detach-other-clients command → :detach-others"))))

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

;;; ── %server-split-window-input-command-p / %forwarded-command-tokens ─────────
;;;
;;; Pure helpers behind %dispatch-forwarded-command — table-driven since each
;;; case differs only in input/expected output, no live socket required.

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

;;; ── %reply-with-command-output: no-op without a live socket ──────────────────

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

;;; ── %drop-client: registry removal ───────────────────────────────────────────

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

;;; ── %accept-pending-connection: listener-fd accept helper ───────────────────

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

;;; ── %dispatch-ready-clients: per-iteration client dispatch ───────────────────

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
