(in-package #:cl-tmux/test)

;;;; Multi-client server tests (src/server-multi.lisp).
;;;;
;;;; The select-multiplexed serve loop is integration-level, but its building
;;;; blocks are pure/observable without a live socket: the smallest-client size
;;;; policy and the per-client message dispatch.  One real-socket integration
;;;; test (gated on unix-socket-available-p) proves two clients both receive a
;;;; broadcast frame.

(def-suite server-multi-suite :description "Multi-client select-multiplexed server")
(in-suite server-multi-suite)

;;; ── %effective-client-size: smallest attached client ─────────────────────────

(test multi-effective-size-is-smallest-client
  "The session renders at the SMALLEST attached client's geometry so every client
   can display the shared broadcast frame."
  (let ((cl-tmux::*clients*
          (list (cl-tmux::%make-client-conn :rows 50 :cols 200)
                (cl-tmux::%make-client-conn :rows 24 :cols 80)
                (cl-tmux::%make-client-conn :rows 40 :cols 120))))
    (multiple-value-bind (rows cols) (cl-tmux::%effective-client-size)
      (is (= 24 rows) "effective rows = smallest client rows")
      (is (= 80 cols) "effective cols = smallest client cols"))))

(test multi-effective-size-no-clients-falls-back
  "With no clients attached, %effective-client-size falls back to *term-rows*/cols."
  (let ((cl-tmux::*clients* nil)
        (cl-tmux::*term-rows* 30)
        (cl-tmux::*term-cols* 100))
    (multiple-value-bind (rows cols) (cl-tmux::%effective-client-size)
      (is (= 30 rows)) (is (= 100 cols)))))

(test multi-effective-size-largest-mode
  "window-size \"largest\" sizes to the biggest attached client."
  (with-fresh-options
    (cl-tmux/options:set-option "window-size" "largest")
    (let ((cl-tmux::*clients*
            (list (cl-tmux::%make-client-conn :rows 50 :cols 200)
                  (cl-tmux::%make-client-conn :rows 24 :cols 80))))
      (multiple-value-bind (rows cols) (cl-tmux::%effective-client-size)
        (is (= 50 rows) "largest rows") (is (= 200 cols) "largest cols")))))

(test multi-effective-size-latest-mode
  "window-size \"latest\" sizes to the most-recent client (front of *clients*)."
  (with-fresh-options
    (cl-tmux/options:set-option "window-size" "latest")
    (let ((cl-tmux::*clients*
            (list (cl-tmux::%make-client-conn :rows 40 :cols 120)   ; most recent
                  (cl-tmux::%make-client-conn :rows 24 :cols 80))))
      (multiple-value-bind (rows cols) (cl-tmux::%effective-client-size)
        (is (= 40 rows) "latest rows") (is (= 120 cols) "latest cols")))))

(test multi-effective-size-manual-mode-keeps-current
  "window-size \"manual\" ignores client sizes and keeps *term-rows*/cols."
  (with-fresh-options
    (cl-tmux/options:set-option "window-size" "manual")
    (let ((cl-tmux::*clients* (list (cl-tmux::%make-client-conn :rows 99 :cols 99)))
          (cl-tmux::*term-rows* 30)
          (cl-tmux::*term-cols* 100))
      (multiple-value-bind (rows cols) (cl-tmux::%effective-client-size)
        (is (= 30 rows) "manual keeps current rows")
        (is (= 100 cols) "manual keeps current cols")))))

;;; ── %handle-multi-client-message: per-client dispatch ────────────────────────

(defun %make-test-conn (&key (rows 24) (cols 80))
  "A socket-less CLIENT-CONN for dispatch tests (paths that never touch the socket)."
  (cl-tmux::%make-client-conn :state (cl-tmux::make-input-state)
                              :rows rows :cols cols))

(test multi-handle-resize-updates-conn-and-effective-size
  "A resize message updates the client's geometry and re-applies the effective size."
  (with-loop-state
    (let* ((s    (make-fake-session))
           (conn (%make-test-conn :rows 24 :cols 80))
           (cl-tmux::*clients* (list conn))
           (payload (cl-tmux/protocol::u16-octets-pair 40 100)))
      (cl-tmux::%handle-multi-client-message cl-tmux::+msg-resize+ payload s conn)
      (is (= 40 (cl-tmux::client-conn-rows conn)) "conn rows updated from the resize")
      (is (= 100 (cl-tmux::client-conn-cols conn)) "conn cols updated from the resize")
      ;; Single client → effective size equals that client's size.
      (is (= 40 cl-tmux::*term-rows*) "effective rows applied to *term-rows*")
      (is (= 100 cl-tmux::*term-cols*) "effective cols applied to *term-cols*"))))

(test multi-resize-marks-client-latest
  "A resize moves the client to the front of *clients* so window-size \"latest\"
   tracks the just-resized client."
  (with-fresh-options
    (cl-tmux/options:set-option "window-size" "latest")
    (with-loop-state
      (let* ((s (make-fake-session))
             (a (%make-test-conn :rows 24 :cols 80))
             (b (%make-test-conn :rows 30 :cols 100))
             (cl-tmux::*clients* (list a b))   ; a is front initially
             (payload (cl-tmux/protocol::u16-octets-pair 50 150)))
        (cl-tmux::%handle-multi-client-message cl-tmux::+msg-resize+ payload s b)
        (is (eq b (first cl-tmux::*clients*)) "the resized client moves to the front")
        (multiple-value-bind (rows cols) (cl-tmux::%effective-client-size)
          (is (= 50 rows) "latest tracks the just-resized client's new rows")
          (is (= 150 cols) "latest tracks the just-resized client's new cols"))))))

(test multi-handle-key-detach-drops-client
  "A ^B d key message yields :drop (the client detaches; the session survives)."
  (let ((s (make-fake-session)))
    (with-isolated-config
      (with-loop-state
        (let ((conn    (%make-test-conn))
              (payload (make-array 2 :element-type '(unsigned-byte 8)
                                     :initial-contents (list 2 (char-code #\d)))))
          (is (eq :drop (cl-tmux::%handle-multi-client-message
                         cl-tmux::+msg-key+ payload s conn))
              "^B d must produce :drop")
          (is-true cl-tmux::*running* "a detach must not end the session"))))))

(test multi-handle-detach-message-drops-client
  "An explicit +msg-detach+ message yields :drop."
  (let ((s (make-fake-session)))
    (with-loop-state
      (is (eq :drop (cl-tmux::%handle-multi-client-message
                     cl-tmux::+msg-detach+ #() s (%make-test-conn)))))))

(test multi-handle-nil-and-unknown-type-drop
  "EOF (NIL type) and an unknown message type both yield :drop."
  (let ((s (make-fake-session)))
    (with-loop-state
      (is (eq :drop (cl-tmux::%handle-multi-client-message nil #() s (%make-test-conn)))
          "NIL type (EOF) → :drop")
      (is (eq :drop (cl-tmux::%handle-multi-client-message 99 #() s (%make-test-conn)))
          "unknown type → :drop"))))

(test multi-handle-detach-other-clients-command
  "A detach-other-clients command message yields :detach-others."
  (let ((s (make-fake-session)))
    (with-loop-state
      (let ((payload (cl-tmux/protocol::encode-command-payload :detach-other-clients)))
        (is (eq :detach-others (cl-tmux::%handle-multi-client-message
                                cl-tmux::+msg-command+ payload s (%make-test-conn)))
            "detach-other-clients command → :detach-others")))))

(test multi-handle-forwarded-command-runs-server-side
  "A general command message (e.g. next-window) is run server-side via
   %run-command-tokens — the CLI / control command-forwarding path."
  (let ((s (make-fake-session :nwindows 2)))
    (with-loop-state
      (let ((payload (cl-tmux/protocol::encode-command-payload :next-window)))
        (is (null (cl-tmux::%handle-multi-client-message
                   cl-tmux::+msg-command+ payload s (%make-test-conn)))
            "a forwarded command returns NIL (keep serving)")
        (is (eq (second (cl-tmux/model:session-windows s))
                (session-active-window s))
            "the forwarded next-window must advance the active window server-side")))))

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

;;; ── Command client: forwards a command to the server ────────────────────────

(test command-client-sends-decodable-command-frame
  "run-command-client forwards a command to the server as a decodable
   +msg-command+ frame (the `cl-tmux <command>` CLI path).  A -t target rides
   along in the args for the server to parse."
  (when (cl-tmux/net:unix-socket-available-p)
    (let* ((name     (format nil "cmdtest-~D" (get-universal-time)))
           (path     (cl-tmux::socket-path name))
           (listener (cl-tmux/net:make-listener path :backlog 4)))
      (unwind-protect
           (progn
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
                             "args carry the -t target through to the server"))))))))
        (cl-tmux/net:close-socket listener)
        (ignore-errors (delete-file path))))))

;;; ── Integration: a broadcast frame reaches every attached client ─────────────

(test multi-broadcast-reaches-all-clients
  "Two clients attached to the server both receive a broadcast frame — the core
   multi-client property (one render fanned out to all)."
  (when (cl-tmux/net:unix-socket-available-p)
    (with-isolated-hooks
      (with-loop-state
        (let* ((s    (make-fake-session))
               (path (format nil "~A/cl-tmux-mtest-~D.sock"
                             (string-right-trim "/" (or (sb-ext:posix-getenv "TMPDIR") "/tmp"))
                             (get-universal-time)))
               (listener (cl-tmux/net:make-listener path :backlog 4)))
          (unwind-protect
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
                               "the client must receive a +msg-frame+ message")))))))
            (cl-tmux/net:close-socket listener)
            (ignore-errors (delete-file path))))))))
