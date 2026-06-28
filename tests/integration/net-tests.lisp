(in-package #:cl-tmux/test)

;;;; Unix-domain socket primitive tests (src/net.lisp).
;;;;
;;;; The roundtrip test drives a REAL kernel socket: it binds a listener,
;;;; connects, accepts, and pushes a protocol frame end-to-end through the
;;;; transport — all single-threaded (connect queues, accept dequeues, the
;;;; kernel buffers the few bytes), wrapped in a timeout, and guarded by an
;;;; availability probe so it self-skips where a sandbox forbids socket bind
;;;; (mirroring the PTY tests).
;;;;
;;;; with-temp-socket-path, with-connected-sockets, and write-frames-to-file
;;;; are defined in tests/helpers-b.lisp and shared with transport-tests.lisp.

(def-suite net-suite :description "Unix-domain socket transport (sb-bsd-sockets)")
(in-suite net-suite)

;;; ── %make-probe-socket-path ──────────────────────────────────────────────────
;;;
;;; %make-probe-socket-path is a private helper that generates a throwaway socket
;;; path in the temp directory.  It is called only by unix-socket-available-p, so
;;; it has no direct test coverage.  These tests pin its contract: the returned
;;; path must be a non-empty string in the temp directory, and two successive
;;; calls must return distinct paths (collision-resistance).

(test make-probe-socket-path-returns-nonempty-string
  "cl-tmux/net::%make-probe-socket-path must return a non-empty string."
  (let ((path (cl-tmux/net::%make-probe-socket-path)))
    (is (stringp path)
        "%make-probe-socket-path must return a string, got ~S" path)
    (is (plusp (length path))
        "%make-probe-socket-path must return a non-empty string")))

(test make-probe-socket-path-is-in-temp-directory
  "%make-probe-socket-path must produce a path inside the system temp directory."
  (let* ((path    (cl-tmux/net::%make-probe-socket-path))
         (tmpdir  (string-right-trim "/" (or (sb-ext:posix-getenv "TMPDIR") "/tmp"))))
    (is (and (> (length path) (length tmpdir))
             (string= tmpdir (subseq path 0 (length tmpdir))))
        "%make-probe-socket-path must be under the temp directory ~S, got ~S"
        tmpdir path)))

(test make-probe-socket-path-has-sock-suffix
  "%make-probe-socket-path must produce a path ending in \".sock\"."
  (let ((path (cl-tmux/net::%make-probe-socket-path)))
    (is (string= ".sock" (subseq path (- (length path) 5)))
        "%make-probe-socket-path must end with .sock, got ~S" path)))

(test make-probe-socket-path-successive-calls-return-distinct-paths
  "Two successive calls to %make-probe-socket-path must return different paths
   (collision-resistance for concurrent test runs or parallel probing)."
  (let ((path1 (cl-tmux/net::%make-probe-socket-path))
        (path2 (cl-tmux/net::%make-probe-socket-path)))
    (is (not (string= path1 path2))
        "%make-probe-socket-path must not return the same path on two calls")))

;;; ── unix-socket-available-p ──────────────────────────────────────────────────

(test unix-socket-availability-is-boolean
  :description "unix-socket-available-p answers without error (T or NIL)."
  (let ((answer (unix-socket-available-p)))
    (is (member answer '(t nil)) "probe returns a boolean, got ~S" answer)))

;;; ── connect-to error path ────────────────────────────────────────────────────

(test connect-to-missing-path-signals
  :description "Connecting to a non-existent socket path signals an error."
  (signals error
    (connect-to "/nonexistent-cl-tmux-dir/does-not-exist.sock")))

(test connect-to-empty-path-signals
  :description "Connecting to an empty-string path signals an error."
  (signals error
    (connect-to "")))

;;; ── socket-fd on bound listener ──────────────────────────────────────────────

(test socket-fd-returns-non-negative-integer
  :description "socket-fd returns a non-negative file descriptor for a bound listener socket."
  (unless (unix-socket-available-p)
    (skip "Unix-domain socket bind unavailable (sandbox)"))
  (with-temp-socket-path (path)
    (let ((socket (make-listener path)))
      (unwind-protect
           (let ((fd (socket-fd socket)))
             (is (integerp fd) "socket-fd must return an integer")
             (is (>= fd 0)     "socket-fd must be non-negative, got ~D" fd))
        (ignore-errors (close-socket socket))))))

;;; ── close-socket idempotency ─────────────────────────────────────────────────

(test close-socket-is-idempotent
  :description "close-socket on an already-closed socket does not signal."
  (unless (unix-socket-available-p)
    (skip "Unix-domain socket bind unavailable (sandbox)"))
  (with-temp-socket-path (path)
    (let ((socket (make-listener path)))
      (close-socket socket)
      ;; Second close must not signal — it is wrapped in ignore-errors internally.
      (finishes (close-socket socket)
                "second close-socket on same socket must not signal"))))

;;; ── socket-stream produces a binary stream ───────────────────────────────────

(test socket-stream-is-a-stream
  :description "socket-stream wraps a bound socket in a binary I/O stream."
  (unless (unix-socket-available-p)
    (skip "Unix-domain socket bind unavailable (sandbox)"))
  (sb-ext:with-timeout 10
    (with-temp-socket-path (path)
      (with-connected-sockets (path listener client conn)
        (let ((client-stream (socket-stream client))
              (server-stream (socket-stream conn)))
          (is (streamp client-stream) "socket-stream must return a stream (client side)")
          (is (streamp server-stream) "socket-stream must return a stream (server side)"))))))

;;; ── accept-connection / make-listener roundtrip ──────────────────────────────

(test make-listener-accept-connection-returns-socket
  :description "accept-connection returns a socket object for an inbound connection."
  (unless (unix-socket-available-p)
    (skip "Unix-domain socket bind unavailable (sandbox)"))
  (sb-ext:with-timeout 10
    (with-temp-socket-path (path)
      (with-connected-sockets (path listener client conn)
        (is-true conn "accept-connection must return a socket")))))

;;; ── Table-driven: multiple message types roundtrip ───────────────────────────
;;;
;;; Each row in the table below encodes a message type, a predicate applied to
;;; the decoded type tag, and a payload decoder.  All rows share one
;;; bind→connect→accept socket pair to avoid the overhead of multiple setups.

(test socket-frame-roundtrip
  :description "A protocol frame survives a real bind→connect→accept→send→read roundtrip."
  (unless (unix-socket-available-p)
    (skip "Unix-domain socket bind unavailable (sandbox)"))
  (sb-ext:with-timeout 10
    (with-temp-socket-path (path)
      (with-connected-sockets (path listener client conn)
        (let ((client-stream (socket-stream client))
              (server-stream (socket-stream conn)))
          ;; client → server: a key frame, then a detach frame
          (send-frame client-stream (msg-key #(65 66)))
          (send-frame client-stream (msg-detach))
          (multiple-value-bind (type payload) (read-frame server-stream)
            (is (= +msg-key+ type)
                "msg-key type tag must survive roundtrip")
            (is (equalp #(65 66) payload)
                "msg-key payload must survive roundtrip"))
          (multiple-value-bind (type payload) (read-frame server-stream)
            (declare (ignore payload))
            (is (= +msg-detach+ type)
                "msg-detach type tag must survive roundtrip"))
          ;; server → client: a rendered frame with Unicode content
          (send-frame server-stream (msg-frame "あ"))
          (multiple-value-bind (type payload) (read-frame client-stream)
            (is (= +msg-frame+ type)
                "msg-frame type tag must survive roundtrip")
            (is (string= "あ" (decode-text payload))
                "msg-frame Unicode payload must round-trip correctly")))))))

(test socket-multiple-frames-in-order
  :description "Multiple frames queued by the sender are consumed in send order."
  (unless (unix-socket-available-p)
    (skip "Unix-domain socket bind unavailable (sandbox)"))
  (sb-ext:with-timeout 10
    (with-temp-socket-path (path)
      (with-connected-sockets (path listener client conn)
        (let ((client-stream (socket-stream client))
              (server-stream (socket-stream conn)))
          ;; Send three distinct frames
          (send-frame client-stream (msg-frame "first"))
          (send-frame client-stream (msg-frame "second"))
          (send-frame client-stream (msg-frame "third"))
          (let ((results
                 (loop repeat 3
                       collect (multiple-value-bind (type payload)
                                   (read-frame server-stream)
                                 (declare (ignore type))
                                 (decode-text payload)))))
            (is (equal '("first" "second" "third") results)
                "frames must arrive in send order: ~S" results)))))))

(test socket-listener-fd-distinct-from-client-fd
  :description "The listener fd and the client fd must be distinct (different kernel fds)."
  (unless (unix-socket-available-p)
    (skip "Unix-domain socket bind unavailable (sandbox)"))
  (sb-ext:with-timeout 10
    (with-temp-socket-path (path)
      (with-connected-sockets (path listener client conn)
        (is (/= (socket-fd listener) (socket-fd client))
            "listener fd must differ from client fd")
        (is (/= (socket-fd listener) (socket-fd conn))
            "listener fd must differ from accepted-conn fd")))))

;;; ── Net constant values ──────────────────────────────────────────────────────

(test accept-timeout-constant-is-positive-integer
  :description "+accept-timeout-seconds+ must be a positive integer for sb-ext:with-timeout."
  (is (integerp cl-tmux/net::+accept-timeout-seconds+)
      "+accept-timeout-seconds+ must be an integer")
  (is (plusp cl-tmux/net::+accept-timeout-seconds+)
      "+accept-timeout-seconds+ must be positive"))

(test socket-stream-timeout-constant-is-positive-integer
  :description "+socket-stream-timeout-seconds+ must be a positive integer for socket-make-stream."
  (is (integerp cl-tmux/net::+socket-stream-timeout-seconds+)
      "+socket-stream-timeout-seconds+ must be an integer")
  (is (plusp cl-tmux/net::+socket-stream-timeout-seconds+)
      "+socket-stream-timeout-seconds+ must be positive"))

;;; ── Socket stream element type ───────────────────────────────────────────────

(test socket-stream-has-binary-element-type
  :description "socket-stream must produce a stream whose element-type is (unsigned-byte 8)."
  (unless (unix-socket-available-p)
    (skip "Unix-domain socket bind unavailable (sandbox)"))
  (sb-ext:with-timeout 10
    (with-temp-socket-path (path)
      (with-connected-sockets (path listener client conn)
        (let ((s (socket-stream client)))
          (is (subtypep (stream-element-type s) '(unsigned-byte 8))
              "socket-stream element-type must be a subtype of (unsigned-byte 8)"))))))

;;; ── msg-command frame through real socket ────────────────────────────────────

(test socket-msg-command-roundtrip
  :description "A msg-command frame survives a real Unix-domain socket send→receive cycle."
  (unless (unix-socket-available-p)
    (skip "Unix-domain socket bind unavailable (sandbox)"))
  (sb-ext:with-timeout 10
    (with-temp-socket-path (path)
      (with-connected-sockets (path listener client conn)
        (let ((client-stream (socket-stream client))
              (server-stream (socket-stream conn)))
          (send-frame client-stream
                      (msg-command :rename-window "1:alpha" '("new-name")))
          (multiple-value-bind (type payload) (read-frame server-stream)
            (is (= +msg-command+ type)
                "msg-command type tag must survive socket roundtrip")
            (multiple-value-bind (command target args)
                (cl-tmux/protocol:decode-command-payload payload)
              (is (eq :rename-window command)
                  "command keyword must survive socket roundtrip")
              (is (string= "1:alpha" target)
                  "target string must survive socket roundtrip")
              (is (equal '("new-name") args)
                  "args must survive socket roundtrip"))))))))

;;; ── Bidirectional socket I/O ─────────────────────────────────────────────────

(test socket-bidirectional-frame-exchange
  :description "Client and server can both send and receive frames on the same socket pair."
  (unless (unix-socket-available-p)
    (skip "Unix-domain socket bind unavailable (sandbox)"))
  (sb-ext:with-timeout 10
    (with-temp-socket-path (path)
      (with-connected-sockets (path listener client conn)
        (let ((client-stream (socket-stream client))
              (server-stream (socket-stream conn)))
          ;; client → server
          (send-frame client-stream (msg-attach 24 80))
          (multiple-value-bind (type payload) (read-frame server-stream)
            (is (= +msg-attach+ type) "server must receive the attach frame")
            (multiple-value-bind (rows cols)
                (cl-tmux/protocol:decode-size payload)
              (is (= 24 rows) "attach rows must arrive at server")
              (is (= 80 cols) "attach cols must arrive at server")))
          ;; server → client
          (send-frame server-stream (msg-frame "rendered output"))
          (multiple-value-bind (type payload) (read-frame client-stream)
            (is (= +msg-frame+ type) "client must receive the frame message")
            (is (string= "rendered output"
                         (cl-tmux/protocol:decode-text payload))
                "frame text must survive the server→client direction")))))))

;;; ── make-listener / connect-to produce usable sockets ───────────────────────

(test make-listener-binds-at-given-path
  :description "make-listener creates a socket that connect-to can reach at the bound path."
  (unless (unix-socket-available-p)
    (skip "Unix-domain socket bind unavailable (sandbox)"))
  (sb-ext:with-timeout 10
    (with-temp-socket-path (path)
      ;; The with-connected-sockets macro drives make-listener + connect-to + accept.
      ;; If any of these fail the test will signal before reaching the IS forms.
      (with-connected-sockets (path listener client conn)
        (is-true listener "make-listener must return a socket")
        (is-true client   "connect-to must return a socket")
        (is-true conn     "accept-connection must return a socket")))))

;;; ── close-socket on never-used socket ────────────────────────────────────────

(test close-socket-on-fresh-socket-does-not-signal
  :description "close-socket on a freshly created socket (never bound) must not signal."
  (unless (unix-socket-available-p)
    (skip "Unix-domain socket bind unavailable (sandbox)"))
  (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (finishes (close-socket socket)
              "close-socket on a freshly created (unbound) socket must not signal")))
