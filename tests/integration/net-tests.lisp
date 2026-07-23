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
;;;; are defined in tests/helpers-net-protocol.lisp and shared with transport-tests.lisp.

(describe "net-suite"

  ;;; ── %make-probe-socket-path ──────────────────────────────────────────────
  ;;;
  ;;; %make-probe-socket-path is a private helper that generates a throwaway socket
  ;;; path in the temp directory.  It is called only by unix-socket-available-p, so
  ;;; it has no direct test coverage.  These tests pin its contract: the returned
  ;;; path must be a non-empty string in the temp directory, and two successive
  ;;; calls must return distinct paths (collision-resistance).

  ;; cl-tmux/net::%make-probe-socket-path must return a non-empty string.
  (it "make-probe-socket-path-returns-nonempty-string"
    (let ((path (cl-tmux/net::%make-probe-socket-path)))
      (expect (stringp path))
      (expect (plusp (length path)))))

  ;; %make-probe-socket-path must produce a path inside the system temp directory.
  (it "make-probe-socket-path-is-in-temp-directory"
    (let* ((path    (cl-tmux/net::%make-probe-socket-path))
           (tmpdir  (string-right-trim "/" (or (sb-ext:posix-getenv "TMPDIR") "/tmp"))))
      (expect (and (> (length path) (length tmpdir))
                   (string= tmpdir (subseq path 0 (length tmpdir)))))))

  ;; %make-probe-socket-path must produce a path ending in ".sock".
  (it "make-probe-socket-path-has-sock-suffix"
    (let ((path (cl-tmux/net::%make-probe-socket-path)))
      (expect (string= ".sock" (subseq path (- (length path) 5))))))

  ;; Two successive calls to %make-probe-socket-path must return different paths
  ;; (collision-resistance for concurrent test runs or parallel probing).
  (it "make-probe-socket-path-successive-calls-return-distinct-paths"
    (let ((path1 (cl-tmux/net::%make-probe-socket-path))
          (path2 (cl-tmux/net::%make-probe-socket-path)))
      (expect (not (string= path1 path2)))))

  ;;; ── unix-socket-available-p ──────────────────────────────────────────────

  ;; unix-socket-available-p answers without error (T or NIL).
  (it "unix-socket-availability-is-boolean"
    (let ((answer (unix-socket-available-p)))
      (expect (member answer '(t nil)))))

  ;;; ── connect-to error path ─────────────────────────────────────────────────

  ;; Connecting to a non-existent socket path signals an error.
  (it "connect-to-missing-path-signals"
    (signals error
      (connect-to "/nonexistent-cl-tmux-dir/does-not-exist.sock")))

  ;; Connecting to an empty-string path signals an error.
  (it "connect-to-empty-path-signals"
    (signals error
      (connect-to "")))

  ;;; ── socket-fd on bound listener ───────────────────────────────────────────

  ;; socket-fd returns a non-negative file descriptor for a bound listener socket.
  (it "socket-fd-returns-non-negative-integer"
    (unless (unix-socket-available-p)
      (skip "Unix-domain socket bind unavailable (sandbox)"))
    (with-temp-socket-path (path)
      (let ((socket (make-listener path)))
        (unwind-protect
             (let ((fd (socket-fd socket)))
               (expect (integerp fd))
               (expect (>= fd 0)))
          (ignore-errors (close-socket socket))))))

  ;;; ── close-socket idempotency ──────────────────────────────────────────────

  ;; close-socket on an already-closed socket does not signal.
  (it "close-socket-is-idempotent"
    (unless (unix-socket-available-p)
      (skip "Unix-domain socket bind unavailable (sandbox)"))
    (with-temp-socket-path (path)
      (let ((socket (make-listener path)))
        (close-socket socket)
        ;; Second close must not signal — it is wrapped in ignore-errors internally.
        (finishes (close-socket socket)
                  "second close-socket on same socket must not signal"))))

  ;;; ── socket-stream produces a binary stream ────────────────────────────────

  ;; socket-stream wraps a bound socket in a binary I/O stream.
  (it "socket-stream-is-a-stream"
    (unless (unix-socket-available-p)
      (skip "Unix-domain socket bind unavailable (sandbox)"))
    (sb-ext:with-timeout 10
      (with-temp-socket-path (path)
        (with-connected-sockets (path listener client conn)
          (let ((client-stream (socket-stream client))
                (server-stream (socket-stream conn)))
            (expect (streamp client-stream))
            (expect (streamp server-stream)))))))

  ;;; ── accept-connection / make-listener roundtrip ───────────────────────────

  ;; accept-connection returns a socket object for an inbound connection.
  (it "make-listener-accept-connection-returns-socket"
    (unless (unix-socket-available-p)
      (skip "Unix-domain socket bind unavailable (sandbox)"))
    (sb-ext:with-timeout 10
      (with-temp-socket-path (path)
        (with-connected-sockets (path listener client conn)
          (expect conn :to-be-truthy)))))

  ;; accept-connection returns NIL (rather than blocking or erroring) when the
  ;; accept itself times out — the race-condition path documented on both
  ;; accept-connection and its sole caller, %accept-pending-connection, but
  ;; previously exercised only via the success path (a real client always
  ;; connected immediately in every other test).
  (it "accept-connection-returns-nil-on-timeout"
    (unless (unix-socket-available-p)
      (skip "Unix-domain socket bind unavailable (sandbox)"))
    (with-temp-socket-path (path)
      (let* ((listener (make-listener path))
             (timeout-mock (make-mock-function
                            (lambda (l) (declare (ignore l)) (error 'sb-ext:timeout)))))
        (unwind-protect
             (with-mocked-functions
                 (((fdefinition 'sb-bsd-sockets:socket-accept) timeout-mock))
               (expect (null (cl-tmux/net:accept-connection listener))))
          (close-socket listener)))))

  ;;; ── Table-driven: multiple message types roundtrip ────────────────────────
  ;;;
  ;;; Each row in the table below encodes a message type, a predicate applied to
  ;;; the decoded type tag, and a payload decoder.  All rows share one
  ;;; bind→connect→accept socket pair to avoid the overhead of multiple setups.

  ;; A protocol frame survives a real bind→connect→accept→send→read roundtrip.
  (it "socket-frame-roundtrip"
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
              (expect (= +msg-key+ type))
              (expect (equalp #(65 66) payload)))
            (multiple-value-bind (type payload) (read-frame server-stream)
              (declare (ignore payload))
              (expect (= +msg-detach+ type)))
            ;; server → client: a rendered frame with Unicode content
            (send-frame server-stream (msg-frame "あ"))
            (multiple-value-bind (type payload) (read-frame client-stream)
              (expect (= +msg-frame+ type))
              (expect (string= "あ" (decode-text payload)))))))))

  ;; Multiple frames queued by the sender are consumed in send order.
  (it "socket-multiple-frames-in-order"
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
              (expect (equal '("first" "second" "third") results))))))))

  ;; The listener fd and the client fd must be distinct (different kernel fds).
  (it "socket-listener-fd-distinct-from-client-fd"
    (unless (unix-socket-available-p)
      (skip "Unix-domain socket bind unavailable (sandbox)"))
    (sb-ext:with-timeout 10
      (with-temp-socket-path (path)
        (with-connected-sockets (path listener client conn)
          (expect (/= (socket-fd listener) (socket-fd client)))
          (expect (/= (socket-fd listener) (socket-fd conn)))))))

  ;;; ── Net constant values ───────────────────────────────────────────────────

  ;; +accept-timeout-seconds+ must be a positive integer for sb-ext:with-timeout.
  (it "accept-timeout-constant-is-positive-integer"
    (expect (integerp cl-tmux/net::+accept-timeout-seconds+))
    (expect (plusp cl-tmux/net::+accept-timeout-seconds+)))

  ;; +socket-stream-timeout-seconds+ must be a positive integer for socket-make-stream.
  (it "socket-stream-timeout-constant-is-positive-integer"
    (expect (integerp cl-tmux/net::+socket-stream-timeout-seconds+))
    (expect (plusp cl-tmux/net::+socket-stream-timeout-seconds+)))

  ;; +connect-timeout-seconds+ must be a positive integer for sb-ext:with-timeout.
  (it "connect-timeout-constant-is-positive-integer"
    (expect (integerp cl-tmux/net::+connect-timeout-seconds+))
    (expect (plusp cl-tmux/net::+connect-timeout-seconds+)))

  ;;; ── Socket stream element type ────────────────────────────────────────────

  ;; socket-stream must produce a stream whose element-type is (unsigned-byte 8).
  (it "socket-stream-has-binary-element-type"
    (unless (unix-socket-available-p)
      (skip "Unix-domain socket bind unavailable (sandbox)"))
    (sb-ext:with-timeout 10
      (with-temp-socket-path (path)
        (with-connected-sockets (path listener client conn)
          (let ((s (socket-stream client)))
            (expect (subtypep (stream-element-type s) '(unsigned-byte 8))))))))

  ;;; ── msg-command frame through real socket ─────────────────────────────────

  ;; A msg-command frame survives a real Unix-domain socket send→receive cycle.
  (it "socket-msg-command-roundtrip"
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
              (expect (= +msg-command+ type))
              (multiple-value-bind (command target args)
                  (cl-tmux/protocol:decode-command-payload payload)
                (expect (eq :rename-window command))
                (expect (string= "1:alpha" target))
                (expect (equal '("new-name") args)))))))))

  ;;; ── Bidirectional socket I/O ──────────────────────────────────────────────

  ;; Client and server can both send and receive frames on the same socket pair.
  (it "socket-bidirectional-frame-exchange"
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
              (expect (= +msg-attach+ type))
              (multiple-value-bind (rows cols)
                  (cl-tmux/protocol:decode-size payload)
                (expect (= 24 rows))
                (expect (= 80 cols))))
            ;; server → client
            (send-frame server-stream (msg-frame "rendered output"))
            (multiple-value-bind (type payload) (read-frame client-stream)
              (expect (= +msg-frame+ type))
              (expect (string= "rendered output"
                                (cl-tmux/protocol:decode-text payload)))))))))

  ;;; ── make-listener / connect-to produce usable sockets ────────────────────

  ;; make-listener creates a socket that connect-to can reach at the bound path.
  (it "make-listener-binds-at-given-path"
    (unless (unix-socket-available-p)
      (skip "Unix-domain socket bind unavailable (sandbox)"))
    (sb-ext:with-timeout 10
      (with-temp-socket-path (path)
        ;; The with-connected-sockets macro drives make-listener + connect-to + accept.
        ;; If any of these fail the test will signal before reaching the EXPECT forms.
        (with-connected-sockets (path listener client conn)
          (expect listener :to-be-truthy)
          (expect client :to-be-truthy)
          (expect conn :to-be-truthy)))))

  ;;; ── make-listener explicit :backlog ───────────────────────────────────────

  ;; make-listener with an explicit :backlog keyword still binds and
  ;; accepts a connection (the keyword is forwarded to socket-listen without error).
  (it "make-listener-accepts-explicit-backlog"
    (unless (unix-socket-available-p)
      (skip "Unix-domain socket bind unavailable (sandbox)"))
    (sb-ext:with-timeout 10
      (with-temp-socket-path (path)
        (let ((listener (make-listener path :backlog 4)))
          (unwind-protect
               (let* ((client (connect-to path))
                      (conn   (accept-connection listener)))
                 (unwind-protect
                      (expect conn :to-be-truthy)
                   (ignore-errors (close-socket client))
                   (ignore-errors (close-socket conn))))
            (ignore-errors (close-socket listener)))))))

  ;;; ── close-socket on never-used socket ─────────────────────────────────────

  ;; close-socket on a freshly created socket (never bound) must not signal.
  (it "close-socket-on-fresh-socket-does-not-signal"
    (unless (unix-socket-available-p)
      (skip "Unix-domain socket bind unavailable (sandbox)"))
    (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
      (finishes (close-socket socket)
                "close-socket on a freshly created (unbound) socket must not signal"))))
