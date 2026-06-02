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
;;;; with-temp-socket-path and write-frames-to-file are defined in
;;;; test/helpers.lisp and shared with transport-tests.lisp.

(def-suite net-suite :description "Unix-domain socket transport (sb-bsd-sockets)")
(in-suite net-suite)

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
      (let ((listener (make-listener path)))
        (unwind-protect
             (let* ((client (connect-to path))
                    (conn   (accept-connection listener))
                    (cstream (socket-stream client))
                    (sstream (socket-stream conn)))
               (is (streamp cstream) "socket-stream must return a stream (client side)")
               (is (streamp sstream) "socket-stream must return a stream (server side)")
               (ignore-errors (close-socket client))
               (ignore-errors (close-socket conn)))
          (ignore-errors (close-socket listener)))))))

;;; ── accept-connection / make-listener roundtrip ──────────────────────────────

(test make-listener-accept-connection-returns-socket
  :description "accept-connection returns a socket object for an inbound connection."
  (unless (unix-socket-available-p)
    (skip "Unix-domain socket bind unavailable (sandbox)"))
  (sb-ext:with-timeout 10
    (with-temp-socket-path (path)
      (let ((listener (make-listener path)))
        (unwind-protect
             (let* ((client (connect-to path))
                    (conn   (accept-connection listener)))
               (is-true conn "accept-connection must return a socket")
               (ignore-errors (close-socket client))
               (ignore-errors (close-socket conn)))
          (ignore-errors (close-socket listener)))))))

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
      (let ((listener (make-listener path)))
        (unwind-protect
             (let* ((client (connect-to path))            ; queued in the backlog
                    (conn   (accept-connection listener))  ; dequeues it
                    (client-stream (socket-stream client))
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
                     "msg-frame Unicode payload must round-trip correctly"))
               (close-socket client)
               (close-socket conn))
          (close-socket listener))))))

(test socket-multiple-frames-in-order
  :description "Multiple frames queued by the sender are consumed in send order."
  (unless (unix-socket-available-p)
    (skip "Unix-domain socket bind unavailable (sandbox)"))
  (sb-ext:with-timeout 10
    (with-temp-socket-path (path)
      (let ((listener (make-listener path)))
        (unwind-protect
             (let* ((client (connect-to path))
                    (conn   (accept-connection listener))
                    (cs (socket-stream client))
                    (ss (socket-stream conn)))
               ;; Send three distinct frames
               (send-frame cs (msg-frame "first"))
               (send-frame cs (msg-frame "second"))
               (send-frame cs (msg-frame "third"))
               (let ((results
                      (loop repeat 3
                            collect (multiple-value-bind (type payload)
                                        (read-frame ss)
                                      (declare (ignore type))
                                      (decode-text payload)))))
                 (is (equal '("first" "second" "third") results)
                     "frames must arrive in send order: ~S" results))
               (ignore-errors (close-socket client))
               (ignore-errors (close-socket conn)))
          (ignore-errors (close-socket listener)))))))

(test socket-listener-fd-distinct-from-client-fd
  :description "The listener fd and the client fd must be distinct (different kernel fds)."
  (unless (unix-socket-available-p)
    (skip "Unix-domain socket bind unavailable (sandbox)"))
  (sb-ext:with-timeout 10
    (with-temp-socket-path (path)
      (let ((listener (make-listener path)))
        (unwind-protect
             (let* ((client (connect-to path))
                    (conn   (accept-connection listener)))
               (is (/= (socket-fd listener) (socket-fd client))
                   "listener fd must differ from client fd")
               (is (/= (socket-fd listener) (socket-fd conn))
                   "listener fd must differ from accepted-conn fd")
               (ignore-errors (close-socket client))
               (ignore-errors (close-socket conn)))
          (ignore-errors (close-socket listener)))))))
