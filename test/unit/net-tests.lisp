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

(test unix-socket-availability-is-boolean
  "unix-socket-available-p answers without error (T or NIL)."
  (let ((answer (unix-socket-available-p)))
    (is (member answer '(t nil)) "probe returns a boolean, got ~S" answer)))

(test connect-to-missing-path-signals
  "Connecting to a non-existent socket path signals an error."
  (signals error
    (connect-to "/nonexistent-cl-tmux-dir/does-not-exist.sock")))

(test socket-fd-returns-non-negative-integer
  "socket-fd returns a non-negative file descriptor for a bound listener socket."
  (unless (unix-socket-available-p)
    (skip "Unix-domain socket bind unavailable (sandbox)"))
  (with-temp-socket-path (path)
    (let ((socket (make-listener path)))
      (unwind-protect
           (let ((fd (socket-fd socket)))
             (is (integerp fd) "socket-fd must return an integer")
             (is (>= fd 0)     "socket-fd must be non-negative, got ~D" fd))
        (ignore-errors (close-socket socket))))))

(test socket-frame-roundtrip
  "A protocol frame survives a real bind→connect→accept→send→read roundtrip."
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
                 (is (= +msg-key+ type))
                 (is (equalp #(65 66) payload)))
               (multiple-value-bind (type payload) (read-frame server-stream)
                 (declare (ignore payload))
                 (is (= +msg-detach+ type)))
               ;; server → client: a rendered frame
               (send-frame server-stream (msg-frame "あ"))
               (multiple-value-bind (type payload) (read-frame client-stream)
                 (is (= +msg-frame+ type))
                 (is (string= "あ" (decode-text payload))))
               (close-socket client)
               (close-socket conn))
          (close-socket listener))))))
