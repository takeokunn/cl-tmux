(in-package #:cl-tmux/test)

;;;; Unix-domain socket primitive tests (src/net.lisp).
;;;;
;;;; The roundtrip test drives a REAL kernel socket: it binds a listener,
;;;; connects, accepts, and pushes a protocol frame end-to-end through the
;;;; transport — all single-threaded (connect queues, accept dequeues, the
;;;; kernel buffers the few bytes), wrapped in a timeout, and guarded by an
;;;; availability probe so it self-skips where a sandbox forbids socket bind
;;;; (mirroring the PTY tests).

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

(test socket-frame-roundtrip
  "A protocol frame survives a real bind→connect→accept→send→read roundtrip."
  (unless (unix-socket-available-p)
    (skip "Unix-domain socket bind unavailable (sandbox)"))
  (sb-ext:with-timeout 10
    (let* ((path (namestring
                  (merge-pathnames (format nil "cl-tmux-net-~D.sock"
                                           (get-universal-time))
                                   (uiop:temporary-directory))))
           (listener (make-listener path)))
      (unwind-protect
           (let* ((client (connect-to path))           ; queued in the backlog
                  (conn   (accept-connection listener)) ; dequeues it
                  (cs     (socket-stream client))
                  (ss     (socket-stream conn)))
             ;; client → server: a key frame, then a detach frame
             (send-frame cs (msg-key #(65 66)))
             (send-frame cs (msg-detach))
             (multiple-value-bind (type payload) (read-frame ss)
               (is (= +msg-key+ type))
               (is (equalp #(65 66) payload)))
             (multiple-value-bind (type payload) (read-frame ss)
               (declare (ignore payload))
               (is (= +msg-detach+ type)))
             ;; server → client: a rendered frame
             (send-frame ss (msg-frame "あ"))
             (multiple-value-bind (type payload) (read-frame cs)
               (is (= +msg-frame+ type))
               (is (string= "あ" (decode-text payload))))
             (close-socket client)
             (close-socket conn))
        (close-socket listener)
        (ignore-errors (delete-file path))))))
