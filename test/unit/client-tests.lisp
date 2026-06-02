(in-package #:cl-tmux/test)

;;;; Client lifecycle tests (src/client.lisp).
;;;;
;;;; run-client itself is integration-level (requires a live socket and raw
;;;; terminal) but its building blocks are unit-testable:
;;;;
;;;;   * socket-path naming — pure string function
;;;;   * with-incoming-frame dispatch — the same Prolog-dispatch macro used by
;;;;     both server (serve-client) and client (run-client); tested via a
;;;;     flexi-stream byte pipe that mimics a socket stream
;;;;   * +msg-frame+/+msg-bye+ routing — the two branches in run-client's
;;;;     inner loop that do not require real I/O
;;;;
;;;; Mock-stream technique: the protocol codec (cl-tmux/protocol) produces
;;;; and consumes byte vectors that are valid regardless of the stream source.
;;;; We feed them into a MAKE-TWO-WAY-STREAM over in-memory byte arrays, giving
;;;; us a real binary stream without sockets or threads.

(def-suite client-suite :description "Client connect/detach lifecycle")
(in-suite client-suite)

;;; ── Function existence ───────────────────────────────────────────────────────

(test client-run-client-is-defined
  "run-client is a defined function (integration tested via e2e-smoke)."
  (is (fboundp 'cl-tmux::run-client) "run-client must be defined"))

;;; ── socket-path naming ───────────────────────────────────────────────────────

(test client-socket-path-format
  "The socket path for session '0' includes the session name."
  (is (search "0" (cl-tmux::socket-path "0"))
      "socket path must contain the session name"))

(test client-socket-path-includes-cl-tmux-prefix
  "socket-path always produces a path that includes 'cl-tmux'."
  (let ((path (cl-tmux::socket-path "mysess")))
    (is (search "cl-tmux" path)
        "socket path must contain the cl-tmux prefix, got ~S" path)))

;;; ── with-incoming-frame dispatch (mock-stream) ───────────────────────────────
;;;
;;; These tests drive with-incoming-frame directly via an octet stream backed
;;; by a byte vector.  This is the exact same macro used in run-client's inner
;;; loop; covering its branches here avoids needing a real socket.
;;;
;;; Mock stream helper: wrap a byte vector in a FLEXI-compatible or
;;; GRAY-stream-like source.  We use the simplest approach available in the
;;; standard: CL:MAKE-STRING-INPUT-STREAM does not work for binary data, so we
;;; use the protocol frame bytes directly via a byte-vector stream built from
;;; sb-gray:fundamental-binary-input-stream — but the cleanest approach is to
;;; use the protocol + transport roundtrip through a temporary file or pipe-stream.
;;;
;;; Since cl-tmux/test already uses read-frame over a socket in net-tests, we
;;; follow the same pattern here: write frames to a temp-file stream, rewind,
;;; then drive with-incoming-frame from it.

(defun %make-frame-stream (frames)
  "Create a binary input stream containing FRAMES (octet vectors) concatenated.
   Returns an open :INPUT stream positioned at byte 0."
  (let* ((all-bytes (apply #'concatenate
                           '(vector (unsigned-byte 8))
                           frames))
         (path      (namestring
                     (merge-pathnames
                      (format nil "cl-tmux-client-test-~D.bin" (get-universal-time))
                      (uiop:temporary-directory)))))
    (with-open-file (out path :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
      (write-sequence all-bytes out))
    (open path :direction :input :element-type '(unsigned-byte 8))))

(test client-with-incoming-frame-msg-bye-matches
  "with-incoming-frame dispatches +msg-bye+ correctly — the :return path that
   run-client uses to exit its inner loop cleanly."
  (let ((stream (%make-frame-stream (list (msg-bye)))))
    (unwind-protect
         (let ((dispatched nil))
           (with-incoming-frame (type payload stream)
             ((null type)
              (setf dispatched :eof))
             ((= type +msg-bye+)
              (setf dispatched :bye))
             ((= type +msg-frame+)
              (setf dispatched :frame)))
           (is (eq :bye dispatched)
               "with-incoming-frame must dispatch +msg-bye+ to the :bye arm"))
      (ignore-errors (close stream)))))

(test client-with-incoming-frame-msg-frame-matches
  "with-incoming-frame dispatches +msg-frame+ correctly — the arm that paints
   the rendered frame string in run-client."
  (let ((stream (%make-frame-stream (list (msg-frame "hello")))))
    (unwind-protect
         (let ((received-text nil))
           (with-incoming-frame (type payload stream)
             ((null type)
              nil)
             ((= type +msg-bye+)
              nil)
             ((= type +msg-frame+)
              (setf received-text (decode-text payload))))
           (is (string= "hello" received-text)
               "msg-frame payload must decode to the original text"))
      (ignore-errors (close stream)))))

(test client-with-incoming-frame-eof-returns-nil-type
  "When the stream is empty, with-incoming-frame binds TYPE to NIL (EOF signal).
   run-client uses (null type) to exit the loop on server close."
  (let ((stream (%make-frame-stream '())))
    ;; Empty stream — no bytes — read-frame must return NIL type.
    (unwind-protect
         (let ((dispatched nil))
           (with-incoming-frame (type payload stream)
             ((null type)
              (setf dispatched :eof))
             (t
              (setf dispatched :unexpected)))
           (is (eq :eof dispatched)
               "empty stream must dispatch to the (null type) EOF arm"))
      (ignore-errors (close stream)))))

(test client-with-incoming-frame-multiple-frames-sequential
  "Consecutive with-incoming-frame calls consume frames in order — verifying
   the transport layer does not over-read when run-client loops."
  (let ((stream (%make-frame-stream (list (msg-frame "first")
                                          (msg-frame "second")
                                          (msg-bye)))))
    (unwind-protect
         (let ((results '()))
           ;; Read all three frames.
           (dotimes (_ 3)
             (with-incoming-frame (type payload stream)
               ((null type)
                (push :eof results))
               ((= type +msg-bye+)
                (push :bye results))
               ((= type +msg-frame+)
                (push (decode-text payload) results))))
           (setf results (nreverse results))
           (is (equal '("first" "second" :bye) results)
               "frames must arrive in order: ~S" results))
      (ignore-errors (close stream)))))

;;; ── detach-others flag wiring ────────────────────────────────────────────────

(test client-detach-others-message-encoding
  "msg-command :detach-other-clients produces a frame whose payload round-trips
   cleanly — this is the frame run-client sends when :detach-others is T."
  (let* ((frame   (msg-command :detach-other-clients nil nil))
         (decoded (multiple-value-list (decode-frame frame))))
    (is (= +msg-command+ (first decoded))
        "msg-command :detach-other-clients must encode as +msg-command+ type")))
