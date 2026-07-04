(in-package #:cl-tmux/test)

;;;; Client lifecycle and outbound client tests (src/client.lisp).
;;;;
;;;; run-client itself is integration-level (requires a live socket and raw
;;;; terminal) but its building blocks are unit-testable:
;;;;
;;;;   * socket-path naming — pure string function
;;;;   * with-incoming-frame dispatch — the same Prolog-dispatch macro used by
;;;;     both server (serve-client) and client (run-client); tested via a real
;;;;     Unix-domain socket roundtrip (same technique as net-tests.lisp), guarded
;;;;     by unix-socket-available-p so tests self-skip in restricted sandboxes
;;;;   * msg-command encoding — verifies the detach-others frame type
;;;;
;;;; Server-frame receive/decode behavior lives in client-receive-tests.lisp.

(def-suite client-suite :description "Client connect/detach lifecycle")
(in-suite client-suite)

;;; ── Function existence ───────────────────────────────────────────────────────

(test client-functions-fbound-table
  :description "All key client mode functions are fbound."
  (dolist (sym '(cl-tmux::run-client
                 cl-tmux::%ensure-server-running
                 cl-tmux::run-attach-simple
                 cl-tmux::run-attach-with-flags))
    (is (fboundp sym) "~A must be fbound" sym)))

;;; socket-path naming is tested canonically in server-tests.lisp since
;;; socket-path is defined in server.lisp.  No duplicate tests here.

;;; ── with-incoming-frame dispatch (socket roundtrip) ─────────────────────────
;;;
;;; These tests drive with-incoming-frame directly via a Unix-domain socket
;;; stream pair.  We write frames from one end and read from the other, exactly
;;; as run-client does.  The macro is in cl-tmux/transport and is used by both
;;; server (serve-client) and client (run-client).

(defmacro with-client-test-socket-pair ((writer-stream reader-stream) &body body)
  "Create a Unix-domain socket pair: listener→accept→connect.
   WRITER-STREAM and READER-STREAM are bidirectional binary streams.
   Writer side simulates the server sending frames; reader side reads them
   (matches the run-client perspective where the server writes and client reads)."
  (let ((path    (gensym "PATH"))
        (lstnr   (gensym "LSTNR"))
        (wsock   (gensym "WSOCK"))
        (rsock   (gensym "RSOCK")))
    `(let ((,path (%test-socket-path "client-dispatch-test")))
       (let ((,lstnr (make-listener ,path)))
         (unwind-protect
              (let* ((,rsock (connect-to ,path))
                     (,wsock (accept-connection ,lstnr))
                     (,writer-stream (socket-stream ,wsock))
                     (,reader-stream (socket-stream ,rsock)))
                (unwind-protect
                     (progn ,@body)
                  (ignore-errors (close-socket ,wsock))
                  (ignore-errors (close-socket ,rsock))))
           (ignore-errors (close-socket ,lstnr))
           (ignore-errors (delete-file ,path)))))))

(defmacro with-guarded-socket-test (&body body)
  "Skip unless Unix-domain sockets are available, then run BODY under a 10-second
   timeout inside a socket-pair.  Eliminates the repeated three-line boilerplate:
     (unless (unix-socket-available-p) (skip ...))
     (sb-ext:with-timeout 10 ...)
     (with-client-test-socket-pair ...)
   that appeared in every socket-roundtrip test."
  (let ((server-side (gensym "SERVER-SIDE"))
        (client-side (gensym "CLIENT-SIDE")))
    `(progn
       (unless (unix-socket-available-p)
         (skip "Unix-domain socket unavailable (sandbox)"))
       (sb-ext:with-timeout 10
         (with-client-test-socket-pair (,server-side ,client-side)
           (symbol-macrolet ((server-side ,server-side)
                             (client-side ,client-side))
             ,@body))))))

;;; with-guarded-socket-test/fd: variant exposing raw socket objects so tests
;;; that need socket-fd (e.g. %read-command-reply's select-fds parameter) can
;;; obtain it without duplicating the full socket lifecycle.
;;;
;;; Binds SERVER-SOCK/CLIENT-SOCK (socket objects), SERVER-STREAM/CLIENT-STREAM
;;; (binary streams), and CLIENT-FD (the integer fd of the client socket).

(defmacro with-guarded-socket-test/fd ((&key (server-sock (gensym "SSOCK"))
                                              (client-sock (gensym "CSOCK"))
                                              (server-stream (gensym "SSTREAM"))
                                              (client-stream (gensym "CSTREAM"))
                                              (client-fd (gensym "CFD")))
                                        &body body)
  "Like with-guarded-socket-test but exposes socket objects and the client fd.
   Useful when a test needs (cl-tmux/net:socket-fd client) alongside the stream."
  (let ((path  (gensym "PATH"))
        (lstnr (gensym "LSTNR")))
    `(progn
       (unless (unix-socket-available-p)
         (skip "Unix-domain socket unavailable (sandbox)"))
       (sb-ext:with-timeout 10
         (let* ((,path   (%test-socket-path "client-dispatch-test"))
                (,lstnr  (make-listener ,path)))
           (unwind-protect
                (let* ((,client-sock  (connect-to ,path))
                       (,server-sock  (accept-connection ,lstnr))
                       (,server-stream (socket-stream ,server-sock))
                       (,client-stream (socket-stream ,client-sock))
                       (,client-fd     (cl-tmux/net:socket-fd ,client-sock)))
                  (declare (ignorable ,server-stream ,client-stream ,client-fd))
                  (unwind-protect
                       (progn ,@body)
                    (ignore-errors (close-socket ,server-sock))
                    (ignore-errors (close-socket ,client-sock))))
             (ignore-errors (close-socket ,lstnr))
             (ignore-errors (delete-file ,path))))))))
