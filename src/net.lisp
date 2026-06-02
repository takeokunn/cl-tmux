(in-package #:cl-tmux/net)

;;;; Unix-domain socket primitives for client/server detach-attach.
;;;;
;;;; Thin wrappers over sb-bsd-sockets so the server/client loops (and tests)
;;;; speak in terms of make-listener / accept-connection / connect-to / a binary
;;;; socket-stream, rather than the raw contrib API.  Frame I/O over the stream
;;;; lives in cl-tmux/transport; message framing in cl-tmux/protocol.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-bsd-sockets))

(defconstant +accept-timeout-seconds+ 5
  "Maximum seconds to block in accept-connection before returning NIL.
   Prevents the server accept loop from blocking forever on a client that
   opens a TCP connection but never sends any data.")

(defconstant +socket-stream-timeout-seconds+ 30
  "Timeout in seconds passed to socket-make-stream for read/write operations.
   Bounds the duration of individual read-sequence / write-sequence calls on a
   socket stream so a hung or slow peer does not block the server indefinitely.")

(defun make-listener (path &key (backlog 1))
  "Bind a Unix-domain stream socket at PATH and start listening (BACKLOG deep)."
  (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (sb-bsd-sockets:socket-bind socket path)
    (sb-bsd-sockets:socket-listen socket backlog)
    socket))

(defun accept-connection (listener)
  "Accept one connection from LISTENER within +accept-timeout-seconds+.
   Returns the connected socket, or NIL when the accept times out.
   Prevents the server accept loop from blocking forever on a client that
   connects at the TCP level but never sends a handshake."
  (handler-case
      (sb-ext:with-timeout +accept-timeout-seconds+
        (sb-bsd-sockets:socket-accept listener))
    (sb-ext:timeout () nil)))

(defun connect-to (path)
  "Connect a fresh Unix-domain stream socket to the listener at PATH."
  (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (sb-bsd-sockets:socket-connect socket path)
    socket))

(defun socket-stream (socket)
  "A bidirectional binary stream over SOCKET (element-type (unsigned-byte 8)).
   The stream is created with a timeout so individual read/write calls do not
   block indefinitely when the peer is hung."
  (sb-bsd-sockets:socket-make-stream socket
                                     :input t :output t
                                     :element-type '(unsigned-byte 8)
                                     :timeout +socket-stream-timeout-seconds+))

(defun socket-fd (socket)
  "The underlying file descriptor of SOCKET (for select-based multiplexing)."
  (sb-bsd-sockets:socket-file-descriptor socket))

(defun close-socket (socket)
  "Close SOCKET, ignoring errors (e.g. already closed by its stream)."
  (ignore-errors (sb-bsd-sockets:socket-close socket)))

(defun %make-probe-socket-path ()
  "Generate a unique throwaway socket path in the temp directory."
  (let ((directory (string-right-trim "/" (or (sb-ext:posix-getenv "TMPDIR") "/tmp"))))
    (format nil "~A/cl-tmux-probe-~D-~D.sock"
            directory (get-universal-time) (random 1000000))))

(defun unix-socket-available-p ()
  "True when a Unix-domain socket can be bound in the temp directory.
   Probes by binding then removing a throwaway socket path; returns NIL when
   the environment forbids it (e.g. a restricted sandbox)."
  (let ((path (%make-probe-socket-path)))
    (handler-case
        (let ((socket (make-listener path)))
          (close-socket socket)
          (ignore-errors (delete-file path))
          t)
      (error () nil))))
