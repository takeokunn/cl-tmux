(in-package #:cl-tmux/net)

;;;; Unix-domain socket primitives for client/server detach-attach.
;;;;
;;;; Thin wrappers over sb-bsd-sockets so the server/client loops (and tests)
;;;; speak in terms of make-listener / accept-connection / connect-to / a binary
;;;; socket-stream, rather than the raw contrib API.  Frame I/O over the stream
;;;; lives in cl-tmux/transport; message framing in cl-tmux/protocol.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-bsd-sockets))

(defun make-listener (path &key (backlog 1))
  "Bind a Unix-domain stream socket at PATH and start listening (BACKLOG deep)."
  (let ((sock (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (sb-bsd-sockets:socket-bind sock path)
    (sb-bsd-sockets:socket-listen sock backlog)
    sock))

(defun accept-connection (listener)
  "Accept one connection from LISTENER; returns the connected socket (blocking)."
  (sb-bsd-sockets:socket-accept listener))

(defun connect-to (path)
  "Connect a fresh Unix-domain stream socket to the listener at PATH."
  (let ((sock (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (sb-bsd-sockets:socket-connect sock path)
    sock))

(defun socket-stream (socket)
  "A bidirectional binary stream over SOCKET (element-type (unsigned-byte 8))."
  (sb-bsd-sockets:socket-make-stream socket
                                     :input t :output t
                                     :element-type '(unsigned-byte 8)))

(defun socket-fd (socket)
  "The underlying file descriptor of SOCKET (for select-based multiplexing)."
  (sb-bsd-sockets:socket-file-descriptor socket))

(defun close-socket (socket)
  "Close SOCKET, ignoring errors (e.g. already closed by its stream)."
  (ignore-errors (sb-bsd-sockets:socket-close socket)))

(defun unix-socket-available-p ()
  "True when a Unix-domain socket can be bound in the temp directory.
   Probes by binding then removing a throwaway socket path; returns NIL when the
   environment forbids it (e.g. a restricted sandbox)."
  (let* ((dir  (or (sb-ext:posix-getenv "TMPDIR") "/tmp"))
         (path (format nil "~A/cl-tmux-probe-~D-~D.sock"
                       (string-right-trim "/" dir)
                       (get-universal-time) (random 1000000))))
    (handler-case
        (let ((sock (make-listener path)))
          (close-socket sock)
          (ignore-errors (delete-file path))
          t)
      (error () nil))))
