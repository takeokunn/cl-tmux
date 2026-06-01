(in-package #:cl-tmux/transport)

;;;; Frame transport over a binary stream (client/server detach-attach).
;;;;
;;;; This is the impure shell around the pure cl-tmux/protocol codec: it moves
;;;; encoded frames across any binary stream — a socket made via
;;;; sb-bsd-sockets:socket-make-stream, or (in tests) a temp-file stream.
;;;; The framing/parsing itself lives in cl-tmux/protocol; here we only do I/O.

(defun send-frame (stream frame)
  "Write FRAME (an octet vector produced by the cl-tmux/protocol msg-* helpers)
   to binary STREAM and flush it."
  (write-sequence frame stream)
  (finish-output stream))

(defun read-frame (stream)
  "Read one complete frame from binary STREAM.
   Returns (values TYPE PAYLOAD), or NIL at end of stream (peer closed or the
   stream ended mid-frame).  Assumes a blocking stream: READ-SEQUENCE fills the
   buffer fully unless EOF is reached, so a short read means the peer hung up."
  (let ((header (make-array +header-size+ :element-type '(unsigned-byte 8))))
    (when (= +header-size+ (read-sequence header stream))
      (let* ((length (read-u32 header 1))
             (frame  (make-array (+ +header-size+ length)
                                 :element-type '(unsigned-byte 8))))
        (replace frame header)
        (when (= (length frame) (read-sequence frame stream :start +header-size+))
          (multiple-value-bind (type payload) (decode-frame frame)
            (values type payload)))))))
