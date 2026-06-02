(in-package #:cl-tmux/transport)

;;;; Frame transport over a binary stream (client/server detach-attach).
;;;;
;;;; This is the impure shell around the pure cl-tmux/protocol codec: it moves
;;;; encoded frames across any binary stream — a socket made via
;;;; sb-bsd-sockets:socket-make-stream, or (in tests) a temp-file stream.
;;;; The framing/parsing itself lives in cl-tmux/protocol; here we only do I/O.

(defconstant +read-frame-timeout-seconds+ 30
  "Maximum seconds to wait for a complete frame before aborting.
   Prevents indefinite blocking on a hung or slow peer.")

(defun send-frame (stream frame)
  "Write FRAME (an octet vector produced by the cl-tmux/protocol msg-* helpers)
   to binary STREAM and flush it."
  (write-sequence frame stream)
  (finish-output stream))

(defun %read-sequence-with-timeout (buffer stream start)
  "Fill BUFFER[START..] from STREAM, returning the number of bytes read.
   Wraps READ-SEQUENCE in a timeout so a hung peer does not block forever.
   Returns the number of bytes actually read (< expected ⇒ short read / EOF)."
  (sb-ext:with-timeout +read-frame-timeout-seconds+
    (read-sequence buffer stream :start start)))

(defun read-frame (stream)
  "Read one complete frame from binary STREAM.
   Returns (values TYPE PAYLOAD), or NIL at end of stream (peer closed,
   mid-frame EOF, or timeout expiry after +read-frame-timeout-seconds+).
   Assumes a blocking stream: READ-SEQUENCE fills the buffer fully unless
   EOF or timeout is reached, so a short read means the peer hung up."
  (handler-case
      (let ((header (make-array +header-size+ :element-type '(unsigned-byte 8))))
        (when (= +header-size+ (%read-sequence-with-timeout header stream 0))
          (let* ((payload-length (read-u32 header 1))
                 (frame          (make-array (+ +header-size+ payload-length)
                                             :element-type '(unsigned-byte 8))))
            (replace frame header)
            (when (= (length frame)
                     (%read-sequence-with-timeout frame stream +header-size+))
              (multiple-value-bind (type payload) (decode-frame frame)
                (values type payload))))))
    (sb-ext:timeout () nil)))

(defmacro with-incoming-frame ((type-var payload-var stream) &rest rules)
  "Read one frame from STREAM, bind TYPE-VAR and PAYLOAD-VAR, then dispatch
   through the Prolog-like rule table RULES.  Each RULE is (condition &rest body).
   NIL type (EOF) must be handled by the caller if no rule matches.

   Prolog analogy:
     handle_frame(nil, _)           :- disconnect.
     handle_frame(msg_detach, _)    :- disconnect.
     handle_frame(msg_key, payload) :- process_keys(session, payload)."
  `(multiple-value-bind (,type-var ,payload-var) (read-frame ,stream)
     (cond ,@(mapcar (lambda (rule)
                       (destructuring-bind (condition &rest body) rule
                         `(,condition ,@body)))
                     rules))))
