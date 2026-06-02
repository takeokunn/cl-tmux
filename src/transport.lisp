(in-package #:cl-tmux/transport)

;;;; Frame transport over a binary stream (client/server detach-attach).
;;;;
;;;; This is the impure shell around the pure cl-tmux/protocol codec: it moves
;;;; encoded frames across any binary stream — a socket made via
;;;; sb-bsd-sockets:socket-make-stream, or (in tests) a temp-file stream.
;;;; The framing/parsing itself lives in cl-tmux/protocol; here we only do I/O.

(defconstant +read-frame-timeout-seconds+ 30
  "Maximum seconds to wait for a complete frame before aborting.
   Prevents indefinite blocking on a hung or slow peer.
   This budget is shared across both the header and payload read phases.")

(defun send-frame (stream frame)
  "Write FRAME (an octet vector produced by the cl-tmux/protocol msg-* helpers)
   to binary STREAM and flush it."
  (write-sequence frame stream)
  (finish-output stream))

(defun %read-exact (buffer stream start end)
  "Fill BUFFER[START..END) from STREAM; return the actual number of bytes read.
   A return value less than (- END START) indicates EOF or short-read."
  (read-sequence buffer stream :start start :end end))

(defun read-frame (stream)
  "Read one complete frame from binary STREAM.
   Returns (values TYPE PAYLOAD), or NIL at end of stream (peer closed,
   mid-frame EOF, or timeout expiry after +read-frame-timeout-seconds+).
   Uses a two-phase read into a single adjustable buffer: the header bytes are
   read first (Phase 1), then the buffer is grown with ADJUST-ARRAY and the
   payload bytes are read directly into the tail (Phase 2).  No intermediate
   header array is allocated and no REPLACE copy is needed.
   A single sb-ext:with-timeout wraps both phases so the total wall-clock
   budget is at most +read-frame-timeout-seconds+ seconds."
  (handler-case
      (sb-ext:with-timeout +read-frame-timeout-seconds+
        (let ((frame (make-array +header-size+
                                 :element-type '(unsigned-byte 8)
                                 :adjustable t
                                 :fill-pointer +header-size+)))
          ;; Phase 1: fill the header region [0 .. +header-size+).
          (when (= +header-size+ (%read-exact frame stream 0 +header-size+))
            (let* ((payload-length (read-u32 frame 1))
                   (total-length   (+ +header-size+ payload-length)))
              ;; Grow the buffer in-place; header bytes at [0 .. +header-size+) are preserved.
              (adjust-array frame total-length :fill-pointer total-length)
              ;; Phase 2: read payload bytes directly into the tail [+header-size+ .. total-length).
              (when (= total-length
                       (%read-exact frame stream +header-size+ total-length))
                (multiple-value-bind (type payload) (decode-frame frame)
                  (values type payload)))))))
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
