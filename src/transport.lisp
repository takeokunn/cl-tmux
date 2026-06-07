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
   A return value less than (- END START) indicates EOF or a short-read at the
   stream boundary.  This is a thin wrapper over READ-SEQUENCE rather than an
   inline call so that the two phases of READ-FRAME can each be named and tested
   independently: the header phase and the payload phase are distinct contracts."
  (read-sequence buffer stream :start start :end end))

;;; ── CPS read-frame state machine ────────────────────────────────────────────
;;;
;;; read-frame is expressed as two CPS continuation steps matching the terminal
;;; parser convention: each step is (data stream k) → next-state-result.
;;; read-header-k reads the 5-byte header and, on success, continues to
;;; read-payload-k.  read-payload-k reads the payload and, on success, decodes
;;; the complete frame.  Both steps return NIL on EOF or short-read.

(defun %read-header-k (stream continuation)
  "Phase 1: read a 5-byte frame header from STREAM into a fresh adjustable buffer.
   On success, call CONTINUATION with the buffer and the decoded payload length.
   Returns NIL when the stream ends before a complete header is available."
  (let ((buffer (make-array +header-size+
                             :element-type '(unsigned-byte 8)
                             :adjustable t
                             :fill-pointer +header-size+)))
    (when (= +header-size+ (%read-exact buffer stream 0 +header-size+))
      (let ((payload-length (read-u32 buffer 1)))
        (funcall continuation buffer payload-length)))))

(defun %read-payload-k (buffer payload-length stream continuation)
  "Phase 2: grow BUFFER to fit PAYLOAD-LENGTH bytes, read the payload from STREAM
   directly into the tail, then call CONTINUATION with the completed buffer.
   Returns NIL when the stream ends before all payload bytes have arrived."
  (let ((total-length (+ +header-size+ payload-length)))
    (adjust-array buffer total-length :fill-pointer total-length)
    (when (= total-length (%read-exact buffer stream +header-size+ total-length))
      (funcall continuation buffer))))

(defun read-frame (stream)
  "Read one complete frame from binary STREAM.
   Returns (values TYPE PAYLOAD), or NIL at end of stream (peer closed,
   mid-frame EOF, or timeout expiry after +read-frame-timeout-seconds+).
   Uses a two-phase CPS read into a single adjustable buffer: %read-header-k
   reads the header (Phase 1), %read-payload-k grows the buffer and reads the
   payload into the tail (Phase 2).  No intermediate header array is allocated
   and no REPLACE copy is needed.
   A single sb-ext:with-timeout wraps both phases so the total wall-clock
   budget is at most +read-frame-timeout-seconds+ seconds."
  (handler-case
      (sb-ext:with-timeout +read-frame-timeout-seconds+
        (%read-header-k stream
          (lambda (buffer payload-length)
            (%read-payload-k buffer payload-length stream
              (lambda (complete-buffer)
                (multiple-value-bind (type payload) (decode-frame complete-buffer)
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
