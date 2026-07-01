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

(defconstant +send-frame-timeout-seconds+ 30
  "Maximum seconds to wait for write-sequence and finish-output to complete
   before aborting.  Mirrors +read-frame-timeout-seconds+ so send-frame is
   self-contained: it does not rely solely on the caller's stream having been
   constructed with its own timeout (e.g. cl-tmux/net:socket-stream).")

(defconstant +max-frame-payload-bytes+ (* 64 1024 1024)
  "Maximum payload size (64 MiB) accepted by read-frame before rejecting.
   Prevents a malicious or buggy peer from triggering unbounded heap allocation.")

(defun send-frame (stream frame)
  "Write FRAME (an octet vector produced by the cl-tmux/protocol msg-* helpers)
   to binary STREAM and flush it, within +send-frame-timeout-seconds+.
   Signals SB-EXT:TIMEOUT when the peer is too slow to accept the write."
  (%validate-outgoing-frame frame)
  (sb-ext:with-timeout +send-frame-timeout-seconds+
    (write-sequence frame stream)
    (finish-output stream)))

(defun %read-exact (buffer stream start end)
  "Fill BUFFER[START..END) from STREAM; return the actual number of bytes read.
   A return value less than (- END START) indicates EOF or a short-read at the
   stream boundary.  This is a thin wrapper over READ-SEQUENCE rather than an
   inline call so that the two phases of READ-FRAME can each be named and tested
   independently: the header phase and the payload phase are distinct contracts."
  (read-sequence buffer stream :start start :end end))

(defun %payload-length-acceptable-p (payload-length)
  "Return true when PAYLOAD-LENGTH is a valid declared payload size.
   The contract: PAYLOAD-LENGTH must be a non-negative integer no greater than
   +max-frame-payload-bytes+.  This check is security-relevant: it prevents a
   malicious or buggy peer from causing unbounded heap growth by sending a frame
   header that declares an astronomically large payload length before any bytes
   of payload are read."
  (and (integerp payload-length)
       (<= 0 payload-length +max-frame-payload-bytes+)))

(defun %validate-outgoing-frame (frame)
  "Validate FRAME (an octet vector) before writing it to a stream.
   Three invariants are enforced:
     1. FRAME must be a vector at least +header-size+ bytes long.
     2. Payload length declared in the header must not exceed
        +max-frame-payload-bytes+.
     3. Total byte count must equal +header-size+ plus declared payload length
        (the frame is self-consistent: no trailing garbage, no truncation).
   Signals an error when any invariant is violated."
  (unless (and (vectorp frame) (>= (length frame) +header-size+))
    (error "Invalid frame: must be a vector of at least ~D bytes, got ~S"
           +header-size+ frame))
  (let* ((payload-length  (read-u32 frame +payload-length-offset+))
         (expected-total  (+ +header-size+ payload-length)))
    (unless (%payload-length-acceptable-p payload-length)
      (error "Invalid frame: declared payload length ~D exceeds +max-frame-payload-bytes+ (~D)"
             payload-length +max-frame-payload-bytes+))
    (unless (= (length frame) expected-total)
      (error "Invalid frame: total length ~D does not match header+payload (~D + ~D = ~D)"
             (length frame) +header-size+ payload-length expected-total))))

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
      (let ((payload-length (read-u32 buffer +payload-length-offset+)))
        (when (%payload-length-acceptable-p payload-length)
          (funcall continuation buffer payload-length))))))

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
