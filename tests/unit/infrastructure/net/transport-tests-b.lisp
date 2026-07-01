(in-package #:cl-tmux/test)

;;;; Frame transport tests (src/transport.lisp) — part II: validation,
;;;; security boundaries, and direct CPS-phase coverage.
;;;;
;;;; %validate-outgoing-frame / %payload-length-acceptable-p guard against
;;;; malformed or malicious frames on both the write side (send-frame) and the
;;;; read side (read-frame's %read-header-k / %read-payload-k phases).  These
;;;; tests pin those boundaries directly, independent of the happy-path
;;;; round-trips covered in transport-tests.lisp.

(in-suite transport-suite)

;;; ── Shared frame-construction helper ────────────────────────────────────────
;;;
;;; %make-frame-with-declared-length covers both the mismatch case (declared ≠
;;; actual) and the oversized case (declared > +max-frame-payload-bytes+).
;;; Having one constructor removes the two one-off flets that previously
;;; duplicated the exact same byte-patching pattern.

(defun %make-frame-with-declared-length (declared-payload-length actual-payload-bytes)
  "Build a frame whose 4-byte length field claims DECLARED-PAYLOAD-LENGTH bytes
   but whose actual body contains ACTUAL-PAYLOAD-BYTES bytes of (zero-filled) payload.
   Byte 0 carries the +msg-frame+ type tag; bytes 1-4 are the overwritten length
   field; the remainder is zeroed payload.
   Used to construct both mismatched-length frames (declared != actual) and
   oversized-declared frames (declared > +max-frame-payload-bytes+)."
  (let* ((total (+ +header-size+ actual-payload-bytes))
         (frame (make-array total :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref frame 0) +msg-frame+)
    (replace frame (cl-tmux/protocol:u32-octets declared-payload-length) :start1 1)
    frame))

;;; ── %validate-outgoing-frame and send-frame validation ──────────────────────

(test send-frame-rejects-too-short-frames
  "send-frame must signal an error when the frame vector is shorter than
   +header-size+ (not a valid frame at all)."
  (with-temp-octet-file (path)
    (with-output-octet-stream (out path)
      (signals error
        (send-frame out (make-array 3 :element-type '(unsigned-byte 8)))
        "send-frame with a 3-byte frame (shorter than 5-byte header) must signal"))))

(test send-frame-rejects-length-field-mismatch
  "%validate-outgoing-frame's third validation clause: total frame length must
   equal +header-size+ plus the declared payload length.  When these disagree
   (declared-length > actual payload, or actual payload > declared-length),
   send-frame must signal an error before any bytes reach the stream."
  (with-temp-octet-file (path)
    (with-output-octet-stream (out path)
      (signals error
        (send-frame out (%make-frame-with-declared-length 10 0))
        "declared=10 actual=0 must signal (header claims more bytes than present)")
      (signals error
        (send-frame out (%make-frame-with-declared-length 1 5))
        "declared=1 actual=5 must signal (header claims fewer bytes than present)"))))

(test send-frame-rejects-oversized-declared-payload-length
  "%validate-outgoing-frame's second validation clause: a frame whose declared
   payload-length field exceeds +max-frame-payload-bytes+ must be rejected by
   send-frame before any bytes reach the stream.  This guards against a
   malicious or buggy peer that sets the 4-byte length field to a value larger
   than 64 MiB -- even when the frame vector is self-consistent (total length
   does match header + declared payload), the size guard fires first."
  (with-temp-octet-file (path)
    (with-output-octet-stream (out path)
      ;; Declared length is exactly one byte over the allowed ceiling.
      (signals error
        (send-frame out (%make-frame-with-declared-length
                         (1+ cl-tmux/transport::+max-frame-payload-bytes+)
                         0))
        "declared length = max+1 must signal (exceeds +max-frame-payload-bytes+)")
      ;; Declared length is the maximum u32 value -- an extreme case.
      (signals error
        (send-frame out (%make-frame-with-declared-length #xFFFFFFFF 0))
        "declared length = 0xFFFFFFFF must signal (far exceeds +max-frame-payload-bytes+)"))))

;;; ── %payload-length-acceptable-p direct coverage ─────────────────────────────
;;;
;;; This predicate is security-critical: it guards read-frame against a malicious
;;; peer that injects a huge declared payload length in the 4-byte header field.
;;; Testing it directly pins the exact security boundary independently of
;;; send-frame/%validate-outgoing-frame.

(test payload-length-acceptable-p-accepts-valid-lengths
  "%payload-length-acceptable-p must return true for 0 and for the exact ceiling
   +max-frame-payload-bytes+, and for a mid-range value."
  (is-true  (cl-tmux/transport::%payload-length-acceptable-p 0)
            "0-byte payload must be acceptable")
  (is-true  (cl-tmux/transport::%payload-length-acceptable-p 1)
            "1-byte payload must be acceptable")
  (is-true  (cl-tmux/transport::%payload-length-acceptable-p
             cl-tmux/transport::+max-frame-payload-bytes+)
            "exactly +max-frame-payload-bytes+ must be acceptable (ceiling is inclusive)"))

(test payload-length-acceptable-p-rejects-oversized-lengths
  "%payload-length-acceptable-p must return NIL for values that exceed the ceiling,
   for negative values, and for the maximum u32 value (0xFFFFFFFF).
   These cases represent the security boundary: if any of them returned true,
   read-frame would attempt to allocate the declared buffer before the peer
   has sent any payload bytes."
  (is-false (cl-tmux/transport::%payload-length-acceptable-p -1)
            "negative payload length must be rejected")
  (is-false (cl-tmux/transport::%payload-length-acceptable-p
             (1+ cl-tmux/transport::+max-frame-payload-bytes+))
            "+max-frame-payload-bytes+ + 1 must be rejected (one over the ceiling)")
  (is-false (cl-tmux/transport::%payload-length-acceptable-p #xFFFFFFFF)
            "#xFFFFFFFF must be rejected (far exceeds the ceiling)"))

;;; ── read-frame rejects oversized declared payload-length in stream ──────────
;;;
;;; These tests cover the read path that the send-frame tests do NOT cover:
;;; a malicious remote peer injects a 5-byte header whose length field exceeds
;;; +max-frame-payload-bytes+.  read-frame must return NIL (not attempt to
;;; allocate the oversized buffer declared by the header).

(test read-frame-rejects-oversized-declared-payload-in-stream
  "A stream whose 5-byte header declares a payload length exceeding
   +max-frame-payload-bytes+ must cause read-frame to return NIL rather than
   attempting to allocate the oversized buffer.  This covers the read-side
   security guard that is distinct from the send-frame/%validate-outgoing-frame
   write-side guard already tested by send-frame-rejects-oversized-declared-payload-length."
  (with-temp-octet-file (path)
    ;; Write a 5-byte header whose length field is max+1 (one byte over the ceiling).
    ;; No payload bytes follow -- a real attacker would stop here.
    (with-output-octet-stream (out path)
      (let* ((oversized-length (1+ cl-tmux/transport::+max-frame-payload-bytes+))
             (header (make-array +header-size+ :element-type '(unsigned-byte 8)
                                               :initial-element 0)))
        (setf (aref header 0) +msg-frame+)
        (replace header (cl-tmux/protocol:u32-octets oversized-length) :start1 1)
        (write-sequence header out)))
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (is (null (read-frame in))
          "read-frame must return NIL when the declared payload length exceeds +max-frame-payload-bytes+"))))

(test read-frame-rejects-max-u32-declared-payload-in-stream
  "A stream whose header declares the maximum u32 payload length (0xFFFFFFFF)
   must cause read-frame to return NIL immediately without attempting any allocation.
   This is the extreme boundary of the security guard."
  (with-temp-octet-file (path)
    (with-output-octet-stream (out path)
      (let ((header (make-array +header-size+ :element-type '(unsigned-byte 8)
                                              :initial-element 0)))
        (setf (aref header 0) +msg-frame+)
        (replace header (cl-tmux/protocol:u32-octets #xFFFFFFFF) :start1 1)
        (write-sequence header out)))
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (is (null (read-frame in))
          "read-frame must return NIL when the declared length is #xFFFFFFFF"))))

;;; ── %validate-outgoing-frame direct coverage ────────────────────────────────
;;;
;;; %validate-outgoing-frame is already exercised indirectly via send-frame, but
;;; testing it directly pins its three validation clauses independently of I/O:
;;;   1. Frame must be a vector of at least +header-size+ bytes.
;;;   2. Declared payload length must not exceed +max-frame-payload-bytes+.
;;;   3. Total frame length must equal +header-size+ + declared payload length.

(test validate-outgoing-frame-accepts-well-formed-frames
  "%validate-outgoing-frame must not signal for well-formed frames produced by
   the msg-* constructors."
  (finishes (cl-tmux/transport::%validate-outgoing-frame (msg-detach))
            "%validate-outgoing-frame must not signal for a valid empty frame")
  (finishes (cl-tmux/transport::%validate-outgoing-frame (msg-key #(1 2 3)))
            "%validate-outgoing-frame must not signal for a valid key frame")
  (finishes (cl-tmux/transport::%validate-outgoing-frame (msg-resize 24 80))
            "%validate-outgoing-frame must not signal for a valid resize frame"))

(test validate-outgoing-frame-rejects-too-short-vector
  "%validate-outgoing-frame signals an error when the frame vector has fewer
   than +header-size+ bytes (not a valid frame at all)."
  (signals error
    (cl-tmux/transport::%validate-outgoing-frame
     (make-array 0 :element-type '(unsigned-byte 8)))
    "empty vector must signal")
  (signals error
    (cl-tmux/transport::%validate-outgoing-frame
     (make-array (1- +header-size+) :element-type '(unsigned-byte 8)))
    "vector shorter than header must signal"))

(test validate-outgoing-frame-rejects-oversized-declared-length
  "%validate-outgoing-frame signals an error when the declared payload length
   in the frame header exceeds +max-frame-payload-bytes+."
  (signals error
    (cl-tmux/transport::%validate-outgoing-frame
     (%make-frame-with-declared-length
      (1+ cl-tmux/transport::+max-frame-payload-bytes+) 0))
    "declared length = max+1 must signal"))

(test validate-outgoing-frame-rejects-self-inconsistent-length
  "%validate-outgoing-frame signals an error when total frame length does not
   equal +header-size+ + declared payload length (self-inconsistent frame)."
  (signals error
    (cl-tmux/transport::%validate-outgoing-frame
     (%make-frame-with-declared-length 5 0))
    "declared=5 actual=0 must signal (header claims more bytes than present)")
  (signals error
    (cl-tmux/transport::%validate-outgoing-frame
     (%make-frame-with-declared-length 0 3))
    "declared=0 actual=3 must signal (header claims fewer bytes than present)"))

;;; ── %read-header-k direct coverage ──────────────────────────────────────────
;;;
;;; %read-header-k reads the 5-byte frame header and, on success, calls its
;;; continuation with (buffer payload-length).  Testing it directly pins the
;;; CPS phase-1 contract independently of the full read-frame pipeline:
;;;   • A complete 5-byte header calls the continuation with the right payload-length.
;;;   • A stream that ends before 5 bytes (EOF/short-read) returns NIL without
;;;     calling the continuation.
;;;   • A header whose declared payload length exceeds +max-frame-payload-bytes+
;;;     returns NIL (security guard fires before the continuation is called).

(test read-header-k-calls-continuation-with-payload-length
  "%read-header-k invokes the continuation with the payload-length from the
   frame header when the stream contains a complete 5-byte header."
  (let ((frame (msg-key #(1 2 3))))   ; 5-byte header + 3-byte payload
    (with-temp-octet-file (path)
      (with-output-octet-stream (out path)
        ;; Write only the 5-byte header (first +header-size+ bytes of frame).
        (write-sequence frame out :end +header-size+))
      (with-open-file (in path :element-type '(unsigned-byte 8))
        (let ((captured-length nil))
          (cl-tmux/transport::%read-header-k
           in
           (lambda (buffer payload-length)
             (declare (ignore buffer))
             (setf captured-length payload-length)))
          (is (= 3 captured-length)
              "%read-header-k must pass payload-length=3 to the continuation"))))))

(test read-header-k-returns-nil-at-eof
  "%read-header-k returns NIL (without calling the continuation) when the
   stream contains fewer than +header-size+ bytes."
  (with-temp-octet-file (path)
    ;; Write only 3 bytes — not a complete header.
    (with-output-octet-stream (out path)
      (write-sequence #(1 2 3) out))
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (let ((called nil))
        (let ((result
               (cl-tmux/transport::%read-header-k
                in
                (lambda (buffer payload-length)
                  (declare (ignore buffer payload-length))
                  (setf called t)
                  :called))))
          (is (null result)
              "%read-header-k must return NIL at EOF before a full header")
          (is (null called)
              "%read-header-k must not call the continuation at EOF"))))))

(test read-header-k-returns-nil-for-oversized-declared-payload
  "%read-header-k returns NIL when the decoded payload length exceeds
   +max-frame-payload-bytes+, enforcing the security guard before the
   continuation is reached."
  (with-temp-octet-file (path)
    ;; Craft a 5-byte header whose length field = max+1.
    (with-output-octet-stream (out path)
      (let* ((oversized (1+ cl-tmux/transport::+max-frame-payload-bytes+))
             (header    (make-array +header-size+ :element-type '(unsigned-byte 8)
                                                  :initial-element 0)))
        (setf (aref header 0) +msg-frame+)
        (replace header (cl-tmux/protocol:u32-octets oversized) :start1 1)
        (write-sequence header out)))
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (let ((called nil))
        (let ((result
               (cl-tmux/transport::%read-header-k
                in
                (lambda (buffer payload-length)
                  (declare (ignore buffer payload-length))
                  (setf called t)
                  :called))))
          (is (null result)
              "%read-header-k must return NIL when declared payload exceeds max")
          (is (null called)
              "%read-header-k must not call the continuation for oversized payload"))))))

;;; ── %read-payload-k direct coverage ─────────────────────────────────────────
;;;
;;; %read-payload-k grows an adjustable header buffer and reads the payload
;;; bytes from the stream directly into the tail, then calls its continuation.
;;; Testing it directly pins the CPS phase-2 contract independently of
;;; %read-header-k: the continuation must receive a complete frame buffer whose
;;; payload bytes match what was written, or NIL when the stream is short.

(test read-payload-k-calls-continuation-with-complete-buffer
  "%read-payload-k reads PAYLOAD-LENGTH bytes and calls the continuation with
   a buffer whose tail contains those bytes verbatim."
  (with-temp-octet-file (path)
    ;; Write 4 payload bytes to the file.
    (with-output-octet-stream (out path)
      (write-sequence #(10 20 30 40) out))
    (with-open-file (in path :element-type '(unsigned-byte 8))
      ;; Construct a 5-element adjustable header buffer (simulating what
      ;; %read-header-k would produce after reading the header).
      (let* ((header-buf (make-array +header-size+ :element-type '(unsigned-byte 8)
                                                   :adjustable t
                                                   :fill-pointer +header-size+))
             (captured nil))
        (cl-tmux/transport::%read-payload-k
         header-buf 4 in
         (lambda (complete-buffer)
           (setf captured (subseq complete-buffer +header-size+))))
        (is (equalp #(10 20 30 40) captured)
            "%read-payload-k must place payload bytes in the tail of the buffer")))))

(test read-payload-k-returns-nil-at-eof
  "%read-payload-k returns NIL (without calling the continuation) when the
   stream ends before all declared payload bytes have arrived."
  (with-temp-octet-file (path)
    ;; Write only 2 bytes but declare 4.
    (with-output-octet-stream (out path)
      (write-sequence #(1 2) out))
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (let* ((header-buf (make-array +header-size+ :element-type '(unsigned-byte 8)
                                                   :adjustable t
                                                   :fill-pointer +header-size+))
             (called nil))
        (let ((result
               (cl-tmux/transport::%read-payload-k
                header-buf 4 in
                (lambda (complete-buffer)
                  (declare (ignore complete-buffer))
                  (setf called t)
                  :called))))
          (is (null result)
              "%read-payload-k must return NIL when stream ends before payload is complete")
          (is (null called)
              "%read-payload-k must not call the continuation when payload is short"))))))
