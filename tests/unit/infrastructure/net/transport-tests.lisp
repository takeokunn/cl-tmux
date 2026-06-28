(in-package #:cl-tmux/test)

;;;; Frame transport tests (src/transport.lisp).
;;;;
;;;; send-frame / read-frame move protocol frames across a binary stream.  We
;;;; exercise them over a temp-file octet stream (no socket needed): write a
;;;; sequence of frames, then read them back and assert type/payload and clean
;;;; end-of-stream handling.  A binary file stream behaves like a socket stream
;;;; for these purposes (blocking read-sequence).
;;;;
;;;; with-temp-octet-file and write-frames-to-file are defined in tests/helpers-b.lisp
;;;; and shared with net-tests.lisp to avoid duplicating the temp-file idiom.

(def-suite transport-suite :description "Frame transport over a binary stream")
(in-suite transport-suite)

(test transport-roundtrips-a-frame
  "A single frame written with send-frame reads back intact via read-frame."
  (with-temp-octet-file (path)
    (write-frames-to-file path (msg-resize 24 80))
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (multiple-value-bind (type payload) (read-frame in)
        (is (= +msg-resize+ type))
        (multiple-value-bind (rows cols) (decode-size payload)
          (is (= 24 rows))
          (is (= 80 cols)))))))

(test transport-reads-sequential-frames
  "Several frames in one stream read back one at a time, in order."
  (with-temp-octet-file (path)
    (write-frames-to-file path (msg-key #(65 66)) (msg-detach) (msg-frame "hi あ"))
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (multiple-value-bind (type payload) (read-frame in)
        (is (= +msg-key+ type))
        (is (equalp #(65 66) payload)))
      (multiple-value-bind (type payload) (read-frame in)
        (declare (ignore payload))
        (is (= +msg-detach+ type)))
      (multiple-value-bind (type payload) (read-frame in)
        (is (= +msg-frame+ type))
        (is (string= "hi あ" (decode-text payload)))))))

(test transport-read-at-eof-returns-nil
  "read-frame on an exhausted stream returns NIL (peer closed)."
  (with-temp-octet-file (path)
    (write-frames-to-file path (msg-detach))
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (multiple-value-bind (type payload) (read-frame in)
        (declare (ignore payload))
        (is (= +msg-detach+ type) "first frame reads back"))
      (is (null (read-frame in)) "second read hits EOF → NIL"))))

(test transport-truncated-frame-returns-nil
  "A stream that ends mid-frame (truncated payload) yields NIL, not garbage."
  (with-temp-octet-file (path)
    ;; Write only the first 4 bytes of a key frame (header is 5 bytes).
    (let ((frame (msg-key #(1 2 3))))
      (write-partial-frame-to-file path frame 4))
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (is (null (read-frame in)) "incomplete header → NIL"))))

(test transport-truncated-payload-returns-nil
  "A full 5-byte header but only part of the payload yields NIL (mid-frame)."
  (with-temp-octet-file (path)
    ;; A key frame with a 3-byte payload: 5-byte header + 3 payload = 8 bytes.
    ;; Write the whole header plus only the first payload byte (6 of 8 bytes).
    (let ((frame (msg-key #(1 2 3))))
      (is (= 8 (length frame)) "header(5) + payload(3)")
      (write-partial-frame-to-file path frame 6))
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (is (null (read-frame in)) "complete header, short payload → NIL"))))

(test transport-empty-payload-frame-roundtrips
  "A frame with an empty payload flushes and round-trips intact."
  (with-temp-octet-file (path)
    (write-frames-to-file path (msg-detach))
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (multiple-value-bind (type payload) (read-frame in)
        (is (= +msg-detach+ type))
        (is (zerop (length payload)) "empty payload comes back empty")))))

(test transport-roundtrips-attach-and-bye-frames
  "send-frame then read-frame returns the same type/payload for attach and bye."
  (with-temp-octet-file (path)
    (write-frames-to-file path (msg-attach 30 120) (msg-bye))
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (multiple-value-bind (type payload) (read-frame in)
        (is (= +msg-attach+ type))
        (multiple-value-bind (rows cols) (decode-size payload)
          (is (= 30 rows))
          (is (= 120 cols))))
      (multiple-value-bind (type payload) (read-frame in)
        (is (= +msg-bye+ type))
        (is (zerop (length payload)) "bye carries no payload"))
      (is (null (read-frame in)) "stream exhausted → NIL"))))

;;; ── with-incoming-frame ──────────────────────────────────────────────────────

(test with-incoming-frame-dispatches-on-type
  "with-incoming-frame reads one frame and dispatches on its type constant."
  (with-temp-octet-file (path)
    (write-frames-to-file path (msg-detach))
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (let ((dispatched nil))
        (with-incoming-frame (type payload in)
          ((= type +msg-detach+) (is (zerop (length payload))
                                     "detach carries an empty payload")
                                 (setf dispatched :detach))
          (t (setf dispatched :other)))
        (is (eq :detach dispatched)
            "with-incoming-frame must dispatch to the detach clause")))))

(test with-incoming-frame-binds-payload
  "with-incoming-frame binds the payload variable for the matching clause."
  (with-temp-octet-file (path)
    (write-frames-to-file path (msg-key #(65 66 67)))
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (let ((captured nil))
        (with-incoming-frame (type payload in)
          ((= type +msg-key+) (setf captured payload))
          (t nil))
        (is (equalp #(65 66 67) captured)
            "with-incoming-frame must expose the frame payload in the matching clause")))))

(test with-incoming-frame-eof-falls-through-to-nil-type
  "At EOF, with-incoming-frame delivers NIL type; a nil-check rule can handle it."
  (with-temp-octet-file (path)
    ;; Write nothing — create an empty file so the stream is immediately at EOF.
    (with-open-file (in path :element-type '(unsigned-byte 8)
                             :if-does-not-exist :create)
      (let ((hit-eof nil))
        (with-incoming-frame (type payload in)
          ((null type) (is (null payload)
                           "EOF delivers a NIL payload")
                       (setf hit-eof t))
          (t nil))
        (is-true hit-eof
                 "with-incoming-frame must reach the nil-type clause at EOF")))))

;;; ── Table-driven round-trip: all typed constructors ─────────────────────────

(test transport-all-typed-constructors-roundtrip
  "Every typed message constructor survives a send-frame / read-frame round-trip."
  (assert-round-tripped-frame-type (msg-attach  24 80)           +msg-attach+)
  (assert-round-tripped-frame-type (msg-key     #(27 65))        +msg-key+)
  (assert-round-tripped-frame-type (msg-resize  30 100)          +msg-resize+)
  (assert-round-tripped-frame-type (msg-detach)                  +msg-detach+)
  (assert-round-tripped-frame-type (msg-frame   "hi")            +msg-frame+)
  (assert-round-tripped-frame-type (msg-bye)                     +msg-bye+)
  (assert-round-tripped-frame-type (msg-reply   "output text")   +msg-reply+)
  (assert-round-tripped-frame-type (msg-command :new-window nil nil) +msg-command+))

;;; ── Transport constant values ────────────────────────────────────────────────

(test read-frame-timeout-constant-is-positive-integer
  "The +read-frame-timeout-seconds+ constant must be a positive integer so
   sb-ext:with-timeout receives a valid duration."
  (is (integerp cl-tmux/transport::+read-frame-timeout-seconds+)
      "+read-frame-timeout-seconds+ must be an integer")
  (is (plusp cl-tmux/transport::+read-frame-timeout-seconds+)
      "+read-frame-timeout-seconds+ must be positive"))

(test max-frame-payload-constant-is-large-positive-integer
  "+max-frame-payload-bytes+ must be a large positive integer (≥ 1 MiB) so
   that the security guard in read-frame accepts realistic frame sizes."
  (is (integerp cl-tmux/transport::+max-frame-payload-bytes+)
      "+max-frame-payload-bytes+ must be an integer")
  (is (>= cl-tmux/transport::+max-frame-payload-bytes+ (* 1024 1024))
      "+max-frame-payload-bytes+ must be at least 1 MiB"))

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
    (with-open-file (out path :direction :output :if-exists :supersede
                              :element-type '(unsigned-byte 8))
      (signals error
        (send-frame out (make-array 3 :element-type '(unsigned-byte 8)))
        "send-frame with a 3-byte frame (shorter than 5-byte header) must signal"))))

(test send-frame-rejects-length-field-mismatch
  "%validate-outgoing-frame's third validation clause: total frame length must
   equal +header-size+ plus the declared payload length.  When these disagree
   (declared-length > actual payload, or actual payload > declared-length),
   send-frame must signal an error before any bytes reach the stream."
  (with-temp-octet-file (path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :element-type '(unsigned-byte 8))
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
    (with-open-file (out path :direction :output :if-exists :supersede
                              :element-type '(unsigned-byte 8))
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
    (with-open-file (out path :direction :output :if-exists :supersede
                              :element-type '(unsigned-byte 8))
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
    (with-open-file (out path :direction :output :if-exists :supersede
                              :element-type '(unsigned-byte 8))
      (let ((header (make-array +header-size+ :element-type '(unsigned-byte 8)
                                              :initial-element 0)))
        (setf (aref header 0) +msg-frame+)
        (replace header (cl-tmux/protocol:u32-octets #xFFFFFFFF) :start1 1)
        (write-sequence header out)))
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (is (null (read-frame in))
          "read-frame must return NIL when the declared length is #xFFFFFFFF"))))

;;; ── msg-command frame transport round-trip ───────────────────────────────────

(test transport-msg-command-frame-roundtrips
  "A msg-command frame (command name, target, args) survives a send-frame / read-frame cycle."
  (with-temp-octet-file (path)
    (write-frames-to-file path (msg-command :new-session "$0" '("-d" "-s" "main")))
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (multiple-value-bind (type payload) (read-frame in)
        (is (= +msg-command+ type)
            "read-frame must return +msg-command+ type")
        (multiple-value-bind (command target args)
            (cl-tmux/protocol:decode-command-payload payload)
          (is (eq :new-session command)
              "command keyword must survive transport round-trip")
          (is (string= "$0" target)
              "target string must survive transport round-trip")
          (is (equal '("-d" "-s" "main") args)
              "args list must survive transport round-trip"))))))

;;; ── Large payload transport ──────────────────────────────────────────────────

(test transport-large-payload-roundtrips
  "A frame with a 65536-byte payload survives a send-frame / read-frame cycle intact."
  (let* ((n       65536)
         (payload (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n) (setf (aref payload i) (logand i #xFF)))
    (with-temp-octet-file (path)
      (write-frames-to-file path (encode-frame +msg-frame+ payload))
      (with-open-file (in path :element-type '(unsigned-byte 8))
        (multiple-value-bind (type decoded) (read-frame in)
          (is (= +msg-frame+ type)
              "large payload frame must decode to +msg-frame+ type")
          (is (= n (length decoded))
              "decoded payload length must equal original length")
          (is (equalp payload decoded)
              "large payload must survive transport round-trip byte-for-byte"))))))

;;; ── with-incoming-frame with multiple clauses ────────────────────────────────

(test with-incoming-frame-first-matching-clause-wins
  "with-incoming-frame evaluates the first matching clause and skips the rest."
  (with-temp-octet-file (path)
    (write-frames-to-file path (msg-resize 10 20))
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (let ((result nil))
        (with-incoming-frame (type payload in)
          ((= type +msg-resize+)
           (multiple-value-bind (rows cols)
               (cl-tmux/protocol:decode-size payload)
             (setf result (list :resize rows cols))))
          ((= type +msg-key+)   (setf result :key))
          (t                    (setf result :other)))
        (is (equal '(:resize 10 20) result)
            "resize clause must fire and decode rows/cols correctly")))))

;;; ── send-frame writes octet vector without signalling ─────────────────────────

(test send-frame-finishes-without-signalling
  "send-frame on an open binary output stream must not signal."
  (with-temp-octet-file (path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :element-type '(unsigned-byte 8))
      (finishes (send-frame out (msg-detach))
                "send-frame must not signal on a valid binary output stream"))))

;;; ── Table-driven: all constructors preserve payload ─────────────────────────
;;;
;;; Same frame-type loop as transport-all-typed-constructors-roundtrip above,
;;; but this table also verifies the payload round-trip for constructors whose
;;; payload is non-empty (attach, key, resize, frame).

(test transport-typed-constructors-payload-roundtrip
  "Each typed constructor's payload survives send-frame / read-frame intact."
  ;; msg-attach: decode-size must recover rows and cols
  (assert-round-tripped-frame-payload
   (msg-attach 15 60)
   (lambda (payload)
     (multiple-value-bind (rows cols) (cl-tmux/protocol:decode-size payload)
       (is (= 15 rows) "attach rows must survive transport")
       (is (= 60 cols) "attach cols must survive transport"))))
  ;; msg-key: payload bytes must be preserved verbatim
  (assert-round-tripped-frame-payload
   (msg-key #(27 91 65))
   (lambda (payload)
     (is (equalp #(27 91 65) payload)
         "key payload bytes must survive transport")))
  ;; msg-resize: decode-size must recover rows and cols
  (assert-round-tripped-frame-payload
   (msg-resize 50 200)
   (lambda (payload)
     (multiple-value-bind (rows cols) (cl-tmux/protocol:decode-size payload)
       (is (= 50 rows)  "resize rows must survive transport")
       (is (= 200 cols) "resize cols must survive transport"))))
  ;; msg-frame: decode-text must recover the Unicode string
  (assert-round-tripped-frame-payload
   (msg-frame "こんにちは")
   (lambda (payload)
     (is (string= "こんにちは" (cl-tmux/protocol:decode-text payload))
         "frame UTF-8 payload must survive transport")))
  ;; msg-reply: decode-text must recover the UTF-8 reply text
  (assert-round-tripped-frame-payload
   (msg-reply "output: 42")
   (lambda (payload)
     (is (string= "output: 42" (cl-tmux/protocol:decode-text payload))
         "reply UTF-8 payload must survive transport")))
  ;; msg-command: decode-command-payload must recover command, target, and args
  (assert-round-tripped-frame-payload
   (msg-command :new-window "$0" '("-d"))
   (lambda (payload)
     (multiple-value-bind (command target args)
         (cl-tmux/protocol:decode-command-payload payload)
       (is (eq :new-window command) "command keyword must survive transport")
       (is (string= "$0" target)    "target string must survive transport")
       (is (equal '("-d") args)     "args list must survive transport")))))

;;; ── %read-exact direct contract tests ───────────────────────────────────────
;;;
;;; These tests exercise the %read-exact helper directly using a file stream so
;;; that the exact-byte-count contract is verified in isolation from read-frame.
;;; The key invariant: %read-exact returns the actual number of bytes read, which
;;; equals (- end start) on a normal read but is less than that at EOF.

(test read-exact-fills-buffer-exactly
  "%read-exact reads exactly (- end start) bytes from a stream with enough data."
  (with-temp-octet-file (path)
    ;; Write 10 known bytes to the file.
    (with-open-file (out path :direction :output :if-exists :supersede
                              :element-type '(unsigned-byte 8))
      (write-sequence #(10 20 30 40 50 60 70 80 90 100) out))
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (let ((buffer (make-array 10 :element-type '(unsigned-byte 8))))
        (let ((bytes-read (cl-tmux/transport::%read-exact buffer in 0 10)))
          (is (= 10 bytes-read)
              "%read-exact must return 10 when 10 bytes are available")
          (is (equalp #(10 20 30 40 50 60 70 80 90 100) buffer)
              "%read-exact must place the bytes at the correct positions"))))))

(test read-exact-returns-short-count-at-eof
  "%read-exact returns less than requested when the stream ends before END."
  (with-temp-octet-file (path)
    ;; Write only 3 bytes but ask for 10.
    (with-open-file (out path :direction :output :if-exists :supersede
                              :element-type '(unsigned-byte 8))
      (write-sequence #(1 2 3) out))
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (let ((buffer (make-array 10 :element-type '(unsigned-byte 8) :initial-element 0)))
        (let ((bytes-read (cl-tmux/transport::%read-exact buffer in 0 10)))
          (is (< bytes-read 10)
              "%read-exact must return less than 10 at EOF (only 3 bytes available)")
          (is (= 3 bytes-read)
              "%read-exact must return the actual byte count (3) at EOF"))))))

(test read-exact-respects-start-offset
  "%read-exact writes bytes starting at the given START offset in the buffer."
  (with-temp-octet-file (path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :element-type '(unsigned-byte 8))
      (write-sequence #(7 8 9) out))
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (let ((buffer (make-array 6 :element-type '(unsigned-byte 8) :initial-element 0)))
        ;; Read into the middle of the buffer (positions 3..6).
        (cl-tmux/transport::%read-exact buffer in 3 6)
        (is (= 0 (aref buffer 0)) "byte 0 must be untouched")
        (is (= 0 (aref buffer 1)) "byte 1 must be untouched")
        (is (= 0 (aref buffer 2)) "byte 2 must be untouched")
        (is (= 7 (aref buffer 3)) "byte 3 must hold first read byte (7)")
        (is (= 8 (aref buffer 4)) "byte 4 must hold second read byte (8)")
        (is (= 9 (aref buffer 5)) "byte 5 must hold third read byte (9)")))))
