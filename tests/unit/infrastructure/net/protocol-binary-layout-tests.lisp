(in-package #:cl-tmux/test)

;;;; Protocol binary integer, frame layout, and attach payload tests.

(in-suite protocol-suite)

;;; ── Message type tag constant values ─────────────────────────────────────────
;;;
;;; The wire protocol fixes the numeric type tags permanently.  Pinning these
;;; byte values catches defconstant drift at test time instead of integration time.

(test message-type-tag-constants-have-expected-values
  "Message type tag constants are fixed wire-protocol values and must not change."
  (dolist (c `((1 ,+msg-attach+  "+msg-attach+ must equal 1")
               (2 ,+msg-key+     "+msg-key+ must equal 2")
               (3 ,+msg-resize+  "+msg-resize+ must equal 3")
               (4 ,+msg-detach+  "+msg-detach+ must equal 4")
               (5 ,+msg-frame+   "+msg-frame+ must equal 5")
               (6 ,+msg-bye+     "+msg-bye+ must equal 6")
               (7 ,+msg-command+ "+msg-command+ must equal 7")
               (8 ,+msg-reply+   "+msg-reply+ must equal 8")
               (5 ,+header-size+ "+header-size+ must equal 5")))
    (destructuring-bind (expected constant desc) c
      (is (= expected constant) "~A" desc))))

;;; ── u16-octets-pair with max values ─────────────────────────────────────────

(test u16-octets-pair-max-values
  "u16-octets-pair encodes the maximum u16 pair (65535, 65535) as four 0xFF bytes."
  (is (equalp #(255 255 255 255)
              (cl-tmux/protocol:u16-octets-pair 65535 65535))
      "max u16 pair must encode to four 0xFF bytes"))

(test u16-octets-pair-asymmetric-values
  "u16-octets-pair with different row and col values encodes each independently."
  (let ((result (cl-tmux/protocol:u16-octets-pair 1 256)))
    (is (= 4 (length result)) "u16-octets-pair must always produce 4 bytes")
    (is (equalp #(0 1 1 0) result)
        "(1, 256) must encode as #(0 1 1 0)")))

;;; ── Table-driven u16/u32 encoder output correctness ─────────────────────────
;;;
;;; The same encode-byte pattern repeats across u16 and u32 with many values.
;;; A single table-driven test makes each case visible and avoids repetition.

(test u16-octets-table-driven-encoding
  "u16-octets encodes each value to the exact expected big-endian byte sequence."
  (dolist (entry '((0      #(0 0))
                   (1      #(0 1))
                   (127    #(0 127))
                   (128    #(0 128))
                   (255    #(0 255))
                   (256    #(1 0))
                   (512    #(2 0))
                   (32767  #(127 255))
                   (32768  #(128 0))
                   (65534  #(255 254))
                   (65535  #(255 255))))
    (destructuring-bind (n expected) entry
      (is (equalp expected (cl-tmux/protocol:u16-octets n))
          "u16-octets(~D) must equal ~S" n expected))))

(test u32-octets-table-driven-encoding
  "u32-octets encodes each value to the exact expected big-endian byte sequence."
  (dolist (entry '((0          #(0 0 0 0))
                   (1          #(0 0 0 1))
                   (255        #(0 0 0 255))
                   (256        #(0 0 1 0))
                   (65535      #(0 0 255 255))
                   (65536      #(0 1 0 0))
                   (#xFFFFFF   #(0 255 255 255))
                   (#x01000000 #(1 0 0 0))
                   (#xFFFFFFFF #(255 255 255 255))))
    (destructuring-bind (n expected) entry
      (is (equalp expected (cl-tmux/protocol:u32-octets n))
          "u32-octets(~D) must equal ~S" n expected))))

;;; ── encode-frame / decode-frame type byte ────────────────────────────────────

(test encode-frame-type-byte-is-first-byte
  "encode-frame places the type tag in byte 0 of the resulting frame."
  (dolist (entry (list (list +msg-attach+ (cl-tmux/protocol:u16-octets-pair 24 80))
                       (list +msg-key+    #(65 66))
                       (list +msg-detach+ #())))
    (destructuring-bind (type-tag payload) entry
      (let ((frame (encode-frame type-tag payload)))
        (is (= type-tag (aref frame 0))
            "type tag ~D must be at byte index 0" type-tag)))))

;;; ── decode-frame with explicit end=start (zero bytes available) ──────────────

(test decode-frame-zero-bytes-available-returns-nil
  "decode-frame with end equal to start (zero available bytes) returns NIL."
  (let ((frame (msg-key #(1 2 3))))
    (multiple-value-bind (type payload next)
        (decode-frame frame 0 0)
      (is (null type)    "type must be NIL when zero bytes available")
      (is (null payload) "payload must be NIL when zero bytes available")
      (is (= 0 next)     "start index must be returned unchanged"))))

;;; ── Pinning tests for wire-protocol constants ────────────────────────────────

(test field-delimiter-constant-is-ascii-nul
  "+field-delimiter+ must equal 0 (ASCII NUL), the byte used to separate
   NUL-delimited fields in a +msg-command+ payload.  Pinning prevents silent
   drift if the constant is ever accidentally edited."
  (is (= 0 cl-tmux/protocol:+field-delimiter+)
      "+field-delimiter+ must be 0 (ASCII NUL)"))

(test attach-flag-read-only-constant-value-is-bit-zero
  "+attach-flag-read-only+ must equal 1 (bit 0 of the flags byte).  This pins
   the bit position so that encode/decode of the read-only flag remain
   compatible with the tmux CLIENT_READONLY wire convention."
  (is (= 1 cl-tmux/protocol:+attach-flag-read-only+)
      "+attach-flag-read-only+ must equal 1 (bit 0)")
  (is (= 1 (logcount cl-tmux/protocol:+attach-flag-read-only+))
      "+attach-flag-read-only+ must have exactly one bit set"))

(test frame-layout-offset-constants-are-consistent
  "The frame-layout constants must be mutually consistent:
     +payload-length-offset+ (1) + 4 bytes == +header-size+ (5)
     +attach-flags-offset+ must equal 4 (rows,cols occupy 4 bytes)
     +cols-offset-in-size-payload+ must equal 2 (after the 2-byte rows u16)"
  (is (= +header-size+
         (+ cl-tmux/protocol:+payload-length-offset+ 4))
      "+payload-length-offset+ + 4 must equal +header-size+")
  (is (= 4 cl-tmux/protocol:+attach-flags-offset+)
      "+attach-flags-offset+ must equal 4 (rows,cols occupy 4 bytes)")
  (is (= 2 cl-tmux/protocol:+cols-offset-in-size-payload+)
      "+cols-offset-in-size-payload+ must equal 2"))

;;; ── decode-frame start/end window narrowing ──────────────────────────────────

(test decode-frame-with-nonzero-start-and-matching-end
  "decode-frame with start > 0 parses correctly when the window [start, end)
   is exactly the size of one frame."
  ;; Put a detach frame at offset 5 in a larger buffer.
  (let* ((prefix  (make-array 5 :element-type '(unsigned-byte 8) :initial-element 0))
         (frame   (msg-detach))
         (buffer  (concatenate '(simple-array (unsigned-byte 8) (*)) prefix frame)))
    (multiple-value-bind (type payload next)
        (decode-frame buffer 5 (length buffer))
      (is (= +msg-detach+ type)
          "type must be +msg-detach+ when decoded at offset 5")
      (is (= 0 (length payload))
          "detach frame carries no payload")
      (is (= (length buffer) next)
          "next index must equal total buffer length after consuming the frame"))))

(test decode-frame-start-equals-end-returns-nil
  "decode-frame with start = end (window of zero bytes) returns (values NIL NIL start)
   regardless of what the buffer contains before or after start."
  (let ((frame (msg-resize 10 20)))
    (multiple-value-bind (type payload next)
        (decode-frame frame 3 3)
      (is (null type)    "type must be NIL for a zero-byte window")
      (is (null payload) "payload must be NIL for a zero-byte window")
      (is (= 3 next)     "start index must be returned unchanged"))))

;;; ── msg-attach with large u16 boundary values ────────────────────────────────

(test msg-attach-max-u16-rows-cols-roundtrip
  "msg-attach with the maximum u16 values (65535 × 65535) round-trips via
   decode-frame + decode-size without truncation or overflow."
  (multiple-value-bind (type payload)
      (decode-frame (msg-attach 65535 65535))
    (is (= +msg-attach+ type))
    (multiple-value-bind (rows cols) (decode-size payload)
      (is (= 65535 rows) "rows must survive the max-u16 round-trip")
      (is (= 65535 cols) "cols must survive the max-u16 round-trip"))))

;;; ── decode-attach-flags with various payload lengths ─────────────────────────

(test decode-attach-flags-exactly-four-bytes-returns-zero
  "decode-attach-flags on a 4-byte payload (no flags byte) returns 0."
  (let ((payload (cl-tmux/protocol:u16-octets-pair 24 80)))
    (is (= 0 (decode-attach-flags payload))
        "4-byte payload must decode flags as 0")))

(test decode-attach-flags-five-bytes-returns-byte-value
  "decode-attach-flags on a 5-byte payload returns byte 4."
  (let* ((size-bytes (cl-tmux/protocol:u16-octets-pair 10 20))
         (payload    (concatenate '(simple-array (unsigned-byte 8) (*))
                                  size-bytes #(42))))
    (is (= 42 (decode-attach-flags payload))
        "5-byte payload must return the fifth byte as the flags value")))

