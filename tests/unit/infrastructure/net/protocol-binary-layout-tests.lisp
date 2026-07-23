(in-package #:cl-tmux/test)

;;;; Protocol binary integer, frame layout, and attach payload tests.

(describe "protocol-suite"

  ;;; ── Message type tag constant values ─────────────────────────────────────────
  ;;;
  ;;; The wire protocol fixes the numeric type tags permanently.  Pinning these
  ;;; byte values catches defconstant drift at test time instead of integration time.

  ;; Message type tag constants are fixed wire-protocol values and must not change.
  (it "message-type-tag-constants-have-expected-values"
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
        (declare (ignore desc))
        (expect (= expected constant)))))

  ;;; ── u16-octets-pair with max values ─────────────────────────────────────────

  ;; u16-octets-pair encodes the maximum u16 pair (65535, 65535) as four 0xFF bytes.
  (it "u16-octets-pair-max-values"
    (expect (equalp #(255 255 255 255)
                (cl-tmux/protocol:u16-octets-pair 65535 65535))))

  ;; u16-octets-pair with different row and col values encodes each independently.
  (it "u16-octets-pair-asymmetric-values"
    (let ((result (cl-tmux/protocol:u16-octets-pair 1 256)))
      (expect (= 4 (length result)))
      (expect (equalp #(0 1 1 0) result))))

  ;;; ── Table-driven u16/u32 encoder output correctness ─────────────────────────
  ;;;
  ;;; The same encode-byte pattern repeats across u16 and u32 with many values.
  ;;; A single table-driven test makes each case visible and avoids repetition.

  ;; u16-octets encodes each value to the exact expected big-endian byte sequence.
  (it "u16-octets-table-driven-encoding"
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
        (expect (equalp expected (cl-tmux/protocol:u16-octets n))))))

  ;; u32-octets encodes each value to the exact expected big-endian byte sequence.
  (it "u32-octets-table-driven-encoding"
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
        (expect (equalp expected (cl-tmux/protocol:u32-octets n))))))

  ;;; ── encode-frame / decode-frame type byte ────────────────────────────────────

  ;; encode-frame places the type tag in byte 0 of the resulting frame.
  (it "encode-frame-type-byte-is-first-byte"
    (dolist (entry (list (list +msg-attach+ (cl-tmux/protocol:u16-octets-pair 24 80))
                         (list +msg-key+    #(65 66))
                         (list +msg-detach+ #())))
      (destructuring-bind (type-tag payload) entry
        (let ((frame (encode-frame type-tag payload)))
          (expect (= type-tag (aref frame 0)))))))

  ;;; ── decode-frame with explicit end=start (zero bytes available) ──────────────

  ;; decode-frame with end equal to start (zero available bytes) returns NIL.
  (it "decode-frame-zero-bytes-available-returns-nil"
    (let ((frame (msg-key #(1 2 3))))
      (multiple-value-bind (type payload next)
          (decode-frame frame 0 0)
        (expect (null type))
        (expect (null payload))
        (expect (= 0 next)))))

  ;;; ── Pinning tests for wire-protocol constants ────────────────────────────────

  ;; +field-delimiter+ must equal 0 (ASCII NUL), the byte used to separate
  ;; NUL-delimited fields in a +msg-command+ payload.  Pinning prevents silent
  ;; drift if the constant is ever accidentally edited.
  (it "field-delimiter-constant-is-ascii-nul"
    (expect (= 0 cl-tmux/protocol:+field-delimiter+)))

  ;; +attach-flag-read-only+ must equal 1 (bit 0 of the flags byte).  This pins
  ;; the bit position so that encode/decode of the read-only flag follow
  ;; the tmux CLIENT_READONLY wire convention.
  (it "attach-flag-read-only-constant-value-is-bit-zero"
    (expect (= 1 cl-tmux/protocol:+attach-flag-read-only+))
    (expect (= 1 (logcount cl-tmux/protocol:+attach-flag-read-only+))))

  ;; The frame-layout constants must be mutually consistent:
  ;;   +payload-length-offset+ (1) + 4 bytes == +header-size+ (5)
  ;;   +attach-flags-offset+ must equal 4 (rows,cols occupy 4 bytes)
  ;;   +cols-offset-in-size-payload+ must equal 2 (after the 2-byte rows u16)
  (it "frame-layout-offset-constants-are-consistent"
    (expect (= +header-size+
           (+ cl-tmux/protocol:+payload-length-offset+ 4)))
    (expect (= 4 cl-tmux/protocol:+attach-flags-offset+))
    (expect (= 2 cl-tmux/protocol:+cols-offset-in-size-payload+)))

  ;;; ── decode-frame start/end window narrowing ──────────────────────────────────

  ;; decode-frame with start > 0 parses correctly when the window [start, end)
  ;; is exactly the size of one frame.
  (it "decode-frame-with-nonzero-start-and-matching-end"
    ;; Put a detach frame at offset 5 in a larger buffer.
    (let* ((prefix  (make-array 5 :element-type '(unsigned-byte 8) :initial-element 0))
           (frame   (msg-detach))
           (buffer  (concatenate '(simple-array (unsigned-byte 8) (*)) prefix frame)))
      (multiple-value-bind (type payload next)
          (decode-frame buffer 5 (length buffer))
        (expect (= +msg-detach+ type))
        (expect (= 0 (length payload)))
        (expect (= (length buffer) next)))))

  ;; decode-frame with start = end (window of zero bytes) returns (values NIL NIL start)
  ;; regardless of what the buffer contains before or after start.
  (it "decode-frame-start-equals-end-returns-nil"
    (let ((frame (msg-resize 10 20)))
      (multiple-value-bind (type payload next)
          (decode-frame frame 3 3)
        (expect (null type))
        (expect (null payload))
        (expect (= 3 next)))))

  ;;; ── msg-attach with large u16 boundary values ────────────────────────────────

  ;; msg-attach with the maximum u16 values (65535 × 65535) round-trips via
  ;; decode-frame + decode-size without truncation or overflow.
  (it "msg-attach-max-u16-rows-cols-roundtrip"
    (multiple-value-bind (type payload)
        (decode-frame (msg-attach 65535 65535))
      (expect (= +msg-attach+ type))
      (multiple-value-bind (rows cols) (decode-size payload)
        (expect (= 65535 rows))
        (expect (= 65535 cols)))))

  ;;; ── decode-attach-flags with various payload lengths ─────────────────────────

  ;; decode-attach-flags on a 4-byte payload (no flags byte) returns 0.
  (it "decode-attach-flags-exactly-four-bytes-returns-zero"
    (let ((payload (cl-tmux/protocol:u16-octets-pair 24 80)))
      (expect (= 0 (decode-attach-flags payload)))))

  ;; decode-attach-flags on a 5-byte payload returns byte 4.
  (it "decode-attach-flags-five-bytes-returns-byte-value"
    (let* ((size-bytes (cl-tmux/protocol:u16-octets-pair 10 20))
           (payload    (concatenate '(simple-array (unsigned-byte 8) (*))
                                    size-bytes #(42))))
      (expect (= 42 (decode-attach-flags payload))))))
