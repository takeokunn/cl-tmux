(in-package #:cl-tmux/test)

;;;; Client/server wire-protocol codec tests (src/protocol.lisp).
;;;;
;;;; Pure octet-level tests: encode a message, decode it back, and assert the
;;;; type, payload, and consumed-byte count.  Also covers streaming concerns:
;;;; partial buffers (incomplete header / payload) and several frames packed
;;;; back-to-back in one buffer.
;;;;
;;;; All protocol helpers referenced here are exported from cl-tmux/protocol
;;;; so tests use single-colon qualified names (cl-tmux/protocol:name) rather
;;;; than double-colon internal access (cl-tmux/protocol::name).  If a helper
;;;; is renamed or unexported, the compile-time package check will catch it.

(describe "protocol-suite"

  ;;; ── Octet encoding/decoding helpers ─────────────────────────────────────────

  ;; u16-octets encodes a 16-bit value as two big-endian bytes.
  (it "u16-octets-big-endian"
    (dolist (c '((0     #(0 0))
                 (1     #(0 1))
                 (256   #(1 0))
                 (65535 #(255 255))))
      (destructuring-bind (n expected) c
        (expect (equalp expected (cl-tmux/protocol:u16-octets n))))))

  ;; u32-octets encodes a 32-bit value as four big-endian bytes.
  (it "u32-octets-big-endian"
    (dolist (c '((0          #(0 0 0 0))
                 (1          #(0 0 0 1))
                 (65536      #(0 1 0 0))
                 (#xFFFFFFFF #(255 255 255 255))))
      (destructuring-bind (n expected) c
        (expect (equalp expected (cl-tmux/protocol:u32-octets n))))))

  ;; u16-octets-pair concatenates two u16 values as 4 big-endian bytes.
  (it "u16-octets-pair-concatenates-two-u16s"
    (expect (equalp #(0 24 0 80) (cl-tmux/protocol:u16-octets-pair 24 80)))
    (expect (equalp #(0 0 0 0)   (cl-tmux/protocol:u16-octets-pair 0 0))))

  ;; read-u16 reads two bytes at START as a big-endian u16.
  (it "read-u16-decodes-big-endian"
    (let ((buffer (make-array 6 :element-type '(unsigned-byte 8)
                                :initial-contents '(0 0 0 24 0 80))))
      (dolist (c '((0 0  "offset 0 → 0")
                   (2 24 "offset 2 → 24")
                   (4 80 "offset 4 → 80")))
        (destructuring-bind (offset expected desc) c
          (declare (ignore desc))
          (expect (= expected (cl-tmux/protocol:read-u16 buffer offset)))))))

  ;;; ── Helpers ─────────────────────────────────────────────────────────────────

  (defun cat-octets (&rest frames)
    "Concatenate octet vectors into one simple octet buffer (a wire stream)."
    (apply #'concatenate '(simple-array (unsigned-byte 8) (*)) frames))

  ;;; ── Frame header format ─────────────────────────────────────────────────────

  ;; A frame is [type][len u32-be][payload]; header is 5 bytes.
  (it "frame-header-layout"
    (let ((frame (encode-frame +msg-key+ (to-octets #(7 8 9)))))
      (expect (= +header-size+ 5))
      (expect (= +msg-key+ (aref frame 0)))
      ;; length = 3, big-endian in bytes 1..4
      (expect (equalp #(0 0 0 3) (subseq frame 1 5)))
      (expect (= 8 (length frame)))))

  ;;; ── Round-trips per message type ────────────────────────────────────────────

  ;; msg-attach carries rows,cols, decodable via decode-size.
  (it "attach-roundtrip"
    (let ((frame (msg-attach 24 80)))
      (assert-decoded-frame-type frame +msg-attach+)
      (assert-decoded-frame-payload
       frame
       (lambda (payload)
         (multiple-value-bind (rows cols) (decode-size payload)
           (expect (= 24 rows))
           (expect (= 80 cols)))))))

  ;; msg-attach with readonly-p sets the trailing flags byte; decode-attach-flags
  ;; recovers +attach-flag-read-only+ while decode-size still recovers rows,cols.
  (it "attach-readonly-flag-roundtrip"
    (multiple-value-bind (type payload) (decode-frame (msg-attach 24 80 t))
      (expect (= +msg-attach+ type))
      (multiple-value-bind (rows cols) (decode-size payload)
        (expect (= 24 rows))
        (expect (= 80 cols)))
      (expect (logtest (decode-attach-flags payload) +attach-flag-read-only+))))

  ;; A 2-arg msg-attach omits the flags byte; decode-attach-flags returns 0 so
  ;; read-only defaults off when the frame omits CLIENT_READONLY.
  (it "attach-no-flag-decodes-zero"
    (multiple-value-bind (type payload) (decode-frame (msg-attach 24 80))
      (declare (ignore type))
      (expect (= 4 (length payload)))
      (expect (= 0 (decode-attach-flags payload)))
      (expect (not (logtest (decode-attach-flags payload) +attach-flag-read-only+)))))

  ;; msg-resize round-trips rows,cols including large values.
  (it "resize-roundtrip"
    (let ((frame (msg-resize 300 1000)))
      (assert-decoded-frame-type frame +msg-resize+)
      (assert-decoded-frame-payload
       frame
       (lambda (payload)
         (multiple-value-bind (rows cols) (decode-size payload)
           (expect (= 300 rows))
           (expect (= 1000 cols)))))))

  ;; msg-key carries raw input bytes verbatim.
  (it "key-roundtrip"
    (let ((frame (msg-key #(27 91 65))))
      (assert-decoded-frame-type frame +msg-key+)
      (assert-decoded-frame-payload
       frame
       (lambda (payload) (expect (equalp #(27 91 65) payload))))))

  ;; msg-detach and msg-bye carry no payload.
  (it "detach-and-bye-are-empty"
    (multiple-value-bind (type payload next) (decode-frame (msg-detach))
      (expect (= +msg-detach+ type))
      (expect (= 0 (length payload)))
      (expect (= +header-size+ next)))
    (assert-decoded-frame-type (msg-bye) +msg-bye+)
    (assert-decoded-frame-payload (msg-bye) (lambda (payload) (expect (= 0 (length payload))))))

  ;; msg-frame carries a UTF-8 string, including multibyte CJK.
  (it "frame-text-roundtrip"
    (let* ((text "hello あ 中 │")
           (frame (msg-frame text)))
      (assert-decoded-frame-type frame +msg-frame+)
      (assert-decoded-frame-payload
       frame
       (lambda (payload) (expect (string= text (decode-text payload)))))))

  ;; msg-reply carries a forwarded command's UTF-8 output text (the display-message
  ;; -p / CLI command-output channel).
  (it "reply-text-roundtrip"
    (let* ((text "session: 0  windows: 2")
           (frame (msg-reply text)))
      (assert-decoded-frame-type frame +msg-reply+)
      (assert-decoded-frame-payload
       frame
       (lambda (payload)
         (expect (string= text (decode-text payload)))))))

  ;;; ── Streaming: partial buffers ──────────────────────────────────────────────

  ;; A buffer shorter than the 5-byte header yields no frame.
  (it "decode-incomplete-header-returns-nil"
    (let ((frame (msg-resize 10 20)))
      (multiple-value-bind (type payload next) (decode-frame frame 0 3)
        (expect (null type))
        (expect (null payload))
        (expect (= 0 next)))))

  ;; A complete header but truncated payload yields no frame.
  (it "decode-incomplete-payload-returns-nil"
    (let ((frame (msg-key #(1 2 3 4 5 6))))   ; header(5) + 6 payload = 11 bytes
      ;; Cut one payload byte short.
      (expect (null (decode-frame frame 0 (1- (length frame)))))))

  ;;; ── Streaming: several frames in one buffer ─────────────────────────────────

  ;; decode-frame consumes exactly one frame and reports the next offset, so a
  ;; stream of concatenated frames decodes sequentially.
  (it "decode-back-to-back-frames"
    (let* ((buffer (cat-octets (msg-detach)
                               (msg-key #(65 66))
                               (msg-resize 5 9))))
      ;; Frame 1 — detach
      (multiple-value-bind (type payload next1) (decode-frame buffer)
        (declare (ignore payload))
        (expect (= +msg-detach+ type))
        ;; Frame 2 — key
        (multiple-value-bind (type2 payload2 next2) (decode-frame buffer next1)
          (expect (= +msg-key+ type2))
          (expect (equalp #(65 66) payload2))
          ;; Frame 3 — resize
          (multiple-value-bind (type3 payload3 next3) (decode-frame buffer next2)
            (expect (= +msg-resize+ type3))
            (multiple-value-bind (rows cols) (decode-size payload3)
              (expect (= 5 rows))
              (expect (= 9 cols)))
            (expect (= (length buffer) next3)))))))

  ;;; ── Large payloads: u32 length bytes exercised ──────────────────────────────

  ;; Frames whose payloads span the upper u32 length bytes encode/decode cleanly.
  ;; For 256, 1000 and 65536-byte payloads the u32-be length field (read-u32 at
  ;; offset 1) must equal the payload length, and the payload round-trips equalp.
  (it "frame-codec-large-payload-roundtrip"
    (dolist (n (list 256 1000 65536))
      (let* ((payload (make-array n :element-type '(unsigned-byte 8))))
        ;; Fill with a recognizable, position-dependent pattern.
        (dotimes (i n)
          (setf (aref payload i) (logand i #xFF)))
        (let ((frame (encode-frame +msg-frame+ payload)))
          (expect (= (+ +header-size+ n) (length frame)))
          ;; Length field stored big-endian at offset 1.
          (expect (= n (cl-tmux/protocol:read-u32 frame 1)))
          (multiple-value-bind (type decoded next) (decode-frame frame)
            (expect (= +msg-frame+ type))
            (expect (= n (length decoded)))
            (expect (equalp payload decoded))
            (expect (= (length frame) next)))))))

  ;; Two large frames packed back-to-back decode sequentially with the right
  ;; next-index offsets, exercising u32 lengths > 255.
  (it "frame-codec-two-large-frames-next-index"
    (let* ((p1 (make-array 1000  :element-type '(unsigned-byte 8)))
           (p2 (make-array 65536 :element-type '(unsigned-byte 8))))
      (dotimes (i 1000)  (setf (aref p1 i) (logand i #xFF)))
      (dotimes (i 65536) (setf (aref p2 i) (logand (* 3 i) #xFF)))
      (let* ((f1 (encode-frame +msg-key+ p1))
             (f2 (encode-frame +msg-frame+ p2))
             (buffer (cat-octets f1 f2)))
        ;; Frame 1
        (multiple-value-bind (type1 payload1 next1) (decode-frame buffer)
          (expect (= +msg-key+ type1))
          (expect (equalp p1 payload1))
          (expect (= (length f1) next1))
          ;; Frame 2 — decode starting at the reported offset.
          (multiple-value-bind (type2 payload2 next2) (decode-frame buffer next1)
            (expect (= +msg-frame+ type2))
            (expect (equalp p2 payload2))
            (expect (= (+ (length f1) (length f2)) next2))
            (expect (= (length buffer) next2)))))))

  ;; Property test: for ANY type byte and ANY payload, encode-frame → decode-frame
  ;; recovers the exact (type payload) pair and NEXT lands exactly on the frame's
  ;; end — generalizes the example-based roundtrip tests above across the whole
  ;; input space rather than a handful of hand-picked payload sizes.
  (it-property "encode-frame/decode-frame round-trips for any type byte and payload"
      ((type (gen-integer :min 0 :max 255))
       (payload-octets (gen-list (gen-integer :min 0 :max 255) :max-length 200)))
    (let* ((payload (to-octets payload-octets))
           (frame   (encode-frame type payload)))
      (multiple-value-bind (decoded-type decoded-payload next) (decode-frame frame)
        (expect (eql type decoded-type))
        (expect (equalp payload decoded-payload))
        (expect (= (length frame) next)))))

  ;; Integer encoders and decoders are symmetric: encode → decode recovers the original value.
  (it "u16-u32-encoders-and-decoders-are-symmetric"
    (dolist (n '(0 1 255 256 65535))
      (expect (= n (cl-tmux/protocol:read-u16 (cl-tmux/protocol:u16-octets n) 0))))
    (dolist (n '(0 1 65536 #xFFFFFF #xFFFFFFFF))
      (expect (= n (cl-tmux/protocol:read-u32 (cl-tmux/protocol:u32-octets n) 0)))))

  ;; Property test: u16-octets/read-u16 and u32-octets/read-u32 are symmetric
  ;; across their FULL value range, generalizing the hand-picked boundary
  ;; values above (0/1/255/256/65535/…) to every representable value.
  (it-property "u16-octets/read-u16 round-trip across the full 16-bit range"
      ((n (gen-integer :min 0 :max 65535)))
    (expect (= n (cl-tmux/protocol:read-u16 (cl-tmux/protocol:u16-octets n) 0))))

  (it-property "u32-octets/read-u32 round-trip across the full 32-bit range"
      ((n (gen-integer :min 0 :max #xFFFFFFFF)))
    (expect (= n (cl-tmux/protocol:read-u32 (cl-tmux/protocol:u32-octets n) 0))))

  ;; u16-octets always yields 2 bytes and u32-octets always yields 4 bytes,
  ;; regardless of the value encoded.
  (it "u16-u32-encoders-produce-correct-byte-widths"
    (dolist (c (list (list 2 (cl-tmux/protocol:u16-octets 0)          "u16(0) = 2 bytes")
                     (list 2 (cl-tmux/protocol:u16-octets 65535)      "u16(max) = 2 bytes")
                     (list 4 (cl-tmux/protocol:u32-octets 0)          "u32(0) = 4 bytes")
                     (list 4 (cl-tmux/protocol:u32-octets #xFFFFFFFF) "u32(max) = 4 bytes")))
      (destructuring-bind (expected-len result desc) c
        (declare (ignore desc))
        (expect (= expected-len (length result))))))

  ;; All eight typed message constructors produce frames that decode to the expected type.
  (it "msg-constructors-produce-correct-frames"
    (dolist (c (list (cons (msg-attach 24 80)                 +msg-attach+)
                      (cons (msg-key #(65))                   +msg-key+)
                      (cons (msg-resize 24 80)                +msg-resize+)
                      (cons (msg-detach)                      +msg-detach+)
                      (cons (msg-frame "hi")                  +msg-frame+)
                      (cons (msg-bye)                         +msg-bye+)
                      (cons (msg-reply "output")              +msg-reply+)
                      (cons (msg-command :new-window nil nil) +msg-command+)))
      (assert-decoded-frame-type (car c) (cdr c))))

  ;;; ── msg-command constructor ──────────────────────────────────────────────────

  ;; msg-command produces a +msg-command+ frame whose payload decodes cleanly.
  (it "msg-command-builds-valid-frame"
    (let ((frame (msg-command :new-window nil nil)))
      (multiple-value-bind (type payload next) (decode-frame frame)
        (expect (= +msg-command+ type))
        (expect (= (length frame) next))
        (multiple-value-bind (command target args)
            (decode-command-payload payload)
          (expect (eq :new-window command))
          (expect (null target))
          (expect (null args)))))
    ;; With target and args
    (let ((frame (msg-command :send-keys "1:2.3" '("hello" "world"))))
      (multiple-value-bind (type payload) (decode-frame frame)
        (expect (= +msg-command+ type))
        (multiple-value-bind (command target args)
            (decode-command-payload payload)
          (expect (eq :send-keys command))
          (expect (string= "1:2.3" target))
          (expect (equal '("hello" "world") args))))))

  ;;; ── encode-command-payload / decode-command-payload round-trips ─────────────

  ;; encode-command-payload round-trips command, target, and args in all combinations.
  (it "encode-decode-command-payload-table"
    (dolist (row '(((:new-window)                               :new-window  nil       nil           "no target/args")
                   ((:select-pane :target "$1:0.0")             :select-pane "$1:0.0"  nil           "with target")
                   ((:send-keys :args ("C-c" ""))               :send-keys   nil       ("C-c" "")    "with args")
                   ((:resize-pane :target "2:0" :args ("-U" "5")) :resize-pane "2:0"  ("-U" "5")    "target+args")
                   (("new-session" :target "$2")                :new-session "$2"      nil           "string command-name")))
      (destructuring-bind (encode-args expected-cmd expected-target expected-args desc) row
        (declare (ignore desc))
        (multiple-value-bind (command target args)
            (decode-command-payload (apply #'encode-command-payload encode-args))
          (expect (eq expected-cmd command))
          (expect (equal expected-target target))
          (expect (equal expected-args args))))))

  ;; A command name containing ':' is not misidentified as a target when only
  ;; one field is present (no ambiguity — target detection requires >= 2 fields).
  (it "decode-command-payload-command-name-with-colon-is-not-misidentified"
    ;; Encode manually: a single NUL-terminated field with ':' in the name.
    ;; With only 1 field decode-command-payload cannot treat it as a target.
    (let* ((name-bytes (babel:string-to-octets "weird:name" :encoding :utf-8))
           (payload    (concatenate '(simple-array (unsigned-byte 8) (*))
                                    name-bytes #(0))))
      (multiple-value-bind (command target args) (decode-command-payload payload)
        (expect (eq (intern "WEIRD:NAME" :keyword) command))
        (expect (null target))
        (expect (null args))))))
