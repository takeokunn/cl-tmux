(in-package #:cl-tmux/test)

;;;; Client/server wire-protocol codec tests (src/protocol.lisp).
;;;;
;;;; Pure octet-level tests: encode a message, decode it back, and assert the
;;;; type, payload, and consumed-byte count.  Also covers streaming concerns:
;;;; partial buffers (incomplete header / payload) and several frames packed
;;;; back-to-back in one buffer.

(def-suite protocol-suite :description "Client/server wire protocol codec")
(in-suite protocol-suite)

;;; ── Helpers ─────────────────────────────────────────────────────────────────

(defun cat-octets (&rest frames)
  "Concatenate octet vectors into one simple octet buffer (a wire stream)."
  (apply #'concatenate '(simple-array (unsigned-byte 8) (*)) frames))

;;; ── Frame header format ─────────────────────────────────────────────────────

(test frame-header-layout
  "A frame is [type][len u32-be][payload]; header is 5 bytes."
  (let ((frame (encode-frame +msg-key+ (to-octets #(7 8 9)))))
    (is (= +header-size+ 5))
    (is (= +msg-key+ (aref frame 0))     "type byte first")
    ;; length = 3, big-endian in bytes 1..4
    (is (equalp #(0 0 0 3) (subseq frame 1 5)) "u32-be length")
    (is (= 8 (length frame)) "header + 3 payload bytes")))

;;; ── Round-trips per message type ────────────────────────────────────────────

(test attach-roundtrip
  "msg-attach carries rows,cols, decodable via decode-size."
  (let ((frame (msg-attach 24 80)))
    (multiple-value-bind (type payload next) (decode-frame frame)
      (is (= +msg-attach+ type))
      (is (= (length frame) next) "consumed the whole frame")
      (multiple-value-bind (rows cols) (decode-size payload)
        (is (= 24 rows))
        (is (= 80 cols))))))

(test resize-roundtrip
  "msg-resize round-trips rows,cols including large values."
  (multiple-value-bind (type payload) (decode-frame (msg-resize 300 1000))
    (is (= +msg-resize+ type))
    (multiple-value-bind (rows cols) (decode-size payload)
      (is (= 300 rows))
      (is (= 1000 cols)))))

(test key-roundtrip
  "msg-key carries raw input bytes verbatim."
  (multiple-value-bind (type payload) (decode-frame (msg-key #(27 91 65)))
    (is (= +msg-key+ type))
    (is (equalp #(27 91 65) payload))))

(test detach-and-bye-are-empty
  "msg-detach and msg-bye carry no payload."
  (multiple-value-bind (type payload next) (decode-frame (msg-detach))
    (is (= +msg-detach+ type))
    (is (= 0 (length payload)))
    (is (= +header-size+ next) "empty payload ⇒ next is just past the header"))
  (multiple-value-bind (type payload) (decode-frame (msg-bye))
    (is (= +msg-bye+ type))
    (is (= 0 (length payload)))))

(test frame-text-roundtrip
  "msg-frame carries a UTF-8 string, including multibyte CJK."
  (let ((text "hello あ 中 │"))
    (multiple-value-bind (type payload) (decode-frame (msg-frame text))
      (is (= +msg-frame+ type))
      (is (string= text (decode-text payload))))))

;;; ── Streaming: partial buffers ──────────────────────────────────────────────

(test decode-incomplete-header-returns-nil
  "A buffer shorter than the 5-byte header yields no frame."
  (let ((frame (msg-resize 10 20)))
    (multiple-value-bind (type payload next) (decode-frame frame 0 3)
      (is (null type))
      (is (null payload))
      (is (= 0 next) "start index returned unchanged when incomplete"))))

(test decode-incomplete-payload-returns-nil
  "A complete header but truncated payload yields no frame."
  (let ((frame (msg-key #(1 2 3 4 5 6))))   ; header(5) + 6 payload = 11 bytes
    ;; Cut one payload byte short.
    (is (null (decode-frame frame 0 (1- (length frame)))))))

;;; ── Streaming: several frames in one buffer ─────────────────────────────────

(test decode-back-to-back-frames
  "decode-frame consumes exactly one frame and reports the next offset, so a
   stream of concatenated frames decodes sequentially."
  (let* ((buffer (cat-octets (msg-detach)
                             (msg-key #(65 66))
                             (msg-resize 5 9))))
    ;; Frame 1 — detach
    (multiple-value-bind (type payload next1) (decode-frame buffer)
      (declare (ignore payload))
      (is (= +msg-detach+ type))
      ;; Frame 2 — key
      (multiple-value-bind (type2 payload2 next2) (decode-frame buffer next1)
        (is (= +msg-key+ type2))
        (is (equalp #(65 66) payload2))
        ;; Frame 3 — resize
        (multiple-value-bind (type3 payload3 next3) (decode-frame buffer next2)
          (is (= +msg-resize+ type3))
          (multiple-value-bind (rows cols) (decode-size payload3)
            (is (= 5 rows))
            (is (= 9 cols)))
          (is (= (length buffer) next3) "consumed the entire stream"))))))

;;; ── Large payloads: u32 length bytes exercised ──────────────────────────────

(test frame-codec-large-payload-roundtrip
  "Frames whose payloads span the upper u32 length bytes encode/decode cleanly.
   For 256, 1000 and 65536-byte payloads the u32-be length field (read-u32 at
   offset 1) must equal the payload length, and the payload round-trips equalp."
  (dolist (n (list 256 1000 65536))
    (let* ((payload (cl-tmux/protocol::make-octets n)))
      ;; Fill with a recognizable, position-dependent pattern.
      (dotimes (i n)
        (setf (aref payload i) (logand i #xFF)))
      (let ((frame (encode-frame +msg-frame+ payload)))
        (is (= (+ +header-size+ n) (length frame))
            "frame is header + payload bytes")
        ;; Length field stored big-endian at offset 1.
        (is (= n (cl-tmux/protocol::read-u32 frame 1))
            "u32-be length field equals payload length")
        (multiple-value-bind (type decoded next) (decode-frame frame)
          (is (= +msg-frame+ type))
          (is (= n (length decoded)) "decoded payload length matches")
          (is (equalp payload decoded) "payload round-trips")
          (is (= (length frame) next) "consumed the whole frame"))))))

(test frame-codec-two-large-frames-next-index
  "Two large frames packed back-to-back decode sequentially with the right
   next-index offsets, exercising u32 lengths > 255."
  (let* ((p1 (cl-tmux/protocol::make-octets 1000))
         (p2 (cl-tmux/protocol::make-octets 65536)))
    (dotimes (i 1000)  (setf (aref p1 i) (logand i #xFF)))
    (dotimes (i 65536) (setf (aref p2 i) (logand (* 3 i) #xFF)))
    (let* ((f1 (encode-frame +msg-key+ p1))
           (f2 (encode-frame +msg-frame+ p2))
           (buffer (cat-octets f1 f2)))
      ;; Frame 1
      (multiple-value-bind (type1 payload1 next1) (decode-frame buffer)
        (is (= +msg-key+ type1))
        (is (equalp p1 payload1))
        (is (= (length f1) next1) "next-index is exactly past the first frame")
        ;; Frame 2 — decode starting at the reported offset.
        (multiple-value-bind (type2 payload2 next2) (decode-frame buffer next1)
          (is (= +msg-frame+ type2))
          (is (equalp p2 payload2))
          (is (= (+ (length f1) (length f2)) next2) "consumed both frames")
          (is (= (length buffer) next2) "consumed the entire stream"))))))
