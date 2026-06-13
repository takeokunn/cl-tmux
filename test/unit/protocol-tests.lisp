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

(def-suite protocol-suite :description "Client/server wire protocol codec")
(in-suite protocol-suite)

;;; ── Octet encoding/decoding helpers ─────────────────────────────────────────

(test u16-octets-big-endian
  "u16-octets encodes a 16-bit value as two big-endian bytes."
  (is (equalp #(0 0)     (cl-tmux/protocol:u16-octets 0)))
  (is (equalp #(0 1)     (cl-tmux/protocol:u16-octets 1)))
  (is (equalp #(1 0)     (cl-tmux/protocol:u16-octets 256)))
  (is (equalp #(255 255) (cl-tmux/protocol:u16-octets 65535))))

(test u32-octets-big-endian
  "u32-octets encodes a 32-bit value as four big-endian bytes."
  (is (equalp #(0 0 0 0)   (cl-tmux/protocol:u32-octets 0)))
  (is (equalp #(0 0 0 1)   (cl-tmux/protocol:u32-octets 1)))
  (is (equalp #(0 1 0 0)   (cl-tmux/protocol:u32-octets 65536)))
  (is (equalp #(255 255 255 255) (cl-tmux/protocol:u32-octets #xFFFFFFFF))))

(test u16-octets-pair-concatenates-two-u16s
  "u16-octets-pair concatenates two u16 values as 4 big-endian bytes."
  (is (equalp #(0 24 0 80) (cl-tmux/protocol:u16-octets-pair 24 80)))
  (is (equalp #(0 0 0 0)   (cl-tmux/protocol:u16-octets-pair 0 0))))

(test read-u16-decodes-big-endian
  "read-u16 reads two bytes at START as a big-endian u16."
  (let ((buffer (make-array 6 :element-type '(unsigned-byte 8)
                              :initial-contents '(0 0 0 24 0 80))))
    (is (= 0  (cl-tmux/protocol:read-u16 buffer 0)))
    (is (= 24 (cl-tmux/protocol:read-u16 buffer 2)))
    (is (= 80 (cl-tmux/protocol:read-u16 buffer 4)))))

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

(test reply-text-roundtrip
  "msg-reply carries a forwarded command's UTF-8 output text (the display-message
   -p / CLI command-output channel)."
  (let ((text "session: 0  windows: 2"))
    (multiple-value-bind (type payload) (decode-frame (msg-reply text))
      (is (= +msg-reply+ type) "frame type is +msg-reply+")
      (is (string= text (decode-text payload)) "payload round-trips the output text"))))

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
    (let* ((payload (make-array n :element-type '(unsigned-byte 8))))
      ;; Fill with a recognizable, position-dependent pattern.
      (dotimes (i n)
        (setf (aref payload i) (logand i #xFF)))
      (let ((frame (encode-frame +msg-frame+ payload)))
        (is (= (+ +header-size+ n) (length frame))
            "frame is header + payload bytes")
        ;; Length field stored big-endian at offset 1.
        (is (= n (cl-tmux/protocol:read-u32 frame 1))
            "u32-be length field equals payload length")
        (multiple-value-bind (type decoded next) (decode-frame frame)
          (is (= +msg-frame+ type))
          (is (= n (length decoded)) "decoded payload length matches")
          (is (equalp payload decoded) "payload round-trips")
          (is (= (length frame) next) "consumed the whole frame"))))))

(test frame-codec-two-large-frames-next-index
  "Two large frames packed back-to-back decode sequentially with the right
   next-index offsets, exercising u32 lengths > 255."
  (let* ((p1 (make-array 1000  :element-type '(unsigned-byte 8)))
         (p2 (make-array 65536 :element-type '(unsigned-byte 8))))
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

(test u16-u32-encoders-and-decoders-are-symmetric
  "Integer encoders and decoders are symmetric: encode → decode recovers the original value."
  (dolist (n '(0 1 255 256 65535))
    (is (= n (cl-tmux/protocol:read-u16 (cl-tmux/protocol:u16-octets n) 0))
        "u16 round-trip failed for ~D" n))
  (dolist (n '(0 1 65536 #xFFFFFF #xFFFFFFFF))
    (is (= n (cl-tmux/protocol:read-u32 (cl-tmux/protocol:u32-octets n) 0))
        "u32 round-trip failed for ~D" n)))

(test u16-u32-encoders-produce-correct-byte-widths
  "u16-octets always yields 2 bytes and u32-octets always yields 4 bytes,
   regardless of the value encoded."
  (is (= 2 (length (cl-tmux/protocol:u16-octets 0)))        "u16(0) = 2 bytes")
  (is (= 2 (length (cl-tmux/protocol:u16-octets 65535)))    "u16(max) = 2 bytes")
  (is (= 4 (length (cl-tmux/protocol:u32-octets 0)))        "u32(0) = 4 bytes")
  (is (= 4 (length (cl-tmux/protocol:u32-octets #xFFFFFFFF))) "u32(max) = 4 bytes"))

(test msg-constructors-produce-correct-frames
  "All eight typed message constructors produce frames that decode to the expected type."
  ;; Verify each constructor actually produces a decodable frame with the right tag.
  (flet ((frame-type (frame)
           (multiple-value-bind (type payload next)
               (decode-frame frame)
             (declare (ignore payload next))
             type)))
    (is (= +msg-attach+  (frame-type (msg-attach 24 80)))             "msg-attach produces +msg-attach+")
    (is (= +msg-key+     (frame-type (msg-key #(65))))                "msg-key produces +msg-key+")
    (is (= +msg-resize+  (frame-type (msg-resize 24 80)))             "msg-resize produces +msg-resize+")
    (is (= +msg-detach+  (frame-type (msg-detach)))                   "msg-detach produces +msg-detach+")
    (is (= +msg-frame+   (frame-type (msg-frame "hi")))               "msg-frame produces +msg-frame+")
    (is (= +msg-bye+     (frame-type (msg-bye)))                      "msg-bye produces +msg-bye+")
    (is (= +msg-reply+   (frame-type (msg-reply "output")))           "msg-reply produces +msg-reply+")
    (is (= +msg-command+ (frame-type (msg-command :new-window nil nil))) "msg-command produces +msg-command+")))

;;; ── msg-command constructor ──────────────────────────────────────────────────

(test msg-command-builds-valid-frame
  "msg-command produces a +msg-command+ frame whose payload decodes cleanly."
  (let ((frame (msg-command :new-window nil nil)))
    (multiple-value-bind (type payload next) (decode-frame frame)
      (is (= +msg-command+ type))
      (is (= (length frame) next) "consumed the whole frame")
      (multiple-value-bind (command target args)
          (decode-command-payload payload)
        (is (eq :new-window command))
        (is (null target))
        (is (null args)))))
  ;; With target and args
  (let ((frame (msg-command :send-keys "1:2.3" '("hello" "world"))))
    (multiple-value-bind (type payload) (decode-frame frame)
      (is (= +msg-command+ type))
      (multiple-value-bind (command target args)
          (decode-command-payload payload)
        (is (eq :send-keys command))
        (is (string= "1:2.3" target))
        (is (equal '("hello" "world") args))))))

;;; ── encode-command-payload / decode-command-payload round-trips ─────────────

(test encode-decode-command-payload-no-target-no-args
  "A command with no target and no args encodes and decodes cleanly."
  (let ((payload (encode-command-payload :new-window)))
    (multiple-value-bind (command target args) (decode-command-payload payload)
      (is (eq :new-window command))
      (is (null target))
      (is (null args)))))

(test encode-decode-command-payload-with-target
  "A command with a target field round-trips target and command-name."
  (multiple-value-bind (command target args)
      (decode-command-payload
       (encode-command-payload :select-pane :target "$1:0.0"))
    (is (eq :select-pane command))
    (is (string= "$1:0.0" target))
    (is (null args))))

(test encode-decode-command-payload-with-args
  "A command with argument strings round-trips all args in order."
  (multiple-value-bind (command target args)
      (decode-command-payload
       (encode-command-payload :send-keys :args '("C-c" "")))
    (is (eq :send-keys command))
    (is (null target))
    (is (equal '("C-c" "") args))))

(test encode-decode-command-payload-target-and-args
  "A command with both target and args round-trips all fields."
  (multiple-value-bind (command target args)
      (decode-command-payload
       (encode-command-payload :resize-pane :target "2:0" :args '("-U" "5")))
    (is (eq :resize-pane command))
    (is (string= "2:0" target))
    (is (equal '("-U" "5") args))))

(test encode-decode-command-payload-string-command-name
  "encode-command-payload accepts a plain string command-name (not keyword)."
  (multiple-value-bind (command target args)
      (decode-command-payload
       (encode-command-payload "new-session" :target "$2"))
    (is (eq :new-session command))
    (is (string= "$2" target))
    (is (null args))))

;;; ── target-field-p predicate ────────────────────────────────────────────────

(test target-field-p-table
  "target-field-p returns T for session sigil ($), colon (:), or dot (.) fields; NIL for plain names."
  (dolist (row '(("$0"            t   "session sigil $ → target")
                 ("$99"           t   "session sigil $99 → target")
                 ("mysession:1"   t   "colon syntax → target")
                 ("0:1"           t   "numeric colon syntax → target")
                 ("0.1"           t   "dot syntax → target")
                 ("mysession:0.2" t   "dot+colon syntax → target")
                 ("new-window"    nil "plain command name → not target")
                 ("select-pane"   nil "plain command name → not target")
                 (""              nil "empty string → not target")))
    (destructuring-bind (input expected desc) row
      (is (eq expected (if (cl-tmux/protocol:target-field-p input) t nil))
          "~A" desc))))

(test decode-command-payload-command-name-with-colon-is-not-misidentified
  "A command name containing ':' is not misidentified as a target when only
   one field is present (no ambiguity — target detection requires >= 2 fields)."
  ;; Encode manually: a single NUL-terminated field with ':' in the name.
  ;; With only 1 field decode-command-payload cannot treat it as a target.
  (let* ((name-bytes (babel:string-to-octets "weird:name" :encoding :utf-8))
         (payload    (concatenate '(simple-array (unsigned-byte 8) (*))
                                  name-bytes #(0))))
    (multiple-value-bind (command target args) (decode-command-payload payload)
      (is (eq (intern "WEIRD:NAME" :keyword) command))
      (is (null target))
      (is (null args)))))
