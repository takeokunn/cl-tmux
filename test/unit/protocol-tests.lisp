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
  "All six typed message constructors produce frames that decode to the expected type."
  ;; Verify each constructor actually produces a decodable frame with the right tag.
  (flet ((frame-type (frame)
           (multiple-value-bind (type payload next)
               (decode-frame frame)
             (declare (ignore payload next))
             type)))
    (is (= +msg-attach+ (frame-type (msg-attach 24 80)))   "msg-attach produces +msg-attach+")
    (is (= +msg-key+    (frame-type (msg-key #(65))))       "msg-key produces +msg-key+")
    (is (= +msg-resize+ (frame-type (msg-resize 24 80)))   "msg-resize produces +msg-resize+")
    (is (= +msg-detach+ (frame-type (msg-detach)))         "msg-detach produces +msg-detach+")
    (is (= +msg-frame+  (frame-type (msg-frame "hi")))     "msg-frame produces +msg-frame+")
    (is (= +msg-bye+    (frame-type (msg-bye)))            "msg-bye produces +msg-bye+")))

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

(test target-field-p-detects-session-sigil
  "A field starting with '$' is a target (session sigil)."
  (is (cl-tmux/protocol:target-field-p "$0"))
  (is (cl-tmux/protocol:target-field-p "$99")))

(test target-field-p-detects-colon-syntax
  "A field containing ':' is a target (session:window format)."
  (is (cl-tmux/protocol:target-field-p "mysession:1"))
  (is (cl-tmux/protocol:target-field-p "0:1")))

(test target-field-p-detects-dot-syntax
  "A field containing '.' is a target (window.pane format)."
  (is (cl-tmux/protocol:target-field-p "0.1"))
  (is (cl-tmux/protocol:target-field-p "mysession:0.2")))

(test target-field-p-rejects-plain-command-names
  "Plain command names (no '$', ':', or '.') are not targets."
  (is (not (cl-tmux/protocol:target-field-p "new-window")))
  (is (not (cl-tmux/protocol:target-field-p "select-pane")))
  (is (not (cl-tmux/protocol:target-field-p ""))))

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

;;; ── read-u32 dedicated test ─────────────────────────────────────────────────

(test read-u32-decodes-big-endian
  "read-u32 reads four bytes at START as a big-endian u32."
  (let ((buffer (make-array 8 :element-type '(unsigned-byte 8)
                              :initial-contents '(0 0 0 0 0 0 1 0))))
    (is (= 0      (cl-tmux/protocol:read-u32 buffer 0)) "all-zero word")
    (is (= 256    (cl-tmux/protocol:read-u32 buffer 4)) "0 0 1 0 = 256")
    (let ((buf2 (cl-tmux/protocol:u32-octets #xDEADBEEF)))
      (is (= #xDEADBEEF (cl-tmux/protocol:read-u32 buf2 0)) "0xDEADBEEF round-trip"))))

;;; ── split-on-nul-bytes ──────────────────────────────────────────────────────

(test split-on-nul-bytes-empty-input-returns-empty-list
  "split-on-nul-bytes on an empty buffer returns an empty list."
  (is (null (cl-tmux/protocol:split-on-nul-bytes #()))
      "empty input must yield nil"))

(test split-on-nul-bytes-single-field
  "split-on-nul-bytes with one NUL-terminated field returns a one-element list."
  (let* ((bytes (babel:string-to-octets "hello" :encoding :utf-8))
         (buf   (concatenate '(simple-array (unsigned-byte 8) (*)) bytes #(0))))
    (is (equal '("hello") (cl-tmux/protocol:split-on-nul-bytes buf))
        "single NUL-terminated field must yield a one-element list")))

(test split-on-nul-bytes-multiple-fields
  "split-on-nul-bytes with multiple NUL-separated fields returns them all."
  (let* ((a (babel:string-to-octets "alpha" :encoding :utf-8))
         (b (babel:string-to-octets "beta"  :encoding :utf-8))
         (c (babel:string-to-octets "gamma" :encoding :utf-8))
         (buf (concatenate '(simple-array (unsigned-byte 8) (*))
                           a #(0) b #(0) c #(0))))
    (is (equal '("alpha" "beta" "gamma")
               (cl-tmux/protocol:split-on-nul-bytes buf))
        "three NUL-terminated fields must be returned in order")))

(test split-on-nul-bytes-no-nul-returns-empty-list
  "split-on-nul-bytes with no NUL byte returns an empty list (no complete field)."
  (let ((buf (babel:string-to-octets "no-nul" :encoding :utf-8)))
    (is (null (cl-tmux/protocol:split-on-nul-bytes buf))
        "no NUL byte → empty list")))

;;; ── command-name-to-string ──────────────────────────────────────────────────

(test command-name-to-string-keyword-to-lowercase
  "command-name-to-string downcases a keyword's symbol-name."
  (is (string= "new-window"
               (cl-tmux/protocol:command-name-to-string :new-window))
      ":new-window → \"new-window\""))

(test command-name-to-string-string-passthrough
  "command-name-to-string returns a plain string unchanged."
  (is (string= "select-pane"
               (cl-tmux/protocol:command-name-to-string "select-pane"))
      "string passthrough"))

;;; ── assemble-command-fields ─────────────────────────────────────────────────

(test assemble-command-fields-no-target-no-args
  "assemble-command-fields returns just the name when target and args are nil."
  (is (equal '("new-window")
             (cl-tmux/protocol:assemble-command-fields "new-window" nil nil))
      "no target, no args → name only"))

(test assemble-command-fields-with-target
  "assemble-command-fields prepends target when supplied."
  (is (equal '("$1:0" "select-pane")
             (cl-tmux/protocol:assemble-command-fields "select-pane" "$1:0" nil))
      "target is prepended before name"))

(test assemble-command-fields-with-args
  "assemble-command-fields appends args after name."
  (is (equal '("send-keys" "C-c" "")
             (cl-tmux/protocol:assemble-command-fields "send-keys" nil '("C-c" "")))
      "args follow name"))

(test assemble-command-fields-target-and-args
  "assemble-command-fields orders fields as: [target] name [args...]."
  (is (equal '("2:0" "resize-pane" "-U" "5")
             (cl-tmux/protocol:assemble-command-fields "resize-pane" "2:0" '("-U" "5")))
      "target precedes name; args follow"))

;;; ── encode-fields-to-buffer ─────────────────────────────────────────────────

(test encode-fields-to-buffer-empty-fields-produces-empty-buffer
  "encode-fields-to-buffer with no fields produces an empty buffer."
  (let ((buf (cl-tmux/protocol:encode-fields-to-buffer '())))
    (is (= 0 (length buf)) "no fields → empty buffer")))

(test encode-fields-to-buffer-single-field-has-trailing-nul
  "encode-fields-to-buffer packs one field followed by a NUL byte."
  (let* ((field-bytes (babel:string-to-octets "hello" :encoding :utf-8))
         (buf (cl-tmux/protocol:encode-fields-to-buffer (list field-bytes))))
    (is (= 6 (length buf)) "5 data bytes + 1 NUL = 6")
    (is (= 0 (aref buf 5)) "last byte must be NUL")))

(test encode-fields-to-buffer-multiple-fields-split-by-nuls
  "encode-fields-to-buffer places a NUL after each field."
  (let* ((f1  (babel:string-to-octets "ab" :encoding :utf-8))
         (f2  (babel:string-to-octets "cd" :encoding :utf-8))
         (buf (cl-tmux/protocol:encode-fields-to-buffer (list f1 f2))))
    ;; Layout: a b NUL c d NUL → 6 bytes
    (is (= 6 (length buf)) "2+1+2+1 = 6 bytes")
    (is (= 0 (aref buf 2)) "NUL after first field at index 2")
    (is (= 0 (aref buf 5)) "NUL after second field at index 5")))

;;; ── to-octets ───────────────────────────────────────────────────────────────

(test to-octets-coerces-list-to-simple-vector
  "to-octets coerces a list of octets to a simple (unsigned-byte 8) vector."
  (let ((result (to-octets '(1 2 3))))
    (is (typep result '(simple-array (unsigned-byte 8) (*)))
        "result must be a simple octet vector")
    (is (equalp #(1 2 3) result) "contents must match")))

(test to-octets-idempotent-on-simple-vector
  "to-octets on an already-simple octet vector returns an equivalent vector."
  (let* ((original #(10 20 30))
         (result   (to-octets original)))
    (is (equalp original result) "content must be preserved")))

;;; ── decode-size / decode-text edge cases ────────────────────────────────────

(test decode-size-zero-rows-zero-cols
  "decode-size decodes a (0,0) payload correctly."
  (multiple-value-bind (rows cols) (decode-size (u16-octets-pair 0 0))
    (is (= 0 rows) "rows must be 0")
    (is (= 0 cols) "cols must be 0")))

(test decode-size-max-u16-values
  "decode-size round-trips the maximum u16 values (65535 x 65535)."
  (multiple-value-bind (rows cols) (decode-size (u16-octets-pair 65535 65535))
    (is (= 65535 rows) "rows must be 65535")
    (is (= 65535 cols) "cols must be 65535")))

(test decode-text-empty-payload
  "decode-text on an empty octet vector returns an empty string."
  (is (string= "" (decode-text #()))
      "empty payload must decode to empty string"))

(test decode-text-ascii
  "decode-text decodes a plain ASCII payload to a string."
  (let ((bytes (babel:string-to-octets "hello" :encoding :utf-8)))
    (is (string= "hello" (decode-text bytes))
        "ASCII payload must decode correctly")))

;;; ── decode-command-payload empty / degenerate input ─────────────────────────

(test decode-command-payload-empty-payload-returns-nil-values
  "decode-command-payload on a zero-byte payload returns (values NIL NIL NIL)
   without signalling; the caller must handle the empty-fields case explicitly."
  (multiple-value-bind (command target args)
      (decode-command-payload #())
    (is (null command) "command must be NIL for empty payload")
    (is (null target)  "target must be NIL for empty payload")
    (is (null args)    "args must be NIL for empty payload")))

(test decode-command-payload-no-nul-byte-returns-nil-values
  "decode-command-payload on a payload with no NUL terminator returns
   (values NIL NIL NIL) — no NUL means no complete field was transmitted."
  (let ((payload (babel:string-to-octets "no-nul-here" :encoding :utf-8)))
    (multiple-value-bind (command target args)
        (decode-command-payload payload)
      (is (null command) "command must be NIL when no NUL found")
      (is (null target)  "target must be NIL when no NUL found")
      (is (null args)    "args must be NIL when no NUL found"))))

;;; ── msg-command edge cases ───────────────────────────────────────────────────

(test msg-command-empty-args-list-roundtrips
  "msg-command with an explicit empty args list produces the same frame as NIL args."
  (let ((frame-nil  (msg-command :new-window nil nil))
        (frame-list (msg-command :new-window nil '())))
    (is (equalp frame-nil frame-list)
        "nil args and empty-list args must produce identical frames")))

(test msg-command-string-command-name-roundtrips
  "msg-command accepts a plain string command-name (not a keyword)."
  (let ((frame (msg-command "split-window" nil nil)))
    (multiple-value-bind (type payload) (decode-frame frame)
      (is (= +msg-command+ type))
      (multiple-value-bind (command target args)
          (decode-command-payload payload)
        (is (eq :split-window command) "string name must round-trip as keyword")
        (is (null target))
        (is (null args))))))

;;; ── Message type tag constant values ─────────────────────────────────────────
;;;
;;; The wire protocol fixes the numeric type tags permanently — any change would
;;; break backwards compatibility.  These tests pin the actual byte values so a
;;; slip in the defconstant definitions is caught immediately at test time rather
;;; than at integration time.

(test message-type-tag-constants-have-expected-values
  "Message type tag constants are fixed wire-protocol values and must not change."
  (is (= 1 +msg-attach+)  "+msg-attach+ must equal 1")
  (is (= 2 +msg-key+)     "+msg-key+ must equal 2")
  (is (= 3 +msg-resize+)  "+msg-resize+ must equal 3")
  (is (= 4 +msg-detach+)  "+msg-detach+ must equal 4")
  (is (= 5 +msg-frame+)   "+msg-frame+ must equal 5")
  (is (= 6 +msg-bye+)     "+msg-bye+ must equal 6")
  (is (= 7 +msg-command+) "+msg-command+ must equal 7")
  (is (= 5 +header-size+) "+header-size+ must equal 5"))

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

;;; ── target-field-p edge cases ────────────────────────────────────────────────

(test target-field-p-single-dollar-is-a-target
  "A lone '$' character is a valid session sigil and therefore a target."
  (is (cl-tmux/protocol:target-field-p "$")
      "bare '$' must be detected as a target sigil"))

(test target-field-p-single-colon-is-a-target
  "A lone ':' character satisfies the contains-char rule for ':'."
  (is (cl-tmux/protocol:target-field-p ":")
      "bare ':' must be detected as a target (contains ':')"))

(test target-field-p-single-dot-is-a-target
  "A lone '.' character satisfies the contains-char rule for '.'."
  (is (cl-tmux/protocol:target-field-p ".")
      "bare '.' must be detected as a target (contains '.')"))

(test target-field-p-integer-string-is-not-a-target
  "A plain integer string (window index) has no sigil and is not a target."
  (is (not (cl-tmux/protocol:target-field-p "0"))
      "\"0\" must not be detected as a target"))

(test target-field-p-long-command-name-is-not-a-target
  "A typical long tmux command name with hyphens but no sigil characters is not a target."
  (is (not (cl-tmux/protocol:target-field-p "copy-mode-search-forward"))
      "hyphenated command name must not be detected as a target"))

;;; ── command-name-to-string edge cases ────────────────────────────────────────

(test command-name-to-string-uppercased-keyword
  "command-name-to-string downcases keywords regardless of how the keyword was interned."
  (is (string= "new-window"
               (cl-tmux/protocol:command-name-to-string :NEW-WINDOW))
      "uppercase keyword symbol-name must be downcased to wire form")
  (is (string= "select-pane"
               (cl-tmux/protocol:command-name-to-string :SELECT-PANE))
      ":SELECT-PANE must downcase to \"select-pane\""))

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

;;; ── to-octets on an empty list ───────────────────────────────────────────────

(test to-octets-empty-list-produces-empty-vector
  "to-octets on an empty list produces an empty (unsigned-byte 8) vector."
  (let ((result (to-octets '())))
    (is (typep result '(simple-array (unsigned-byte 8) (*)))
        "result must be a simple octet vector")
    (is (= 0 (length result)) "result must have zero elements")))

;;; ── split-on-nul-bytes trailing data after final NUL ─────────────────────────

(test split-on-nul-bytes-trailing-bytes-after-last-nul-are-ignored
  "split-on-nul-bytes ignores bytes that follow the final NUL (incomplete field)."
  (let* ((a     (babel:string-to-octets "alpha" :encoding :utf-8))
         ;; 'beta' bytes appended WITHOUT a terminating NUL.
         (b     (babel:string-to-octets "beta"  :encoding :utf-8))
         (buf   (concatenate '(simple-array (unsigned-byte 8) (*))
                             a #(0) b)))
    (is (equal '("alpha")
               (cl-tmux/protocol:split-on-nul-bytes buf))
        "only the NUL-terminated field must be returned; trailing bytes are ignored")))

;;; ── assemble-command-fields preserves arg order ──────────────────────────────

(test assemble-command-fields-preserves-multiple-args-order
  "assemble-command-fields appends many args in the supplied order."
  (is (equal '("cmd" "a" "b" "c" "d")
             (cl-tmux/protocol:assemble-command-fields "cmd" nil '("a" "b" "c" "d")))
      "four args must appear in order after the command name"))

;;; ── encode-fields-to-buffer / split-on-nul-bytes are symmetric ──────────────

(test encode-fields-to-buffer-and-split-on-nul-bytes-are-symmetric
  "Encoding a list of strings with encode-fields-to-buffer and decoding with
   split-on-nul-bytes must recover the original strings."
  (let* ((strings  '("alpha" "beta" "gamma" "delta"))
         (octets   (mapcar (lambda (s)
                             (babel:string-to-octets s :encoding :utf-8))
                           strings))
         (buf      (cl-tmux/protocol:encode-fields-to-buffer octets))
         (decoded  (cl-tmux/protocol:split-on-nul-bytes buf)))
    (is (equal strings decoded)
        "round-trip through encode-fields-to-buffer + split-on-nul-bytes must be lossless")))
