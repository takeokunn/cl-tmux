(in-package #:cl-tmux/test)

;;;; protocol tests — part B: read-u32, split-on-nul-bytes,
;;;; frame codec edge cases, encode/decode-command-payload round-trips,
;;;; target-field-p, decode-command-payload colon disambiguation.

(in-suite protocol-suite)

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

(test command-name-to-string-table
  "command-name-to-string downcases keywords (any case) and passes strings through unchanged."
  (dolist (c '((:new-window  "new-window"  "lowercase keyword → downcased")
               (:NEW-WINDOW  "new-window"  "uppercase keyword → downcased")
               (:SELECT-PANE "select-pane" "uppercase keyword → downcased")
               ("select-pane" "select-pane" "string → pass through")))
    (destructuring-bind (input expected desc) c
      (is (string= expected (cl-tmux/protocol:command-name-to-string input))
          "~A" desc))))

;;; ── assemble-command-fields ─────────────────────────────────────────────────

(test assemble-command-fields-table
  "assemble-command-fields orders fields as [target] name [args...]."
  (dolist (c '(("new-window"  nil    nil          ("new-window")               "name only")
               ("select-pane" "$1:0" nil          ("$1:0" "select-pane")       "target + name")
               ("send-keys"   nil    ("C-c" "")   ("send-keys" "C-c" "")       "name + args")
               ("resize-pane" "2:0"  ("-U" "5")   ("2:0" "resize-pane" "-U" "5") "target + name + args")))
    (destructuring-bind (name target args expected desc) c
      (is (equal expected (cl-tmux/protocol:assemble-command-fields name target args))
          "~A" desc))))

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
;;; The wire protocol fixes the numeric type tags permanently — changing them
;;; would alter the byte-level contract.  These tests pin the actual byte values so a
;;; slip in the defconstant definitions is caught immediately at test time rather
;;; than at integration time.

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

;;; ── target-field-p edge cases ────────────────────────────────────────────────

(test target-field-p-table
  "target-field-p recognizes sigil characters ($, :, .) as targets; plain names/numbers are not."
  (dolist (c '(("$"                        t   "bare '$' is a target")
               (":"                        t   "bare ':' is a target")
               ("."                        t   "bare '.' is a target")
               ("0"                        nil "plain integer is not a target")
               ("copy-mode-search-forward" nil "hyphenated command name is not a target")))
    (destructuring-bind (input expected desc) c
      (if expected
          (is-true  (cl-tmux/protocol:target-field-p input) "~A" desc)
          (is-false (cl-tmux/protocol:target-field-p input) "~A" desc)))))

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
     +attach-flags-offset+ == +attach-size-bytes+ (rows,cols occupy 4 bytes)
     +cols-offset-in-size-payload+ must equal 2 (after the 2-byte rows u16)"
  (is (= +header-size+
         (+ cl-tmux/protocol:+payload-length-offset+ 4))
      "+payload-length-offset+ + 4 must equal +header-size+")
  (is (= cl-tmux/protocol:+attach-flags-offset+
         cl-tmux/protocol:+attach-size-bytes+)
      "+attach-flags-offset+ must equal +attach-size-bytes+")
  (is (= 2 cl-tmux/protocol:+cols-offset-in-size-payload+)
      "+cols-offset-in-size-payload+ must equal 2"))
