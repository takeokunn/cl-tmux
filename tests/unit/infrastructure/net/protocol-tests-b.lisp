(in-package #:cl-tmux/test)

;;;; Protocol field and text codec tests.

(describe "protocol-suite"

  ;; ── read-u32 dedicated test ─────────────────────────────────────────────────

  ;; read-u32 reads four bytes at START as a big-endian u32.
  (it "read-u32-decodes-big-endian"
    (let ((buffer (make-array 8 :element-type '(unsigned-byte 8)
                                :initial-contents '(0 0 0 0 0 0 1 0))))
      (expect (= 0      (cl-tmux/protocol:read-u32 buffer 0)))
      (expect (= 256    (cl-tmux/protocol:read-u32 buffer 4)))
      (let ((buf2 (cl-tmux/protocol:u32-octets #xDEADBEEF)))
        (expect (= #xDEADBEEF (cl-tmux/protocol:read-u32 buf2 0))))))

  ;; ── split-on-nul-bytes ──────────────────────────────────────────────────────

  ;; split-on-nul-bytes on an empty buffer returns an empty list.
  (it "split-on-nul-bytes-empty-input-returns-empty-list"
    (expect (null (cl-tmux/protocol:split-on-nul-bytes #()))))

  ;; split-on-nul-bytes with one NUL-terminated field returns a one-element list.
  (it "split-on-nul-bytes-single-field"
    (let* ((bytes (babel:string-to-octets "hello" :encoding :utf-8))
           (buf   (concatenate '(simple-array (unsigned-byte 8) (*)) bytes #(0))))
      (expect (equal '("hello") (cl-tmux/protocol:split-on-nul-bytes buf)))))

  ;; split-on-nul-bytes with multiple NUL-separated fields returns them all.
  (it "split-on-nul-bytes-multiple-fields"
    (let* ((a (babel:string-to-octets "alpha" :encoding :utf-8))
           (b (babel:string-to-octets "beta"  :encoding :utf-8))
           (c (babel:string-to-octets "gamma" :encoding :utf-8))
           (buf (concatenate '(simple-array (unsigned-byte 8) (*))
                             a #(0) b #(0) c #(0))))
      (expect (equal '("alpha" "beta" "gamma")
                     (cl-tmux/protocol:split-on-nul-bytes buf)))))

  ;; split-on-nul-bytes with no NUL byte returns an empty list (no complete field).
  (it "split-on-nul-bytes-no-nul-returns-empty-list"
    (let ((buf (babel:string-to-octets "no-nul" :encoding :utf-8)))
      (expect (null (cl-tmux/protocol:split-on-nul-bytes buf)))))

  ;; ── command-name-to-string ──────────────────────────────────────────────────

  ;; command-name-to-string downcases keywords (any case) and passes strings through unchanged.
  (it "command-name-to-string-table"
    (dolist (c '((:new-window  "new-window"  "lowercase keyword → downcased")
                 (:NEW-WINDOW  "new-window"  "uppercase keyword → downcased")
                 (:SELECT-PANE "select-pane" "uppercase keyword → downcased")
                 ("select-pane" "select-pane" "string → pass through")))
      (destructuring-bind (input expected desc) c
        (declare (ignore desc))
        (expect (string= expected (cl-tmux/protocol:command-name-to-string input))))))

  ;; ── assemble-command-fields ─────────────────────────────────────────────────

  ;; assemble-command-fields orders fields as [target] name [args...].
  (it "assemble-command-fields-table"
    (dolist (c '(("new-window"  nil    nil          ("new-window")               "name only")
                 ("select-pane" "$1:0" nil          ("$1:0" "select-pane")       "target + name")
                 ("send-keys"   nil    ("C-c" "")   ("send-keys" "C-c" "")       "name + args")
                 ("resize-pane" "2:0"  ("-U" "5")   ("2:0" "resize-pane" "-U" "5") "target + name + args")))
      (destructuring-bind (name target args expected desc) c
        (declare (ignore desc))
        (expect (equal expected (cl-tmux/protocol:assemble-command-fields name target args))))))

  ;; ── encode-fields-to-buffer ─────────────────────────────────────────────────

  ;; encode-fields-to-buffer with no fields produces an empty buffer.
  (it "encode-fields-to-buffer-empty-fields-produces-empty-buffer"
    (let ((buf (cl-tmux/protocol:encode-fields-to-buffer '())))
      (expect (= 0 (length buf)))))

  ;; encode-fields-to-buffer packs one field followed by a NUL byte.
  (it "encode-fields-to-buffer-single-field-has-trailing-nul"
    (let* ((field-bytes (babel:string-to-octets "hello" :encoding :utf-8))
           (buf (cl-tmux/protocol:encode-fields-to-buffer (list field-bytes))))
      (expect (= 6 (length buf)))
      (expect (= 0 (aref buf 5)))))

  ;; encode-fields-to-buffer places a NUL after each field.
  (it "encode-fields-to-buffer-multiple-fields-split-by-nuls"
    (let* ((f1  (babel:string-to-octets "ab" :encoding :utf-8))
           (f2  (babel:string-to-octets "cd" :encoding :utf-8))
           (buf (cl-tmux/protocol:encode-fields-to-buffer (list f1 f2))))
      ;; Layout: a b NUL c d NUL → 6 bytes
      (expect (= 6 (length buf)))
      (expect (= 0 (aref buf 2)))
      (expect (= 0 (aref buf 5)))))

  ;; ── to-octets ───────────────────────────────────────────────────────────────

  ;; to-octets coerces a list of octets to a simple (unsigned-byte 8) vector.
  (it "to-octets-coerces-list-to-simple-vector"
    (let ((result (to-octets '(1 2 3))))
      (expect (typep result '(simple-array (unsigned-byte 8) (*))))
      (expect (equalp #(1 2 3) result))))

  ;; to-octets on an already-simple octet vector returns an equivalent vector.
  (it "to-octets-idempotent-on-simple-vector"
    (let* ((original #(10 20 30))
           (result   (to-octets original)))
      (expect (equalp original result))))

  ;; ── decode-size / decode-text edge cases ────────────────────────────────────

  ;; decode-size decodes a (0,0) payload correctly.
  (it "decode-size-zero-rows-zero-cols"
    (multiple-value-bind (rows cols) (decode-size (u16-octets-pair 0 0))
      (expect (= 0 rows))
      (expect (= 0 cols))))

  ;; decode-size round-trips the maximum u16 values (65535 x 65535).
  (it "decode-size-max-u16-values"
    (multiple-value-bind (rows cols) (decode-size (u16-octets-pair 65535 65535))
      (expect (= 65535 rows))
      (expect (= 65535 cols))))

  ;; decode-text on an empty octet vector returns an empty string.
  (it "decode-text-empty-payload"
    (expect (string= "" (decode-text #()))))

  ;; decode-text decodes a plain ASCII payload to a string.
  (it "decode-text-ascii"
    (let ((bytes (babel:string-to-octets "hello" :encoding :utf-8)))
      (expect (string= "hello" (decode-text bytes)))))

  ;; ── decode-command-payload empty / degenerate input ─────────────────────────

  ;; decode-command-payload on a zero-byte payload returns (values NIL NIL NIL)
  ;; without signalling; the caller must handle the empty-fields case explicitly.
  (it "decode-command-payload-empty-payload-returns-nil-values"
    (multiple-value-bind (command target args)
        (decode-command-payload #())
      (expect (null command))
      (expect (null target))
      (expect (null args))))

  ;; decode-command-payload on a payload with no NUL terminator returns
  ;; (values NIL NIL NIL) — no NUL means no complete field was transmitted.
  (it "decode-command-payload-no-nul-byte-returns-nil-values"
    (let ((payload (babel:string-to-octets "no-nul-here" :encoding :utf-8)))
      (multiple-value-bind (command target args)
          (decode-command-payload payload)
        (expect (null command))
        (expect (null target))
        (expect (null args)))))

  ;; ── msg-command edge cases ───────────────────────────────────────────────────

  ;; msg-command with an explicit empty args list produces the same frame as NIL args.
  (it "msg-command-empty-args-list-roundtrips"
    (let ((frame-nil  (msg-command :new-window nil nil))
          (frame-list (msg-command :new-window nil '())))
      (expect (equalp frame-nil frame-list))))

  ;; msg-command accepts a plain string command-name (not a keyword).
  (it "msg-command-string-command-name-roundtrips"
    (let ((frame (msg-command "split-window" nil nil)))
      (multiple-value-bind (type payload) (decode-frame frame)
        (expect (= +msg-command+ type))
        (multiple-value-bind (command target args)
            (decode-command-payload payload)
          (expect (eq :split-window command))
          (expect (null target))
          (expect (null args))))))

  ;; ── to-octets on an empty list ───────────────────────────────────────────────

  ;; to-octets on an empty list produces an empty (unsigned-byte 8) vector.
  (it "to-octets-empty-list-produces-empty-vector"
    (let ((result (to-octets '())))
      (expect (typep result '(simple-array (unsigned-byte 8) (*))))
      (expect (= 0 (length result)))))

  ;; ── split-on-nul-bytes trailing data after final NUL ─────────────────────────

  ;; split-on-nul-bytes ignores bytes that follow the final NUL (incomplete field).
  (it "split-on-nul-bytes-trailing-bytes-after-last-nul-are-ignored"
    (let* ((a     (babel:string-to-octets "alpha" :encoding :utf-8))
           ;; 'beta' bytes appended WITHOUT a terminating NUL.
           (b     (babel:string-to-octets "beta"  :encoding :utf-8))
           (buf   (concatenate '(simple-array (unsigned-byte 8) (*))
                               a #(0) b)))
      (expect (equal '("alpha")
                     (cl-tmux/protocol:split-on-nul-bytes buf)))))

  ;; ── assemble-command-fields preserves arg order ──────────────────────────────

  ;; assemble-command-fields appends many args in the supplied order.
  (it "assemble-command-fields-preserves-multiple-args-order"
    (expect (equal '("cmd" "a" "b" "c" "d")
                   (cl-tmux/protocol:assemble-command-fields "cmd" nil '("a" "b" "c" "d")))))

  ;; ── encode-fields-to-buffer / split-on-nul-bytes are symmetric ──────────────

  ;; Encoding a list of strings with encode-fields-to-buffer and decoding with
  ;; split-on-nul-bytes must recover the original strings.
  (it "encode-fields-to-buffer-and-split-on-nul-bytes-are-symmetric"
    (let* ((strings  '("alpha" "beta" "gamma" "delta"))
           (octets   (mapcar (lambda (s)
                               (babel:string-to-octets s :encoding :utf-8))
                             strings))
           (buf      (cl-tmux/protocol:encode-fields-to-buffer octets))
           (decoded  (cl-tmux/protocol:split-on-nul-bytes buf)))
      (expect (equal strings decoded)))))
