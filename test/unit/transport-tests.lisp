(in-package #:cl-tmux/test)

;;;; Frame transport tests (src/transport.lisp).
;;;;
;;;; send-frame / read-frame move protocol frames across a binary stream.  We
;;;; exercise them over a temp-file octet stream (no socket needed): write a
;;;; sequence of frames, then read them back and assert type/payload and clean
;;;; end-of-stream handling.  A binary file stream behaves like a socket stream
;;;; for these purposes (blocking read-sequence).
;;;;
;;;; with-temp-octet-file and write-frames-to-file are defined in test/helpers.lisp
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
      (is (= +msg-detach+ (read-frame in)) "first frame reads back")
      (is (null (read-frame in)) "second read hits EOF → NIL"))))

(test transport-truncated-frame-returns-nil
  "A stream that ends mid-frame (truncated payload) yields NIL, not garbage."
  (with-temp-octet-file (path)
    ;; Write only the first 4 bytes of a key frame (header is 5 bytes).
    (let ((frame (msg-key #(1 2 3))))
      (with-open-file (out path :direction :output :if-exists :supersede
                                :element-type '(unsigned-byte 8))
        (write-sequence (subseq frame 0 4) out)))
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (is (null (read-frame in)) "incomplete header → NIL"))))

(test transport-truncated-payload-returns-nil
  "A full 5-byte header but only part of the payload yields NIL (mid-frame)."
  (with-temp-octet-file (path)
    ;; A key frame with a 3-byte payload: 5-byte header + 3 payload = 8 bytes.
    ;; Write the whole header plus only the first payload byte (6 of 8 bytes).
    (let ((frame (msg-key #(1 2 3))))
      (is (= 8 (length frame)) "header(5) + payload(3)")
      (with-open-file (out path :direction :output :if-exists :supersede
                                :element-type '(unsigned-byte 8))
        (write-sequence (subseq frame 0 6) out)))
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
