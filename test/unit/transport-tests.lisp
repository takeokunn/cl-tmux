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
      (multiple-value-bind (type payload) (read-frame in)
        (declare (ignore payload))
        (is (= +msg-detach+ type) "first frame reads back"))
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

;;; ── with-incoming-frame ──────────────────────────────────────────────────────

(test with-incoming-frame-dispatches-on-type
  "with-incoming-frame reads one frame and dispatches on its type constant."
  (with-temp-octet-file (path)
    (write-frames-to-file path (msg-detach))
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (let ((dispatched nil))
        (with-incoming-frame (type payload in)
          ((= type +msg-detach+) (setf dispatched :detach))
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
    ;; Write nothing — the stream is immediately at EOF.
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (let ((hit-eof nil))
        (with-incoming-frame (type payload in)
          ((null type) (setf hit-eof t))
          (t nil))
        (is-true hit-eof
                 "with-incoming-frame must reach the nil-type clause at EOF")))))

;;; ── Table-driven round-trip: all typed constructors ─────────────────────────

(test transport-all-typed-constructors-roundtrip
  "Every typed message constructor survives a send-frame / read-frame round-trip."
  (flet ((round-trip (frame expected-type)
           (with-temp-octet-file (path)
             (write-frames-to-file path frame)
             (with-open-file (in path :element-type '(unsigned-byte 8))
               (multiple-value-bind (type payload) (read-frame in)
                 (declare (ignore payload))
                 (is (= expected-type type)
                     "round-trip type mismatch: expected ~D got ~S"
                     expected-type type))))))
    (round-trip (msg-attach  24 80)    +msg-attach+)
    (round-trip (msg-key     #(27 65)) +msg-key+)
    (round-trip (msg-resize  30 100)   +msg-resize+)
    (round-trip (msg-detach)           +msg-detach+)
    (round-trip (msg-frame   "hi")     +msg-frame+)
    (round-trip (msg-bye)              +msg-bye+)))

;;; ── Transport constant value ─────────────────────────────────────────────────

(test read-frame-timeout-constant-is-positive-integer
  "The +read-frame-timeout-seconds+ constant must be a positive integer so
   sb-ext:with-timeout receives a valid duration."
  (is (integerp cl-tmux/transport::+read-frame-timeout-seconds+)
      "+read-frame-timeout-seconds+ must be an integer")
  (is (plusp cl-tmux/transport::+read-frame-timeout-seconds+)
      "+read-frame-timeout-seconds+ must be positive"))

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
  (flet ((rt (frame check-fn)
           (with-temp-octet-file (path)
             (write-frames-to-file path frame)
             (with-open-file (in path :element-type '(unsigned-byte 8))
               (multiple-value-bind (type payload) (read-frame in)
                 (declare (ignore type))
                 (funcall check-fn payload))))))
    ;; msg-attach: decode-size must recover rows and cols
    (rt (msg-attach 15 60)
        (lambda (payload)
          (multiple-value-bind (rows cols) (cl-tmux/protocol:decode-size payload)
            (is (= 15 rows) "attach rows must survive transport")
            (is (= 60 cols) "attach cols must survive transport"))))
    ;; msg-key: payload bytes must be preserved verbatim
    (rt (msg-key #(27 91 65))
        (lambda (payload)
          (is (equalp #(27 91 65) payload)
              "key payload bytes must survive transport")))
    ;; msg-resize: decode-size must recover rows and cols
    (rt (msg-resize 50 200)
        (lambda (payload)
          (multiple-value-bind (rows cols) (cl-tmux/protocol:decode-size payload)
            (is (= 50 rows)  "resize rows must survive transport")
            (is (= 200 cols) "resize cols must survive transport"))))
    ;; msg-frame: decode-text must recover the Unicode string
    (rt (msg-frame "こんにちは")
        (lambda (payload)
          (is (string= "こんにちは" (cl-tmux/protocol:decode-text payload))
              "frame UTF-8 payload must survive transport")))))
