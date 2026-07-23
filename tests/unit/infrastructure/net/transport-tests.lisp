(in-package #:cl-tmux/test)

;;;; Frame transport tests (src/transport.lisp) — part I: round-trips.
;;;;
;;;; send-frame / read-frame move protocol frames across a binary stream.  We
;;;; exercise them over a temp-file octet stream (no socket needed): write a
;;;; sequence of frames, then read them back and assert type/payload and clean
;;;; end-of-stream handling.  A binary file stream behaves like a socket stream
;;;; for these purposes (blocking read-sequence).
;;;;
;;;; with-temp-octet-file and write-frames-to-file are defined in tests/helpers-net-protocol.lisp
;;;; and shared with net-tests.lisp to avoid duplicating the temp-file idiom.
;;;;
;;;; Validation, security-boundary, and CPS-phase-direct coverage lives in
;;;; transport-tests-b.lisp (same transport-suite).

(describe "transport-suite"

  ;; A single frame written with send-frame reads back intact via read-frame.
  (it "transport-roundtrips-a-frame"
    (with-temp-octet-file (path)
      (write-frames-to-file path (msg-resize 24 80))
      (with-open-file (in path :element-type '(unsigned-byte 8))
        (multiple-value-bind (type payload) (read-frame in)
          (expect (= +msg-resize+ type))
          (multiple-value-bind (rows cols) (decode-size payload)
            (expect (= 24 rows))
            (expect (= 80 cols)))))))

  ;; Several frames in one stream read back one at a time, in order.
  (it "transport-reads-sequential-frames"
    (with-temp-octet-file (path)
      (write-frames-to-file path (msg-key #(65 66)) (msg-detach) (msg-frame "hi あ"))
      (with-open-file (in path :element-type '(unsigned-byte 8))
        (multiple-value-bind (type payload) (read-frame in)
          (expect (= +msg-key+ type))
          (expect (equalp #(65 66) payload)))
        (multiple-value-bind (type payload) (read-frame in)
          (declare (ignore payload))
          (expect (= +msg-detach+ type)))
        (multiple-value-bind (type payload) (read-frame in)
          (expect (= +msg-frame+ type))
          (expect (string= "hi あ" (decode-text payload)))))))

  ;; read-frame on an exhausted stream returns NIL (peer closed).
  (it "transport-read-at-eof-returns-nil"
    (with-temp-octet-file (path)
      (write-frames-to-file path (msg-detach))
      (with-open-file (in path :element-type '(unsigned-byte 8))
        (multiple-value-bind (type payload) (read-frame in)
          (declare (ignore payload))
          (expect (= +msg-detach+ type)))
        (expect (null (read-frame in))))))

  ;; A stream that ends mid-frame (truncated payload) yields NIL, not garbage.
  (it "transport-truncated-frame-returns-nil"
    (with-temp-octet-file (path)
      ;; Write only the first 4 bytes of a key frame (header is 5 bytes).
      (let ((frame (msg-key #(1 2 3))))
        (write-partial-frame-to-file path frame 4))
      (with-open-file (in path :element-type '(unsigned-byte 8))
        (expect (null (read-frame in))))))

  ;; A full 5-byte header but only part of the payload yields NIL (mid-frame).
  (it "transport-truncated-payload-returns-nil"
    (with-temp-octet-file (path)
      ;; A key frame with a 3-byte payload: 5-byte header + 3 payload = 8 bytes.
      ;; Write the whole header plus only the first payload byte (6 of 8 bytes).
      (let ((frame (msg-key #(1 2 3))))
        (expect (= 8 (length frame)))
        (write-partial-frame-to-file path frame 6))
      (with-open-file (in path :element-type '(unsigned-byte 8))
        (expect (null (read-frame in))))))

  ;; A frame with an empty payload flushes and round-trips intact.
  (it "transport-empty-payload-frame-roundtrips"
    (with-temp-octet-file (path)
      (write-frames-to-file path (msg-detach))
      (with-open-file (in path :element-type '(unsigned-byte 8))
        (multiple-value-bind (type payload) (read-frame in)
          (expect (= +msg-detach+ type))
          (expect (zerop (length payload)))))))

  ;; send-frame then read-frame returns the same type/payload for attach and bye.
  (it "transport-roundtrips-attach-and-bye-frames"
    (with-temp-octet-file (path)
      (write-frames-to-file path (msg-attach 30 120) (msg-bye))
      (with-open-file (in path :element-type '(unsigned-byte 8))
        (multiple-value-bind (type payload) (read-frame in)
          (expect (= +msg-attach+ type))
          (multiple-value-bind (rows cols) (decode-size payload)
            (expect (= 30 rows))
            (expect (= 120 cols))))
        (multiple-value-bind (type payload) (read-frame in)
          (expect (= +msg-bye+ type))
          (expect (zerop (length payload))))
        (expect (null (read-frame in))))))

  ;;; ── with-incoming-frame ──────────────────────────────────────────────────────

  ;; with-incoming-frame reads one frame and dispatches on its type constant.
  (it "with-incoming-frame-dispatches-on-type"
    (with-temp-octet-file (path)
      (write-frames-to-file path (msg-detach))
      (with-open-file (in path :element-type '(unsigned-byte 8))
        (let ((dispatched nil))
          (with-incoming-frame (type payload in)
            ((= type +msg-detach+) (expect (zerop (length payload)))
                                   (setf dispatched :detach))
            (t (setf dispatched :other)))
          (expect (eq :detach dispatched))))))

  ;; with-incoming-frame binds the payload variable for the matching clause.
  (it "with-incoming-frame-binds-payload"
    (with-temp-octet-file (path)
      (write-frames-to-file path (msg-key #(65 66 67)))
      (with-open-file (in path :element-type '(unsigned-byte 8))
        (let ((captured nil))
          (with-incoming-frame (type payload in)
            ((= type +msg-key+) (setf captured payload))
            (t nil))
          (expect (equalp #(65 66 67) captured))))))

  ;; At EOF, with-incoming-frame delivers NIL type; a nil-check rule can handle it.
  (it "with-incoming-frame-eof-falls-through-to-nil-type"
    (with-temp-octet-file (path)
      ;; Write nothing — create an empty file so the stream is immediately at EOF.
      (with-open-file (in path :element-type '(unsigned-byte 8)
                               :if-does-not-exist :create)
        (let ((hit-eof nil))
          (with-incoming-frame (type payload in)
            ((null type) (expect (null payload))
                         (setf hit-eof t))
            (t nil))
          (expect hit-eof :to-be-truthy)))))

  ;; with-incoming-frame evaluates the first matching clause and skips the rest.
  (it "with-incoming-frame-first-matching-clause-wins"
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
          (expect (equal '(:resize 10 20) result))))))

  ;;; ── Table-driven round-trip: all typed constructors ─────────────────────────

  ;; Every typed message constructor survives a send-frame / read-frame round-trip.
  (it "transport-all-typed-constructors-roundtrip"
    (assert-round-tripped-frame-type (msg-attach  24 80)           +msg-attach+)
    (assert-round-tripped-frame-type (msg-key     #(27 65))        +msg-key+)
    (assert-round-tripped-frame-type (msg-resize  30 100)          +msg-resize+)
    (assert-round-tripped-frame-type (msg-detach)                  +msg-detach+)
    (assert-round-tripped-frame-type (msg-frame   "hi")            +msg-frame+)
    (assert-round-tripped-frame-type (msg-bye)                     +msg-bye+)
    (assert-round-tripped-frame-type (msg-reply   "output text")   +msg-reply+)
    (assert-round-tripped-frame-type (msg-command :new-window nil nil) +msg-command+))

  ;; Each typed constructor's payload survives send-frame / read-frame intact.
  (it "transport-typed-constructors-payload-roundtrip"
    ;; msg-attach: decode-size must recover rows and cols
    (assert-round-tripped-frame-payload
     (msg-attach 15 60)
     (lambda (payload)
       (multiple-value-bind (rows cols) (cl-tmux/protocol:decode-size payload)
         (expect (= 15 rows))
         (expect (= 60 cols)))))
    ;; msg-key: payload bytes must be preserved verbatim
    (assert-round-tripped-frame-payload
     (msg-key #(27 91 65))
     (lambda (payload)
       (expect (equalp #(27 91 65) payload))))
    ;; msg-resize: decode-size must recover rows and cols
    (assert-round-tripped-frame-payload
     (msg-resize 50 200)
     (lambda (payload)
       (multiple-value-bind (rows cols) (cl-tmux/protocol:decode-size payload)
         (expect (= 50 rows))
         (expect (= 200 cols)))))
    ;; msg-frame: decode-text must recover the Unicode string
    (assert-round-tripped-frame-payload
     (msg-frame "こんにちは")
     (lambda (payload)
       (expect (string= "こんにちは" (cl-tmux/protocol:decode-text payload)))))
    ;; msg-reply: decode-text must recover the UTF-8 reply text
    (assert-round-tripped-frame-payload
     (msg-reply "output: 42")
     (lambda (payload)
       (expect (string= "output: 42" (cl-tmux/protocol:decode-text payload)))))
    ;; msg-command: decode-command-payload must recover command, target, and args
    (assert-round-tripped-frame-payload
     (msg-command :new-window "$0" '("-d"))
     (lambda (payload)
       (multiple-value-bind (command target args)
           (cl-tmux/protocol:decode-command-payload payload)
         (expect (eq :new-window command))
         (expect (string= "$0" target))
         (expect (equal '("-d") args))))))

  ;;; ── msg-command frame transport round-trip ───────────────────────────────────

  ;; A msg-command frame (command name, target, args) survives a send-frame / read-frame cycle.
  (it "transport-msg-command-frame-roundtrips"
    (with-temp-octet-file (path)
      (write-frames-to-file path (msg-command :new-session "$0" '("-d" "-s" "main")))
      (with-open-file (in path :element-type '(unsigned-byte 8))
        (multiple-value-bind (type payload) (read-frame in)
          (expect (= +msg-command+ type))
          (multiple-value-bind (command target args)
              (cl-tmux/protocol:decode-command-payload payload)
            (expect (eq :new-session command))
            (expect (string= "$0" target))
            (expect (equal '("-d" "-s" "main") args)))))))

  ;;; ── Large payload transport ──────────────────────────────────────────────────

  ;; A frame with a 65536-byte payload survives a send-frame / read-frame cycle intact.
  (it "transport-large-payload-roundtrips"
    (let* ((n       65536)
           (payload (make-array n :element-type '(unsigned-byte 8))))
      (dotimes (i n) (setf (aref payload i) (logand i #xFF)))
      (with-temp-octet-file (path)
        (write-frames-to-file path (encode-frame +msg-frame+ payload))
        (with-open-file (in path :element-type '(unsigned-byte 8))
          (multiple-value-bind (type decoded) (read-frame in)
            (expect (= +msg-frame+ type))
            (expect (= n (length decoded)))
            (expect (equalp payload decoded)))))))

  ;;; ── send-frame writes octet vector without signalling ─────────────────────────

  ;; send-frame on an open binary output stream must not signal.
  (it "send-frame-finishes-without-signalling"
    (with-temp-octet-file (path)
      (with-output-octet-stream (out path)
        (finishes (send-frame out (msg-detach))
                  "send-frame must not signal on a valid binary output stream"))))

  ;;; ── Transport constant values ────────────────────────────────────────────────

  ;; The +read-frame-timeout-seconds+ constant must be a positive integer so
  ;; sb-ext:with-timeout receives a valid duration.
  (it "read-frame-timeout-constant-is-positive-integer"
    (expect (integerp cl-tmux/transport::+read-frame-timeout-seconds+))
    (expect (plusp cl-tmux/transport::+read-frame-timeout-seconds+)))

  ;; +max-frame-payload-bytes+ must be a large positive integer (≥ 1 MiB) so
  ;; that the security guard in read-frame accepts realistic frame sizes.
  (it "max-frame-payload-constant-is-large-positive-integer"
    (expect (integerp cl-tmux/transport::+max-frame-payload-bytes+))
    (expect (>= cl-tmux/transport::+max-frame-payload-bytes+ (* 1024 1024))))

  ;;; ── %read-exact direct contract tests ───────────────────────────────────────
  ;;;
  ;;; These tests exercise the %read-exact helper directly using a file stream so
  ;;; that the exact-byte-count contract is verified in isolation from read-frame.
  ;;; The key invariant: %read-exact returns the actual number of bytes read, which
  ;;; equals (- end start) on a normal read but is less than that at EOF.

  ;; %read-exact reads exactly (- end start) bytes from a stream with enough data.
  (it "read-exact-fills-buffer-exactly"
    (with-temp-octet-file (path)
      ;; Write 10 known bytes to the file.
      (with-output-octet-stream (out path)
        (write-sequence #(10 20 30 40 50 60 70 80 90 100) out))
      (with-open-file (in path :element-type '(unsigned-byte 8))
        (let ((buffer (make-array 10 :element-type '(unsigned-byte 8))))
          (let ((bytes-read (cl-tmux/transport::%read-exact buffer in 0 10)))
            (expect (= 10 bytes-read))
            (expect (equalp #(10 20 30 40 50 60 70 80 90 100) buffer)))))))

  ;; %read-exact returns less than requested when the stream ends before END.
  (it "read-exact-returns-short-count-at-eof"
    (with-temp-octet-file (path)
      ;; Write only 3 bytes but ask for 10.
      (with-output-octet-stream (out path)
        (write-sequence #(1 2 3) out))
      (with-open-file (in path :element-type '(unsigned-byte 8))
        (let ((buffer (make-array 10 :element-type '(unsigned-byte 8) :initial-element 0)))
          (let ((bytes-read (cl-tmux/transport::%read-exact buffer in 0 10)))
            (expect (< bytes-read 10))
            (expect (= 3 bytes-read)))))))

  ;; %read-exact writes bytes starting at the given START offset in the buffer.
  (it "read-exact-respects-start-offset"
    (with-temp-octet-file (path)
      (with-output-octet-stream (out path)
        (write-sequence #(7 8 9) out))
      (with-open-file (in path :element-type '(unsigned-byte 8))
        (let ((buffer (make-array 6 :element-type '(unsigned-byte 8) :initial-element 0)))
          ;; Read into the middle of the buffer (positions 3..6).
          (cl-tmux/transport::%read-exact buffer in 3 6)
          (expect (= 0 (aref buffer 0)))
          (expect (= 0 (aref buffer 1)))
          (expect (= 0 (aref buffer 2)))
          (expect (= 7 (aref buffer 3)))
          (expect (= 8 (aref buffer 4)))
          (expect (= 9 (aref buffer 5))))))))
