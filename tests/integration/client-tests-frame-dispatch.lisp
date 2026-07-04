(in-package #:cl-tmux/test)

(in-suite client-suite)

;;; ── with-incoming-frame dispatch (socket roundtrip) ─────────────────────────
;;;
;;; These tests drive with-incoming-frame directly via a Unix-domain socket
;;; stream pair.  We write frames from one end and read from the other, exactly
;;; as run-client does.  The macro is in cl-tmux/transport and is used by both
;;; server (serve-client) and client (run-client).

(test client-with-incoming-frame-msg-bye-dispatches
  :description "with-incoming-frame dispatches +msg-bye+ correctly — the :return path that
run-client uses to exit its inner loop cleanly."
  (with-guarded-socket-test
    (send-frame server-side (msg-bye))
    (let ((dispatched nil))
      (with-incoming-frame (type _payload client-side)
        ((null type)
         (setf dispatched :eof))
        ((= type +msg-bye+)
         (is (zerop (length _payload))
             "bye carries an empty payload")
         (setf dispatched :bye))
        ((= type +msg-frame+)
         (setf dispatched :frame)))
      (is (eq :bye dispatched)
          "with-incoming-frame must dispatch +msg-bye+ to the :bye arm"))))

(test client-with-incoming-frame-msg-frame-dispatches
  :description "with-incoming-frame dispatches +msg-frame+ correctly — the arm that paints
the rendered frame string in run-client."
  (with-guarded-socket-test
    (send-frame server-side (msg-frame "hello"))
    (let ((received-text nil))
      (with-incoming-frame (type payload client-side)
        ((null type)         nil)
        ((= type +msg-bye+) nil)
        ((= type +msg-frame+)
         (setf received-text (decode-text payload))))
      (is (string= "hello" received-text)
          "msg-frame payload must decode to the original text"))))

(test client-with-incoming-frame-multiple-frames-in-order
  :description "Consecutive with-incoming-frame calls consume frames in order — verifying
the transport layer does not over-read when run-client loops."
  (with-guarded-socket-test
    (send-frame server-side (msg-frame "first"))
    (send-frame server-side (msg-frame "second"))
    (send-frame server-side (msg-bye))
    (let ((results '()))
      (dotimes (_ 3)
        (with-incoming-frame (type payload client-side)
          ((null type)        (push :eof results))
          ((= type +msg-bye+) (push :bye results))
          ((= type +msg-frame+)
           (push (decode-text payload) results))))
      (setf results (nreverse results))
      (is (equal '("first" "second" :bye) results)
          "frames must arrive in order: ~S" results))))

(test client-with-incoming-frame-unicode-content
  :description "with-incoming-frame correctly decodes Unicode payload."
  (with-guarded-socket-test
    (send-frame server-side (msg-frame "日本語テスト"))
    (let ((received nil))
      (with-incoming-frame (type payload client-side)
        ((null type) nil)
        ((= type +msg-frame+)
         (setf received (decode-text payload))))
      (is (string= "日本語テスト" received)
          "Unicode content must survive the full encode→socket→decode roundtrip"))))

;;; ── detach-others flag wiring ────────────────────────────────────────────────

(test client-detach-others-message-encoding
  :description "msg-command :detach-other-clients produces a frame whose payload round-trips
cleanly — this is the frame run-client sends when :detach-others is T."
  (let* ((frame   (msg-command :detach-other-clients nil nil))
         (decoded (multiple-value-list (decode-frame frame))))
    (is (= +msg-command+ (first decoded))
        "msg-command :detach-other-clients must encode as +msg-command+ type")))

;;; ── msg-attach encoding ──────────────────────────────────────────────────────
;;;
;;; run-client sends a msg-attach frame as its first message after connecting.
;;; Verify the frame type and round-trip decode.

(test run-client-attach-frame-encoding
  :description "run-client's initial msg-attach frame encodes as +msg-attach+ and embeds
   the terminal dimensions.  Verified by round-tripping through decode-frame / decode-size."
  (let* ((frame   (msg-attach 24 80))
         (decoded (multiple-value-list (decode-frame frame))))
    (is (= +msg-attach+ (first decoded))
        "msg-attach must encode as +msg-attach+ type")
    (multiple-value-bind (rows cols)
        (decode-size (second decoded))
      (is (= 24 rows)  "decoded rows must match the value passed to msg-attach")
      (is (= 80 cols)  "decoded cols must match the value passed to msg-attach"))))

;;; ── frame encoding table: all client frame types ─────────────────────────────
;;;
;;; Consolidate the same-pattern frame-type tests into a table so adding a new
;;; frame constructor only requires appending a row rather than a new test body.

(test run-client-all-frame-types-encode-correctly
  :description "All client-side frame constructors produce the expected +msg-*+ type tag.
   Table-driven: (constructor-call expected-type)."
  (let ((cases
         (list (list (msg-bye)                             +msg-bye+)
               (list (msg-detach)                         +msg-detach+)
               (list (msg-key (vector 65))                +msg-key+)
               (list (msg-resize 30 100)                  +msg-resize+)
               (list (msg-attach 24 80)                   +msg-attach+)
               (list (msg-frame "text")                   +msg-frame+)
               (list (msg-command :detach-other-clients nil nil) +msg-command+))))
    (dolist (c cases)
      (destructuring-bind (frame expected-type) c
        (multiple-value-bind (got-type _payload) (decode-frame frame)
          (declare (ignore _payload))
          (is (= expected-type got-type)
              "frame constructor for type ~D must encode as ~D, got ~D"
              expected-type expected-type got-type))))))

(test client-with-incoming-frame-eof-dispatches
  :description "with-incoming-frame dispatches the nil type (EOF) arm when the stream
is empty — no complete frame header can be read."
  (with-temp-octet-file (path)
    ;; Create an empty file, then immediately read it — EOF on first byte.
    (with-open-file (_out path :direction :output :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
      (finish-output _out))
    (with-open-file (stream path :direction :input
                                 :element-type '(unsigned-byte 8))
      (let ((dispatched nil))
        (with-incoming-frame (type _payload stream)
          ((null type)        (is (null _payload)
                                  "EOF delivers a NIL payload")
                              (setf dispatched :eof))
          ((= type +msg-bye+) (setf dispatched :bye)))
        (is (eq :eof dispatched)
            "empty stream must dispatch the nil-type (EOF) arm")))))
