(in-package #:cl-tmux/test)

(describe "client-suite"

  ;;; ── with-incoming-frame dispatch (socket roundtrip) ─────────────────────────
  ;;;
  ;;; These tests drive with-incoming-frame directly via a Unix-domain socket
  ;;; stream pair.  We write frames from one end and read from the other, exactly
  ;;; as run-client does.  The macro is in cl-tmux/transport and is used by both
  ;;; server (serve-client) and client (run-client).

  ;; with-incoming-frame dispatches +msg-bye+ correctly — the :return path that
  ;; run-client uses to exit its inner loop cleanly.
  (it "client-with-incoming-frame-msg-bye-dispatches"
    (with-guarded-socket-test
      (send-frame server-side (msg-bye))
      (let ((dispatched nil))
        (with-incoming-frame (type _payload client-side)
          ((null type)
           (setf dispatched :eof))
          ((= type +msg-bye+)
           (expect (zerop (length _payload)))
           (setf dispatched :bye))
          ((= type +msg-frame+)
           (setf dispatched :frame)))
        (expect (eq :bye dispatched)))))

  ;; with-incoming-frame dispatches +msg-frame+ correctly — the arm that paints
  ;; the rendered frame string in run-client.
  (it "client-with-incoming-frame-msg-frame-dispatches"
    (with-guarded-socket-test
      (send-frame server-side (msg-frame "hello"))
      (let ((received-text nil))
        (with-incoming-frame (type payload client-side)
          ((null type)         nil)
          ((= type +msg-bye+) nil)
          ((= type +msg-frame+)
           (setf received-text (decode-text payload))))
        (expect (string= "hello" received-text)))))

  ;; Consecutive with-incoming-frame calls consume frames in order — verifying
  ;; the transport layer does not over-read when run-client loops.
  (it "client-with-incoming-frame-multiple-frames-in-order"
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
        (expect (equal '("first" "second" :bye) results)))))

  ;; with-incoming-frame correctly decodes Unicode payload.
  (it "client-with-incoming-frame-unicode-content"
    (with-guarded-socket-test
      (send-frame server-side (msg-frame "日本語テスト"))
      (let ((received nil))
        (with-incoming-frame (type payload client-side)
          ((null type) nil)
          ((= type +msg-frame+)
           (setf received (decode-text payload))))
        (expect (string= "日本語テスト" received)))))

  ;;; ── detach-others flag wiring ────────────────────────────────────────────────

  ;; msg-command :detach-other-clients produces a frame whose payload round-trips
  ;; cleanly — this is the frame run-client sends when :detach-others is T.
  (it "client-detach-others-message-encoding"
    (let* ((frame   (msg-command :detach-other-clients nil nil))
           (decoded (multiple-value-list (decode-frame frame))))
      (expect (= +msg-command+ (first decoded)))))

  ;;; ── msg-attach encoding ──────────────────────────────────────────────────────
  ;;;
  ;;; run-client sends a msg-attach frame as its first message after connecting.
  ;;; Verify the frame type and round-trip decode.

  ;; run-client's initial msg-attach frame encodes as +msg-attach+ and embeds
  ;; the terminal dimensions.  Verified by round-tripping through decode-frame / decode-size.
  (it "run-client-attach-frame-encoding"
    (let* ((frame   (msg-attach 24 80))
           (decoded (multiple-value-list (decode-frame frame))))
      (expect (= +msg-attach+ (first decoded)))
      (multiple-value-bind (rows cols)
          (decode-size (second decoded))
        (expect (= 24 rows))
        (expect (= 80 cols)))))

  ;;; ── frame encoding table: all client frame types ─────────────────────────────
  ;;;
  ;;; Consolidate the same-pattern frame-type tests into a table so adding a new
  ;;; frame constructor only requires appending a row rather than a new test body.

  ;; All client-side frame constructors produce the expected +msg-*+ type tag.
  ;; Table-driven: (constructor-call expected-type).
  (it "run-client-all-frame-types-encode-correctly"
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
            (expect (= expected-type got-type)))))))

  ;; with-incoming-frame dispatches the nil type (EOF) arm when the stream
  ;; is empty — no complete frame header can be read.
  (it "client-with-incoming-frame-eof-dispatches"
    (with-temp-octet-file (path)
      ;; Create an empty file, then immediately read it — EOF on first byte.
      (with-open-file (_out path :direction :output :element-type '(unsigned-byte 8)
                                :if-exists :supersede)
        (finish-output _out))
      (with-open-file (stream path :direction :input
                                   :element-type '(unsigned-byte 8))
        (let ((dispatched nil))
          (with-incoming-frame (type _payload stream)
            ((null type)        (expect (null _payload))
                                (setf dispatched :eof))
            ((= type +msg-bye+) (setf dispatched :bye)))
          (expect (eq :eof dispatched)))))))
