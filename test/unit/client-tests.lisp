(in-package #:cl-tmux/test)

;;;; Client lifecycle tests (src/client.lisp).
;;;;
;;;; run-client itself is integration-level (requires a live socket and raw
;;;; terminal) but its building blocks are unit-testable:
;;;;
;;;;   * socket-path naming — pure string function
;;;;   * with-incoming-frame dispatch — the same Prolog-dispatch macro used by
;;;;     both server (serve-client) and client (run-client); tested via a real
;;;;     Unix-domain socket roundtrip (same technique as net-tests.lisp), guarded
;;;;     by unix-socket-available-p so tests self-skip in restricted sandboxes
;;;;   * msg-command encoding — verifies the detach-others frame type

(def-suite client-suite :description "Client connect/detach lifecycle")
(in-suite client-suite)

;;; ── Function existence ───────────────────────────────────────────────────────

(test client-run-client-is-defined
  :description "run-client is a defined function (integration tested via e2e-smoke)."
  (is (fboundp 'cl-tmux::run-client) "run-client must be defined"))

;;; socket-path naming is tested canonically in server-tests.lisp since
;;; socket-path is defined in server.lisp.  No duplicate tests here.

;;; ── with-incoming-frame dispatch (socket roundtrip) ─────────────────────────
;;;
;;; These tests drive with-incoming-frame directly via a Unix-domain socket
;;; stream pair.  We write frames from one end and read from the other, exactly
;;; as run-client does.  The macro is in cl-tmux/transport and is used by both
;;; server (serve-client) and client (run-client).

(defun %client-test-socket-path ()
  "Unique throwaway socket path for client dispatch tests."
  (let ((dir (string-right-trim "/" (or (sb-ext:posix-getenv "TMPDIR") "/tmp"))))
    (format nil "~A/cl-tmux-client-dispatch-test-~D.sock" dir (get-universal-time))))

(defmacro with-client-test-socket-pair ((writer-stream reader-stream) &body body)
  "Create a Unix-domain socket pair: listener→accept→connect.
   WRITER-STREAM and READER-STREAM are bidirectional binary streams.
   Writer side simulates the server sending frames; reader side reads them
   (matches the run-client perspective where the server writes and client reads)."
  (let ((path    (gensym "PATH"))
        (lstnr   (gensym "LSTNR"))
        (wsock   (gensym "WSOCK"))
        (rsock   (gensym "RSOCK")))
    `(let ((,path (%client-test-socket-path)))
       (let ((,lstnr (make-listener ,path)))
         (unwind-protect
              (let* ((,rsock (connect-to ,path))
                     (,wsock (accept-connection ,lstnr))
                     (,writer-stream (socket-stream ,wsock))
                     (,reader-stream (socket-stream ,rsock)))
                (unwind-protect
                     (progn ,@body)
                  (ignore-errors (close-socket ,wsock))
                  (ignore-errors (close-socket ,rsock))))
           (ignore-errors (close-socket ,lstnr))
           (ignore-errors (delete-file ,path)))))))

(defmacro with-guarded-socket-test (&body body)
  "Skip unless Unix-domain sockets are available, then run BODY under a 10-second
   timeout inside a socket-pair.  Eliminates the repeated three-line boilerplate:
     (unless (unix-socket-available-p) (skip ...))
     (sb-ext:with-timeout 10 ...)
     (with-client-test-socket-pair ...)
   that appeared in every socket-roundtrip test."
  (let ((server-side (gensym "SERVER-SIDE"))
        (client-side (gensym "CLIENT-SIDE")))
    `(progn
       (unless (unix-socket-available-p)
         (skip "Unix-domain socket unavailable (sandbox)"))
       (sb-ext:with-timeout 10
         (with-client-test-socket-pair (,server-side ,client-side)
           (symbol-macrolet ((server-side ,server-side)
                             (client-side ,client-side))
             ,@body))))))

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

;;; ── %ensure-server-running existence ─────────────────────────────────────────

(test client-ensure-server-running-is-fbound
  :description "%ensure-server-running is a defined function."
  (is (fboundp 'cl-tmux::%ensure-server-running)
      "%ensure-server-running must be fbound"))

;;; ── run-attach-simple existence ──────────────────────────────────────────────

(test client-run-attach-simple-is-fbound
  :description "run-attach-simple is a defined function (replaces the former run-client-with-autostart wrapper)."
  (is (fboundp 'cl-tmux::run-attach-simple)
      "run-attach-simple must be fbound"))

;;; ── *startup-modes* dispatch table ──────────────────────────────────────────

(test startup-modes-has-server-entry
  :description "*startup-modes* contains a 'server' entry whose handler is run-server."
  (let ((entry (assoc "server" cl-tmux::*startup-modes* :test #'equal)))
    (is-true entry "*startup-modes* must have a 'server' entry")
    ;; Entry cdr is a plist: (handler-symbol &key :raw-args-p bool).
    (is (eq 'cl-tmux::run-server (first (cdr entry)))
        "server entry handler must be run-server")))

;;; startup-modes-has-attach-entry and startup-modes-has-attach-session-entry
;;; are subsumed by startup-modes-attach-handler-is-run-attach-simple and
;;; startup-modes-attach-session-handler-is-run-attach-with-flags below.

;;; %startup-mode-raw-args-p is tested canonically in main-tests.lisp.

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

;;; ── run-attach-with-flags existence ─────────────────────────────────────────

(test client-run-attach-with-flags-is-fbound
  :description "run-attach-with-flags is a defined function (the attach-session mode handler)."
  (is (fboundp 'cl-tmux::run-attach-with-flags)
      "run-attach-with-flags must be fbound"))

;;; ── *startup-modes* handler symbols are symbols ──────────────────────────────
;;;
;;; Handlers stored as symbols (not function objects) is the key architectural
;;; property that makes test stubs with SETF FDEFINITION work.

(test startup-modes-all-handlers-are-symbols
  :description "Every entry in *startup-modes* stores its handler as a symbol, not a
   function object.  This is required so test stubs with (setf fdefinition) work."
  (dolist (entry cl-tmux::*startup-modes*)
    (let ((handler (first (cdr entry))))
      (is (symbolp handler)
          "handler for mode ~S must be a symbol, got ~S"
          (car entry) handler))))

(test startup-modes-attach-handler-is-run-attach-simple
  :description "*startup-modes* 'attach' entry handler is run-attach-simple."
  (let ((entry (assoc "attach" cl-tmux::*startup-modes* :test #'equal)))
    (is-true entry "*startup-modes* must have an 'attach' entry")
    (is (eq 'cl-tmux::run-attach-simple (first (cdr entry)))
        "attach entry handler must be run-attach-simple")))

(test startup-modes-attach-session-handler-is-run-attach-with-flags
  :description "*startup-modes* 'attach-session' entry handler is run-attach-with-flags
   and has :raw-args-p T so it receives the full argv tail."
  (let ((entry (assoc "attach-session" cl-tmux::*startup-modes* :test #'equal)))
    (is-true entry "*startup-modes* must have an 'attach-session' entry")
    (is (eq 'cl-tmux::run-attach-with-flags (first (cdr entry)))
        "attach-session handler must be run-attach-with-flags")
    (is-true (getf (rest (cdr entry)) :raw-args-p)
             "attach-session entry must have :raw-args-p T")))

;;; ── with-incoming-frame EOF (nil type) arm via empty file stream ─────────────
;;;
;;; We test the nil-type (EOF) arm of with-incoming-frame by reading from an
;;; empty temp file — no socket required.  An empty stream causes read-frame to
;;; return nil (no complete header), which triggers the (null type) arm.

(test client-with-incoming-frame-eof-dispatches
  :description "with-incoming-frame dispatches the nil type (EOF) arm when the stream
is empty — no complete frame header can be read."
  (with-temp-octet-file (path)
    ;; Create an empty file, then immediately read it — EOF on first byte.
    (with-open-file (_out path :direction :output :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
      (declare (ignore _out)))
    (with-open-file (stream path :direction :input
                                 :element-type '(unsigned-byte 8))
      (let ((dispatched nil))
        (with-incoming-frame (type _payload stream)
          ((null type)        (setf dispatched :eof))
          ((= type +msg-bye+) (setf dispatched :bye)))
        (is (eq :eof dispatched)
            "empty stream must dispatch the nil-type (EOF) arm")))))
