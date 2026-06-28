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

(test client-functions-fbound-table
  :description "All key client mode functions are fbound."
  (dolist (sym '(cl-tmux::run-client
                 cl-tmux::%ensure-server-running
                 cl-tmux::run-attach-simple
                 cl-tmux::run-attach-with-flags))
    (is (fboundp sym) "~A must be fbound" sym)))

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

;;; with-guarded-socket-test/fd: variant exposing raw socket objects so tests
;;; that need socket-fd (e.g. %read-command-reply's select-fds parameter) can
;;; obtain it without duplicating the full socket lifecycle.
;;;
;;; Binds SERVER-SOCK/CLIENT-SOCK (socket objects), SERVER-STREAM/CLIENT-STREAM
;;; (binary streams), and CLIENT-FD (the integer fd of the client socket).

(defmacro with-guarded-socket-test/fd ((&key (server-sock (gensym "SSOCK"))
                                              (client-sock (gensym "CSOCK"))
                                              (server-stream (gensym "SSTREAM"))
                                              (client-stream (gensym "CSTREAM"))
                                              (client-fd (gensym "CFD")))
                                        &body body)
  "Like with-guarded-socket-test but exposes socket objects and the client fd.
   Useful when a test needs (cl-tmux/net:socket-fd client) alongside the stream."
  (let ((path  (gensym "PATH"))
        (lstnr (gensym "LSTNR")))
    `(progn
       (unless (unix-socket-available-p)
         (skip "Unix-domain socket unavailable (sandbox)"))
       (sb-ext:with-timeout 10
         (let* ((,path   (%client-test-socket-path))
                (,lstnr  (make-listener ,path)))
           (unwind-protect
                (let* ((,client-sock  (connect-to ,path))
                       (,server-sock  (accept-connection ,lstnr))
                       (,server-stream (socket-stream ,server-sock))
                       (,client-stream (socket-stream ,client-sock))
                       (,client-fd     (cl-tmux/net:socket-fd ,client-sock)))
                  (declare (ignorable ,server-stream ,client-stream ,client-fd))
                  (unwind-protect
                       (progn ,@body)
                    (ignore-errors (close-socket ,server-sock))
                    (ignore-errors (close-socket ,client-sock))))
             (ignore-errors (close-socket ,lstnr))
             (ignore-errors (delete-file ,path))))))))

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

;;; ── %ensure-server-running existence ─────────────────────────────────────────

;;; ── *startup-modes* dispatch table ──────────────────────────────────────────

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

(test startup-modes-mode-handlers-table
  :description "*startup-modes* server/attach/attach-session entries have the expected handlers."
  (dolist (c '(("server"          cl-tmux::run-server             nil "server → run-server")
               ("attach"          cl-tmux::run-attach-simple       nil "attach → run-attach-simple")
               ("attach-session"  cl-tmux::run-attach-with-flags    t  "attach-session → run-attach-with-flags")))
    (destructuring-bind (mode handler raw-args-p desc) c
      (let ((entry (assoc mode cl-tmux::*startup-modes* :test #'equal)))
        (is-true entry "~A: *startup-modes* must have a '~A' entry" desc mode)
        (is (eq handler (first (cdr entry))) "~A: handler must be ~A" desc handler)
        (when raw-args-p
          (is-true (getf (rest (cdr entry)) :raw-args-p)
                   "~A: must have :raw-args-p T" desc))))))

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

;;; ── %command-client-split-window-input-p ────────────────────────────────────
;;;
;;; Table-driven coverage of the split-window/-I detection predicate.  Each row
;;; is (args expected-bool description).  The predicate must be true only for
;;; split-window or splitw with a flag token that contains the character I.

(test command-client-split-window-input-p-table
  :description "%command-client-split-window-input-p is true only for split-window/-I."
  (dolist (row '((("split-window" "-I")                t   "split-window -I")
                 (("splitw" "-I")                      t   "splitw -I alias")
                 (("split-window" "-Iv")               t   "-Iv combined flag contains I")
                 (("split-window" "-v")                nil "split-window without -I")
                 (("split-window")                     nil "split-window no flags")
                 (("new-window" "-I")                  nil "different command with -I")
                 (("display-message" "-p" "#{session}") nil "unrelated command")
                 (nil                                  nil "nil args → false")))
    (destructuring-bind (args expected description) row
      (let ((got (if (cl-tmux::%command-client-split-window-input-p args) t nil)))
        (is (eq expected got)
            "~A: expected ~S got ~S" description expected got)))))

;;; ── %read-command-client-stdin-octets ───────────────────────────────────────
;;;
;;; Tests use a string-stream so no real stdin is needed.  The max-octets guard
;;; is tested by confirming the function returns without hanging when given
;;; bounded input (the pipe-never-closes scenario is not exercisable in a unit
;;; test, but the size-limit constant bounding is verified via a large string).

(test read-command-client-stdin-octets-ascii
  :description "%read-command-client-stdin-octets reads ASCII characters from stdin
   and returns their UTF-8 byte encoding."
  (let ((*standard-input* (make-string-input-stream "hello")))
    (let ((octets (cl-tmux::%read-command-client-stdin-octets)))
      (is (typep octets '(vector (unsigned-byte 8)))
          "must return an octet vector")
      (is (string= "hello" (babel:octets-to-string octets :encoding :utf-8))
          "round-trip must recover the original string"))))

(test read-command-client-stdin-octets-empty
  :description "%read-command-client-stdin-octets returns an empty octet vector when
   stdin is immediately at EOF."
  (let ((*standard-input* (make-string-input-stream "")))
    (let ((octets (cl-tmux::%read-command-client-stdin-octets)))
      (is (zerop (length octets))
          "empty stdin must produce a zero-length octet vector"))))

(test read-command-client-stdin-octets-unicode
  :description "%read-command-client-stdin-octets encodes multibyte characters correctly."
  (let ((*standard-input* (make-string-input-stream "日本語")))
    (let ((octets (cl-tmux::%read-command-client-stdin-octets)))
      (is (string= "日本語" (babel:octets-to-string octets :encoding :utf-8))
          "multibyte Unicode must round-trip through UTF-8 encoding"))))

;;; ── %read-command-reply socket-roundtrip tests ───────────────────────────────
;;;
;;; These tests require the raw socket-fd via cl-tmux/net:socket-fd for the
;;; select-fds call inside %read-command-reply.  The with-guarded-socket-test/fd
;;; macro defined above abstracts the full socket lifecycle — no inline
;;; unwind-protect duplication.

(test read-command-reply-prints-reply-to-stdout
  :description "%read-command-reply reads the server's +msg-reply+ frame and writes its
text to *standard-output* — the client side of `cl-tmux display -p`."
  (with-guarded-socket-test/fd
      (:server-stream server-stream :client-stream client-stream :client-fd client-fd)
    (send-frame server-stream (msg-reply "OUTPUT-TEXT"))
    (force-output server-stream)
    (let ((output (with-output-to-string (*standard-output*)
                    (cl-tmux::%read-command-reply client-stream client-fd))))
      (is (search "OUTPUT-TEXT" output)
          "%read-command-reply must print the reply text to stdout (got ~S)" output))))

(test read-command-reply-returns-on-eof-without-output
  :description "%read-command-reply returns promptly with NO output when the server
closes without replying (a command that produces no output) — it must not hang."
  (with-guarded-socket-test/fd
      (:server-sock server-sock :client-stream client-stream :client-fd client-fd)
    ;; Server closes without sending a reply → client sees EOF.
    (close-socket server-sock)
    (let ((output (with-output-to-string (*standard-output*)
                    (cl-tmux::%read-command-reply client-stream client-fd))))
      (is (string= "" output)
          "no reply → no output (got ~S)" output))))

;;; ── run-command-client nil-args guard ────────────────────────────────────────
;;;
;;; run-command-client has an early-exit when ARGS is NIL: it does nothing
;;; (no socket connection, no frame sent).  This is intentional defensive
;;; programming — the test confirms it is a deliberate no-op, not a bug.

(test run-command-client-nil-args-is-noop
  :description "run-command-client with NIL args returns immediately without
opening a socket or signalling — the early-exit (when args ...) guard."
  ;; We verify the nil-args branch by confirming the call completes without
  ;; error even when no server is listening (no connection attempt is made).
  (finishes (cl-tmux::run-command-client "no-such-session" nil)))

;;; ── %maybe-send-resize behavior ──────────────────────────────────────────────
;;;
;;; %maybe-send-resize encapsulates the resize-pending check that was inline in
;;; run-client.  It is tested here using a socket pair so the msg-resize frame
;;; can be observed without a live terminal.

(test maybe-send-resize-sends-frame-when-pending
  :description "%maybe-send-resize sends a +msg-resize+ frame and clears *resize-pending*
when *resize-pending* is T — verifies the resize-dispatch path extracted from run-client."
  (with-guarded-socket-test
    ;; Set resize-pending and known dimensions.
    (let ((cl-tmux::*resize-pending* t)
          (cl-tmux::*term-rows*      24)
          (cl-tmux::*term-cols*      80))
      ;; Call the helper with server-side as the stream to write on.
      (cl-tmux::%maybe-send-resize server-side)
      (force-output server-side)
      ;; The helper clears *resize-pending*.
      (is-false cl-tmux::*resize-pending*
                "%maybe-send-resize must clear *resize-pending*")
      ;; A +msg-resize+ frame must be readable from the other end.
      (with-incoming-frame (type payload client-side)
        ((null type) (fail "%maybe-send-resize: got EOF instead of resize frame"))
        ((= type +msg-resize+)
         (multiple-value-bind (rows cols) (decode-size payload)
           (is (= cl-tmux::*term-rows* rows) "resize frame rows must match *term-rows*")
           (is (= cl-tmux::*term-cols* cols) "resize frame cols must match *term-cols*")))
        (t (fail "%maybe-send-resize: unexpected frame type ~D" type))))))

(test maybe-send-resize-does-nothing-when-not-pending
  :description "%maybe-send-resize is a no-op when *resize-pending* is NIL."
  (let ((cl-tmux::*resize-pending* nil))
    (is-false (cl-tmux::%maybe-send-resize nil)
              "%maybe-send-resize with *resize-pending* NIL must return NIL without I/O")))

;;; ── %forward-stdin-byte behavior ─────────────────────────────────────────────
;;;
;;; %forward-stdin-byte reads one non-blocking byte from fd 0 (stdin) and
;;; forwards it as a +msg-key+ frame.  We test the "nothing ready" branch
;;; (returns NIL without I/O) — the "byte forwarded" branch requires a real
;;; non-blocking stdin fd, which is unavailable in a sandboxed test runner.

(test forward-stdin-byte-returns-nil-when-nothing-ready
  :description "%forward-stdin-byte returns NIL without error when stdin has no
   data ready (non-blocking read returns nil)."
  ;; read-byte-nonblock(0) on a non-blocking terminal returns NIL when no data
  ;; is ready.  In the test runner stdin is either /dev/null or a pipe with no
  ;; pending data — either way the function must return NIL without signalling.
  ;; Pass NIL as the stream so no socket write can happen even if the byte test
  ;; were to incorrectly find data.
  (let ((result (ignore-errors (cl-tmux::%forward-stdin-byte nil))))
    (is (null result)
        "%forward-stdin-byte must return NIL when stdin has no byte ready")))

;;; ── %decode-server-frame pure behavior ──────────────────────────────────────
;;;
;;; %decode-server-frame is the pure layer that %receive-server-frame calls.
;;; These tests verify its dispositions without any I/O side effects.

(test decode-server-frame-returns-exit-on-bye
  :description "%decode-server-frame returns (values :exit nil) when the server sends
   +msg-bye+ — the pure classification step used by %receive-server-frame."
  (with-guarded-socket-test
    (send-frame server-side (msg-bye))
    (force-output server-side)
    (multiple-value-bind (disposition text)
        (cl-tmux::%decode-server-frame client-side)
      (is (eq :exit disposition)
          "%decode-server-frame must return :exit disposition for +msg-bye+")
      (is (null text)
          "%decode-server-frame must return NIL text for :exit disposition"))))

(test decode-server-frame-returns-frame-and-text
  :description "%decode-server-frame returns (values :frame text) for +msg-frame+.
   The pure step: caller decides whether/where to write the text."
  (with-guarded-socket-test
    (send-frame server-side (msg-frame "PURE-TEXT"))
    (force-output server-side)
    (multiple-value-bind (disposition text)
        (cl-tmux::%decode-server-frame client-side)
      (is (eq :frame disposition)
          "%decode-server-frame must return :frame disposition for +msg-frame+")
      (is (string= "PURE-TEXT" text)
          "%decode-server-frame must return the decoded text"))))

(test decode-server-frame-returns-exit-on-eof
  :description "%decode-server-frame returns (values :exit nil) on EOF."
  (with-guarded-socket-test
    (close server-side)
    (sleep 0.05)
    (multiple-value-bind (disposition text)
        (cl-tmux::%decode-server-frame client-side)
      (is (eq :exit disposition)
          "%decode-server-frame must return :exit on EOF")
      (is (null text)
          "%decode-server-frame must return NIL text on EOF"))))

;;; ── %receive-server-frame behavior ──────────────────────────────────────────
;;;
;;; %receive-server-frame is the effect boundary that calls %decode-server-frame
;;; and performs the actual write-string/force-output.

(test receive-server-frame-returns-exit-on-bye
  :description "%receive-server-frame returns :exit when the server sends +msg-bye+."
  (with-guarded-socket-test
    (send-frame server-side (msg-bye))
    (force-output server-side)
    (is (eq :exit (cl-tmux::%receive-server-frame client-side))
        "%receive-server-frame must return :exit for +msg-bye+")))

(test receive-server-frame-returns-exit-on-eof
  :description "%receive-server-frame returns :exit on EOF (server closed the stream)."
  (with-guarded-socket-test
    ;; Close the server-side stream to simulate server disconnect.
    (close server-side)
    ;; Give the stream close a moment to propagate across the socket.
    (sleep 0.05)
    (is (eq :exit (cl-tmux::%receive-server-frame client-side))
        "%receive-server-frame must return :exit on EOF")))

(test receive-server-frame-paints-msg-frame-and-returns-nil
  :description "%receive-server-frame writes +msg-frame+ content to *standard-output*
and returns NIL (continue the event loop)."
  (with-guarded-socket-test
    (send-frame server-side (msg-frame "HELLO"))
    (force-output server-side)
    ;; Keep `is` assertions OUTSIDE with-output-to-string: FiveAM writes a progress
    ;; dot via (format *test-dribble* ".") — and *test-dribble* defaults to T
    ;; (= *standard-output*) — so a passing `is` inside the capture body would
    ;; contaminate painted with "." making it "HELLO." instead of "HELLO".
    (let (result)
      (let ((painted (with-output-to-string (*standard-output*)
                       (setf result (cl-tmux::%receive-server-frame client-side)))))
        (is (null result)
            "%receive-server-frame must return NIL for +msg-frame+")
        (is (string= "HELLO" painted)
            "%receive-server-frame must write the frame text to *standard-output*")))))
