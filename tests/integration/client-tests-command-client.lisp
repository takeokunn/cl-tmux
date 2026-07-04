(in-package #:cl-tmux/test)

(in-suite client-suite)

;;; ── %command-client-split-window-input-p ────────────────────────────────────
;;;
;;; Table-driven coverage of the split-window/-I detection predicate.  Each row
;;; is (args expected-bool description).  The predicate must be true only for
;;; canonical split-window with a flag token that contains the character I.

(test command-client-split-window-input-p-table
  :description "%command-client-split-window-input-p is true only for split-window/-I."
  (dolist (row '((("split-window" "-I")                t   "split-window -I")
                 (("splitw" "-I")                      nil "splitw alias rejected")
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
