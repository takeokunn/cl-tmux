(in-package #:cl-tmux/test)

;;;; Client receive/decode integration tests (src/client.lisp).
;;;;
;;;; client-tests.lisp declares client-suite first; this file keeps the
;;;; server-frame receive/decode behavior separate from outbound client tests.

(describe "client-suite"

  ;; ── %decode-server-frame pure behavior ──────────────────────────────────────
  ;;
  ;; %decode-server-frame is the pure layer that %receive-server-frame calls.
  ;; These tests verify its dispositions without any I/O side effects.

  ;; %decode-server-frame returns (values :exit nil) when the server sends
  ;; +msg-bye+ — the pure classification step used by %receive-server-frame.
  (it "decode-server-frame-returns-exit-on-bye"
    (with-guarded-socket-test
      (send-frame server-side (msg-bye))
      (force-output server-side)
      (multiple-value-bind (disposition text)
          (cl-tmux::%decode-server-frame client-side)
        (expect (eq :exit disposition))
        (expect (null text)))))

  ;; %decode-server-frame returns (values :frame text) for +msg-frame+.
  ;; The pure step: caller decides whether/where to write the text.
  (it "decode-server-frame-returns-frame-and-text"
    (with-guarded-socket-test
      (send-frame server-side (msg-frame "PURE-TEXT"))
      (force-output server-side)
      (multiple-value-bind (disposition text)
          (cl-tmux::%decode-server-frame client-side)
        (expect (eq :frame disposition))
        (expect (string= "PURE-TEXT" text)))))

  ;; %decode-server-frame returns (values :exit nil) on EOF.
  (it "decode-server-frame-returns-exit-on-eof"
    (with-guarded-socket-test
      (close server-side)
      (sleep 0.05)
      (multiple-value-bind (disposition text)
          (cl-tmux::%decode-server-frame client-side)
        (expect (eq :exit disposition))
        (expect (null text)))))

  ;; ── %receive-server-frame behavior ──────────────────────────────────────────
  ;;
  ;; %receive-server-frame is the effect boundary that calls %decode-server-frame
  ;; and performs the actual write-string/force-output.

  ;; %receive-server-frame returns :exit when the server sends +msg-bye+.
  (it "receive-server-frame-returns-exit-on-bye"
    (with-guarded-socket-test
      (send-frame server-side (msg-bye))
      (force-output server-side)
      (expect (eq :exit (cl-tmux::%receive-server-frame client-side)))))

  ;; %receive-server-frame returns :exit on EOF (server closed the stream).
  (it "receive-server-frame-returns-exit-on-eof"
    (with-guarded-socket-test
      ;; Close the server-side stream to simulate server disconnect.
      (close server-side)
      ;; Give the stream close a moment to propagate across the socket.
      (sleep 0.05)
      (expect (eq :exit (cl-tmux::%receive-server-frame client-side)))))

  ;; %receive-server-frame writes +msg-frame+ content to *standard-output*
  ;; and returns NIL (continue the event loop).
  (it "receive-server-frame-paints-msg-frame-and-returns-nil"
    (with-guarded-socket-test
      (send-frame server-side (msg-frame "HELLO"))
      (force-output server-side)
      ;; Keep `expect` assertions OUTSIDE with-output-to-string: FiveAM writes a progress
      ;; dot via (format *test-dribble* ".") — and *test-dribble* defaults to T
      ;; (= *standard-output*) — so a passing `is` inside the capture body would
      ;; contaminate painted with "." making it "HELLO." instead of "HELLO".
      (let (result)
        (let ((painted (with-output-to-string (*standard-output*)
                         (setf result (cl-tmux::%receive-server-frame client-side)))))
          (expect (null result))
          (expect (string= "HELLO" painted))))))

  ;; ── %utf8-char-byte-count table-driven tests ────────────────────────────────
  ;;
  ;; %utf8-char-byte-count is a private helper with four boundary thresholds.
  ;; These table-driven tests make every boundary condition explicit — analogous
  ;; to the %command-client-split-window-input-p table above — so the split
  ;; points are auditable without requiring Unicode knowledge.

  ;; %utf8-char-byte-count returns the correct UTF-8 byte width for
  ;; boundary values in each of the four encoding ranges.  Tests at and just below
  ;; each threshold (0x80, 0x800, 0x10000) make the boundaries explicit.
  (it "utf8-char-byte-count-table"
    ;; Each row: (char-code expected-byte-count description)
    (dolist (row '((#x0000  1 "U+0000 is 1-byte (lowest codepoint)")
                   (#x0041  1 "U+0041 'A' is 1-byte ASCII")
                   (#x007F  1 "U+007F is 1-byte (just below 2-byte threshold 0x80)")
                   (#x0080  2 "U+0080 is 2-byte (exactly at 2-byte threshold)")
                   (#x00FF  2 "U+00FF is 2-byte (Latin-1 supplement)")
                   (#x07FF  2 "U+07FF is 2-byte (just below 3-byte threshold 0x800)")
                   (#x0800  3 "U+0800 is 3-byte (exactly at 3-byte threshold)")
                   (#x3042  3 "U+3042 hiragana is 3-byte")
                   (#xFFFF  3 "U+FFFF is 3-byte (just below 4-byte threshold 0x10000)")
                   (#x10000 4 "U+10000 is 4-byte (exactly at 4-byte threshold)")
                   (#x1F600 4 "U+1F600 emoji is 4-byte")))
      (destructuring-bind (code expected description) row
        (declare (ignore description))
        ;; Guard: skip codepoints beyond the Lisp image's char-code-limit.
        (when (< code char-code-limit)
          (let ((got (cl-tmux::%utf8-char-byte-count (code-char code))))
            (expect (= expected got)))))))

  ;; ── %receive-if-ready behavior ──────────────────────────────────────────────
  ;;
  ;; %receive-if-ready is the event-loop glue that calls %receive-server-frame
  ;; only when the server fd appears in the ready set.  These tests cover the
  ;; "not ready" branch (returns NIL without I/O) and the "ready → delegates"
  ;; branch (returns :exit when the server sends +msg-bye+).

  ;; %receive-if-ready returns NIL without I/O when the server fd is
  ;; NOT in the READY list — the non-blocking guard must prevent reads on idle fds.
  (it "receive-if-ready-returns-nil-when-fd-not-in-ready-set"
    ;; Any fd value not in the ready list; NIL stream ensures no I/O if guard fails.
    (expect (null (cl-tmux::%receive-if-ready nil 99 '(0 1 2)))))

  ;; %receive-if-ready returns :exit when the server socket fd is in
  ;; the READY list and %receive-server-frame returns :exit (+msg-bye+ frame).
  (it "receive-if-ready-returns-exit-on-bye-when-fd-ready"
    (with-guarded-socket-test/fd (:server-stream server-stream :client-stream client-stream
                                   :client-fd client-fd)
      (send-frame server-stream (msg-bye))
      (force-output server-stream)
      ;; Wait for the frame to be readable.
      (cl-tmux/pty:select-fds (list client-fd) 1000000)
      ;; Ready set contains the client fd: %receive-if-ready must dispatch.
      (let ((result (cl-tmux::%receive-if-ready client-stream client-fd
                                                (list client-fd))))
        (expect (eq :exit result))))))
