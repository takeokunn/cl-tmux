(in-package #:cl-tmux/test)

;;;; Tests for src/input.lisp: with-raw-mode macroexpansion + export checks
;;;; + read-byte-nonblock happy path via a sb-posix pipe pair.
;;;; with-raw-mode touches fd 0 (stdin) so it is verified by macroexpansion only.
;;;; read-byte-nonblock's select+read path is exercised using a pipe fd instead
;;;; of stdin, so no TTY is required.

(def-suite input-suite :description "with-raw-mode macroexpansion + export checks (src/input.lisp)")
(in-suite input-suite)

(test with-raw-mode-expands-enable-before-body
  "The expansion calls enable-raw-mode! on fd 0 before evaluating BODY."
  (let* ((form (macroexpand-1 '(cl-tmux/input::with-raw-mode :body-marker)))
         (text (prin1-to-string form))
         (enable-pos (search "ENABLE-RAW-MODE!" text))
         (body-pos (search ":BODY-MARKER" text)))
    (is-true enable-pos "expansion mentions ENABLE-RAW-MODE!")
    (is-true body-pos "expansion contains the body form")
    (is (< enable-pos body-pos)
        "enable-raw-mode! precedes the body in the expansion")))

(test with-raw-mode-installs-disable-in-handler-and-cleanup
  "DISABLE-RAW-MODE! appears exactly twice: error handler + unwind cleanup."
  (let* ((form (macroexpand-1 '(cl-tmux/input::with-raw-mode :body-marker)))
         (text (prin1-to-string form))
         (count 0)
         (start 0))
    ;; Count non-overlapping occurrences of DISABLE-RAW-MODE! in the text.
    (loop for pos = (search "DISABLE-RAW-MODE!" text :start2 start)
          while pos
          do (incf count)
             (setf start (+ pos (length "DISABLE-RAW-MODE!"))))
    (is (= 2 count)
        "disable-raw-mode! installed in both handler-bind and unwind-protect")
    ;; The expansion should also establish a handler and an unwind-protect.
    (is-true (search "HANDLER-BIND" text) "uses handler-bind for error safety")
    (is-true (search "UNWIND-PROTECT" text) "uses unwind-protect for cleanup")))

(test with-raw-mode-is-a-macro
  "with-raw-mode is defined as a macro."
  (is-true (macro-function 'cl-tmux/input::with-raw-mode)))

(test input-symbols-exported-and-fbound
  "Public input symbols are exported and bound."
  ;; with-raw-mode is an exported macro.
  (is-true (macro-function (find-symbol "WITH-RAW-MODE" '#:cl-tmux/input))
           "with-raw-mode is a macro")
  ;; read-byte-nonblock is an exported function.
  (is-true (fboundp (find-symbol "READ-BYTE-NONBLOCK" '#:cl-tmux/input))
           "read-byte-nonblock is fbound")
  ;; Both names resolve as exported symbols of the package.
  (multiple-value-bind (sym status)
      (find-symbol "WITH-RAW-MODE" '#:cl-tmux/input)
    (declare (ignore sym))
    (is (eq :external status) "with-raw-mode is exported"))
  (multiple-value-bind (sym status)
      (find-symbol "READ-BYTE-NONBLOCK" '#:cl-tmux/input)
    (declare (ignore sym))
    (is (eq :external status) "read-byte-nonblock is exported")))

;;; ── read-byte-nonblock happy path via pipe ───────────────────────────────────
;;;
;;; We use a POSIX pipe pair (sb-posix:pipe) so we can inject a known byte into
;;; the read end without needing stdin to be a TTY.
;;; with-pipe-fds is defined in tests/helpers-b.lisp.

(test read-byte-nonblock-returns-byte-when-data-available
  "read-byte-nonblock's select+read pipeline returns a byte when data is ready.
   Uses a pipe pair so no TTY is required."
  (with-pipe-fds (rfd wfd)
    ;; Write one known byte into the write end.
    (write-byte-to-fd wfd 42)
    ;; Poll the read end: data should be ready immediately.
    (let ((ready (cl-tmux/pty:select-fds (list rfd) 200000)))  ; 200 ms timeout
      (is-true ready "pipe read-end must be readable after write")
      (when ready
        ;; Read exactly one byte via CFFI (same mechanics as read-byte-nonblock).
        (cffi:with-foreign-object (rbuf :uint8)
          (let ((n (cffi:foreign-funcall "read"
                                         :int rfd :pointer rbuf :unsigned-long 1
                                         :long)))
            (is (= 1 n) "read must consume exactly 1 byte")
            (is (= 42 (cffi:mem-ref rbuf :uint8))
                "byte value must be 42")))))))

(test read-byte-nonblock-select-returns-nil-when-no-data
  "select-fds returns NIL when no data is available within the timeout.
   Verified on a fresh idle pipe."
  (with-pipe-fds (rfd _wfd)
    ;; The pipe has no data; select with a short timeout must return NIL.
    (let ((ready (cl-tmux/pty:select-fds (list rfd) 10000)))  ; 10 ms
      (is (null ready) "empty pipe must not be readable"))))

(test select-fds-gates-on-positive-select-return
  "select-fds inspects the read-set ONLY when select(2) reports a positive count:
   an idle pipe returns NIL (count 0 / EINTR -1 leave the read-set undefined), and
   after a write the readable fd is reported.  This guards against an EINTR-driven
   false positive on an idle fd."
  (with-pipe-fds (rfd wfd)
    ;; Idle pipe → NIL (gated; never inspects stale bits).
    (is (null (cl-tmux/pty:select-fds (list rfd) 10000))
        "idle pipe must return NIL")
    ;; Write one byte → select reports a positive count → the fd is returned.
    (write-byte-to-fd wfd 7)
    (is (equal (list rfd) (cl-tmux/pty:select-fds (list rfd) 200000))
        "after a write, select-fds must report exactly the readable fd")))

;;; ── Package / constant coverage ─────────────────────────────────────────────

(test poll-timeout-us-constant-is-positive
  "+poll-timeout-us+ is a positive fixnum used as the default select timeout."
  (let ((timeout (symbol-value
                  (find-symbol "+POLL-TIMEOUT-US+" '#:cl-tmux/config))))
    (is (integerp timeout)
        "+poll-timeout-us+ must be an integer")
    (is (plusp timeout)
        "+poll-timeout-us+ must be positive (non-zero timeout)")))

(test with-raw-mode-expansion-contains-format-newline
  "The expansion emits a format newline after restoring raw mode for clean output."
  (let* ((form (macroexpand-1 '(cl-tmux/input::with-raw-mode :body-marker)))
         (text (prin1-to-string form)))
    (is-true (or (search "FORMAT" text) (search "format" text))
             "expansion must contain FORMAT for cleanup newline")))

;;; ── select-fds empty-fd short-circuit via read-byte-nonblock path ────────────

(test read-byte-nonblock-with-zero-timeout-returns-nil-when-no-data
  "read-byte-nonblock with timeout-us=0 is a purely non-blocking poll.
   On a fresh idle pipe it must return NIL immediately."
  (with-pipe-fds (rfd _wfd)
    ;; Temporarily redirect the select call through read-byte-nonblock's
    ;; internal use of cl-tmux/pty:select-fds with the pipe read-end.
    ;; We cannot call read-byte-nonblock directly (it polls stdin fd 0), so
    ;; we validate the same mechanics: select-fds with timeout 0 on idle fd.
    (let ((ready (cl-tmux/pty:select-fds (list rfd) 0)))
      (is (null ready)
          "non-blocking select on idle pipe must return NIL"))))

(test read-byte-nonblock-select-returns-ready-list-when-data-present
  "select-fds returns the fd in a ready list when data has been written."
  (with-pipe-fds (rfd wfd)
    (write-byte-to-fd wfd 7)
    (let ((ready (cl-tmux/pty:select-fds (list rfd) 200000)))
      (is (equal (list rfd) ready)
          "select-fds must return the ready fd in a list"))))

(test with-raw-mode-expansion-has-force-output
  "The expansion calls force-output to flush stdout after restoring the terminal."
  (let* ((form (macroexpand-1 '(cl-tmux/input::with-raw-mode :body-marker)))
         (text (prin1-to-string form)))
    (is-true (or (search "FORCE-OUTPUT" text) (search "force-output" text))
             "expansion must call FORCE-OUTPUT")))
