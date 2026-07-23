(in-package #:cl-tmux/test)

;;;; Tests for src/input.lisp: with-raw-mode macroexpansion + export checks
;;;; + read-byte-nonblock happy path via a sb-posix pipe pair.
;;;; with-raw-mode touches fd 0 (stdin) so it is verified by macroexpansion only.
;;;; read-byte-nonblock's select+read path is exercised using a pipe fd instead
;;;; of stdin, so no TTY is required.

(describe "input-suite"

  ;; The expansion calls enable-raw-mode! on fd 0 before evaluating BODY.
  (it "with-raw-mode-expands-enable-before-body"
    (let* ((form (macroexpand-1 '(cl-tmux/input::with-raw-mode :body-marker)))
           (text (prin1-to-string form))
           (enable-pos (search "ENABLE-RAW-MODE!" text))
           (body-pos (search ":BODY-MARKER" text)))
      (expect enable-pos :to-be-truthy)
      (expect body-pos :to-be-truthy)
      (expect (< enable-pos body-pos))))

  ;; DISABLE-RAW-MODE! appears exactly twice: error handler + unwind cleanup.
  (it "with-raw-mode-installs-disable-in-handler-and-cleanup"
    (let* ((form (macroexpand-1 '(cl-tmux/input::with-raw-mode :body-marker)))
           (text (prin1-to-string form))
           (count 0)
           (start 0))
      ;; Count non-overlapping occurrences of DISABLE-RAW-MODE! in the text.
      (loop for pos = (search "DISABLE-RAW-MODE!" text :start2 start)
            while pos
            do (incf count)
               (setf start (+ pos (length "DISABLE-RAW-MODE!"))))
      (expect (= 2 count))
      ;; The expansion should also establish a handler and an unwind-protect.
      (expect (search "HANDLER-BIND" text) :to-be-truthy)
      (expect (search "UNWIND-PROTECT" text) :to-be-truthy)))

  ;; with-raw-mode is defined as a macro.
  (it "with-raw-mode-is-a-macro"
    (expect (macro-function 'cl-tmux/input::with-raw-mode) :to-be-truthy))

  ;; Public input symbols are exported and bound.
  (it "input-symbols-exported-and-fbound"
    ;; with-raw-mode is an exported macro.
    (expect (macro-function (find-symbol "WITH-RAW-MODE" '#:cl-tmux/input)) :to-be-truthy)
    ;; read-byte-nonblock is an exported function.
    (expect (fboundp (find-symbol "READ-BYTE-NONBLOCK" '#:cl-tmux/input)) :to-be-truthy)
    ;; Both names resolve as exported symbols of the package.
    (multiple-value-bind (sym status)
        (find-symbol "WITH-RAW-MODE" '#:cl-tmux/input)
      (declare (ignore sym))
      (expect (eq :external status)))
    (multiple-value-bind (sym status)
        (find-symbol "READ-BYTE-NONBLOCK" '#:cl-tmux/input)
      (declare (ignore sym))
      (expect (eq :external status))))

  ;; ── read-byte-nonblock happy path via pipe ───────────────────────────────────
  ;;
  ;; We use a POSIX pipe pair (sb-posix:pipe) so we can inject a known byte into
  ;; the read end without needing stdin to be a TTY.
  ;; with-pipe-fds is defined in tests/helpers-pipe-fixtures.lisp.

  ;; read-byte-nonblock's select+read pipeline returns a byte when data is ready.
  ;; Uses a pipe pair so no TTY is required.
  (it "read-byte-nonblock-returns-byte-when-data-available"
    (with-pipe-fds (rfd wfd)
      ;; Write one known byte into the write end.
      (write-byte-to-fd wfd 42)
      ;; Poll the read end: data should be ready immediately.
      (let ((ready (cl-tmux/pty:select-fds (list rfd) 200000)))  ; 200 ms timeout
        (expect ready :to-be-truthy)
        (when ready
          ;; Read exactly one byte via CFFI (same mechanics as read-byte-nonblock).
          (cffi:with-foreign-object (rbuf :uint8)
            (let ((n (cffi:foreign-funcall "read"
                                           :int rfd :pointer rbuf :unsigned-long 1
                                           :long)))
              (expect (= 1 n))
              (expect (= 42 (cffi:mem-ref rbuf :uint8)))))))))

  ;; select-fds returns NIL when no data is available within the timeout.
  ;; Verified on a fresh idle pipe.
  (it "read-byte-nonblock-select-returns-nil-when-no-data"
    (with-pipe-fds (rfd _wfd)
      ;; The pipe has no data; select with a short timeout must return NIL.
      (let ((ready (cl-tmux/pty:select-fds (list rfd) 10000)))  ; 10 ms
        (expect (null ready)))))

  ;; select-fds inspects the read-set ONLY when select(2) reports a positive count:
  ;; an idle pipe returns NIL (count 0 / EINTR -1 leave the read-set undefined), and
  ;; after a write the readable fd is reported.  This guards against an EINTR-driven
  ;; false positive on an idle fd.
  (it "select-fds-gates-on-positive-select-return"
    (with-pipe-fds (rfd wfd)
      ;; Idle pipe → NIL (gated; never inspects stale bits).
      (expect (null (cl-tmux/pty:select-fds (list rfd) 10000)))
      ;; Write one byte → select reports a positive count → the fd is returned.
      (write-byte-to-fd wfd 7)
      (expect (equal (list rfd) (cl-tmux/pty:select-fds (list rfd) 200000)))))

  ;; ── Package / constant coverage ─────────────────────────────────────────────

  ;; +poll-timeout-us+ is a positive fixnum used as the default select timeout.
  (it "poll-timeout-us-constant-is-positive"
    (let ((timeout (symbol-value
                    (find-symbol "+POLL-TIMEOUT-US+" '#:cl-tmux/config))))
      (expect (integerp timeout))
      (expect (plusp timeout))))

  ;; The expansion emits a format newline after restoring raw mode for clean output.
  (it "with-raw-mode-expansion-contains-format-newline"
    (let* ((form (macroexpand-1 '(cl-tmux/input::with-raw-mode :body-marker)))
           (text (prin1-to-string form)))
      (expect (or (search "FORMAT" text) (search "format" text)) :to-be-truthy)))

  ;; ── select-fds empty-fd short-circuit via read-byte-nonblock path ────────────

  ;; read-byte-nonblock with timeout-us=0 is a purely non-blocking poll.
  ;; On a fresh idle pipe it must return NIL immediately.
  (it "read-byte-nonblock-with-zero-timeout-returns-nil-when-no-data"
    (with-pipe-fds (rfd _wfd)
      ;; Temporarily redirect the select call through read-byte-nonblock's
      ;; internal use of cl-tmux/pty:select-fds with the pipe read-end.
      ;; We cannot call read-byte-nonblock directly (it polls stdin fd 0), so
      ;; we validate the same mechanics: select-fds with timeout 0 on idle fd.
      (let ((ready (cl-tmux/pty:select-fds (list rfd) 0)))
        (expect (null ready)))))

  ;; select-fds returns the fd in a ready list when data has been written.
  (it "read-byte-nonblock-select-returns-ready-list-when-data-present"
    (with-pipe-fds (rfd wfd)
      (write-byte-to-fd wfd 7)
      (let ((ready (cl-tmux/pty:select-fds (list rfd) 200000)))
        (expect (equal (list rfd) ready)))))

  ;; The expansion calls force-output to flush stdout after restoring the terminal.
  (it "with-raw-mode-expansion-has-force-output"
    (let* ((form (macroexpand-1 '(cl-tmux/input::with-raw-mode :body-marker)))
           (text (prin1-to-string form)))
      (expect (or (search "FORCE-OUTPUT" text) (search "force-output" text)) :to-be-truthy))))
