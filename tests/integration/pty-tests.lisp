(in-package #:cl-tmux/test)

;;;; PTY integration tests.  These spawn a real shell over a pseudo-terminal
;;;; and exercise the spawn/write/read/select pipeline end to end.
;;;;
;;;; PTY allocation needs /dev/ptmx, which sandboxed Nix builds do not provide.
;;;; When allocation fails we (skip) rather than fail, so the same suite runs
;;;; both in `nix develop` (real PTY) and `nix flake check` (sandboxed).
;;;;
;;;; pty-available-p is imported from cl-tmux/pty; no local shadow needed.

(defun drain-pty (fd &key (deadline-seconds 3.0) (stop-marker nil) (quiet-windows 1))
  "Read from FD until STOP-MARKER appears in the decoded output, DEADLINE-SECONDS
   elapses, or QUIET-WINDOWS consecutive empty 200ms select polls occur (meaning
   the shell has truly gone idle).  Returns the accumulated string.

   quiet-windows > 1 is useful before testing idleness: it certifies actual shell
   stability rather than just elapsed time, eliminating a race where the shell sends
   a late output burst right after drain returns."
  (let ((acc  (make-array 0 :element-type '(unsigned-byte 8) :adjustable t
                            :fill-pointer 0))
        (end  (+ (get-internal-real-time)
                 (* deadline-seconds internal-time-units-per-second)))
        (quiet-count 0))
    (loop
      (when (> (get-internal-real-time) end) (return))
      (if (select-fds (list fd) 200000)          ; 200 ms poll
          (let ((chunk (pty-read-blocking fd 4096)))
            (setf quiet-count 0)
            (when chunk
              (loop for b across chunk
                    do (vector-push-extend b acc))))
          (progn
            (incf quiet-count)
            (when (>= quiet-count quiet-windows) (return))))
      (let ((text (map 'string #'code-char acc)))
        (when (and stop-marker (search stop-marker text))
          (return-from drain-pty text))))
    (map 'string #'code-char acc)))

(describe "pty-suite"

  (it "shell-echoes-command-output"
    (unless (pty-available-p)
      (skip "no PTY available (sandboxed environment)"))
    (with-pty-shell (fd pid)
      (let ((marker "CLTMUX_MARKER_42"))
        ;; Give the shell a moment to start, then send a command.
        (sleep 0.2)
        (pty-write fd (format nil "echo ~A~%" marker))
        (let ((out (drain-pty fd :stop-marker marker)))
          (expect (search marker out))))))

  (it "pty-write-accepts-octet-vector"
    (unless (pty-available-p)
      (skip "no PTY available (sandboxed environment)"))
    (with-pty-shell (fd pid)
      (let ((bytes (map '(simple-array (unsigned-byte 8) (*))
                        #'char-code
                        (format nil "printf DONE_OCTETS~%"))))
        (sleep 0.2)
        (pty-write fd bytes)
        (let ((out (drain-pty fd :stop-marker "DONE_OCTETS")))
          (expect (search "DONE_OCTETS" out))))))

  (it "select-times-out-when-idle"
    (unless (pty-available-p)
      (skip "no PTY available (sandboxed environment)"))
    (with-pty-shell (fd pid)
      ;; Drain until two consecutive quiet 200ms windows: certifies the shell has
      ;; truly settled before we test that no further output arrives.
      (drain-pty fd :deadline-seconds 2.0 :quiet-windows 2)
      (let ((ready (select-fds (list fd) 100000)))  ; 100 ms, no input sent
        (expect (null ready)))))

  ;; Exercises the real resize path: spawned PTY per pane + ioctl(TIOCSWINSZ) +
  ;; screen-resize, across a split and a subsequent terminal resize.
  (it "split-then-relayout-keeps-panes-fitting"
    (unless (pty-available-p)
      (skip "no PTY available (sandboxed environment)"))
    (with-session (session 24 80)
      (let ((win (session-active-window session)))
        ;; Split vertically → two panes side by side.
        (window-split session win :h)
        (expect (= 2 (length (window-panes win))))
        ;; Now resize the terminal larger and relayout.
        (window-relayout win 40 120)
        (let ((ps (window-panes win)))
          ;; All panes fit within the new geometry, no overlap.
          (dolist (p ps)
            (expect (<= (+ (pane-x p) (pane-width p))  120))
            (expect (<= (+ (pane-y p) (pane-height p)) 40))
            (expect (plusp (pane-width  p)))
            (expect (plusp (pane-height p))))
          (destructuring-bind (a b) ps
            ;; divider column separates the two panes after relayout
            (expect (< (+ (pane-x a) (pane-width a)) (pane-x b))))))))

  ;; pty-child-exit-status reports KIND = :signaled when the child dies from a
  ;; signal (vs :exited for a normal exit code).  SIGKILL (9) cannot be trapped,
  ;; so the spawned shell is guaranteed to terminate by signal; reaping it via
  ;; pty-child-exit-status must yield (values 9 :signaled).
  (it "pty-child-exit-status-reports-signaled-kind"
    (unless (pty-available-p)
      (skip "no PTY available (sandboxed environment)"))
    (with-pty-shell (fd pid)
      (sleep 0.2)
      (sb-posix:kill pid 9)              ; SIGKILL — untrappable
      (multiple-value-bind (code kind) (cl-tmux/pty:pty-child-exit-status fd)
        (expect (eq kind :signaled))
        (expect (= code 9)))))

  ;; kill-pane on the last pane kills the window; session has 0 windows.
  (it "cmd-kill-pane-closes-fd"
    (unless (pty-available-p)
      (skip "no PTY available (sandboxed environment)"))
    ;; with-session handles cleanup of all pane PTYs via unwind-protect.
    (with-session (session 24 80)
      (let ((win (session-active-window session)))
        (declare (ignore win))
        ;; kill-pane on the sole pane must not signal an error.
        (finishes (kill-pane session))
        ;; Killing the only pane removes the window; no windows remain.
        (expect (null (session-windows session))))))

  ;; After splitting vertically and killing one pane, exactly one pane remains.
  (it "split-and-kill-returns-to-single"
    (unless (pty-available-p)
      (skip "no PTY available (sandboxed environment)"))
    (with-session (session 24 80)
      (let ((win (session-active-window session)))
        ;; Split → 2 panes.
        (window-split session win :h)
        (expect (= 2 (length (window-panes win))))
        ;; Kill the active (second) pane → 1 pane should remain.
        (kill-pane session)
        (expect (= 1 (length (window-panes (session-active-window session))))))))

  ;;;; ── Un-gated sandbox-safe unit tests ──────────────────────────────────────
  ;;;; These run real assertions without /dev/ptmx, a tty, or a socket.

  ;; pty-close must never kill(-1)/kill(0): a non-positive pid and a negative
  ;; master fd are both no-ops, so the call simply finishes without signalling.
  (it "pty-close-ignores-non-positive-pid"
    (finishes (cl-tmux/pty:pty-close -1 -1))
    (finishes (cl-tmux/pty:pty-close -1 0)))

  ;; terminal-size returns rows/cols clamped to the sane 1..+max-sane-*+ range.
  ;; In the sandbox ioctl fails and it falls back to 24x80 — still in range.
  (it "terminal-size-returns-sane-clamped-geometry"
    (multiple-value-bind (rows cols) (cl-tmux/pty:terminal-size)
      (expect (<= 1 rows cl-tmux/pty::+max-sane-rows+))
      (expect (<= 1 cols cl-tmux/pty::+max-sane-cols+))))

  ;; The fallback 24x80 values used when ioctl fails are themselves sane.
  (it "terminal-size-fallback-values-are-sane"
    (expect (<= 1 24 cl-tmux/pty::+max-sane-rows+))
    (expect (<= 1 80 cl-tmux/pty::+max-sane-cols+)))

  ;; +max-sane-rows+ and +max-sane-cols+ are positive and at least 80/24.
  (it "max-sane-bounds-are-reasonable"
    (expect (>= cl-tmux/pty::+max-sane-rows+ 24))
    (expect (>= cl-tmux/pty::+max-sane-cols+ 80)))

  ;; select-fds short-circuits on an empty fd list regardless of timeout,
  ;; returning nil without ever calling select(2).
  (it "select-fds-empty-list-returns-nil"
    (expect (null (cl-tmux/pty:select-fds '() 0)))
    (expect (null (cl-tmux/pty:select-fds '() 100000)))
    (expect (null (cl-tmux/pty:select-fds '() -1))))

  ;; pty-write's etypecase accepts only strings and octet vectors; any other
  ;; type signals an error before any fd write is attempted.
  (it "pty-write-rejects-bad-type"
    (signals error (cl-tmux/pty:pty-write -1 42))
    (signals error (cl-tmux/pty:pty-write -1 '(1 2 3))))

  ;; An empty octet vector is guarded by (plusp len): no write(2) is issued,
  ;; so writing to a bogus fd -1 finishes without error.
  (it "pty-write-empty-is-noop"
    (let ((empty (make-array 0 :element-type '(unsigned-byte 8))))
      (finishes (cl-tmux/pty:pty-write -1 empty))))

  ;; A negative fd is the "no PTY / dead pane" sentinel (pane-fd -1).  pty-write's
  ;; (>= fd 0) guard must silently skip the write for a NON-EMPTY octet payload,
  ;; rather than let cl-tty-kit's fd-write-octets assert a non-negative fd and
  ;; signal.  This guard is load-bearing: dead panes hold pane-fd = -1.  A real
  ;; octet vector (not the literal #(1 2 3), which is a simple-vector and would
  ;; hit the type guard) exercises the fd guard specifically.
  (it "pty-write-negative-fd-is-noop"
    (let ((bytes (make-array 3 :element-type '(unsigned-byte 8)
                               :initial-contents '(1 2 3))))
      (finishes (cl-tmux/pty:pty-write -1 bytes))))

  ;;; ── Octet round-trip through the cl-tty-kit-backed I/O ──────────────────────

  ;; pty-write (octet vector) -> pty-read-blocking round-trips the exact bytes
  ;; through a real pipe, now that both delegate to cl-tty-kit's byte-transparent
  ;; fd-write-octets / fd-read-octets.  Includes 0, 127, 128, 255 to prove no
  ;; character re-encoding corrupts high bytes.
  (it "pty-write-pty-read-octet-round-trip"
    (with-pipe-fds (rfd wfd)
      (let ((original (make-array 5 :element-type '(unsigned-byte 8)
                                    :initial-contents '(0 1 127 128 255))))
        (cl-tmux/pty:pty-write wfd original)
        (let ((recovered (cl-tmux/pty:pty-read-blocking rfd 4096)))
          (expect (equalp original recovered))
          (expect (typep recovered '(simple-array (unsigned-byte 8) (*))))))))

  ;; pty-read-blocking returns NIL when read(2) returns 0 (EOF) or negative.
  (it "pty-read-blocking-returns-nil-on-closed-fd"
    ;; A pipe whose write end is closed immediately delivers EOF on the read end.
    ;; with-pipe-fds is defined in tests/helpers-pipe-fixtures.lisp.
    (with-pipe-fds (rfd wfd)
      ;; Close the write end so the read end gets EOF.
      (sb-posix:close wfd)
      ;; wfd is now closed; with-pipe-fds will call ignore-errors on the second close.
      (let ((result (cl-tmux/pty:pty-read-blocking rfd 1)))
        (expect (null result)))))

  ;; select-fds always returns a list (possibly nil), never another type.
  (it "select-fds-returns-list-type"
    (let ((result (cl-tmux/pty:select-fds '() 0)))
      (expect (listp result))))

  ;; select-fds returns the readable fd in a list when data is available on a pipe.
  (it "select-fds-with-pipe-data-returns-ready-fd"
    (with-pipe-fds (rfd wfd)
      (write-byte-to-fd wfd 99)
      (let ((ready (cl-tmux/pty:select-fds (list rfd) 200000)))
        (expect (equal (list rfd) ready)))))

  ;; select-fds with timeout-us=0 returns NIL immediately on an idle fd.
  (it "select-fds-zero-timeout-is-non-blocking"
    (with-pipe-fds (rfd _wfd)
      (let ((ready (cl-tmux/pty:select-fds (list rfd) 0)))
        (expect (null ready)))))

  ;; pty-read-blocking returns an (unsigned-byte 8) vector containing the written bytes.
  (it "pty-read-blocking-returns-octet-vector-when-data-available"
    (with-pipe-fds (rfd wfd)
      (cffi:with-foreign-object (buf :uint8 3)
        (setf (cffi:mem-aref buf :uint8 0) 1
              (cffi:mem-aref buf :uint8 1) 2
              (cffi:mem-aref buf :uint8 2) 3)
        (cffi:foreign-funcall "write" :int wfd :pointer buf :unsigned-long 3 :long))
      (let ((result (cl-tmux/pty:pty-read-blocking rfd 4096)))
        (expect result :to-be-truthy)
        (expect (= 3 (length result)))
        (expect (= 1 (aref result 0)))
        (expect (= 2 (aref result 1)))
        (expect (= 3 (aref result 2))))))

  ;; pty-close with a valid positive pid but negative fd sends SIGHUP but skips close.
  (it "pty-close-positive-pid-negative-fd-is-noop"
    ;; We can't test the kill call directly without a real process, but pty-close
    ;; with a bogus high pid should not error (kill may fail with ESRCH, ignored).
    (finishes (cl-tmux/pty:pty-close -1 99999999)
              "pty-close with negative fd and unknown pid must not signal"))

  ;;; ── terminal-size rows/cols order ───────────────────────────────────────────

  ;; terminal-size delegates to cl-tty-kit:terminal-size (which returns COLUMNS
  ;; first) and SWAPS to cl-tmux's (values ROWS COLS) contract.  This guards the
  ;; transpose: when a real TTY reports a non-square size, ROWS must be the row
  ;; count and COLS the column count — not swapped.  On the standard-ish 24x80
  ;; terminal, and on the sandbox fallback (also 24x80), rows<=cols; more
  ;; importantly rows must equal cl-tty-kit's ROWS value, cols its COLUMNS value.
  (it "terminal-size-returns-rows-then-cols-not-transposed"
    (multiple-value-bind (rows cols) (cl-tmux/pty:terminal-size)
      (multiple-value-bind (kit-cols kit-rows) (cl-tty-kit:terminal-size 1)
        (if (and (integerp kit-rows) (integerp kit-cols)
                 (<= 1 kit-rows cl-tmux/pty::+max-sane-rows+)
                 (<= 1 kit-cols cl-tmux/pty::+max-sane-cols+))
            ;; Real TTY: cl-tmux's ROWS/COLS must map to cl-tty-kit's ROWS/COLUMNS.
            (progn
              (expect (= rows kit-rows))
              (expect (= cols kit-cols)))
            ;; No TTY / out-of-range: cl-tmux falls back to 24x80 (rows x cols).
            (progn
              (expect (= rows cl-tmux/pty:+default-term-rows+))
              (expect (= cols cl-tmux/pty:+default-term-cols+)))))))

  ;;; ── New coverage: spawn helpers and microsecond constants ──────────────────

  ;; %string-non-empty-p accepts only strings with positive length.
  (it "string-non-empty-p-rejects-empty-and-non-strings"
    (expect (cl-tmux/pty::%string-non-empty-p "x") :to-be-truthy)
    (expect (cl-tmux/pty::%string-non-empty-p "") :to-be-falsy)
    (expect (cl-tmux/pty::%string-non-empty-p nil) :to-be-falsy)
    (expect (cl-tmux/pty::%string-non-empty-p 42) :to-be-falsy))

  ;; %spawn-environment-assignments emits TERM first, then valid extra env pairs.
  (it "spawn-environment-assignments-preserves-override-order"
    (expect (equal '("TERM=xterm-256color" "FOO=bar" "TERM=screen")
                   (cl-tmux/pty::%spawn-environment-assignments
                    "xterm-256color"
                    '(("FOO" . "bar") ("TERM" . "screen") ("BAD" . 1) (42 . "no"))))))

  ;; +microseconds-per-second+ is 1000000.
  (it "microseconds-per-second-is-one-million"
    (expect (= 1000000 cl-tmux/pty::+microseconds-per-second+)))

  ;; %setup-timeval decomposes microseconds into (seconds, remainder) correctly.
  ;; 1500000 us = 1 second + 500000 us.
  (it "setup-timeval-decomposes-correctly"
    (cffi:with-foreign-object (tv :long 2)
      (cl-tmux/pty::%setup-timeval tv 1500000)
      (expect (= 1 (cffi:mem-aref tv :long 0)))
      (expect (= 500000 (cffi:mem-aref tv :long 1)))))

  ;; %setup-timeval with 0 produces (0, 0) — purely non-blocking.
  (it "setup-timeval-zero-timeout"
    (cffi:with-foreign-object (tv :long 2)
      (cl-tmux/pty::%setup-timeval tv 0)
      (expect (= 0 (cffi:mem-aref tv :long 0)))
      (expect (= 0 (cffi:mem-aref tv :long 1)))))

  ;; %setup-timeval with 50000 us (50 ms) produces (0, 50000).
  (it "setup-timeval-sub-second-timeout"
    (cffi:with-foreign-object (tv :long 2)
      (cl-tmux/pty::%setup-timeval tv 50000)
      (expect (= 0 (cffi:mem-aref tv :long 0)))
      (expect (= 50000 (cffi:mem-aref tv :long 1))))))
