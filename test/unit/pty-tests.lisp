(in-package #:cl-tmux/test)

;;;; PTY integration tests.  These spawn a real shell over a pseudo-terminal
;;;; and exercise the spawn/write/read/select pipeline end to end.
;;;;
;;;; PTY allocation needs /dev/ptmx, which sandboxed Nix builds do not provide.
;;;; When allocation fails we (skip) rather than fail, so the same suite runs
;;;; both in `nix develop` (real PTY) and `nix flake check` (sandboxed).
;;;;
;;;; pty-available-p is imported from cl-tmux/pty; no local shadow needed.

(def-suite pty-suite :description "PTY / shell integration")
(in-suite pty-suite)

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

(test shell-echoes-command-output
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (with-pty-shell (fd pid)
    (let ((marker "CLTMUX_MARKER_42"))
      ;; Give the shell a moment to start, then send a command.
      (sleep 0.2)
      (pty-write fd (format nil "echo ~A~%" marker))
      (let ((out (drain-pty fd :stop-marker marker)))
        (is (search marker out)
            "expected marker ~S in shell output, got ~S" marker out)))))

(test pty-write-accepts-octet-vector
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (with-pty-shell (fd pid)
    (let ((bytes (map '(simple-array (unsigned-byte 8) (*))
                      #'char-code
                      (format nil "printf DONE_OCTETS~%"))))
      (sleep 0.2)
      (pty-write fd bytes)
      (let ((out (drain-pty fd :stop-marker "DONE_OCTETS")))
        (is (search "DONE_OCTETS" out))))))

(test select-times-out-when-idle
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (with-pty-shell (fd pid)
    ;; Drain until two consecutive quiet 200ms windows: certifies the shell has
    ;; truly settled before we test that no further output arrives.
    (drain-pty fd :deadline-seconds 2.0 :quiet-windows 2)
    (let ((ready (select-fds (list fd) 100000)))  ; 100 ms, no input sent
      (is (null ready) "idle PTY should not be readable"))))

(test split-then-relayout-keeps-panes-fitting
  "Exercises the real resize path: spawned PTY per pane + ioctl(TIOCSWINSZ) +
   screen-resize, across a split and a subsequent terminal resize."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (with-session (session 24 80)
    (let ((win (session-active-window session)))
      ;; Split vertically → two panes side by side.
      (window-split win :h)
      (is (= 2 (length (window-panes win))))
      ;; Now resize the terminal larger and relayout.
      (window-relayout win 40 120)
      (let ((ps (window-panes win)))
        ;; All panes fit within the new geometry, no overlap.
        (dolist (p ps)
          (is (<= (+ (pane-x p) (pane-width p))  120))
          (is (<= (+ (pane-y p) (pane-height p)) 40))
          (is (plusp (pane-width  p)))
          (is (plusp (pane-height p))))
        (destructuring-bind (a b) ps
          (is (< (+ (pane-x a) (pane-width a)) (pane-x b))
              "divider column separates the two panes after relayout"))))))

(test cmd-kill-pane-closes-fd
  "kill-pane on the last pane kills the window; session has 0 windows."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  ;; with-session handles cleanup of all pane PTYs via unwind-protect.
  (with-session (session 24 80)
    (let ((win (session-active-window session)))
      (declare (ignore win))
      ;; kill-pane on the sole pane must not signal an error.
      (finishes (kill-pane session))
      ;; Killing the only pane removes the window; no windows remain.
      (is (null (session-windows session))
          "session should have no windows after killing the last pane"))))

(test split-and-kill-returns-to-single
  "After splitting vertically and killing one pane, exactly one pane remains."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (with-session (session 24 80)
    (let ((win (session-active-window session)))
      ;; Split → 2 panes.
      (window-split win :h)
      (is (= 2 (length (window-panes win))))
      ;; Kill the active (second) pane → 1 pane should remain.
      (kill-pane session)
      (is (= 1 (length (window-panes (session-active-window session))))
          "one pane should remain after killing one of two"))))

;;;; ── Un-gated sandbox-safe unit tests ──────────────────────────────────────
;;;; These run real assertions without /dev/ptmx, a tty, or a socket.

(test pty-close-ignores-non-positive-pid
  "pty-close must never kill(-1)/kill(0): a non-positive pid and a negative
   master fd are both no-ops, so the call simply finishes without signalling."
  (finishes (cl-tmux/pty:pty-close -1 -1))
  (finishes (cl-tmux/pty:pty-close -1 0)))

(test terminal-size-returns-sane-clamped-geometry
  "terminal-size returns rows/cols clamped to the sane 1..+max-sane-*+ range.
   In the sandbox ioctl fails and it falls back to 24x80 — still in range."
  (multiple-value-bind (rows cols) (cl-tmux/pty:terminal-size)
    (is (<= 1 rows cl-tmux/pty::+max-sane-rows+))
    (is (<= 1 cols cl-tmux/pty::+max-sane-cols+))))

(test terminal-size-fallback-values-are-sane
  "The fallback 24x80 values used when ioctl fails are themselves sane."
  (is (<= 1 24 cl-tmux/pty::+max-sane-rows+)
      "fallback rows (24) must be within sane range")
  (is (<= 1 80 cl-tmux/pty::+max-sane-cols+)
      "fallback cols (80) must be within sane range"))

(test max-sane-bounds-are-reasonable
  "+max-sane-rows+ and +max-sane-cols+ are positive and at least 80/24."
  (is (>= cl-tmux/pty::+max-sane-rows+ 24)
      "+max-sane-rows+ must accommodate typical 24-row terminal")
  (is (>= cl-tmux/pty::+max-sane-cols+ 80)
      "+max-sane-cols+ must accommodate typical 80-col terminal"))

(test select-fds-empty-list-returns-nil
  "select-fds short-circuits on an empty fd list regardless of timeout,
   returning nil without ever calling select(2)."
  (is (null (cl-tmux/pty:select-fds '() 0)))
  (is (null (cl-tmux/pty:select-fds '() 100000)))
  (is (null (cl-tmux/pty:select-fds '() -1))))

(test pty-write-rejects-bad-type
  "pty-write's etypecase accepts only strings and octet vectors; any other
   type signals an error before any fd write is attempted."
  (signals error (cl-tmux/pty:pty-write -1 42))
  (signals error (cl-tmux/pty:pty-write -1 '(1 2 3))))

(test pty-write-empty-is-noop
  "An empty octet vector is guarded by (plusp len): no write(2) is issued,
   so writing to a bogus fd -1 finishes without error."
  (let ((empty (make-array 0 :element-type '(unsigned-byte 8))))
    (finishes (cl-tmux/pty:pty-write -1 empty))))

;;; ── Internal helper round-trips ─────────────────────────────────────────────

(test octets-to-foreign-and-back-round-trip
  "%octets-to-foreign + %foreign-to-octets forms a lossless round-trip."
  (let* ((original (make-array 5 :element-type '(unsigned-byte 8)
                                 :initial-contents '(0 1 127 128 255))))
    (cffi:with-foreign-object (buf :uint8 5)
      (cl-tmux/pty::%octets-to-foreign original buf 5)
      (let ((recovered (cl-tmux/pty::%foreign-to-octets buf 5)))
        (is (equalp original recovered)
            "round-trip through foreign memory must preserve all byte values")))))

(test octets-to-foreign-partial-copy
  "%octets-to-foreign copies exactly LEN bytes; only those bytes are written."
  (let ((src (make-array 4 :element-type '(unsigned-byte 8)
                           :initial-contents '(10 20 30 40))))
    (cffi:with-foreign-object (buf :uint8 4)
      ;; Zero the buffer first so we can check untouched bytes.
      (dotimes (i 4) (setf (cffi:mem-aref buf :uint8 i) 0))
      ;; Copy only 2 bytes.
      (cl-tmux/pty::%octets-to-foreign src buf 2)
      (is (= 10 (cffi:mem-aref buf :uint8 0)) "byte 0 must be 10")
      (is (= 20 (cffi:mem-aref buf :uint8 1)) "byte 1 must be 20")
      (is (= 0  (cffi:mem-aref buf :uint8 2)) "byte 2 must remain 0 (not written)")
      (is (= 0  (cffi:mem-aref buf :uint8 3)) "byte 3 must remain 0 (not written)"))))

(test foreign-to-octets-produces-correct-element-type
  "%foreign-to-octets returns a simple-array of (unsigned-byte 8)."
  (cffi:with-foreign-object (buf :uint8 3)
    (setf (cffi:mem-aref buf :uint8 0) 1
          (cffi:mem-aref buf :uint8 1) 2
          (cffi:mem-aref buf :uint8 2) 3)
    (let ((result (cl-tmux/pty::%foreign-to-octets buf 3)))
      (is (= 3 (length result)) "result must have length 3")
      (is (= 1 (aref result 0)) "element 0 must be 1")
      (is (= 2 (aref result 1)) "element 1 must be 2")
      (is (= 3 (aref result 2)) "element 2 must be 3"))))

(test pty-read-blocking-returns-nil-on-closed-fd
  "pty-read-blocking returns NIL when read(2) returns 0 (EOF) or negative."
  ;; A pipe whose write end is closed immediately delivers EOF on the read end.
  ;; with-pipe-fds is defined in test/helpers.lisp.
  (with-pipe-fds (rfd wfd)
    ;; Close the write end so the read end gets EOF.
    (sb-posix:close wfd)
    ;; wfd is now closed; with-pipe-fds will call ignore-errors on the second close.
    (let ((result (cl-tmux/pty:pty-read-blocking rfd 1)))
      (is (null result) "read on EOF pipe must return NIL"))))

(test select-fds-returns-list-type
  "select-fds always returns a list (possibly nil), never another type."
  (let ((result (cl-tmux/pty:select-fds '() 0)))
    (is (listp result) "empty-fd result must be a list")))

(test select-fds-with-pipe-data-returns-ready-fd
  "select-fds returns the readable fd in a list when data is available on a pipe."
  (with-pipe-fds (rfd wfd)
    (write-byte-to-fd wfd 99)
    (let ((ready (cl-tmux/pty:select-fds (list rfd) 200000)))
      (is (equal (list rfd) ready)
          "ready list must contain exactly rfd after write"))))

(test select-fds-zero-timeout-is-non-blocking
  "select-fds with timeout-us=0 returns NIL immediately on an idle fd."
  (with-pipe-fds (rfd _wfd)
    (let ((ready (cl-tmux/pty:select-fds (list rfd) 0)))
      (is (null ready)
          "non-blocking select on idle pipe must return NIL"))))

(test pty-read-blocking-returns-octet-vector-when-data-available
  "pty-read-blocking returns an (unsigned-byte 8) vector containing the written bytes."
  (with-pipe-fds (rfd wfd)
    (cffi:with-foreign-object (buf :uint8 3)
      (setf (cffi:mem-aref buf :uint8 0) 1
            (cffi:mem-aref buf :uint8 1) 2
            (cffi:mem-aref buf :uint8 2) 3)
      (cffi:foreign-funcall "write" :int wfd :pointer buf :unsigned-long 3 :long))
    (let ((result (cl-tmux/pty:pty-read-blocking rfd 4096)))
      (is-true result "pty-read-blocking must return non-NIL when data is present")
      (is (= 3 (length result)) "must return exactly 3 bytes")
      (is (= 1 (aref result 0)) "byte 0 must be 1")
      (is (= 2 (aref result 1)) "byte 1 must be 2")
      (is (= 3 (aref result 2)) "byte 2 must be 3"))))

(test pty-close-positive-pid-negative-fd-is-noop
  "pty-close with a valid positive pid but negative fd sends SIGHUP but skips close."
  ;; We can't test the kill call directly without a real process, but pty-close
  ;; with a bogus high pid should not error (kill may fail with ESRCH, ignored).
  (finishes (cl-tmux/pty:pty-close -1 99999999)
            "pty-close with negative fd and unknown pid must not signal"))

(test octets-to-foreign-zero-len-is-noop
  "%octets-to-foreign with len=0 writes nothing and finishes without error."
  (let ((src (make-array 0 :element-type '(unsigned-byte 8))))
    (cffi:with-foreign-object (buf :uint8 1)
      (setf (cffi:mem-aref buf :uint8 0) 0)
      (finishes (cl-tmux/pty::%octets-to-foreign src buf 0)
                "%octets-to-foreign len=0 must not error")
      (is (= 0 (cffi:mem-aref buf :uint8 0))
          "zero-len copy must not touch the buffer"))))

(test foreign-to-octets-zero-len-returns-empty-vector
  "%foreign-to-octets with byte-count=0 returns an empty octet vector."
  (cffi:with-foreign-object (buf :uint8 1)
    (let ((result (cl-tmux/pty::%foreign-to-octets buf 0)))
      (is (= 0 (length result)) "zero-len result must be empty")
      (is (typep result '(simple-array (unsigned-byte 8) (*)))
          "result must be an octet vector"))))

;;; ── New coverage: spawn helpers and microsecond constants ──────────────────

(test non-empty-string-p-rejects-empty-and-non-strings
  "%non-empty-string-p accepts only strings with positive length."
  (is-true (cl-tmux/pty::%non-empty-string-p "x"))
  (is-false (cl-tmux/pty::%non-empty-string-p ""))
  (is-false (cl-tmux/pty::%non-empty-string-p nil))
  (is-false (cl-tmux/pty::%non-empty-string-p 42)))

(test spawn-environment-assignments-preserves-override-order
  "%spawn-environment-assignments emits TERM first, then valid extra env pairs."
  (is (equal '("TERM=xterm-256color" "FOO=bar" "TERM=screen")
             (cl-tmux/pty::%spawn-environment-assignments
              "xterm-256color"
              '(("FOO" . "bar") ("TERM" . "screen") ("BAD" . 1) (42 . "no"))))))

(test microseconds-per-second-is-one-million
  "+microseconds-per-second+ is 1000000."
  (is (= 1000000 cl-tmux/pty::+microseconds-per-second+)
      "+microseconds-per-second+ must be 1000000"))

(test setup-timeval-decomposes-correctly
  "%setup-timeval decomposes microseconds into (seconds, remainder) correctly.
   1500000 us = 1 second + 500000 us."
  (cffi:with-foreign-object (tv :long 2)
    (cl-tmux/pty::%setup-timeval tv 1500000)
    (is (= 1 (cffi:mem-aref tv :long 0)) "seconds component must be 1")
    (is (= 500000 (cffi:mem-aref tv :long 1)) "microseconds component must be 500000")))

(test setup-timeval-zero-timeout
  "%setup-timeval with 0 produces (0, 0) — purely non-blocking."
  (cffi:with-foreign-object (tv :long 2)
    (cl-tmux/pty::%setup-timeval tv 0)
    (is (= 0 (cffi:mem-aref tv :long 0)) "seconds must be 0")
    (is (= 0 (cffi:mem-aref tv :long 1)) "microseconds must be 0")))

(test setup-timeval-sub-second-timeout
  "%setup-timeval with 50000 us (50 ms) produces (0, 50000)."
  (cffi:with-foreign-object (tv :long 2)
    (cl-tmux/pty::%setup-timeval tv 50000)
    (is (= 0 (cffi:mem-aref tv :long 0)) "seconds must be 0 for sub-second timeout")
    (is (= 50000 (cffi:mem-aref tv :long 1)) "microseconds must be 50000")))
