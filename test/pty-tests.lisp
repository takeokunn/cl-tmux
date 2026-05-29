(in-package #:cl-tmux/test)

;;;; PTY integration tests.  These spawn a real shell over a pseudo-terminal
;;;; and exercise the fork/exec/write/read/select pipeline end to end.
;;;;
;;;; PTY allocation needs /dev/ptmx, which sandboxed Nix builds do not provide.
;;;; When allocation fails we (skip) rather than fail, so the same suite runs
;;;; both in `nix develop` (real PTY) and `nix flake check` (sandboxed).
;;;;
;;;; pty-available-p is imported from cl-tmux/pty; no local shadow needed.

(def-suite pty-suite :description "PTY / shell integration")
(in-suite pty-suite)

(defun drain-pty (fd &key (deadline-seconds 3.0) (stop-marker nil))
  "Read from FD until STOP-MARKER appears in the decoded output or
   DEADLINE-SECONDS elapses.  Returns the accumulated string."
  (let ((acc  (make-array 0 :element-type '(unsigned-byte 8) :adjustable t
                            :fill-pointer 0))
        (end  (+ (get-internal-real-time)
                 (* deadline-seconds internal-time-units-per-second))))
    (loop
      (when (> (get-internal-real-time) end) (return))
      (when (select-fds (list fd) 200000)        ; 200 ms
        (let ((chunk (pty-read-blocking fd 4096)))
          (when chunk
            (loop for b across chunk
                  do (vector-push-extend b acc)))))
      (let ((text (map 'string #'code-char acc)))
        (when (and stop-marker (search stop-marker text))
          (return-from drain-pty text))))
    (map 'string #'code-char acc)))

(test shell-echoes-command-output
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (multiple-value-bind (fd pid) (forkpty-with-shell 24 80)
    (unwind-protect
         (let ((marker "CLTMUX_MARKER_42"))
           ;; Give the shell a moment to start, then send a command.
           (sleep 0.2)
           (pty-write fd (format nil "echo ~A~%" marker))
           (let ((out (drain-pty fd :stop-marker marker)))
             (is (search marker out)
                 "expected marker ~S in shell output, got ~S" marker out)))
      (pty-close fd pid))))

(test pty-write-accepts-octet-vector
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (multiple-value-bind (fd pid) (forkpty-with-shell 24 80)
    (unwind-protect
         (let ((bytes (map '(simple-array (unsigned-byte 8) (*))
                           #'char-code
                           (format nil "printf DONE_OCTETS~%"))))
           (sleep 0.2)
           (pty-write fd bytes)
           (let ((out (drain-pty fd :stop-marker "DONE_OCTETS")))
             (is (search "DONE_OCTETS" out))))
      (pty-close fd pid))))

(test select-times-out-when-idle
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (multiple-value-bind (fd pid) (forkpty-with-shell 24 80)
    (unwind-protect
         (progn
           ;; Drain the initial shell prompt, then expect idleness.
           (drain-pty fd :deadline-seconds 0.5)
           (let ((ready (select-fds (list fd) 100000)))  ; 100 ms, no input sent
             (is (null ready) "idle PTY should not be readable")))
      (pty-close fd pid))))

(test split-then-relayout-keeps-panes-fitting
  "Exercises the real resize path: forkpty per pane + ioctl(TIOCSWINSZ) +
   screen-resize, across a split and a subsequent terminal resize."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (let ((session (create-initial-session 24 80)))
    (unwind-protect
         (let ((win (session-active-window session)))
           ;; Split vertically → two panes side by side.
           (window-split win :vertical)
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
                   "divider column separates the two panes after relayout"))))
      ;; Clean up every shell we forked (initial + split).
      (dolist (p (all-panes session))
        (ignore-errors (pty-close (pane-fd p) (pane-pid p)))))))

(test cmd-kill-pane-closes-fd
  "kill-pane on the last pane kills the window; session has 0 windows."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (let ((session (create-initial-session 24 80)))
    ;; Capture the active pane fd before kill so we can verify no error.
    (let* ((win  (session-active-window session))
           (pane (window-active-pane win)))
      (declare (ignore pane))
      ;; kill-pane on the sole pane must not signal an error.
      (finishes (kill-pane session))
      ;; Killing the only pane removes the window; no windows remain.
      (is (null (session-windows session))
          "session should have no windows after killing the last pane"))))

(test split-and-kill-returns-to-single
  "After splitting vertically and killing one pane, exactly one pane remains."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (let ((session (create-initial-session 24 80)))
    (unwind-protect
         (let ((win (session-active-window session)))
           ;; Split → 2 panes.
           (window-split win :vertical)
           (is (= 2 (length (window-panes win))))
           ;; Kill the active (second) pane → 1 pane should remain.
           (kill-pane session)
           (is (= 1 (length (window-panes (session-active-window session))))
               "one pane should remain after killing one of two"))
      ;; Clean up any surviving shells.
      (dolist (p (all-panes session))
        (ignore-errors (pty-close (pane-fd p) (pane-pid p)))))))

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

(test disable-raw-mode-noop-when-not-saved
  "disable-raw-mode! is a no-op when no termios was saved: it must not touch
   the fd and must leave *saved-termios* nil."
  (let ((cl-tmux/pty::*saved-termios* nil))
    (finishes (cl-tmux/pty:disable-raw-mode! -1))
    (is (null cl-tmux/pty::*saved-termios*))))

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
