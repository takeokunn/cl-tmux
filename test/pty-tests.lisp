(in-package #:cl-tmux/test)

;;;; PTY integration tests.  These spawn a real shell over a pseudo-terminal
;;;; and exercise the fork/exec/write/read/select pipeline end to end.
;;;;
;;;; PTY allocation needs /dev/ptmx, which sandboxed Nix builds do not provide.
;;;; When allocation fails we (skip) rather than fail, so the same suite runs
;;;; both in `nix develop` (real PTY) and `nix flake check` (sandboxed).

(def-suite pty-suite :description "PTY / shell integration")
(in-suite pty-suite)

(defun pty-available-p ()
  "True if we can allocate a PTY in this environment."
  (handler-case
      (multiple-value-bind (fd pid) (forkpty-with-shell 24 80)
        (pty-close fd pid)
        t)
    (error () nil)))

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
