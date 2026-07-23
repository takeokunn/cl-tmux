(in-package #:cl-tmux)

;;;; PTY reader thread — CPS state machine.
;;;;
;;;; This file contains the per-pane I/O thread and state machine.  The
;;;; remain-on-exit and alert-action helpers live in runtime-reader-alerts.lisp.
;;;; It is loaded after runtime.lisp (shared state, channel sync, prompt
;;;; history) and before runtime-timer.lisp.
;;;;
;;;; Threading model recap:
;;;;   * One reader thread per pane: blocking read(PTY fd) -> pane-feed ->
;;;;     screen update -> sets *dirty* T.
;;;;   * Main thread: select(stdin, 50 ms) -> key dispatch -> render when dirty.

;;; -- PTY reader thread -------------------------------------------------------
;;;
;;; CPS state machine: each state function takes (pane) and returns the next
;;; state function (or NIL to stop).

(defvar *reader-scratch-buffer* nil
  "Per-reader-thread scratch octet buffer reused by reader-reading-state to read
   one PTY chunk without allocating a fresh +pty-buf-size+ buffer on every read.
   Bound (thread-locally) around each reader loop in %pane-reader-loop, so each
   pane's reader thread owns a distinct buffer.  pty-read-blocking-into returns a
   fresh exact-size copy of the bytes read, so handing that copy downstream is
   safe even though the scratch buffer is overwritten by the next read.")

(defun reader-idle-state (pane)
  "Poll the pane PTY fd; transition to reading if data is available."
  (if (select-fds (list (pane-fd pane)) +pty-poll-timeout-us+)
      #'reader-reading-state
      #'reader-idle-state))

(defun reader-reading-state (pane)
  "Read one PTY chunk and feed it to PANE; transition to eof if EOF."
  (let ((bytes (pty-read-blocking-into (pane-fd pane) *reader-scratch-buffer*)))
    (if (null bytes)
        #'reader-eof-state
        (progn
          (when (pane-pipe-fd pane)
            (pipe-pane-write pane bytes))
          (pane-feed pane bytes)
          (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-pane-output+ pane bytes)
          (%update-window-on-pane-output (cl-tmux/model:pane-window pane) pane)
          (%mark-dirty)
          #'reader-idle-state))))

(defconstant +remain-on-exit-poll-seconds+ 0.1
  "Sleep granularity (seconds) for the remain-on-exit parking spin loop.
   Derived from +status-timer-poll-seconds+ for consistency: both loops yield
   the CPU at the same cadence.")

(defun reader-remain-on-exit-state (pane)
  "CPS spin state: park the reader thread while *running* is true.
   Returns itself to keep the driver loop alive, or NIL when *running* clears.
   Uses a short sleep so the loop yields the CPU; the pane stays visible.
   The loop is bounded by the *running* sentinel: when the server shuts down,
   stop-reader-threads sets *running* NIL and joins this thread with a timeout."
  (declare (ignore pane))
  (when *running*
    (sleep +remain-on-exit-poll-seconds+)
    #'reader-remain-on-exit-state))

(defun reader-eof-state (pane)
  "Fire the pane-exited hook and determine the next CPS state.
   When 'remain-on-exit' is set, write a notice to the pane screen and
   transition to reader-remain-on-exit-state so the pane stays visible.
   Otherwise return NIL to stop the reader loop immediately."
  (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-pane-exited+ pane)
  ;; The child has exited and the master fd is now at EOF.  Mark the pane DEAD:
  ;; close the master fd (nothing else closes it on the remain-on-exit path — a
  ;; leak) and reset pane-fd/pane-pid to -1.  #{pane_dead} keys on (<= pane-fd 0)
  ;; (format.lisp), and respawn-pane (without -k) is gated on the pane being dead —
  ;; both were wrong because the reader never reset the fd.  Resetting pane-pid too
  ;; prevents a later teardown (e.g. %destroy-session) from re-signalling a stale
  ;; (possibly OS-reused) pid; respawn-pane re-establishes both slots.  pty-close
  ;; guards non-positive fd/pid, so no-PTY panes (fd -1) are an untouched no-op.
  (when (> (pane-fd pane) 0)
    ;; Record the death BEFORE pty-close (which forgets the child process):
    ;; exit code / signal / time drive #{pane_dead_status}/#{pane_dead_signal}/
    ;; #{pane_dead_time} and the remain-on-exit banner.
    (multiple-value-bind (code kind)
        (ignore-errors (cl-tmux/pty:pty-child-exit-status (pane-fd pane)))
      (when code
        (ecase kind
          (:exited   (setf (cl-tmux/model:pane-dead-status pane) code))
          (:signaled (setf (cl-tmux/model:pane-dead-signal pane) code)))))
    (setf (cl-tmux/model:pane-dead-time pane) (get-universal-time))
    (close-pane-pty pane)
    (setf (pane-fd pane) -1
          (pane-pid pane) -1))
  (let ((remain-on-exit
          (handler-case (cl-tmux/options:get-option-for-context "remain-on-exit" :pane pane)
            (error () nil))))
    (when remain-on-exit
      ;; Write the remain-on-exit-format banner (reverse-video) to the pane screen.
      (%write-remain-on-exit-banner pane)
      ;; tmux fires pane-died (in addition to the unconditional pane-exited above)
      ;; only on the remain-on-exit branch, where the dead pane stays visible.
      (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-pane-died+ pane)
      (%mark-dirty)
      ;; Return the parking state: the driver loop calls it on each tick.
      #'reader-remain-on-exit-state)))

(defun %run-reader-states (pane initial-state)
  "Drive the CPS reader state machine for PANE starting from INITIAL-STATE."
  (loop for state = initial-state then (funcall state pane)
        while (and *running* state)))

(defun %pane-reader-loop (pane)
  "Feed PTY output into PANE screen until EOF or *running* becomes NIL."
  ;; Allocate ONE scratch read buffer for this reader thread (one thread per
  ;; pane) and bind it thread-locally for reader-reading-state to reuse, so the
  ;; hot read path no longer allocates a +pty-buf-size+ buffer per read.
  (let ((*reader-scratch-buffer*
          (make-array +pty-buf-size+ :element-type '(unsigned-byte 8))))
    (%run-reader-states pane #'reader-idle-state)))

(defun start-reader-thread (pane)
  "Spawn a thread running %pane-reader-loop for PANE."
  (make-thread (lambda () (%pane-reader-loop pane))
               :name (format nil "pty-reader-~D" (pane-id pane))))

(defun stop-reader-threads (threads)
  "Signal shutdown and join each thread in THREADS with a bounded timeout."
  (setf *running* nil)
  (dolist (thread threads)
    (ignore-errors
      (%join-thread-with-timeout thread +reader-thread-join-timeout+))))
