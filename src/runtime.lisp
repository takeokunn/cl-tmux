(in-package #:cl-tmux)

;;;; Runtime state and per-pane I/O threading.
;;;;
;;;; Threading model:
;;;;   * One reader thread per pane: blocking read(PTY fd) -> pane-feed ->
;;;;     screen update -> sets *dirty* T.
;;;;   * Main thread (see events.lisp): select(stdin, 50 ms) -> key dispatch or
;;;;     PTY forward -> render when *dirty*.
;;;;
;;;; All PTY children are forked before any reader threads start (see main.lisp)
;;;; to avoid fork-in-multithreaded-process hazards.

;;; -- Shared state -----------------------------------------------------------

(defvar *dirty*   t   "Set by reader threads; cleared by the main render step.")
(defvar *running* t   "Loop sentinel; set nil by :detach command.")
(defvar *resize-pending* nil
  "Set by the SIGWINCH handler; the event loop relayouts once and clears it.
   Polling terminal-size every frame is fragile (a transient garbage read
   triggers a spurious resize storm), so geometry is re-read only on signal.")
(defvar *term-rows* 24)
(defvar *term-cols* 80)

;;; -- SIGWINCH ---------------------------------------------------------------

(defun install-sigwinch-handler ()
  "Arm SIGWINCH so terminal resizes flag a one-shot relayout."
  (sb-sys:enable-interrupt
   sb-unix:sigwinch
   (lambda (&rest _)
     (declare (ignore _))
     (setf *resize-pending* t
           *dirty*           t))))

;;; -- PTY reader thread ------------------------------------------------------
;;;
;;; Data/logic separation: %pane-reader-loop is the pure logic (testable);
;;; start-reader-thread is the threading mechanism (the effect).

(defun %pane-reader-loop (pane)
  "Feed PTY output into PANE's screen until EOF or *running* becomes NIL.
   Polls with +pty-poll-timeout-us+ so the loop observes *running* even when
   the shell is silent, avoiding an eternal block on pty-read-blocking."
  (loop while *running* do
    (when (select-fds (list (pane-fd pane)) +pty-poll-timeout-us+)
      (let ((bytes (pty-read-blocking (pane-fd pane) +pty-buf-size+)))
        (unless bytes (return))   ; EOF — shell exited
        (pane-feed pane bytes)
        (setf *dirty* t)))))

(defun start-reader-thread (pane)
  "Spawn a thread running %pane-reader-loop for PANE."
  (make-thread (lambda () (%pane-reader-loop pane))
               :name (format nil "pty-reader-~D" (pane-id pane))))
