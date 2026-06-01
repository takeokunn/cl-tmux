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
(defvar *server-sessions* nil
  "Alist mapping session-name (string) to session object for the running server.")

;;; -- Wait-for channel synchronization ----------------------------------------
;;;
;;; *wait-channels* maps channel-name (string) to a list:
;;;   (:locked t/nil :cv condition-var :lock lock)
;;; :wait-for name  — block until channel is signaled
;;; :wait-for -S name — signal channel (unblock waiters)
;;; :wait-for -L name — lock channel (prevent signal until unlocked)
;;; :wait-for -U name — unlock channel

(defparameter *wait-channels* (make-hash-table :test #'equal)
  "Maps channel-name string to a plist (:lock lock :cv cv :locked bool).")

(defun %ensure-channel (name)
  "Return the plist for channel NAME, creating it if absent."
  (or (gethash name *wait-channels*)
      (let* ((lk (make-lock (format nil "wf-~A" name)))
             (cv (make-condition-variable :name (format nil "wf-cv-~A" name)))
             (ch (list :lock lk :cv cv :locked nil)))
        (setf (gethash name *wait-channels*) ch)
        ch)))

(defun wait-for-channel (name)
  "Block the calling thread until channel NAME is signaled."
  (let* ((ch (%ensure-channel name))
         (lk (getf ch :lock))
         (cv (getf ch :cv)))
    (with-lock-held (lk)
      (condition-wait cv lk))))

(defun signal-channel (name)
  "Signal all threads blocked on channel NAME."
  (let* ((ch (%ensure-channel name))
         (lk (getf ch :lock))
         (cv (getf ch :cv)))
    (unless (getf ch :locked)
      (with-lock-held (lk)
        (condition-notify cv)))))

(defun lock-channel (name)
  "Lock channel NAME so signal-channel is a no-op until unlocked."
  (let ((ch (%ensure-channel name)))
    (setf (getf ch :locked) t)))

(defun unlock-channel (name)
  "Unlock channel NAME, allowing signal-channel to proceed."
  (let ((ch (%ensure-channel name)))
    (setf (getf ch :locked) nil)))

;;; NOTE: popup, menu structs, *active-popup*, *active-menu* live in
;;; src/prompt.lisp (cl-tmux/prompt package) so the renderer can see them.

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
   the shell is silent, avoiding an eternal block on pty-read-blocking.
   When PANE has an active pipe-fd, bytes are also tee'd to it."
  (loop while *running* do
    (when (select-fds (list (pane-fd pane)) +pty-poll-timeout-us+)
      (let ((bytes (pty-read-blocking (pane-fd pane) +pty-buf-size+)))
        (unless bytes (return))   ; EOF — shell exited
        ;; Tee output to pipe-pane if active.
        (when (pane-pipe-fd pane)
          (pipe-pane-write pane bytes))
        (pane-feed pane bytes)
        (setf *dirty* t)))))

(defun start-reader-thread (pane)
  "Spawn a thread running %pane-reader-loop for PANE."
  (make-thread (lambda () (%pane-reader-loop pane))
               :name (format nil "pty-reader-~D" (pane-id pane))))
