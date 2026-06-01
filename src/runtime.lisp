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

;;; -- Status-bar timer -------------------------------------------------------
;;;
;;; The status-interval option (default 15 seconds) controls how often the
;;; status bar is repainted even without user input (e.g. to update the clock).
;;; *status-dirty* is set by the timer thread; the render loop clears it.

(defparameter *status-dirty* nil
  "Set by the status-bar timer thread to trigger a status-bar repaint.")

(defvar *status-timer-thread* nil
  "Thread object for the status-bar interval timer, or NIL if not started.")

(defun start-status-timer ()
  "Start a background thread that sets *STATUS-DIRTY* every STATUS-INTERVAL seconds.
   Only one timer thread runs at a time; calling this when one is already
   running is a no-op.  Returns the thread object."
  (when (and *status-timer-thread*
             (bordeaux-threads:thread-alive-p *status-timer-thread*))
    (return-from start-status-timer *status-timer-thread*))
  (setf *status-timer-thread*
        (bordeaux-threads:make-thread
         (lambda ()
           (loop while *running* do
             (let ((interval (cl-tmux/options:get-option "status-interval" 15)))
               (sleep (max 1 interval)))
             (setf *status-dirty* t
                   *dirty*        t)))
         :name "cl-tmux-status-timer")))

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

;;; -- Message log -------------------------------------------------------------
;;;
;;; *message-log* holds recent :display-message entries as (timestamp . text)
;;; cons pairs.  Capped at 100 entries to prevent unbounded growth.

(defvar *message-log* nil
  "A list of (timestamp . text) cons pairs for :show-messages.
   Prepended on each new message; capped at 100 entries.")

(defun add-message-log (msg)
  "Prepend MSG to *message-log*, capping the list at 100 entries."
  (push (cons (get-universal-time) msg) *message-log*)
  (when (> (length *message-log*) 100)
    (setf *message-log* (subseq *message-log* 0 100))))

;;; -- Clock mode --------------------------------------------------------------

(defvar *clock-mode-pane-id* nil
  "When non-NIL, the pane-id of the pane displaying a digital clock overlay.
   The renderer overlays the clock when this matches the rendered pane's id.")

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
