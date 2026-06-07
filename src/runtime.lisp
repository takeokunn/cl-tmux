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
  "Set by the SIGWINCH handler; the event loop relayouts once and clears it.")
(defvar *term-rows* 24)
(defvar *term-cols* 80)
(defvar *server-sessions* nil
  "Alist mapping session-name (string) to session object for the running server.")
(defvar *status-timer* nil "Background thread for status-interval redraws.")

;;; -- Named constants --------------------------------------------------------

(defconstant +max-message-log-entries+ 100
  "Maximum number of entries retained in *message-log*.")

(defconstant +reader-thread-join-timeout+ 10
  "Seconds (real number) to wait for a PTY reader thread to terminate before
   giving up.  Passed directly to bordeaux-threads:join-thread :timeout, which
   expects a real number of seconds on SBCL.")

(defconstant +wait-for-channel-timeout+ 30
  "Seconds before wait-for-channel gives up waiting for a signal.
   A bounded wait prevents indefinite blocking when signal-channel is
   never called (e.g., after an unexpected server shutdown).")

;;; -- Wait-for channel synchronization ----------------------------------------

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
  "Block the calling thread until channel NAME is signaled, or until
   +wait-for-channel-timeout+ seconds elapse.  Returns T if signaled, NIL
   on timeout.  A bounded wait prevents indefinite blocking when the
   corresponding signal-channel is never called."
  (let* ((ch (%ensure-channel name))
         (lk (getf ch :lock))
         (cv (getf ch :cv)))
    (with-lock-held (lk)
      (condition-wait cv lk :timeout +wait-for-channel-timeout+))))

(defun signal-channel (name)
  "Signal all threads blocked on channel NAME."
  (let* ((ch (%ensure-channel name))
         (lk (getf ch :lock))
         (cv (getf ch :cv)))
    (unless (getf ch :locked)
      (with-lock-held (lk)
        (condition-notify cv)))))

(defun lock-channel (name)
  "Lock channel NAME so signal-channel is suppressed (a no-op) until unlocked.
   While a channel is locked, any call to signal-channel for the same NAME
   checks the :locked flag and skips the condition-notify entirely.  This
   allows callers to temporarily block notifications without losing them
   permanently — the channel is not destroyed, only silenced."
  (let ((ch (%ensure-channel name)))
    (setf (getf ch :locked) t)))

(defun unlock-channel (name)
  "Unlock channel NAME, allowing subsequent signal-channel calls to notify waiters.
   Paired with lock-channel: once unlocked, signal-channel will again call
   condition-notify on the channel's condition variable.  Does not retroactively
   deliver signals that were suppressed while the channel was locked."
  (let ((ch (%ensure-channel name)))
    (setf (getf ch :locked) nil)))

;;; -- Message log -------------------------------------------------------------

(defvar *message-log* nil
  "A list of (timestamp . text) cons pairs for :show-messages.")

(defun add-message-log (msg)
  "Prepend MSG to *message-log*, capping the list at +max-message-log-entries+."
  (push (cons (get-universal-time) msg) *message-log*)
  (when (> (length *message-log*) +max-message-log-entries+)
    (setf *message-log* (subseq *message-log* 0 +max-message-log-entries+))))

;;; -- Prompt history ----------------------------------------------------------

(defconstant +max-prompt-history+ 100
  "Maximum number of entries retained in *prompt-history*.")

(defvar *prompt-history* nil
  "A list of strings — the most recent command-prompt inputs, newest first.
   Populated by the :command-prompt handler; shown by :show-prompt-history.")

(defun add-prompt-history (entry)
  "Prepend ENTRY to *prompt-history*, capping at +max-prompt-history+."
  (when (and (stringp entry) (plusp (length entry)))
    (push entry *prompt-history*)
    (when (> (length *prompt-history*) +max-prompt-history+)
      (setf *prompt-history* (subseq *prompt-history* 0 +max-prompt-history+)))))

;;; -- Clock mode --------------------------------------------------------------

(defvar *clock-mode-pane-id* nil
  "When non-NIL, the pane-id of the pane displaying a digital clock overlay.")

;;; NOTE: popup, menu structs, *active-popup*, *active-menu* live in
;;; src/prompt.lisp (cl-tmux/prompt package) so the renderer can see them.

;;; -- SIGWINCH ---------------------------------------------------------------

(defun install-sigwinch-handler ()
  "Arm SIGWINCH so terminal resizes flag a one-shot relayout."
  (sb-sys:enable-interrupt
   sb-unix:sigwinch
   (lambda (&rest ignored)
     (declare (ignore ignored))
     (setf *resize-pending* t
           *dirty*           t))))

;;; -- PTY reader thread ------------------------------------------------------
;;;
;;; CPS state machine: each state function takes (pane) and returns the next
;;; state function (or NIL to stop).

;;; ANSI SGR sequence displayed on the pane when remain-on-exit is active.
;;; SGR 7 = reverse video; SGR 0 (implicit via reset) restores normal.
;;; Defined as a variable (not defconstant) because SBCL's DEFCONSTANT
;;; requires EQL identity across reloads, which string values fail.
(defvar +remain-on-exit-message+
  (format nil "~C[7m[Process exited]~C[m" #\Escape #\Escape)
  "Reverse-video banner written to the pane screen when remain-on-exit is set.")

(defun reader-idle-state (pane)
  "Poll the pane PTY fd; transition to reading if data is available."
  (if (select-fds (list (pane-fd pane)) +pty-poll-timeout-us+)
      #'reader-reading-state
      #'reader-idle-state))

(defun reader-reading-state (pane)
  "Read one PTY chunk and feed it to PANE; transition to eof if EOF."
  (let ((bytes (pty-read-blocking (pane-fd pane) +pty-buf-size+)))
    (cond
      ((null bytes)
       #'reader-eof-state)
      (t
       (when (pane-pipe-fd pane)
         (pipe-pane-write pane bytes))
       (pane-feed pane bytes)
       (setf *dirty* t)
       #'reader-idle-state))))

(defun reader-remain-on-exit-state (pane)
  "CPS spin state: park the reader thread while *running* is true.
   Returns itself to keep the driver loop alive, or NIL when *running* clears.
   Uses a short sleep so the loop yields the CPU; the pane stays visible."
  (declare (ignore pane))
  (if *running*
      (progn (sleep 0.1) #'reader-remain-on-exit-state)
      nil))

(defun reader-eof-state (pane)
  "Fire the pane-exited hook and determine the next CPS state.
   When 'remain-on-exit' is set, write a notice to the pane screen and
   transition to reader-remain-on-exit-state so the pane stays visible.
   Otherwise return NIL to stop the reader loop immediately."
  (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-pane-exited+ pane)
  (let ((remain-on-exit
          (handler-case (cl-tmux/options:get-option "remain-on-exit")
            (error () nil))))
    (cond
      (remain-on-exit
       ;; Write [Process exited] banner to the pane screen.
       (let ((screen (pane-screen pane)))
         (when screen
           (let ((message-bytes
                   (babel:string-to-octets +remain-on-exit-message+
                                           :encoding :utf-8)))
             (cl-tmux/terminal/emulator:screen-process-bytes screen message-bytes))))
       (setf *dirty* t)
       ;; Return the parking state: the driver loop calls it on each tick.
       #'reader-remain-on-exit-state)
      (t nil))))

(defun %run-reader-states (pane initial-state)
  "Drive the CPS reader state machine for PANE starting from INITIAL-STATE."
  (loop for state = initial-state then (funcall state pane)
        while (and *running* state)))

(defun %pane-reader-loop (pane)
  "Feed PTY output into PANE screen until EOF or *running* becomes NIL."
  (%run-reader-states pane #'reader-idle-state))

(defun start-reader-thread (pane)
  "Spawn a thread running %pane-reader-loop for PANE."
  (make-thread (lambda () (%pane-reader-loop pane))
               :name (format nil "pty-reader-~D" (pane-id pane))))

(defun stop-reader-threads (threads)
  "Signal shutdown and join each thread in THREADS with a bounded timeout."
  (setf *running* nil)
  (dolist (thread threads)
    (ignore-errors
      (bordeaux-threads:join-thread thread
                                    :timeout +reader-thread-join-timeout+))))

;;; -- Status interval timer --------------------------------------------------

(defconstant +status-timer-poll-seconds+ 0.1
  "Granularity (seconds) at which the status timer re-checks *running* and the
   elapsed interval.  Kept small so the thread shuts down promptly when *running*
   clears instead of holding a multi-second sleep that would outlive the
   join-thread timeout and leak the thread (which, fatally, makes every
   subsequent fork() fail with \"Cannot fork with multiple threads running\").")

(defun %maybe-auto-dismiss-overlay ()
  "Auto-dismiss the active overlay when display-time milliseconds have elapsed.
   display-time is in ms (default 750); the timer resolution is 0.1s so actual
   dismiss may lag up to 100ms.  Only affects transient overlays shown without
   :no-timer; long-lived paged overlays (list-keys, list-sessions) use :no-timer."
  (when (cl-tmux/prompt:overlay-active-p)
    (let* ((display-time-ms (or (cl-tmux/options:get-option "display-time") 750))
           (display-secs    (/ display-time-ms 1000.0))
           (shown-at        cl-tmux/prompt::*overlay-shown-at*)
           (elapsed         (- (get-universal-time) shown-at)))
      (when (and (plusp shown-at) (>= elapsed display-secs))
        (cl-tmux/prompt:clear-overlay)
        t))))

(defun start-status-timer (dirty-fn)
  "Start a background thread that calls DIRTY-FN every status-interval seconds.
   DIRTY-FN should mark the session dirty to trigger a status bar redraw.
   The thread polls *running* at +status-timer-poll-seconds+ granularity and
   accumulates elapsed time, firing DIRTY-FN once per status-interval, so it
   exits within one poll tick of *running* clearing.
   Also drives auto-dismiss of transient overlays per display-time option.
   Returns the thread object."
  (make-thread
   (lambda ()
     (let ((elapsed 0))
       (loop while *running*
             do (sleep +status-timer-poll-seconds+)
                (incf elapsed +status-timer-poll-seconds+)
                ;; Auto-dismiss transient overlays after display-time ms.
                (when (%maybe-auto-dismiss-overlay) (funcall dirty-fn))
                (let ((interval (max 1 (cl-tmux/options:get-option "status-interval"))))
                  (when (and *running* (>= elapsed interval))
                    (setf elapsed 0)
                    (funcall dirty-fn))))))
   :name "cl-tmux-status-timer"))
