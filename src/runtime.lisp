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

(defun %mark-window-activity (win)
  "Mark WIN as having activity for monitor-activity: set the activity flag, fire
   the alert-activity hook, and show a visual-activity overlay when that option is
   on.  No-op when WIN is NIL, monitor-activity is off for WIN, or the flag is
   already set.  Extracted from reader-reading-state so the alert-activity firing
   is unit-testable without a live PTY."
  (when (and win
             (cl-tmux/options:get-option-for-context "monitor-activity" :window win)
             (not (cl-tmux/model:window-activity-flag win)))
    (setf (cl-tmux/model:window-activity-flag win) t)
    ;; Fire the alert-activity hook (matches real tmux).
    (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-alert-activity+ win)
    ;; visual-activity on: show a transient message overlay so the user knows
    ;; which background window has activity (matches real tmux).
    (when (cl-tmux/options:get-option "visual-activity")
      (show-transient-overlay
       (format nil "Activity in window ~A (~A)"
               (cl-tmux/model:window-id win)
               (cl-tmux/model:window-name win))))))

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
       ;; monitor-activity: set the window's activity flag when bytes arrive
       ;; in a background window and monitor-activity is enabled.
       ;; Also stamp last-output-time for monitor-silence and clear silence-flag
       ;; (receiving output resets the silence timer for this window).
       (let ((win (cl-tmux/model:pane-window pane)))
         (when win
           ;; Always update last-output-time (used by monitor-silence timer).
           (setf (cl-tmux/model:window-last-output-time win) (get-universal-time))
           ;; Clear silence flag: new output resets the silence state.
           (setf (cl-tmux/model:window-silence-flag win) nil)
           ;; Activity flag + alert-activity hook + visual overlay.
           (%mark-window-activity win)))
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
          (handler-case (cl-tmux/options:get-option-for-context "remain-on-exit" :pane pane)
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

;;; *last-activity-time*: updated by process-byte on each keystroke; used by
;;; lock-after-time to measure idle time.  Initialised to the startup time so
;;; lock-after-time does not fire immediately on an idle session start.
(defvar *last-activity-time* 0
  "Universal-time of the most recent keypress.  Stamped in process-byte.")

(defun %check-lock-after-time (session dirty-fn)
  "Lock SESSION when lock-after-time seconds of keyboard idle have elapsed.
   lock-after-time = 0 (default) disables the auto-lock.  A no-op when the
   session is already locked."
  (let ((lock-secs (cl-tmux/options:get-option "lock-after-time")))
    (when (and (integerp lock-secs) (> lock-secs 0)
               (not (cl-tmux/model:session-locked-p session)))
      (when (>= (- (get-universal-time) *last-activity-time*) lock-secs)
        (setf (cl-tmux/model:session-locked-p session) t)
        (funcall dirty-fn)))))

(defun %check-monitor-silence (sessions dirty-fn)
  "For each window in SESSIONS with monitor-silence enabled, set
   window-silence-flag when no PTY output has arrived for monitor-silence seconds.
   monitor-silence = 0 (default) disables per-window silence monitoring."
  (let ((silence-secs (cl-tmux/options:get-option "monitor-silence")))
    (when (and (integerp silence-secs) (> silence-secs 0))
      (let ((now (get-universal-time)))
        (dolist (entry sessions)
          (let ((sess (cdr entry)))
            (dolist (win (cl-tmux/model:session-windows sess))
              (let ((last-out (cl-tmux/model:window-last-output-time win)))
                (when (and (> last-out 0)
                           (not (cl-tmux/model:window-silence-flag win))
                           (>= (- now last-out) silence-secs))
                  (setf (cl-tmux/model:window-silence-flag win) t)
                  ;; Fire the alert-silence hook (matches real tmux).
                  (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-alert-silence+ win)
                  (funcall dirty-fn))))))))))

(defun start-status-timer (dirty-fn &key session server-sessions-fn)
  "Start a background thread that drives periodic session maintenance:
   - STATUS-INTERVAL: calls DIRTY-FN every N seconds to refresh the status bar.
   - DISPLAY-TIME: auto-dismisses transient overlays after configured ms.
   - LOCK-AFTER-TIME: locks SESSION after N seconds of keyboard inactivity.
   - MONITOR-SILENCE: sets window-silence-flag after N seconds of PTY silence.
   SESSION and SERVER-SESSIONS-FN are optional; lock/silence checks are skipped
   when absent.  Returns the thread object."
  (make-thread
   (lambda ()
     (let ((elapsed 0))
       (loop while *running*
             do (sleep +status-timer-poll-seconds+)
                (incf elapsed +status-timer-poll-seconds+)
                ;; Auto-dismiss transient overlays after display-time ms.
                (when (%maybe-auto-dismiss-overlay) (funcall dirty-fn))
                ;; Status-interval: refresh the status bar every N seconds.
                ;; status-interval 0 DISABLES the periodic redraw entirely (tmux
                ;; semantics) — the status bar then only updates on other dirty
                ;; events, not on a timer.  A positive value fires every N seconds.
                (let ((interval (cl-tmux/options:get-option "status-interval")))
                  (when (and *running* (integerp interval) (> interval 0)
                             (>= elapsed interval))
                    (setf elapsed 0)
                    (funcall dirty-fn)))
                ;; lock-after-time: auto-lock on inactivity.
                (when (and *running* session)
                  (ignore-errors (%check-lock-after-time session dirty-fn)))
                ;; monitor-silence: flag windows with no recent PTY output.
                (when (and *running* server-sessions-fn)
                  (ignore-errors
                    (%check-monitor-silence (funcall server-sessions-fn) dirty-fn))))))
   :name "cl-tmux-status-timer"))
