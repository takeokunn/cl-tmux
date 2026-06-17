(in-package #:cl-tmux)

;;;; Runtime state and per-pane I/O threading.
;;;;
;;;; Threading model:
;;;;   * One reader thread per pane: blocking read(PTY fd) -> pane-feed ->
;;;;     screen update -> sets *dirty* T.
;;;;   * Main thread (see events.lisp): select(stdin, 50 ms) -> key dispatch or
;;;;     PTY forward -> render when *dirty*.
;;;;
;;;; PTY children may be spawned while reader/status threads are active, so
;;;; teardown must reliably join background threads and close pane processes.

;;; -- Shared state -----------------------------------------------------------

(defvar *dirty*   t   "Set by reader threads; cleared by the main render step.")
(defvar *running* t   "Loop sentinel; set nil by :detach command.")
(defvar *resize-pending* nil
  "Set by the SIGWINCH handler; the event loop relayouts once and clears it.")
(defvar *term-rows* 24)
(defvar *term-cols* 80)
(defvar *server-sessions* nil
  "Alist mapping session-name (string) to session object for the running server.")
(defvar *server-marked-pane* nil
  "The single server-wide marked pane (set by mark-pane / select-pane -m).
   NIL when no pane is marked.  Mirrors tmux's global marked-pane singleton.")
(defvar *client-read-only* nil
  "When non-NIL the attached client is read-only: keystrokes and mouse events
   are NOT forwarded to panes.  Set by attach-session -r.")
(defvar *status-timer* nil "Background thread for status-interval redraws.")

;;; -- Named constants --------------------------------------------------------

(defconstant +max-message-log-entries+ 100
  "Maximum number of entries retained in *message-log*.")

(defconstant +reader-thread-join-timeout+ 10
  "Seconds (real number) to wait for a PTY reader thread to terminate before
   giving up when the Lisp implementation supports bounded thread joins.")

(defun %join-thread-with-timeout (thread &optional (timeout +reader-thread-join-timeout+))
  "Join THREAD with a bounded wait when available.

   Bordeaux Threads does not standardize a timeout argument to JOIN-THREAD; SBCL
   provides one on SB-THREAD:JOIN-THREAD.  On other implementations, poll for the
   thread to exit and only call the portable join once it is already dead."
  #+sbcl
  (sb-thread:join-thread thread :timeout timeout)
  #-sbcl
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout internal-time-units-per-second)))))
    (loop while (and (bordeaux-threads:thread-alive-p thread)
                     (< (get-internal-real-time) deadline))
          do (sleep 0.01))
    (unless (bordeaux-threads:thread-alive-p thread)
      (bordeaux-threads:join-thread thread))))

(defconstant +wait-for-channel-timeout+ 30
  "Seconds before wait-for-channel gives up waiting for a signal.
   A bounded wait prevents indefinite blocking when signal-channel is
   never called (e.g., after an unexpected server shutdown).")

(defconstant +default-display-time-ms+ 750
  "Default overlay display time in milliseconds when the display-time option is
   unset.  The status timer checks every +status-timer-poll-seconds+, so actual
   dismiss may lag up to that granularity.")

(defconstant +ms-per-second+ 1000.0
  "Milliseconds per second; used to convert display-time (ms) to seconds.")

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

(defun %cap-list (list limit)
  "Return LIST truncated to at most LIMIT elements; returns LIST unchanged when
   it already fits."
  (if (> (length list) limit) (subseq list 0 limit) list))

;;; -- Message log -------------------------------------------------------------

(defvar *message-log* nil
  "A list of (timestamp . text) cons pairs for :show-messages.")

(defvar *current-client-conn* nil
  "The client connection currently being served by the server-side command path,
   or NIL when running commands without a specific client context.")

(defun %message-log-limit ()
  "The effective message-log cap: the `message-limit` option (tmux default 1000),
   falling back to +max-message-log-entries+ when unset."
  (or (cl-tmux/options:get-option "message-limit")
      +max-message-log-entries+))

(defun %append-message-log-entry (log entry)
  "Prepend ENTRY to LOG and cap the result at the effective message-log limit."
  (%cap-list (cons entry log) (%message-log-limit)))

(defun add-message-log (msg)
  "Prepend MSG to *message-log*, capping the list at the `message-limit` option
   (tmux default 1000), falling back to +max-message-log-entries+ when unset."
  (let ((entry (cons (get-universal-time) msg)))
    (setf *message-log* (%append-message-log-entry *message-log* entry))
    (when *current-client-conn*
      (setf (client-conn-message-log *current-client-conn*)
            (%append-message-log-entry
             (client-conn-message-log *current-client-conn*)
             entry)))))

;;; -- Prompt history ----------------------------------------------------------

(defconstant +max-prompt-history+ 100
  "Maximum number of entries retained in *prompt-history*.")

(defvar *prompt-history* nil
  "A list of strings — the most recent command-prompt inputs, newest first.
   Populated by the :command-prompt handler; shown by :show-prompt-history.")

(defun %prompt-history-path ()
  "The configured history-file path (a non-empty string) or NIL when unset —
   NIL means command-prompt history is in-memory only (no persistence)."
  (let ((p (ignore-errors (cl-tmux/options:get-option "history-file"))))
    (and (stringp p) (plusp (length p)) p)))

(defun save-prompt-history ()
  "Write *prompt-history* to the history-file, one entry per line, OLDEST first
   (so a later load preserves recency order).  No-op when history-file is unset;
   best-effort (I/O errors are ignored)."
  (let ((path (%prompt-history-path)))
    (when path
      (ignore-errors
        (with-open-file (s path :direction :output :if-exists :supersede
                                :if-does-not-exist :create)
          (dolist (entry (reverse *prompt-history*))
            (write-line entry s)))))))

(defun %effective-prompt-history-limit ()
  "The effective command-prompt history cap: the `prompt-history-limit` option
   (tmux default 100), falling back to +max-prompt-history+ when unset."
  (or (cl-tmux/options:get-option "prompt-history-limit") +max-prompt-history+))

(defun load-prompt-history ()
  "Load *prompt-history* from the history-file (one entry per line, oldest first),
   newest-first in memory, capped at +max-prompt-history+.  No-op when the option
   is unset or the file is unreadable."
  (let ((path (%prompt-history-path)))
    (when (and path (probe-file path))
      (ignore-errors
        (with-open-file (s path :direction :input :if-does-not-exist nil)
          (when s
            (let ((entries nil))
              (loop for line = (read-line s nil nil) while line
                    do (when (plusp (length line)) (push line entries)))
              (setf *prompt-history*
                    (subseq entries 0 (min (length entries) (%effective-prompt-history-limit)))))))))))

(defun add-prompt-history (entry)
  "Prepend ENTRY to *prompt-history*, capping at the prompt-history-limit option,
   and persist to the history-file when that option is set."
  (when (and (stringp entry) (plusp (length entry)))
    (push entry *prompt-history*)
    (let ((limit (%effective-prompt-history-limit)))
      (setf *prompt-history* (%cap-list *prompt-history* limit)))
    (save-prompt-history)))

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
  "Fallback reverse-video banner written to the pane screen when remain-on-exit is
   set but remain-on-exit-format is empty or fails to expand.")

(defun %remain-on-exit-banner (pane)
  "The reverse-video banner for a pane kept open by remain-on-exit: the
   remain-on-exit-format option expanded as a format string and wrapped in reverse
   video.  Falls back to +remain-on-exit-message+ on any error or an empty result.
   Expanded against a NIL context (literal text and global-scoped formats resolve;
   a pane-thread context is intentionally not built here)."
  (let* ((fmt  (ignore-errors
                 (cl-tmux/options:get-option-for-context "remain-on-exit-format"
                                                         :pane pane)))
         (text (and fmt (plusp (length fmt))
                    (ignore-errors (cl-tmux/format:expand-format fmt nil)))))
    (if (and text (plusp (length text)))
        (format nil "~C[7m~A~C[m" #\Escape text #\Escape)
        +remain-on-exit-message+)))

(defun %write-remain-on-exit-banner (pane)
  "Write the remain-on-exit banner bytes to PANE's screen.
   This is a side-effectful helper extracted from reader-eof-state so the CPS
   state function itself remains pure (only returns the next state)."
  (let ((screen (pane-screen pane)))
    (when screen
      (let ((banner-bytes (babel:string-to-octets (%remain-on-exit-banner pane)
                                                  :encoding :utf-8)))
        (cl-tmux/terminal/emulator:screen-process-bytes screen banner-bytes)))))

(defun reader-idle-state (pane)
  "Poll the pane PTY fd; transition to reading if data is available."
  (if (select-fds (list (pane-fd pane)) +pty-poll-timeout-us+)
      #'reader-reading-state
      #'reader-idle-state))

;;; -- Alert-action dispatch table -------------------------------------------
;;;
;;; Maps (action current-p) to a fire decision.
;;; none → never, current → only current window, any → always,
;;; other (default) → only non-current windows.

(defmacro define-alert-action-rules (&rest rules)
  "Define %alert-action-fires-p as a cond dispatch over ACTION/CURRENT-P.
   Each RULE is (action-string result-form) where RESULT-FORM may reference
   the CURRENT-P variable.  A final (t ...) fallback arm handles the 'other'
   default."
  (let ((action-sym  (gensym "ACTION"))
        (current-sym (gensym "CURRENT-P")))
    `(defun %alert-action-fires-p (,action-sym ,current-sym)
       "Whether an activity/silence alert should fire given the ACTION
   (none/current/other/any) and whether the window is the CURRENT (viewed) one:
     none    → never;          current → only the current window;
     any     → always;         other (default) → only non-current windows."
       (cond
         ,@(mapcar (lambda (rule)
                     (destructuring-bind (action-str result) rule
                       `((string-equal ,action-sym ,action-str)
                         ,(subst current-sym 'current-p result))))
                   rules)))))

(define-alert-action-rules
  ("none"    nil)
  ("current" current-p)
  ("any"     t)
  ;; "other" (default): fires only for non-current windows.
  ("other"   (not current-p)))

(defun %window-is-current-p (win)
  "True when WIN is the active (currently-viewed) window of any registered session.
   Used to honour activity-action/silence-action's current-vs-other distinction."
  (and win
       (some (lambda (entry)
               (eq win (cl-tmux/model:session-active-window (cdr entry))))
             *server-sessions*)))

(defun %mark-window-activity (win)
  "Mark WIN as having activity for monitor-activity: set the activity flag, fire
   the alert-activity hook, and show a visual-activity overlay when that option is
   on.  No-op when WIN is NIL, monitor-activity is off for WIN, the flag is already
   set, or activity-action says not to alert this window (none/current/other/any).
   Extracted from reader-reading-state so the alert-activity firing is
   unit-testable without a live PTY."
  (when (and win
             (cl-tmux/options:get-option-for-context "monitor-activity" :window win)
             (not (cl-tmux/model:window-activity-flag win))
             (%alert-action-fires-p
              (or (cl-tmux/options:get-option "activity-action") "other")
              (%window-is-current-p win)))
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

(defun %update-window-on-pane-output (win)
  "Update window-level state when new bytes arrive on a pane's PTY.
   Stamps last-output-time, clears the silence flag (new output resets the
   silence timer), and fires the activity alert logic.
   Extracted from reader-reading-state to keep the CPS state function focused
   on I/O dispatch."
  (when win
    ;; Always update last-output-time (used by monitor-silence timer).
    (setf (cl-tmux/model:window-last-output-time win) (get-universal-time))
    ;; Clear silence flag: new output resets the silence state.
    (setf (cl-tmux/model:window-silence-flag win) nil)
    ;; Activity flag + alert-activity hook + visual overlay.
    (%mark-window-activity win)))

(defun reader-reading-state (pane)
  "Read one PTY chunk and feed it to PANE; transition to eof if EOF."
  (let ((bytes (pty-read-blocking (pane-fd pane) +pty-buf-size+)))
    (if (null bytes)
        #'reader-eof-state
        (progn
          (when (pane-pipe-fd pane)
            (pipe-pane-write pane bytes))
          (pane-feed pane bytes)
          (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-pane-output+ pane bytes)
          (%update-window-on-pane-output (cl-tmux/model:pane-window pane))
          (setf *dirty* t)
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
    (ignore-errors (pty-close (pane-fd pane) (pane-pid pane)))
    (setf (pane-fd pane) -1
          (pane-pid pane) -1))
  (let ((remain-on-exit
          (handler-case (cl-tmux/options:get-option-for-context "remain-on-exit" :pane pane)
            (error () nil))))
    (when remain-on-exit
      ;; Write the remain-on-exit-format banner (reverse-video) to the pane screen.
      (%write-remain-on-exit-banner pane)
      (setf *dirty* t)
      ;; Return the parking state: the driver loop calls it on each tick.
      #'reader-remain-on-exit-state)))

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
      (%join-thread-with-timeout thread +reader-thread-join-timeout+))))
