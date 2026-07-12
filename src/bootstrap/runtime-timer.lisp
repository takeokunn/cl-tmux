(in-package #:cl-tmux)

;;;; Status interval timer: poll loop, lock-after-time, monitor-silence,
;;;;  overlay auto-dismiss, and start-status-timer.

;;; -- Status interval timer --------------------------------------------------

(defconstant +status-timer-poll-seconds+ 0.1
  "Granularity (seconds) at which the status timer re-checks *running* and the
   elapsed interval.  Kept small so the thread shuts down promptly when *running*
   clears instead of holding a multi-second sleep that would outlive the
   join-thread timeout and leak the thread (which, fatally, makes every
   subsequent tests or runtime state).")

(defun %maybe-auto-dismiss-overlay ()
  "Auto-dismiss the active overlay when display-time milliseconds have elapsed.
   display-time is in ms (default +default-display-time-ms+); the timer
   resolution is +status-timer-poll-seconds+ so actual dismiss may lag up to
   that amount.  Only affects transient overlays shown without :no-timer;
   long-lived paged overlays (list-keys, list-sessions) use :no-timer."
  (when (cl-tmux/prompt:overlay-active-p)
    (let* ((display-time-ms (or (cl-tmux/options:get-option "display-time")
                                +default-display-time-ms+))
           (display-secs    (/ display-time-ms +ms-per-second+))
           (shown-at        (cl-tmux/prompt:overlay-shown-at))
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

(defun %fire-silence-alert (win dirty-fn)
  "Set the silence flag, fire the alert-silence hook, and show a visual overlay
   for WIN.  Calls DIRTY-FN to request a repaint.
   Extracted from %check-monitor-silence to reduce nesting and separate the
   traversal logic (in the caller) from the per-window action."
  (setf (cl-tmux/model:window-silence-flag win) t)
  ;; Fire the alert-silence hook (matches real tmux).
  (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-alert-silence+ win)
  ;; visual-silence on/both: show a transient overlay naming the quiet window
  ;; (mirrors visual-activity in %mark-window-activity).
  (when (%visual-alert-message-p "visual-silence")
    (%show-window-alert-overlay "Silence" win))
  (funcall dirty-fn))

(defun %window-monitor-silence-due-p (win session now silence-secs silence-action)
  "True when WIN has been quiet long enough to trigger monitor-silence."
  (let ((last-output-time (cl-tmux/model:window-last-output-time win)))
    (and (> last-output-time 0)
         (not (cl-tmux/model:window-silence-flag win))
         (>= (- now last-output-time) silence-secs)
         ;; silence-action gates which windows alert.
         (%alert-action-fires-p silence-action
                                (eq win (cl-tmux/model:session-active-window session))))))

(defun %check-monitor-silence-session (session now silence-secs silence-action dirty-fn)
  "Scan SESSION's windows and fire alerts for any silence matches."
  (dolist (win (cl-tmux/model:session-windows session))
    (when (%window-monitor-silence-due-p win session now silence-secs silence-action)
      (%fire-silence-alert win dirty-fn))))

(defun %check-monitor-silence (sessions dirty-fn)
  "For each window in SESSIONS with monitor-silence enabled, set
   window-silence-flag when no PTY output has arrived for monitor-silence seconds.
   monitor-silence = 0 (default) disables per-window silence monitoring."
  (let ((silence-secs (cl-tmux/options:get-option "monitor-silence")))
    (when (and (integerp silence-secs) (> silence-secs 0))
      (let ((now (get-universal-time))
            (silence-action (or (cl-tmux/options:get-option "silence-action") "other")))
        (dolist (entry sessions)
          (%check-monitor-silence-session (cdr entry) now silence-secs silence-action
                                          dirty-fn))))))

(defun %timer-tick-overlay (dirty-fn)
  "Check if the active overlay has expired; dismiss it and call DIRTY-FN if so."
  (when (%maybe-auto-dismiss-overlay) (funcall dirty-fn)))

(defun %timer-tick-status-interval (elapsed dirty-fn)
  "Refresh the status bar when ELAPSED seconds have met or exceeded status-interval.
   Returns the new elapsed value (reset to 0 if interval fired, else unchanged).
   status-interval 0 disables the periodic redraw (tmux semantics)."
  (let ((interval (cl-tmux/options:get-option "status-interval")))
    (if (and *running* (integerp interval) (> interval 0)
             (>= elapsed interval))
        (progn (funcall dirty-fn) 0)
        elapsed)))

(defmacro %with-running-timer-check (condition &body body)
  "Run BODY when *running* and CONDITION are true, ignoring failures."
  `(when (and *running* ,condition)
     (ignore-errors ,@body)))

(defun %timer-tick-lock (session dirty-fn)
  "Check lock-after-time inactivity for SESSION; no-op when SESSION is NIL."
  (when session
    (%with-running-timer-check session
      (%check-lock-after-time session dirty-fn))))

(defun %timer-tick-silence (server-sessions-fn dirty-fn)
  "Check monitor-silence thresholds; no-op when SERVER-SESSIONS-FN is NIL."
  (when server-sessions-fn
    (%with-running-timer-check server-sessions-fn
      (%check-monitor-silence (funcall server-sessions-fn) dirty-fn))))

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
                (%timer-tick-overlay dirty-fn)
                (setf elapsed (%timer-tick-status-interval elapsed dirty-fn))
                (%timer-tick-lock session dirty-fn)
                (%timer-tick-silence server-sessions-fn dirty-fn))))
   :name "cl-tmux-status-timer"))
