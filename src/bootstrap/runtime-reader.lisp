(in-package #:cl-tmux)

;;;; PTY reader thread — CPS state machine.
;;;;
;;;; This file contains the per-pane I/O thread and the alert-action dispatch
;;;; table.  It is loaded after runtime.lisp (shared state, channel sync,
;;;; prompt history) and before runtime-timer.lisp.
;;;;
;;;; Threading model recap:
;;;;   * One reader thread per pane: blocking read(PTY fd) -> pane-feed ->
;;;;     screen update -> sets *dirty* T.
;;;;   * Main thread: select(stdin, 50 ms) -> key dispatch -> render when dirty.

;;; -- PTY reader thread -------------------------------------------------------
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

(defun %pane-death-context (pane)
  "A minimal format context carrying PANE's death record, so
   remain-on-exit-format can reference #{pane_dead_status} /
   #{pane_dead_signal} / #{pane_dead_time} (a full session context is
   intentionally not built on the reader thread)."
  (flet ((num-or-empty (v) (if v (format nil "~D" v) "")))
    (list :pane-dead        "1"
          :pane-dead-status (num-or-empty (cl-tmux/model:pane-dead-status pane))
          :pane-dead-signal (num-or-empty (cl-tmux/model:pane-dead-signal pane))
          :pane-dead-time   (num-or-empty (cl-tmux/model:pane-dead-time pane)))))

(defun %remain-on-exit-banner (pane)
  "The reverse-video banner for a pane kept open by remain-on-exit: the
   remain-on-exit-format option expanded as a format string and wrapped in reverse
   video.  Falls back to +remain-on-exit-message+ on any error or an empty result.
   Expanded against the pane's death-record context so the tmux default's
   #{pane_dead_status}/#{pane_dead_signal}/#{pane_dead_time} references resolve."
  (let* ((fmt  (ignore-errors
                 (cl-tmux/options:get-option-for-context "remain-on-exit-format"
                                                         :pane pane)))
         (text (and fmt (plusp (length fmt))
                    (ignore-errors (cl-tmux/format:expand-format
                                    fmt (%pane-death-context pane))))))
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

(defun %visual-alert-message-p (option-name)
  "True when OPTION-NAME (visual-bell / visual-activity / visual-silence — tmux
   off/on/both enums) requests the transient message overlay: on or both."
  (let ((value (cl-tmux/options:get-option option-name)))
    (and (stringp value)
         (member value '("on" "both") :test #'string-equal)
         t)))

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
    ;; visual-activity on/both: show a transient message overlay so the user
    ;; knows which background window has activity (matches real tmux).
    (when (%visual-alert-message-p "visual-activity")
      (show-transient-overlay
       (format nil "Activity in window ~A (~A)"
               (cl-tmux/model:window-id win)
               (cl-tmux/model:window-name win))))))

(defun %mark-window-bell (win pane)
  "Bell alert logic for a BEL left pending on PANE's screen (tmux alerts.c):
   entirely gated on monitor-bell.  Sets WIN's sticky bell flag (the status `!',
   cleared when the window is selected), fires the alert-bell hook, and shows the
   visual-bell message overlay (visual-bell on/both).  Like tmux's WINLINK_BELL,
   the alert only applies to a window that is not currently viewed; the audible
   relay for the viewed window is handled by the renderer.  The flag-transition
   guard keeps the hook/overlay firing once per alert, not once per PTY chunk."
  (let ((screen (and pane (cl-tmux/model:pane-screen pane))))
    (when (and win screen
               (cl-tmux/terminal:screen-bell-pending screen)
               (cl-tmux/options:get-option-for-context "monitor-bell" :window win)
               (not (cl-tmux/model:window-bell-flag win))
               (not (%window-is-current-p win)))
      (setf (cl-tmux/model:window-bell-flag win) t)
      ;; bell-action gates the alert itself (hook + visual message) for this
      ;; non-current window: any/other fire, none/current do not.
      (when (%alert-action-fires-p
             (or (cl-tmux/options:get-option "bell-action") "any")
             nil)
        (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-alert-bell+ win)
        (when (%visual-alert-message-p "visual-bell")
          (show-transient-overlay
           (format nil "Bell in window ~A (~A)"
                   (cl-tmux/model:window-id win)
                   (cl-tmux/model:window-name win))))))))

(defun %update-window-on-pane-output (win pane)
  "Update window-level state when new bytes arrive on PANE's PTY.
   Stamps last-output-time, clears the silence flag (new output resets the
   silence timer), and fires the activity and bell alert logic.
   Extracted from reader-reading-state to keep the CPS state function focused
   on I/O dispatch."
  (when win
    ;; Always update last-output-time (used by monitor-silence timer).
    (setf (cl-tmux/model:window-last-output-time win) (get-universal-time))
    ;; Clear silence flag: new output resets the silence state.
    (setf (cl-tmux/model:window-silence-flag win) nil)
    ;; Activity flag + alert-activity hook + visual overlay.
    (%mark-window-activity win)
    ;; Sticky bell flag + alert-bell hook + visual-bell overlay.
    (%mark-window-bell win pane)))

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
          (%update-window-on-pane-output (cl-tmux/model:pane-window pane) pane)
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
    (ignore-errors (pty-close (pane-fd pane) (pane-pid pane)))
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
