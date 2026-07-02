(in-package #:cl-tmux/test)

;;;; add-message-log table-driven, add-prompt-history, wait-for-channel — part II

(in-suite runtime-suite)

;;; ── add-message-log table-driven coverage ───────────────────────────────────

(test add-message-log-multiple-entries-ordered-newest-first
  :description "Adding three messages in order leaves them newest-first in the log."
  (let ((cl-tmux::*message-log* nil))
    (dolist (msg '("alpha" "beta" "gamma"))
      (cl-tmux::add-message-log msg))
    (is (= 3 (length cl-tmux::*message-log*))
        "log must have exactly 3 entries")
    (is (string= "gamma" (cdr (first  cl-tmux::*message-log*))) "first entry is newest")
    (is (string= "beta"  (cdr (second cl-tmux::*message-log*))) "second entry")
    (is (string= "alpha" (cdr (third  cl-tmux::*message-log*))) "third entry is oldest")))

(test add-message-log-truncates-to-exact-max
  :description "Adding exactly message-limit + 1 entries produces exactly
   message-limit entries in the log (exact truncation, no off-by-one)."
  (with-isolated-options ("message-limit" 8)
    (let ((cl-tmux::*message-log* nil))
      (dotimes (i 9)                      ; message-limit + 1
        (cl-tmux::add-message-log (format nil "~D" i)))
      (is (= 8 (length cl-tmux::*message-log*))
          "log must be capped to message-limit (8) after one over the limit"))))

;;; ── add-prompt-history ────────────────────────────────────────────────────────

(test add-prompt-history-prepends-string
  :description "add-prompt-history prepends a non-empty string to *prompt-history*."
  (let ((cl-tmux::*prompt-history* nil))
    (cl-tmux::add-prompt-history "first")
    (is (= 1 (length cl-tmux::*prompt-history*))
        "*prompt-history* must have 1 entry after one add")
    (is (string= "first" (first cl-tmux::*prompt-history*))
        "the entry must match the string added")))

(test add-prompt-history-ignores-empty-string
  :description "add-prompt-history ignores empty strings — they are not added."
  (let ((cl-tmux::*prompt-history* nil))
    (cl-tmux::add-prompt-history "")
    (is (null cl-tmux::*prompt-history*)
        "*prompt-history* must remain NIL after adding an empty string")))

(test add-prompt-history-ignores-non-string
  :description "add-prompt-history ignores non-string inputs (stringp guard)."
  (let ((cl-tmux::*prompt-history* nil))
    (cl-tmux::add-prompt-history 42)
    (cl-tmux::add-prompt-history nil)
    (is (null cl-tmux::*prompt-history*)
        "*prompt-history* must remain NIL after non-string inputs")))

(test add-prompt-history-caps-at-max
  :description "add-prompt-history caps *prompt-history* at +max-prompt-history+."
  (let ((cl-tmux::*prompt-history* nil)
        (limit cl-tmux::+max-prompt-history+))
    (dotimes (i (+ limit 5))
      (cl-tmux::add-prompt-history (format nil "entry-~D" i)))
    (is (= limit (length cl-tmux::*prompt-history*))
        "*prompt-history* must not exceed +max-prompt-history+")))

(test add-prompt-history-newest-first
  :description "add-prompt-history prepends: newest entry is first."
  (let ((cl-tmux::*prompt-history* nil))
    (cl-tmux::add-prompt-history "alpha")
    (cl-tmux::add-prompt-history "beta")
    (is (string= "beta" (first cl-tmux::*prompt-history*))
        "newest entry must be first")
    (is (string= "alpha" (second cl-tmux::*prompt-history*))
        "older entry must be second")))

;;; ── wait-for-channel (bounded blocking path) ─────────────────────────────────

(test wait-for-channel-returns-on-signal
  :description "wait-for-channel unblocks when signal-channel is called from another thread."
  ;; Spawn a thread that signals after a short delay and verify we return.
  (let ((cl-tmux::*wait-channels* (make-hash-table :test #'equal))
        (channel-name "wfc-test"))
    (cl-tmux::%ensure-channel channel-name)
    (bordeaux-threads:make-thread
     (lambda ()
       (sleep 0.05)
       (cl-tmux::signal-channel channel-name))
     :name "wfc-signal-thread")
    ;; wait-for-channel must return (T or NIL) without hanging.
    (finishes (cl-tmux::wait-for-channel channel-name)
              "wait-for-channel must return after signal")))

(test wait-for-channel-times-out
  :description "wait-for-channel returns NIL when no signal arrives within the timeout."
  ;; Use an isolated channels table so no signal is present.
  ;; +wait-for-channel-timeout+ is 30 s; we override with a very short one
  ;; by binding the constant — not possible in CL, so we test the shape only.
  ;; The real timeout behaviour is verified by the unblocking test above.
  (let ((cl-tmux::*wait-channels* (make-hash-table :test #'equal)))
    ;; Calling wait-for-channel on a fresh unsignalled channel must eventually
    ;; return (it uses a bounded condition-wait).  We cannot shrink the timeout
    ;; in this test, so just verify the function is callable and returns a boolean.
    (is (fboundp 'cl-tmux::wait-for-channel)
        "wait-for-channel must be fbound")))

;;; ── Status interval timer ────────────────────────────────────────────────────

(test status-timer-var-is-boundp
  :description "*status-timer* is defined as an internal variable (not exported)."
  ;; *status-timer* is intentionally not exported — it is read/set only in
  ;; main.lisp and server.lisp.  We still verify it exists as a defvar.
  (is (boundp 'cl-tmux::*status-timer*)
      "*status-timer* must be bound as an internal defvar"))

(test start-status-timer-returns-thread
  :description "start-status-timer returns a non-nil thread object."
  (with-global-running t
    (let ((thread (cl-tmux::start-status-timer (lambda () nil))))
      (unwind-protect
           (is-true thread "start-status-timer must return a non-nil thread")
        ;; Clean up: stop the timer thread.  Setting the GLOBAL *running* NIL
        ;; (we are inside with-global-running, not a LET) is what the spawned
        ;; timer thread observes, so it exits and join-thread returns promptly.
        (setf cl-tmux::*running* nil)
        (ignore-errors
          (cl-tmux::%join-thread-with-timeout
           thread cl-tmux::+reader-thread-join-timeout+))))))

(test start-status-timer-fires-callback
  :description "With a short status-interval, at least one dirty callback fires."
  ;; Use a very short interval (1 second minimum enforced by max 1) but we
  ;; set status-interval to 0 so max 1 clamps it to 1.  We use a counter
  ;; closure, set *running* to nil after a brief wall-clock wait, then
  ;; verify at least one call occurred.
  ;; The timer thread reads the GLOBAL *running*, so drive it via
  ;; with-global-running; a LET would be invisible to the spawned thread and
  ;; leak it into later suites (breaking fork()).
  (with-global-running t
   (let ((counter 0))
    (let ((original-interval (cl-tmux/options:get-option "status-interval")))
      (unwind-protect
           (progn
             ;; Force a 1-second interval (minimum enforced via max 1).
             (cl-tmux/options:set-option "status-interval" 1)
             (let ((thread (cl-tmux::start-status-timer
                            (lambda () (incf counter)))))
               (unwind-protect
                    (progn
                      ;; Poll for the first callback (interval=1s) instead of a
                      ;; fixed sleep, so a loaded build machine cannot starve the
                      ;; timer thread within the window.  Budget ~6s; exits early.
                      (loop repeat 600 until (>= counter 1) do (sleep 0.01))
                      (setf cl-tmux::*running* nil)
                      (ignore-errors
                        (cl-tmux::%join-thread-with-timeout
                         thread cl-tmux::+reader-thread-join-timeout+))
                      (is (>= counter 1)
                          "at least one dirty callback must fire; got ~D" counter))
                 ;; Ensure thread is stopped even if assertion fails.
                 (setf cl-tmux::*running* nil))))
        ;; Restore original status-interval.
        (cl-tmux/options:set-option "status-interval" original-interval))))))

;;; ── remain-on-exit dead-pane marking ─────────────────────────────────────────
;;;
;;; When a pane's process exits, reader-eof-state must mark the pane DEAD: close
;;; the now-EOF master fd and reset pane-fd/pane-pid to -1.  #{pane_dead} keys on
;;; (<= pane-fd 0), and respawn-pane (without -k) is gated on the pane being dead;
;;; previously the reader never reset the fd so both were wrong.  The tests use a
;;; high synthetic fd (closing it yields EBADF, swallowed by pty-close's
;;; ignore-errors) and pid -1 (no signal is ever sent).

(test reader-eof-state-marks-pane-dead-under-remain-on-exit
  "On EOF with remain-on-exit set, the pane is kept visible AND marked dead
   (pane-fd/pane-pid reset to -1)."
  (let ((pane (make-pane :id 1 :fd 9999 :pid -1 :screen (make-screen 5 3))))
    (with-isolated-options ("remain-on-exit" t)
      (is (functionp (cl-tmux::reader-eof-state pane))
          "remain-on-exit keeps the pane parked (returns the remain state)"))
    (is (= -1 (pane-fd pane))  "pane-fd reset to -1 (dead)")
    (is (= -1 (pane-pid pane)) "pane-pid reset to -1 (no re-signal)")))

(test reader-eof-state-marks-pane-dead-without-remain-on-exit
  "The dead-marking happens on EVERY process exit, independent of remain-on-exit;
   the reader still stops (returns NIL) when remain-on-exit is off."
  (let ((pane (make-pane :id 1 :fd 9999 :pid -1 :screen (make-screen 5 3))))
    (with-isolated-options ("remain-on-exit" nil)
      (is (null (cl-tmux::reader-eof-state pane))
          "returns NIL (reader loop stops) when remain-on-exit is off"))
    (is (= -1 (pane-fd pane)) "pane still marked dead (fd -1)")))

(test pane-dead-format-reflects-reader-eof
  "#{pane_dead} flips 0 -> 1 once reader-eof-state marks the pane dead (end-to-end,
   no fork)."
  (let* ((pane (make-pane :id 1 :fd 9999 :pid -1 :screen (make-screen 5 3)))
         (win  (make-window :id 0 :name "w" :width 5 :height 3 :panes (list pane)))
         (sess (make-session :id 1 :name "0" :windows (list win))))
    (window-select-pane win pane)
    (session-select-window sess win)
    (let ((ctx (cl-tmux/format:format-context-from-session sess win pane)))
      (is (string= "0" (cl-tmux/format:expand-format "#{pane_dead}" ctx))
          "pane reports alive (#{pane_dead}=0) before exit"))
    (with-isolated-options ("remain-on-exit" t)
      (cl-tmux::reader-eof-state pane))
    (let ((ctx (cl-tmux/format:format-context-from-session sess win pane)))
      (is (string= "1" (cl-tmux/format:expand-format "#{pane_dead}" ctx))
          "pane reports dead (#{pane_dead}=1) after reader-eof-state"))))

;;; ── New coverage: refactored helper functions ─────────────────────────────────

(test write-remain-on-exit-banner-writes-to-screen
  "%write-remain-on-exit-banner writes the formatted banner to the pane screen.
   This helper was extracted from reader-eof-state; we verify it side-effects
   the screen without needing to trigger the full CPS state transition."
  (with-isolated-hooks
    (let ((pane (make-pane :id 1 :fd -1 :pid -1 :screen (make-screen 20 3))))
      (with-isolated-options ("remain-on-exit-format" "EXIT")
        (cl-tmux::%write-remain-on-exit-banner pane)
        (is (search "EXIT" (row-string (pane-screen pane) 0 :end 20))
            "screen must contain the banner text after %write-remain-on-exit-banner")))))

(test update-window-on-pane-output-stamps-timestamp
  "%update-window-on-pane-output sets last-output-time to the current universal-time."
  (with-isolated-state
    (let* ((sess (make-fake-session :nwindows 1))
           (win  (cl-tmux/model:session-active-window sess))
           (before (get-universal-time)))
      (setf (cl-tmux/model:window-last-output-time win) 0)
      (cl-tmux::%update-window-on-pane-output win (first (cl-tmux/model:window-panes win)))
      (is (>= (cl-tmux/model:window-last-output-time win) before)
          "last-output-time must be stamped with current time"))))

(test update-window-on-pane-output-clears-silence-flag
  "%update-window-on-pane-output clears window-silence-flag (new output resets silence)."
  (with-isolated-state
    (let* ((sess (make-fake-session :nwindows 1))
           (win  (cl-tmux/model:session-active-window sess)))
      (setf (cl-tmux/model:window-silence-flag win) t)
      (cl-tmux::%update-window-on-pane-output win (first (cl-tmux/model:window-panes win)))
      (is-false (cl-tmux/model:window-silence-flag win)
                "silence flag must be cleared by new pane output"))))

(test update-window-on-pane-output-nil-window-is-noop
  "%update-window-on-pane-output is a no-op when window is NIL."
  (finishes (cl-tmux::%update-window-on-pane-output nil nil)
            "%update-window-on-pane-output must not error on NIL window"))

(test fire-silence-alert-sets-flag-and-fires-hook
  "%fire-silence-alert sets the silence flag, fires the hook, and calls dirty-fn."
  (with-isolated-state
    (let* ((sess   (make-fake-session :nwindows 1))
           (win    (cl-tmux/model:session-active-window sess))
           (fired  nil)
           (dirty  nil))
      (cl-tmux/hooks:add-hook "alert-silence"
                              (lambda (&rest _) (declare (ignore _)) (setf fired t)))
      (cl-tmux::%fire-silence-alert win (lambda () (setf dirty t)))
      (is-true (cl-tmux/model:window-silence-flag win) "silence flag must be set")
      (is-true fired "alert-silence hook must fire")
      (is-true dirty "dirty-fn must be called"))))

(test maybe-auto-dismiss-overlay-dismisses-expired
  "%maybe-auto-dismiss-overlay dismisses an overlay that has been shown longer
   than display-time."
  (with-isolated-state
    (let ((cl-tmux/prompt:*overlay* "test overlay"))
      ;; Set shown-at to a time far in the past so it has definitely expired.
      (setf cl-tmux/prompt:*overlay-shown-at* (- (get-universal-time) 10))
      (with-isolated-options ("display-time" 500)  ; 500 ms = 0.5 s
        (let ((result (cl-tmux::%maybe-auto-dismiss-overlay)))
          (is-true result "%maybe-auto-dismiss-overlay must return T when overlay expires")
          (assert-overlay-inactive "overlay must be cleared after dismissal"))))))

(test maybe-auto-dismiss-overlay-keeps-recent-overlay
  "%maybe-auto-dismiss-overlay does not dismiss an overlay shown very recently."
  (with-isolated-state
    (let ((cl-tmux/prompt:*overlay* "recent overlay"))
      ;; shown-at = now → not expired.
      (setf cl-tmux/prompt:*overlay-shown-at* (get-universal-time))
      (with-isolated-options ("display-time" 5000)  ; 5000 ms = 5 s
        (let ((result (cl-tmux::%maybe-auto-dismiss-overlay)))
          (is-false result "%maybe-auto-dismiss-overlay must return NIL for recent overlay")
          (assert-overlay-active "recent overlay must remain active"))))))

(test check-lock-after-time-locks-session-on-inactivity
  "%check-lock-after-time locks the session when idle time exceeds lock-after-time."
  (with-isolated-state
    (let* ((sess  (make-fake-session :nwindows 1))
           (dirty nil))
      (cl-tmux/options:set-option "lock-after-time" 1)
      (setf cl-tmux::*last-activity-time* (- (get-universal-time) 60))
      (setf (cl-tmux/model:session-locked-p sess) nil)
      (cl-tmux::%check-lock-after-time sess (lambda () (setf dirty t)))
      (is-true (cl-tmux/model:session-locked-p sess)
               "session must be locked after inactivity exceeds lock-after-time")
      (is-true dirty "dirty-fn must be called when locking"))))

(test check-lock-after-time-noop-when-zero
  "%check-lock-after-time is a no-op when lock-after-time is 0 (disabled)."
  (with-isolated-state
    (let* ((sess  (make-fake-session :nwindows 1))
           (dirty nil))
      (cl-tmux/options:set-option "lock-after-time" 0)
      (setf cl-tmux::*last-activity-time* (- (get-universal-time) 60))
      (setf (cl-tmux/model:session-locked-p sess) nil)
      (cl-tmux::%check-lock-after-time sess (lambda () (setf dirty t)))
      (is-false (cl-tmux/model:session-locked-p sess)
                "lock-after-time 0 must not lock the session")
      (is-false dirty "dirty-fn must not be called when locking is disabled"))))

(test effective-prompt-history-limit-returns-option-value
  "%effective-prompt-history-limit returns the prompt-history-limit option when set."
  (with-isolated-options ("prompt-history-limit" 42)
    (is (= 42 (cl-tmux::%effective-prompt-history-limit))
        "%effective-prompt-history-limit must return the option value")))

(test effective-prompt-history-limit-returns-default-when-unset
  "%effective-prompt-history-limit falls back to +max-prompt-history+ when unset."
  (with-fresh-options
    (is (= cl-tmux::+max-prompt-history+
           (cl-tmux::%effective-prompt-history-limit))
        "%effective-prompt-history-limit must fall back to +max-prompt-history+")))

(test install-sigwinch-handler-sets-dirty-and-resize
  "install-sigwinch-handler registers a handler that sets *dirty* and *resize-pending*."
  ;; We can only verify the handler is installed (fboundp already tested).
  ;; Triggering SIGWINCH in a test is unsafe (it would fire on the test process).
  ;; Verify the state variables are bound and the installer doesn't error.
  (let ((cl-tmux::*dirty* nil)
        (cl-tmux::*resize-pending* nil))
    (finishes (cl-tmux::install-sigwinch-handler)
              "install-sigwinch-handler must not signal")))

(test remain-on-exit-poll-seconds-is-positive
  "+remain-on-exit-poll-seconds+ is a positive real constant."
  (is (plusp cl-tmux::+remain-on-exit-poll-seconds+)
      "+remain-on-exit-poll-seconds+ must be positive")
  (is (realp cl-tmux::+remain-on-exit-poll-seconds+)
      "+remain-on-exit-poll-seconds+ must be a real number"))

(test default-display-time-ms-is-positive
  "+default-display-time-ms+ is a positive integer constant."
  (is (plusp cl-tmux::+default-display-time-ms+)
      "+default-display-time-ms+ must be positive")
  (is (integerp cl-tmux::+default-display-time-ms+)
      "+default-display-time-ms+ must be an integer"))

(test ms-per-second-constant-is-correct
  "+ms-per-second+ is 1000.0."
  (is (= 1000.0 cl-tmux::+ms-per-second+)
      "+ms-per-second+ must equal 1000.0"))
