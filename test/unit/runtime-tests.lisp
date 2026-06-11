(in-package #:cl-tmux/test)

;;;; global variables, reader-thread CPS, stop-reader-threads, wait-for-channel, status timer, remain-on-exit — part I

(def-suite runtime-suite :description "Runtime state variables and threading utilities")
(in-suite runtime-suite)

;;; ── Test fixture macros ──────────────────────────────────────────────────────

(defmacro with-dead-pane ((pane-var) &body body)
  "Bind PANE-VAR to a standard dead pane (fd=-1, pid=-1, 5×3 screen) for BODY.
   Eliminates the repeated (make-pane :id 1 :fd -1 :pid -1 :screen (make-screen 5 3))
   boilerplate."
  `(let ((,pane-var (make-pane :id 1 :fd -1 :pid -1 :screen (make-screen 5 3))))
     ,@body))

(defmacro with-isolated-state (&body body)
  "Run BODY with both config and hooks isolated (combined with-isolated-config +
   with-isolated-hooks).  Eliminates the double nesting in tests that touch both
   option reads and hook firing."
  `(with-isolated-config
     (with-isolated-hooks
       ,@body)))

;;; ── Global variables exist and have sensible types ───────────────────────────

(test runtime-globals-exist
  :description "*running*, *dirty*, *resize-pending*, *term-rows*, *term-cols* are all boundp."
  (is (boundp 'cl-tmux::*running*)        "*running* must be bound")
  (is (boundp 'cl-tmux::*dirty*)          "*dirty* must be bound")
  (is (boundp 'cl-tmux::*resize-pending*) "*resize-pending* must be bound")
  (is (integerp cl-tmux::*term-rows*)     "*term-rows* must be an integer")
  (is (integerp cl-tmux::*term-cols*)     "*term-cols* must be an integer"))

(test runtime-term-rows-positive
  :description "*term-rows* default is a positive integer (at least 1 row)."
  (is (plusp cl-tmux::*term-rows*) "*term-rows* must be a positive integer, got ~D"
      cl-tmux::*term-rows*))

(test runtime-term-cols-positive
  :description "*term-cols* default is a positive integer (at least 1 column)."
  (is (plusp cl-tmux::*term-cols*) "*term-cols* must be a positive integer, got ~D"
      cl-tmux::*term-cols*))

(test runtime-max-message-log-entries-is-constant
  :description "+max-message-log-entries+ is a positive integer constant."
  (is (constantp 'cl-tmux::+max-message-log-entries+) "+max-message-log-entries+ must be a constant")
  (is (integerp cl-tmux::+max-message-log-entries+) "constant must be an integer")
  (is (plusp cl-tmux::+max-message-log-entries+) "constant must be positive"))

(test runtime-reader-thread-join-timeout-is-constant
  :description "+reader-thread-join-timeout+ is a positive integer constant."
  (is (integerp cl-tmux::+reader-thread-join-timeout+) "join timeout must be an integer")
  (is (plusp cl-tmux::+reader-thread-join-timeout+)    "join timeout must be positive"))

;;; ── %pane-reader-loop ────────────────────────────────────────────────────────

(test pane-reader-loop-is-fbound
  :description "%pane-reader-loop is a defined function (data/logic separation from start-reader-thread)."
  (is (fboundp 'cl-tmux::%pane-reader-loop)
      "%pane-reader-loop must be fbound"))

(test pane-reader-loop-exits-when-running-nil
  :description "%pane-reader-loop exits immediately when *running* is NIL without error."
  (with-dead-pane (pane)
    (let ((cl-tmux::*running* nil)
          (cl-tmux::*dirty*   nil))
      (finishes (cl-tmux::%pane-reader-loop pane)
                "%pane-reader-loop must return cleanly when *running* is NIL")
      (is-false cl-tmux::*dirty* "*dirty* must remain NIL when loop exits immediately"))))

;;; ── CPS reader states ────────────────────────────────────────────────────────

(test reader-eof-state-returns-nil-without-remain-on-exit
  :description "reader-eof-state returns NIL when remain-on-exit is not set."
  (with-dead-pane (pane)
    (with-isolated-options ("remain-on-exit" nil)
      (is (null (cl-tmux::reader-eof-state pane))
          "reader-eof-state must return NIL when remain-on-exit is not set"))))

(test reader-eof-state-returns-remain-state-when-option-set
  :description "reader-eof-state returns #'reader-remain-on-exit-state when remain-on-exit is set."
  (with-dead-pane (pane)
    (with-isolated-options ("remain-on-exit" t)
      (let ((result (cl-tmux::reader-eof-state pane)))
        (is (functionp result)
            "reader-eof-state must return a function when remain-on-exit is set")))))

(test reader-eof-state-honors-pane-local-remain-on-exit
  :description "reader-eof-state honors a PANE-LOCAL remain-on-exit override at
   runtime: with the GLOBAL remain-on-exit NIL but the pane-local value set to
   T, reader-eof-state must return the parking state #'reader-remain-on-exit-state
   (proving runtime.lisp's get-option-for-pane read honors per-pane overrides)."
  (with-isolated-state
    (let* ((sess (make-fake-session))
           (pane (cl-tmux/model:session-active-pane sess))
           (cl-tmux::*dirty* nil))
      (cl-tmux/options:set-option "remain-on-exit" nil)
      (cl-tmux/options:set-option-for-pane "remain-on-exit" "on" pane)
      (let ((result (cl-tmux::reader-eof-state pane)))
        (is (eq #'cl-tmux::reader-remain-on-exit-state result)
            "reader-eof-state must return the remain-on-exit parking state when the
             pane-local override is set, even though the global value is NIL")))))

(test remain-on-exit-banner-uses-format-option
  :description "%remain-on-exit-banner expands remain-on-exit-format and wraps it in
   reverse video; an empty format falls back to the built-in message."
  (let ((pane (make-pane :id 1 :fd -1 :pid -1 :screen (make-screen 10 3))))
    (with-isolated-options ("remain-on-exit-format" "DEAD")
      (let ((banner (cl-tmux::%remain-on-exit-banner pane)))
        (is (search "DEAD" banner) "banner must contain the format text (got ~S)" banner)
        (is (search (format nil "~C[7m" #\Escape) banner)
            "banner must be reverse-video (SGR 7)")))
    (with-isolated-options ("remain-on-exit-format" "")
      (is (string= cl-tmux::+remain-on-exit-message+
                   (cl-tmux::%remain-on-exit-banner pane))
          "empty format must fall back to the built-in message"))))

(test reader-eof-state-writes-format-banner-to-screen
  :description "reader-eof-state writes the remain-on-exit-format banner to the pane
   screen when remain-on-exit is set."
  (with-isolated-hooks
    (let ((pane (make-pane :id 1 :fd -1 :pid -1 :screen (make-screen 10 3))))
      (with-isolated-options ("remain-on-exit" t "remain-on-exit-format" "BYE")
        (cl-tmux::reader-eof-state pane)
        (is (search "BYE" (row-string (pane-screen pane) 0 :end 10))
            "the pane screen must show the custom banner text")))))

(test reader-reading-state-honors-window-local-monitor-activity
  :description "Pins the per-window resolution at the migrated reader-reading-state
   activity-flag site (src/runtime.lisp): that site reads
   (get-option-for-context \"monitor-activity\" :window win) to decide whether to
   set window-activity-flag for a non-active window.  reader-reading-state itself
   needs a live PTY fd (pty-read-blocking; fake panes have fd -1 → immediate EOF,
   not useful), so we directly assert the OBSERVABLE decision the migrated site
   makes: with global monitor-activity NIL, a window whose LOCAL value is on
   resolves T (activity tracked), while a window with no override resolves NIL
   (opted out)."
  (with-isolated-state
    ;; >=2 windows so there is a NON-ACTIVE background window (the activity-flag
    ;; path only fires for non-active windows).
    (let* ((sess        (make-fake-session :nwindows 2))
           (active-win  (cl-tmux/model:session-active-window sess))
           (bg-win      (find-if-not (lambda (w) (eq w active-win))
                                     (cl-tmux/model:session-windows sess))))
      (is (not (null bg-win)) "must have a non-active background window")
      (cl-tmux/options:set-option "monitor-activity" nil)              ; global = NIL
      ;; Window-local "on" on the background window.
      (cl-tmux/options:set-option-for-window "monitor-activity" "on" bg-win)
      (is (eq t (cl-tmux/options:get-option-for-context "monitor-activity" :window bg-win))
          "window-local on must resolve T at the migrated read site (global NIL)")
      ;; The active window has no local override → resolves to global NIL.
      (is (null (cl-tmux/options:get-option-for-context "monitor-activity" :window active-win))
          "a window without the override must resolve NIL (global NIL)"))))

(test reader-reading-state-window-local-monitor-activity-off-over-global-on
  :description "Companion falsey-honoring check at the same migrated site: with
   global monitor-activity on, a window whose LOCAL value is off (NIL) opts out —
   the per-window read returns NIL, proving the present-but-falsey window override
   is honored at the reader-reading-state activity-flag site."
  (with-isolated-state
    (let* ((sess       (make-fake-session :nwindows 2))
           (active-win (cl-tmux/model:session-active-window sess))
           (bg-win     (find-if-not (lambda (w) (eq w active-win))
                                    (cl-tmux/model:session-windows sess))))
      (cl-tmux/options:set-option "monitor-activity" t)               ; global = T
      (cl-tmux/options:set-option-for-window "monitor-activity" "off" bg-win) ; window = NIL
      (is (null (cl-tmux/options:get-option-for-context "monitor-activity" :window bg-win))
          "window-local off (NIL) must win over global on (T) at the migrated site"))))

(test mark-window-activity-fires-alert-activity-hook
  :description "%mark-window-activity sets the activity flag AND fires the
   alert-activity hook (tmux alert hook, previously never fired)."
  (with-isolated-state
    (let* ((sess  (make-fake-session :nwindows 1))
           (win   (cl-tmux/model:session-active-window sess))
           (fired nil))
      (cl-tmux/options:set-option "monitor-activity" "on")
      (setf (cl-tmux/model:window-activity-flag win) nil)
      (cl-tmux/hooks:add-hook "alert-activity"
                              (lambda (&rest _) (declare (ignore _)) (setf fired t)))
      (cl-tmux::%mark-window-activity win)
      (is-true (cl-tmux/model:window-activity-flag win) "activity flag must be set")
      (is-true fired "the alert-activity hook must fire"))))

(test monitor-silence-fires-alert-silence-hook
  :description "%check-monitor-silence fires the alert-silence hook when a window
   crosses the silence threshold (tmux alert hook, previously never fired)."
  (with-isolated-state
    (let* ((sess  (make-fake-session :nwindows 1))
           (win   (cl-tmux/model:session-active-window sess))
           (fired nil))
      (cl-tmux/options:set-option "monitor-silence" 5)
      ;; silence-action "any" so the alert fires even for the (current) window
      ;; under test (default "other" would suppress the current window).
      (cl-tmux/options:set-option "silence-action" "any")
      (setf (cl-tmux/model:window-last-output-time win) (- (get-universal-time) 100)
            (cl-tmux/model:window-silence-flag win) nil)
      (cl-tmux/hooks:add-hook "alert-silence"
                              (lambda (&rest _) (declare (ignore _)) (setf fired t)))
      (cl-tmux::%check-monitor-silence (list (cons 1 sess)) (lambda () nil))
      (is-true (cl-tmux/model:window-silence-flag win) "silence flag must be set")
      (is-true fired "the alert-silence hook must fire"))))

(test monitor-silence-default-is-zero-no-op
  :description "With the registered default monitor-silence = 0, %check-monitor-silence
   is a no-op: no window crosses a (disabled) threshold, so no flag is set."
  (with-isolated-state
    (let* ((sess (make-fake-session :nwindows 1))
           (win  (cl-tmux/model:session-active-window sess)))
      (is (eql 0 (cl-tmux/options:get-option "monitor-silence"))
          "monitor-silence must default to 0 (registered)")
      ;; Window has been silent for a long time, but monitoring is off (0).
      (setf (cl-tmux/model:window-last-output-time win) (- (get-universal-time) 100)
            (cl-tmux/model:window-silence-flag win) nil)
      (cl-tmux::%check-monitor-silence (list (cons 1 sess)) (lambda () nil))
      (is-false (cl-tmux/model:window-silence-flag win)
                "monitor-silence 0 must not set the silence flag"))))

(test monitor-silence-visual-silence-shows-overlay
  :description "When visual-silence is on, crossing the silence threshold shows a
   transient overlay naming the quiet window (mirrors visual-activity)."
  (with-isolated-state
    (let* ((sess (make-fake-session :nwindows 1))
           (win  (cl-tmux/model:session-active-window sess))
           (cl-tmux/prompt:*overlay* nil))
      (cl-tmux/options:set-option "monitor-silence" 5)
      (cl-tmux/options:set-option "silence-action" "any")  ; fire for the current window
      (cl-tmux/options:set-option "visual-silence" t)
      (setf (cl-tmux/model:window-last-output-time win) (- (get-universal-time) 100)
            (cl-tmux/model:window-silence-flag win) nil)
      (cl-tmux::%check-monitor-silence (list (cons 1 sess)) (lambda () nil))
      (is-true (cl-tmux/prompt:overlay-active-p)
               "visual-silence must show an overlay when silence is detected"))))

(test prompt-history-persists-to-history-file
  "add-prompt-history saves to history-file and load-prompt-history restores it
   (newest first)."
  (with-fresh-options
    (let ((path (format nil "~A/cl-tmux-hist-~D.txt"
                        (string-right-trim "/" (or (sb-ext:posix-getenv "TMPDIR") "/tmp"))
                        (get-universal-time))))
      (unwind-protect
           (progn
             (let ((cl-tmux::*prompt-history* nil))
               (cl-tmux/options:set-option "history-file" path)
               (cl-tmux::add-prompt-history "first")
               (cl-tmux::add-prompt-history "second"))
             ;; Fresh in-memory history; loading from the file restores both.
             (let ((cl-tmux::*prompt-history* nil))
               (cl-tmux::load-prompt-history)
               (is (equal '("second" "first") cl-tmux::*prompt-history*)
                   "loaded history must be newest-first")))
        (ignore-errors (delete-file path))))))

(test prompt-history-no-file-is-in-memory-only
  "With history-file unset (default \"\"), history stays in memory and add does not error."
  (with-fresh-options
    (let ((cl-tmux::*prompt-history* nil))
      (is (null (cl-tmux::%prompt-history-path))
          "no history path when history-file is empty")
      (cl-tmux::add-prompt-history "x")
      (is (equal '("x") cl-tmux::*prompt-history*)
          "in-memory history still works without a file"))))

(test alert-action-fires-p-policy-matrix
  "%alert-action-fires-p maps an activity/silence action × current-ness to a fire
   decision: none→never, current→only current, any→always, other→only non-current."
  (is-false (cl-tmux::%alert-action-fires-p "none" t))
  (is-false (cl-tmux::%alert-action-fires-p "none" nil))
  (is-true  (cl-tmux::%alert-action-fires-p "current" t))
  (is-false (cl-tmux::%alert-action-fires-p "current" nil))
  (is-true  (cl-tmux::%alert-action-fires-p "any" t))
  (is-true  (cl-tmux::%alert-action-fires-p "any" nil))
  (is-false (cl-tmux::%alert-action-fires-p "other" t))
  (is-true  (cl-tmux::%alert-action-fires-p "other" nil)))

(test silence-action-none-suppresses-alert
  "silence-action none suppresses the silence alert (and flag) even when the
   threshold is crossed."
  (with-isolated-state
    (let* ((sess (make-fake-session :nwindows 1))
           (win  (cl-tmux/model:session-active-window sess))
           (fired nil))
      (cl-tmux/options:set-option "monitor-silence" 5)
      (cl-tmux/options:set-option "silence-action" "none")
      (setf (cl-tmux/model:window-last-output-time win) (- (get-universal-time) 100)
            (cl-tmux/model:window-silence-flag win) nil)
      (cl-tmux/hooks:add-hook "alert-silence"
                              (lambda (&rest _) (declare (ignore _)) (setf fired t)))
      (cl-tmux::%check-monitor-silence (list (cons 1 sess)) (lambda () nil))
      (is-false fired "silence-action none must suppress the alert hook")
      (is-false (cl-tmux/model:window-silence-flag win)
                "silence-action none must not set the silence flag"))))

(test reader-remain-on-exit-state-returns-nil-when-not-running
  :description "reader-remain-on-exit-state returns NIL immediately when *running* is NIL."
  (with-dead-pane (pane)
    (let ((cl-tmux::*running* nil))
      (is (null (cl-tmux::reader-remain-on-exit-state pane))
          "remain-on-exit state must return NIL when *running* is NIL"))))

;;; Table-driven fbound checks for CPS reader state functions.
(test reader-state-functions-are-all-fbound
  :description "All CPS reader state machine functions are defined."
  (dolist (sym '(cl-tmux::reader-idle-state
                 cl-tmux::reader-reading-state
                 cl-tmux::reader-remain-on-exit-state
                 cl-tmux::reader-eof-state
                 cl-tmux::%run-reader-states
                 cl-tmux::start-reader-thread
                 cl-tmux::install-sigwinch-handler
                 cl-tmux::start-status-timer))
    (is (fboundp sym) "~A must be fbound" sym)))

(test run-reader-states-exits-when-running-nil
  :description "%run-reader-states exits immediately when *running* is NIL, even
given a non-NIL initial state (loop while *running*)."
  (with-dead-pane (pane)
    (let* ((cl-tmux::*running* nil)
           ;; A state function that should never be called.
           (boom (lambda (_p) (declare (ignore _p))
                   (error "state function called despite *running*=NIL"))))
      (finishes (cl-tmux::%run-reader-states pane boom)
                "%run-reader-states must exit immediately when *running* is NIL"))))

;;; ── stop-reader-threads ──────────────────────────────────────────────────────

(test stop-reader-threads-sets-running-nil
  :description "stop-reader-threads sets *running* to NIL regardless of thread count."
  (let ((cl-tmux::*running* t))
    (cl-tmux::stop-reader-threads '())
    (is-false cl-tmux::*running* "*running* must be NIL after stop-reader-threads")))

(test stop-reader-threads-empty-list
  :description "stop-reader-threads is a no-op on an empty thread list (no join attempted)."
  (let ((cl-tmux::*running* t))
    (finishes (cl-tmux::stop-reader-threads '())
              "stop-reader-threads with empty list must not signal")
    (is-false cl-tmux::*running*)))

(test stop-reader-threads-joins-already-dead-thread
  :description "stop-reader-threads tolerates joining a thread that has already exited."
  ;; with-global-running NIL sets the GLOBAL *running* the spawned thread reads,
  ;; so its (loop while *running*) exits immediately and the thread is already
  ;; dead by the time stop-reader-threads joins it.  A LET binding would be
  ;; invisible to the child thread, leaving it looping forever.
  (with-global-running nil
    (let ((thread (bordeaux-threads:make-thread
                   (lambda ()
                     (loop while cl-tmux::*running* do (sleep 0.001)))
                   :name "test-dead-thread")))
      (sleep 0.05)
      (finishes (cl-tmux::stop-reader-threads (list thread))
                "stop-reader-threads must not signal when joining a dead thread")
      (is-false cl-tmux::*running*))))


;;; ── add-message-log ──────────────────────────────────────────────────────────

(test add-message-log-prepends-entry
  :description "add-message-log prepends a (timestamp . text) cons and caps the log."
  (let ((cl-tmux::*message-log* nil))
    (cl-tmux::add-message-log "hello")
    (is (= 1 (length cl-tmux::*message-log*))
        "log should have 1 entry after one add-message-log")
    (is (string= "hello" (cdr (first cl-tmux::*message-log*)))
        "log entry text must match what was added")))

(test add-message-log-caps-at-message-limit
  :description "add-message-log caps *message-log* at the message-limit option."
  (with-isolated-options ("message-limit" 5)
    (let ((cl-tmux::*message-log* nil))
      (dotimes (i 12)
        (cl-tmux::add-message-log (format nil "msg-~D" i)))
      (is (= 5 (length cl-tmux::*message-log*))
          "*message-log* must be capped at message-limit (5), got ~D"
          (length cl-tmux::*message-log*)))))

(test add-prompt-history-caps-at-prompt-history-limit
  :description "add-prompt-history caps *prompt-history* at the prompt-history-limit option."
  (with-isolated-options ("prompt-history-limit" 4)
    (let ((cl-tmux::*prompt-history* nil))
      (dotimes (i 9)
        (cl-tmux::add-prompt-history (format nil "cmd-~D" i)))
      (is (= 4 (length cl-tmux::*prompt-history*))
          "*prompt-history* must be capped at prompt-history-limit (4), got ~D"
          (length cl-tmux::*prompt-history*)))))

(test add-message-log-newest-first
  :description "add-message-log prepends: the most recently added entry is first."
  (let ((cl-tmux::*message-log* nil))
    (cl-tmux::add-message-log "first")
    (cl-tmux::add-message-log "second")
    (is (string= "second" (cdr (first cl-tmux::*message-log*)))
        "newest entry must be first in the log")
    (is (string= "first" (cdr (second cl-tmux::*message-log*)))
        "older entry must be second in the log")))

(test add-message-log-entry-has-timestamp
  :description "Each log entry has a non-zero timestamp (from get-universal-time)."
  (let ((cl-tmux::*message-log* nil))
    (cl-tmux::add-message-log "timed")
    (let ((ts (car (first cl-tmux::*message-log*))))
      (is (integerp ts) "log entry timestamp must be an integer")
      (is (plusp ts)    "log entry timestamp must be positive"))))

;;; ── Constants coverage ────────────────────────────────────────────────────────

(test wait-for-channel-timeout-constant-is-positive
  :description "+wait-for-channel-timeout+ is a positive integer constant."
  (is (integerp cl-tmux::+wait-for-channel-timeout+)
      "+wait-for-channel-timeout+ must be an integer")
  (is (plusp cl-tmux::+wait-for-channel-timeout+)
      "+wait-for-channel-timeout+ must be positive"))

;;; ── Global variable coverage ──────────────────────────────────────────────────

(test clock-mode-pane-id-var-is-boundp
  :description "*clock-mode-pane-id* is defined and initially NIL."
  (is (boundp 'cl-tmux::*clock-mode-pane-id*)
      "*clock-mode-pane-id* must be bound")
  (is (null cl-tmux::*clock-mode-pane-id*)
      "*clock-mode-pane-id* must default to NIL"))

(test server-sessions-var-is-boundp
  :description "*server-sessions* is defined and is a list (possibly nil)."
  (is (boundp 'cl-tmux::*server-sessions*)
      "*server-sessions* must be bound")
  (is (listp cl-tmux::*server-sessions*)
      "*server-sessions* must be a list"))

(test message-log-var-is-boundp
  :description "*message-log* is defined and initially NIL."
  (is (boundp 'cl-tmux::*message-log*)
      "*message-log* must be bound"))

;;; ── Wait-for channel synchronization ─────────────────────────────────────────

(test ensure-channel-creates-entry
  :description "%ensure-channel creates a plist with :lock and :cv keys."
  (let ((cl-tmux::*wait-channels* (make-hash-table :test #'equal)))
    (let ((ch (cl-tmux::%ensure-channel "test-ch")))
      (is-true ch "%ensure-channel must return a plist")
      (is-true (getf ch :lock) "channel plist must have :lock")
      (is-true (getf ch :cv)   "channel plist must have :cv"))))

(test ensure-channel-is-idempotent
  :description "%ensure-channel returns the same plist for the same channel name."
  (let ((cl-tmux::*wait-channels* (make-hash-table :test #'equal)))
    (let ((ch1 (cl-tmux::%ensure-channel "idem"))
          (ch2 (cl-tmux::%ensure-channel "idem")))
      (is (eq ch1 ch2)
          "%ensure-channel must return the same plist on repeated calls"))))

(test lock-and-unlock-channel-toggle-flag
  :description "lock-channel sets :locked T; unlock-channel sets :locked NIL."
  (let ((cl-tmux::*wait-channels* (make-hash-table :test #'equal)))
    (cl-tmux::lock-channel "lk-test")
    (let ((ch (cl-tmux::%ensure-channel "lk-test")))
      (is-true (getf ch :locked) "lock-channel must set :locked to T"))
    (cl-tmux::unlock-channel "lk-test")
    (let ((ch (cl-tmux::%ensure-channel "lk-test")))
      (is-false (getf ch :locked) "unlock-channel must set :locked to NIL"))))

(test signal-channel-locked-is-noop
  :description "signal-channel does not error when the channel is locked."
  (let ((cl-tmux::*wait-channels* (make-hash-table :test #'equal)))
    (cl-tmux::lock-channel "sig-locked")
    ;; signal-channel on a locked channel is a no-op (no cv-notify) — must not signal.
    (finishes (cl-tmux::signal-channel "sig-locked")
              "signal-channel on a locked channel must not signal an error")))

(test wait-for-signal-unblocks
  :description "signal-channel creates/signals a channel; the full lock/unlock/signal
   lifecycle is safe with no waiters.  Uses isolated *wait-channels*."
  ;; Test the channel API with an isolated channels table.
  (let ((cl-tmux::*wait-channels* (make-hash-table :test #'equal)))
    ;; Ensure a channel exists.
    (cl-tmux::%ensure-channel "test-chan")
    ;; Lock and unlock must not error.
    (finishes (cl-tmux::lock-channel "test-chan")
              "lock-channel must not signal")
    (finishes (cl-tmux::unlock-channel "test-chan")
              "unlock-channel must not signal")
    ;; Signal with no waiters must be a safe no-op.
    (finishes (cl-tmux::signal-channel "test-chan")
              "signal-channel with no waiters must not signal")
    ;; When locked, signal-channel is suppressed.
    (cl-tmux::lock-channel "test-chan")
    (finishes (cl-tmux::signal-channel "test-chan")
              "signal-channel while locked must not signal")
    (cl-tmux::unlock-channel "test-chan")
    ;; After unlock, signal proceeds normally.
    (finishes (cl-tmux::signal-channel "test-chan")
              "signal-channel after unlock must not signal")))

(test ensure-channel-stores-in-hash-table
  :description "%ensure-channel stores the plist in *wait-channels* by name."
  (let ((cl-tmux::*wait-channels* (make-hash-table :test #'equal)))
    (cl-tmux::%ensure-channel "stored-ch")
    (is-true (gethash "stored-ch" cl-tmux::*wait-channels*)
             "*wait-channels* must contain entry after %ensure-channel")))

(test channel-locked-flag-defaults-to-nil
  :description "A freshly created channel has :locked NIL."
  (let ((cl-tmux::*wait-channels* (make-hash-table :test #'equal)))
    (let ((ch (cl-tmux::%ensure-channel "fresh-lock")))
      (is-false (getf ch :locked)
                "new channel must start with :locked NIL"))))

(test lock-channel-then-signal-then-unlock-is-safe
  :description "The lock→signal→unlock sequence completes without error and leaves
   the channel unlocked."
  (let ((cl-tmux::*wait-channels* (make-hash-table :test #'equal)))
    (cl-tmux::lock-channel "seq-ch")
    (finishes (cl-tmux::signal-channel "seq-ch")
              "signal-channel while locked must not error")
    (cl-tmux::unlock-channel "seq-ch")
    (let ((ch (cl-tmux::%ensure-channel "seq-ch")))
      (is-false (getf ch :locked)
                "channel must be unlocked after unlock-channel"))))

(test multiple-distinct-channels-independent
  :description "Two channels with different names are stored independently."
  (let ((cl-tmux::*wait-channels* (make-hash-table :test #'equal)))
    (let ((ch-a (cl-tmux::%ensure-channel "ch-a"))
          (ch-b (cl-tmux::%ensure-channel "ch-b")))
      (is (not (eq ch-a ch-b))
          "distinct channel names must produce distinct plists")
      (cl-tmux::lock-channel "ch-a")
      (let ((ch-a2 (cl-tmux::%ensure-channel "ch-a"))
            (ch-b2 (cl-tmux::%ensure-channel "ch-b")))
        (is-true  (getf ch-a2 :locked) "ch-a must be locked")
        (is-false (getf ch-b2 :locked) "ch-b must remain unlocked")))))
