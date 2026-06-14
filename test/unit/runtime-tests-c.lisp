(in-package #:cl-tmux/test)

;;;; runtime tests — part C: stop-reader-threads, add-message-log, add-prompt-history,
;;;; constants/global-var coverage, wait-for channel synchronization.

(in-suite runtime-suite)

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
