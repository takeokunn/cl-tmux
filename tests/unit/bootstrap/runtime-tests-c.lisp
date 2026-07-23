(in-package #:cl-tmux/test)

;;;; runtime tests — part C: stop-reader-threads, add-message-log, add-prompt-history,
;;;; constants/global-var coverage, wait-for channel synchronization.

(describe "runtime-suite"

  ;; ── stop-reader-threads ──────────────────────────────────────────────────────

  ;; stop-reader-threads sets *running* to NIL regardless of thread count.
  (it "stop-reader-threads-sets-running-nil"
    (let ((cl-tmux::*running* t))
      (cl-tmux::stop-reader-threads '())
      (expect cl-tmux::*running* :to-be-falsy)))

  ;; stop-reader-threads is a no-op on an empty thread list (no join attempted).
  (it "stop-reader-threads-empty-list"
    (let ((cl-tmux::*running* t))
      (finishes (cl-tmux::stop-reader-threads '())
                "stop-reader-threads with empty list must not signal")
      (expect cl-tmux::*running* :to-be-falsy)))

  ;; stop-reader-threads tolerates joining a thread that has already exited.
  (it "stop-reader-threads-joins-already-dead-thread"
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
        (expect cl-tmux::*running* :to-be-falsy))))

  ;; ── add-message-log ──────────────────────────────────────────────────────────

  ;; add-message-log caps *message-log* at the message-limit option.
  (it "add-message-log-caps-at-message-limit"
    (with-isolated-options ("message-limit" 5)
      (let ((cl-tmux::*message-log* nil))
        (dotimes (i 12)
          (cl-tmux::add-message-log (format nil "msg-~D" i)))
        (expect (= 5 (length cl-tmux::*message-log*))))))

  ;; add-prompt-history caps *prompt-history* at the prompt-history-limit option.
  (it "add-prompt-history-caps-at-prompt-history-limit"
    (with-isolated-options ("prompt-history-limit" 4)
      (let ((cl-tmux::*prompt-history* nil))
        (dotimes (i 9)
          (cl-tmux::add-prompt-history (format nil "cmd-~D" i)))
        (expect (= 4 (length cl-tmux::*prompt-history*))))))

  ;; add-message-log prepends: the most recently added entry is first.
  (it "add-message-log-newest-first"
    (let ((cl-tmux::*message-log* nil))
      (cl-tmux::add-message-log "first")
      (cl-tmux::add-message-log "second")
      (expect (string= "second" (cdr (first cl-tmux::*message-log*))))
      (expect (string= "first" (cdr (second cl-tmux::*message-log*))))))

  ;; Each log entry has a non-zero timestamp (from get-universal-time).
  (it "add-message-log-entry-has-timestamp"
    (let ((cl-tmux::*message-log* nil))
      (cl-tmux::add-message-log "timed")
      (let ((ts (car (first cl-tmux::*message-log*))))
        (expect (integerp ts))
        (expect (plusp ts)))))

  ;; add-message-log also appends to the current client's message log.
  (it "add-message-log-mirrors-to-current-client-log"
    (let ((cl-tmux::*message-log* nil)
          (cl-tmux::*current-client-conn* (cl-tmux::%make-client-conn
                                           :state (cl-tmux::make-input-state))))
      (cl-tmux::add-message-log "client-scoped")
      (expect (string= "client-scoped" (cdr (first cl-tmux::*message-log*))))
      (expect (string= "client-scoped"
                       (cdr (first (cl-tmux::client-conn-message-log
                                    cl-tmux::*current-client-conn*)))))))

  ;; ── Constants coverage ────────────────────────────────────────────────────────

  ;; +wait-for-channel-timeout+ is a positive integer constant.
  (it "wait-for-channel-timeout-constant-is-positive"
    (expect (integerp cl-tmux::+wait-for-channel-timeout+))
    (expect (plusp cl-tmux::+wait-for-channel-timeout+)))

  ;; ── Global variable coverage ──────────────────────────────────────────────────

  ;; *clock-mode-pane-id* is defined and initially NIL.
  (it "clock-mode-pane-id-var-is-boundp"
    (expect (boundp 'cl-tmux::*clock-mode-pane-id*))
    (expect (null cl-tmux::*clock-mode-pane-id*)))

  ;; *server-sessions* is defined and is a list (possibly nil).
  (it "server-sessions-var-is-boundp"
    (expect (boundp 'cl-tmux::*server-sessions*))
    (expect (listp cl-tmux::*server-sessions*)))

  ;; *message-log* is defined and initially NIL.
  (it "message-log-var-is-boundp"
    (expect (boundp 'cl-tmux::*message-log*)))

  ;; ── Wait-for channel synchronization ─────────────────────────────────────────

  ;; %ensure-channel creates a plist with :lock and :cv keys.
  (it "ensure-channel-creates-entry"
    (let ((cl-tmux::*wait-channels* (make-hash-table :test #'equal)))
      (let ((ch (cl-tmux::%ensure-channel "test-ch")))
        (expect ch :to-be-truthy)
        (expect (getf ch :lock) :to-be-truthy)
        (expect (getf ch :cv) :to-be-truthy))))

  ;; %ensure-channel returns the same plist for the same channel name.
  (it "ensure-channel-is-idempotent"
    (let ((cl-tmux::*wait-channels* (make-hash-table :test #'equal)))
      (let ((ch1 (cl-tmux::%ensure-channel "idem"))
            (ch2 (cl-tmux::%ensure-channel "idem")))
        (expect (eq ch1 ch2)))))

  ;; lock-channel sets :locked T; unlock-channel sets :locked NIL.
  (it "lock-and-unlock-channel-toggle-flag"
    (let ((cl-tmux::*wait-channels* (make-hash-table :test #'equal)))
      (cl-tmux::lock-channel "lk-test")
      (let ((ch (cl-tmux::%ensure-channel "lk-test")))
        (expect (getf ch :locked) :to-be-truthy))
      (cl-tmux::unlock-channel "lk-test")
      (let ((ch (cl-tmux::%ensure-channel "lk-test")))
        (expect (getf ch :locked) :to-be-falsy))))

  ;; signal-channel does not error when the channel is locked.
  (it "signal-channel-locked-is-noop"
    (let ((cl-tmux::*wait-channels* (make-hash-table :test #'equal)))
      (cl-tmux::lock-channel "sig-locked")
      ;; signal-channel on a locked channel is a no-op (no cv-notify) — must not signal.
      (finishes (cl-tmux::signal-channel "sig-locked")
                "signal-channel on a locked channel must not signal an error")))

  ;; signal-channel creates/signals a channel; the full lock/unlock/signal
  ;; lifecycle is safe with no waiters.  Uses isolated *wait-channels*.
  (it "wait-for-signal-unblocks"
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

  ;; %ensure-channel stores the plist in *wait-channels* by name.
  (it "ensure-channel-stores-in-hash-table"
    (let ((cl-tmux::*wait-channels* (make-hash-table :test #'equal)))
      (cl-tmux::%ensure-channel "stored-ch")
      (expect (gethash "stored-ch" cl-tmux::*wait-channels*) :to-be-truthy)))

  ;; A freshly created channel has :locked NIL.
  (it "channel-locked-flag-defaults-to-nil"
    (let ((cl-tmux::*wait-channels* (make-hash-table :test #'equal)))
      (let ((ch (cl-tmux::%ensure-channel "fresh-lock")))
        (expect (getf ch :locked) :to-be-falsy))))

  ;; The lock→signal→unlock sequence completes without error and leaves
  ;; the channel unlocked.
  (it "lock-channel-then-signal-then-unlock-is-safe"
    (let ((cl-tmux::*wait-channels* (make-hash-table :test #'equal)))
      (cl-tmux::lock-channel "seq-ch")
      (finishes (cl-tmux::signal-channel "seq-ch")
                "signal-channel while locked must not error")
      (cl-tmux::unlock-channel "seq-ch")
      (let ((ch (cl-tmux::%ensure-channel "seq-ch")))
        (expect (getf ch :locked) :to-be-falsy))))

  ;; Two channels with different names are stored independently.
  (it "multiple-distinct-channels-independent"
    (let ((cl-tmux::*wait-channels* (make-hash-table :test #'equal)))
      (let ((ch-a (cl-tmux::%ensure-channel "ch-a"))
            (ch-b (cl-tmux::%ensure-channel "ch-b")))
        (expect (not (eq ch-a ch-b)))
        (cl-tmux::lock-channel "ch-a")
        (let ((ch-a2 (cl-tmux::%ensure-channel "ch-a"))
              (ch-b2 (cl-tmux::%ensure-channel "ch-b")))
          (expect (getf ch-a2 :locked) :to-be-truthy)
          (expect (getf ch-b2 :locked) :to-be-falsy))))))
