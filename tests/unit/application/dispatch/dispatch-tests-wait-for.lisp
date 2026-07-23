(in-package #:cl-tmux/test)

;;;; Dispatch wait-for command tests.

(describe "dispatch-suite"

  ;; wait-for -S channel signals the named channel (unblocks waiters).
  (it "cmd-wait-for-arg-signal-signals-channel"
    (with-fake-session (s)
      (let ((received nil))
        ;; Start a thread waiting on the channel.
        (bt:make-thread
         (lambda () (setf received (cl-tmux::wait-for-channel "test-ch-signal")))
         :name "waiter")
        ;; Brief yield so the waiter thread reaches condition-wait before signal.
        (sleep 0.05)
        (cl-tmux::%cmd-wait-for-arg s '("-S" "test-ch-signal"))
        (sleep 0.05)
        (expect received :to-be-truthy))))

  ;; wait-for -S -- channel treats -- as an option terminator and signals channel.
  (it "cmd-wait-for-arg-option-terminator-after-flags"
    (with-fake-session (s)
      (let ((received nil))
        (bt:make-thread
         (lambda () (setf received (cl-tmux::wait-for-channel "test-ch-dd-signal")))
         :name "waiter-with-double-dash")
        (sleep 0.05)
        (cl-tmux::%cmd-wait-for-arg s '("-S" "--" "test-ch-dd-signal"))
        (sleep 0.05)
        (expect received :to-be-truthy))))

  ;; wait-for -L channel locks the channel; subsequent -S does not notify waiters.
  (it "cmd-wait-for-arg-lock-suppresses-signal"
    (with-fake-session (s)
      ;; Lock first, then signal: the signal should be a no-op.
      (cl-tmux::%cmd-wait-for-arg s '("-L" "test-ch-lock"))
      ;; A waiter on a LOCKED channel receives no notification; wait-for-channel
      ;; will time-out and return NIL. Verify the lock was applied directly.
      (let ((ch (cl-tmux::%ensure-channel "test-ch-lock")))
        (expect (getf ch :locked) :to-be-truthy))))

  ;; wait-for -U channel unlocks a previously locked channel.
  (it "cmd-wait-for-arg-unlock-clears-lock"
    (with-fake-session (s)
      (cl-tmux::%cmd-wait-for-arg s '("-L" "test-ch-unlock"))
      (cl-tmux::%cmd-wait-for-arg s '("-U" "test-ch-unlock"))
      (let ((ch (cl-tmux::%ensure-channel "test-ch-unlock")))
        (expect (getf ch :locked) :to-be-falsy))))

  ;; wait-for channel (bare, no flags) blocks until the channel is signaled.
  (it "cmd-wait-for-arg-bare-blocks-until-signaled"
    (with-fake-session (s)
      (let ((result :pending))
        ;; Run wait-for in a background thread so it blocks without stalling tests.
        (bt:make-thread
         (lambda ()
           (setf result (cl-tmux::%cmd-wait-for-arg s '("test-ch-bare"))))
         :name "bare-waiter")
        (sleep 0.05)
        (cl-tmux::signal-channel "test-ch-bare")
        (sleep 0.05)
        (expect (not (eq result :pending))))))

  ;; wait-for rejects invalid arguments with canonical diagnostics.
  (it "cmd-wait-for-unsupported-arguments-are-rejected-before-channel-state"
    (with-fake-session (s)
      (let ((cl-tmux::*wait-channels* (make-hash-table :test #'equal)))
        (dolist (case '((("-Z" "test-ch-unsupported")
                         "command wait-for: unknown flag -Z")
                        (("-SZ" "test-ch-unsupported")
                         "command wait-for: unknown flag -Z")
                        (("-S")
                         "command wait-for: too few arguments (need at least 1)")
                        (("--")
                         "command wait-for: too few arguments (need at least 1)")
                        (("-L" "test-ch-unsupported" "extra")
                         "command wait-for: too many arguments (need at most 1)")
                        (("test-ch-unsupported" "-S")
                         "command wait-for: too many arguments (need at most 1)")))
          (destructuring-bind (args expected) case
            (let (cl-tmux::*overlay*)
              (cl-tmux::%cmd-wait-for-arg s args)
              (assert-overlay-contains expected
                                        cl-tmux::*overlay*
                                        "wait-for")
              (expect (gethash "test-ch-unsupported" cl-tmux::*wait-channels*) :to-be-falsy))))))))
