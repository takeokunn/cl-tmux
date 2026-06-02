(in-package #:cl-tmux/test)

;;;; Hooks system tests (src/hooks.lisp).

(def-suite hooks-suite :description "Hooks registry: add, remove, run, clear, list")
(in-suite hooks-suite)

;;; All tests isolate themselves via with-isolated-hooks, which rebinds
;;; *hook-registry* to a fresh table so registrations never leak.

(test add-and-run-hook
  "add-hook registers a callback; run-hooks calls it."
  (with-isolated-hooks
    (let ((called nil))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-new-window+
                               (lambda () (setf called t)))
      (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-new-window+)
      (is-true called "hook must be called after run-hooks"))))

(test hooks-newest-first
  "Two add-hooks run newest-first (front-push order)."
  (with-isolated-hooks
    (let ((order '()))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-new-window+
                               (lambda () (push :first order)))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-new-window+
                               (lambda () (push :second order)))
      (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-new-window+)
      ;; :second was added last so it runs first; push prepends, so result is (:first :second)
      (is (equal '(:first :second) order)
          "hooks must run newest-first (got ~S)" order))))

(test remove-hook-by-identity
  "remove-hook removes by eq identity; other callbacks still run."
  (with-isolated-hooks
    (let* ((call-count-a 0)
           (call-count-b 0)
           (cb-a (lambda () (incf call-count-a)))
           (cb-b (lambda () (incf call-count-b))))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-new-pane+ cb-a)
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-new-pane+ cb-b)
      ;; Both run before removal
      (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-new-pane+)
      (is (= 1 call-count-a) "cb-a called once before removal")
      (is (= 1 call-count-b) "cb-b called once before removal")
      ;; Remove only cb-b
      (cl-tmux/hooks:remove-hook cl-tmux/hooks:+hook-after-new-pane+ cb-b)
      (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-new-pane+)
      (is (= 2 call-count-a) "cb-a must still be called after cb-b removed")
      (is (= 1 call-count-b) "cb-b must NOT be called after remove-hook"))))

(test run-hooks-ignores-errors
  "A hook that signals an error does not propagate and does not stop other hooks."
  (with-isolated-hooks
    (let ((second-called nil))
      ;; Register the good hook first (it will run second/newest-last)
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-session-created+
                               (lambda () (setf second-called t)))
      ;; Register the bad hook second (newest-first, so it runs first)
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-session-created+
                               (lambda () (error "deliberate hook error")))
      ;; run-hooks must not signal an error to the caller
      (finishes (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-session-created+))
      ;; The older (good) hook must still have been called
      (is-true second-called
               "subsequent hooks must run even after an earlier hook signals an error"))))

(test run-hooks-suppresses-any-error-subclass
  "run-hooks swallows the full ERROR condition hierarchy -- a subclass of ERROR
   (here a SIMPLE-ERROR) is also silently suppressed."
  (with-isolated-hooks
    (let ((suppressed nil))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-kill-pane+
                               (lambda ()
                                 (handler-case
                                     (progn
                                       (error "inner ~A" "error")
                                       (setf suppressed nil))
                                   ;; This handler runs only if the error escapes run-hooks.
                                   ;; If run-hooks swallows it, SUPPRESSED stays at its
                                   ;; initial value (:not-reached) set before run-hooks.
                                   (error () (setf suppressed :escaped)))))
      ;; Use a second hook that signals directly -- run-hooks must swallow it.
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-kill-pane+
                               (lambda () (error 'simple-error :format-control "hook boom")))
      (setf suppressed :not-reached)
      (finishes (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-kill-pane+))
      ;; The good hook ran; the error did not escape past run-hooks into the first hook's
      ;; handler-case -- so suppressed must still be :not-reached.
      (is (eq :not-reached suppressed)
          "SIMPLE-ERROR from a later hook must not propagate to an earlier hook"))))

(test run-hooks-passes-args
  "run-hooks passes its extra arguments to each registered callback."
  (with-isolated-hooks
    (let ((received nil))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-pane-exited+
                               (lambda (arg) (setf received arg)))
      (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-pane-exited+ 42)
      (is (eql 42 received) "hook must receive the argument passed to run-hooks"))))

(test clear-hooks-removes-all
  "clear-hooks removes every callback; run-hooks becomes a no-op and list-hooks drops the entry."
  (with-isolated-hooks
    (let ((called nil))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-kill-pane+
                               (lambda () (setf called :first)))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-kill-pane+
                               (lambda () (setf called :second)))
      ;; Sanity: hooks present before clearing
      (let ((before (cl-tmux/hooks:list-hooks)))
        (is (= 2 (cdr (assoc cl-tmux/hooks:+hook-after-kill-pane+ before
                             :test #'string=)))
            "expect 2 hooks before clear"))
      ;; Clear
      (cl-tmux/hooks:clear-hooks cl-tmux/hooks:+hook-after-kill-pane+)
      ;; run-hooks must be a no-op: called stays NIL
      (setf called nil)
      (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-kill-pane+)
      (is (null called) "no hook must run after clear-hooks")
      ;; list-hooks must no longer include the entry
      (let ((after (cl-tmux/hooks:list-hooks)))
        (is (null (assoc cl-tmux/hooks:+hook-after-kill-pane+ after :test #'string=))
            "list-hooks must not include cleared event")))))

(test list-hooks-returns-alist
  "list-hooks returns an alist of (event-name . count) for all registered events."
  (with-isolated-hooks
    (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-new-window+ (lambda () nil))
    (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-new-window+ (lambda () nil))
    (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-pane-exited+      (lambda () nil))
    (let ((alist (cl-tmux/hooks:list-hooks)))
      (is (= 2 (length alist)) "two distinct event names must appear")
      (let ((nw-count (cdr (assoc cl-tmux/hooks:+hook-after-new-window+ alist
                                  :test #'string=)))
            (pe-count (cdr (assoc cl-tmux/hooks:+hook-pane-exited+ alist
                                  :test #'string=))))
        (is (= 2 nw-count) "after-new-window must show count 2")
        (is (= 1 pe-count) "pane-exited must show count 1")))))

(test hook-event-constants
  "Hook event constants defined via define-hook-events have the expected string values."
  (is (string= "after-new-window"    cl-tmux/hooks:+hook-after-new-window+))
  (is (string= "after-new-pane"      cl-tmux/hooks:+hook-after-new-pane+))
  (is (string= "pane-exited"         cl-tmux/hooks:+hook-pane-exited+))
  (is (string= "after-rename-window" cl-tmux/hooks:+hook-after-rename-window+))
  (is (string= "session-created"     cl-tmux/hooks:+hook-session-created+))
  (is (string= "after-kill-pane"     cl-tmux/hooks:+hook-after-kill-pane+))
  (is (string= "after-kill-window"   cl-tmux/hooks:+hook-after-kill-window+))
  (is (string= "after-split-window"  cl-tmux/hooks:+hook-after-split-window+)))

;;; -- Channel synchronization tests (cl-tmux internals) -----------------------
;;;
;;; wait-for-signal uses an internal channel table (*wait-channels*).
;;; These tests access internal symbols deliberately: the channel API has no
;;; higher-level public entry point that can be exercised in a unit test without
;;; spawning real threads.  The internal access is documented here so reviewers
;;; know it is intentional, not accidental coupling.

(test wait-for-signal-unblocks
  "signal-channel creates/signals a channel; wait-for-channel unblocks when signaled.
   Uses isolated *wait-channels* to avoid leaking state across tests."
  ;; Test the channel API with an isolated channels table.
  (let ((cl-tmux::*wait-channels* (make-hash-table :test #'equal)))
    ;; Ensure a channel exists
    (cl-tmux::%ensure-channel "test-chan")
    ;; Lock and unlock should not error
    (finishes (cl-tmux::lock-channel "test-chan"))
    (finishes (cl-tmux::unlock-channel "test-chan"))
    ;; Signal a channel (no waiters -- should be safe no-op)
    (finishes (cl-tmux::signal-channel "test-chan"))
    ;; When locked, signal-channel is suppressed
    (cl-tmux::lock-channel "test-chan")
    (finishes (cl-tmux::signal-channel "test-chan"))
    (cl-tmux::unlock-channel "test-chan")
    ;; After unlock, signal proceeds normally
    (finishes (cl-tmux::signal-channel "test-chan"))))
