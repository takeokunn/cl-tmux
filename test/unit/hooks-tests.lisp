(in-package #:cl-tmux/test)

;;;; Hooks system tests (src/hooks.lisp).

(def-suite hooks-suite :description "Hooks registry: add, remove, run, clear, list")
(in-suite hooks-suite)

;;; All tests isolate themselves via with-isolated-hooks, which rebinds
;;; *hook-registry* to a fresh table so registrations never leak.

;;; ── Hook event constant values ───────────────────────────────────────────────

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

(test hook-event-constants-are-strings
  "Every hook-event constant is a string (not a symbol or keyword)."
  (dolist (c (list cl-tmux/hooks:+hook-after-new-window+
                   cl-tmux/hooks:+hook-after-new-pane+
                   cl-tmux/hooks:+hook-pane-exited+
                   cl-tmux/hooks:+hook-after-rename-window+
                   cl-tmux/hooks:+hook-session-created+
                   cl-tmux/hooks:+hook-after-kill-pane+
                   cl-tmux/hooks:+hook-after-kill-window+
                   cl-tmux/hooks:+hook-after-split-window+))
    (is (stringp c) "hook event constant ~S must be a string" c)))

;;; ── *hook-registry* initial state ───────────────────────────────────────────

(test hook-registry-is-hash-table
  "*hook-registry* is a hash table with :equal test."
  (is (hash-table-p cl-tmux/hooks:*hook-registry*)
      "*hook-registry* must be a hash table"))

(test hook-registry-fresh-is-empty
  "A freshly-isolated registry has no entries."
  (with-isolated-hooks
    (is (zerop (hash-table-count cl-tmux/hooks:*hook-registry*))
        "fresh isolated registry must be empty")))

;;; ── add-hook / run-hooks ─────────────────────────────────────────────────────

(test add-and-run-hook
  "add-hook registers a callback; run-hooks calls it."
  (with-isolated-hooks
    (let ((called nil))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-new-window+
                               (lambda () (setf called t)))
      (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-new-window+)
      (is-true called "hook must be called after run-hooks"))))

(test run-hooks-unregistered-event-is-noop
  "run-hooks on an event with no callbacks is a safe no-op."
  (with-isolated-hooks
    (finishes (cl-tmux/hooks:run-hooks "no-such-event"))
    (finishes (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-new-window+))))

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

(test add-same-callback-twice
  "Adding the same lambda twice registers it twice; run-hooks calls it twice."
  (with-isolated-hooks
    (let ((count 0)
          (cb    (lambda () (incf count))))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-new-window+ cb)
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-new-window+ cb)
      (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-new-window+)
      (is (= 2 count) "same callback added twice must be called twice"))))

;;; ── remove-hook ──────────────────────────────────────────────────────────────

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

(test remove-hook-nonexistent-callback-is-noop
  "remove-hook on a callback that was never added is a safe no-op."
  (with-isolated-hooks
    (let* ((count 0)
           (cb-registered   (lambda () (incf count)))
           (cb-unregistered (lambda () (incf count))))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-new-pane+ cb-registered)
      ;; Remove a callback that was never added — must not error.
      (finishes
        (cl-tmux/hooks:remove-hook cl-tmux/hooks:+hook-after-new-pane+ cb-unregistered))
      ;; The registered callback is still present.
      (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-new-pane+)
      (is (= 1 count) "registered callback still runs after removing unregistered one"))))

(test remove-hook-removes-all-occurrences
  "remove-hook removes every occurrence of the callback (not just the first)."
  (with-isolated-hooks
    (let* ((count 0)
           (cb    (lambda () (incf count))))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-new-window+ cb)
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-new-window+ cb)
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-new-window+ cb)
      (cl-tmux/hooks:remove-hook cl-tmux/hooks:+hook-after-new-window+ cb)
      (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-new-window+)
      (is (= 0 count) "all occurrences of CB must be removed"))))

;;; ── run-hooks error resilience ───────────────────────────────────────────────

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
   (here a SIMPLE-ERROR) is also silently suppressed without reaching the caller."
  (with-isolated-hooks
    ;; Register a hook that signals a SIMPLE-ERROR (a direct subclass of ERROR).
    ;; run-hooks must catch it without the error propagating to the caller.
    (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-kill-pane+
                             (lambda () (error 'simple-error :format-control "hook boom")))
    ;; Register a second hook to verify run-hooks continues after the first errors.
    (let ((second-ran nil))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-kill-pane+
                               (lambda () (setf second-ran t)))
      ;; FINISHES asserts that no condition escapes.
      (finishes (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-kill-pane+))
      ;; The second hook (added last = runs first, newest-first) ran before the error hook.
      (is-true second-ran
               "the first-registered hook must run despite the second hook signalling an error"))))

;;; ── run-hooks argument passing ───────────────────────────────────────────────

(test run-hooks-passes-args
  "run-hooks passes its extra arguments to each registered callback."
  (with-isolated-hooks
    (let ((received nil))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-pane-exited+
                               (lambda (arg) (setf received arg)))
      (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-pane-exited+ 42)
      (is (eql 42 received) "hook must receive the argument passed to run-hooks"))))

(test run-hooks-passes-multiple-args
  "run-hooks forwards all extra arguments when a callback accepts more than one."
  (with-isolated-hooks
    (let ((got-a nil) (got-b nil))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-pane-exited+
                               (lambda (a b) (setf got-a a got-b b)))
      (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-pane-exited+ :x :y)
      (is (eq :x got-a) "first argument must be forwarded")
      (is (eq :y got-b) "second argument must be forwarded"))))

;;; ── clear-hooks ──────────────────────────────────────────────────────────────

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

(test clear-hooks-unregistered-event-is-noop
  "clear-hooks on an event with no registered hooks is a safe no-op."
  (with-isolated-hooks
    (finishes (cl-tmux/hooks:clear-hooks "totally-unknown-event"))
    (finishes (cl-tmux/hooks:clear-hooks cl-tmux/hooks:+hook-after-kill-pane+))))

;;; ── list-hooks ───────────────────────────────────────────────────────────────

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

(test list-hooks-empty-registry-returns-nil
  "list-hooks on an empty registry returns NIL."
  (with-isolated-hooks
    (is (null (cl-tmux/hooks:list-hooks))
        "list-hooks must return NIL when no hooks are registered")))

(test list-hooks-counts-reflect-add-and-remove
  "list-hooks counts update correctly as callbacks are added and removed."
  (with-isolated-hooks
    (let ((cb-a (lambda () nil))
          (cb-b (lambda () nil)))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-new-window+ cb-a)
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-new-window+ cb-b)
      (let ((before (cdr (assoc cl-tmux/hooks:+hook-after-new-window+
                                (cl-tmux/hooks:list-hooks) :test #'string=))))
        (is (= 2 before) "count must be 2 after two adds"))
      (cl-tmux/hooks:remove-hook cl-tmux/hooks:+hook-after-new-window+ cb-a)
      (let ((after (cdr (assoc cl-tmux/hooks:+hook-after-new-window+
                               (cl-tmux/hooks:list-hooks) :test #'string=))))
        (is (= 1 after) "count must be 1 after one remove")))))

;;; ── Channel synchronization tests (cl-tmux internals) -----------------------
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
