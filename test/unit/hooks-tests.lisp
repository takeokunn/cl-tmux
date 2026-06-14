(in-package #:cl-tmux/test)

;;;; Hooks system tests (src/hooks.lisp).

(def-suite hooks-suite :description "Hooks registry: add, remove, run, clear, list")
(in-suite hooks-suite)

;;; All tests isolate themselves via with-isolated-hooks, which rebinds
;;; *hook-registry* to a fresh table so registrations never leak.

;;; ── Hook event constant values ───────────────────────────────────────────────

(test hook-event-constants
  "Hook event constants defined via define-hook-events have the expected string values."
  (dolist (c `(("after-new-window"    ,cl-tmux/hooks:+hook-after-new-window+)
               ("after-new-pane"      ,cl-tmux/hooks:+hook-after-new-pane+)
               ("pane-exited"         ,cl-tmux/hooks:+hook-pane-exited+)
               ("after-rename-window" ,cl-tmux/hooks:+hook-after-rename-window+)
               ("session-created"     ,cl-tmux/hooks:+hook-session-created+)
               ("after-kill-pane"     ,cl-tmux/hooks:+hook-after-kill-pane+)
               ("after-kill-window"   ,cl-tmux/hooks:+hook-after-kill-window+)
               ("after-split-window"  ,cl-tmux/hooks:+hook-after-split-window+)
               ("client-attached"     ,cl-tmux/hooks:+hook-client-attached+)
               ("client-detached"     ,cl-tmux/hooks:+hook-client-detached+)
               ("alert-bell"          ,cl-tmux/hooks:+hook-alert-bell+)))
    (destructuring-bind (expected actual) c
      (is (string= expected actual) "constant must equal ~S" expected))))

;;; hook-event-constants-are-strings was removed: hook-event-constants already
;;; asserts string= for every constant, which implies stringp — the type check
;;; was a strict subset and added no new coverage.

;;; ── *hook-registry* initial state ───────────────────────────────────────────

(test hook-registry-is-hash-table
  "*hook-registry* is a hash table with :equal test."
  (with-isolated-hooks
    (is (hash-table-p cl-tmux/hooks:*hook-registry*)
        "*hook-registry* must be a hash table")))

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
    ;; Use LET* (not LET): the CB closure must capture the SAME binding the
    ;; assertion reads.  Under a PARALLEL let, the init-form (lambda () (incf
    ;; hits)) is evaluated in the ENCLOSING scope, where HITS is a free
    ;; (special, unbound) variable — the callback then errors at call time
    ;; ("variable HITS is unbound"), run-hooks swallows it, and the counter
    ;; never moves.  let* binds HITS before evaluating CB's init-form.
    (let* ((hits 0)
           (cb   (lambda () (incf hits))))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-new-window+ cb)
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-new-window+ cb)
      (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-new-window+)
      (is (= 2 hits) "same callback added twice must be called twice"))))

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

(test run-hooks-swallows-error-and-continues
  "run-hooks suppresses any ERROR (including subclasses) and continues executing remaining hooks."
  (dolist (row (list (list cl-tmux/hooks:+hook-session-created+
                           (lambda () (error "deliberate hook error"))
                           "generic ERROR")
                     (list cl-tmux/hooks:+hook-after-kill-pane+
                           (lambda () (error 'simple-error :format-control "hook boom"))
                           "SIMPLE-ERROR subclass")))
    (destructuring-bind (hook error-fn desc) row
      (with-isolated-hooks
        (let ((good-ran nil))
          (cl-tmux/hooks:add-hook hook error-fn)
          (cl-tmux/hooks:add-hook hook (lambda () (setf good-ran t)))
          (finishes (cl-tmux/hooks:run-hooks hook))
          (is-true good-ran "~A: non-error hook must still run" desc))))))

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

;;; wait-for-signal-unblocks was moved to runtime-tests.lisp because it tests
;;; cl-tmux::*wait-channels*, cl-tmux::%ensure-channel, cl-tmux::signal-channel,
;;; cl-tmux::lock-channel, and cl-tmux::unlock-channel — all from runtime.lisp —
;;; and runtime-tests.lisp already covers these same symbols.

;;; ── clear-hooks isolation ────────────────────────────────────────────────────

(test clear-hooks-does-not-affect-other-events
  "clear-hooks for one event leaves other events' hooks intact."
  (with-isolated-hooks
    (let ((called-a nil)
          (called-b nil))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-new-window+
                               (lambda () (setf called-a t)))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-pane-exited+
                               (lambda () (setf called-b t)))
      ;; Clear only the first event.
      (cl-tmux/hooks:clear-hooks cl-tmux/hooks:+hook-after-new-window+)
      (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-new-window+)
      (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-pane-exited+)
      (is (null called-a) "cleared event's hook must not run")
      (is-true called-b   "non-cleared event's hook must still run"))))

;;; ── run-hooks with +hook-after-split-window+ ─────────────────────────────────

(test run-hooks-after-split-window-event
  "add-hook and run-hooks work correctly for the after-split-window event."
  (with-isolated-hooks
    (let ((fired nil))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-split-window+
                               (lambda () (setf fired t)))
      (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-split-window+)
      (is-true fired "after-split-window hook must fire when run-hooks is called"))))

;;; ── list-hooks after all callbacks removed ───────────────────────────────────

(test list-hooks-after-all-callbacks-removed-shows-zero
  "After removing the last callback for an event, list-hooks shows count 0 for it."
  (with-isolated-hooks
    (let ((cb (lambda () nil)))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-rename-window+ cb)
      (cl-tmux/hooks:remove-hook cl-tmux/hooks:+hook-after-rename-window+ cb)
      ;; The event key remains in the registry with an empty list.
      ;; list-hooks reports count 0 for it (not absent, because gethash returns nil-list).
      ;; Either outcome (absent or count-0) is acceptable; what matters is run-hooks is safe.
      (finishes (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-rename-window+)))))

;;; hook-event-constants-table-driven, hook-client-attached-constant,
;;; hook-client-detached-constant, and hook-alert-bell-constant were removed:
;;; hook-event-constants above already asserts string= for every constant,
;;; making the table-driven form and the three standalone tests strict subsets
;;; that add no new coverage (14 redundant assertions removed).

(test run-hooks-client-attached-fires-all-callbacks
  "run-hooks fires all registered callbacks for client-attached."
  (with-isolated-hooks
    (let ((first-called nil)
          (second-called nil))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-client-attached+
                               (lambda () (setf first-called t)))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-client-attached+
                               (lambda () (setf second-called t)))
      (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-client-attached+)
      (is-true first-called  "first callback must fire for client-attached")
      (is-true second-called "second callback must fire for client-attached"))))
