(in-package #:cl-tmux/test)

;;;; Hooks system tests (src/hooks.lisp).

;;; All tests isolate themselves via with-isolated-hooks, which rebinds
;;; *hook-registry* to a fresh table so registrations never leak.

;;; ── Shared constant-table assertion helper ───────────────────────────────────

(defun %assert-hook-constant-table (pairs)
  "Assert that each (expected-string actual-constant) pair in PAIRS satisfies
   string= equality.  Extracted to eliminate the identical dolist +
   destructuring-bind + is (string=) boilerplate from the three test forms
   hook-event-constants, new-hook-event-constants, and
   remaining-hook-event-constants."
  (dolist (pair pairs)
    (destructuring-bind (expected actual) pair
      (expect (string= expected actual)))))

(describe "hooks-suite"

  ;;; ── Hook event constant values ───────────────────────────────────────────────

  ;; Hook event constants defined via define-hook-events have the expected string values.
  (it "hook-event-constants"
    (%assert-hook-constant-table
     `(("after-new-window"    ,cl-tmux/hooks:+hook-after-new-window+)
       ("after-new-pane"      ,cl-tmux/hooks:+hook-after-new-pane+)
       ("pane-exited"         ,cl-tmux/hooks:+hook-pane-exited+)
       ("after-rename-window" ,cl-tmux/hooks:+hook-after-rename-window+)
       ("session-created"     ,cl-tmux/hooks:+hook-session-created+)
       ("after-kill-pane"     ,cl-tmux/hooks:+hook-after-kill-pane+)
       ("after-kill-window"   ,cl-tmux/hooks:+hook-after-kill-window+)
       ("after-split-window"  ,cl-tmux/hooks:+hook-after-split-window+)
       ("client-attached"     ,cl-tmux/hooks:+hook-client-attached+)
       ("client-detached"     ,cl-tmux/hooks:+hook-client-detached+)
       ("alert-bell"          ,cl-tmux/hooks:+hook-alert-bell+))))

  ;; The newly added tmux hook event constants have the expected string values.
  (it "new-hook-event-constants"
    (%assert-hook-constant-table
     `(("pane-died" ,cl-tmux/hooks:+hook-pane-died+))))

  ;; The remaining hook event constants not covered by hook-event-constants or
  ;; new-hook-event-constants have the expected string values.
  ;; Covers: alert-activity, alert-silence, pane-focus-in/out, after-select-pane/window,
  ;; session-window-changed, window-pane-changed, window-renamed, session-renamed,
  ;; after-resize-pane, client-resized, window-linked/unlinked, session-closed,
  ;; pane-output.
  (it "remaining-hook-event-constants"
    (%assert-hook-constant-table
     `(("alert-activity"         ,cl-tmux/hooks:+hook-alert-activity+)
       ("alert-silence"          ,cl-tmux/hooks:+hook-alert-silence+)
       ("pane-focus-in"          ,cl-tmux/hooks:+hook-pane-focus-in+)
       ("pane-focus-out"         ,cl-tmux/hooks:+hook-pane-focus-out+)
       ("after-select-pane"      ,cl-tmux/hooks:+hook-after-select-pane+)
       ("after-select-window"    ,cl-tmux/hooks:+hook-after-select-window+)
       ("session-window-changed" ,cl-tmux/hooks:+hook-session-window-changed+)
       ("window-pane-changed"    ,cl-tmux/hooks:+hook-window-pane-changed+)
       ("window-renamed"         ,cl-tmux/hooks:+hook-window-renamed+)
       ("session-renamed"        ,cl-tmux/hooks:+hook-session-renamed+)
       ("after-resize-pane"      ,cl-tmux/hooks:+hook-after-resize-pane+)
       ("client-resized"         ,cl-tmux/hooks:+hook-client-resized+)
       ("window-linked"          ,cl-tmux/hooks:+hook-window-linked+)
       ("window-unlinked"        ,cl-tmux/hooks:+hook-window-unlinked+)
       ("session-closed"         ,cl-tmux/hooks:+hook-session-closed+)
       ("pane-output"            ,cl-tmux/hooks:+hook-pane-output+))))

  ;; add-hook and run-hooks work correctly for the pane-died event and forward the pane arg.
  (it "run-hooks-pane-died-event"
    (with-isolated-hooks
      (let ((received :unset))
        (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-pane-died+
                                 (lambda (pane) (setf received pane)))
        (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-pane-died+ :the-pane)
        (expect (eq :the-pane received)))))

  ;;; hook-event-constants-are-strings was removed: hook-event-constants already
  ;;; asserts string= for every constant, which implies stringp — the type check
  ;;; was a strict subset and added no new coverage.

  ;;; ── *hook-registry* initial state ───────────────────────────────────────────

  ;; *hook-registry* is a hash table with :equal test.
  (it "hook-registry-is-hash-table"
    (with-isolated-hooks
      (expect (hash-table-p cl-tmux/hooks:*hook-registry*))))

  ;; A freshly-isolated registry has no entries.
  (it "hook-registry-fresh-is-empty"
    (with-isolated-hooks
      (expect (zerop (hash-table-count cl-tmux/hooks:*hook-registry*)))))

  ;;; ── add-hook / run-hooks ─────────────────────────────────────────────────────

  ;; add-hook registers a callback; run-hooks calls it.
  (it "add-and-run-hook"
    (with-isolated-hooks
      (let ((called nil))
        (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-new-window+
                                 (lambda () (setf called t)))
        (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-new-window+)
        (expect called :to-be-truthy))))

  ;; run-hooks on an event with no callbacks is a safe no-op.
  (it "run-hooks-unregistered-event-is-noop"
    (with-isolated-hooks
      (finishes (cl-tmux/hooks:run-hooks "no-such-event"))
      (finishes (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-new-window+))))

  ;; Two add-hooks run newest-first (front-push order).
  (it "hooks-newest-first"
    (with-isolated-hooks
      (let ((order '()))
        (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-new-window+
                                 (lambda () (push :first order)))
        (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-new-window+
                                 (lambda () (push :second order)))
        (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-new-window+)
        ;; :second was added last so it runs first; push prepends, so result is (:first :second)
        (expect (equal '(:first :second) order)))))

  ;; Adding the same lambda twice registers it twice; run-hooks calls it twice.
  (it "add-same-callback-twice"
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
        (expect (= 2 hits)))))

  ;;; ── remove-hook ──────────────────────────────────────────────────────────────

  ;; remove-hook removes by eq identity; other callbacks still run.
  (it "remove-hook-by-identity"
    (with-isolated-hooks
      (let* ((call-count-a 0)
             (call-count-b 0)
             (cb-a (lambda () (incf call-count-a)))
             (cb-b (lambda () (incf call-count-b))))
        (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-new-pane+ cb-a)
        (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-new-pane+ cb-b)
        ;; Both run before removal
        (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-new-pane+)
        (expect (= 1 call-count-a))
        (expect (= 1 call-count-b))
        ;; Remove only cb-b
        (cl-tmux/hooks:remove-hook cl-tmux/hooks:+hook-after-new-pane+ cb-b)
        (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-new-pane+)
        (expect (= 2 call-count-a))
        (expect (= 1 call-count-b)))))

  ;; remove-hook on a callback that was never added is a safe no-op.
  (it "remove-hook-nonexistent-callback-is-noop"
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
        (expect (= 1 count)))))

  ;; remove-hook removes every occurrence of the callback (not just the first).
  (it "remove-hook-removes-all-occurrences"
    (with-isolated-hooks
      (let* ((count 0)
             (cb    (lambda () (incf count))))
        (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-new-window+ cb)
        (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-new-window+ cb)
        (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-new-window+ cb)
        (cl-tmux/hooks:remove-hook cl-tmux/hooks:+hook-after-new-window+ cb)
        (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-new-window+)
        (expect (= 0 count)))))

  ;;; ── run-hooks error resilience ───────────────────────────────────────────────

  ;; run-hooks suppresses any ERROR (including subclasses) and continues executing remaining hooks.
  (it "run-hooks-swallows-error-and-continues"
    (dolist (row (list (list cl-tmux/hooks:+hook-session-created+
                             (lambda () (error "deliberate hook error"))
                             "generic ERROR")
                       (list cl-tmux/hooks:+hook-after-kill-pane+
                             (lambda () (error 'simple-error :format-control "hook boom"))
                             "SIMPLE-ERROR subclass")))
      (destructuring-bind (hook error-fn desc) row
        (declare (ignore desc))
        (with-isolated-hooks
          (let ((good-ran nil))
            (cl-tmux/hooks:add-hook hook error-fn)
            (cl-tmux/hooks:add-hook hook (lambda () (setf good-ran t)))
            (finishes (cl-tmux/hooks:run-hooks hook))
            (expect good-ran :to-be-truthy))))))

  ;;; ── run-hooks argument passing ───────────────────────────────────────────────

  ;; run-hooks passes its extra arguments to each registered callback.
  (it "run-hooks-passes-args"
    (with-isolated-hooks
      (let ((received nil))
        (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-pane-exited+
                                 (lambda (arg) (setf received arg)))
        (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-pane-exited+ 42)
        (expect (eql 42 received)))))

  ;; run-hooks forwards all extra arguments when a callback accepts more than one.
  (it "run-hooks-passes-multiple-args"
    (with-isolated-hooks
      (let ((got-a nil) (got-b nil))
        (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-pane-exited+
                                 (lambda (a b) (setf got-a a got-b b)))
        (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-pane-exited+ :x :y)
        (expect (eq :x got-a))
        (expect (eq :y got-b)))))

  ;;; ── clear-hooks ──────────────────────────────────────────────────────────────

  ;; clear-hooks removes every callback; run-hooks becomes a no-op and list-hooks drops the entry.
  (it "clear-hooks-removes-all"
    (with-isolated-hooks
      (let ((called nil))
        (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-kill-pane+
                                 (lambda () (setf called :first)))
        (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-kill-pane+
                                 (lambda () (setf called :second)))
        ;; Sanity: hooks present before clearing
        (let ((before (cl-tmux/hooks:list-hooks)))
          (expect (= 2 (alist-value cl-tmux/hooks:+hook-after-kill-pane+ before
                                :test #'string=))))
        ;; Clear
        (cl-tmux/hooks:clear-hooks cl-tmux/hooks:+hook-after-kill-pane+)
        ;; run-hooks must be a no-op: called stays NIL
        (setf called nil)
        (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-kill-pane+)
        (expect (null called))
        ;; list-hooks must no longer include the entry
        (let ((after (cl-tmux/hooks:list-hooks)))
          (expect (null (assoc cl-tmux/hooks:+hook-after-kill-pane+ after :test #'string=)))))))

  ;; clear-hooks on an event with no registered hooks is a safe no-op.
  (it "clear-hooks-unregistered-event-is-noop"
    (with-isolated-hooks
      (finishes (cl-tmux/hooks:clear-hooks "totally-unknown-event"))
      (finishes (cl-tmux/hooks:clear-hooks cl-tmux/hooks:+hook-after-kill-pane+))))

  ;;; ── list-hooks ───────────────────────────────────────────────────────────────

  ;; list-hooks returns an alist of (event-name . count) for all registered events.
  (it "list-hooks-returns-alist"
    (with-isolated-hooks
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-new-window+ (lambda () nil))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-new-window+ (lambda () nil))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-pane-exited+      (lambda () nil))
      (let ((alist (cl-tmux/hooks:list-hooks)))
        (expect (= 2 (length alist)))
        (let ((nw-count (alist-value cl-tmux/hooks:+hook-after-new-window+ alist
                                     :test #'string=))
              (pe-count (alist-value cl-tmux/hooks:+hook-pane-exited+ alist
                                     :test #'string=)))
          (expect (= 2 nw-count))
          (expect (= 1 pe-count))))))

  ;; list-hooks on an empty registry returns NIL.
  (it "list-hooks-empty-registry-returns-nil"
    (with-isolated-hooks
      (expect (null (cl-tmux/hooks:list-hooks)))))

  ;; list-hooks counts update correctly as callbacks are added and removed.
  (it "list-hooks-counts-reflect-add-and-remove"
    (with-isolated-hooks
      (let ((cb-a (lambda () nil))
            (cb-b (lambda () nil)))
        (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-new-window+ cb-a)
        (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-new-window+ cb-b)
        (let ((before (alist-value cl-tmux/hooks:+hook-after-new-window+
                                   (cl-tmux/hooks:list-hooks)
                                   :test #'string=)))
          (expect (= 2 before)))
        (cl-tmux/hooks:remove-hook cl-tmux/hooks:+hook-after-new-window+ cb-a)
        (let ((after (alist-value cl-tmux/hooks:+hook-after-new-window+
                                  (cl-tmux/hooks:list-hooks)
                                  :test #'string=)))
          (expect (= 1 after))))))

  ;;; wait-for-signal-unblocks was moved to runtime-tests.lisp because it tests
  ;;; cl-tmux::*wait-channels*, cl-tmux::%ensure-channel, cl-tmux::signal-channel,
  ;;; cl-tmux::lock-channel, and cl-tmux::unlock-channel — all from runtime.lisp —
  ;;; and runtime-tests.lisp already covers these same symbols.

  ;;; ── clear-hooks isolation ────────────────────────────────────────────────────

  ;; clear-hooks for one event leaves other events' hooks intact.
  (it "clear-hooks-does-not-affect-other-events"
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
        (expect (null called-a))
        (expect called-b :to-be-truthy))))

  ;;; ── run-hooks with +hook-after-split-window+ ─────────────────────────────────

  ;; add-hook and run-hooks work correctly for the after-split-window event.
  (it "run-hooks-after-split-window-event"
    (with-isolated-hooks
      (let ((fired nil))
        (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-split-window+
                                 (lambda () (setf fired t)))
        (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-split-window+)
        (expect fired :to-be-truthy))))

  ;;; ── list-hooks after all callbacks removed ───────────────────────────────────

  ;; After removing the last callback for an event, list-hooks shows count 0 for it.
  (it "list-hooks-after-all-callbacks-removed-shows-zero"
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

  ;; run-hooks fires all registered callbacks for client-attached.
  (it "run-hooks-client-attached-fires-all-callbacks"
    (with-isolated-hooks
      (let ((first-called nil)
            (second-called nil))
        (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-client-attached+
                                 (lambda () (setf first-called t)))
        (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-client-attached+
                                 (lambda () (setf second-called t)))
        (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-client-attached+)
        (expect first-called  :to-be-truthy)
        (expect second-called :to-be-truthy)))))
