(in-package #:cl-tmux/test)

;;;; Hooks system tests (src/hooks.lisp).

(def-suite hooks-suite :description "Hooks registry: add, remove, run, clear, list")
(in-suite hooks-suite)

;;; All tests isolate themselves by rebinding *hook-registry* to a fresh table.

(test add-and-run-hook
  "add-hook registers a callback; run-hooks calls it."
  (let ((cl-tmux/hooks:*hook-registry* (make-hash-table :test #'equal)))
    (let ((called nil))
      (cl-tmux/hooks:add-hook "after-new-window" (lambda () (setf called t)))
      (cl-tmux/hooks:run-hooks "after-new-window")
      (is-true called "hook must be called after run-hooks"))))

(test hooks-receive-args
  "run-hooks passes its extra arguments to each registered hook."
  (let ((cl-tmux/hooks:*hook-registry* (make-hash-table :test #'equal)))
    (let ((received nil))
      (cl-tmux/hooks:add-hook "pane-exited"
                               (lambda (arg) (setf received arg)))
      (cl-tmux/hooks:run-hooks "pane-exited" 42)
      (is (eql 42 received) "hook must receive the argument passed to run-hooks"))))

(test remove-hook
  "add-hook then remove-hook: the hook is not called after removal."
  (let ((cl-tmux/hooks:*hook-registry* (make-hash-table :test #'equal)))
    (let* ((call-count 0)
           (cb (lambda () (incf call-count))))
      (cl-tmux/hooks:add-hook "after-new-pane" cb)
      (cl-tmux/hooks:run-hooks "after-new-pane")
      (is (= 1 call-count) "hook called once before removal")
      (cl-tmux/hooks:remove-hook "after-new-pane" cb)
      (cl-tmux/hooks:run-hooks "after-new-pane")
      (is (= 1 call-count) "hook must NOT be called after remove-hook"))))

(test run-hooks-ignores-errors
  "A hook that signals an error does not propagate to the caller."
  (let ((cl-tmux/hooks:*hook-registry* (make-hash-table :test #'equal)))
    (let ((second-called nil))
      (cl-tmux/hooks:add-hook "session-created"
                               (lambda () (setf second-called t)))
      (cl-tmux/hooks:add-hook "session-created"
                               (lambda () (error "deliberate hook error")))
      ;; The error from the first-to-run (newest) hook must be silently eaten.
      (is (not (nth-value 0 (ignore-errors
                               (cl-tmux/hooks:run-hooks "session-created"))))
          "run-hooks must not signal an error when a hook fails")
      ;; However, the older (second) hook must still have been called.
      (is-true second-called
               "subsequent hooks must run even after an earlier hook signals an error"))))

(test list-hooks
  "list-hooks returns an alist with the correct counts for each event."
  (let ((cl-tmux/hooks:*hook-registry* (make-hash-table :test #'equal)))
    (cl-tmux/hooks:add-hook "after-new-window" (lambda () nil))
    (cl-tmux/hooks:add-hook "after-new-window" (lambda () nil))
    (cl-tmux/hooks:add-hook "pane-exited"      (lambda () nil))
    (let ((alist (cl-tmux/hooks:list-hooks)))
      (is (= 2 (length alist)) "two distinct event names must appear")
      (let ((nw-count (cdr (assoc "after-new-window" alist :test #'string=)))
            (pe-count (cdr (assoc "pane-exited"      alist :test #'string=))))
        (is (= 2 nw-count) "after-new-window must show count 2")
        (is (= 1 pe-count) "pane-exited must show count 1")))))
