(in-package #:cl-tmux/test)

;;;; Top-level test runner, on cl-weave.
;;;;
;;;; Every file registers its own top-level (describe ...) block directly with
;;;; cl-weave.  RUN-TESTS walks the whole suite tree with cl-weave's runner in
;;;; single-worker (sequential) mode and fails on any failure.

;;; Sequential execution is REQUIRED — not a performance choice.  Integration
;;; suites share global session, runtime, socket, and PTY state; running them
;;; concurrently would leave reader/status/background threads from one test
;;; visible to another.  Each test that spawns a background thread/server
;;; joins it itself (see WITH-LOOP-STATE in helpers-loop-fixtures.lisp), so
;;; isolation does not depend on suite boundaries or execution order.

(defun run-tests ()
  "Run every registered suite SEQUENTIALLY (single worker) through cl-weave,
report the results, and signal an error (non-zero exit under Nix) on any
failure."
  (unless (cl-weave:run-all :reporter :spec :max-workers 1)
    (error "cl-tmux test suite failed"))
  t)
