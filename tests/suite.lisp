(in-package #:cl-tmux/test)

;;;; Top-level test runner, on cl-weave.
;;;;
;;;; Every suite registers through the FiveAM-surface shim (fiveam-compat.lisp)
;;;; into cl-weave's suite tree.  RUN-TESTS walks that tree with cl-weave's
;;;; runner in single-worker (sequential) mode and fails on any failure.

;; The umbrella suites other files attach to via `(in-suite …)`.
(def-suite cl-tmux-suite :description "All cl-tmux tests")
(def-suite server-suite :description "Server registry and bootstrap behavior")

;;; Sequential execution is REQUIRED — not a performance choice.  Integration
;;; suites share global session, runtime, socket, and PTY state; running them
;;; concurrently would leave reader/status/background threads from one test
;;; visible to another.  Background threads are joined at each top-level suite
;;; boundary by an after-all hook registered in fiveam-compat.lisp's
;;; ENSURE-SHIM-SUITE — the same granularity FiveAM's runner used, so a
;;; server/reader a test spawns survives into the next test of the same suite.

(defun run-tests ()
  "Run every registered suite SEQUENTIALLY (single worker) through cl-weave,
report the results, and signal an error (non-zero exit under Nix) on any
failure."
  (unless (cl-weave:run-all :reporter :spec :max-workers 1)
    (error "cl-tmux test suite failed"))
  t)
