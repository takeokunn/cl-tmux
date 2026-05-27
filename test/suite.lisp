(in-package #:cl-tmux/test)

;;;; Top-level test runner.  Aggregates the per-area suites and exposes a
;;;; single entry point used by `(asdf:test-system :cl-tmux)` and the Nix
;;;; `checks` derivation.

(def-suite cl-tmux-suite :description "All cl-tmux tests")

(defun run-tests ()
  "Run every suite.  Signals an error (non-zero exit under Nix) on failure."
  (let ((results (append
                  (run 'terminal-suite)
                  (run 'layout-suite)
                  (run 'pty-suite))))
    (explain! results)
    (unless (results-status results)
      (error "cl-tmux test suite failed"))
    t))
