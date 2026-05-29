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
                  (run 'model-suite)
                  (run 'config-suite)
                  (run 'renderer-suite)
                  (run 'events-suite)
                  (run 'commands-suite)
                  (run 'prompt-suite)
                  (run 'protocol-suite)
                  (run 'transport-suite)
                  (run 'net-suite)
                  (run 'server-suite)
                  (run 'pty-suite)
                  (run 'input-suite)
                  (run 'main-suite))))
    (explain! results)
    (unless (results-status results)
      (error "cl-tmux test suite failed"))
    t))
