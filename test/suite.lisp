(in-package #:cl-tmux/test)

;;;; Top-level test runner.  Aggregates the per-area suites and exposes a
;;;; single entry point used by `(asdf:test-system :cl-tmux)` and the Nix
;;;; `checks` derivation.

(def-suite cl-tmux-suite :description "All cl-tmux tests")

(defparameter *all-suites*
  '(terminal-suite layout-tree-suite layout-geometry-suite model-suite
    format-suite target-suite buffer-suite control-suite options-suite
    hooks-suite config-suite config-directives-suite renderer-suite
    dispatch-suite events-suite mouse-suite commands-suite
    overlay-suite prompt-suite protocol-suite transport-suite
    net-suite server-suite server-multi-suite pty-ffi-suite pty-rawmode-suite
    pty-suite input-suite runtime-suite client-suite
    main-suite advanced-suite)
  "Every per-area suite, run in this order by RUN-TESTS.")

(defun run-tests ()
  "Run every suite SEQUENTIALLY in the calling (main) thread, collect the
results, explain them together, and signal an error (non-zero exit under Nix)
on any failure.

Sequential execution is REQUIRED — not a performance choice.  Many integration
suites fork a PTY via sb-posix:fork, which SBCL refuses with \"Cannot fork with
multiple threads running\" whenever ANY other thread is alive.  A prior version
ran the suites across eight bordeaux-threads workers for speed; that left eight
threads alive and made every forkpty in the MODEL / PTY / SERVER / DISPATCH
suites fail (28 errors) or skip (21).  STOP-CL-TMUX-THREADS is called after each
suite to join any PTY-reader / status-timer thread a test spawned (e.g. via
new-session or a dispatched :split), so the next suite again starts
single-threaded."
  (let ((all-results '()))
    (dolist (suite *all-suites*)
      (setf all-results (append all-results (run suite)))
      (stop-cl-tmux-threads))
    (explain! all-results)
    (unless (results-status all-results)
      (error "cl-tmux test suite failed"))
    t))
