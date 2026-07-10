(in-package #:cl-tmux/test)

;;;; Top-level test runner.  Aggregates the per-area suites and exposes a
;;;; single entry point used by `(asdf:test-system :cl-tmux)` and the Nix
;;;; `checks` derivation.

(def-suite cl-tmux-suite :description "All cl-tmux tests")

(def-suite server-suite :description "Server registry and bootstrap behavior")

(defparameter *all-suites*
  '(terminal-suite layout-tree-suite layout-geometry-suite model-suite
    format-suite target-suite buffer-suite control-suite options-suite
    hooks-suite config-suite config-directives-suite renderer-suite
    dispatch-suite events-suite mouse-suite commands-suite
    overlay-suite prompt-suite protocol-suite transport-suite
    net-suite server-suite server-multi-suite pty-ffi-suite pty-rawmode-suite
    pty-unit-suite pty-suite input-suite runtime-suite client-suite
    main-suite advanced-suite)
  "Every per-area suite, run in this order by RUN-TESTS.")

(defun %collect-suite-test-names (suite-name table)
  "Record in TABLE the name of every test-case reachable from SUITE-NAME.
   A suite's TESTS slot holds a FiveAM TEST-BUNDLE whose %TESTS hash maps
   child names (tests and sub-suites) to their objects."
  (let ((obj (get-test suite-name)))
    (typecase obj
      (fiveam::test-suite
       (loop for child being the hash-keys of (fiveam::%tests (fiveam::tests obj))
             do (%collect-suite-test-names child table)))
      (fiveam::test-case
       (setf (gethash suite-name table) t)))))

(defun orphan-test-names ()
  "Names of registered test-cases NOT reachable from any suite in *ALL-SUITES*.
   A test lands here when its file forgets IN-SUITE (or names a suite that is
   not in the runner's list) — it compiles fine but silently never runs.
   RUN-TESTS fails on any orphan so coverage holes cannot creep in."
  (let ((reachable (make-hash-table :test #'eq)))
    (dolist (suite *all-suites*)
      (%collect-suite-test-names suite reachable))
    (loop for name in (test-names)
          for obj = (get-test name)
          when (and (typep obj 'fiveam::test-case)
                    (not (gethash name reachable)))
            collect name)))

(defun run-tests ()
  "Run every suite SEQUENTIALLY in the calling (main) thread, collect the
results, explain them together, and signal an error (non-zero exit under Nix)
on any failure or on orphan tests that no suite would ever run.

Sequential execution is REQUIRED — not a performance choice.  Integration suites
share global session, runtime, socket, and PTY state; running them concurrently
leaves reader/status/background threads from one suite visible to another.
STOP-CL-TMUX-THREADS is called after each suite to join any PTY-reader /
status-timer thread a test spawned (e.g. via new-session or a dispatched
:split), keeping the next suite deterministic."
  (let ((orphans (orphan-test-names)))
    (when orphans
      (error "~D test(s) are not reachable from *all-suites* and would never ~
              run — add IN-SUITE to their files: ~{~S~^, ~}"
             (length orphans) orphans)))
  (let ((all-results '()))
    (dolist (suite *all-suites*)
      (setf all-results (append all-results (run suite)))
      (stop-cl-tmux-threads))
    (explain! all-results)
    (unless (results-status all-results)
      (error "cl-tmux test suite failed"))
    t))
