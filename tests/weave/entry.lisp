;;;; Entry point for the reasoning cl-weave suite.
;;;;
;;;; The suites are registered as a side effect of loading the spec files
;;;; above (the top-level describe / deftest-queries forms run at load time),
;;;; so the runner just walks the global registry.  `run-all' returns T only
;;;; when every event passed; the ASDF test-op turns a NIL into an error.

(in-package #:cl-tmux/weave-tests)

(defun run-weave-tests (&key (reporter :spec))
  "Run the cl-tmux reasoning cl-weave suite; return T on success."
  (cl-weave:run-all :reporter reporter))
