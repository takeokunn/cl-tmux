(in-package #:cl-tmux/test)

;;;; Top-level test runner.  Aggregates the per-area suites and exposes a
;;;; single entry point used by `(asdf:test-system :cl-tmux)` and the Nix
;;;; `checks` derivation.

(def-suite cl-tmux-suite :description "All cl-tmux tests")

(defun run-tests ()
  "Run every suite in parallel using bordeaux-threads.
Each group of ~4 suites runs in its own thread; results are collected
into a shared list protected by a lock, then explained together.
Signals an error (non-zero exit under Nix) on any failure."
  (let* ((all-results '())
         (lock (bordeaux-threads:make-lock "run-tests-lock"))
         (suite-groups
           '((terminal-suite layout-tree-suite layout-geometry-suite model-suite)
             (format-suite target-suite buffer-suite options-suite)
             (hooks-suite config-suite config-directives-suite renderer-suite)
             (dispatch-suite events-suite mouse-suite commands-suite)
             (overlay-suite prompt-suite protocol-suite transport-suite)
             (net-suite server-suite pty-ffi-suite pty-rawmode-suite)
             (pty-suite input-suite runtime-suite client-suite)
             (main-suite advanced-suite)))
         (threads
           (mapcar (lambda (group)
                     (bordeaux-threads:make-thread
                      (lambda ()
                        (let ((group-results
                               (reduce #'append
                                       (mapcar (lambda (suite) (run suite))
                                               group)
                                       :initial-value '())))
                          (bordeaux-threads:with-lock-held (lock)
                            (setf all-results
                                  (append all-results group-results)))))
                      :name (format nil "test-group-~A" (first group))))
                   suite-groups)))
    (dolist (thread threads)
      (bordeaux-threads:join-thread thread))
    (explain! all-results)
    (unless (results-status all-results)
      (error "cl-tmux test suite failed"))
    t))
