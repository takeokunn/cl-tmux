;;;; Package for the cl-weave suite over the cl-dataflow copy-mode lifecycle
;;;; read-model (src/dataflow/), mirroring tests/weave/package.lisp for the
;;;; cl-prolog reasoning read-model.

(defpackage #:cl-tmux/dataflow-tests
  (:use #:cl #:cl-weave)
  ;; cl-weave shadows cl:describe; take cl-weave's.
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-tmux/dataflow
                #:copy-mode-lifecycle-machine
                #:screen-copy-mode-lifecycle-state
                #:copy-mode-lifecycle-states
                #:copy-mode-lifecycle-events
                #:copy-mode-lifecycle-terminal-states
                #:copy-mode-lifecycle-unreachable-states
                #:copy-mode-lifecycle-deterministic-p
                #:copy-mode-lifecycle->dot
                #:copy-mode-lifecycle->mermaid)
  (:import-from #:cl-tmux/terminal
                #:make-screen
                #:screen-copy-mode-p
                #:screen-copy-selecting)
  (:export #:run-dataflow-tests))
