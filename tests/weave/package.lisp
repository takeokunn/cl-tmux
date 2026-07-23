;;;; Package for the cl-weave regression suite over the reasoning read-model.
;;;;
;;;; This suite is the "advanced usage" of both dogfooded libraries at once:
;;;;   * cl-weave  — describe/it, custom matchers, and around-each fixtures
;;;;   * cl-prolog — queried through the reasoning API and, for raw queries,
;;;;                 through cl-prolog's own cl-weave bridge (deftest-queries).

(defpackage #:cl-tmux/weave-tests
  (:use #:cl #:cl-weave)
  ;; cl-weave shadows cl:describe; take cl-weave's.
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-tmux/reasoning
                ;; Projection + construction
                #:snapshot-key-bindings
                #:build-key-rulebase
                #:current-key-rulebase
                ;; Query helpers
                #:key-command
                #:keys-running
                #:repeatable-commands
                #:binding-conflicts
                #:shadowing-bindings
                #:unique-bindings
                #:explain-binding
                ;; Command-metadata read-model
                #:current-command-rulebase
                #:command-accepts-flag-p
                #:commands-with-flag
                #:flags-of-command
                #:scriptable-commands
                ;; Prolog query vocabulary (predicate symbols)
                #:binding
                #:conflict
                #:shadows-root
                #:unique-binding
                #:repeatable
                #:repeatable-command
                #:note
                #:command
                #:usage
                #:accepts-flag
                #:scriptable)
  ;; cl-prolog's own cl-weave helpers for asserting raw queries.
  (:import-from #:cl-prolog/weave
                #:assert-query
                #:deftest-queries)
  (:export #:run-weave-tests))
