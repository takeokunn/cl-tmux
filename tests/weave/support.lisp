;;;; Shared support for the reasoning cl-weave suite.
;;;;
;;;; `fresh-default-rulebase' isolates the key-table store the same way the
;;;; FiveAM suite's WITH-ISOLATED-CONFIG does — a fresh `*key-tables*' rebound
;;;; only for the duration of the build — then projects the standard default
;;;; bindings into a self-contained rulebase.  The rulebase is plain data, so
;;;; it outlives the dynamic binding and never leaks store state between tests.

(in-package #:cl-tmux/weave-tests)

(defun fresh-default-rulebase ()
  "Return a reasoning rulebase for cl-tmux's standard default key bindings.

Runs against a private `*key-tables*' so the global store is untouched."
  (let ((cl-tmux/config:*key-tables* (make-hash-table :test #'equal)))
    (cl-tmux/config:initialize-default-key-tables)
    (cl-tmux::install-extended-key-bindings)
    (current-key-rulebase)))

(defun fresh-default-snapshot ()
  "Return the binding-plist snapshot for the standard default key bindings."
  (let ((cl-tmux/config:*key-tables* (make-hash-table :test #'equal)))
    (cl-tmux/config:initialize-default-key-tables)
    (cl-tmux::install-extended-key-bindings)
    (snapshot-key-bindings)))

;;; The suite binds *RULEBASE* per test through an around-each fixture (see
;;; key-reasoning-tests.lisp) so each `it' sees an independent rulebase.
(defvar *rulebase* nil
  "The per-test reasoning rulebase, established by the around-each fixture.")
