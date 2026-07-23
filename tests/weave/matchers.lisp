;;;; Custom cl-weave matchers for the reasoning read-model.
;;;;
;;;; These are domain-specific matchers registered with `cl-weave:defmatcher'.
;;;; A matcher receives (ACTUAL EXPECTED) where EXPECTED is the list of args
;;;; that followed the matcher keyword, and returns
;;;;   (values PASS-P REPORTED-ACTUAL REPORTED-EXPECTED).
;;;; They compose with `:not' automatically.

(in-package #:cl-tmux/weave-tests)

(cl-weave:defmatcher :to-resolve-to (actual expected)
  "Passes when a (RULEBASE TABLE KEY) triple resolves to the expected command.

Usage: (expect (list rulebase table key) :to-resolve-to command)."
  (destructuring-bind (rulebase table key) actual
    (destructuring-bind (command) expected
      (multiple-value-bind (resolved found) (key-command rulebase table key)
        (values (and found (equal resolved command))
                (list :resolved resolved :found found)
                (list :command command))))))

(cl-weave:defmatcher :to-be-unbound (actual expected)
  "Passes when a (RULEBASE TABLE KEY) triple has no binding.

Usage: (expect (list rulebase table key) :to-be-unbound)."
  (declare (ignore expected))
  (destructuring-bind (rulebase table key) actual
    (multiple-value-bind (resolved found) (key-command rulebase table key)
      (values (not found)
              (list :resolved resolved :found found)
              :unbound))))

(cl-weave:defmatcher :to-prove (actual expected)
  "Passes when the Prolog GOAL proves against the rulebase ACTUAL.

Usage: (expect rulebase :to-prove '(binding \"prefix\" #\\c :new-window))."
  (destructuring-bind (goal) expected
    (values (cl-prolog:prolog-succeeds-p actual goal)
            (list :rulebase :opaque)
            (list :goal goal))))

(cl-weave:defmatcher :to-run-command (actual expected)
  "Passes when COMMAND is bound to at least one (TABLE . KEY) in the rulebase.

Usage: (expect rulebase :to-run-command command)."
  (destructuring-bind (command) expected
    (let ((locations (keys-running actual command)))
      (values (and locations t)
              (list :locations locations)
              (list :command command)))))
