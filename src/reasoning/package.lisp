;;;; Package for the Prolog-backed key-binding reasoning read-model.
;;;;
;;;; This subsystem is an *additive* introspection layer built on the
;;;; dependency-free `cl-prolog' engine.  It projects cl-tmux's live
;;;; key-table store into a Prolog rulebase and answers relational
;;;; questions about it that the imperative store cannot express directly
;;;; (reverse lookup, cross-table conflicts, repeatable-command inference).
;;;;
;;;; It deliberately lives in its own ASDF system (`cl-tmux/reasoning',
;;;; depending on `cl-tmux' + `cl-prolog') so the core binary and its
;;;; FiveAM suite carry no new dependency.  The core is untouched.
;;;;
;;;; Only `\=' is imported from cl-prolog: builtin goals dispatch on symbol
;;;; identity, so inequality goals in clause data must reference the engine's
;;;; own symbol, whereas the domain predicates below are ordinary symbols
;;;; owned by this package.

(defpackage #:cl-tmux/reasoning
  (:use #:cl)
  (:import-from #:cl-prolog #:|\\=|)
  (:documentation
   "A cl-prolog read-model over cl-tmux key tables: projection, a small
    rule set, and query helpers for binding introspection.")
  (:export
   ;; Projection + rulebase construction
   #:snapshot-key-bindings
   #:build-key-rulebase
   #:current-key-rulebase
   ;; Query helpers (return Lisp values, not raw solutions)
   #:key-command
   #:keys-running
   #:repeatable-commands
   #:binding-conflicts
   #:shadowing-bindings
   #:explain-binding
   ;; Command-metadata read-model (second cold-path domain)
   #:build-command-rulebase
   #:current-command-rulebase
   #:command-usage-facts
   #:command-accepts-flag-p
   #:commands-with-flag
   #:flags-of-command
   #:scriptable-commands
   ;; Prolog query vocabulary — the predicate symbols used inside the
   ;; rulebase, exported so callers (and tests) can pose raw queries.
   #:binding
   #:repeatable
   #:note
   #:conflict
   #:shadows-root
   #:repeatable-command
   #:command
   #:usage
   #:accepts-flag
   #:scriptable
   #:flag-shared))
