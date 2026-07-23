;;;; Package for the cl-dataflow-backed copy-mode lifecycle read-model.
;;;;
;;;; This subsystem is an *additive* introspection layer built on the
;;;; dependency-free `cl-dataflow' state-machine primitives (which in turn sit
;;;; on `cl-prolog', already a core dependency -- see src/reasoning/).  It
;;;; makes the copy-mode lifecycle -- already documented as a Prolog-style
;;;; rule table at the top of commands-copy-mode.lisp -- an inspectable,
;;;; exportable (DOT/Mermaid) state machine, and offers a pure function that
;;;; reads a SCREEN's current lifecycle state without touching it.
;;;;
;;;; Cold-path only, mirroring src/reasoning/: never called from the hot
;;;; per-keystroke dispatch loop.  copy-mode-enter/-exit/-begin-selection/
;;;; -cancel-selection/-yank (commands-copy-mode*.lisp) remain the imperative
;;;; source of truth; this module only mirrors their documented transitions.
;;;;
;;;; Scope note: rectangle-select (copy-mode-rectangle-on/off) is an
;;;; orthogonal flag on SCREEN, settable independently of whether a selection
;;;; is active, not a nested lifecycle state -- modeling it as a fourth state
;;;; here would misrepresent the real transition graph, so it is left out.

(defpackage #:cl-tmux/dataflow
  (:use #:cl)
  (:documentation
   "A cl-dataflow read-model over cl-tmux's copy-mode lifecycle: a declarative
    state machine plus DOT/Mermaid export and a pure screen -> state reader.")
  (:export
   #:copy-mode-lifecycle-machine
   #:screen-copy-mode-lifecycle-state
   #:copy-mode-lifecycle-states
   #:copy-mode-lifecycle-events
   #:copy-mode-lifecycle-terminal-states
   #:copy-mode-lifecycle-unreachable-states
   #:copy-mode-lifecycle-deterministic-p
   #:copy-mode-lifecycle->dot
   #:copy-mode-lifecycle->mermaid))
