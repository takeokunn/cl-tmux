;;;; Copy-mode lifecycle: the cl-dataflow state machine and read-model.
;;;;
;;;; States mirror the booleans commands-copy-mode.lisp already mutates:
;;;;   "normal"     -- (screen-copy-mode-p screen) is NIL
;;;;   "copy-mode"  -- copy-mode-p T, copy-selecting NIL
;;;;   "selecting"  -- copy-mode-p T, copy-selecting T
;;;;
;;;; Events name the commands that drive each transition, taken directly from
;;;; the Prolog-style rule table documented above copy-mode-enter in
;;;; commands-copy-mode.lisp:
;;;;   copy_mode(enter, Screen)           :- ...
;;;;   copy_mode(exit, Screen)            :- ...
;;;;   copy_mode(begin_selection, Screen) :- ...
;;;;   copy_mode(cancel, Screen)          :- ...
;;;;   copy_mode(yank, Screen)            :- ..., cancel, exit.

(in-package #:cl-tmux/dataflow)

(defun copy-mode-lifecycle-machine ()
  "Return a fresh cl-dataflow state machine for the copy-mode lifecycle,
   starting in the \"normal\" (not-in-copy-mode) state."
  (cl-dataflow:make-state-machine
   :state "normal"
   :transitions
   (list
    (cl-dataflow:make-transition "normal"    "enter"            "copy-mode")
    (cl-dataflow:make-transition "copy-mode" "exit"              "normal")
    (cl-dataflow:make-transition "copy-mode" "begin-selection"   "selecting")
    (cl-dataflow:make-transition "selecting" "cancel-selection"  "copy-mode")
    (cl-dataflow:make-transition "selecting" "exit"              "normal")
    (cl-dataflow:make-transition "selecting" "yank"              "normal"))))

(defun screen-copy-mode-lifecycle-state (screen)
  "Return SCREEN's current copy-mode lifecycle state as one of the
   copy-mode-lifecycle-machine state strings (\"normal\" / \"copy-mode\" /
   \"selecting\"), read directly off SCREEN's own copy-mode-p / copy-selecting
   slots.  Pure: never mutates SCREEN."
  (cond
    ((not (cl-tmux/terminal:screen-copy-mode-p screen)) "normal")
    ((cl-tmux/terminal:screen-copy-selecting screen)    "selecting")
    (t                                                  "copy-mode")))

(defun copy-mode-lifecycle-states ()
  "Return every state name in the copy-mode lifecycle machine."
  (cl-dataflow:state-machine-states (copy-mode-lifecycle-machine)))

(defun copy-mode-lifecycle-events ()
  "Return every event name the copy-mode lifecycle machine accepts."
  (cl-dataflow:state-machine-event-types (copy-mode-lifecycle-machine)))

(defun copy-mode-lifecycle-terminal-states ()
  "Return the copy-mode lifecycle states with no outgoing transition.
   Expected to be empty: every real copy-mode state can always reach
   \"normal\" again (via exit, or yank from \"selecting\")."
  (cl-dataflow:state-machine-terminal-states (copy-mode-lifecycle-machine)))

(defun copy-mode-lifecycle-unreachable-states ()
  "Return copy-mode lifecycle states unreachable from \"normal\".
   Expected to be empty; a non-empty result would mean a state was added
   without a transition into it from the rest of the graph."
  (cl-dataflow:state-machine-unreachable-states (copy-mode-lifecycle-machine)))

(defun copy-mode-lifecycle-deterministic-p ()
  "True when every (state, event) pair in the copy-mode lifecycle machine has
   at most one transition -- i.e. dispatch is never ambiguous."
  (cl-dataflow:state-machine-deterministic-p (copy-mode-lifecycle-machine)))

(defun copy-mode-lifecycle->dot ()
  "Render the copy-mode lifecycle machine as Graphviz DOT, for `dot -Tsvg`
   or documentation."
  (cl-dataflow:state-machine->dot (copy-mode-lifecycle-machine) :name "copy_mode"))

(defun copy-mode-lifecycle->mermaid ()
  "Render the copy-mode lifecycle machine as a Mermaid stateDiagram block,
   for embedding directly in Markdown docs."
  (cl-dataflow:state-machine->mermaid (copy-mode-lifecycle-machine)))
