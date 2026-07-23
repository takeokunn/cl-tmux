;;;; cl-weave specs for the cl-dataflow copy-mode lifecycle read-model.

(in-package #:cl-tmux/dataflow-tests)

(describe "copy-mode lifecycle state machine"

  (describe "structure"
    (it "starts in the normal state"
      (expect (cl-dataflow:state-machine-state (copy-mode-lifecycle-machine))
              :to-equal "normal"))

    (it "has exactly the three documented states"
      (expect (sort (copy-list (copy-mode-lifecycle-states)) #'string<)
              :to-equal (list "copy-mode" "normal" "selecting")))

    (it "has exactly the five documented events"
      (expect (sort (copy-list (copy-mode-lifecycle-events)) #'string<)
              :to-equal (list "begin-selection" "cancel-selection" "enter" "exit" "yank")))

    (it "has no terminal (dead-end) states -- every state can return to normal"
      (expect (copy-mode-lifecycle-terminal-states) :to-be-null))

    (it "has no states unreachable from normal"
      (expect (copy-mode-lifecycle-unreachable-states) :to-be-null))

    (it "is deterministic -- no (state, event) pair is ambiguous"
      (expect (copy-mode-lifecycle-deterministic-p) :to-be-truthy)))

  (describe "transitions"
    (it "enter moves normal -> copy-mode"
      (expect (cl-dataflow:state-machine-state
               (cl-dataflow:run-state-machine (copy-mode-lifecycle-machine) '("enter")))
              :to-equal "copy-mode"))

    (it "begin-selection moves copy-mode -> selecting"
      (expect (cl-dataflow:state-machine-state
               (cl-dataflow:run-state-machine (copy-mode-lifecycle-machine)
                                              '("enter" "begin-selection")))
              :to-equal "selecting"))

    (it "cancel-selection returns selecting -> copy-mode, not all the way to normal"
      (expect (cl-dataflow:state-machine-state
               (cl-dataflow:run-state-machine (copy-mode-lifecycle-machine)
                                              '("enter" "begin-selection" "cancel-selection")))
              :to-equal "copy-mode"))

    (it "yank returns selecting -> normal in one step (cancel + exit, per the Prolog rule)"
      (expect (cl-dataflow:state-machine-state
               (cl-dataflow:run-state-machine (copy-mode-lifecycle-machine)
                                              '("enter" "begin-selection" "yank")))
              :to-equal "normal"))

    (it "exit returns to normal from either copy-mode or selecting"
      (expect (cl-dataflow:state-machine-state
               (cl-dataflow:run-state-machine (copy-mode-lifecycle-machine) '("enter" "exit")))
              :to-equal "normal")
      (expect (cl-dataflow:state-machine-state
               (cl-dataflow:run-state-machine (copy-mode-lifecycle-machine)
                                              '("enter" "begin-selection" "exit")))
              :to-equal "normal")))

  (describe "screen-copy-mode-lifecycle-state (pure reader)"
    (it "reads \"normal\" off a fresh screen"
      (let ((screen (make-screen 80 24)))
        (expect (screen-copy-mode-lifecycle-state screen) :to-equal "normal")))

    (it "reads \"copy-mode\" once copy-mode-p is set without a selection"
      (let ((screen (make-screen 80 24)))
        (setf (screen-copy-mode-p screen) t)
        (expect (screen-copy-mode-lifecycle-state screen) :to-equal "copy-mode")))

    (it "reads \"selecting\" once a selection is also active"
      (let ((screen (make-screen 80 24)))
        (setf (screen-copy-mode-p screen) t
              (screen-copy-selecting screen) t)
        (expect (screen-copy-mode-lifecycle-state screen) :to-equal "selecting")))

    (it "never mutates the screen it reads"
      (let ((screen (make-screen 80 24)))
        (screen-copy-mode-lifecycle-state screen)
        (expect (screen-copy-mode-p screen) :to-be-falsy))))

  (describe "export"
    (it "renders non-empty Graphviz DOT" (expect (copy-mode-lifecycle->dot) :to-be-type-of 'string)
      (expect (copy-mode-lifecycle->dot) :to-contain "digraph"))

    (it "renders a non-empty Mermaid stateDiagram block"
      (expect (copy-mode-lifecycle->mermaid) :to-be-type-of 'string)
      (expect (copy-mode-lifecycle->mermaid) :to-contain "state"))))
