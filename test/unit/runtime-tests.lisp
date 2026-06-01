(in-package #:cl-tmux/test)

;;;; Runtime state tests (src/runtime.lisp).
;;;; Tests: runtime-suite — global variables and their initial state.

(def-suite runtime-suite :description "Runtime state variables")
(in-suite runtime-suite)

(test runtime-globals-exist
  "*running*, *dirty*, *resize-pending*, *term-rows*, *term-cols* are all boundp."
  (is (boundp 'cl-tmux::*running*)        "*running* must be bound")
  (is (boundp 'cl-tmux::*dirty*)          "*dirty* must be bound")
  (is (boundp 'cl-tmux::*resize-pending*) "*resize-pending* must be bound")
  (is (integerp cl-tmux::*term-rows*)     "*term-rows* must be an integer")
  (is (integerp cl-tmux::*term-cols*)     "*term-cols* must be an integer"))

(test pane-reader-loop-is-fbound
  "%pane-reader-loop is a defined function (data/logic separation from start-reader-thread)."
  (is (fboundp 'cl-tmux::%pane-reader-loop)
      "%pane-reader-loop must be fbound"))

(test pane-reader-loop-exits-when-running-nil
  "%pane-reader-loop exits immediately when *running* is NIL.
   This verifies the loop sentinel is checked without needing a real PTY."
  (let ((cl-tmux::*running* nil)
        (cl-tmux::*dirty*   nil)
        (pane (make-pane :id 1 :fd -1 :pid -1 :screen (make-screen 5 3))))
    ;; With *running* = NIL, the loop body is never entered.
    ;; %pane-reader-loop should return without error.
    (finishes (cl-tmux::%pane-reader-loop pane))
    ;; *dirty* must not have been set (no data was read).
    (is-false cl-tmux::*dirty* "*dirty* must remain NIL when loop exits immediately")))
