(in-package #:cl-tmux/test)

;;;; reader CPS state machine contracts

(describe "runtime-suite"

  ;; reader-remain-on-exit-state returns NIL immediately when *running* is NIL.
  (it "reader-remain-on-exit-state-returns-nil-when-not-running"
    (with-dead-pane (pane)
      (let ((cl-tmux::*running* nil))
        (expect (null (cl-tmux::reader-remain-on-exit-state pane))))))

  ;; All CPS reader state machine functions are defined.
  (it "reader-state-functions-are-all-fbound"
    (dolist (sym '(cl-tmux::reader-idle-state
                   cl-tmux::reader-reading-state
                   cl-tmux::reader-remain-on-exit-state
                   cl-tmux::reader-eof-state
                   cl-tmux::%run-reader-states
                   cl-tmux::start-reader-thread
                   cl-tmux::install-sigwinch-handler
                   cl-tmux::start-status-timer))
      (expect (fboundp sym))))

  ;; %run-reader-states exits immediately when *running* is NIL, even
  ;; given a non-NIL initial state (loop while *running*).
  (it "run-reader-states-exits-when-running-nil"
    (with-dead-pane (pane)
      (let* ((cl-tmux::*running* nil)
             (boom (lambda (_p)
                     (declare (ignore _p))
                     (error "state function called despite *running*=NIL"))))
        (finishes (cl-tmux::%run-reader-states pane boom)
                  "%run-reader-states must exit immediately when *running* is NIL")))))
