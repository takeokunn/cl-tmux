(in-package #:cl-tmux/test)

;;;; Dispatch session tests - server-lifecycle command-name reachability.

(in-suite dispatch-suite)

;;; -- Server-lifecycle command-name reachability ------------------------------
;;;
;;; Regression: kill-server / start-server / send-prefix all have working
;;; dispatch handlers (dispatch-handlers.lisp) but were absent from
;;; define-named-command-table, so %dispatch-named-command returned the
;;; :unknown-command sentinel and they could not be invoked from the `C-b :`
;;; prompt or control mode.  These assert the name -> keyword wiring exists.

(test named-commands-server-lifecycle-reachable
  "kill-server / start-server / send-prefix are reachable by command name."
  (let ((cl-tmux::*running* t)
        (cl-tmux::*server-sessions* nil)
        (*overlay* nil))
    (with-empty-session (s)
      ;; start-server: recognised -> no-op overlay, NOT the unknown sentinel.
      (is (not (eq :unknown-command
                   (cl-tmux::%dispatch-named-command s "start-server")))
          "start-server must be a recognised command name")
      ;; send-prefix: recognised; an empty session has no active pane -> safe no-op.
      (is (not (eq :unknown-command
                   (cl-tmux::%dispatch-named-command s "send-prefix")))
          "send-prefix must be a recognised command name")
      ;; kill-server: recognised -> dispatches :kill-server, returns :quit and
      ;; clears *running*.
      (is (eq :quit (cl-tmux::%dispatch-named-command s "kill-server"))
          "kill-server must dispatch to :kill-server (returns :quit)")
      (is (null cl-tmux::*running*)
          "kill-server clears *running*")
      ;; Control: a genuinely unknown name still returns the :unknown-command
      ;; sentinel, confirming the assertions above are meaningful.
      (is (eq :unknown-command
              (cl-tmux::%dispatch-named-command s "definitely-not-a-command-xyz"))
          "unknown names still return :unknown-command"))))
