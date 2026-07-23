(in-package #:cl-tmux/test)

;;;; Dispatch session tests - server-lifecycle command-name reachability.

(describe "dispatch-suite"

  ;; -- Server-lifecycle command-name reachability ------------------------------
  ;;
  ;; Regression: kill-server / start-server / send-prefix all have working
  ;; dispatch handlers (dispatch-handlers.lisp) but were absent from
  ;; define-named-command-table, so %dispatch-named-command returned the
  ;; :unknown-command sentinel and they could not be invoked from the `C-b :`
  ;; prompt or control mode.  These assert the name -> keyword wiring exists.

  ;; kill-server / start-server / send-prefix are reachable by command name.
  (it "named-commands-server-lifecycle-reachable"
    (let ((cl-tmux::*running* t)
          (cl-tmux::*server-sessions* nil)
          (*overlay* nil))
      (with-empty-session (s)
        ;; start-server: recognised -> no-op overlay, NOT the unknown sentinel.
        (expect (not (eq :unknown-command
                         (cl-tmux::%dispatch-named-command s "start-server"))))
        ;; send-prefix: recognised; an empty session has no active pane -> safe no-op.
        (expect (not (eq :unknown-command
                         (cl-tmux::%dispatch-named-command s "send-prefix"))))
        ;; kill-server: recognised -> dispatches :kill-server, returns :quit and
        ;; clears *running*.
        (expect (eq :quit (cl-tmux::%dispatch-named-command s "kill-server")))
        (expect (null cl-tmux::*running*))
        ;; Control: a genuinely unknown name still returns the :unknown-command
        ;; sentinel, confirming the assertions above are meaningful.
        (expect (eq :unknown-command
                    (cl-tmux::%dispatch-named-command s "definitely-not-a-command-xyz")))))))
