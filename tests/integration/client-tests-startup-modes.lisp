(in-package #:cl-tmux/test)

(in-suite client-suite)

;;; ── *startup-modes* handler symbols are symbols ──────────────────────────────
;;;
;;; Handlers stored as symbols (not function objects) is the key architectural
;;; property that makes test stubs with SETF FDEFINITION work.

(test startup-modes-all-handlers-are-symbols
  :description "Every entry in *startup-modes* stores its handler as a symbol, not a
   function object.  This is required so test stubs with (setf fdefinition) work."
  (dolist (entry cl-tmux::*startup-modes*)
    (let ((handler (first (cdr entry))))
      (is (symbolp handler)
          "handler for mode ~S must be a symbol, got ~S"
          (car entry) handler))))

(test startup-modes-mode-handlers-table
  :description "*startup-modes* server/attach/attach-session entries have the expected handlers."
  (dolist (c '(("server"          cl-tmux::run-server             nil "server → run-server")
               ("attach"          cl-tmux::run-attach-simple       nil "attach → run-attach-simple")
               ("attach-session"  cl-tmux::run-attach-with-flags    t  "attach-session → run-attach-with-flags")))
    (destructuring-bind (mode handler raw-args-p desc) c
      (let ((entry (assoc mode cl-tmux::*startup-modes* :test #'equal)))
        (is-true entry "~A: *startup-modes* must have a '~A' entry" desc mode)
        (is (eq handler (first (cdr entry))) "~A: handler must be ~A" desc handler)
        (when raw-args-p
          (is-true (getf (rest (cdr entry)) :raw-args-p)
                   "~A: must have :raw-args-p T" desc))))))
