(in-package #:cl-tmux/test)

;;; ── *startup-modes* handler symbols are symbols ──────────────────────────────
;;;
;;; Handlers stored as symbols (not function objects) is the key architectural
;;; property that makes test stubs with SETF FDEFINITION work.

(describe "client-suite"

  ;; Every entry in *startup-modes* stores its handler as a symbol, not a
  ;; function object.  This is required so test stubs with (setf fdefinition) work.
  (it "startup-modes-all-handlers-are-symbols"
    (dolist (entry cl-tmux::*startup-modes*)
      (let ((handler (first (cdr entry))))
        (expect (symbolp handler)))))

  ;; *startup-modes* server/attach/attach-session entries have the expected handlers.
  (it "startup-modes-mode-handlers-table"
    (dolist (c '(("server"          cl-tmux::run-server             nil "server → run-server")
                 ("attach"          cl-tmux::run-attach-simple       nil "attach → run-attach-simple")
                 ("attach-session"  cl-tmux::run-attach-with-flags    t  "attach-session → run-attach-with-flags")))
      (destructuring-bind (mode handler raw-args-p desc) c
        (declare (ignore desc))
        (let ((entry (assoc mode cl-tmux::*startup-modes* :test #'equal)))
          (expect entry :to-be-truthy)
          (expect (eq handler (first (cdr entry))))
          (when raw-args-p
            (expect (getf (rest (cdr entry)) :raw-args-p) :to-be-truthy)))))))
