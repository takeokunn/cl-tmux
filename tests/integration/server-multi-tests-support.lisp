(in-package #:cl-tmux/test)

;;;; Multi-client server test support (src/bootstrap/server-multi.lisp and
;;;; src/bootstrap/server-multi-loop.lisp).

(def-suite server-multi-suite :description "Multi-client select-multiplexed server")

(defun %make-test-conn (&key (rows 24) (cols 80))
  "A socket-less CLIENT-CONN for dispatch tests (paths that never touch the socket)."
  (cl-tmux::%make-client-conn :state (cl-tmux::make-input-state)
                              :rows rows :cols cols))
