(in-package #:cl-tmux/test)

;;;; Client lifecycle tests (src/client.lisp).
;;;; Tests: client-suite — function existence and socket-path format.

(def-suite client-suite :description "Client connect/detach lifecycle")
(in-suite client-suite)

(test client-run-client-is-defined
  "run-client is a defined function (integration tested via e2e-smoke)."
  (is (fboundp 'cl-tmux::run-client) "run-client must be defined"))

(test client-socket-path-format
  "The socket path for session '0' includes the session name."
  (is (search "0" (cl-tmux::socket-path "0"))
      "socket path must contain the session name"))
