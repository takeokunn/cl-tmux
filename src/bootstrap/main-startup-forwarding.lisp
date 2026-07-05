;;; Startup command-client forwarding helpers.

(in-package :cl-tmux)

(defmacro %try-or-nil (&body body)
  "Run BODY, returning its value, or NIL if it signals an ERROR.
   Used for risky I/O (e.g. socket connections) where any failure should
   collapse to a boolean/NIL result rather than propagate."
  `(handler-case (progn ,@body) (error () nil)))

(defun %forward-startup-command (server-name command raw-args)
  "Forward COMMAND and RAW-ARGS to SERVER-NAME, returning T on success.
   Stale socket files are common after crashes; connection failures must not make
   startup/list/kill CLI commands crash before they can print a useful result."
  (when server-name
    (%try-or-nil
      (run-command-client server-name (cons command raw-args))
      t)))

(defun %forward-or-die (command raw-args)
  "Forward COMMAND + RAW-ARGS to a running server, or exit 1 on failure.
   Looks up the running server, forwards the command, and prints a tmux-style
   connection-failure message when no server is reachable."
  (let ((server-name (%running-server-name)))
    (unless (%forward-startup-command server-name command raw-args)
      (format *error-output*
              "error connecting to ~A (No such file or directory)~%"
              (socket-path (or server-name "0")))
      (sb-ext:exit :code 1))))

(defmacro define-forwarding-commands (&rest specs)
  "Generate a forwarding run-COMMAND function for each SPEC (fn cmd doc).
   Each function forwards its RAW-ARGS to the running server via
   %forward-or-die, then exits 0.  Adding a command is a one-row data change."
  `(progn
     ,@(mapcar (lambda (spec)
                 (destructuring-bind (fn-name cmd-name doc) spec
                   `(defun ,fn-name (raw-args)
                      ,doc
                      (%forward-or-die ,cmd-name raw-args)
                      (sb-ext:exit :code 0))))
               specs)))

;;; Forwarding-command fact table.
;;;
;;; All commands that simply forward to a running server live here.
;;; Adding a new one only requires a new row, not a logic change.

(define-forwarding-commands
  (run-kill-server         "kill-server"
   "Send kill-server command via the socket, then exit.")
  (run-list-sessions       "list-sessions"
   "Print a list of active sessions to stdout and exit.")
  (run-list-windows        "list-windows"
   "Print a list of windows to stdout and exit.")
  (run-display-message     "display-message"
   "Forward display-message to the running server and exit.")
  (run-show-options        "show-options"
   "Forward show-options to the running server and exit.")
  (run-show-window-options "show-window-options"
   "Forward show-window-options to the running server and exit."))
