;;; Startup mode dispatch and CLI entry points.
;;;
;;; This file is loaded after main.lisp so it can reuse the runtime helpers
;;; defined there without making src/main.lisp carry the full startup surface.

(in-package :cl-tmux)

;;; ── Startup mode dispatch (data / logic separation) ─────────────────────────
;;;
;;; *startup-modes* is the DATA: a map from mode-name strings to handler
;;; functions.  main is the LOGIC: it looks up the mode and dispatches.
;;; Adding a new mode only requires adding an entry to the alist, not changing
;;; the dispatch logic.
;;;
;;; All modes, including "attach-session" (flag-aware), live in this table.
;;; Each handler is a symbol so test stubs that rebind the function cell with
;;; SETF FDEFINITION are honoured at dispatch time.
;;;
;;; Handlers that need RAW-ARGS (the full argv tail) receive them directly.
;;; Handlers that need only a session NAME extract (or (first rest) "0")
;;; outside the handler - this is the one-argument convention.

(defparameter *startup-modes*
  '(("server"         . (run-server))
    ("attach"         . (run-attach-simple))
    ("attach-session" . (run-attach-with-flags :raw-args-p t))
    ;; new-session: create a new session (optionally named) and attach to it.
    ("new-session"    . (run-new-session :raw-args-p t))
    ("new"            . (run-new-session :raw-args-p t))
    ;; has-session: exit 0 if session exists, 1 otherwise (useful in scripts).
    ("has-session"    . (run-has-session :raw-args-p t))
    ;; kill-server: terminate the server process.
    ("kill-server"    . (run-kill-server :raw-args-p t))
    ;; list-sessions: print sessions to stdout.
    ("list-sessions"  . (run-list-sessions :raw-args-p t))
    ("ls"             . (run-list-sessions :raw-args-p t))
    ;; list-windows: print windows via the running server.
    ("list-windows"   . (run-list-windows :raw-args-p t))
    ("lsw"            . (run-list-windows :raw-args-p t))
    ;; list-commands: print known commands to stdout without requiring a TTY.
    ("list-commands"  . (run-list-commands :raw-args-p t))
    ;; display-message: command-client path; real tmux requires a running server.
    ("display-message" . (run-display-message :raw-args-p t))
    ;; show-options: command-client path; real tmux requires a running server.
    ("show-options"    . (run-show-options :raw-args-p t))
    ;; show-window-options: command-client path; real tmux requires a running server.
    ("show-window-options" . (run-show-window-options :raw-args-p t))
    ;; source-file: load a config file directly (useful for testing configs).
    ("source-file"    . (run-source-file :raw-args-p t))
    ;; -C / control: control mode - text protocol on stdin/stdout (iTerm2/tmuxp).
    ("-C"             . (run-control-mode :raw-args-p t))
    ("control"        . (run-control-mode :raw-args-p t)))
  "Mode-name -> plist dispatch table for the binary entry point.
   Each entry is (mode-name . (handler-symbol &key :raw-args-p bool)).
   :raw-args-p T means the handler receives the full raw argv tail rather
   than a single session-name string.
   Storing handler symbols (not function objects) means test stubs that rebind
   the function cell with SETF FDEFINITION are honoured at dispatch time.")

(defconstant +server-socket-poll-interval-seconds+ 0.1
  "Seconds between socket-existence probes while waiting for a server to start.")

(defconstant +server-socket-poll-max-iterations+ 30
  "Maximum number of socket-existence probes (30 x 0.1 s = 3 s total wait).")

(defun %ensure-server-running (session-name)
  "Start a background server for SESSION-NAME if no socket exists.
   Uses sb-ext:run-program with *posix-argv* to spawn a separate process.
   Only enters the polling loop when run-program succeeded.
   Polls every +server-socket-poll-interval-seconds+ for up to
   +server-socket-poll-max-iterations+ iterations for the socket to appear."
  (let* ((socket-path (socket-path session-name))
         (exe         (first sb-ext:*posix-argv*))
         (args        (list "server" session-name)))
    (unless (probe-file socket-path)
      ;; Guard: run-program may fail in test environments or when the
      ;; binary is not yet on PATH.  Only poll if the spawn succeeded.
      ;; :wait nil means non-blocking, so run-program returns after starting the child.
      (let ((launched (ignore-errors
                        (sb-ext:run-program exe args
                                            :wait nil
                                            :output nil :error nil))))
        ;; Poll only when we actually attempted a launch.  This avoids the
        ;; unconditional 3-second dead-time when run-program silently failed.
        (when launched
          (loop repeat +server-socket-poll-max-iterations+
                until (probe-file socket-path)
                do (sleep +server-socket-poll-interval-seconds+)))))))

(defun %startup-mode-raw-args-p (mode-name)
  "Return T when the startup mode named MODE-NAME receives the full raw argv tail.
   Returns NIL for unknown mode names or modes that receive only a session name."
  (let ((entry (cdr (assoc mode-name *startup-modes* :test #'equal))))
    (when entry
      (not (null (getf (rest entry) :raw-args-p))))))

(defun %application-argv ()
  "Return cl-tmux application arguments from SBCL's process argv.
   The Nix wrapper starts the saved core as `sbcl --core ... --no-userinit ...`;
   in that shape SBCL runtime options can appear before the real cl-tmux command."
  (let* ((argv (rest sb-ext:*posix-argv*))
         (marker (or (position "--no-userinit" argv :test #'string= :from-end t)
                     (position "--end-toplevel-options" argv :test #'string= :from-end t))))
    (if marker
        (nthcdr (1+ marker) argv)
        argv)))

(defun run-attach-simple (name)
  "Auto-start a server for NAME if not running, then attach as a client.
   This is the handler for the bare 'attach' mode (no flag parsing)."
  (%ensure-server-running name)
  (run-client name))

(defun run-attach-with-flags (raw-args)
  "Parse attach flags from RAW-ARGS and attach to the named session.
   -r sets *client-read-only* so no keystrokes or mouse events reach panes."
  (multiple-value-bind (name detach-p readonly-p) (%parse-attach-flags raw-args)
    (setf *client-read-only* readonly-p)
    (%ensure-server-running name)
    (run-client name :detach-others detach-p)))

(define-flag-parser %parse-new-session-flags
    ((name nil) (win-name nil) (detach nil) (start-dir nil))
  (:value "-s" name)
  (:value "-n" win-name)
  (:bool  "-d" detach)
  (:value "-c" start-dir))

(defun %socket-file-session-name (path)
  "Extract the cl-tmux session/server name from a socket PATH, or NIL."
  (when path
    (let* ((name (pathname-name path))
           (prefix "cl-tmux-"))
      (when (and name
                 (>= (length name) (length prefix))
                 (string= prefix name :end2 (length prefix)))
        (subseq name (length prefix))))))

(defun %running-server-name (&optional preferred-name)
  "Return the best known running server socket name, preferring PREFERRED-NAME.
   Falls back to the default \"0\" socket, then to the first cl-tmux socket in
   TMPDIR.  This supports CLI command forwarding even when the first server was
   launched with `new-session -s NAME` or `attach NAME`."
  (cond
    ((and preferred-name (probe-file (socket-path preferred-name)))
     preferred-name)
    ((probe-file (socket-path "0")) "0")
    (t
     (let* ((env-tmpdir (sb-ext:posix-getenv "TMPDIR"))
            (tmpdir (if (and env-tmpdir (plusp (length env-tmpdir)))
                        (string-right-trim "/" env-tmpdir)
                        "/tmp"))
            (pattern (merge-pathnames "cl-tmux-*.sock"
                                      (parse-namestring (format nil "~A/" tmpdir)))))
       (%socket-file-session-name (first (ignore-errors (directory pattern))))))))

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

;;; ── Forwarding-command fact table ───────────────────────────────────────────
;;;
;;; All commands that simply forward to a running server live here.
;;; Adding a new one only requires a new row — no logic change needed.

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

(defun run-new-session (raw-args)
  "Create a new session (optionally named via -s) and attach to it.
   If a server already exists, forward the full new-session command to it so
   flags such as -n, -c, -x, -y, -t and -A are handled by the server-side tmux
   command implementation.  Otherwise start the first server and attach to it."
  (multiple-value-bind (name _win-name detach _start-dir)
      (%parse-new-session-flags raw-args)
    (declare (ignore _win-name _start-dir))
    (let* ((session-name (or name "0"))
           (server-name (%running-server-name)))
      (if server-name
          (progn
            (%forward-startup-command server-name "new-session" raw-args)
            (unless detach
              (run-client server-name)))
          (progn
            (%ensure-server-running session-name)
            (unless detach
              (run-client session-name)))))))

(defun run-has-session (raw-args)
  "Exit 0 when a session named by -t exists, exit 1 otherwise.
   Checks for the server socket file (no live connection required)."
  (let* ((target (loop for (flag value) on raw-args by #'cddr
                       when (string= flag "-t") return value))
         (name   (or target "0"))
         (socket (socket-path name)))
    (if (probe-file socket)
        (sb-ext:exit :code 0)
        (progn
          (format *error-output*
                  "error connecting to ~A (No such file or directory)~%"
                  socket)
          (sb-ext:exit :code 1)))))

(defun %list-commands-arguments (raw-args)
  "Return (values FORMAT NAME) for list-commands' no-server stdout path."
  (loop with fmt = nil
        with name = nil
        with index = 0
        while (< index (length raw-args))
        for arg = (nth index raw-args)
        do (cond
             ((string= arg "-F")
              (incf index)
              (when (< index (length raw-args))
                (setf fmt (nth index raw-args))))
             ((and (null name)
                   (not (string= arg "")))
              (setf name arg)))
           (incf index)
        finally (return (values fmt name))))

(defun run-list-commands (raw-args)
  "Print tmux public command names to stdout and exit.
   This covers the no-server query path used by scripts and list-commands checks.
   The in-session list-commands command remains implemented by the dispatcher."
  (multiple-value-bind (fmt name) (%list-commands-arguments raw-args)
    (dolist (command-name (%list-command-public-names name))
      (format t "~A~%" (%format-list-command-entry fmt command-name))))
  (sb-ext:exit :code 0))

(defun run-source-file (raw-args)
  "Load and apply a tmux config file, then exit.
   Usage: tmux source-file <path>
   Applies config directives from PATH against the global defaults.
   Useful for pre-loading a config before the multiplexer starts."
  (let ((path (or (first raw-args) (cl-tmux/config:config-file-path))))
    (when (and path (probe-file path))
      (handler-case
          (cl-tmux/config:load-config-file path)
        (error (c)
          (format *error-output* "source-file: ~A~%" c)
          (sb-ext:exit :code 1))))
    (sb-ext:exit :code 0)))

(defun main ()
  "Binary entry point - dispatches on the first argv item via *startup-modes*.
   Each entry in *startup-modes* is a plist (handler-symbol &key :raw-args-p).
   :raw-args-p T modes receive the full argv tail; all others receive a single
   session name (defaulting to \"0\").
   Unrecognized or absent modes fall through to run-standalone."
  (let* ((argv    (%application-argv))
         (mode    (first argv))
         (rest    (rest argv))
         (entry   (cdr (assoc mode *startup-modes* :test #'equal))))
    (if entry
        (let ((handler    (first entry))
              (raw-args-p (%startup-mode-raw-args-p mode)))
          ;; Dispatch: :raw-args-p modes receive the full tail; name-only modes
          ;; receive a single session name so their signature stays (name).
          (if raw-args-p
              (funcall (symbol-function handler) rest)
              (funcall (symbol-function handler) (or (first rest) "0"))))
        ;; No recognized mode: forward to a running server as a command client
        ;; (`cl-tmux <command>` against an existing server), else run standalone.
        (%dispatch-unknown-mode mode rest))))

(defun %dispatch-unknown-mode (mode rest)
  "Handle an argv whose first item is not a known startup mode.
   When MODE names a command AND a default-session server is already running
   (its socket exists), forward MODE + REST to it as a command client; otherwise
   run the standalone multiplexer (the bare-invocation / no-server behaviour).
   Guarding on an existing socket keeps `cl-tmux` (no args) and the no-server
   case unchanged - only an explicit subcommand against a live server forwards."
  (if (and mode (probe-file (socket-path "0")))
      (run-command-client "0" (cons mode rest))
      (run-standalone)))
