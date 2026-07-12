;;; Startup command handlers.
;;;
;;; Attach/new-session/list/source/version handlers live here.
;;; main-startup.lisp keeps the mode table, argv parsing, usage, and dispatch.

(in-package :cl-tmux)

(defun %attach-session (name detach-others-p)
  "Ensure NAME's server is running, then attach the client."
  (%ensure-server-running name)
  (run-client name :detach-others detach-others-p))

(defun %attach-unless-detached (name detach)
  "Attach to NAME when DETACH is not requested."
  (unless detach
    (run-client name)))

(defun %run-new-session-locally (session-name detach)
  "Create SESSION-NAME locally and attach unless DETACH is requested."
  (%ensure-server-running session-name)
  (%attach-unless-detached session-name detach))

(defun %forward-new-session (server-name raw-args detach)
  "Forward new-session to SERVER-NAME and attach unless DETACH is requested."
  (%forward-startup-command server-name "new-session" raw-args)
  (%attach-unless-detached server-name detach))

(defun run-attach-simple (name)
  "Auto-start a server for NAME if not running, then attach as a client.
   This is the handler for the bare 'attach' mode (no flag parsing)."
  (%attach-session name nil))

(defun run-attach-with-flags (raw-args)
  "Parse attach flags from RAW-ARGS and attach to the named session.
   -r sets *client-read-only* so no keystrokes or mouse events reach panes."
  (multiple-value-bind (name detach-p readonly-p) (%parse-attach-flags raw-args)
    (setf *client-read-only* readonly-p)
    (%attach-session name detach-p)))

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
          (%forward-new-session server-name raw-args detach)
          (%run-new-session-locally session-name detach)))))

(defun %flag-value (raw-args flag)
  "Return the value that follows FLAG in RAW-ARGS, or NIL."
  (loop for (arg value) on raw-args by #'cddr
        when (and arg (string= arg flag))
          return value))

(defun %first-positional-arg (raw-args)
  "Return the first non-empty positional argument in RAW-ARGS, or NIL."
  (find-if (lambda (arg)
             (and (stringp arg)
                  (plusp (length arg))
                  (not (char= (char arg 0) #\-))))
           raw-args))

(defun run-has-session (raw-args)
  "Exit 0 when a session named by -t exists, exit 1 otherwise.
   The socket file must exist AND accept connections — a stale socket left by
   a crashed server does not count as a live session (tmux would unlink it)."
  (let* ((target (%flag-value raw-args "-t"))
         (name   (or target "0"))
         (socket (socket-path name)))
    (if (and (probe-file socket)
             (not (%stale-socket-p socket)))
        (sb-ext:exit :code 0)
        (progn
          (format *error-output*
                  "error connecting to ~A (No such file or directory)~%"
                  socket)
          (sb-ext:exit :code 1)))))

(defun %list-commands-arguments (raw-args)
  "Return (values FORMAT NAME) for list-commands' no-server stdout path."
  (values (%flag-value raw-args "-F")
          (%first-positional-arg raw-args)))

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
  (let* ((args (or raw-args
                   (let ((path (cl-tmux/config:config-file-path)))
                     (when (and path (probe-file path))
                       (list path)))))
         (previous-log *message-log*))
    (handler-case
        (let ((ok (cl-tmux/config:source-files args)))
          (%emit-source-file-diagnostics ok previous-log)
          (sb-ext:exit :code (if ok 0 1)))
      (error (c)
        (format *error-output* "source-file: ~A~%" c)
        (sb-ext:exit :code 1)))))

(defun run-version (raw-args)
  "Print the cl-tmux version to stdout and exit 0 (the tmux -V behaviour)."
  (declare (ignore raw-args))
  (format t "cl-tmux ~A~%" (cl-tmux/version:version-string))
  (sb-ext:exit :code 0))

(defun %usage-string ()
  "One-page usage summary for -h/--help and bad-flag errors."
  (format nil "usage: cl-tmux [-L socket-name] [-S socket-path] [command [flags]]~%~
               ~%~
               Run with no command to start a standalone session.~%~
               ~%~
               Commands:~%~
               ~2Tserver [name]~24Trun a headless server owning session NAME~%~
               ~2Tattach [name]~24Tattach to session NAME (auto-starts a server)~%~
               ~2Tattach-session -t name~30T-d detach others, -r read-only~%~
               ~2Tnew-session [-s name] [-n window] [-d] [-c dir]~%~
               ~2Thas-session -t name~24Texit 0 when the session exists~%~
               ~2Tkill-server~24Tterminate the server~%~
               ~2Tlist-sessions | list-windows | list-commands~%~
               ~2Tdisplay-message | show-options | show-window-options~%~
               ~2Tsource-file path~24Tapply a config file and exit~%~
               ~2T-C | control~24Tcontrol mode (text protocol on stdin/stdout)~%~
               ~2T-V | --version~24Tprint the version and exit~%~
               ~%~
               Any other command word is forwarded to a running server~%~
               (e.g. `cl-tmux send-keys -t 0 ls Enter`).~%"))

(defun run-usage (raw-args)
  "Print the usage summary to stdout and exit 0 (-h/--help)."
  (declare (ignore raw-args))
  (write-string (%usage-string))
  (sb-ext:exit :code 0))

(defun %emit-source-file-diagnostics (ok previous-log) (unless ok
            (dolist (entry (reverse (ldiff *message-log* previous-log)))
              (format *error-output* "~A~%" (cdr entry)))))

(defmacro %startup-mode (mode-name handler &key raw-args-p)
  `(cons ,mode-name
         (list ',handler
               ,@(when raw-args-p
                   '(:raw-args-p t)))))

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
  (list (%startup-mode "server" run-server)
        (%startup-mode "attach" run-attach-simple)
        (%startup-mode "attach-session" run-attach-with-flags :raw-args-p t)
        ;; new-session: create a new session (optionally named) and attach to it.
        (%startup-mode "new-session" run-new-session :raw-args-p t)
        ;; has-session: exit 0 if session exists, 1 otherwise (useful in scripts).
        (%startup-mode "has-session" run-has-session :raw-args-p t)
        ;; kill-server: terminate the server process.
        (%startup-mode "kill-server" run-kill-server :raw-args-p t)
        ;; list-sessions: print sessions to stdout.
        (%startup-mode "list-sessions" run-list-sessions :raw-args-p t)
        ;; list-windows: print windows via the running server.
        (%startup-mode "list-windows" run-list-windows :raw-args-p t)
        ;; list-commands: print known commands to stdout without requiring a TTY.
        (%startup-mode "list-commands" run-list-commands :raw-args-p t)
        ;; display-message: command-client path; real tmux requires a running server.
        (%startup-mode "display-message" run-display-message :raw-args-p t)
        ;; show-options: command-client path; real tmux requires a running server.
        (%startup-mode "show-options" run-show-options :raw-args-p t)
        ;; show-window-options: command-client path; real tmux requires a running server.
        (%startup-mode "show-window-options" run-show-window-options :raw-args-p t)
        ;; source-file: load a config file directly (useful for testing configs).
        (%startup-mode "source-file" run-source-file :raw-args-p t)
        ;; -C / control: control mode - text protocol on stdin/stdout (iTerm2/tmuxp).
        (%startup-mode "-C" run-control-mode :raw-args-p t)
        (%startup-mode "control" run-control-mode :raw-args-p t)
        ;; -V: print the version and exit (tmux -V). --version/-h/--help are
        ;; cl-tmux conveniences; tmux only prints usage on a bad flag.
        (%startup-mode "-V" run-version :raw-args-p t)
        (%startup-mode "--version" run-version :raw-args-p t)
        (%startup-mode "-h" run-usage :raw-args-p t)
        (%startup-mode "--help" run-usage :raw-args-p t))
  "Mode-name -> plist dispatch table for the binary entry point.
   Each entry is (mode-name . (handler-symbol &key :raw-args-p bool)).
   :raw-args-p T means the handler receives the full raw argv tail rather
   than a single session-name string.
   Storing handler symbols (not function objects) means test stubs that rebind
   the function cell with SETF FDEFINITION are honoured at dispatch time.")

(defun %startup-mode-entry (mode-name)
  "Return the raw *startup-modes* entry for MODE-NAME, or NIL when unknown."
  (cdr (assoc mode-name *startup-modes* :test #'equal)))

(defun %startup-mode-raw-args-p (mode-name)
  "Return T when the startup mode named MODE-NAME receives the full raw argv tail.
   Returns NIL for unknown mode names or modes that receive only a session name."
  (let ((entry (%startup-mode-entry mode-name)))
    (when entry
      (not (null (getf (rest entry) :raw-args-p))))))
