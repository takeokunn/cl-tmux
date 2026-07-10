;;; Startup mode dispatch and CLI entry point.
;;;
;;; Socket discovery/server auto-start helpers live in main-startup-socket.lisp.
;;; Command-client forwarding helpers live in main-startup-forwarding.lisp.
;;; This file owns the mode table and binary entry-point dispatch.

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
    ;; has-session: exit 0 if session exists, 1 otherwise (useful in scripts).
    ("has-session"    . (run-has-session :raw-args-p t))
    ;; kill-server: terminate the server process.
    ("kill-server"    . (run-kill-server :raw-args-p t))
    ;; list-sessions: print sessions to stdout.
    ("list-sessions"  . (run-list-sessions :raw-args-p t))
    ;; list-windows: print windows via the running server.
    ("list-windows"   . (run-list-windows :raw-args-p t))
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
    ("control"        . (run-control-mode :raw-args-p t))
    ;; -V: print the version and exit (tmux -V). --version/-h/--help are
    ;; cl-tmux conveniences; tmux only prints usage on a bad flag.
    ("-V"             . (run-version :raw-args-p t))
    ("--version"      . (run-version :raw-args-p t))
    ("-h"             . (run-usage :raw-args-p t))
    ("--help"         . (run-usage :raw-args-p t)))
  "Mode-name -> plist dispatch table for the binary entry point.
   Each entry is (mode-name . (handler-symbol &key :raw-args-p bool)).
   :raw-args-p T means the handler receives the full raw argv tail rather
   than a single session-name string.
   Storing handler symbols (not function objects) means test stubs that rebind
   the function cell with SETF FDEFINITION are honoured at dispatch time.")

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
   The socket file must exist AND accept connections — a stale socket left by
   a crashed server does not count as a live session (tmux would unlink it)."
  (let* ((target (loop for (flag value) on raw-args by #'cddr
                       when (string= flag "-t") return value))
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
  (let* ((args (or raw-args
                   (let ((path (cl-tmux/config:config-file-path)))
                     (when (and path (probe-file path))
                       (list path)))))
         (previous-log *message-log*))
    (handler-case
        (let ((ok (cl-tmux/config:source-files args)))
          (unless ok
            (dolist (entry (reverse (ldiff *message-log* previous-log)))
              (format *error-output* "~A~%" (cdr entry))))
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

(defun %consume-global-socket-flags (argv)
  "Consume tmux's global socket flags from the front of ARGV, before the
   command word: -L <socket-name> and -S <socket-path>, in both the separated
   (-L name) and attached (-Lname) getopt forms.  Sets *socket-name-override* /
   *socket-path-override* and returns the remaining argv."
  (loop
    (let ((head (first argv)))
      (cond
        ((null head) (return argv))
        ((string= head "-L")
         (pop argv)
         (when argv (setf *socket-name-override* (pop argv))))
        ((string= head "-S")
         (pop argv)
         (when argv (setf *socket-path-override* (pop argv))))
        ((and (> (length head) 2) (string= "-L" head :end2 2))
         (setf *socket-name-override* (subseq head 2))
         (pop argv))
        ((and (> (length head) 2) (string= "-S" head :end2 2))
         (setf *socket-path-override* (subseq head 2))
         (pop argv))
        (t (return argv))))))

(defun main ()
  "Binary entry point - dispatches on the first argv item via *startup-modes*.
   tmux's global socket flags (-L socket-name / -S socket-path) are consumed
   from the front of argv before mode dispatch.
   Each entry in *startup-modes* is a plist (handler-symbol &key :raw-args-p).
   :raw-args-p T modes receive the full argv tail; all others receive a single
   session name (defaulting to \"0\").
   Unrecognized or absent modes fall through to run-standalone."
  (let* ((argv    (%consume-global-socket-flags (%application-argv)))
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
   An unknown dash-flag is a usage error: print the usage summary to stderr
   and exit 1 (tmux's bad-flag behaviour) instead of silently starting a
   standalone session on a typo.
   When MODE names a command AND a default-session server is already running
   (its socket exists), forward MODE + REST to it as a command client; otherwise
   run the standalone multiplexer (the bare-invocation / no-server behaviour).
   Guarding on an existing socket keeps `cl-tmux` (no args) and the no-server
   case unchanged - only an explicit subcommand against a live server forwards."
  (cond
    ((and mode (plusp (length mode)) (char= (char mode 0) #\-))
     (write-string (%usage-string) *error-output*)
     (sb-ext:exit :code 1))
    ((and mode (probe-file (socket-path "0")))
     (run-command-client "0" (cons mode rest)))
    (t (run-standalone))))
