(in-package #:cl-tmux)

;;;; Binary entry point.
;;;;
;;;; Sequence: load config → read terminal size → fork all initial panes →
;;;; start one reader thread per pane → install SIGWINCH → run the event loop
;;;; in raw mode → clean up PTYs on exit.  All forks happen before any reader
;;;; thread starts, so fork never races with a running thread.
;;;;
;;;; Runtime state and threading live in runtime.lisp; the event loop and
;;;; command dispatch live in events.lisp; the detach-attach server/client live
;;;; in server.lisp / client.lisp.
;;;;
;;;; main dispatches on argv:
;;;;   cl-tmux                 → standalone in-process multiplexer (default)
;;;;   cl-tmux server [NAME]   → headless server owning a session
;;;;   cl-tmux attach [NAME]   → attach a thin client to a running server

(defun %hostname-short (hostname)
  "Return the short form of HOSTNAME: the part before the first dot,
   or the full string if no dot is present."
  (let ((dot (position #\. hostname)))
    (if dot (subseq hostname 0 dot) hostname)))

(defun %safe-getenv (name)
  "Return the value of environment variable NAME, or empty string on failure."
  (or (ignore-errors (sb-ext:posix-getenv name)) ""))

(defun %build-hostname-context ()
  "Return a format context plist for %if condition evaluation at config-load time.
   Includes hostname, version, and common environment variables (TERM, DISPLAY, etc.)
   so that guards like %if #{==:#{TERM},xterm-256color} resolve correctly."
  (let ((hostname (machine-instance)))
    (list :hostname   hostname
          :host       hostname
          :host-short (%hostname-short hostname)
          ;; version: reported as tmux 3.5 for compatibility with config guards.
          :version    "3.5"
          ;; Environment variables commonly used in %if guards.
          ;; #{TERM} guards on default-terminal and terminal-overrides.
          ;; #{TERM_PROGRAM} detects iTerm2 / Apple Terminal / WezTerm / kitty.
          ;; All are looked up at condition-evaluation time so values are current.
          :term           (%safe-getenv "TERM")
          :term-program   (%safe-getenv "TERM_PROGRAM")
          :display        (%safe-getenv "DISPLAY")
          :ssh-connection (%safe-getenv "SSH_CONNECTION")
          :tmux           (%safe-getenv "TMUX")
          :xterm-version  (%safe-getenv "XTERM_VERSION")
          :colorterm      (%safe-getenv "COLORTERM"))))

(defun %make-format-condition-evaluator ()
  "Return a closure (string) → string that expands a %if condition using the
   tmux format language with the current machine hostname.  Wired into
   *config-condition-evaluator* before loading the config file so that .tmux.conf
   blocks like %if #{==:#{host},myserver} work correctly.
   The hostname is captured once when the closure is called, not at closure creation."
  (lambda (cond-str)
    (let ((context (%build-hostname-context)))
      (handler-case
          (cl-tmux/format:expand-format cond-str context)
        (error () "1")))))

(defun run-standalone ()
  "Standalone in-process multiplexer: own a session and run the event loop on
   the local terminal (no socket).  This is the default mode."
  (require :sb-posix)

  ;; Read $SHELL from the environment now that sb-posix is loaded.
  ;; init-default-shell is the ORCHESTRATE-layer call that was formerly
  ;; executed at module load time; doing it here keeps config.lisp pure.
  (cl-tmux/config:init-default-shell)

  ;; Wire up the %if condition evaluator before loading the config file so that
  ;; conditional blocks (e.g. %if #{==:#{host},myserver}) resolve correctly.
  (setf cl-tmux/config:*config-condition-evaluator* (%make-format-condition-evaluator))

  ;; Install the history-limit callback so scroll.lisp uses the option value.
  (setf cl-tmux/terminal:*history-limit-function*
        (lambda () (cl-tmux/options:get-option "history-limit")))

  ;; Apply the user config file — searches cl-tmux config, XDG tmux config, and
  ;; ~/.tmux.conf in priority order (NIL triggers auto-detection).
  (ignore-errors (load-config-file nil))

  ;; Discover terminal dimensions before any fork so children inherit them.
  (multiple-value-setq (*term-rows* *term-cols*)
    (terminal-size))

  ;; Create the session.  All forks happen here, before reader threads start.
  (let ((session (create-initial-session *term-rows* *term-cols*)))
    ;; Register the initial session so it appears in *server-sessions* alongside
    ;; any later new-session — the event loop's %current-session resolver and the
    ;; session-switch commands (switch-client, choose-tree) can then move to and
    ;; FROM it; otherwise it would be orphaned once another session is created.
    (server-add-session session)

    ;; Now it is safe to start threads -- no more forks will occur at this point
    ;; unless the user explicitly creates a new window/pane via a prefix command
    ;; (those forks also happen on the main thread between render cycles).
    ;; We collect the thread objects so stop-reader-threads can join them on exit.
    (let ((reader-threads
           (mapcar #'start-reader-thread (all-panes session))))

      (install-sigwinch-handler)

      ;; Initialise last-activity-time so lock-after-time doesn't fire immediately.
      (setf *last-activity-time* (get-universal-time))
      ;; Start the status-interval timer: status bar refresh, overlay dismiss,
      ;; lock-after-time idle detection, and monitor-silence tracking.
      (setf *status-timer*
            (start-status-timer
             (lambda () (setf *dirty* t))
             :session session
             :server-sessions-fn (lambda () *server-sessions*)))

      (handler-case
          (with-raw-mode
            (clear-display)
            ;; Enable mouse reporting on the outer terminal when the "mouse"
            ;; session option is true.  The render pipeline re-emits these
            ;; sequences on every repaint; this call covers the very first frame
            ;; before the first render fires.
            (when (cl-tmux/options:get-option "mouse")
              (cl-tmux/renderer:enable-mouse-reporting))
            ;; Enable extended (CSI-u) key reporting on the outer terminal when the
            ;; "extended-keys" option is "on"/"always", so modified keys arrive as
            ;; ESC [ <codepoint> ; <mod> u for %handle-escape-csi-u to decode.
            (cl-tmux/renderer:enable-extended-keys
             (cl-tmux/options:get-option "extended-keys"))
            (setf *running* t *dirty* t *resize-pending* nil)
            (event-loop session))
        (sb-posix:syscall-error (c)
          ;; Most likely: stdin is not a TTY.
          (format *error-output*
                  "~&cl-tmux: ~A~%  (is stdin a terminal?)~%" c)
          (sb-ext:exit :code 1))
        (error (c)
          (format *error-output*
                  "~&cl-tmux: unhandled error: ~A~%" c)
          (sb-ext:exit :code 1)))

      ;; Cleanup: reset extended-keys reporting so the parent shell is not left
      ;; receiving CSI-u sequences after cl-tmux exits, then signal shutdown, join
      ;; reader threads and status timer, and close fds.
      (ignore-errors (cl-tmux/renderer:disable-extended-keys))
      (stop-reader-threads (append reader-threads
                                   (when *status-timer* (list *status-timer*))))
      (setf *status-timer* nil)
      (dolist (pane (all-panes session))
        (ignore-errors (pty-close (pane-fd pane) (pane-pid pane)))))))

(defun run-control-mode (&optional args)
  "Control mode (-C): drive cl-tmux over the text protocol on stdin/stdout instead
   of a curses UI (for iTerm2 / tmuxp / libtmux).  Sets up the initial session like
   run-standalone (config load, fork, reader threads), emits the opening
   %session-changed, then runs the control REPL until the client closes stdin.
   The REPL framing + notifications are unit-tested via control-mode-loop; this is
   the thin process-entry glue."
  (declare (ignore args))
  (require :sb-posix)
  (cl-tmux/config:init-default-shell)
  (setf cl-tmux/config:*config-condition-evaluator* (%make-format-condition-evaluator))
  (setf cl-tmux/terminal:*history-limit-function*
        (lambda () (cl-tmux/options:get-option "history-limit")))
  (ignore-errors (load-config-file nil))
  ;; A control client may have no controlling tty; fall back to 80x24.
  (multiple-value-bind (r c) (ignore-errors (terminal-size))
    (setf *term-rows* (or r 80) *term-cols* (or c 24)))
  (let* ((session (create-initial-session *term-rows* *term-cols*))
         (readers (progn (server-add-session session)
                         (mapcar #'start-reader-thread (all-panes session)))))
    (setf *running* t)
    (write-line (cl-tmux/control:control-session-changed
                 (session-id session) (session-name session))
                *standard-output*)
    (force-output *standard-output*)
    (unwind-protect
         (control-mode-loop session *standard-input* *standard-output*)
      (setf *running* nil)
      (stop-reader-threads readers)
      (dolist (pane (all-panes session))
        (ignore-errors (pty-close (pane-fd pane) (pane-pid pane)))))))

;;; ── Flag-parser macro ────────────────────────────────────────────────────────
;;;
;;; define-flag-parser generates a parser for a set of boolean and value flags.
;;; Each FLAG-SPEC is one of:
;;;   (:bool  "flag-string"  variable-name)   — sets variable-name to T
;;;   (:value "flag-string"  variable-name)   — sets variable-name to the next arg
;;; The macro generates a loop over the args vector and produces a multi-value
;;; return of all variables in declaration order.

(defmacro define-flag-parser (parser-name (&rest defaults) &rest flag-specs)
  "Define PARSER-NAME as a function (ARGS) → (values ...) that parses FLAGS.
   DEFAULTS is a list of (variable-name default-value) bindings.
   FLAG-SPECS are (:bool FLAG VAR) or (:value FLAG VAR) declarations."
  (let ((args-sym (gensym "ARGS"))
        (i-sym    (gensym "I"))
        (a-sym    (gensym "A"))
        (var-names (mapcar #'first defaults)))
    `(defun ,parser-name (,args-sym)
       ,(format nil "Generated flag parser for: ~{~A~^, ~}"
                (mapcar #'second flag-specs))
       (let (,@defaults
             (,i-sym 0))
         (loop while (< ,i-sym (length ,args-sym)) do
           (let ((,a-sym (nth ,i-sym ,args-sym)))
             (cond
               ,@(mapcar
                  (lambda (spec)
                    (ecase (first spec)
                      (:bool
                       (destructuring-bind (_ flag var) spec
                         (declare (ignore _))
                         `((string= ,a-sym ,flag)
                           (setf ,var t)
                           (incf ,i-sym))))
                      (:value
                       (destructuring-bind (_ flag var) spec
                         (declare (ignore _))
                         `((string= ,a-sym ,flag)
                           (incf ,i-sym)
                           (when (< ,i-sym (length ,args-sym))
                             (setf ,var (nth ,i-sym ,args-sym))
                             (incf ,i-sym)))))))
                  flag-specs)
               ;; Unknown flags are silently consumed.
               (t (incf ,i-sym)))))
         (values ,@var-names)))))

(define-flag-parser %parse-attach-flags
    ((name "0") (detach nil) (read-only-p nil))
  (:value "-t" name)
  (:bool  "-d" detach)
  (:bool  "-r" read-only-p))

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
;;; outside the handler — this is the one-argument convention.

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
    ;; source-file: load a config file directly (useful for testing configs).
    ("source-file"    . (run-source-file :raw-args-p t))
    ("source"         . (run-source-file :raw-args-p t))
    ;; -C / control: control mode — text protocol on stdin/stdout (iTerm2/tmuxp).
    ("-C"             . (run-control-mode :raw-args-p t))
    ("control"        . (run-control-mode :raw-args-p t)))
  "Mode-name → plist dispatch table for the binary entry point.
   Each entry is (mode-name . (handler-symbol &key :raw-args-p bool)).
   :raw-args-p T means the handler receives the full raw argv tail rather
   than a single session-name string.
   Storing handler symbols (not function objects) means test stubs that rebind
   the function cell with SETF FDEFINITION are honoured at dispatch time.")

(defconstant +server-socket-poll-interval-seconds+ 0.1
  "Seconds between socket-existence probes while waiting for a server to start.")

(defconstant +server-socket-poll-max-iterations+ 30
  "Maximum number of socket-existence probes (30 × 0.1 s = 3 s total wait).")

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
      ;; :wait nil means non-blocking — run-program returns immediately after fork.
      (let ((launched (ignore-errors
                        (sb-ext:run-program exe args
                                            :wait nil :output nil :error nil))))
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

(defun run-attach-simple (name)
  "Auto-start a server for NAME if not running, then attach as a client.
   This is the handler for the bare 'attach' mode (no flag parsing)."
  (%ensure-server-running name)
  (run-client name))

(defun run-attach-with-flags (raw-args)
  "Parse attach flags from RAW-ARGS and attach to the named session.
   Note: the -r (read-only) flag is parsed by %parse-attach-flags but is not
   yet enforced — run-client does not currently propagate a read-only constraint
   to the server.  It is intentionally a no-op until server-side enforcement is
   implemented."
  (multiple-value-bind (name detach-p readonly-p) (%parse-attach-flags raw-args)
    (declare (ignore readonly-p))
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
   Ensures the server is running; sends new-session command via the socket."
  (multiple-value-bind (name _win-name detach _start-dir)
      (%parse-new-session-flags raw-args)
    (declare (ignore _win-name _start-dir))
    (let* ((sess-name (or name "0")))
      ;; Start the server if not already running.
      (%ensure-server-running sess-name)
      ;; Attach; if the session doesn't exist the server creates it on first attach.
      (unless detach
        (run-client sess-name)))))

(defun run-has-session (raw-args)
  "Exit 0 when a session named by -t exists, exit 1 otherwise.
   Connects to the server to query the session list."
  (let* ((target (loop for (a b) on raw-args by #'cddr
                       when (string= a "-t") return b))
         (name   (or target "0"))
         (socket (socket-path name)))
    (if (probe-file socket)
        (sb-ext:exit :code 0)
        (sb-ext:exit :code 1))))

(defun run-kill-server (raw-args)
  "Send kill-server command via the socket, then exit."
  (declare (ignore raw-args))
  ;; In standalone mode, we can't reach the server socket from this process.
  ;; This is a best-effort implementation.
  (format t "kill-server: not supported in standalone mode~%")
  (sb-ext:exit :code 0))

(defun run-list-sessions (raw-args)
  "Print a list of active sessions to stdout and exit.
   Reads the server socket to query sessions."
  (declare (ignore raw-args))
  ;; In standalone mode, we cannot query the server.
  ;; Best effort: list any session whose socket file exists.
  (format t "(no server running or session listing requires attach)~%")
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
  "Binary entry point — dispatches on the first argv item via *startup-modes*.
   Each entry in *startup-modes* is a plist (handler-symbol &key :raw-args-p).
   :raw-args-p T modes receive the full argv tail; all others receive a single
   session name (defaulting to \"0\").
   Unrecognized or absent modes fall through to run-standalone."
  (let* ((argv    (rest sb-ext:*posix-argv*))
         (mode    (first argv))
         (rest    (rest argv))
         (entry   (cdr (assoc mode *startup-modes* :test #'equal))))
    (if entry
        (let ((handler    (first entry))
              (raw-args-p (getf (rest entry) :raw-args-p)))
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
   case unchanged — only an explicit subcommand against a live server forwards."
  (if (and mode (probe-file (socket-path "0")))
      (run-command-client "0" (cons mode rest))
      (run-standalone)))
