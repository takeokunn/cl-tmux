(in-package #:cl-tmux)

;;;; Binary entry point.
;;;;
;;;; Sequence: load config → read terminal size → spawn initial panes →
;;;; start one reader thread per pane → install SIGWINCH → run the event loop
;;;; in raw mode → clean up PTYs on exit.
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
          :version    (cl-tmux/version:version-string)
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
  (lambda (condition-string)
    (let ((context (%build-hostname-context)))
      (handler-case
          (cl-tmux/format:expand-format condition-string context)
        (error () "1")))))

(defun %wire-option-callbacks ()
  "Install the option-reader callbacks that the terminal/emulator layer uses.
   Pure assignment — no I/O, no process spawning.  Extracted from %initialize-session-environment
   so the callback wiring is unit-testable independently of the config-file load."
  (setf cl-tmux/terminal:*history-limit-function*
        (lambda () (cl-tmux/options:get-option "history-limit")))
  (setf cl-tmux/terminal:*alternate-screen-enabled-function*
        (lambda () (cl-tmux/options:get-option "alternate-screen")))
  (setf cl-tmux/terminal:*scroll-on-clear-function*
        (lambda () (cl-tmux/options:get-option "scroll-on-clear"))))

(defun %initialize-session-environment ()
  "Set up the shared session environment before spawning any panes.
   Loads the default shell, wires the %if condition evaluator, installs
   the history/alternate-screen/scroll-on-clear option callbacks, and applies
   the user config file.  Called from both run-standalone and run-control-mode
   to avoid duplicating the initialization boilerplate."
  (cl-tmux/config:init-default-shell)
  (setf cl-tmux/config:*config-condition-evaluator* (%make-format-condition-evaluator))
  (%wire-option-callbacks)
  (ignore-errors (load-config-file nil)))

(defun run-standalone ()
  "Standalone in-process multiplexer: own a session and run the event loop on
   the local terminal (no socket).  This is the default mode."
  (require :sb-posix)
  (%initialize-session-environment)

  ;; Load persisted command-prompt history (history-file option), now that the
  ;; config has set the option.
  (ignore-errors (load-prompt-history))

  ;; Discover terminal dimensions before the initial pane is created.
  (multiple-value-setq (*term-rows* *term-cols*)
    (terminal-size))

  ;; Create the initial session before reader threads start.
  (let ((session (create-initial-session *term-rows* *term-cols*)))
    ;; Register the initial session so it appears in *server-sessions* alongside
    ;; any later new-session — the event loop's %current-session resolver and the
    ;; session-switch commands (switch-client, choose-tree) can then move to and
    ;; FROM it; otherwise it would be orphaned once another session is created.
    (server-add-session session)

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
            ;; Request focus in/out reporting from the outer terminal when the
            ;; focus-events option is on, so %notify-pane-focus can forward focus
            ;; to the active pane's application.
            (when (cl-tmux/options:get-option "focus-events")
              (cl-tmux/renderer:enable-focus-reporting))
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

      (%cleanup-after-session session reader-threads))))

(defun %close-all-pane-ptys (session)
  "Close the PTY fd of every pane in SESSION, ignoring errors on already-closed fds."
  (dolist (pane (all-panes session))
    (ignore-errors (pty-close (pane-fd pane) (pane-pid pane)))))

(defun %cleanup-after-session (session reader-threads)
  "Tear down the session after the event loop exits.
   Disables extended-keys and focus reporting on the outer terminal, signals
   shutdown, joins reader threads and the status timer, and closes all pane fds.
   Extracted from run-standalone to reduce visual nesting depth."
  (ignore-errors (cl-tmux/renderer:disable-extended-keys))
  (when (cl-tmux/options:get-option "focus-events")
    (ignore-errors (cl-tmux/renderer:disable-focus-reporting)))
  (stop-reader-threads (append reader-threads
                               (when *status-timer* (list *status-timer*))))
  (setf *status-timer* nil)
  (%close-all-pane-ptys session))

(defun run-control-mode (&optional args)
  "Control mode (-C): drive cl-tmux over the text protocol on stdin/stdout instead
   of a curses UI (for iTerm2 / tmuxp / libtmux).  Sets up the initial session like
   run-standalone (config load, pane spawn, reader threads), emits the opening
   %session-changed, then runs the control REPL until the client closes stdin.
   The REPL framing + notifications are unit-tested via control-mode-loop; this is
   the thin process-entry glue."
  (declare (ignore args))
  (require :sb-posix)
  (%initialize-session-environment)
  ;; A control client may have no controlling tty; fall back to 24 rows x 80 cols
  ;; (terminal-size returns rows then cols, matching the runtime defaults).
  (multiple-value-bind (rows cols) (ignore-errors (terminal-size))
    (setf *term-rows* (or rows 24) *term-cols* (or cols 80)))
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
      (%close-all-pane-ptys session))))

;;; ── Flag-parser macro ────────────────────────────────────────────────────────
;;;
;;; define-flag-parser generates a parser for a set of boolean and value flags.
;;; Each FLAG-SPEC is one of:
;;;   (:bool  "flag-string"  variable-name)   — sets variable-name to T
;;;   (:value "flag-string"  variable-name)   — sets variable-name to the next arg
;;; The macro generates a loop over the args vector and produces a multi-value
;;; return of all variables in declaration order.
;;;
;;; The generated cond has a final (t (incf index)) fallback arm that silently
;;; consumes any argument not matching a known flag.  This is intentional:
;;; flag parsers must tolerate extra arguments (e.g., positional args following
;;; flags) without signalling an error.  Unknown flags are thus silently skipped.

(defmacro define-flag-parser (parser-name (&rest defaults) &rest flag-specs)
  "Define PARSER-NAME as a function (ARGS) → (values ...) that parses FLAGS.
   DEFAULTS is a list of (variable-name default-value) bindings.
   FLAG-SPECS are (:bool FLAG VAR) or (:value FLAG VAR) declarations.
   Unknown flags are silently consumed (see the generated fallback arm)."
  (let ((args-sym   (gensym "ARGS"))
        (index-sym  (gensym "INDEX"))
        (arg-sym    (gensym "ARG"))
        (var-names  (mapcar #'first defaults)))
    `(defun ,parser-name (,args-sym)
       ,(format nil "Generated flag parser for: ~{~A~^, ~}"
                (mapcar #'second flag-specs))
       (let (,@defaults
             (,index-sym 0))
         (loop while (< ,index-sym (length ,args-sym)) do
           (let ((,arg-sym (nth ,index-sym ,args-sym)))
             (cond
               ,@(mapcar
                  (lambda (spec)
                    (ecase (first spec)
                      (:bool
                       (destructuring-bind (_ flag variable) spec
                         (declare (ignore _))
                         `((string= ,arg-sym ,flag)
                           (setf ,variable t)
                           (incf ,index-sym))))
                      (:value
                       (destructuring-bind (_ flag variable) spec
                         (declare (ignore _))
                         `((string= ,arg-sym ,flag)
                           (incf ,index-sym)
                           (when (< ,index-sym (length ,args-sym))
                             (setf ,variable (nth ,index-sym ,args-sym))
                             (incf ,index-sym)))))))
                  flag-specs)
               ;; Unknown flags are silently consumed.  This is intentional:
               ;; parsers must tolerate extra/positional arguments without error.
               (t (incf ,index-sym)))))
         (values ,@var-names)))))

(define-flag-parser %parse-attach-flags
    ((name "0") (detach nil) (read-only-p nil))
  (:value "-t" name)
  (:bool  "-d" detach)
  (:bool  "-r" read-only-p))
