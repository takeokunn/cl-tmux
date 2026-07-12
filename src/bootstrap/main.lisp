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

(defun %mode-keys-from-editor-string (editor)
  "Return \"vi\" or \"emacs\" derived from EDITOR, a $VISUAL/$EDITOR value, or
   NIL when EDITOR is NIL or empty.  Mirrors tmux's main() logic: take the
   basename (the part after the last '/') and check for the substring \"vi\"."
  (when (and editor (plusp (length editor)))
    (let* ((slash (position #\/ editor :from-end t))
           (base  (if slash (subseq editor (1+ slash)) editor)))
      (if (search "vi" base) "vi" "emacs"))))

(defun %apply-editor-mode-keys ()
  "Auto-detect vi vs emacs key bindings from $VISUAL (preferred) or $EDITOR and
   apply the result to both the global status-keys and mode-keys options, matching
   tmux's startup behavior.  Called before the user config is loaded so an explicit
   `set -g mode-keys ...` in .tmux.conf still wins.  When neither variable is set,
   the registry defaults (emacs) are left untouched."
  (let* ((visual (%safe-getenv "VISUAL"))
         (editor (if (plusp (length visual)) visual (%safe-getenv "EDITOR")))
         (keys   (%mode-keys-from-editor-string editor)))
    (when keys
      (cl-tmux/options:set-option "status-keys" keys)
      (cl-tmux/options:set-option "mode-keys" keys))))

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
  (%apply-editor-mode-keys)
  (%wire-option-callbacks)
  (ignore-errors (load-config-file nil)))

(defun %enable-negotiated-terminal-features ()
  "Enable mouse reporting, extended (CSI-u) key reporting, and focus-event
   reporting on the outer terminal, each gated by its own session option.
   The render pipeline re-emits these sequences on every repaint; this call
   only covers the very first frame, before the first render fires.
   Extracted from run-standalone so the terminal-feature negotiation is a
   single named step in the startup sequence."
  ;; Enable mouse reporting on the outer terminal when the "mouse" session
  ;; option is true.
  (when (cl-tmux/options:get-option "mouse")
    (cl-tmux/renderer:enable-mouse-reporting))
  ;; Enable extended (CSI-u) key reporting on the outer terminal when the
  ;; "extended-keys" option is "on"/"always", so modified keys arrive as
  ;; ESC [ <codepoint> ; <mod> u for %handle-escape-csi-u to decode.
  (cl-tmux/renderer:enable-extended-keys
   (cl-tmux/options:get-option "extended-keys"))
  ;; Request focus in/out reporting from the outer terminal when the
  ;; focus-events option is on, so %notify-pane-focus can forward focus to
  ;; the active pane's application.
  (when (cl-tmux/options:get-option "focus-events")
    (cl-tmux/renderer:enable-focus-reporting)))

(defun %die-with-message (format-string &rest format-args)
  "Print FORMAT-STRING/FORMAT-ARGS to *error-output* and exit with code 1.
   Shared tail call for run-standalone's fatal top-level error handlers."
  (apply #'format *error-output* format-string format-args)
  (sb-ext:exit :code 1))

(defun %start-session-and-readers ()
  "Discover the terminal size, create and register the initial session, start
   one reader thread per pane, install SIGWINCH, and start the status timer.
   Returns (values session reader-threads).  Extracted from run-standalone so
   session/reader-thread/timer startup is a single named step."
  ;; Discover terminal dimensions before the initial pane is created.
  (multiple-value-setq (*term-rows* *term-cols*)
    (terminal-size))
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
             #'%mark-dirty
             :session session
             :server-sessions-fn (lambda () *server-sessions*)))
      (values session reader-threads))))

(defun %run-event-loop-with-handlers (session)
  "Run SESSION's event loop in raw mode, catching stdin-not-a-tty and any other
   top-level error and reporting them via %die-with-message.  Extracted from
   run-standalone so the handler-case wrapping is a single named step."
  (handler-case
      (with-raw-mode
        (clear-display)
        (%enable-negotiated-terminal-features)
        (setf *running* t *dirty* t *resize-pending* nil)
        (event-loop session))
    (sb-posix:syscall-error (c)
      ;; Most likely: stdin is not a TTY.
      (%die-with-message "~&cl-tmux: ~A~%  (is stdin a terminal?)~%" c))
    (error (c)
      (%die-with-message "~&cl-tmux: unhandled error: ~A~%" c))))

(defun run-standalone ()
  "Standalone in-process multiplexer: own a session and run the event loop on
   the local terminal (no socket).  This is the default mode."
  (require :sb-posix)
  (%initialize-session-environment)

  ;; Load persisted command-prompt history (history-file option), now that the
  ;; config has set the option.
  (ignore-errors (load-prompt-history))

  (multiple-value-bind (session reader-threads) (%start-session-and-readers)
    (%run-event-loop-with-handlers session)
    (%cleanup-after-session session reader-threads)))

(defun %close-all-pane-ptys (session)
  "Close the PTY fd of every pane in SESSION, ignoring errors on already-closed fds."
  (dolist (pane (all-panes session))
    (close-pane-pty pane)))

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
  ;; A control client may have no controlling tty; fall back to the shared
  ;; default terminal size (terminal-size returns rows then cols, matching
  ;; the runtime defaults).
  (multiple-value-bind (rows cols) (ignore-errors (terminal-size))
    (setf *term-rows* (or rows cl-tmux/pty:+default-term-rows+)
          *term-cols* (or cols cl-tmux/pty:+default-term-cols+)))
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
