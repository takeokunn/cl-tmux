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

(defun run-standalone ()
  "Standalone in-process multiplexer: own a session and run the event loop on
   the local terminal (no socket).  This is the default mode."
  (require :sb-posix)

  ;; Apply the user config file (~/.cl-tmux.conf or $CL_TMUX_CONF) if present.
  (ignore-errors (load-config-file))

  ;; Discover terminal dimensions before any fork so children inherit them.
  (multiple-value-setq (*term-rows* *term-cols*)
    (terminal-size))

  ;; Create the session.  All forks happen here, before reader threads start.
  (let ((session (create-initial-session *term-rows* *term-cols*)))

    ;; Now it is safe to start threads -- no more forks will occur at this point
    ;; unless the user explicitly creates a new window/pane via a prefix command
    ;; (those forks also happen on the main thread between render cycles).
    ;; We collect the thread objects so stop-reader-threads can join them on exit.
    (let ((reader-threads
           (mapcar #'start-reader-thread (all-panes session))))

      (install-sigwinch-handler)

      (handler-case
          (with-raw-mode
            (clear-display)
            ;; Enable mouse reporting on the outer terminal when the "mouse"
            ;; session option is true.  The render pipeline re-emits these
            ;; sequences on every repaint; this call covers the very first frame
            ;; before the first render fires.
            (when (cl-tmux/options:get-option "mouse")
              (cl-tmux/renderer:enable-mouse-reporting))
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

      ;; Cleanup: signal shutdown, join reader threads, then close fds.
      (stop-reader-threads reader-threads)
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
    ((name "0") (detach nil) (ro nil))
  (:value "-t" name)
  (:bool  "-d" detach)
  (:bool  "-r" ro))

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
  '(("server"         . run-server)
    ("attach"         . run-client-with-autostart)
    ("attach-session" . run-attach-with-flags))
  "Mode-name → function-name (symbol) dispatch table for the binary entry point.
   Storing symbols (not function objects) means test stubs that rebind the
   function cell with SETF FDEFINITION are honoured at dispatch time.")

(defun %ensure-server-running (session-name)
  "Start a background server for SESSION-NAME if no socket exists.
   Uses sb-ext:run-program with *posix-argv* to spawn a separate process safely.
   Polls every 100 ms for up to 3 seconds for the socket to appear.
   SBCL-only: on other implementations emits a warning and returns nil."
  (let ((socket-path (socket-path session-name)))
    (unless (probe-file socket-path)
      #+sbcl
      (let ((exe  (first sb-ext:*posix-argv*))
            (args (list "server" session-name)))
        ;; Guard: run-program may fail in test environments or when the binary
        ;; is not yet on PATH. In that case we proceed and run-client will fail
        ;; gracefully with a connection error.
        (ignore-errors
          (sb-ext:run-program exe args :wait nil :output nil :error nil
                              :timeout 30)))
      #-sbcl
      (warn "Cannot auto-start server: not running on SBCL")
      ;; Poll for the socket to appear (up to 3 seconds, 100 ms intervals).
      ;; Bounded loop (max 30 iterations = 3 seconds) prevents infinite wait.
      (loop for i from 0 to 30
            until (probe-file socket-path)
            do (sleep 0.1)))))

(defun run-client-with-autostart (name)
  "Auto-start a server for NAME if not running, then attach as a client."
  (%ensure-server-running name)
  (run-client name))

(defun run-attach-with-flags (raw-args)
  "Parse attach flags from RAW-ARGS and attach to the named session.
   Note: the -r (read-only) flag is parsed by %parse-attach-flags but is not
   yet enforced — run-client does not currently propagate a read-only constraint
   to the server.  It is intentionally a no-op until server-side enforcement is
   implemented."
  (multiple-value-bind (name detach-p _readonly-p) (%parse-attach-flags raw-args)
    (declare (ignore _readonly-p))
    (%ensure-server-running name)
    (run-client name :detach-others detach-p)))

(defun %startup-mode-raw-args-p (mode)
  "Return T if MODE takes the full raw-args list rather than a single name.
   Currently only attach-session needs the raw args for flag parsing."
  (string= mode "attach-session"))

(defun main ()
  "Binary entry point — dispatches on the first argv item via *startup-modes*.
   Modes in the table receive either the full raw-args list (for flag-aware
   modes like attach-session) or a single session name (default \"0\").
   Unrecognized or absent modes fall through to run-standalone."
  (let* ((argv    (rest sb-ext:*posix-argv*))
         (mode    (first argv))
         (rest    (rest argv))
         (handler (cdr (assoc mode *startup-modes* :test #'equal))))
    (if handler
        ;; Dispatch: flag-aware modes get the full rest; name-only modes get a
        ;; single session name so their signature stays (name) not (args).
        (if (%startup-mode-raw-args-p mode)
            (funcall (symbol-function handler) rest)
            (funcall (symbol-function handler) (or (first rest) "0")))
        ;; Default: no recognized mode → standalone multiplexer.
        (run-standalone))))
