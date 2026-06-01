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
    (dolist (pane (all-panes session))
      (start-reader-thread pane))

    (install-sigwinch-handler)

    (handler-case
        (with-raw-mode
          (clear-display)
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

    ;; Cleanup: kill shells, close fds.
    (setf *running* nil)
    (dolist (pane (all-panes session))
      (ignore-errors (pty-close (pane-fd pane) (pane-pid pane))))))

;;; ── Startup mode dispatch (data / logic separation) ─────────────────────────
;;;
;;; *startup-modes* is the DATA: a map from mode-name strings to handler
;;; functions.  main is the LOGIC: it looks up the mode and dispatches.
;;; Adding a new mode only requires adding an entry to the alist, not changing
;;; the dispatch logic.

(defparameter *startup-modes*
  '(("server" . run-server)
    ("attach" . run-client))
  "Mode-name → function-name (symbol) dispatch table for the binary entry point.
   Storing symbols (not function objects) means test stubs that rebind the
   function cell with SETF FDEFINITION are honoured at dispatch time.")

(defun main ()
  "Binary entry point — dispatches on the first argv item.
   Unrecognized or absent modes fall through to run-standalone."
  (let* ((args    (rest sb-ext:*posix-argv*))
         (mode    (first args))
         (name    (or (second args) "0"))
         (handler (cdr (assoc mode *startup-modes* :test #'equal))))
    (if handler
        (funcall handler name)
        (run-standalone))))
