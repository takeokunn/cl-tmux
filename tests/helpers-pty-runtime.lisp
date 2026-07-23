(in-package #:cl-tmux/test)

;;; ── PTY availability probe (test-only) ─────────────────────────────────────
;;;
;;; pty-available-p is a testing artifact: it spawns a real shell and immediately
;;; kills it purely to check PTY access.  It lives here (test helpers) rather
;;; than in production source so the production pty.lisp has no test-only code.

(defun pty-available-p ()
  "Return T if a PTY-backed shell can be spawned on this system, NIL otherwise.
   Used as a skip guard in integration tests that require /dev/ptmx."
  (handler-case
      (multiple-value-bind (fd pid) (forkpty-with-shell 8 20)
        (cl-tmux/pty:pty-close fd pid)
        t)
    (error () nil)))

(defmacro with-pty-available (&body body)
  "Run BODY only when PTY-backed shells are available."
  `(when (pty-available-p)
     ,@body))

(defmacro with-pty-session (session-spec &body body)
  "Run BODY in a fake session only when PTY-backed shells are available."
  (let ((session-var (if (consp session-spec) (first session-spec) session-spec))
        (session-args (if (consp session-spec) (rest session-spec) nil)))
    `(with-pty-available
       (with-fake-session (,session-var ,@session-args)
         ,@body))))

(defmacro with-pty-run-command-line-overlay ((session-spec command &key context)
                                             &body body)
  "Run %RUN-COMMAND-LINE for COMMAND in a fake session only when PTYs exist."
  (let ((session-var (if (consp session-spec) (first session-spec) session-spec))
        (session-args (if (consp session-spec) (rest session-spec) nil)))
    `(with-pty-session (,session-var ,@session-args)
       (with-run-command-line-overlay (,session-var ,command :context ,context)
         ,@body))))

(defmacro with-pty-command-preserving-focus ((session-spec command &key count-form active-form
                                                           count-context focus-context)
                                              &body body)
  "Run COMMAND in a PTY-backed fake session and assert it changes COUNT-FORM
   while leaving ACTIVE-FORM unchanged."
  (let ((session-var (if (consp session-spec) (first session-spec) session-spec))
        (session-args (if (consp session-spec) (rest session-spec) nil)))
    `(with-pty-session (,session-var ,@session-args)
       (let ((before-count ,count-form)
             (before-active ,active-form))
         (cl-tmux::%run-command-line ,session-var ,command)
         (let ((after-count ,count-form))
           (expect (> after-count before-count)))
         (expect (eq before-active ,active-form))
         ,@body))))

(defmacro with-pty-command-increasing-count ((session-spec command &key count-form count-context)
                                             &body body)
  "Run COMMAND in a PTY-backed fake session and assert it increases COUNT-FORM."
  (let ((session-var (if (consp session-spec) (first session-spec) session-spec))
        (session-args (if (consp session-spec) (rest session-spec) nil)))
    `(with-pty-session (,session-var ,@session-args)
       (let ((before-count ,count-form))
         (cl-tmux::%run-command-line ,session-var ,command)
         (expect (> ,count-form before-count))
         ,@body))))

(defun %forkpty-with-retry (rows cols &key (attempts 3))
  "Spawn a PTY shell, retrying transient allocation failures.
   Even after pty-available-p succeeds, the sandboxed builder can transiently
   run out of PTYs (SBCL signals \"could not find a pty\"), so a one-shot
   spawn makes the suite flaky.  Returns (values fd pid), or NIL when every
   attempt failed."
  (loop repeat attempts
        do (handler-case
               (multiple-value-bind (fd pid) (forkpty-with-shell rows cols)
                 (return (values fd pid)))
             (error () (sleep 0.1)))
        finally (return nil)))

(defmacro with-pty-shell ((fd-var pid-var &key (rows 24) (cols 80)) &body body)
  "Spawn a shell on a fresh PTY of ROWS×COLS; bind FD-VAR and PID-VAR.
   Closes the PTY via unwind-protect on exit, even if BODY signals.
   Skips the enclosing test when PTY allocation keeps failing transiently."
  `(multiple-value-bind (,fd-var ,pid-var) (%forkpty-with-retry ,rows ,cols)
     (if (null ,fd-var)
         (skip "PTY allocation failed transiently (sandboxed environment)")
         (unwind-protect
              (progn ,@body)
           (pty-close ,fd-var ,pid-var)))))

;;; ── PTY port initialization ─────────────────────────────────────────────────
;;;
;;; Any test that creates a real pane (create-initial-session, session-new-window,
;;; respawn-pane) goes through cl-tmux/ports:spawn-pty.  Install the CFFI adapter
;;; now so the port vars are non-NIL for the duration of the test run.
;;; Tests that need a mock port can rebind *spawn-pty* / *write-pty* / etc.
;;; around individual test bodies.

(cl-tmux/pty:install-pty-port)
