(in-package #:cl-tmux/model)

;;; ── PTY-backed pane factory ─────────────────────────────────────────────────
;;;
;;; Data/logic separation: %fork-pane encapsulates the "how to allocate a pane
;;; with a live shell behind it" into one named step, keeping callers free to
;;; express the "where to attach it" concern independently.

(defvar *pane-extra-env* nil
  "Dynamic variable: alist of (NAME . VALUE) pairs to set in the NEXT pane's
   child environment.  Bound by callers that need per-pane env vars (e.g.
   new-window -e VAR=val).  Consumed by %fork-pane and reset to NIL after use.")

;;; ── Shared option-reading helper for pane spawn operations ─────────────────
;;;
;;; Both %fork-pane and respawn-pane read the same two options and apply the
;;; same (and … (plusp (length …))) guard.  %read-shell-spawn-options captures
;;; that shared logic in one named step.

(defun %read-shell-spawn-options ()
  "Read the 'default-terminal' and 'default-command' options for PTY spawn calls.
   Returns (values term-or-nil command-or-nil) where a value is NIL when the
   option is unset or empty — matching the guard (and val (plusp (length val)))."
  (let ((term (cl-tmux/options:get-option "default-terminal"))
        (cmd  (cl-tmux/options:get-option "default-command")))
    (values (and term (plusp (length term)) term)
            (and cmd  (plusp (length cmd))  cmd))))

(defun %spawn-pty-with-default-options (rows cols &key start-dir default-command environment)
  "Spawn a PTY shell using the configured default-terminal and default-command.
   ROWS is the number of terminal rows; COLS is the number of terminal columns.
   Returns (values fd pid slave-path).  Shared by %fork-pane and respawn-pane.
   Calls the cl-tmux/ports:spawn-pty port (installed by install-pty-port)."
  (spawn-pty rows cols
             :start-dir start-dir
             :default-command default-command
             :environment environment))

;;; ── %spawn-shell-for-pane — shared spawn skeleton ───────────────────────────
;;;
;;; %fork-pane and respawn-pane both: (1) read the default-terminal/default-command
;;; options, (2) assemble a child environment that merges the session overlay with
;;; *pane-extra-env* (consuming and resetting it), then (3) spawn a PTY with the
;;; resolved default-command.  %spawn-shell-for-pane captures that shared skeleton;
;;; callers differ only in what they do with the resulting (fd pid slave-path).

(defun %spawn-shell-for-pane (session rows cols &key start-dir default-command extra-env)
  "Spawn a shell for a pane at COLS x ROWS, merging SESSION's environment overlay
   with EXTRA-ENV and the consumed *PANE-EXTRA-ENV*.
   DEFAULT-COMMAND overrides the configured 'default-command' option when given.
   Returns (values fd pid slave-path term command) — TERM and COMMAND are the
   resolved default-terminal/default-command options, returned so callers that
   need the resolved command (e.g. respawn-pane's :default-command fallback)
   do not have to read the options a second time."
  (multiple-value-bind (term command) (%read-shell-spawn-options)
    (let ((environment (session-child-environment session
                                                   :term term
                                                   :extra-env (append extra-env
                                                                      *pane-extra-env*))))
      ;; Consume *pane-extra-env*: reset so a later pane spawn without -e starts clean.
      (setf *pane-extra-env* nil)
      (multiple-value-bind (fd pid slave-path)
          (%spawn-pty-with-default-options rows cols
                                           :start-dir start-dir
                                           :default-command (or default-command command)
                                           :environment environment)
        (values fd pid slave-path term command)))))

(defun %fork-pane (session id x y cols rows &key start-dir)
  "Spawn a shell and build a PTY-backed pane at position (X,Y) sized COLS x ROWS.
   COLS is the number of terminal columns; ROWS is the number of terminal rows.
   START-DIR: when non-NIL, the child shell is started in that directory.
   SESSION supplies the child environment overlay used for spawn.
   When 'default-command' is set to a non-empty string, it is run via sh -c.
   Extra environment variables may be injected via the *PANE-EXTRA-ENV* dynamic
   variable (alist of (NAME . VALUE)), which is consumed once and reset.
   Returns the new pane.  The PTY file descriptor and child PID are embedded
   in the pane struct; callers should call close-pty on them at teardown."
  (multiple-value-bind (fd pid slave-path term command)
      (%spawn-shell-for-pane session rows cols :start-dir start-dir)
    (declare (ignore term))
    (make-pane :id id :x x :y y :width cols :height rows
               :fd fd :pid pid :tty (or slave-path "")
               :start-command (or command "")
               :start-path (or start-dir
                               (ignore-errors (sb-posix:getcwd))
                               "")
               :screen (make-screen cols rows))))

(defun %make-input-pane (id x y w h)
  "Build a pane without a backing PTY, used by split-window -I."
  (make-pane :id id :x x :y y :width w :height h
             :fd -1 :pid -1 :tty ""
             :screen (make-screen w h)))

(defun respawn-pane (session pane &key start-dir default-command extra-env)
  "Restart PANE's PTY process, keeping geometry and screen intact.
   Closes the old PTY fd (sending SIGHUP to the child), spawns a fresh shell on
   a new PTY, and updates the pane's FD and PID.  The existing screen is
   preserved so the renderer can continue without a layout change.
   Returns the updated pane."
  (let ((old-fd  (pane-fd  pane))
        (old-pid (pane-pid pane))
        (cols    (pane-width  pane))
        (rows    (pane-height pane)))
    ;; Close the old PTY; ignore errors (process may have already exited).
    (ignore-errors (close-pty old-fd old-pid))
    ;; Open a fresh PTY-backed shell at the same geometry, respecting options.
    (multiple-value-bind (new-fd new-pid slave-path)
        (%spawn-shell-for-pane session rows cols
                               :start-dir start-dir
                               :default-command default-command
                               :extra-env extra-env)
      (setf (pane-fd pane) new-fd
            (pane-pid pane) new-pid
            (pane-tty pane) (or slave-path "")
            (pane-start-command pane) (or default-command "")
            (pane-start-path pane) (or start-dir
                                       (ignore-errors (sb-posix:getcwd))
                                       "")
            ;; The pane is alive again — clear the death record so
            ;; #{pane_dead_status} and friends read empty.
            (pane-dead-status pane) nil
            (pane-dead-signal pane) nil
            (pane-dead-time pane) nil))
    pane))
