(in-package #:cl-tmux/model)

;;; ── Pane ───────────────────────────────────────────────────────────────────

(defstruct pane
  "One terminal pane: a PTY fd + virtual screen + position within its window."
  ;; ── Identity ──────────────────────────────────────────────────────────────
  (id       0   :type fixnum)
  ;; ── Geometry ──────────────────────────────────────────────────────────────
  (x        0   :type fixnum)
  (y        0   :type fixnum)
  (width    80  :type fixnum)
  (height   24  :type fixnum)
  ;; ── PTY file descriptors ──────────────────────────────────────────────────
  (fd       -1  :type fixnum)         ; master PTY file descriptor
  (pid      -1  :type fixnum)         ; child process PID
  (pipe-fd  nil)                      ; NIL or stream for pipe-pane output tee
  (pipe-output-stream nil)            ; NIL or stream for command stdout -> pane
  (pipe-output-thread nil)            ; NIL or copier thread for command stdout
  (pipe-process nil)                  ; NIL or uiop process-info for pipe-pane command
  ;; ── Terminal emulator ─────────────────────────────────────────────────────
  (screen   nil)
  ;; ── Window back-pointer and state ─────────────────────────────────────────
  (window   nil)                      ; back-pointer to the owning window (set on attach)
  (marked           nil)              ; T when this pane is the marked pane (C-b m)
  (input-disabled   nil :type boolean) ; T when select-pane -d disables input
  ;; ── Identity strings ──────────────────────────────────────────────────────
  (title    "" :type string)          ; pane title set via OSC 0/2 (#{pane_title})
  (tty      "" :type string)          ; slave PTY device path, e.g. /dev/pts/3 (#{pane_tty})
  ;; ── Death record (remain-on-exit / #{pane_dead_status} family) ───────────
  (dead-status nil)                   ; NIL or exit code of the dead child
  (dead-signal nil)                   ; NIL or terminating signal number
  (dead-time   nil)                   ; NIL or universal-time when the pane died
  ;; ── Per-pane option overrides ─────────────────────────────────────────────
  (local-options (make-hash-table :test #'equal) :type hash-table))

(defun pane-pipe-active-p (pane)
  "Return T when PANE has any pipe-pane direction active."
  (and pane
       (or (pane-pipe-fd pane)
           (pane-pipe-output-stream pane)
           (pane-pipe-output-thread pane)
           (pane-pipe-process pane))))

(defun pane-live-p (pane)
  "Return T when PANE still has a live PTY master fd."
  (and pane (> (pane-fd pane) 0)))

;;; ── Response-queue drain helper (logic layer) ──────────────────────────────
;;;
;;; Draining pending terminal-query responses lives here as a named step so that
;;; pane-feed can express the "drain" concern independently of the "process" concern.
;;; The queue is populated by the CPS parser under the screen lock; it is drained
;;; outside the lock so pty-write never blocks while holding the screen lock.

(defun %drain-response-queue (pane screen)
  "Drain SCREEN's response queue, writing each reply to PANE's PTY fd.
   Replies are reversed from newest-first to arrival order before writing.
   Pure I/O at the orchestration boundary — no screen struct mutation.
   Returns NIL."
  (when (screen-response-queue screen)
    (let ((replies (nreverse (screen-response-queue screen))))
      (setf (screen-response-queue screen) nil)
      (when (> (pane-fd pane) 0)
        (dolist (reply replies)
          (write-pty (pane-fd pane) reply))))))

(defun pane-feed (pane bytes)
  "Feed raw PTY bytes into PANE's screen, then drain any device-report replies
   (DA1/DA2/CPR/DSR/DECRQM/XTGETTCAP/DECRQSS/OSC-color) back to the PTY.
   The response queue is populated by the CPS parser under the screen lock;
   it is drained outside the lock so write-pty never blocks while holding it."
  (let ((screen (pane-screen pane)))
    (with-lock-held ((screen-lock screen))
      (screen-process-bytes screen bytes))
    (%drain-response-queue pane screen)))

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
  (multiple-value-bind (fd pid slave-path)
      (%spawn-shell-for-pane session rows cols :start-dir start-dir)
    (make-pane :id id :x x :y y :width cols :height rows
               :fd fd :pid pid :tty (or slave-path "")
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
            ;; The pane is alive again — clear the death record so
            ;; #{pane_dead_status} and friends read empty.
            (pane-dead-status pane) nil
            (pane-dead-signal pane) nil
            (pane-dead-time pane) nil))
    pane))

;;; ── pane-reposition ──────────────────────────────────────────────────────────
;;;
;;; Data/logic separation mirrors the zoom helpers in window.lisp:
;;;   %update-pane-geometry — pure slot mutation (data)
;;;   pane-reposition       — geometry update then PTY/screen resize (effects)

(defun %update-pane-geometry (pane x y width height)
  "Update PANE's position and dimension slots to X, Y, WIDTH, HEIGHT.
   Pure data mutation — no I/O side effects."
  (setf (pane-x pane)      x
        (pane-y pane)      y
        (pane-width  pane) width
        (pane-height pane) height))

(defun %pane-border-status-reservation (status height)
  "Return (values CONTENT-Y-OFFSET CONTENT-HEIGHT) for a pane allocated HEIGHT rows,
   given the STATUS string from the pane-border-status option.
   When STATUS is \"top\" or \"bottom\" and HEIGHT > 1, one row is reserved for
   the border-status title line:
     \"top\"    → offset 1 (title on the allocated top row), content height-1
     \"bottom\" → offset 0 (title on the allocated bottom row), content height-1
     \"off\" / \"\" / too short → offset 0, full height (no reservation).
   PURE function: STATUS is passed in rather than read from the option store,
   enforcing data/logic separation.  The pane's geometry becomes the CONTENT
   rectangle; the title row is drawn by %render-pane-border-status."
  (if (and (not (member status '("off" "") :test #'string=)) (> height 1))
      (values (if (string= status "top") 1 0) (1- height))
      (values 0 height)))

(defun pane-reposition (pane x y width height)
  "Move and resize PANE to X,Y with WIDTH x HEIGHT.
   Updates the geometry slots, then resizes the underlying PTY and virtual screen.
   When pane-border-status is on, one row of the allocation is reserved for the
   title line, so the pane's CONTENT geometry (and the app's PTY/screen) is one
   row shorter — the title no longer overwrites pane content."
  (let ((status (cl-tmux/options:get-option "pane-border-status" "off")))
    (multiple-value-bind (content-y-offset content-height)
        (%pane-border-status-reservation status height)
      (%update-pane-geometry pane x (+ y content-y-offset) width content-height)
      (when (> (pane-fd pane) 0)
        (resize-pty (pane-fd pane) content-height width))
      (let ((screen (pane-screen pane)))
        (with-lock-held ((screen-lock screen))
          (screen-resize screen width content-height))))))
