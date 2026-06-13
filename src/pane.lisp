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
  (pipe-fd  nil)                      ; NIL or file-descriptor for pipe-pane output tee
  ;; ── Terminal emulator ─────────────────────────────────────────────────────
  (screen   nil)
  ;; ── Window back-pointer and state ─────────────────────────────────────────
  (window   nil)                      ; back-pointer to the owning window (set on attach)
  (marked           nil)              ; T when this pane is the marked pane (C-b m)
  (input-disabled   nil :type boolean) ; T when select-pane -d disables input
  ;; ── Identity strings ──────────────────────────────────────────────────────
  (title    "" :type string)          ; pane title set via OSC 0/2 (#{pane_title})
  (tty      "" :type string)          ; slave PTY device path, e.g. /dev/pts/3 (#{pane_tty})
  ;; ── Per-pane option overrides ─────────────────────────────────────────────
  (local-options (make-hash-table :test #'equal) :type hash-table))

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
          (pty-write (pane-fd pane) reply))))))

(defun pane-feed (pane bytes)
  "Feed raw PTY bytes into PANE's screen, then drain any device-report replies
   (DA1/DA2/CPR/DSR/DECRQM/XTGETTCAP/DECRQSS/OSC-color) back to the PTY.
   The response queue is populated by the CPS parser under the screen lock;
   it is drained outside the lock so pty-write never blocks while holding it."
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

;;; ── Shared option-reading helper for fork operations ───────────────────────
;;;
;;; Both %fork-pane and respawn-pane read the same two options and apply the
;;; same (and … (plusp (length …))) guard.  %read-shell-fork-options captures
;;; that shared logic in one named step.

(defun %read-shell-fork-options ()
  "Read the 'default-terminal' and 'default-command' options for PTY fork calls.
   Returns (values term-or-nil command-or-nil) where a value is NIL when the
   option is unset or empty — matching the guard (and val (plusp (length val)))."
  (let ((term (cl-tmux/options:get-option "default-terminal"))
        (cmd  (cl-tmux/options:get-option "default-command")))
    (values (and term (plusp (length term)) term)
            (and cmd  (plusp (length cmd))  cmd))))

(defun %forkpty-with-default-options (h w &key start-dir extra-env)
  "Fork a PTY shell using the configured default-terminal and default-command.
   Returns (values fd pid slave-path).  Shared by %fork-pane and respawn-pane."
  (multiple-value-bind (term command) (%read-shell-fork-options)
    (forkpty-with-shell h w
                        :start-dir start-dir
                        :term term
                        :default-command command
                        :extra-env extra-env)))

(defun %fork-pane (id x y w h &key start-dir)
  "Fork a shell and build a PTY-backed pane at position (X,Y) sized W×H.
   START-DIR: when non-NIL, the child shell is started in that directory.
   The TERM environment variable is set from the 'default-terminal' option.
   When 'default-command' is set to a non-empty string, it is run via sh -c.
   Extra environment variables may be injected via the *PANE-EXTRA-ENV* dynamic
   variable (alist of (NAME . VALUE)), which is consumed once and reset.
   Returns the new pane.  The PTY file descriptor and child PID are embedded
   in the pane struct; callers should call pty-close on them at teardown."
  ;; Merge update-environment vars with *pane-extra-env*.
  ;; *pane-extra-env* entries take precedence (placed last = later setenv).
  (let ((environment-pairs (append (get-update-environment-vars) *pane-extra-env*)))
    ;; Consume *pane-extra-env*: reset so a later fork without -e starts clean.
    (setf *pane-extra-env* nil)
    (multiple-value-bind (fd pid slave-path)
        (%forkpty-with-default-options h w :start-dir start-dir :extra-env environment-pairs)
      (make-pane :id id :x x :y y :width w :height h
                 :fd fd :pid pid :tty (or slave-path "")
                 :screen (make-screen w h)))))

(defun respawn-pane (pane)
  "Restart PANE's PTY process, keeping geometry and screen intact.
   Closes the old PTY fd (sending SIGHUP to the child), forks a fresh shell on
   a new PTY, and updates the pane's FD and PID.  The existing screen is
   preserved so the renderer can continue without a layout change.
   Returns the updated pane."
  (let ((old-fd  (pane-fd  pane))
        (old-pid (pane-pid pane))
        (w       (pane-width  pane))
        (h       (pane-height pane)))
    ;; Close the old PTY; ignore errors (process may have already exited).
    (ignore-errors (pty-close old-fd old-pid))
    ;; Open a fresh PTY-backed shell at the same geometry, respecting options.
    (multiple-value-bind (new-fd new-pid slave-path)
        (%forkpty-with-default-options h w)
      (setf (pane-fd  pane) new-fd
            (pane-pid pane) new-pid
            (pane-tty pane) (or slave-path "")))
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

(defun %pane-border-status-reservation (height)
  "Return (values CONTENT-Y-OFFSET CONTENT-HEIGHT) for a pane allocated HEIGHT rows,
   reserving ONE row within the allocation for the pane-border-status title line
   when the option is \"top\"/\"bottom\" and there is room (height > 1):
     top    → offset 1 (title on the allocated top row), content height-1
     bottom → offset 0 (title on the allocated bottom row), content height-1
     off / too short → offset 0, full height (no reservation).
   The pane's geometry becomes the CONTENT rectangle; the title row sits just
   outside it (drawn by %render-pane-border-status)."
  (let ((status (cl-tmux/options:get-option "pane-border-status" "off")))
    (if (and (not (string= status "off")) (not (string= status "")) (> height 1))
        (values (if (string= status "top") 1 0) (1- height))
        (values 0 height))))

(defun pane-reposition (pane x y width height)
  "Move and resize PANE to X,Y with WIDTH x HEIGHT.
   Updates the geometry slots, then resizes the underlying PTY and virtual screen.
   When pane-border-status is on, one row of the allocation is reserved for the
   title line, so the pane's CONTENT geometry (and the app's PTY/screen) is one
   row shorter — the title no longer overwrites pane content."
  (multiple-value-bind (content-y-offset content-height)
      (%pane-border-status-reservation height)
    (%update-pane-geometry pane x (+ y content-y-offset) width content-height)
    (set-pty-size (pane-fd pane) content-height width)
    (let ((screen (pane-screen pane)))
      (with-lock-held ((screen-lock screen))
        (screen-resize screen width content-height)))))
