(in-package #:cl-tmux/model)

;;; ── Pane ───────────────────────────────────────────────────────────────────

(defstruct pane
  "One terminal pane: a PTY fd + virtual screen + position within its window."
  (id       0   :type fixnum)
  (x        0   :type fixnum)
  (y        0   :type fixnum)
  (width    80  :type fixnum)
  (height   24  :type fixnum)
  (fd       -1  :type fixnum)
  (pid      -1  :type fixnum)
  (screen   nil)
  (pipe-fd  nil)    ; NIL or file-descriptor for pipe-pane output tee
  (window   nil)    ; back-pointer to the owning window (set on attach)
  (marked   nil)    ; T when this pane is the marked pane (C-b m)
  (title    "" :type string)      ; pane title set via OSC 0/2 (#{pane_title})
  (local-options (make-hash-table :test #'equal) :type hash-table)) ; per-pane option overrides

(defun pane-feed (pane bytes)
  "Feed raw PTY bytes into PANE's screen, holding the screen lock."
  (let ((screen (pane-screen pane)))
    (with-lock-held ((screen-lock screen))
      (screen-process-bytes screen bytes))))

;;; ── PTY-backed pane factory ─────────────────────────────────────────────────
;;;
;;; Data/logic separation: %fork-pane encapsulates the "how to allocate a pane
;;; with a live shell behind it" into one named step, keeping callers free to
;;; express the "where to attach it" concern independently.

(defun %fork-pane (id x y w h &key start-dir)
  "Fork a shell and build a PTY-backed pane at position (X,Y) sized W×H.
   START-DIR: when non-NIL, the child shell is started in that directory.
   The TERM environment variable is set from the 'default-terminal' option.
   When 'default-command' is set to a non-empty string, it is run via sh -c.
   Returns the new pane.  The PTY file descriptor and child PID are embedded
   in the pane struct; callers should call pty-close on them at teardown."
  (let* ((term    (cl-tmux/options:get-option "default-terminal"))
         (cmd     (cl-tmux/options:get-option "default-command")))
    (multiple-value-bind (fd pid)
        (forkpty-with-shell h w
                            :start-dir start-dir
                            :term (and term (plusp (length term)) term)
                            :default-command (and cmd (plusp (length cmd)) cmd))
      (make-pane :id id :x x :y y :width w :height h
                 :fd fd :pid pid :screen (make-screen w h)))))

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
    (let* ((term (cl-tmux/options:get-option "default-terminal"))
           (cmd  (cl-tmux/options:get-option "default-command")))
      (multiple-value-bind (new-fd new-pid)
          (forkpty-with-shell h w
                              :term (and term (plusp (length term)) term)
                              :default-command (and cmd (plusp (length cmd)) cmd))
        (setf (pane-fd  pane) new-fd
              (pane-pid pane) new-pid)))
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

(defun pane-reposition (pane x y width height)
  "Move and resize PANE to X,Y with WIDTH x HEIGHT.
   Updates the geometry slots, then resizes the underlying PTY and virtual screen."
  (%update-pane-geometry pane x y width height)
  (set-pty-size (pane-fd pane) height width)
  (let ((screen (pane-screen pane)))
    (with-lock-held ((screen-lock screen))
      (screen-resize screen width height))))
