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
  ;; ── Spawn record (#{pane_start_command} / #{pane_start_path}) ────────────
  (start-command "" :type string)     ; resolved command the pane started with
  (start-path    "" :type string)     ; initial working directory
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

;;; Pane spawn and geometry helpers were split into:
;;;   - pane-geometry.lisp
;;;   - pane-spawn.lisp
