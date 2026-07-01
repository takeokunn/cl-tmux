(in-package #:cl-tmux)

;;;; Main event loop: resize/dirty handling, automatic window renaming, and the
;;;; top-level read/dispatch/repaint cycle.

(defun %handle-resize (session)
  "Re-read terminal geometry and relayout the active window after SIGWINCH."
  (setf *resize-pending* nil)
  (multiple-value-setq (*term-rows* *term-cols*) (terminal-size))
  (let ((window (session-active-window session)))
    (when window
      (window-relayout window (- *term-rows* *status-height*) *term-cols*)))
  ;; client-resized hook: the client terminal changed size (SIGWINCH).
  (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-client-resized+))

;;; ── Automatic window renaming ────────────────────────────────────────────────
;;;
;;; %maybe-rename-window-from-title is called once per dirty repaint and drives
;;; a small fallback chain, each layer reading a different struct depending on
;;; what information is available for the active pane:
;;;
;;;   %maybe-rename-window-from-title (session)
;;;     reads the SESSION's active window/pane and the automatic-rename option;
;;;     the entry point, called from %handle-dirty.
;;;   %auto-rename-name (session window pane screen)
;;;     reads the PANE's pid to decide whether a real process is running.
;;;   %rename-from-format-string (session window pane screen)
;;;     reads WINDOW/PANE via a FORMAT-CONTEXT to expand automatic-rename-format.
;;;   %rename-from-osc-title (screen)
;;;     reads only the SCREEN's OSC 0/2 title string — the final fallback when
;;;     no process is running or the format expansion is empty.
;;;
;;; Precedence: automatic-rename-format (real process) → OSC 0/2 title (no
;;; process, or empty format) → no rename (empty title too).

(defun %rename-from-osc-title (screen allow-title)
  "Return the OSC 0/2 title recorded on SCREEN when ALLOW-TITLE is non-NIL and
   the title is non-empty, otherwise return the empty string.
   Final fallback of the rename chain: used by %auto-rename-name when no real
   process is running, and by %rename-from-format-string when the
   automatic-rename-format expansion yields an empty result."
  (if allow-title
      (let ((title (screen-title screen)))
        (if (plusp (length title)) title ""))
      ""))

(defun %rename-from-format-string (session window pane screen allow-title)
  "Expand automatic-rename-format for a real-PTY PANE and return the result.
   Falls back to (%rename-from-osc-title screen allow-title) when the format
   expansion yields an empty string."
  (let* ((format-string (or (cl-tmux/options:get-option "automatic-rename-format")
                            "#{pane_current_command}"))
         (context (cl-tmux/format:format-context-from-session session window pane))
         (expanded-name (cl-tmux/format:expand-format format-string context)))
    (if (and expanded-name (plusp (length expanded-name)))
        expanded-name
        (%rename-from-osc-title screen allow-title))))

(defun %auto-rename-name (session window pane screen &key (allow-title t))
  "Compute the new automatic window name for WINDOW using automatic-rename-format.
   For a PANE with no real process (pid <= 0), prefer the OSC 0/2 SCREEN title
   directly; the format-string result would just be the shell basename fallback.
   Falls back to the OSC 0/2 screen title when the format yields an empty string.
   ALLOW-TITLE NIL (allow-rename off) suppresses the OSC-title fallback, so
   command-following still works but applications cannot rename via their title."
  (let* ((pid (and pane (cl-tmux/model:pane-pid pane)))
         (has-real-process (and pid (> pid 0))))
    (if has-real-process
        (%rename-from-format-string session window pane screen allow-title)
        (%rename-from-osc-title screen allow-title))))

(defun %automatic-rename-enabled-p (active-window)
  "Return T when ACTIVE-WINDOW should have its name auto-tracked.
   Honors both the WINDOW-AUTOMATIC-RENAME-P struct flag and the per-window
   \"automatic-rename\" option (`set-window-option -w automatic-rename off`).
   Independent of allow-rename: command-following must keep working even with
   allow-rename off (that option only governs app-set OSC titles)."
  (and (window-automatic-rename-p active-window)
       (cl-tmux/options:get-option-for-context
        "automatic-rename" :window active-window)))

(defun %apply-automatic-rename (session active-window active-pane screen)
  "Compute the automatic name for ACTIVE-WINDOW and apply it via RENAME-WINDOW
   when it differs from the current name.  allow-rename (default on) gates
   only the app's OSC-title fallback inside %AUTO-RENAME-NAME; `set -g
   allow-rename off` stops apps renaming windows via their title without
   freezing automatic command-following."
  (let ((new-name (%auto-rename-name session active-window active-pane screen
                                     :allow-title (cl-tmux/options:get-option
                                                   "allow-rename"))))
    (when (and (plusp (length new-name))
               (string/= new-name (window-name active-window)))
      ;; Auto-rename must NOT disable automatic-rename, or it would fire only
      ;; once; keep it on so the name keeps tracking the foreground process.
      (rename-window active-window new-name :disable-automatic-rename nil)
      (setf *dirty* t))))

(defun %maybe-rename-window-from-title (session)
  "If automatic-rename is enabled for SESSION's active window, update its name
   using automatic-rename-format (default: #{pane_current_command}).  Falls
   back to the OSC 0/2 screen title.  Routed through RENAME-WINDOW for hooks."
  (let* ((active-pane   (session-active-pane session))
         (screen        (when active-pane (pane-screen active-pane)))
         (active-window (session-active-window session)))
    (when (and screen active-window (%automatic-rename-enabled-p active-window))
      (%apply-automatic-rename session active-window active-pane screen))))

(defun %handle-dirty (session)
  "Fit the active window to current terminal size and repaint."
  (setf *dirty* nil)
  (%maybe-rename-window-from-title session)
  (let ((window (session-active-window session)))
    (when window
      (ensure-window-fits window (- *term-rows* *status-height*) *term-cols*)))
  (render-session session *term-rows* *term-cols*))

;;; +event-loop-max-idle-iterations+ bounds the number of consecutive nil reads
;;; before the loop yields briefly, preventing a tight spin when *running* stays
;;; T but no I/O arrives (e.g., during a hang-debugging session).
(defconstant +event-loop-max-idle-iterations+ 10000
  "Maximum consecutive nil reads before the event loop yields (safety bound).
   At the poll timeout of +poll-timeout-us+ microseconds per read, 10 000 reads
   take at most (* 10000 +poll-timeout-us+) microseconds before the idle yield
   fires.  Readers can compute the worst-case idle latency from both constants.")

;;; +event-loop-idle-sleep-seconds+ is the duration of the brief yield that fires
;;; after +event-loop-max-idle-iterations+ consecutive nil reads.  Extracted to a
;;; named constant so its relationship to +poll-timeout-us+ is explicit.
(defconstant +event-loop-idle-sleep-seconds+ 0.001
  "Duration in seconds of the idle yield in the event loop (1 ms).
   Prevents CPU starvation when *running* is T but no bytes arrive for many
   consecutive poll cycles.  See also +event-loop-max-idle-iterations+.")

(defun %read-and-dispatch-one-byte (session state idle-counter)
  "Read at most one byte (bounded by +poll-timeout-us+) and route it to
   PROCESS-BYTE.  Returns the next IDLE-COUNTER value: reset to 0 on a byte
   arrival (also stamping *last-activity-time* and stopping the loop via
   *running* on :quit/:detach), or incremented — and yielded via
   +event-loop-idle-sleep-seconds+ once it reaches
   +event-loop-max-idle-iterations+ — when no byte arrived."
  (let ((byte (read-byte-nonblock +poll-timeout-us+)))
    (if byte
        (progn
          ;; Stamp last-activity-time so lock-after-time can measure idle.
          (setf *last-activity-time* (get-universal-time))
          (when (member (process-byte session byte state) '(:quit :detach))
            (setf *running* nil))
          0)
        (let ((next-idle-counter (1+ idle-counter)))
          (if (>= next-idle-counter +event-loop-max-idle-iterations+)
              (progn (sleep +event-loop-idle-sleep-seconds+) 0)
              next-idle-counter)))))

(defun %process-one-event-cycle (session state idle-counter)
  "Run one full iteration of the event loop's body for SESSION: resolve the
   most-recently-touched session, honour repeat-time/escape-time, read and
   dispatch one byte, then handle any pending resize/dirty repaint.
   Returns the next IDLE-COUNTER value (see %READ-AND-DISPATCH-ONE-BYTE)."
  ;; Follow the most-recently-touched session: session-switch commands
  ;; (switch-client, choose-tree, last-session) session-touch their target, and
  ;; re-resolving here makes the single client's display + input follow the
  ;; switch.  Falls back to the initial SESSION when the registry is empty.
  (let ((session (%current-session session)))
    ;; Honour repeat-time: reset to ground when the repeat window closes.
    (%reset-repeat-if-expired state)
    ;; Honour escape-time: forward a lone ESC to the pane when no follow-up byte
    ;; has arrived within escape-time ms (critical for vim ESC in insert mode).
    (%flush-esc-if-timed-out state session)
    (let ((next-idle-counter (%read-and-dispatch-one-byte session state idle-counter)))
      (when *resize-pending* (%handle-resize session))
      (when *dirty*           (%handle-dirty session))
      next-idle-counter)))

(defun event-loop (session)
  "In-process event loop: read stdin, route keystrokes, repaint on dirty.
   The loop is bounded by +event-loop-max-idle-iterations+ idle reads before
   yielding for +event-loop-idle-sleep-seconds+ seconds, so a stuck *running*
   flag cannot spin the CPU unboundedly.
   Worst-case idle latency ≈ (* +event-loop-max-idle-iterations+ +poll-timeout-us+)
   microseconds before the yield fires."
  (let ((state        (make-input-state))
        (idle-counter 0))
    (loop while *running* do
      (setf idle-counter (%process-one-event-cycle session state idle-counter)))))
