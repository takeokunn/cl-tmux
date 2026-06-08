(in-package #:cl-tmux)

;;;; Key bindings, synchronize-panes, event loop.

;;; ── Additional key bindings ─────────────────────────────────────────────────
;;;
;;; These extend config.lisp's prefix defaults.  They live in a FUNCTION (not
;;; top-level side effects) so test isolation (with-isolated-config) can reinstall
;;; them onto a fresh *key-tables*, keeping the isolated table consistent with the
;;; live image (e.g. C-b z = zoom-toggle, C-b L = last-session).

(defun install-extended-key-bindings ()
  "Install the prefix bindings that extend config.lisp's defaults.  Idempotent.
   Called once at load time, and again by with-isolated-config under test."
  ;; Session management
  ;; C-b s — choose-session (interactive overlay listing all sessions)
  (set-key-binding #\s :choose-session)
  ;; C-b ( / C-b ) — switch to prev/next session
  (set-key-binding #\( :switch-client-prev)
  (set-key-binding #\) :switch-client-next)
  ;; C-b L — last-session (switch to most recently active previous session)
  (set-key-binding #\L :last-session)
  ;; C-b D — choose-client (show overlay with client info)
  (set-key-binding #\D :choose-client)
  ;; Window management
  ;; C-b w — choose-window (interactive menu listing all windows)
  (set-key-binding #\w :choose-window)
  ;; C-b l — last-window (switch to previously active window)
  (set-key-binding #\l :last-window)
  ;; C-b f — find-window (search window names and pane titles)
  (set-key-binding #\f :find-window)
  ;; C-b . — move-window-prompt (prompt for target index and move active window)
  (set-key-binding #\. :move-window-prompt)
  ;; C-b ' — select-window-prompt (prompt for window index or name)
  (set-key-binding #\' :select-window-prompt)
  ;; C-b E — select-layout-spread (even-horizontal layout alias)
  (set-key-binding #\E :select-layout-spread)
  ;; C-b Space — next-layout (cycle through layouts)
  (set-key-binding (code-char 32) :next-layout)
  ;; Pane management
  ;; C-b ! — break-pane (move active pane to a new window)
  (set-key-binding #\! :break-pane)
  ;; C-b { / C-b } — swap-pane backward / forward
  (set-key-binding #\{ :swap-pane-backward)
  (set-key-binding #\} :swap-pane-forward)
  ;; C-b ; — last-pane (jump to previously active pane)
  (set-key-binding #\; :last-pane)
  ;; C-b q — display-panes (show pane numbers)
  (set-key-binding #\q :display-panes)
  ;; C-b z — zoom-toggle (zoom in/out on active pane) — standard lowercase tmux binding
  (set-key-binding #\z :zoom-toggle)
  ;; C-b m — mark-pane (set the marked pane)
  (set-key-binding #\m :mark-pane)
  ;; C-b M — clear-mark (clear the marked pane)
  (set-key-binding (code-char 77) :clear-mark)
  ;; Copy/paste/buffers
  ;; C-b C-b — send-prefix (forward one literal C-b byte to the active pane)
  (set-key-binding (code-char 2) :send-prefix)
  ;; C-b # — list-buffers (show all paste buffers)
  (set-key-binding (code-char 35) :list-buffers)
  ;; C-b = — choose-buffer (interactively pick a paste buffer)
  (set-key-binding (code-char 61) :choose-buffer)
  ;; C-b - — delete-buffer (delete most recent paste buffer)
  (set-key-binding (code-char 45) :delete-buffer)
  ;; Misc
  ;; C-b : — command-prompt (open interactive command line)
  (set-key-binding #\: :command-prompt)
  ;; C-b r — refresh-client (force a full terminal redraw)
  (set-key-binding #\r :refresh-client)
  ;; C-b t — clock-mode (toggle digital clock overlay on active pane)
  (set-key-binding #\t :clock-mode)
  ;; C-b i — display-info (show session/window/pane info summary)
  (set-key-binding #\i :display-info)
  ;; C-b ~ — show-messages (show recent display-message log)
  (set-key-binding (code-char 126) :show-messages)
  ;; C-b C-o (15) — rotate-window (rotate panes forward, matching real tmux)
  (set-key-binding (code-char 15) :rotate-window)
  (values))

;; Install once at load time so the running image has the full default set.
(install-extended-key-bindings)

;;; *prefix-active* is set to T while waiting for the command key after the
;;; prefix key (C-b).  The format engine reads it for #{client_prefix}.
;;; Updated by process-byte; written on the event-loop thread only.
(defvar *prefix-active* nil
  "T when the input state machine is in %after-prefix-input-state (prefix pressed,
   waiting for command key).  Exposed as #{client_prefix} in format strings.")

(defstruct input-state
  "Opaque CPS keystroke-processing state. Holds the current continuation.
   REPEAT-ENTERED-AT is set (via GET-INTERNAL-REAL-TIME) when the state
   transitions to a repeatable binding so that repeat-time can be honoured.
   ESC-ENTERED-AT is set when we begin accumulating an escape sequence; used by
   %flush-esc-if-timed-out to implement the tmux escape-time disambiguation window."
  (continuation #'%ground-input-state :type function)
  (repeat-entered-at nil)
  (esc-entered-at nil))

(defun %reset-repeat-if-expired (state)
  "If STATE is in a repeatable prefix position and REPEAT-TIME ms have elapsed
   since REPEAT-ENTERED-AT, reset to ground state.  Otherwise a no-op.
   Called once per event-loop iteration before processing any new byte."
  (when (input-state-repeat-entered-at state)
    (let* ((repeat-ms  (or (cl-tmux/options:get-option "repeat-time") 500))
           (elapsed-ms (/ (- (get-internal-real-time)
                              (input-state-repeat-entered-at state))
                           (/ internal-time-units-per-second 1000))))
      (when (>= elapsed-ms repeat-ms)
        (setf (input-state-continuation state) #'%ground-input-state
              (input-state-repeat-entered-at state) nil)))))

(defun process-byte (session byte state)
  "Feed BYTE to SESSION through the CPS keystroke pipeline STATE.
   Returns :QUIT, :DETACH, or NIL. Mutates STATE's continuation in place."
  (multiple-value-bind (outcome next)
      (funcall (input-state-continuation state) session byte)
    (let ((new-cont (or next #'%ground-input-state)))
      (setf (input-state-continuation state) new-cont)
      ;; Track entry into repeat mode: a :repeatable outcome means the binding
      ;; had the -r flag; stamp the timestamp so %reset-repeat-if-expired works.
      (when (eq outcome :repeatable)
        (setf (input-state-repeat-entered-at state) (get-internal-real-time)))
      ;; Leaving repeat mode: clear the timestamp.
      (unless (eq new-cont #'%after-prefix-input-state)
        (setf (input-state-repeat-entered-at state) nil))
      ;; Track prefix state for #{client_prefix} format variable.
      (setf *prefix-active* (eq new-cont #'%after-prefix-input-state))
      ;; Track ESC accumulation: stamp esc-entered-at when we receive a lone ESC
      ;; byte (byte 27) and transition OUT of ground state (entering escape-input-k).
      ;; Clear it when we return to ground (sequence completed or aborted).
      (cond
        ((and (= byte +byte-esc+)
              (not (eq new-cont #'%ground-input-state))
              (not (eq new-cont #'%after-prefix-input-state)))
         (setf (input-state-esc-entered-at state) (get-internal-real-time)))
        ((eq new-cont #'%ground-input-state)
         (setf (input-state-esc-entered-at state) nil))))
    outcome))

(defun %flush-esc-if-timed-out (state session)
  "If escape-time ms have elapsed since we started accumulating an ESC sequence
   with no follow-up byte, forward a lone ESC to the active pane and reset to ground.
   Implements the tmux 'escape-time' server option (default 500ms).
   Critical for vim/neovim: lone ESC in insert mode must reach the program promptly."
  (when (input-state-esc-entered-at state)
    (let* ((esc-ms   (or (cl-tmux/options:get-server-option "escape-time") 500))
           (elapsed  (/ (- (get-internal-real-time)
                            (input-state-esc-entered-at state))
                         (/ internal-time-units-per-second 1000))))
      (when (>= elapsed esc-ms)
        ;; Forward the lone ESC byte to the active pane.
        (let* ((win  (session-active-window session))
               (pane (and win (window-active-pane win))))
          (when (and pane (> (pane-fd pane) 0))
            (pty-write (pane-fd pane)
                       (make-array 1 :element-type '(unsigned-byte 8)
                                     :initial-element +byte-esc+))))
        (setf (input-state-continuation state) #'%ground-input-state
              (input-state-esc-entered-at state) nil)
        (setf *dirty* t)))))

;;; ── Synchronize-panes broadcast ─────────────────────────────────────────────
;;;
;;; When the "synchronize-panes" window option is T, keystrokes sent to the
;;; active pane are also broadcast to every other pane in the same window.

(defun %forward-octets-synchronized (session octets)
  "Forward OCTETS to the active pane.  If synchronize-panes is enabled on
   the active window, also write to all other panes in the window."
  (let* ((window      (session-active-window session))
         (active-pane (and window (window-active-pane window))))
    (when active-pane
      (pty-write (pane-fd active-pane) octets)
      ;; Broadcast when synchronize-panes is enabled.
      (when (cl-tmux/options:get-option "synchronize-panes")
        (dolist (pane (window-panes window))
          (unless (eq pane active-pane)
            (ignore-errors (pty-write (pane-fd pane) octets))))))))

;;; -- Main event loop --------------------------------------------------------

(defun %handle-resize (session)
  "Re-read terminal geometry and relayout the active window after SIGWINCH."
  (setf *resize-pending* nil)
  (multiple-value-setq (*term-rows* *term-cols*) (terminal-size))
  (let ((window (session-active-window session)))
    (when window
      (window-relayout window (- *term-rows* *status-height*) *term-cols*))))

(defun %auto-rename-name (session window pane screen)
  "Compute the new automatic window name using automatic-rename-format option.
   For panes with no real process (pid <= 0), prefer the OSC 0/2 screen title
   directly; the format-string result would just be the shell basename fallback.
   Falls back to the OSC 0/2 screen title when the format yields an empty string."
  (let* ((has-real-process (let ((pid (and pane (cl-tmux/model:pane-pid pane))))
                             (and pid (> pid 0))))
         (osc-title (screen-title screen)))
    (if (not has-real-process)
        ;; No real PTY: OSC title is more meaningful than the shell-basename fallback.
        (if (plusp (length osc-title)) osc-title "")
        ;; Real PTY: expand the automatic-rename-format option.
        (let* ((fmt (or (cl-tmux/options:get-option "automatic-rename-format")
                        "#{pane_current_command}"))
               (ctx (cl-tmux/format:format-context-from-session session window pane))
               (new-name (cl-tmux/format:expand-format fmt ctx)))
          (if (and new-name (plusp (length new-name)))
              new-name
              ;; Fallback to OSC 0/2 screen title
              (if (plusp (length osc-title)) osc-title ""))))))

(defun %maybe-rename-window-from-title (session)
  "If automatic-rename is enabled for the active window, update its name using
   automatic-rename-format (default: #{pane_current_command}).  Falls back to
   the OSC 0/2 screen title.  Routed through RENAME-WINDOW for hooks."
  (let* ((active-pane   (session-active-pane session))
         (screen        (when active-pane (pane-screen active-pane)))
         (active-window (session-active-window session)))
    (when (and screen active-window
               (window-automatic-rename-p active-window)
               (cl-tmux/options:get-option "allow-rename"))
      (let ((new-name (%auto-rename-name session active-window active-pane screen)))
        (when (and (plusp (length new-name))
                   (not (string= new-name (window-name active-window))))
          (rename-window active-window new-name)
          (setf *dirty* t))))))

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
      ;; Honour repeat-time: reset to ground when the repeat window closes.
      (%reset-repeat-if-expired state)
      ;; Honour escape-time: forward a lone ESC to the pane when no follow-up byte
      ;; has arrived within escape-time ms (critical for vim ESC key in insert mode).
      (%flush-esc-if-timed-out state session)
      (let ((byte (read-byte-nonblock +poll-timeout-us+)))
        (if byte
            (progn
              (setf idle-counter 0)
              ;; Stamp last-activity-time so lock-after-time can measure idle.
              (setf *last-activity-time* (get-universal-time))
              (when (member (process-byte session byte state) '(:quit :detach))
                (setf *running* nil)))
            (progn
              (incf idle-counter)
              (when (>= idle-counter +event-loop-max-idle-iterations+)
                (setf idle-counter 0)
                (sleep +event-loop-idle-sleep-seconds+)))))
      (when *resize-pending* (%handle-resize session))
      (when *dirty*           (%handle-dirty session)))))
