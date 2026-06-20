(in-package #:cl-tmux)

;;;; Key bindings, synchronize-panes, event loop.

;;; ── Additional key bindings ─────────────────────────────────────────────────
;;;
;;; These extend config.lisp's prefix defaults.  They live in a FUNCTION (not
;;; top-level side effects) so test isolation (with-isolated-config) can reinstall
;;; them onto a fresh *key-tables*, keeping the isolated table consistent with the
;;; live image (e.g. C-b z = zoom-toggle, C-b L = last-session).

(defparameter *extended-prefix-bindings*
  `((#\s :choose-session)
    (#\( :switch-client-prev)
    (#\) :switch-client-next)
    (#\L :last-session)
    (#\D :choose-client)
    (#\w :choose-window)
    (#\l :last-window)
    (#\f :find-window)
    (#\. :move-window-prompt)
    (#\' :select-window-prompt)
    (#\E :select-layout-spread)
    (,(code-char 32) :next-layout)
    ("M-1" '("select-layout" "even-horizontal"))
    ("M-2" '("select-layout" "even-vertical"))
    ("M-3" '("select-layout" "main-horizontal"))
    ("M-4" '("select-layout" "main-vertical"))
    ("M-5" '("select-layout" "tiled"))
    ("M-n" '("next-window" "-a"))
    ("M-p" '("previous-window" "-a"))
    ("M-o" '("rotate-window" "-D"))
    (#\! :break-pane)
    (#\{ :swap-pane-backward)
    (#\} :swap-pane-forward)
    (#\; :last-pane)
    (#\q :display-panes)
    (#\z :zoom-toggle)
    (#\m :mark-pane)
    (,(code-char 77) :clear-mark)
    ("PageUp" '("copy-mode-enter" "-u"))
    (,(code-char 2) :send-prefix)
    (,(code-char 35) :list-buffers)
    (,(code-char 61) :choose-buffer)
    (,(code-char 45) :delete-buffer)
    (#\: :command-prompt)
    (#\C '("customize-mode"))
    (#\r :refresh-client)
    (#\t :clock-mode)
    (#\i :display-info)
    (,(code-char 126) :show-messages)
    (,(code-char 15) :rotate-window)
    (,(code-char 26) :suspend-client))
  "Prefix bindings that extend config.lisp's defaults.")

(defun %install-extended-key-binding (binding)
  (destructuring-bind (key command) binding
    (key-table-bind +table-prefix+ key command)))

(defun install-extended-key-bindings ()
  "Install the prefix bindings that extend config.lisp's defaults.  Idempotent.
   Called once at load time, and again by with-isolated-config under test."
  (mapc #'%install-extended-key-binding *extended-prefix-bindings*)
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
         ;; Sequence completed or aborted: stop the escape-time timer and drop the
         ;; replay buffer so a later flush can't resend a stale partial sequence.
         (setf (input-state-esc-entered-at state) nil
               *esc-accum-buffer* nil))))
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
        (if (prompt-active-p)
            ;; Prompt-local ESC is a cancel key, not pane input.  The state
            ;; machine defers it briefly to distinguish lone ESC from arrows.
            (handle-prompt-key +byte-esc+)
            ;; Forward the full accumulated partial sequence to the active pane.
            ;; In the common vim case nothing has accumulated past the ESC, so
            ;; this is a lone ESC — identical to the historical behaviour.  When
            ;; a multi-byte partial is pending (e.g. a held Alt+O = ESC O),
            ;; replaying the whole buffer keeps every byte.
            (let* ((win   (session-active-window session))
                   (pane  (and win (window-active-pane win)))
                   (accum *esc-accum-buffer*)
                   (bytes (if (and accum (plusp (fill-pointer accum)))
                              (subseq accum 0 (fill-pointer accum))
                              (make-array 1 :element-type '(unsigned-byte 8)
                                            :initial-element +byte-esc+))))
              (when (and pane (cl-tmux/model:pane-live-p pane) (not *client-read-only*))
                (pty-write (pane-fd pane) bytes))))
        (setf (input-state-continuation state) #'%ground-input-state
              (input-state-esc-entered-at state) nil
              *esc-accum-buffer* nil)
        (setf *dirty* t)))))

;;; ── Synchronize-panes broadcast ─────────────────────────────────────────────
;;;
;;; When the "synchronize-panes" window option is T, keystrokes sent to the
;;; active pane are also broadcast to every other pane in the same window.

(defun %forward-octets-synchronized (session octets)
  "Forward OCTETS to the active pane.  If synchronize-panes is enabled on
   the active window, also write to all other panes in the window.
   Panes with pane-input-disabled set (select-pane -d) receive no input.
   No-op when *client-read-only* is set (attach-session -r)."
  (unless *client-read-only*
    (let* ((window      (session-active-window session))
           (active-pane (and window (window-active-pane window))))
      (when (and active-pane
                 ;; select-pane -d: input disabled for this pane — swallow keystrokes.
                 (not (pane-input-disabled active-pane)))
        (pty-write (pane-fd active-pane) octets)
        ;; Broadcast when synchronize-panes is enabled, skipping disabled panes.
        ;; Read the window-local override (falls back to global then default).
        (when (cl-tmux/options:get-option-for-context "synchronize-panes" :window window)
          (dolist (pane (window-panes window))
            (unless (or (eq pane active-pane)
                        (pane-input-disabled pane))
              (ignore-errors (pty-write (pane-fd pane) octets)))))))))

;;; -- Main event loop --------------------------------------------------------

(defun %handle-resize (session)
  "Re-read terminal geometry and relayout the active window after SIGWINCH."
  (setf *resize-pending* nil)
  (multiple-value-setq (*term-rows* *term-cols*) (terminal-size))
  (let ((window (session-active-window session)))
    (when window
      (window-relayout window (- *term-rows* *status-height*) *term-cols*)))
  ;; client-resized hook: the client terminal changed size (SIGWINCH).
  (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-client-resized+))

(defun %auto-rename-name (session window pane screen &key (allow-title t))
  "Compute the new automatic window name using automatic-rename-format option.
   For panes with no real process (pid <= 0), prefer the OSC 0/2 screen title
   directly; the format-string result would just be the shell basename fallback.
   Falls back to the OSC 0/2 screen title when the format yields an empty string.
   ALLOW-TITLE NIL (allow-rename off) suppresses the OSC-title fallback, so
   command-following still works but applications cannot rename via their title."
  (let* ((has-real-process (let ((pid (and pane (cl-tmux/model:pane-pid pane))))
                             (and pid (> pid 0))))
         (osc-title (if allow-title (screen-title screen) "")))
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
                ;; Per-window "automatic-rename" option (default on); honors
                ;; `set-window-option -w automatic-rename off`.  Independent of allow-rename:
               ;; command-following must keep working even with allow-rename off
               ;; (that option only governs app-set OSC titles, handled below).
               (cl-tmux/options:get-option-for-context
                "automatic-rename" :window active-window))
      ;; allow-rename (default on) gates only the app's OSC-title fallback inside
      ;; %auto-rename-name; `set -g allow-rename off` stops apps renaming windows
      ;; via their title without freezing automatic command-following.
      (let ((new-name (%auto-rename-name session active-window active-pane screen
                                         :allow-title (cl-tmux/options:get-option
                                                       "allow-rename"))))
        (when (and (plusp (length new-name))
                   (string/= new-name (window-name active-window)))
          ;; Auto-rename must NOT disable automatic-rename, or it would fire only
          ;; once; keep it on so the name keeps tracking the foreground process.
          (rename-window active-window new-name :disable-automatic-rename nil)
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
        (when *dirty*           (%handle-dirty session))))))
