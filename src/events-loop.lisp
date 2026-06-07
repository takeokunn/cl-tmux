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

(defstruct input-state
  "Opaque CPS keystroke-processing state. Holds the current continuation."
  (continuation #'%ground-input-state :type function))

(defun process-byte (session byte state)
  "Feed BYTE to SESSION through the CPS keystroke pipeline STATE.
   Returns :QUIT, :DETACH, or NIL. Mutates STATE's continuation in place."
  (multiple-value-bind (outcome next)
      (funcall (input-state-continuation state) session byte)
    (setf (input-state-continuation state) (or next #'%ground-input-state))
    outcome))

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

(defun %maybe-rename-window-from-title (session)
  "If the active pane has set an OSC title and the window's automatic-rename
   option is enabled, propagate the title to the active window name.
   Window renaming is routed through RENAME-WINDOW (commands layer) to ensure
   hook dispatch and consistent state mutation."
  (let* ((active-pane   (session-active-pane session))
         (screen        (when active-pane (pane-screen active-pane)))
         (active-window (session-active-window session)))
    (when (and screen active-window
               (window-automatic-rename-p active-window)
               (not (string= (screen-title screen) ""))
               (not (string= (screen-title screen) (window-name active-window))))
      (rename-window active-window (screen-title screen))
      (setf *dirty* t))))

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
      (let ((byte (read-byte-nonblock +poll-timeout-us+)))
        (if byte
            (progn
              (setf idle-counter 0)
              (when (member (process-byte session byte state) '(:quit :detach))
                (setf *running* nil)))
            (progn
              (incf idle-counter)
              (when (>= idle-counter +event-loop-max-idle-iterations+)
                (setf idle-counter 0)
                (sleep +event-loop-idle-sleep-seconds+)))))
      (when *resize-pending* (%handle-resize session))
      (when *dirty*           (%handle-dirty session)))))
