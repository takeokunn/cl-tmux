(in-package #:cl-tmux)

;;;; Key bindings, synchronize-panes, event loop.

;;; ── Additional key bindings ─────────────────────────────────────────────────

;;; Session management
;; C-b s — choose-session (interactive overlay listing all sessions)
(set-key-binding #\s :choose-session)
;; C-b ( / C-b ) — switch to prev/next session
(set-key-binding #\( :switch-client-prev)
(set-key-binding #\) :switch-client-next)
;; C-b L — last-session (switch to most recently active previous session)
(set-key-binding #\L :last-session)
;; C-b D — choose-client (show overlay with client info)
(set-key-binding #\D :choose-client)

;;; Window management
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

;;; Pane management
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

;;; Copy/paste/buffers
;; C-b C-b — send-prefix (forward one literal C-b byte to the active pane)
(set-key-binding (code-char 2) :send-prefix)
;; C-b # — list-buffers (show all paste buffers)
(set-key-binding (code-char 35) :list-buffers)
;; C-b = — choose-buffer (interactively pick a paste buffer)
(set-key-binding (code-char 61) :choose-buffer)
;; C-b - — delete-buffer (delete most recent paste buffer)
(set-key-binding (code-char 45) :delete-buffer)

;;; Misc
;; C-b : — command-prompt (open interactive command line)
(set-key-binding #\: :command-prompt)
;; C-b t — clock-mode (toggle digital clock overlay on active pane)
(set-key-binding #\t :clock-mode)
;; C-b i — display-info (show session/window/pane info summary)
(set-key-binding #\i :display-info)
;; C-b ~ — show-messages (show recent display-message log)
(set-key-binding (code-char 126) :show-messages)

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
  (let* ((win (session-active-window session))
         (ap  (and win (window-active-pane win))))
    (when ap
      (pty-write (pane-fd ap) octets)
      ;; Broadcast when synchronize-panes is enabled.
      (when (cl-tmux/options:get-option "synchronize-panes")
        (dolist (p (window-panes win))
          (unless (eq p ap)
            (ignore-errors (pty-write (pane-fd p) octets))))))))

;;; -- Main event loop --------------------------------------------------------

(defun %handle-resize (session)
  "Re-read terminal geometry and relayout the active window after SIGWINCH."
  (setf *resize-pending* nil)
  (multiple-value-setq (*term-rows* *term-cols*) (terminal-size))
  (let ((win (session-active-window session)))
    (when win
      (window-relayout win (- *term-rows* *status-height*) *term-cols*))))

(defun %maybe-rename-window-from-title (session)
  "If the active pane has set an OSC title and the window's automatic-rename
   option is enabled, propagate the title to the active window name."
  (let* ((ap  (session-active-pane session))
         (sc  (when ap (pane-screen ap)))
         (win (session-active-window session)))
    (when (and sc win
               (window-automatic-rename-p win)
               (not (string= (screen-title sc) "")))
      (unless (string= (screen-title sc) (window-name win))
        (setf (window-name win) (screen-title sc))
        (setf *dirty* t)))))

(defun %handle-dirty (session)
  "Fit the active window to current terminal size and repaint."
  (setf *dirty* nil)
  (%maybe-rename-window-from-title session)
  (let ((win (session-active-window session)))
    (when win
      (ensure-window-fits win (- *term-rows* *status-height*) *term-cols*)))
  (render-session session *term-rows* *term-cols*))

(defun event-loop (session)
  "In-process event loop: read stdin, route keystrokes, repaint on dirty."
  (let ((state (make-input-state)))
    (loop while *running* do
      (let ((b (read-byte-nonblock +poll-timeout-us+)))
        (when (and b (member (process-byte session b state) '(:quit :detach)))
          (setf *running* nil)))
      (when *resize-pending* (%handle-resize session))
      (when *dirty*           (%handle-dirty session)))))
