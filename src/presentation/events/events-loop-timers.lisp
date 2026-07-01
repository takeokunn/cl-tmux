(in-package #:cl-tmux)

;;;; CPS process-byte dispatch, escape-time / repeat-time timer plumbing, and
;;;; the synchronize-panes input broadcast.

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
  ;; How many repeatable keys have been pressed in the current repeat sequence;
  ;; 1 for the first, so the first repeat window can honour initial-repeat-time.
  (repeat-key-count 0 :type (integer 0))
  (esc-entered-at nil))

(defun %repeat-window-ms (repeat-key-count)
  "Return the repeat-timeout window in milliseconds for the REPEAT-KEY-COUNT-th
   repeatable key.  The first key of a repeat sequence (count 1) honours a
   non-zero initial-repeat-time; every other key (and a zero initial-repeat-time)
   uses repeat-time.  Mirrors tmux 3.5+'s server_client_repeat_time."
  (let ((repeat-ms  (or (cl-tmux/options:get-option "repeat-time") 500))
        (initial-ms (or (cl-tmux/options:get-option "initial-repeat-time") 0)))
    (if (and (= repeat-key-count 1) (plusp initial-ms))
        initial-ms
        repeat-ms)))

(defun %reset-repeat-if-expired (state)
  "If STATE is in a repeatable prefix position and the repeat window has elapsed
   since REPEAT-ENTERED-AT, reset to ground state.  Otherwise a no-op.  The first
   key of a sequence uses initial-repeat-time (when set), the rest repeat-time.
   Called once per event-loop iteration before processing any new byte."
  (when (input-state-repeat-entered-at state)
    (let* ((window-ms  (%repeat-window-ms (input-state-repeat-key-count state)))
           (elapsed-ms (/ (- (get-internal-real-time)
                              (input-state-repeat-entered-at state))
                           (/ internal-time-units-per-second 1000))))
      (when (>= elapsed-ms window-ms)
        (setf (input-state-continuation state) #'%ground-input-state
              (input-state-repeat-entered-at state) nil
              (input-state-repeat-key-count state) 0)))))

(defun %track-repeat-state (state outcome new-continuation)
  "Update STATE's repeat-mode bookkeeping after one keystroke.
   OUTCOME is the value returned by the CPS continuation that just ran;
   NEW-CONTINUATION is the state STATE has just transitioned into.
   A :REPEATABLE outcome means the binding that just fired had the -r flag,
   so we stamp REPEAT-ENTERED-AT (for %reset-repeat-if-expired) and count the
   key.  Repeat mode is armed for BOTH the prefix path
   (%after-prefix-input-state) and root -n -r bindings
   (%after-root-repeat-input-state); staying in either continuation keeps the
   repeat window open, otherwise the timestamp and count are cleared."
  (when (eq outcome :repeatable)
    (setf (input-state-repeat-entered-at state) (get-internal-real-time))
    (incf (input-state-repeat-key-count state)))
  (unless (or (eq new-continuation #'%after-prefix-input-state)
              (eq new-continuation #'%after-root-repeat-input-state))
    (setf (input-state-repeat-entered-at state) nil
          (input-state-repeat-key-count state) 0)))

(defun %track-esc-accum-state (state byte new-continuation)
  "Update STATE's escape-accumulation timestamp after one keystroke.
   BYTE is the byte just processed and NEW-CONTINUATION is the state STATE has
   just transitioned into.  Stamps ESC-ENTERED-AT when a lone ESC byte (27)
   transitions OUT of ground state (entering escape-input-k), so
   %flush-esc-if-timed-out can implement the escape-time disambiguation
   window.  Clears the timestamp and drops the replay buffer once the
   sequence completes or aborts back to ground state, so a later flush can't
   resend a stale partial sequence."
  (cond
    ((and (= byte +byte-esc+)
          (not (eq new-continuation #'%ground-input-state))
          (not (eq new-continuation #'%after-prefix-input-state)))
     (setf (input-state-esc-entered-at state) (get-internal-real-time)))
    ((eq new-continuation #'%ground-input-state)
     (setf (input-state-esc-entered-at state) nil
           *esc-accum-buffer* nil))))

(defun process-byte (session byte state)
  "Feed BYTE to SESSION through the CPS keystroke pipeline STATE.
   Returns :QUIT, :DETACH, or NIL. Mutates STATE's continuation in place."
  (multiple-value-bind (outcome next)
      (funcall (input-state-continuation state) session byte)
    (let ((new-continuation (or next #'%ground-input-state)))
      (setf (input-state-continuation state) new-continuation)
      (%track-repeat-state state outcome new-continuation)
      ;; Track prefix state for #{client_prefix} format variable.
      (setf *prefix-active* (eq new-continuation #'%after-prefix-input-state))
      (%track-esc-accum-state state byte new-continuation))
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
