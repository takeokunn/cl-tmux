(in-package #:cl-tmux)

;;;; CPS keystroke state machine and escape sequence processing.

;;; ── Copy-mode numeric prefix ─────────────────────────────────────────────────
;;;
;;; *copy-mode-prefix* accumulates digit bytes (0-9) pressed while copy mode
;;; is active.  When a non-digit navigation key is pressed, the accumulated
;;; count (clamped to min 1) is applied and the prefix is reset to 0.
;;; The variable lives on the main event-loop thread; no locking is needed.

(defvar *copy-mode-prefix* 0
  "Accumulated numeric prefix for copy-mode repeat counts.
   Set to 0 between commands.  Updated exclusively on the event-loop thread.")

(defun %make-escape-buffer (byte)
  "Return a fresh adjustable byte vector with BYTE as its sole element.
   Used to start escape-sequence accumulation: the ESC byte is the first element
   and subsequent bytes are appended as the CPS continuation reads them."
  (let ((buf (make-array 8 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
    (vector-push-extend byte buf)
    buf))

;;; ── Named CPS state functions ────────────────────────────────────────────────
;;;
;;; Rules read like Prolog clauses:
;;;   ground_state(_, _)  :- overlay_active, !, dispatch_overlay_key.
;;;   ground_state(_, _)  :- prompt_active,  !, handle_prompt_key.
;;;   ground_state(_, 2)  :- !, transition(after_prefix_state).
;;;   ground_state(S, 27) :- copy_mode_active(S), !, start_escape_accumulation.
;;;   ground_state(S, B)  :- forward_octets(S, [B]).

(define-cps-state %ground-input-state (session byte)
  ;; ── Locked session: any key unlocks ────────────────────────────────────────
  ((session-locked-p session)
   (setf (session-locked-p session) nil
         *dirty* t)
   (values nil #'%ground-input-state))
  ;; ── Active menu: j/k navigate, Enter selects, q/Esc dismisses ───────────────
  ;; Menu takes priority over overlay so choose-session/choose-window is
  ;; navigable with the keyboard; the overlay just renders the menu content.
  ;; Dispatch to :menu-next/:menu-prev/:menu-select/:menu-dismiss which each
  ;; call show-overlay to redraw.  No direct call to %format-menu here because
  ;; this file loads before dispatch-core.lisp (where %format-menu is defined).
  ((menu-active-p)
   (cond
     ;; j — next item
     ((= byte +byte-j+)
      (dispatch-command session :menu-next byte)
      (setf *dirty* t))
     ;; k — previous item
     ((= byte +byte-k+)
      (dispatch-command session :menu-prev byte)
      (setf *dirty* t))
     ;; Enter — select current item
     ((= byte 13)
      (dispatch-command session :menu-select byte)
      (setf *dirty* t))
     ;; q / Escape — dismiss menu
     ((or (= byte +byte-q+) (= byte +byte-esc+))
      (dispatch-command session :menu-dismiss byte)
      (setf *dirty* t))
     ;; Digit 0-9: jump to that item index, then dispatch menu-next with 0 delta
     ;; to trigger overlay refresh via the dispatch-handlers.lisp path.
     ((and (>= byte 48) (<= byte 57))
      (let* ((n   (- byte 48))
             (len (length (menu-items *active-menu*))))
        (when (< n len)
          (setf (menu-selected-index *active-menu*) n)
          ;; Trigger show-overlay refresh via dispatch (avoids direct %format-menu call).
          (dispatch-command session :menu-next byte)
          (dispatch-command session :menu-prev byte)
          (setf *dirty* t))))
     ;; All other keys swallowed while menu is open
     (t nil))
   (values nil #'%ground-input-state))
  ;; ── Global overlays take priority ─────────────────────────────────────────
  ;; j/k scroll; q/Esc dismiss; Up/Down arrows accumulate as ESC sequences and
  ;; are routed to overlay-scroll inside make-escape-input-k; all other keys
  ;; are swallowed so the pager stays open until explicitly dismissed.
  ((overlay-active-p)
   (cond
     ;; scroll down one line
     ((= byte +byte-j+)   (overlay-scroll 1)  (setf *dirty* t))
     ;; scroll up one line
     ((= byte +byte-k+)   (overlay-scroll -1) (setf *dirty* t))
     ;; dismiss
     ((= byte +byte-q+)   (clear-overlay)     (setf *dirty* t))
     ((= byte +byte-esc+)                                         ; Esc — may be arrow
      (setf *dirty* t)
      (return-from %ground-input-state
        (values nil (%overlay-escape-second-byte (%make-escape-buffer byte)))))
     ;; all other keys: swallow (keep overlay open)
     (t nil))
   (values nil #'%ground-input-state))
  ;; ── Active prompt captures all input ──────────────────────────────────────
  ((prompt-active-p)
   (if (= byte +byte-esc+)
       (values nil (make-prompt-escape-input-k (%make-escape-buffer byte)))
       (progn
         (handle-prompt-key byte)
         (values nil #'%ground-input-state))))
  ;; ── Active custom key table (switch-client -T <table>) ─────────────────────
  ;; While the client is in a user key table, keys are looked up THERE (not
  ;; root/prefix) and the table PERSISTS until a binding switches back (e.g.
  ;; switch-client -T root) — enabling modal keymaps like a resize mode.  Unbound
  ;; keys are consumed (ignored), so the mode is truly modal.  Guarded on
  ;; *key-table* being a non-root custom table, so the normal flow below is
  ;; completely unaffected when no custom table is active (the default).
  ((and *key-table* (string/= *key-table* +table-root+))
   (let ((entry (key-table-lookup *key-table* (code-char byte))))
     (when entry
       (%run-key-table-binding session entry byte)))
   (setf *dirty* t)
   (values nil #'%ground-input-state))
  ;; ── Root key-table: check for bindings that fire without any prefix ────────
  ;; Looked up before the prefix-key check so that -n bindings can intercept
  ;; keys that would otherwise be forwarded to the pane.
  ((let ((entry (key-table-lookup +table-root+ (code-char byte))))
     (when entry
       (%run-key-table-binding session entry byte)
       (setf *dirty* t)))
   (values nil #'%ground-input-state))
  ;; ── Prefix key: arm command dispatcher ────────────────────────────────────
  ;; Check the RUNTIME variable *prefix-key-code* (not the compile-time constant
  ;; +prefix-key-code+) so that `set -g prefix C-a` actually remaps the prefix.
  ;; Also check *prefix2-key-code* when a second prefix has been configured.
  ((or (= byte *prefix-key-code*)
       (and *prefix2-key-code* (= byte *prefix2-key-code*)))
   (values nil #'%after-prefix-input-state))
  ;; ── ESC: always accumulate for mouse events, arrows, copy mode ───────────
  ;; Even in copy mode we accumulate: arrow keys arrive as ESC [ FINAL and are
  ;; handled by handle-copy-mode-escape inside make-escape-input-k.  A lone ESC
  ;; (2-byte non-CSI) or unrecognised sequence exits copy mode instead of
  ;; forwarding to the pane (handled in make-escape-input-k).
  ((= byte +byte-esc+)
   (values nil (make-escape-input-k session (%make-escape-buffer byte))))
  ;; ── Copy-mode single-byte navigation (unprefixed) ─────────────────────────
  ;; These keys are intercepted ONLY when copy mode is active; they are never
  ;; forwarded to the pane.  The check comes before the default forward branch.
  ;; Numeric prefix: digit bytes 0-9 accumulate *copy-mode-prefix*.  '0' with a
  ;; zero prefix goes to line-start instead (vi convention: 0 = BOL when no count).
  ((%copy-mode-active-p session)
   (let ((screen (%active-screen session)))
     (when screen
       ;; Digit accumulation: build a numeric count for the next command.
       ;; '0' with prefix=0 falls through to line-start (handled by case below).
       (cond
         ;; Accumulate digit into prefix (1-9 always; 0 only when prefix already set)
         ((and (>= byte +byte-digit-0+) (<= byte +byte-digit-9+)
               (or (> byte +byte-digit-0+) (plusp *copy-mode-prefix*)))
          (setf *copy-mode-prefix*
                (+ (* *copy-mode-prefix* 10) (- byte +byte-digit-0+))))
         ;; Non-digit (or bare '0'): dispatch with accumulated count then reset.
         (t
          (let ((count (max 1 *copy-mode-prefix*)))
            (setf *copy-mode-prefix* 0)
            ;; First: check the active copy-mode key table for user-defined
            ;; overrides (bind -T copy-mode-vi ... / bind -T copy-mode ...).
            ;; When a table binding is found, execute it and SKIP the hardcoded dispatch.
            (let* ((ch       (code-char byte))
                   (entry    (key-table-lookup (%active-copy-mode-table) ch))
                   (handled  nil))
              (when entry
                (%run-key-table-binding session entry byte)
                (setf handled t))
              (unless handled
                (multiple-value-bind (dispatched new-state)
                    (%dispatch-copy-mode-byte screen byte count session)
                  (when (and dispatched new-state)
                    (return-from %ground-input-state
                      (values nil new-state))))))))))
   (setf *dirty* t))
   (values nil #'%ground-input-state))
  ;; ── Default: forward raw byte to active pane (+ synchronize-panes broadcast) ─
  (t
   (%forward-octets-synchronized session
                                  (make-array 1 :element-type '(unsigned-byte 8)
                                               :initial-element byte))
   (values nil #'%ground-input-state)))
