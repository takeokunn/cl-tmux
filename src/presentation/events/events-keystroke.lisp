(in-package #:cl-tmux)

;;;; CPS ground-state keystroke coordinator.

;;; ── Overlay pager key dispatch helper ────────────────────────────────────────
;;;
;;; Extracted from %ground-input-state's overlay-active-p clause to keep the
;;; top-level CPS state a flat ordered list.  Returns the CPS continuation to
;;; resume with: NIL for ground state, or the escape-accumulator continuation
;;; armed by a lone Esc (which may turn out to be an arrow key).

(defun %dispatch-overlay-key (byte)
  "Handle one BYTE while the overlay pager is active.
   j/k scroll; q dismisses; Esc arms escape accumulation (may be an arrow key);
   all other keys are swallowed so the pager stays open until dismissed.
   Always marks *dirty* and returns the CPS continuation to resume with (NIL
   for ground state)."
  (setf *dirty* t)
  (cond
    ((= byte +byte-j+)   (overlay-scroll 1)  nil)
    ((= byte +byte-k+)   (overlay-scroll -1) nil)
    ((= byte +byte-q+)   (clear-overlay)     nil)
    ((= byte +byte-esc+) (%overlay-escape-second-byte (%make-escape-buffer byte)))
    (t nil)))

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
  ;; %dispatch-menu-key handles all key variants and always returns NIL so we
  ;; stay in ground state.  No direct call to %format-menu here because this
  ;; file loads before dispatch-core.lisp (where %format-menu is defined).
  ((menu-active-p)
   (%dispatch-menu-key session byte)
   (values nil #'%ground-input-state))
  ;; ── Global overlays take priority ─────────────────────────────────────────
  ;; j/k scroll; q/Esc dismiss; Up/Down arrows accumulate as ESC sequences and
  ;; are routed to overlay-scroll inside make-escape-input-k; all other keys
  ;; are swallowed so the pager stays open until explicitly dismissed.
  ((overlay-active-p)
   (values nil (%dispatch-overlay-key byte)))
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
  ;; ── Copy-mode single-byte navigation (unprefixed) ─────────────────────────
  ;; Copy mode has its own active table, so ordinary bytes are resolved there
  ;; before root/prefix bindings.  ESC is left to the escape accumulator below:
  ;; it may be a lone Escape, an arrow key, mouse input, or an extended key.
  ;; Numeric prefix: digit bytes 0-9 accumulate *copy-mode-prefix*.  '0' with a
  ;; zero prefix goes to line-start instead (vi convention: 0 = BOL when no count).
  ((and (%copy-mode-active-p session) (/= byte +byte-esc+))
   (%dispatch-copy-mode-ground-byte session byte))
  ;; ── assume-paste-time: rapid consecutive keys are a paste ──────────────────
  ;; tmux (server_client_assume_paste): keys arriving within assume-paste-time
  ;; milliseconds of the previous key bypass ALL binding interpretation (root
  ;; -n bindings and the prefix key below) and go straight to the pane, so
  ;; pasted text containing bound characters cannot trigger commands.  Placed
  ;; after the modal clauses (menu/overlay/prompt/copy-mode) — those consume
  ;; keys regardless, matching tmux's ordering.
  ((%assume-paste-byte-p)
   (%stamp-ground-key-time)
   (%forward-octets-synchronized session
                                  (make-array 1 :element-type '(unsigned-byte 8)
                                                :initial-element byte))
   (values nil #'%ground-input-state))
  ;; ── Root key-table: check for bindings that fire without any prefix ────────
  ;; Looked up before the prefix-key check so that -n bindings can intercept
  ;; keys that would otherwise be forwarded to the pane.
  ((%key-table-entry-by-candidates +table-root+ (%single-byte-key-candidates byte))
   ;; %run-root-table-binding runs the binding and arms repeat mode (returning
   ;; :repeatable + a root-scoped repeat state) when the binding has the -r flag,
   ;; so `bind -n -r` keys repeat without the prefix within repeat-time.
   (%run-root-table-binding session byte))
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
  ;; ── Default: forward raw byte to active pane (+ synchronize-panes broadcast) ─
  (t
   (%stamp-ground-key-time)
   (%forward-octets-synchronized session
                                  (make-array 1 :element-type '(unsigned-byte 8)
                                               :initial-element byte))
   (values nil #'%ground-input-state)))
