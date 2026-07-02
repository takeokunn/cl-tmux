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

;;; ── assume-paste-time (tmux server_client_assume_paste) ─────────────────────
;;;
;;; When two ground-state keys arrive within assume-paste-time milliseconds,
;;; tmux assumes a paste is in progress and bypasses key-binding interpretation
;;; (root-table -n bindings and the prefix key), forwarding the bytes to the
;;; pane instead — so pasted text containing bound characters does not trigger
;;; commands.  Bracketed paste (DECSET 2004) is the primary mechanism; this is
;;; the fallback for terminals pasting without it.

(defvar *last-ground-key-time* nil
  "internal-real-time of the previous PANE-FORWARDED ground-state key byte, or
   NIL before any.  Only forwarded (content) bytes stamp it: a paste burst is a
   stream of pane content, so \"the previous key was content, moments ago\" is
   the paste signal — binding/prefix keys do not count as paste context.
   Updated exclusively on the event-loop thread.")

(defun %stamp-ground-key-time ()
  "Record the arrival time of a pane-forwarded ground-state key byte."
  (setf *last-ground-key-time* (get-internal-real-time)))

(defun %assume-paste-byte-p ()
  "True when this key arrives within assume-paste-time milliseconds of the
   previous pane-forwarded key.  assume-paste-time 0 (or a non-integer value)
   disables the heuristic."
  (let ((prev *last-ground-key-time*)
        (ms   (let ((value (cl-tmux/options:get-option "assume-paste-time")))
                (if (integerp value) value 0))))
    (and prev
         (plusp ms)
         (< (- (get-internal-real-time) prev)
            (* ms (floor internal-time-units-per-second 1000))))))

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

(defun %make-escape-buffer (byte)
  "Return a fresh adjustable byte vector with BYTE as its sole element.
   Used to start escape-sequence accumulation: the ESC byte is the first element
   and subsequent bytes are appended as the CPS continuation reads them."
  (let ((escape-buffer (make-array 8 :element-type '(unsigned-byte 8)
                                      :fill-pointer 0 :adjustable t)))
    (vector-push-extend byte escape-buffer)
    escape-buffer))

;;; ── Menu key dispatch helper ─────────────────────────────────────────────────
;;;
;;; When the interactive menu overlay is active, most keystrokes are consumed
;;; and routed to the menu navigation commands rather than the active pane.
;;; Extracted from %ground-input-state to keep the top-level CPS state readable.
;;;
;;; define-menu-key-rules follows the same Prolog-like rule style as
;;; define-copy-mode-vi-rules and define-cps-state: each RULE is a
;;; (CONDITION &rest BODY) clause, matched in order.  Uniform "dispatch one
;;; menu command" arms are declarative facts; the digit-jump and default arms
;;; keep their custom bodies verbatim.

(defmacro define-menu-key-rules (&rest rules)
  "Build %DISPATCH-MENU-KEY from an ordered table of (CONDITION &rest BODY)
   rules.  Each matched BODY is responsible for marking *dirty* itself; the
   generated function always returns NIL so the caller stays in ground state
   regardless of which key was pressed."
  `(defun %dispatch-menu-key (session byte)
     "Dispatch BYTE to the active menu overlay and mark the display dirty.
      j — next item; k — previous item; Enter — select; q/Esc — dismiss;
      digit 0-9 — jump to that item index then refresh.  All other keys are
      swallowed (the menu remains open).  Always returns NIL so the caller
      stays in ground state regardless of which key was pressed."
     (declare (ignorable session byte))
     (cond
       ,@(mapcar
          (lambda (rule)
            (destructuring-bind (condition &rest body) rule
              `(,condition ,@body)))
          rules)
       (t nil))
     nil))

(define-menu-key-rules
  ;; j — next item
  ((= byte +byte-j+)
   (dispatch-command session :menu-next byte)
   (setf *dirty* t))
  ;; k — previous item
  ((= byte +byte-k+)
   (dispatch-command session :menu-prev byte)
   (setf *dirty* t))
  ;; Enter — select current item
  ((= byte +byte-enter+)
   (dispatch-command session :menu-select byte)
   (setf *dirty* t))
  ;; q / Escape — dismiss menu
  ((or (= byte +byte-q+) (= byte +byte-esc+))
   (dispatch-command session :menu-dismiss byte)
   (setf *dirty* t))
  ;; Digit 0-9: jump to that item index, then dispatch menu-next with 0 delta
  ;; to trigger overlay refresh via the dispatch-handlers.lisp path.
  ((and (>= byte +byte-digit-0+) (<= byte +byte-digit-9+))
   (let* ((digit  (- byte +byte-digit-0+))
          (length (length (menu-items *active-menu*))))
     (when (< digit length)
       (setf (menu-selected-index *active-menu*) digit)
       ;; Trigger show-overlay refresh via dispatch (avoids direct %format-menu call).
       (dispatch-command session :menu-next byte)
       (dispatch-command session :menu-prev byte)
       (setf *dirty* t)))))

;;; ── Copy-mode ground-state dispatch ──────────────────────────────────────────
;;;
;;; Extracted from %ground-input-state so the top-level CPS state stays a flat
;;; ordered list of clauses.  %copy-mode-accumulate-digit is itself a small CPS
;;; state function: it accepts the pending BYTE and returns (values COUNT-OR-NIL)
;;; — NIL means "byte consumed into *copy-mode-prefix*, wait for the next byte";
;;; a non-NIL COUNT means "prefix accumulation is complete, dispatch with COUNT".
;;; This expresses the digit accumulator as data flowing through the same
;;; (byte) → outcome protocol as the rest of the keystroke pipeline, rather than
;;; as an ad hoc mutation buried inside the ground-state cond.

(defun %copy-mode-accumulate-digit (byte)
  "Fold BYTE into *copy-mode-prefix* when it continues a numeric prefix.
   Returns NIL when BYTE was consumed as a prefix digit (caller should wait for
   the next byte).  Returns the resolved repeat COUNT (>= 1) and resets
   *copy-mode-prefix* to 0 when BYTE is not a prefix digit — i.e. when the
   accumulated count is ready to be applied to a navigation command.
   '0' with prefix=0 is NOT accumulated (vi convention: bare 0 = beginning of
   line, only 1-9 or a non-zero prefix followed by 0 continue the prefix)."
  (if (and (>= byte +byte-digit-0+) (<= byte +byte-digit-9+)
           (or (> byte +byte-digit-0+) (plusp *copy-mode-prefix*)))
      (progn
        (setf *copy-mode-prefix*
              (+ (* *copy-mode-prefix* 10) (- byte +byte-digit-0+)))
        nil)
      (let ((count (max 1 *copy-mode-prefix*)))
        (setf *copy-mode-prefix* 0)
        count)))

(defun %run-copy-mode-key-table-entry-or-dispatch (session screen byte count)
  "Resolve BYTE against the active copy-mode key table and either run the
   matching user binding or fall through to %DISPATCH-COPY-MODE-BYTE.
   First checks the active copy-mode key table for user-defined overrides
   (`bind -T copy-mode-vi ...` / `bind -T copy-mode ...`); legacy Ctrl bytes
   and single-byte special keys are probed by their canonical tmux name
   (\"C-b\", \"Enter\", \"BSpace\", ...), matching keys stored by the
   key-binding table.  Returns the CPS continuation to resume with (a
   char-jump continuation) or NIL for ground state."
  (let ((entry (%key-table-entry-by-candidates
                (%active-copy-mode-table)
                (%single-byte-key-candidates byte))))
    (if entry
        (progn (%run-key-table-binding session entry byte) nil)
        (multiple-value-bind (dispatched new-state)
            (%dispatch-copy-mode-byte screen byte count session)
          (declare (ignore dispatched))
          new-state))))

(defun %dispatch-copy-mode-ground-byte (session byte)
  "Handle one BYTE of unprefixed copy-mode navigation from ground state.
   Copy mode has its own active table, so ordinary bytes are resolved there
   before root/prefix bindings.  Numeric prefix digits accumulate via
   %copy-mode-accumulate-digit; once a non-digit byte resolves the count, the
   byte is resolved via %run-copy-mode-key-table-entry-or-dispatch.  Returns
   (values NIL NEXT-STATE) where NEXT-STATE is the CPS continuation to resume
   with (ground state unless a char-jump command armed a one-byte
   continuation)."
  (let ((screen (%active-screen session)))
    (when screen
      (let ((count (%copy-mode-accumulate-digit byte)))
        (when count
          (let ((new-state (%run-copy-mode-key-table-entry-or-dispatch
                             session screen byte count)))
            (when new-state
              (setf *dirty* t)
              (return-from %dispatch-copy-mode-ground-byte
                (values nil new-state))))))))
  (setf *dirty* t)
  (values nil #'%ground-input-state))

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
