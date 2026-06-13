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
   (setf (session-locked-p session) nil)
   (setf *dirty* t)
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
      (let ((buffer (make-array 8 :element-type '(unsigned-byte 8)
                                  :fill-pointer 0 :adjustable t)))
        (vector-push-extend byte buffer)
        (setf *dirty* t)
        (return-from %ground-input-state
          (values nil (%overlay-escape-second-byte buffer)))))
     ;; all other keys: swallow (keep overlay open)
     (t nil))
   (values nil #'%ground-input-state))
  ;; ── Active prompt captures all input ──────────────────────────────────────
  ((prompt-active-p)
   (handle-prompt-key byte)
   (values nil #'%ground-input-state))
  ;; ── Active custom key table (switch-client -T <table>) ─────────────────────
  ;; While the client is in a user key table, keys are looked up THERE (not
  ;; root/prefix) and the table PERSISTS until a binding switches back (e.g.
  ;; switch-client -T root) — enabling modal keymaps like a resize mode.  Unbound
  ;; keys are consumed (ignored), so the mode is truly modal.  Guarded on
  ;; *key-table* being a non-root custom table, so the normal flow below is
  ;; completely unaffected when no custom table is active (the default).
  ((and *key-table* (not (equal *key-table* +table-root+)))
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
   (let ((buffer (make-array 8 :element-type '(unsigned-byte 8)
                               :fill-pointer 0 :adjustable t)))
     (vector-push-extend byte buffer)
     (values nil (make-escape-input-k session buffer))))
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
            ;; First: check the copy-mode-vi and copy-mode key tables for
            ;; user-defined overrides (bind -T copy-mode-vi ...).
            ;; When a table binding is found, execute it and SKIP the hardcoded dispatch.
            (let* ((ch       (code-char byte))
                   (entry    (or (key-table-lookup "copy-mode-vi" ch)
                                 (key-table-lookup "copy-mode" ch)))
                   (handled  nil))
              (when entry
                (%run-key-table-binding session entry byte)
                (setf handled t))
              (unless handled
            (flet ((repeat (fn)
                     "Call FN COUNT times on SCREEN."
                     (dotimes (_ count) (funcall fn screen)))
                   (make-char-jump-k (jump-fn sc n)
                     "Return a CPS continuation that reads one byte then calls JUMP-FN
                      on SC for that char N times, then returns to %ground-input-state."
                     (lambda (_s2 byte2)
                       (declare (ignore _s2))
                       (dotimes (_ n) (funcall jump-fn sc (code-char byte2)))
                       (setf *dirty* t)
                       (values nil #'%ground-input-state))))
              (case byte
                ;; q / C-c (3) / Q — exit copy mode (cancel)
                (#.+byte-q+  (copy-mode-exit screen))
                (81          (copy-mode-exit screen))               ; Q
                (3           (copy-mode-exit screen))               ; C-c
                ;; Enter (13) / C-j (10) — copy selection and cancel
                ((13 10)     (copy-mode-yank screen))
                ;; i — exit copy mode (non-standard but kept for compat)
                (105         (copy-mode-exit screen))               ; i
                ;; h / C-h (8) — move cursor left
                ((104 8)     (repeat (lambda (s) (copy-mode-move-cursor s :left))))  ; h / C-h
                ;; l — move cursor right
                (108        (repeat (lambda (s) (copy-mode-move-cursor s :right)))) ; l
                ;; j / C-n (14) — move cursor down (viewport follows at edge)
                ((#.+byte-j+ 14)
                 (repeat (lambda (s) (copy-mode-move-cursor s :down))))
                ;; k / C-p (16) — move cursor up (viewport follows at edge)
                ((#.+byte-k+ 16)
                 (repeat (lambda (s) (copy-mode-move-cursor s :up))))
                ;; J — scroll-down (viewport scrolls toward newer, cursor stays; vi J = C-e)
                (74         (repeat #'copy-mode-scroll-down-line))  ; J
                ;; K — scroll-up (viewport scrolls toward older, cursor stays; vi K = C-y)
                (75         (repeat #'copy-mode-scroll-up-line))    ; K
                ;; w — word forward
                (119        (repeat #'copy-mode-word-forward))        ; w
                ;; W — WORD forward (whitespace-delimited, vi W)
                (87         (repeat #'copy-mode-space-forward))       ; W
                ;; b — word backward
                (98         (repeat #'copy-mode-word-backward))       ; b
                ;; B — WORD backward (whitespace-delimited, vi B)
                (66         (repeat #'copy-mode-space-backward))      ; B
                ;; e — word end
                (101        (repeat #'copy-mode-word-end))            ; e
                ;; E — WORD end (whitespace-delimited, vi E)
                (69         (repeat #'copy-mode-space-end))           ; E
                ;; 0 — line start (bare '0' with no prefix)
                (48         (copy-mode-line-start screen))            ; 0
                ;; ^ — back-to-indentation (first non-blank, vi ^)
                (94         (copy-mode-back-to-indentation screen))   ; ^
                ;; $ — line end
                (36         (copy-mode-line-end screen))              ; $
                ;; g — jump to top (maximum scrollback)
                (103        (copy-mode-top screen))
                ;; G — jump to bottom (offset = 0, live view)
                (71         (copy-mode-bottom screen))
                ;; H — cursor to top of screen
                (72         (copy-mode-high screen))                  ; H
                ;; M — cursor to middle of screen
                (#.(char-code #\M) (copy-mode-middle screen))        ; M
                ;; L — cursor to bottom of screen
                (76         (copy-mode-low screen))                   ; L
                ;; C-f (6) — page down
                (6          (repeat #'copy-mode-page-down))
                ;; C-b (2) — page up
                (2          (repeat #'copy-mode-page-up))
                ;; C-u (21) — scroll up half page
                (21         (repeat #'copy-mode-half-page-up))
                ;; C-d (4) — scroll down half page
                (4          (repeat #'copy-mode-half-page-down))
                ;; C-e (5) — scroll down one line
                (5          (repeat #'copy-mode-scroll-down-line))
                ;; C-y (25) — scroll up one line
                (25         (repeat #'copy-mode-scroll-up-line))
                ;; V — begin line selection
                (86         (copy-mode-begin-line-selection screen))  ; V
                ;; D — copy to end of line
                (68         (copy-mode-copy-end-of-line screen))      ; D
                ;; Y — copy current line
                (89         (copy-mode-copy-line screen))             ; Y
                ;; Space / v — begin selection
                ((32 118)   (copy-mode-begin-selection screen))
                ;; y — yank selection
                (121        (copy-mode-yank screen))
                ;; A — append selection to paste buffer and cancel
                (65         (copy-mode-append-selection screen))      ; A
                ;; o / O — swap mark and cursor ends of selection (vi o / O)
                ((111 79)   (copy-mode-other-end screen))             ; o, O
                ;; C-v (22) — toggle rectangle select (real tmux default)
                (22         (copy-mode-toggle-rectangle screen))      ; C-v
                ;; r — refresh-from-pane (real tmux default; no-op here, kept for compat)
                ;; (114 nil) — no action
                ;; z — scroll-middle: scroll viewport so cursor row is centered (vi z)
                (122        (copy-mode-scroll-middle screen))         ; z
                ;; n — search next
                (110        (copy-mode-search-next screen))           ; n
                ;; N — search prev
                (78         (copy-mode-search-prev screen))           ; N
                ;; / — interactive search forward prompt
                (47 (%copy-mode-search-prompt session "/" #'copy-mode-search-forward))
                ;; ? — interactive search backward prompt
                (63 (%copy-mode-search-prompt session "?" #'copy-mode-search-backward))
                ;; C-s (19) — incremental forward search (tmux search-forward-incremental)
                (19  (copy-mode-search-forward-incremental screen))
                ;; C-r (18) — incremental backward search (tmux search-backward-incremental)
                (18  (copy-mode-search-backward-incremental screen))
                ;; f — jump forward to char on line (vi f<char>): need 2nd byte.
                ;; Count prefix (e.g. 3f<char>) repeats the jump COUNT times.
                (#.+byte-f+
                 (setf *dirty* t)
                 (return-from %ground-input-state
                   (values nil (make-char-jump-k #'copy-mode-jump-forward screen count))))
                ;; F — jump backward to char on line (vi F<char>)
                (#.+byte-capital-f+
                 (setf *dirty* t)
                 (return-from %ground-input-state
                   (values nil (make-char-jump-k #'copy-mode-jump-backward screen count))))
                ;; t — jump to just before next char (vi t<char>)
                (#.+byte-t+
                 (setf *dirty* t)
                 (return-from %ground-input-state
                   (values nil (make-char-jump-k #'copy-mode-jump-to screen count))))
                ;; T — jump to just after previous char (vi T<char>)
                (#.+byte-capital-t+
                 (setf *dirty* t)
                 (return-from %ground-input-state
                   (values nil (make-char-jump-k #'copy-mode-jump-to-backward screen count))))
                ;; { — previous-paragraph (jump to nearest blank line above; vi {)
                (123 (repeat #'copy-mode-previous-paragraph))
                ;; } — next-paragraph (jump to nearest blank line below; vi })
                (125 (repeat #'copy-mode-next-paragraph))
                ;; % — jump to matching bracket (vi %)
                (37  (copy-mode-next-matching-bracket screen))
                ;; ; — repeat last jump (vi ;)
                (59 (dotimes (_ count) (copy-mode-jump-again screen)))
                ;; , — reverse last jump (vi ,)
                (44 (dotimes (_ count) (copy-mode-jump-reverse screen)))
                ;; m — set mark at current cursor position (vi m)
                (109 (copy-mode-set-mark screen))
                ;; ' — jump to mark (vi ')
                (39  (copy-mode-jump-to-mark screen))
                ;; Any other byte is consumed without forwarding (no passthrough in copy mode)
                (otherwise nil)))))))))
     (setf *dirty* t))
   (values nil #'%ground-input-state))
  ;; ── Default: forward raw byte to active pane (+ synchronize-panes broadcast) ─
  (t
   (%forward-octets-synchronized session
                                  (make-array 1 :element-type '(unsigned-byte 8)
                                               :initial-element byte))
   (values nil #'%ground-input-state)))

