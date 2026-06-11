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
          (values nil (make-overlay-escape-k buffer)))))
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
       (let ((cmd (key-table-command entry)))
         (cond
           ((and (consp cmd) (eq (car cmd) :sequence))
            (dolist (subcmd (cdr cmd)) (%run-command-tokens session subcmd)))
           ((consp cmd) (%run-command-tokens session cmd))
           (t (dispatch-command session cmd byte))))))
   (setf *dirty* t)
   (values nil #'%ground-input-state))
  ;; ── Root key-table: check for bindings that fire without any prefix ────────
  ;; Looked up before the prefix-key check so that -n bindings can intercept
  ;; keys that would otherwise be forwarded to the pane.
  ((let ((entry (key-table-lookup +table-root+ (code-char byte))))
     (when entry
       ;; A -n binding's command is a keyword (built-in), a token LIST, or a
       ;; :sequence of token lists (from bind -n key cmd1 \; cmd2).
       (let ((cmd (key-table-command entry)))
         (cond
           ((and (consp cmd) (eq (car cmd) :sequence))
            (dolist (subcmd (cdr cmd))
              (%run-command-tokens session subcmd)))
           ((consp cmd)
            (%run-command-tokens session cmd))
           (t
            (dispatch-command session cmd byte))))
       (setf *dirty* t)
       t))
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
                (let ((cmd (key-table-command entry)))
                  (if (consp cmd)
                      (%run-command-tokens session cmd)
                      (dispatch-command session cmd byte)))
                (setf handled t))
              (unless handled
            (flet ((repeat (fn)
                     "Call FN COUNT times on SCREEN."
                     (dotimes (_ count) (funcall fn screen))))
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
                (47
                 (prompt-start "search" ""
                               (lambda (term)
                                 (setf *dirty* t)
                                 (when (and (stringp term) (plusp (length term)))
                                   (copy-mode-search-forward screen term)))))
                ;; ? — interactive search backward prompt
                (63
                 (prompt-start "search-back" ""
                               (lambda (term)
                                 (setf *dirty* t)
                                 (when (and (stringp term) (plusp (length term)))
                                   (copy-mode-search-backward screen term)))))
                ;; C-s (19) — incremental forward search (tmux search-forward-incremental)
                (19  (copy-mode-search-forward-incremental screen))
                ;; C-r (18) — incremental backward search (tmux search-backward-incremental)
                (18  (copy-mode-search-backward-incremental screen))
                ;; f — jump forward to char on line (vi f<char>): need 2nd byte.
                ;; Count prefix (e.g. 3f<char>) repeats the jump COUNT times.
                (102
                 (let ((sc screen) (n count))
                   (setf *dirty* t)
                   (return-from %ground-input-state
                     (values nil
                             (lambda (_s2 byte2)
                               (declare (ignore _s2))
                               (dotimes (_ n) (copy-mode-jump-forward sc (code-char byte2)))
                               (setf *dirty* t)
                               (values nil #'%ground-input-state))))))
                ;; F — jump backward to char on line (vi F<char>)
                (70
                 (let ((sc screen) (n count))
                   (setf *dirty* t)
                   (return-from %ground-input-state
                     (values nil
                             (lambda (_s2 byte2)
                               (declare (ignore _s2))
                               (dotimes (_ n) (copy-mode-jump-backward sc (code-char byte2)))
                               (setf *dirty* t)
                               (values nil #'%ground-input-state))))))
                ;; t — jump to just before next char (vi t<char>)
                (116
                 (let ((sc screen) (n count))
                   (setf *dirty* t)
                   (return-from %ground-input-state
                     (values nil
                             (lambda (_s2 byte2)
                               (declare (ignore _s2))
                               (dotimes (_ n) (copy-mode-jump-to sc (code-char byte2)))
                               (setf *dirty* t)
                               (values nil #'%ground-input-state))))))
                ;; T — jump to just after previous char (vi T<char>)
                (84
                 (let ((sc screen) (n count))
                   (setf *dirty* t)
                   (return-from %ground-input-state
                     (values nil
                             (lambda (_s2 byte2)
                               (declare (ignore _s2))
                               (dotimes (_ n) (copy-mode-jump-to-backward sc (code-char byte2)))
                               (setf *dirty* t)
                               (values nil #'%ground-input-state))))))
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

(defun %prefix-csi-arrow-cmd (final-byte)
  "Map a CSI arrow final byte to a pane-select command keyword, or NIL."
  (case final-byte
    (#.+byte-arrow-up+    :select-pane-up)
    (#.+byte-arrow-down+  :select-pane-down)
    (#.+byte-arrow-right+ :select-pane-right)
    (#.+byte-arrow-left+  :select-pane-left)
    (otherwise nil)))

(defun %arrow-final-name (final-byte)
  "Base tmux key name for an arrow FINAL-BYTE — \"Up\"/\"Down\"/\"Left\"/\"Right\"
   — or NIL when FINAL-BYTE is not an arrow.  These strings match exactly what
   %parse-key-token stores for a `bind Up ...`-style directive, so they double as
   key-table lookup keys."
  (cond
    ((= final-byte +byte-arrow-up+)    "Up")
    ((= final-byte +byte-arrow-down+)  "Down")
    ((= final-byte +byte-arrow-left+)  "Left")
    ((= final-byte +byte-arrow-right+) "Right")
    (t nil)))

(defconstant +byte-csi-mod-shift+ 50
  "CSI modifier '2' — Shift key (0x32), as in ESC [ 1 ; 2 A (Shift+Up).")

(defun %modifier-prefix (mod-value)
  "Build the canonical C-/M-/S- modifier prefix (always in that order) for a CSI
   MOD-VALUE — 1 + a bitmask where bit0=Shift, bit1=Alt/Meta, bit2=Ctrl.  So 2→S-,
   3→M-, 5→C-, 6→C-S-, 7→C-M-, 8→C-M-S-; returns \"\" for 1 (no modifier) or any
   value with no recognised bit.  Shared by the CSI-u, modifier-arrow, and
   modified-function-key paths so every modified key is named identically and
   matches the string %parse-key-token stores for `bind C-S-Up`/`bind C-F5`."
  (let ((bits (max 0 (- mod-value 1))))
    (concatenate 'string
                 (if (logbitp 2 bits) "C-" "")
                 (if (logbitp 1 bits) "M-" "")
                 (if (logbitp 0 bits) "S-" ""))))

(defun %modifier-arrow-key-name (mod-byte final-byte)
  "Canonical tmux key name for a modifier+arrow CSI sequence — \"C-Up\",
   \"M-Left\", \"S-Down\", \"C-S-Up\", etc.  MOD-BYTE is the digit byte from
   ESC [ 1 ; N FINAL (2=Shift, 3=Meta, 5=Ctrl, 6=Ctrl+Shift, …); it is decoded
   through %modifier-prefix so combined modifiers resolve too.  Returns NIL for a
   non-arrow final byte or a MOD-BYTE carrying no modifier, leaving the sequence
   to its built-in default / pane forward."
  (let ((base   (%arrow-final-name final-byte))
        (prefix (%modifier-prefix (- mod-byte +byte-digit-0+))))
    (when (and base (plusp (length prefix)))
      (concatenate 'string prefix base))))

;;; ── Extended keys (CSI u / fixterms) ────────────────────────────────────────
;;;
;;; With extended-keys on, a terminal disambiguates keys that normally collapse to
;;; one byte (C-i vs Tab, C-S-a vs C-a, ...) by sending ESC [ <codepoint> ; <mod> u.
;;; MOD is 1 + a bitmask: bit0 Shift, bit1 Alt, bit2 Ctrl (so Shift=2, Alt=3, Ctrl=5,
;;; Ctrl+Shift=6, Ctrl+Alt+Shift=8).  These helpers turn such a sequence into the
;;; canonical tmux key name (C-/M-/S- order) used by the key-table lookup.

(defun %csi-u-base-key (codepoint)
  "Base key name for a CSI-u CODEPOINT: a named key for the specials (Tab, Enter,
   Escape, Space, BSpace) else the literal character for a printable code; NIL for
   an unhandled (control/out-of-range) codepoint."
  (case codepoint
    (9   "Tab")
    (13  "Enter")
    (27  "Escape")
    (32  "Space")
    (127 "BSpace")
    (t   (when (<= 33 codepoint 126) (string (code-char codepoint))))))

(defun %csi-u-key-name (codepoint mod-value)
  "Canonical tmux key name for the CSI-u sequence ESC [ CODEPOINT ; MOD-VALUE u.
   MOD-VALUE is 1 + a (Shift=1, Alt=2, Ctrl=4) bitmask; the prefix is built in
   C-/M-/S- order (e.g. \"C-S-a\", \"M-Up\", \"C-Space\", \"S-Tab\", \"a\").  Returns
   NIL when the codepoint has no base key."
  (let ((base (%csi-u-base-key codepoint)))
    (when base
      (concatenate 'string (%modifier-prefix mod-value) base))))

(defun %csi-u-parse-params (buffer length)
  "Parse the numeric parameters of a u-terminated CSI sequence
   ESC [ <codepoint> ; <mod> u held in BUFFER (LENGTH octets).  Returns
   (values CODEPOINT MOD-VALUE); MOD-VALUE defaults to 1 when the ; <mod> field is
   omitted (ESC [ <codepoint> u).  Returns NIL when the codepoint field is empty or
   non-numeric.  A sub-parameter on the modifier (the kitty `<mod>:<event>` form) is
   tolerated — only the leading integer is taken."
  (let ((text (map 'string #'code-char (subseq buffer 2 (1- length)))) ; drop ESC [ and u
        (codepoint nil)
        (mod 1))
    (let ((semi (position #\; text)))
      (if semi
          (progn
            (setf codepoint (ignore-errors (parse-integer text :end semi)))
            (let ((m (ignore-errors (parse-integer text :start (1+ semi)
                                                       :junk-allowed t))))
              (when m (setf mod m))))
          (setf codepoint (ignore-errors (parse-integer text)))))
    (when codepoint (values codepoint mod))))

(defun %csi-u-control-byte (codepoint)
  "The legacy control byte for Ctrl + CODEPOINT, or NIL when CODEPOINT has no
   control form.  Letters a-z / A-Z → 1-26; Space and @ → NUL (0); the symbols
   [ \\ ] ^ _ → 27-31.  This is the byte a non-extended terminal would emit, used as
   the transparent fallback when an extended Ctrl chord is unbound."
  (cond
    ((<= 97 codepoint 122) (- codepoint 96))  ; a-z → 1..26
    ((<= 65 codepoint 90)  (- codepoint 64))  ; A-Z → 1..26
    ((= codepoint 32) 0)                       ; Space → NUL
    ((= codepoint 64) 0)                       ; @     → NUL
    ((<= 91 codepoint 95) (- codepoint 64))   ; [ \ ] ^ _ → 27..31
    (t nil)))

(defun %csi-u-legacy-octets (codepoint bits)
  "The legacy byte encoding for the CSI-u chord CODEPOINT + modifier BITS (bit0
   Shift, bit1 Alt, bit2 Ctrl), or NIL when the chord has no one-/two-byte legacy
   form and must be matched by name instead.  Mirrors what a non-extended terminal
   sends, so re-injecting these octets keeps the chord transparent to the inner
   application:
     C-M-<key> → ESC ^X   |   C-<key> → ^X
     M-<char>  → ESC <ch> |   plain / Shift-only printable → <ch>"
  (let ((ctrl (logbitp 2 bits))
        (alt  (logbitp 1 bits))
        (cb   (%csi-u-control-byte codepoint)))
    (cond
      ((and ctrl alt cb)                       (vector +byte-esc+ cb))
      ((and ctrl cb)                           (vector cb))
      ((and alt (not ctrl) (<= 33 codepoint 126)) (vector +byte-esc+ codepoint))
      ((and (not ctrl) (not alt) (<= 32 codepoint 126)) (vector codepoint))
      (t nil))))

(defun %feed-octets-through-ground (session octets)
  "Re-inject OCTETS into the keystroke state machine starting at ground state,
   threading the CPS continuation so a multi-byte legacy form (the ESC <char> meta
   encoding) dispatches exactly as if the bytes had been typed.  This reuses the
   whole root/prefix/custom-table/copy-mode/forward dispatch tree for the legacy
   fallback of an unbound extended-keys chord."
  (let ((state #'%ground-input-state))
    (loop for b across octets
          do (multiple-value-bind (_ next) (funcall state session b)
               (declare (ignore _))
               (setf state (or next #'%ground-input-state))))))

(defun %handle-escape-csi-u (session buffer length)
  "Decode and dispatch a complete CSI-u (extended-keys) sequence
   ESC [ <codepoint> ; <mod> u in BUFFER.  A root-table binding for the canonical
   chord name wins first (covers string-only chords like C-S-a / S-Tab that have no
   legacy byte — the disambiguation extended-keys exists for).  Otherwise the chord
   falls back to its legacy byte form, re-injected through ground state so Ctrl /
   Alt / plain chords stay transparent (and still hit any `bind -n C-a`, the prefix
   key, copy mode, or the pane).  A chord with neither a binding nor a legacy form
   forwards the raw sequence."
  (multiple-value-bind (codepoint mod-value) (%csi-u-parse-params buffer length)
    (let* ((bits (and codepoint (max 0 (- mod-value 1))))
           (key  (and codepoint (%csi-u-key-name codepoint mod-value))))
      (cond
        ((null key)
         (unless (%copy-mode-active-p session)
           (%forward-octets-synchronized session (subseq buffer 0 length))))
        ((%try-bound-string-key session +table-root+ key))
        (t
         (let ((octets (%csi-u-legacy-octets codepoint bits)))
           (if octets
               (%feed-octets-through-ground session octets)
               (unless (%copy-mode-active-p session)
                 (%forward-octets-synchronized session (subseq buffer 0 length))))))))))

(defun %meta-key-name (byte)
  "Canonical tmux key name for the Meta/Alt chord that arrives as ESC then BYTE.
   \"M-a\", \"M-1\", \"M-/\", and \"M-Space\" (byte 32).  Returns NIL for control
   bytes and DEL, which are not standalone meta chords, so the caller forwards
   them unchanged.  This is the exact inverse of the M-<char> encoding produced
   by send-keys (commands.lisp), keeping input decode and output encode symmetric."
  (cond
    ((= byte +byte-space+) "M-Space")
    ((and (> byte +byte-space+) (< byte 127))  ; 33..126 — printable graphic
     (concatenate 'string "M-" (string (code-char byte))))
    (t nil)))

(defun %run-key-table-binding (session entry byte)
  "Execute the command bound to a key-table ENTRY.  Mirrors the root/prefix
   dispatch convention: a (:sequence . cmds) runs each sub-command in order, a
   bare token LIST runs as one command line, and a keyword dispatches as a
   built-in.  BYTE is the originating key byte (used only by built-in dispatch;
   pass NIL for synthetic chords like modifier+arrow)."
  (let ((cmd (key-table-command entry)))
    (cond
      ((and (consp cmd) (eq (car cmd) :sequence))
       (dolist (subcmd (cdr cmd))
         (%run-command-tokens session subcmd)))
      ((consp cmd)
       (%run-command-tokens session cmd))
      (t
       (dispatch-command session cmd byte)))))

(defun %try-bound-string-key (session table key-string)
  "Look up the string KEY-STRING (e.g. \"C-Up\", \"M-Left\", \"Up\") in key
   TABLE.  When a binding exists, run it, mark the screen dirty, and return T;
   otherwise return NIL so the caller can fall back to its hardcoded default.
   This is the hook that lets `bind -T prefix C-Up <cmd>` and `bind -n M-Left
   <cmd>` override the built-in resize/select behaviour."
  (let ((entry (and key-string (key-table-lookup table key-string))))
    (when entry
      (%run-key-table-binding session entry nil)
      (setf *dirty* t)
      t)))

(defun %dispatch-modifier-arrow (session mod-byte final-byte)
  "Handle the modifier+arrow combination inside the 6-byte CSI sequence.
   MOD-BYTE is +byte-csi-mod-ctrl+ (Ctrl) or +byte-csi-mod-meta+ (Meta).
   FINAL-BYTE is the arrow final byte.

   A user binding for the canonical key name (e.g. `bind -T prefix C-Up <cmd>`)
   takes precedence; only when the prefix table has no such binding do we fall
   back to the built-in default — C-arrow resizes 1 cell, M-arrow resizes 5."
  ;; User override in the prefix table wins over the hardcoded default.
  (let ((key (%modifier-arrow-key-name mod-byte final-byte)))
    (when (%try-bound-string-key session +table-prefix+ key)
      (return-from %dispatch-modifier-arrow)))
  (let ((window (session-active-window session)))
    (when window
      (cond
        ;; C-arrow: ESC [ 1 ; 5 FINAL  → resize 1 cell
        ((= mod-byte +byte-csi-mod-ctrl+)
         (case final-byte
           (#.+byte-arrow-up+    (resize-pane window :up    1))
           (#.+byte-arrow-down+  (resize-pane window :down  1))
           (#.+byte-arrow-right+ (resize-pane window :right 1))
           (#.+byte-arrow-left+  (resize-pane window :left  1))))
        ;; M-arrow: ESC [ 1 ; 3 FINAL  → resize 5 cells (standard :resize-* amount)
        ((= mod-byte +byte-csi-mod-meta+)
         (let ((command (case final-byte
                          (#.+byte-arrow-up+    :resize-up)
                          (#.+byte-arrow-down+  :resize-down)
                          (#.+byte-arrow-right+ :resize-right)
                          (#.+byte-arrow-left+  :resize-left)
                          (otherwise nil))))
           (when command (dispatch-command session command nil))))))))

(defun %make-prefix-csi-k (session buffer)
  "CPS continuation: accumulate ESC [ FINAL for post-prefix arrow key sequences.
   Dispatches :select-pane-up/down/left/right on ESC [ A/B/D/C (3-byte CSI).
   Dispatches C-arrow (ESC [ 1 ; 5 FINAL) to resize-pane with amount=1.
   Dispatches M-arrow (ESC [ 1 ; 3 FINAL) to :resize-{dir} with amount=5.
   Unrecognised sequences are silently discarded (no passthrough-prefix)."
  ;; SESSION is captured from the %make-prefix-csi-k call; _ignored-session is
  ;; structurally required by the CPS protocol but always equals captured SESSION.
  (lambda (_ignored-session byte)
    (declare (ignore _ignored-session))
    (vector-push-extend byte buffer)
    ;; Publish the partial buffer so an escape-time timeout replays it whole
    ;; (symmetry with make-escape-input-k); prevents dropping a stuck C-b ESC O.
    (setf *esc-accum-buffer* buffer)
    (let ((length (fill-pointer buffer)))
      (cond
        ;; ── SS3 introducer after prefix: ESC O — defer one byte ──────────
        ;; ESC O P/Q/R/S (F1-F4) / ESC O H/F (Home/End) so `bind F1 <cmd>`
        ;; works.  Deferred before the 2-byte meta branch below claims it.
        ((and (= length 2) (= (aref buffer 1) +byte-ss3-o+))
         (values nil (%make-prefix-csi-k session buffer)))
        ;; ── SS3 function key after prefix: ESC O <final> ─────────────────
        ((and (= length 3) (= (aref buffer 1) +byte-ss3-o+))
         (let ((key (%ss3-key-name (aref buffer 2))))
           (when key (%try-bound-string-key session +table-prefix+ key)))
         (values nil #'%ground-input-state))
        ;; ── Function / navigation key after prefix: ESC [ <digits> ~ ─────
        ;; F5 ESC[15~ … F12, PageUp ESC[5~, Home ESC[1~, Delete ESC[3~, so
        ;; `bind F5 <cmd>` / `bind PPage <cmd>` resolve in the prefix table.
        ;; The tilde terminator keeps this disjoint from the ESC[1;MOD arrow
        ;; branches below (those end in a letter).
        ((and (>= length 4) (= (aref buffer 1) +byte-csi-bracket+)
              (<= +byte-digit-0+ (aref buffer 2) +byte-digit-9+)
              (= (aref buffer (1- length)) +byte-tilde+))
         (let ((key (%csi-tilde-key buffer length)))
           (when key (%try-bound-string-key session +table-prefix+ key)))
         (values nil #'%ground-input-state))
        ;; Complete 3-byte CSI sequence: ESC [ FINAL
        ((and (= length 3) (= (aref buffer 1) +byte-csi-bracket+))
         (let ((final-byte (aref buffer 2)))
           (cond
             ;; A digit final begins a parameterised sequence — ESC [ 1 ; MOD
             ;; FINAL (modifier-arrow) or ESC [ N ~ (function key).  Keep
             ;; accumulating; the tilde / modifier branches resolve it.  (Was
             ;; limited to '1', which dropped the '~' of ESC [ 5 ~ etc.)
             ((<= +byte-digit-0+ final-byte +byte-digit-9+)
              (values nil (%make-prefix-csi-k session buffer)))
             (t
              ;; A user binding (`bind -T prefix Up <cmd>`) overrides the built-in
              ;; select-pane default; fall back only when the key is unbound.
              (let ((name    (%arrow-final-name final-byte))
                    (command (%prefix-csi-arrow-cmd final-byte)))
                (unless (%try-bound-string-key session +table-prefix+ name)
                  ;; dispatch-command always returns NIL; the when's value is discarded.
                  (when command (dispatch-command session command nil)))
                (values nil #'%ground-input-state))))))
        ;; 4-byte sequence starting ESC [ 1 ; — keep accumulating
        ((and (= length 4) (= (aref buffer 1) +byte-csi-bracket+)
              (= (aref buffer 2) +byte-csi-param-1+)
              (= (aref buffer 3) +byte-csi-semi+))
         (values nil (%make-prefix-csi-k session buffer)))
        ;; 5-byte: ESC [ 1 ; MOD — keep accumulating for the final letter
        ((and (= length 5) (= (aref buffer 1) +byte-csi-bracket+)
              (= (aref buffer 2) +byte-csi-param-1+)
              (= (aref buffer 3) +byte-csi-semi+))
         (values nil (%make-prefix-csi-k session buffer)))
        ;; Complete 6-byte modifier CSI: ESC [ 1 ; MOD FINAL
        ((and (= length 6) (= (aref buffer 1) +byte-csi-bracket+)
              (= (aref buffer 2) +byte-csi-param-1+)
              (= (aref buffer 3) +byte-csi-semi+))
         (%dispatch-modifier-arrow session (aref buffer 4) (aref buffer 5))
         (setf *dirty* t)
         (values nil #'%ground-input-state))
        ;; 2-byte non-CSI: a prefix meta chord (C-b then Alt+key → ESC <key>).
        ;; Look up `bind M-<key>` in the prefix table; if unbound, discard as
        ;; before (no passthrough after the prefix).
        ((and (= length 2) (/= (aref buffer 1) +byte-csi-bracket+))
         (%try-bound-string-key session +table-prefix+
                                (%meta-key-name (aref buffer 1)))
         (values nil #'%ground-input-state))
        ;; Buffer at capacity (>= 6 bytes but unrecognised) — discard and return
        ;; to ground to avoid permanent stuck-state on malformed CSI sequences.
        ((>= length 6)
         (values nil #'%ground-input-state))
        ;; Still accumulating (1-5 bytes so far)
        (t (values nil (%make-prefix-csi-k session buffer)))))))

(define-cps-state %after-prefix-input-state (session byte)
  ;; ESC introduces a multi-byte prefix sequence (C-b arrow/modifier key sequences).
  ;; The buffer needs to be adjustable so %make-prefix-csi-k can vector-push-extend
  ;; up to 6 bytes for modifier sequences like ESC [ 1 ; 5 A (C-Up).
  ((= byte +byte-esc+)
   (let ((buffer (make-array 8 :element-type '(unsigned-byte 8)
                               :fill-pointer 0 :adjustable t)))
     (vector-push-extend byte buffer)
     (values nil (%make-prefix-csi-k session buffer))))
  ;; Single-byte: dispatch the command table.  When the binding is repeatable
  ;; (-r flag), stay in after-prefix state so the user can press the key again
  ;; without the prefix key (e.g. C-b H H H to resize left three times).
  (t
   (let ((result (dispatch-prefix-command session byte)))
     (if (eq result :repeatable)
         (values nil #'%after-prefix-input-state)
         (values result #'%ground-input-state)))))

;;; ── SGR mouse sequence parser ────────────────────────────────────────────────
;;;
;;; SGR format: ESC [ < Pb ; Px ; Py M (press) or m (release)
;;; Terminated by 'M' (press) or 'm' (release).
;;;
;;; ASCII 'M' = 77.  It serves as both the X10 mouse-sequence intro final byte
;;; and the SGR press final byte.  A single named constant covers both roles.

(defconstant +byte-ascii-m+     77  "ASCII 'M' (0x4D) — X10 mouse intro and SGR press final.")
(defconstant +byte-sgr-press+   77  "ASCII 'M' (0x4D) — SGR mouse press final byte.")
(defconstant +byte-sgr-release+ 109 "ASCII 'm' (0x6D) — SGR mouse release final byte.")

(defun %parse-sgr-mouse (buffer length)
  "Parse an SGR mouse sequence from BUFFER (of LENGTH bytes).
   Expected: ESC [ < Pb ; Px ; Py M|m
   Returns (values btn col row release-p) on success, or (values nil nil nil nil) on failure.
   Coordinates in BUFFER are 1-based; returned col/row are 0-based."
  ;; Minimum: ESC [ < D ; D ; D M = 9 bytes
  (when (and (>= length 9)
             (= (aref buffer 0) +byte-esc+)
             (= (aref buffer 1) +byte-csi-bracket+)
             (= (aref buffer 2) +byte-sgr-lt+))
    (let* ((parameter-string (map 'string #'code-char (subseq buffer 3 length)))
           (final-char        (char parameter-string (1- (length parameter-string))))
           (release-p         (char= final-char #\m))
           (params-str        (subseq parameter-string 0 (1- (length parameter-string))))
           (parts             (loop for start = 0 then (1+ semi)
                                    for semi  = (position #\; params-str :start start)
                                    collect (subseq params-str start (or semi (length params-str)))
                                    while semi)))
      (when (= (length parts) 3)
        (let ((btn (parse-integer (first  parts) :junk-allowed t))
              (col (parse-integer (second parts) :junk-allowed t))
              (row (parse-integer (third  parts) :junk-allowed t)))
          (when (and (integerp btn) (integerp col) (integerp row))
            ;; SGR coords are 1-based; convert to 0-based
            (values btn (1- col) (1- row) release-p)))))))

(defun %sgr-mouse-sequence-p (buffer length)
  "True when BUFFER looks like the start of an SGR mouse sequence: ESC [ <."
  (and (>= length 3)
       (= (aref buffer 0) +byte-esc+)
       (= (aref buffer 1) +byte-csi-bracket+)
       (= (aref buffer 2) +byte-sgr-lt+)))

(defun %sgr-mouse-terminated-p (buffer length)
  "True when BUFFER ends with 'M' (press) or 'm' (release) — SGR mouse final byte."
  (when (> length 3)
    (let ((last-byte (aref buffer (1- length))))
      (or (= last-byte +byte-sgr-press+)
          (= last-byte +byte-sgr-release+)))))

(defun %csi-u-terminated-p (buffer length)
  "True when BUFFER holds a complete CSI-u sequence ESC [ <digits/;> u — i.e. the
   final byte is 'u' and every parameter byte between '[' and 'u' is a digit or ';'.
   The all-digit/semicolon middle excludes mouse (M / <) and arrow/function-key
   finals, so only genuine extended-keys sequences match."
  (and (>= length 4)
       (= (aref buffer 1) +byte-csi-bracket+)
       (= (aref buffer (1- length)) +byte-ascii-u+)
       (loop for i from 2 below (1- length)
             for b = (aref buffer i)
             always (or (<= +byte-digit-0+ b +byte-digit-9+)
                        (= b +byte-csi-semi+)))))

(defun %csi-u-accumulating-p (buffer length)
  "True when BUFFER is the in-progress prefix of a CSI-u sequence: ESC [ <digit>
   followed only by digits/semicolons, not yet terminated, and under the length
   bound (16 — a max-codepoint chord ESC [ 1114111 ; 8 u is 12 bytes).  The leading
   digit distinguishes it from mouse (buf[2] = M / <) and arrow (buf[2] a letter)
   CSI sequences, so accumulation defers their premature forwarding until the 'u'
   terminator (or a non-CSI-u byte) arrives."
  (and (>= length 3)
       (< length 16)
       (= (aref buffer 1) +byte-csi-bracket+)
       (<= +byte-digit-0+ (aref buffer 2) +byte-digit-9+)
       (let ((last (aref buffer (1- length))))
         (or (<= +byte-digit-0+ last +byte-digit-9+)
             (= last +byte-csi-semi+)))))

;;; ── make-escape-input-k: sub-state decoder helpers ──────────────────────────
;;;
;;; The escape accumulator is decomposed into named helper functions so each
;;; protocol cohort (X10 mouse, SGR mouse, copy-mode CSI, function-key) is
;;; independently readable and testable.

(defun %handle-escape-x10-mouse (session buffer)
  "Decode a complete 6-byte X10 mouse sequence from BUFFER and dispatch it.
   Returns (values nil #'%ground-input-state) always."
  (let* ((raw-btn   (aref buffer 3))
         (raw-col   (aref buffer 4))
         (raw-row   (aref buffer 5))
         ;; X10 encoding: btn+32, col/row+33 (1-based → subtract 1 for 0-based)
         (btn       (- raw-btn 32))
         (col       (- raw-col 33))
         (row       (- raw-row 33))
         (release-p (= raw-btn (+ +mouse-btn-release-x10+ 32))))  ; btn 3+32=35 = release in X10
    (%dispatch-mouse-event session btn col row release-p))
  (values nil #'%ground-input-state))

(defun %handle-escape-sgr-mouse (session buffer length)
  "Dispatch a completed SGR mouse sequence from BUFFER (LENGTH bytes).
   Returns (values nil #'%ground-input-state) always."
  (multiple-value-bind (btn col row release-p)
      (%parse-sgr-mouse buffer length)
    (when btn
      (%dispatch-mouse-event session btn col row release-p)))
  (values nil #'%ground-input-state))

(defun %csi-tilde-parse (buffer length)
  "Parse an ESC [ <param> [ ; <mod> ] ~ sequence (the fields live at indices
   2..LENGTH-2).  Returns (values PARAM MOD-VALUE); MOD-VALUE is 1 when no ';mod'
   field is present (the unmodified key).  Returns NIL when a field is empty or
   non-numeric, so a malformed sequence falls through to a raw pane forward.
   The ';mod' field is what makes a modified function key — ESC [ 1 5 ; 5 ~ is
   Ctrl+F5 (PARAM 15, MOD 5)."
  (let ((semi (position +byte-csi-semi+ buffer :start 2 :end (1- length))))
    (flet ((digits (start end)
             (when (< start end)
               (let ((value 0))
                 (loop for i from start below end
                       for byte = (aref buffer i)
                       do (if (<= +byte-digit-0+ byte +byte-digit-9+)
                              (setf value (+ (* value 10) (- byte +byte-digit-0+)))
                              (return-from %csi-tilde-parse nil)))
                 value))))
      (if semi
          (let ((param (digits 2 semi))
                (mod   (digits (1+ semi) (1- length))))
            (if (and param mod) (values param mod) nil))
          (let ((param (digits 2 (1- length))))
            (when param (values param 1)))))))

(defun %csi-tilde-key (buffer length)
  "Canonical key name for an ESC [ <param> [;<mod>] ~ sequence, or NIL.  Joins the
   base navigation/function key (%csi-tilde-key-name) with any modifier prefix
   (%modifier-prefix): ESC[15~ → \"F5\", ESC[15;5~ → \"C-F5\", ESC[1;2~ → \"S-Home\"."
  (multiple-value-bind (param mod) (%csi-tilde-parse buffer length)
    (let ((base (and param (%csi-tilde-key-name param))))
      (when base (concatenate 'string (%modifier-prefix (or mod 1)) base)))))

(defun %csi-tilde-key-name (param)
  "Map the numeric PARAM of an ESC [ <param> ~ sequence to its canonical tmux key
   name, or NIL when PARAM is not a recognised navigation/function key.  Covers
   Home/End/Insert/Delete, PageUp/PageDown, and the vt-style F1–F12 finals.  The
   names match those produced by %parse-key-token on the bind side (after its
   alias normalisation), so `bind -n F5 <cmd>` / `bind -n PPage <cmd>` resolve."
  (case param
    ((1 7) "Home")
    (2     "Insert")
    (3     "Delete")
    ((4 8) "End")
    (5     "PageUp")
    (6     "PageDown")
    (11 "F1") (12 "F2") (13 "F3") (14 "F4")
    (15 "F5") (17 "F6") (18 "F7") (19 "F8")
    (20 "F9") (21 "F10") (23 "F11") (24 "F12")
    (otherwise nil)))

(defun %handle-escape-csi-tilde (session buffer length)
  "Handle a complete ESC [ <param> ~ sequence at root (LENGTH bytes).  A root-table
   binding for the canonical key name wins first, so `bind -n F5 <cmd>`,
   `bind -n PageUp <cmd>`, `bind -n Home <cmd>` … fire.  Failing that the legacy
   behaviour is preserved: PageUp/PageDown scroll in copy mode, and any other or
   unbound key is forwarded raw so the pane's application still receives it.
   Returns (values nil #'%ground-input-state)."
  (multiple-value-bind (param mod) (%csi-tilde-parse buffer length)
    (let* ((base (and param (%csi-tilde-key-name param)))
           (key  (and base (concatenate 'string (%modifier-prefix (or mod 1)) base))))
      (cond
        ;; A user binding (`bind -n F5`/`bind -n C-F5`/`bind -n PageUp`) wins.
        ((and key (%try-bound-string-key session +table-root+ key)))
        ;; PageUp in copy mode (unmodified only): scroll up one screenful.
        ((and (eql param 5) (eql mod 1) (%copy-mode-active-p session))
         (let ((screen (%active-screen session)))
           (when screen
             (copy-mode-scroll screen (screen-height screen))
             (setf *dirty* t))))
        ;; PageDown in copy mode (unmodified only): scroll down one screenful.
        ((and (eql param 6) (eql mod 1) (%copy-mode-active-p session))
         (let ((screen (%active-screen session)))
           (when screen
             (copy-mode-scroll screen (- (screen-height screen)))
             (setf *dirty* t))))
        ;; Unbound and not a copy-mode scroll: forward raw bytes to the pane.
        (t
         (unless (%copy-mode-active-p session)
           (%forward-octets-synchronized session (subseq buffer 0 length)))))))
  (values nil #'%ground-input-state))

(defun %ss3-key-name (final-byte)
  "Map the final byte of an SS3 sequence ESC O <final> to its canonical tmux key
   name, or NIL when it is not a recognised bindable key.  Covers F1-F4 (the
   xterm/screen encoding `ESC O P/Q/R/S`, which the ESC [ N ~ path does NOT carry)
   and Home/End (ESC O H/F).  Names match %parse-key-token's bind-side output."
  (case (code-char final-byte)
    (#\P "F1") (#\Q "F2") (#\R "F3") (#\S "F4")
    (#\H "Home") (#\F "End")
    (otherwise nil)))

(defun %handle-escape-ss3 (session buffer)
  "Handle a complete 3-byte SS3 sequence ESC O <final> from BUFFER.  A root-table
   binding for the canonical key name wins first, so `bind -n F1 <cmd>` fires;
   otherwise the raw 3 bytes are forwarded to the pane so the application still
   receives the function key (transparency).  Returns ground state.

   Deferring ESC O to reach this point sacrifices a `bind -n M-O` *binding* (ESC O
   is now read as an SS3 prefix, matching tmux's own resolution), but a held Alt+O
   still reaches the pane: %flush-esc-if-timed-out replays the buffered ESC O."
  (let ((key (%ss3-key-name (aref buffer 2))))
    (unless (and key (%try-bound-string-key session +table-root+ key))
      (unless (%copy-mode-active-p session)
        (%forward-octets-synchronized session (subseq buffer 0 3)))))
  (values nil #'%ground-input-state))

(defun %handle-escape-csi-3byte (session buffer)
  "Handle a 3-byte CSI sequence ESC [ FINAL from BUFFER (not X10 / not SGR).
   If the third byte is a digit, returns (values T NIL) meaning keep accumulating.
   Otherwise dispatches copy-mode-escape or forwards the bytes to the pane,
   then returns (values NIL #'%ground-input-state)."
  (let ((third-byte (aref buffer 2)))
    (if (and (>= third-byte +byte-digit-0+) (<= third-byte +byte-digit-9+))
        ;; Could be ESC [ N ~ (4-byte); keep accumulating
        (values t nil)
        ;; Non-digit final byte: dispatch or forward
        (progn
          (unless (handle-copy-mode-escape session buffer)
            ;; Not in copy mode (or unrecognised): forward raw bytes to pane.
            ;; When application cursor keys mode is active (DEC ?1h), remap
            ;; ESC [ A/B/C/D (CSI) to ESC O A/B/C/D (SS3) before forwarding.
            (unless (%copy-mode-active-p session)
              (let* ((screen   (%active-screen session))
                     (app-keys (and screen (screen-app-cursor-keys screen)))
                     (ss3-seq  (and app-keys (%arrow-final-to-ss3-bytes third-byte))))
                (if ss3-seq
                    (%forward-octets-synchronized session ss3-seq)
                    (%forward-octets-synchronized session (subseq buffer 0 3))))))
          (values nil #'%ground-input-state)))))

(defun make-escape-input-k (session buffer)
  "CPS continuation: accumulate an ESC [... sequence one byte at a time.

   X10 mouse: ESC [ M <btn+32> <col+33> <row+33> — 6 bytes total.
     Detected when buf[0]=ESC buf[1]=[ buf[2]=M and we still need 3 more bytes.
     Dispatched via %DISPATCH-MOUSE-EVENT when length reaches 6.

   SGR mouse: ESC [ < Pb ; Px ; Py M|m — variable length, terminated by M or m.
     Detected when buf[2]='<' (60).  Accumulated until final byte M or m arrives.

   Copy-mode 3-byte CSI (ESC [ FINAL): try HANDLE-COPY-MODE-ESCAPE; if not
     handled and not in copy mode, forward to the active pane.

   2-byte non-CSI (ESC X): forward to the active pane.

   Otherwise: keep accumulating."
  ;; SESSION is captured from the make-escape-input-k call; the lambda parameter
  ;; _ignored-session is structurally required by the CPS protocol (SESSION BYTE)
  ;; → values, but is always the same object as the captured SESSION.  We ignore
  ;; the parameter to keep the protocol uniform across all CPS state functions.
  (lambda (_ignored-session byte)
    (declare (ignore _ignored-session))
    (vector-push-extend byte buffer)
    ;; Expose the growing buffer so %flush-esc-if-timed-out can replay the FULL
    ;; partial sequence (e.g. a held ESC O) rather than dropping all but the ESC.
    (setf *esc-accum-buffer* buffer)
    (let ((length (fill-pointer buffer)))
      (cond
        ;; ── SS3 introducer: ESC O — defer one byte to disambiguate ───────
        ;; ESC O P/Q/R/S (F1-F4) and ESC O H/F (Home/End) vs Alt+O.  Keep
        ;; accumulating; if no third byte arrives, escape-time flushes the
        ;; buffered ESC O to the pane (Alt+O passthrough preserved).
        ((and (= length 2) (= (aref buffer 1) +byte-ss3-o+))
         (values nil (make-escape-input-k session buffer)))
        ;; ── SS3 function key complete: ESC O <final> ─────────────────────
        ((and (= length 3) (= (aref buffer 1) +byte-ss3-o+))
         (%handle-escape-ss3 session buffer))
        ;; ── X10 mouse: complete 6-byte sequence ESC [ M btn col row ──────
        ((and (= length 6)
              (= (aref buffer 1) +byte-csi-bracket+)
              (= (aref buffer 2) +byte-ascii-m+))
         (%handle-escape-x10-mouse session buffer))
        ;; ── Still accumulating X10 mouse intro (ESC [ M + up to 2 more) ──
        ((and (>= length 3)
              (= (aref buffer 1) +byte-csi-bracket+)
              (= (aref buffer 2) +byte-ascii-m+)
              (< length 6))
         (values nil (make-escape-input-k session buffer)))
        ;; ── SGR mouse terminated: ESC [ < Pb ; Px ; Py M|m ───────────────
        ((and (%sgr-mouse-sequence-p buffer length)
              (%sgr-mouse-terminated-p buffer length))
         (%handle-escape-sgr-mouse session buffer length))
        ;; ── SGR mouse still accumulating ──────────────────────────────────
        ((%sgr-mouse-sequence-p buffer length)
         (values nil (make-escape-input-k session buffer)))
        ;; ── CSI-u extended-keys complete: ESC [ <codepoint> ; <mod> u ─────
        ;; Placed before the 3-byte CSI / function-key / modifier-arrow branches
        ;; so a digit-leading u-terminated chord is decoded here rather than being
        ;; misread as an arrow/function key.  %handle-escape-csi-u resolves a
        ;; binding by name, else re-injects the legacy byte form for transparency.
        ((%csi-u-terminated-p buffer length)
         (%handle-escape-csi-u session buffer length)
         (values nil #'%ground-input-state))
        ;; ── CSI-u still accumulating: ESC [ <digits/;> (no terminator yet) ─
        ;; Defers the digit-leading CSI so multi-digit codepoints (ESC [ 9 7 …)
        ;; are not eaten by the generic 3-/4-byte forwards below.  Real
        ;; modifier-arrows (ESC [ 1 ; N FINAL) pass through here too and resolve
        ;; at the modifier-arrow-complete branch once the final letter arrives.
        ((%csi-u-accumulating-p buffer length)
         (values nil (make-escape-input-k session buffer)))
        ;; ── Focus in/out: ESC [ I (gained) / ESC [ O (lost) from the outer ─
        ;; terminal's ?1004 reporting.  Deliver the focus change to the active
        ;; pane (its app gets ESC[I/ESC[O when it enabled ?1004); never forward
        ;; the raw bytes.  Placed before the generic 3-byte CSI handling.
        ((and (= length 3) (= (aref buffer 1) +byte-csi-bracket+)
              (or (= (aref buffer 2) +byte-focus-in+)
                  (= (aref buffer 2) +byte-focus-out+)))
         (%notify-pane-focus (session-active-pane session)
                             (= (aref buffer 2) +byte-focus-in+))
         (values nil #'%ground-input-state))
        ;; ── 3-byte CSI: ESC [ FINAL — not X10 and not SGR ────────────────
        ((and (= length 3) (= (aref buffer 1) +byte-csi-bracket+)
              (/= (aref buffer 2) +byte-ascii-m+)
              (/= (aref buffer 2) +byte-sgr-lt+))
         (multiple-value-bind (keep-accumulating next-state)
             (%handle-escape-csi-3byte session buffer)
           (if keep-accumulating
               (values nil (make-escape-input-k session buffer))
               (values nil next-state))))
        ;; ── Function / navigation key: ESC [ <digits> ~ (any width) ──────
        ;; Single-digit (Home ESC [ 1 ~, PageUp ESC [ 5 ~, Delete ESC [ 3 ~)
        ;; and multi-digit (F5 ESC [ 15 ~ … F12 ESC [ 24 ~) alike.  Map the
        ;; parameter to a canonical key name and run a root binding
        ;; (`bind -n F5 <cmd>`); unbound keys scroll copy mode (PageUp/Down)
        ;; or forward raw to the pane.  Placed ahead of the modifier-arrow
        ;; (ESC [ 1 ; MOD …) branches — those end in a letter, not '~', so the
        ;; tilde terminator keeps the two cohorts disjoint.
        ((and (>= length 4) (= (aref buffer 1) +byte-csi-bracket+)
              (<= +byte-digit-0+ (aref buffer 2) +byte-digit-9+)
              (= (aref buffer (1- length)) +byte-tilde+))
         (%handle-escape-csi-tilde session buffer length))
        ;; ── Modifier+arrow at root: ESC [ 1 ; MOD FINAL (6 bytes) ─────────
        ;; A bare (no-prefix) C-Up / M-Left / S-Down etc.  Without this branch
        ;; the generic 4-byte forward below would ship "ESC [ 1 ;" to the pane
        ;; and mangle the chord.  Accumulate ESC [ 1 ; ... until the final
        ;; letter, then look up the canonical key name in the ROOT table so
        ;; `bind -n C-Up <cmd>` fires.  Unbound chords (or non-arrow finals such
        ;; as Ctrl+Home) fall through to the pane unchanged.
        ((and (>= length 4) (<= length 5)
              (= (aref buffer 1) +byte-csi-bracket+)
              (= (aref buffer 2) +byte-csi-param-1+)
              (= (aref buffer 3) +byte-csi-semi+))
         (values nil (make-escape-input-k session buffer)))
        ((and (= length 6)
              (= (aref buffer 1) +byte-csi-bracket+)
              (= (aref buffer 2) +byte-csi-param-1+)
              (= (aref buffer 3) +byte-csi-semi+))
         (let ((key (%modifier-arrow-key-name (aref buffer 4) (aref buffer 5))))
           (unless (%try-bound-string-key session +table-root+ key)
             (unless (%copy-mode-active-p session)
               (%forward-octets-synchronized session (subseq buffer 0 length)))))
         (values nil #'%ground-input-state))
        ;; ── Digit-leading CSI with a non-'u' terminator — forward raw ─────
        ;; Reached after the arrow/function-key handlers above declined it: an
        ;; accumulated digit CSI that ended in something other than 'u' (F5–F12
        ;; ESC [ 15 ~, a paste marker ESC [ 200 ~, a modified function key
        ;; ESC [ 5 ; 5 ~, …).  These have no multiplexer binding, so forward the
        ;; whole sequence to the pane unchanged — matching the legacy behaviour
        ;; the generic forward gave before CSI-u accumulation deferred them.
        ((and (>= length 4)
              (= (aref buffer 1) +byte-csi-bracket+)
              (<= +byte-digit-0+ (aref buffer 2) +byte-digit-9+))
         (unless (%copy-mode-active-p session)
           (%forward-octets-synchronized session (subseq buffer 0 length)))
         (values nil #'%ground-input-state))
        ;; ── 4-byte accumulation: ESC [ N (not yet '~') — keep buffering ───
        ((and (= length 4) (= (aref buffer 1) +byte-csi-bracket+)
              (/= (aref buffer 3) +byte-tilde+))
         ;; Forward if no terminating ~ and not copy mode, return to ground
         (unless (%copy-mode-active-p session)
           (%forward-octets-synchronized session (subseq buffer 0 length)))
         (values nil #'%ground-input-state))
        ;; ── 2-byte non-CSI sequence: ESC X ────────────────────────────────
        ;; In copy mode, a lone ESC (or ESC + non-CSI byte) clears any active
        ;; selection but STAYS in copy mode (tmux default: Escape → clear-selection).
        ;; Use `q` or `i` to exit copy mode.
        ;; Outside copy mode, a `bind -n M-<key>` root binding (ESC <key>)
        ;; overrides forwarding; only when unbound do we forward to the pane.
        ((and (= length 2) (/= (aref buffer 1) +byte-csi-bracket+))
         (cond
           ;; In copy mode: check copy-mode-vi / copy-mode key tables for an
           ;; M-<key> binding first (emacs meta bindings: M-f, M-b, M-w, …).
           ;; Only fall through to clear-selection if no table entry matches.
           ((%copy-mode-active-p session)
            (let* ((meta-name (%meta-key-name (aref buffer 1)))
                   (entry     (or (and meta-name (key-table-lookup "copy-mode-vi" meta-name))
                                  (and meta-name (key-table-lookup "copy-mode"    meta-name)))))
              (if entry
                  (progn (%run-key-table-binding session entry nil)
                         (setf *dirty* t))
                  ;; No table binding: ESC clears the active selection.
                  (let ((screen (%active-screen session)))
                    (when screen (copy-mode-clear-selection screen))
                    (setf *dirty* t)))))
           ((%try-bound-string-key session +table-root+
                                   (%meta-key-name (aref buffer 1))))
           (t
            (%forward-octets-synchronized session (subseq buffer 0 length))))
         (values nil #'%ground-input-state))
        ;; ── Buffer overflow guard (> 32 unrecognised bytes) ───────────────
        ((> length 32)
         (unless (%copy-mode-active-p session)
           (%forward-octets-synchronized session (subseq buffer 0 length)))
         (values nil #'%ground-input-state))
        ;; ── Still accumulating ─────────────────────────────────────────────
        (t (values nil (make-escape-input-k session buffer)))))))
