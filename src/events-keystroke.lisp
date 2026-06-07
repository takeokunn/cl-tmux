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
  ((= byte +prefix-key-code+)
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
            (let* ((ch    (code-char byte))
                   (entry (or (key-table-lookup "copy-mode-vi" ch)
                              (key-table-lookup "copy-mode" ch))))
              (when entry
                (let ((cmd (key-table-command entry)))
                  (if (consp cmd)
                      (%run-command-tokens session cmd)
                      (dispatch-command session cmd byte)))))
            (flet ((repeat (fn)
                     "Call FN COUNT times on SCREEN."
                     (dotimes (_ count) (funcall fn screen))))
              (case byte
                ;; q / i — exit copy mode
                (#.+byte-q+ (copy-mode-exit screen))
                (105        (copy-mode-exit screen))               ; i
                ;; h — move cursor left
                (104        (repeat (lambda (s) (copy-mode-move-cursor s :left))))  ; h
                ;; l — move cursor right
                (108        (repeat (lambda (s) (copy-mode-move-cursor s :right)))) ; l
                ;; j / C-n (14) — move cursor down (viewport follows at edge)
                ((#.+byte-j+ 14)
                 (repeat (lambda (s) (copy-mode-move-cursor s :down))))
                ;; k / C-p (16) — move cursor up (viewport follows at edge)
                ((#.+byte-k+ 16)
                 (repeat (lambda (s) (copy-mode-move-cursor s :up))))
                ;; w — word forward
                (119        (repeat #'copy-mode-word-forward))        ; w
                ;; b — word backward
                (98         (repeat #'copy-mode-word-backward))       ; b
                ;; e — word end
                (101        (repeat #'copy-mode-word-end))            ; e
                ;; 0 — line start (bare '0' with no prefix)
                (48         (copy-mode-line-start screen))            ; 0
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
                ;; A — append selection to paste buffer
                (65         (copy-mode-append-selection screen))      ; A
                ;; r — toggle rectangle select
                (114        (copy-mode-toggle-rectangle screen))      ; r
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
                ;; Any other byte is consumed without forwarding (no passthrough in copy mode)
                (otherwise nil)))))))
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

(defun %dispatch-modifier-arrow (session mod-byte final-byte)
  "Handle the modifier+arrow combination inside the 6-byte CSI sequence.
   MOD-BYTE is +byte-csi-mod-ctrl+ (Ctrl) or +byte-csi-mod-meta+ (Meta).
   FINAL-BYTE is the arrow final byte.  Dispatches resize-pane or :resize-*."
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
    (let ((length (fill-pointer buffer)))
      (cond
        ;; Complete 3-byte CSI sequence: ESC [ FINAL
        ((and (= length 3) (= (aref buffer 1) +byte-csi-bracket+))
         (let ((final-byte (aref buffer 2)))
           (cond
             ;; ESC [ 1 may be start of ESC [ 1 ; MOD FINAL — keep accumulating
             ((= final-byte +byte-csi-param-1+)
              (values nil (%make-prefix-csi-k session buffer)))
             (t
              (let ((command (%prefix-csi-arrow-cmd final-byte)))
                ;; dispatch-command always returns NIL; the when's value is discarded.
                (when command (dispatch-command session command nil))
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
        ;; 2-byte non-CSI: silently discard after prefix (no passthrough)
        ((and (= length 2) (/= (aref buffer 1) +byte-csi-bracket+))
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

(defun %handle-escape-function-key (session buffer)
  "Handle a complete 4-byte ESC [ N ~ function-key sequence from BUFFER.
   Dispatches PageUp/PageDown in copy mode; forwards to pane otherwise.
   Returns (values nil #'%ground-input-state)."
  (let ((parameter-byte (aref buffer 2)))
    (cond
      ;; PageUp in copy mode: ESC [ 5 ~
      ((and (= parameter-byte +byte-page-up-param+) (%copy-mode-active-p session))
       (let ((screen (%active-screen session)))
         (when screen
           (copy-mode-scroll screen (screen-height screen))
           (setf *dirty* t))))
      ;; PageDown in copy mode: ESC [ 6 ~
      ((and (= parameter-byte +byte-page-down-param+) (%copy-mode-active-p session))
       (let ((screen (%active-screen session)))
         (when screen
           (copy-mode-scroll screen (- (screen-height screen)))
           (setf *dirty* t))))
      ;; Outside copy mode: forward raw bytes to pane
      (t
       (unless (%copy-mode-active-p session)
         (%forward-octets session (subseq buffer 0 4))))))
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
                    (%forward-octets session ss3-seq)
                    (%forward-octets session (subseq buffer 0 3))))))
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
    (let ((length (fill-pointer buffer)))
      (cond
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
        ;; ── 3-byte CSI: ESC [ FINAL — not X10 and not SGR ────────────────
        ((and (= length 3) (= (aref buffer 1) +byte-csi-bracket+)
              (/= (aref buffer 2) +byte-ascii-m+)
              (/= (aref buffer 2) +byte-sgr-lt+))
         (multiple-value-bind (keep-accumulating next-state)
             (%handle-escape-csi-3byte session buffer)
           (if keep-accumulating
               (values nil (make-escape-input-k session buffer))
               (values nil next-state))))
        ;; ── 4-byte function key: ESC [ N ~ ────────────────────────────────
        ;; PageUp = ESC [ 5 ~ (+byte-page-up-param+ / +byte-tilde+).
        ;; PageDown = ESC [ 6 ~ (+byte-page-down-param+ / +byte-tilde+).
        ((and (= length 4) (= (aref buffer 1) +byte-csi-bracket+)
              (= (aref buffer 3) +byte-tilde+))
         (%handle-escape-function-key session buffer))
        ;; ── 4-byte accumulation: ESC [ N (not yet '~') — keep buffering ───
        ((and (= length 4) (= (aref buffer 1) +byte-csi-bracket+)
              (/= (aref buffer 3) +byte-tilde+))
         ;; Forward if no terminating ~ and not copy mode, return to ground
         (unless (%copy-mode-active-p session)
           (%forward-octets session (subseq buffer 0 length)))
         (values nil #'%ground-input-state))
        ;; ── 2-byte non-CSI sequence: ESC X ────────────────────────────────
        ;; In copy mode, a lone ESC (or ESC + non-CSI byte) exits copy mode.
        ;; Outside copy mode, forward the raw bytes to the pane.
        ((and (= length 2) (/= (aref buffer 1) +byte-csi-bracket+))
         (if (%copy-mode-active-p session)
             (let ((screen (%active-screen session)))
               (when screen (copy-mode-exit screen))
               (setf *dirty* t))
             (%forward-octets session (subseq buffer 0 length)))
         (values nil #'%ground-input-state))
        ;; ── Buffer overflow guard (> 32 unrecognised bytes) ───────────────
        ((> length 32)
         (unless (%copy-mode-active-p session)
           (%forward-octets session (subseq buffer 0 length)))
         (values nil #'%ground-input-state))
        ;; ── Still accumulating ─────────────────────────────────────────────
        (t (values nil (make-escape-input-k session buffer)))))))
