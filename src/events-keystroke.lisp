(in-package #:cl-tmux)

;;;; CPS keystroke state machine and escape sequence processing.

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
     ((= byte 106)  (overlay-scroll 1)  (setf *dirty* t))   ; j
     ;; scroll up one line
     ((= byte 107)  (overlay-scroll -1) (setf *dirty* t))   ; k
     ;; dismiss
     ((= byte 113)  (clear-overlay)     (setf *dirty* t))   ; q
     ((= byte 27)                                            ; Esc — may be arrow
      (let ((buf (make-array 8 :element-type '(unsigned-byte 8)
                               :fill-pointer 0 :adjustable t)))
        (vector-push-extend byte buf)
        (setf *dirty* t)
        (return-from %ground-input-state
          (values nil (make-overlay-escape-k buf)))))
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
  ((let ((entry (key-table-lookup "root" (code-char byte))))
     (when entry
       (dispatch-command session (key-table-command entry) byte)
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
   (let ((buf (make-array 8 :element-type '(unsigned-byte 8)
                            :fill-pointer 0 :adjustable t)))
     (vector-push-extend byte buf)
     (values nil (make-escape-input-k session buf))))
  ;; ── Copy-mode single-byte navigation (unprefixed) ─────────────────────────
  ;; These keys are intercepted ONLY when copy mode is active; they are never
  ;; forwarded to the pane.  The check comes before the default forward branch.
  ((copy-mode-active-p session)
   (let ((sc (%active-screen session)))
     (when sc
       (case byte
         ;; q / i — exit copy mode
         (#.+byte-q+ (copy-mode-exit sc))
         (105        (copy-mode-exit sc))               ; i
         ;; h — move cursor left
         (104        (copy-mode-move-cursor sc :left))  ; h
         ;; l — move cursor right
         (108        (copy-mode-move-cursor sc :right)) ; l
         ;; j / C-n (14) — move cursor down (viewport follows at edge)
         ((106 14)   (copy-mode-move-cursor sc :down))  ; j, C-n
         ;; k / C-p (16) — move cursor up (viewport follows at edge)
         ((107 16)   (copy-mode-move-cursor sc :up))    ; k, C-p
         ;; w — word forward
         (119        (copy-mode-word-forward sc))        ; w
         ;; b — word backward
         (98         (copy-mode-word-backward sc))       ; b
         ;; e — word end
         (101        (copy-mode-word-end sc))            ; e
         ;; 0 — line start
         (48         (copy-mode-line-start sc))          ; 0
         ;; $ — line end
         (36         (copy-mode-line-end sc))            ; $
         ;; g — jump to top (maximum scrollback)
         (103        (copy-mode-top sc))
         ;; G — jump to bottom (offset = 0, live view)
         (71         (copy-mode-bottom sc))
         ;; H — cursor to top of screen
         (72         (copy-mode-high sc))                ; H
         ;; M — cursor to middle of screen
         (#.(char-code #\M) (copy-mode-middle sc))      ; M
         ;; L — cursor to bottom of screen
         (76         (copy-mode-low sc))                 ; L
         ;; C-f (6) — page down
         (6          (copy-mode-page-down sc))
         ;; C-b (2) — page up
         (2          (copy-mode-page-up sc))
         ;; C-u (21) — scroll up half page
         (21         (copy-mode-half-page-up sc))
         ;; C-d (4) — scroll down half page
         (4          (copy-mode-half-page-down sc))
         ;; C-e (5) — scroll down one line
         (5          (copy-mode-scroll-down-line sc))
         ;; C-y (25) — scroll up one line
         (25         (copy-mode-scroll-up-line sc))
         ;; V — begin line selection
         (86         (copy-mode-begin-line-selection sc)) ; V
         ;; D — copy to end of line
         (68         (copy-mode-copy-end-of-line sc))    ; D
         ;; Y — copy current line
         (89         (copy-mode-copy-line sc))           ; Y
         ;; Space / v — begin selection
         ((32 118)   (copy-mode-begin-selection sc))
         ;; y — yank selection
         (121        (copy-mode-yank sc))
         ;; n — search next
         (110        (copy-mode-search-next sc))         ; n
         ;; N — search prev
         (78         (copy-mode-search-prev sc))         ; N
         ;; Any other byte is consumed without forwarding (no passthrough in copy mode)
         (otherwise nil)))
     (setf *dirty* t))
   (values nil #'%ground-input-state))
  ;; ── Default: forward raw byte to active pane (+ synchronize-panes broadcast) ─
  (t
   (%forward-octets-synchronized session
                                  (make-array 1 :element-type '(unsigned-byte 8)
                                               :initial-element byte))
   (values nil #'%ground-input-state)))

(defun %make-prefix-csi-k (session buf)
  "CPS continuation: accumulate ESC [ FINAL for post-prefix arrow key sequences.
   Dispatches :select-pane-up/down/left/right on ESC [ A/B/D/C (3-byte CSI).
   Dispatches C-arrow (ESC [ 1 ; 5 FINAL, 8 bytes) to :resize-{dir} (1 cell).
   Dispatches M-arrow (ESC [ 1 ; 3 FINAL, 8 bytes) to :resize-{dir} (5 cells).
   Unrecognised sequences are silently discarded (no passthrough-prefix)."
  ;; SESSION is captured from the %make-prefix-csi-k call; _session is ignored
  ;; for the same reason as in make-escape-input-k (see comment there).
  (lambda (_session byte)
    (declare (ignore _session))
    (vector-push-extend byte buf)
    (let ((len (fill-pointer buf)))
      (cond
        ;; Complete 3-byte CSI sequence: ESC [ FINAL
        ((and (= len 3) (= (aref buf 1) +byte-csi-bracket+))
         (let* ((final (aref buf 2))
                ;; Detect whether this is the start of a longer modifier sequence:
                ;; ESC [ 1 — intermediate, not a final CSI byte yet.
                (is-param-1 (= final 49))) ; '1' = 49
           (cond
             ;; ESC [ 1 may be start of ESC [ 1 ; MOD FINAL — keep accumulating
             (is-param-1
              (values nil (%make-prefix-csi-k session buf)))
             (t
              (let ((cmd (case final
                           (65 :select-pane-up)
                           (66 :select-pane-down)
                           (67 :select-pane-right)
                           (68 :select-pane-left)
                           (otherwise nil))))
                ;; Unrecognised 3-byte CSI: silently discard (no passthrough).
                (values (when cmd (dispatch-command session cmd nil))
                        #'%ground-input-state))))))
        ;; 4-byte sequence starting ESC [ 1 ; — keep accumulating
        ((and (= len 4) (= (aref buf 1) +byte-csi-bracket+)
              (= (aref buf 2) 49)  ; '1'
              (= (aref buf 3) 59)) ; ';'
         (values nil (%make-prefix-csi-k session buf)))
        ;; 5-byte: ESC [ 1 ; MOD — keep accumulating for the final letter
        ((and (= len 5) (= (aref buf 1) +byte-csi-bracket+)
              (= (aref buf 2) 49) (= (aref buf 3) 59))
         (values nil (%make-prefix-csi-k session buf)))
        ;; Complete 6-byte modifier CSI: ESC [ 1 ; MOD FINAL
        ;; (where MOD=53='5' for Ctrl, MOD=51='3' for Meta)
        ((and (= len 6) (= (aref buf 1) +byte-csi-bracket+)
              (= (aref buf 2) 49) (= (aref buf 3) 59))
         (let* ((mod   (aref buf 4))
                (final (aref buf 5))
                (cmd
                  (cond
                    ;; C-arrow: ESC [ 1 ; 5 FINAL  (mod=53='5') → resize 1 cell
                    ((= mod 53)
                     (case final
                       (65 :c-arrow-up)    ; A
                       (66 :c-arrow-down)  ; B
                       (67 :c-arrow-right) ; C
                       (68 :c-arrow-left)  ; D
                       (otherwise nil)))
                    ;; M-arrow: ESC [ 1 ; 3 FINAL  (mod=51='3') → resize 5 cells
                    ((= mod 51)
                     (case final
                       (65 :resize-up)    ; A
                       (66 :resize-down)  ; B
                       (67 :resize-right) ; C
                       (68 :resize-left)  ; D
                       (otherwise nil)))
                    (t nil))))
           ;; C-arrow bindings dispatch resize-pane with amount=1 directly.
           ;; M-arrow bindings use the standard :resize-* commands (amount=5).
           ;; Unrecognised modifier sequence: silently discard.
           (let ((win (session-active-window session)))
             (when win
               (case cmd
                 (:c-arrow-up    (resize-pane win :up    1))
                 (:c-arrow-down  (resize-pane win :down  1))
                 (:c-arrow-right (resize-pane win :right 1))
                 (:c-arrow-left  (resize-pane win :left  1))
                 (otherwise
                  (when cmd (dispatch-command session cmd nil))))))
           (setf *dirty* t)
           (values nil #'%ground-input-state)))
        ;; 2-byte non-CSI: silently discard after prefix (no passthrough)
        ((and (= len 2) (/= (aref buf 1) +byte-csi-bracket+))
         (values nil #'%ground-input-state))
        ;; Buffer at capacity (>= 6 bytes but unrecognised) — discard and return
        ;; to ground to avoid permanent stuck-state on malformed CSI sequences.
        ((>= len 6)
         (values nil #'%ground-input-state))
        ;; Still accumulating (1-5 bytes so far)
        (t (values nil (%make-prefix-csi-k session buf)))))))

(define-cps-state %after-prefix-input-state (session byte)
  ;; ESC introduces a multi-byte prefix sequence (C-b arrow/modifier key sequences).
  ;; The buffer needs to be adjustable so %make-prefix-csi-k can vector-push-extend
  ;; up to 6 bytes for modifier sequences like ESC [ 1 ; 5 A (C-Up).
  ((= byte +byte-esc+)
   (let ((buf (make-array 8 :element-type '(unsigned-byte 8)
                            :fill-pointer 0 :adjustable t)))
     (vector-push-extend byte buf)
     (values nil (%make-prefix-csi-k session buf))))
  ;; Single-byte: dispatch the command table and return to ground.
  (t (values (dispatch-prefix-command session byte) #'%ground-input-state)))

(defconstant +byte-mouse-intro+ 77
  "ASCII 'M' (0x4D) — the final byte of X10 mouse sequences ESC [ M.")

;;; ── SGR mouse sequence parser ────────────────────────────────────────────────
;;;
;;; SGR format: ESC [ < Pb ; Px ; Py M (press) or m (release)
;;; Terminated by 'M' (press) or 'm' (release).

(defun %parse-sgr-mouse (buf len)
  "Parse an SGR mouse sequence from BUF (length LEN).
   Expected: ESC [ < Pb ; Px ; Py M|m
   Returns (values btn col row release-p) on success, or (values nil nil nil nil) on failure.
   Coordinates in BUF are 1-based; returned col/row are 0-based."
  ;; Minimum: ESC [ < D ; D ; D M = 9 bytes
  (when (and (>= len 9)
             (= (aref buf 0) 27)
             (= (aref buf 1) +byte-csi-bracket+)
             (= (aref buf 2) 60))  ; '<' = 60
    (let* ((s       (map 'string #'code-char (subseq buf 3 len)))
           (final   (char s (1- (length s))))
           (release-p (char= final #\m))
           (params  (subseq s 0 (1- (length s))))
           (parts   (loop for start = 0 then (1+ semi)
                          for semi  = (position #\; params :start start)
                          collect (subseq params start (or semi (length params)))
                          while semi)))
      (when (= (length parts) 3)
        (let ((btn (parse-integer (first  parts) :junk-allowed t))
              (col (parse-integer (second parts) :junk-allowed t))
              (row (parse-integer (third  parts) :junk-allowed t)))
          (when (and (integerp btn) (integerp col) (integerp row))
            ;; SGR coords are 1-based; convert to 0-based
            (values btn (1- col) (1- row) release-p))))))))

(defun %sgr-mouse-sequence-p (buf len)
  "True when BUF looks like the start of an SGR mouse sequence: ESC [ <."
  (and (>= len 3)
       (= (aref buf 0) 27)
       (= (aref buf 1) +byte-csi-bracket+)
       (= (aref buf 2) 60)))  ; '<' = 60

(defun %sgr-mouse-terminated-p (buf len)
  "True when BUF ends with 'M' (press) or 'm' (release) — SGR mouse final byte."
  (when (> len 3)
    (let ((last (aref buf (1- len))))
      (or (= last 77) (= last 109)))))   ; 'M'=77, 'm'=109

(defun make-escape-input-k (session buf)
  "CPS continuation: accumulate an ESC [... sequence one byte at a time.

   X10 mouse: ESC [ M <btn+32> <col+33> <row+33> — 6 bytes total.
     Detected when buf[0]=ESC buf[1]=[ buf[2]=M and we still need 3 more bytes.
     Dispatched via %DISPATCH-MOUSE-EVENT when len reaches 6.

   SGR mouse: ESC [ < Pb ; Px ; Py M|m — variable length, terminated by M or m.
     Detected when buf[2]='<' (60).  Accumulated until final byte M or m arrives.

   Copy-mode 3-byte CSI (ESC [ FINAL): try HANDLE-COPY-MODE-ESCAPE; if not
     handled and not in copy mode, forward to the active pane.

   2-byte non-CSI (ESC X): forward to the active pane.

   Otherwise: keep accumulating."
  ;; SESSION is captured from the make-escape-input-k call; the lambda parameter
  ;; _session is structurally required by the CPS protocol (SESSION BYTE) → values,
  ;; but is always the same object as the captured SESSION.  We ignore the parameter
  ;; to keep the protocol uniform across all CPS state functions.
  (lambda (_session byte)
    (declare (ignore _session))
    (vector-push-extend byte buf)
    (let ((len (fill-pointer buf)))
      (cond
        ;; ── X10 mouse: ESC [ M btn col row (6 bytes) ──────────────────────
        ((and (= len 6)
              (= (aref buf 1) +byte-csi-bracket+)
              (= (aref buf 2) +byte-mouse-intro+))
         (let* ((raw-btn (aref buf 3))
                (raw-col (aref buf 4))
                (raw-row (aref buf 5))
                ;; X10 encoding: btn+32, col/row+33 (1-based → subtract 1 for 0-based)
                (btn     (- raw-btn 32))
                (col     (- raw-col 33))
                (row     (- raw-row 33))
                (release-p (= raw-btn (+ +mouse-btn-release-x10+ 32))))  ; btn 3+32=35 = release in X10
           (%dispatch-mouse-event session btn col row release-p))
         (values nil #'%ground-input-state))
        ;; ── Still accumulating X10 mouse intro (ESC [ M + up to 2 more) ──
        ((and (>= len 3)
              (= (aref buf 1) +byte-csi-bracket+)
              (= (aref buf 2) +byte-mouse-intro+)
              (< len 6))
         (values nil (make-escape-input-k session buf)))
        ;; ── SGR mouse terminated: ESC [ < Pb ; Px ; Py M|m ───────────────
        ((and (%sgr-mouse-sequence-p buf len)
              (%sgr-mouse-terminated-p buf len))
         (multiple-value-bind (btn col row release-p)
             (%parse-sgr-mouse buf len)
           (when btn
             (%dispatch-mouse-event session btn col row release-p)))
         (values nil #'%ground-input-state))
        ;; ── SGR mouse still accumulating ──────────────────────────────────
        ((%sgr-mouse-sequence-p buf len)
         (values nil (make-escape-input-k session buf)))
        ;; ── Copy-mode or forward: 3-byte CSI ESC [ FINAL ──────────────────
        ((and (= len 3) (= (aref buf 1) +byte-csi-bracket+)
              (/= (aref buf 2) +byte-mouse-intro+)
              (/= (aref buf 2) 60))          ; not SGR '<'
         ;; Check whether this is the start of a 4-byte ESC [ N ~ sequence
         ;; (function key / PageUp / PageDown): parameter digit followed by '~'.
         ;; Digits 5 and 6 (53='5', 54='6') indicate PageUp/PageDown.
         ;; If so, keep accumulating; otherwise dispatch or forward.
         (let ((third (aref buf 2)))
           (if (and (>= third 48) (<= third 57))  ; '0'..'9' — possible N~ seq
               ;; Could be ESC [ N ~ (4-byte); keep accumulating
               (values nil (make-escape-input-k session buf))
               (progn
                 (unless (handle-copy-mode-escape session buf)
                   ;; Not in copy mode (or unrecognised): forward raw bytes to pane.
                   ;; When application cursor keys mode is active (DEC ?1h), remap
                   ;; ESC [ A/B/C/D (CSI) to ESC O A/B/C/D (SS3) before forwarding.
                   (unless (copy-mode-active-p session)
                     (let* ((sc       (%active-screen session))
                            (app-keys (and sc (screen-app-cursor-keys sc)))
                            (ss3-seq  (and app-keys (%arrow-final-to-ss3-bytes third))))
                       (if ss3-seq
                           (%forward-octets session ss3-seq)
                           (%forward-octets session (subseq buf 0 len))))))
                 (values nil #'%ground-input-state)))))
        ;; ── 4-byte function key: ESC [ N ~ ────────────────────────────────
        ;; PageUp = ESC [ 5 ~ (53 126), PageDown = ESC [ 6 ~ (54 126).
        ((and (= len 4) (= (aref buf 1) +byte-csi-bracket+)
              (= (aref buf 3) 126))           ; '~' = 126
         (let ((n (aref buf 2)))
           (cond
             ;; PageUp in copy mode
             ((and (= n 53) (copy-mode-active-p session))
              (let ((sc (%active-screen session)))
                (when sc
                  (copy-mode-scroll sc (screen-height sc))
                  (setf *dirty* t))))
             ;; PageDown in copy mode
             ((and (= n 54) (copy-mode-active-p session))
              (let ((sc (%active-screen session)))
                (when sc
                  (copy-mode-scroll sc (- (screen-height sc)))
                  (setf *dirty* t))))
             ;; Outside copy mode: forward raw bytes to pane
             (t
              (unless (copy-mode-active-p session)
                (%forward-octets session (subseq buf 0 len))))))
         (values nil #'%ground-input-state))
        ;; ── 4-byte accumulation: ESC [ N (not yet '~') — keep buffering ───
        ((and (= len 4) (= (aref buf 1) +byte-csi-bracket+)
              (/= (aref buf 3) 126))
         ;; Forward if no terminating ~ and not copy mode, return to ground
         (unless (copy-mode-active-p session)
           (%forward-octets session (subseq buf 0 len)))
         (values nil #'%ground-input-state))
        ;; ── 2-byte non-CSI sequence: ESC X ────────────────────────────────
        ;; In copy mode, a lone ESC (or ESC + non-CSI byte) exits copy mode.
        ;; Outside copy mode, forward the raw bytes to the pane.
        ((and (= len 2) (/= (aref buf 1) +byte-csi-bracket+))
         (if (copy-mode-active-p session)
             (let ((sc (%active-screen session)))
               (when sc (copy-mode-exit sc))
               (setf *dirty* t))
             (%forward-octets session (subseq buf 0 len)))
         (values nil #'%ground-input-state))
        ;; ── Buffer overflow guard (> 32 unrecognised bytes) ───────────────
        ((> len 32)
         (unless (copy-mode-active-p session)
           (%forward-octets session (subseq buf 0 len)))
         (values nil #'%ground-input-state))
        ;; ── Still accumulating ─────────────────────────────────────────────
        (t (values nil (make-escape-input-k session buf)))))))
