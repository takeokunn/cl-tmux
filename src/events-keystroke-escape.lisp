(in-package #:cl-tmux)

;;; ── SGR mouse sequence parser ────────────────────────────────────────────────
;;;
;;; SGR format: ESC [ < Pb ; Px ; Py M (press) or m (release)
;;; Terminated by 'M' (press) or 'm' (release).
;;;
;;; ASCII 'M' = 77.  It serves as both the X10 mouse-sequence intro final byte
;;; and the SGR press final byte.  +byte-ascii-m+ and +byte-sgr-release+ are
;;; defined canonically in events-core.lisp and reused here.

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
      (or (= last-byte +byte-ascii-m+)
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

;;; ── CPS return helpers ───────────────────────────────────────────────────────

(defun %ground-values ()
  "The standard CPS return for 'sequence consumed, reset to ground state':
   (values NIL #'%ground-input-state).  Named so call sites are self-documenting."
  (values nil #'%ground-input-state))

;;; ── make-escape-input-k: sub-state decoder helpers ──────────────────────────
;;;
;;; The escape accumulator is decomposed into named helper functions so each
;;; protocol cohort (X10 mouse, SGR mouse, copy-mode CSI, function-key) is
;;; independently readable and testable.

(defun %handle-escape-x10-mouse (session buffer)
  "Decode a complete 6-byte X10 mouse sequence from BUFFER and dispatch it.
   Returns (%ground-values) always."
  (let* ((raw-btn   (aref buffer 3))
         (raw-col   (aref buffer 4))
         (raw-row   (aref buffer 5))
         ;; X10 encoding: btn+32, col/row+33 (1-based → subtract 1 for 0-based)
         (btn       (- raw-btn 32))
         (col       (- raw-col 33))
         (row       (- raw-row 33))
         (release-p (= raw-btn (+ +mouse-btn-release-x10+ 32))))  ; btn 3+32=35 = release in X10
    (%dispatch-mouse-event session btn col row release-p))
  (%ground-values))

(defun %handle-escape-sgr-mouse (session buffer length)
  "Dispatch a completed SGR mouse sequence from BUFFER (LENGTH bytes).
   Returns (%ground-values) always."
  (multiple-value-bind (btn col row release-p)
      (%parse-sgr-mouse buffer length)
    (when btn
      (%dispatch-mouse-event session btn col row release-p)))
  (%ground-values))

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
                       if (<= +byte-digit-0+ byte +byte-digit-9+)
                         do (setf value (+ (* value 10) (- byte +byte-digit-0+)))
                       else return nil
                       finally (return value))))))
      (if semi
          (let ((param (digits 2 semi))
                (mod   (digits (1+ semi) (1- length))))
            (and param mod (values param mod)))
          (let ((param (digits 2 (1- length))))
            (when param (values param 1)))))))

(defun %csi-tilde-key (buffer length)
  "Canonical key name for an ESC [ <param> [;<mod>] ~ sequence, or NIL.  Joins the
   base navigation/function key (%csi-tilde-key-name) with any modifier prefix
   (%modifier-prefix): ESC[15~ → \"F5\", ESC[15;5~ → \"C-F5\", ESC[1;2~ → \"S-Home\"."
  (multiple-value-bind (param mod) (%csi-tilde-parse buffer length)
    (let ((base (and param (%csi-tilde-key-name param))))
      (when base (concatenate 'string (%modifier-prefix (or mod 1)) base)))))

;;; ── Key-name fact tables (Prolog-style) ─────────────────────────────────────
;;;
;;; define-key-lookup-table generates a (param → key-name) lookup function from a
;;; flat fact table.  Integer literals dispatch via EQL; character literals are
;;; normalised to their char-code at macro-expansion time so the generated COND
;;; stays homogeneous (integer comparisons throughout, no runtime CHAR= overhead).
;;; Used for both the CSI-tilde (numeric param) and SS3 (final character) tables.

(defmacro define-key-lookup-table (fn-name param-var doc &rest specs)
  "Generate a key-lookup function FN-NAME(PARAM-VAR) → key-name-string | nil.
   Integer specs dispatch via EQL; character specs use CHAR-CODE so the generated
   COND stays homogeneous (integer comparisons throughout)."
  `(defun ,fn-name (,param-var)
     ,doc
     (cond ,@(mapcar (lambda (spec)
                       `((eql ,param-var ,(let ((k (first spec)))
                                            (if (characterp k) (char-code k) k)))
                         ,(second spec)))
                     specs)
           (t nil))))

(define-key-lookup-table %csi-tilde-key-name param
  "Map the numeric PARAM of an ESC [ <param> ~ sequence to its canonical tmux key
   name, or NIL when PARAM is not a recognised navigation/function key.  Covers
   Home/End/Insert/Delete, PageUp/PageDown, and the vt-style F1-F12 finals."
  (1 "Home") (7 "Home")
  (2 "Insert")
  (3 "Delete")
  (4 "End") (8 "End")
  (5 "PageUp") (6 "PageDown")
  (11 "F1") (12 "F2") (13 "F3") (14 "F4")
  (15 "F5") (17 "F6") (18 "F7") (19 "F8")
  (20 "F9") (21 "F10") (23 "F11") (24 "F12"))

(defun %handle-escape-csi-tilde (session buffer length)
  "Handle a complete ESC [ <param> ~ sequence at root (LENGTH bytes).  A root-table
   binding for the canonical key name wins first, so `bind -n F5 <cmd>`,
   `bind -n PageUp <cmd>`, `bind -n Home <cmd>` … fire.  Failing that the legacy
   behaviour is preserved: PageUp/PageDown scroll in copy mode, and any other or
   unbound key is forwarded raw so the pane's application still receives it.
   Returns (%ground-values)."
  (let ((key (%csi-tilde-key buffer length)))
    (cond
      ((and key (%try-bound-string-key session +table-root+ key)))
      ;; Unmodified PageUp/Down in copy mode: scroll one screenful.
      ;; key="PageUp"→positive delta, "PageDown"→negative.  Modified variants
      ;; (key="C-PageUp" etc.) fall through to the raw-forward branch below.
      ((and (member key '("PageUp" "PageDown") :test #'string=)
            (%copy-mode-active-p session))
       (let ((screen (%active-screen session)))
         (when screen
           (copy-mode-scroll screen (if (string= key "PageUp")
                                        (screen-height screen)
                                        (- (screen-height screen))))
           (setf *dirty* t))))
      (t
       (%forward-unless-copy-mode session buffer length))))
  (%ground-values))

(define-key-lookup-table %ss3-key-name final-byte
  "Map the final byte of an SS3 sequence ESC O <final> to its canonical tmux key
   name, or NIL when it is not a recognised bindable key.  Covers F1-F4
   (ESC O P/Q/R/S, the xterm/screen encoding not carried by the ESC[N~ path)
   and Home/End (ESC O H/F)."
  (#\P "F1") (#\Q "F2") (#\R "F3") (#\S "F4")
  (#\H "Home") (#\F "End"))

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
      (%forward-unless-copy-mode session buffer 3)))
  (%ground-values))

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
          (%ground-values)))))

(defun %forward-unless-copy-mode (session buffer length)
  "Forward BUFFER[0..LENGTH) to the active pane unless copy mode is active."
  (unless (%copy-mode-active-p session)
    (%forward-octets-synchronized session (subseq buffer 0 length))))

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
      (flet ((accumulate () (values nil (make-escape-input-k session buffer))))
      (cond
        ;; ── SS3 introducer: ESC O — defer one byte to disambiguate ───────
        ;; ESC O P/Q/R/S (F1-F4) and ESC O H/F (Home/End) vs Alt+O.  Keep
        ;; accumulating; if no third byte arrives, escape-time flushes the
        ;; buffered ESC O to the pane (Alt+O passthrough preserved).
        ((and (= length 2) (= (aref buffer 1) +byte-ss3-o+))
         (accumulate))
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
         (accumulate))
        ;; ── SGR mouse terminated: ESC [ < Pb ; Px ; Py M|m ───────────────
        ((and (%sgr-mouse-sequence-p buffer length)
              (%sgr-mouse-terminated-p buffer length))
         (%handle-escape-sgr-mouse session buffer length))
        ;; ── SGR mouse still accumulating ──────────────────────────────────
        ((%sgr-mouse-sequence-p buffer length)
         (accumulate))
        ;; ── CSI-u extended-keys complete: ESC [ <codepoint> ; <mod> u ─────
        ;; Placed before the 3-byte CSI / function-key / modifier-arrow branches
        ;; so a digit-leading u-terminated chord is decoded here rather than being
        ;; misread as an arrow/function key.  %handle-escape-csi-u resolves a
        ;; binding by name, else re-injects the legacy byte form for transparency.
        ((%csi-u-terminated-p buffer length)
         (%handle-escape-csi-u session buffer length)
         (%ground-values))
        ;; ── CSI-u still accumulating: ESC [ <digits/;> (no terminator yet) ─
        ;; Defers the digit-leading CSI so multi-digit codepoints (ESC [ 9 7 …)
        ;; are not eaten by the generic 3-/4-byte forwards below.  Real
        ;; modifier-arrows (ESC [ 1 ; N FINAL) pass through here too and resolve
        ;; at the modifier-arrow-complete branch once the final letter arrives.
        ((%csi-u-accumulating-p buffer length)
         (accumulate))
        ;; ── Focus in/out: ESC [ I (gained) / ESC [ O (lost) from the outer ─
        ;; terminal's ?1004 reporting.  Deliver the focus change to the active
        ;; pane (its app gets ESC[I/ESC[O when it enabled ?1004); never forward
        ;; the raw bytes.  Placed before the generic 3-byte CSI handling.
        ((and (= length 3) (= (aref buffer 1) +byte-csi-bracket+)
              (or (= (aref buffer 2) +byte-focus-in+)
                  (= (aref buffer 2) +byte-focus-out+)))
         (%notify-pane-focus (session-active-pane session)
                             (= (aref buffer 2) +byte-focus-in+))
         (%ground-values))
        ;; ── 3-byte CSI: ESC [ FINAL — not X10 and not SGR ────────────────
        ((and (= length 3) (= (aref buffer 1) +byte-csi-bracket+)
              (/= (aref buffer 2) +byte-ascii-m+)
              (/= (aref buffer 2) +byte-sgr-lt+))
         (multiple-value-bind (keep-accumulating next-state)
             (%handle-escape-csi-3byte session buffer)
           (if keep-accumulating
               (accumulate)
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
         (accumulate))
        ((and (= length 6)
              (= (aref buffer 1) +byte-csi-bracket+)
              (= (aref buffer 2) +byte-csi-param-1+)
              (= (aref buffer 3) +byte-csi-semi+))
         (let ((key (%modifier-arrow-key-name (aref buffer 4) (aref buffer 5))))
           (unless (%try-bound-string-key session +table-root+ key)
             (%forward-unless-copy-mode session buffer length)))
         (%ground-values))
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
         (%forward-unless-copy-mode session buffer length)
         (%ground-values))
        ;; ── 4-byte accumulation: ESC [ N (not yet '~') — keep buffering ───
        ((and (= length 4) (= (aref buffer 1) +byte-csi-bracket+)
              (/= (aref buffer 3) +byte-tilde+))
         (%forward-unless-copy-mode session buffer length)
         (%ground-values))
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
                   (entry     (when meta-name
                                (or (key-table-lookup "copy-mode-vi" meta-name)
                                    (key-table-lookup "copy-mode"    meta-name)))))
              (if entry
                  (%run-key-table-binding session entry nil)
                  ;; No table binding: ESC clears the active selection.
                  (let ((screen (%active-screen session)))
                    (when screen (copy-mode-clear-selection screen))))
              (setf *dirty* t)))
           ((%try-bound-string-key session +table-root+
                                   (%meta-key-name (aref buffer 1))))
           (t
            (%forward-octets-synchronized session (subseq buffer 0 length))))
         (%ground-values))
        ;; ── Buffer overflow guard (> 32 unrecognised bytes) ───────────────
        ((> length 32)
         (%forward-unless-copy-mode session buffer length)
         (%ground-values))
        ;; ── Still accumulating ─────────────────────────────────────────────
        (t (accumulate)))))))
