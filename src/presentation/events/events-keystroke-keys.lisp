(in-package #:cl-tmux)

;;;; Arrow-key fact table, modifier/extended-key helpers,
;;;;  CSI-u parsing, and %make-prefix-csi-k.

;;; ── Arrow-key fact table (Prolog-style) ──────────────────────────────────────
;;;
;;; One table encodes all three facets of an arrow key:
;;;   arrow_key(byte, "Name", :select-cmd).
;;; %prefix-csi-arrow-cmd and %arrow-final-name are projections of this single
;;; fact table, guaranteeing that adding or renaming an arrow key is a one-line
;;; change and the two functions stay in sync automatically.

(defmacro define-arrow-key-table (&rest specs)
  "Build %PREFIX-CSI-ARROW-CMD and %ARROW-FINAL-NAME from a unified fact table.
   Each SPEC is (final-byte-constant key-name pane-select-command)."
  `(progn
     (defun %prefix-csi-arrow-cmd (final-byte)
       "Map a CSI arrow FINAL-BYTE to a pane-select command keyword, or NIL."
       (cond ,@(mapcar (lambda (spec)
                         `((= final-byte ,(first spec)) ,(third spec)))
                       specs)
             (t nil)))
     (defun %arrow-final-name (final-byte)
       "Canonical tmux key name (\"Up\"/\"Down\"/\"Left\"/\"Right\") for FINAL-BYTE,
        or NIL when not an arrow.  Matches what %parse-key-token stores for
        `bind Up ...` directives — used as key-table lookup keys."
       (cond ,@(mapcar (lambda (spec)
                         `((= final-byte ,(first spec)) ,(second spec)))
                       specs)
             (t nil)))))

(define-arrow-key-table
  (+byte-arrow-up+    "Up"    :select-pane-up)
  (+byte-arrow-down+  "Down"  :select-pane-down)
  (+byte-arrow-right+ "Right" :select-pane-right)
  (+byte-arrow-left+  "Left"  :select-pane-left))

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
   an unhandled (control/out-of-range) codepoint.
   The graphic range is +byte-first-graphic+ (33='!') through +byte-last-graphic+
   (126='~'), excluding Space (32, handled above) and DEL (127 = +byte-del+, mapped
   to BSpace)."
  (case codepoint
    (9   "Tab")
    (13  "Enter")
    (27  "Escape")
    (32  "Space")
    (127 "BSpace")
    (t   (when (<= +byte-first-graphic+ codepoint +byte-last-graphic+)
           (string (code-char codepoint))))))

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
  (let* ((text      (map 'string #'code-char (subseq buffer 2 (1- length)))) ; drop ESC [ and u
         (semi      (position #\; text))
         (codepoint (if semi
                        (%parse-integer-or-nil text :end semi)
                        (%parse-integer-or-nil text)))
         (mod       (if semi
                        (or (%parse-integer-or-nil text :start (1+ semi)
                                                   :junk-allowed t)
                            1)
                        1)))
    (when codepoint (values codepoint mod))))

(defun %control-byte-key-name (byte)
  "Return a printable base key name for a Ctrl BYTE, or NIL.
   Ctrl bytes 1-26 (^A..^Z) map to lowercase a-z via byte+96.
   Ctrl bytes +byte-esc+ (27) through 31 (^[..^_) map to [..\_ via
   byte + +byte-ctrl-to-upper-offset+ (64).  The +64 offset recovers the symbol
   character: ESC (27) → '[' (91), FS (28) → '\\', GS (29) → ']', RS (30) → '^',
   US (31) → '_'."
  (cond
    ((<= 1 byte 26)
     (string (code-char (+ byte 96))))                          ; ^A..^Z → a..z
    ((<= +byte-esc+ byte 31)
     (string (code-char (+ byte +byte-ctrl-to-upper-offset+))))  ; ^[..^_ → [..\_
    (t nil)))

(defun %meta-key-name (byte)
  "Canonical tmux key name for the Meta/Alt chord that arrives as ESC then BYTE.
   \"M-a\", \"M-1\", \"M-/\", and \"M-Space\" (byte 32).  Returns NIL for control
   bytes and DEL (+byte-del+, 127), which are not standalone meta chords, so the
   caller forwards them unchanged.  The upper bound (< byte +byte-del+) makes the
   DEL exclusion self-documenting.  This is the exact inverse of the M-<char> encoding
   produced by send-keys (commands.lisp), keeping input decode and output encode
   symmetric."
  (cond
    ((= byte +byte-space+) "M-Space")
    ((and (> byte +byte-space+) (< byte +byte-del+))  ; 33..126 — printable graphic
     (concatenate 'string "M-" (string (code-char byte))))
    (t nil)))

(defun %single-byte-key-candidates (byte)
  "Lookup candidates for a single raw input BYTE in the active key table.
   The candidates are the raw character object, any named special key (Tab,
   Enter, BSpace), and the canonical C-<base> control name when BYTE is a
   control code."
  (remove nil
          (list (code-char byte)
                (case byte
                  (9 "Tab")
                  (13 "Enter")
                  (127 "BSpace")
                  (t nil))
                (let ((base (%control-byte-key-name byte)))
                  (and base (concatenate 'string "C-" base))))))

(defun %key-table-entry-by-candidates (table candidates)
  "Return the first key-table entry in TABLE that matches one of CANDIDATES."
  (loop for candidate in candidates
        for entry = (key-table-lookup table candidate)
        when entry
          return entry))

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

(defun %run-bound-string-key (session table key-string)
  "Run string KEY-STRING from TABLE and return its entry, or NIL if unbound."
  (let ((entry (and key-string (key-table-lookup table key-string))))
    (when entry
      (%run-key-table-binding session entry nil)
      (setf *dirty* t)
      entry)))

(defun %try-bound-string-key (session table key-string)
  "Look up the string KEY-STRING (e.g. \"C-Up\", \"M-Left\", \"Up\") in key
   TABLE.  When a binding exists, run it, mark the screen dirty, and return T;
   otherwise return NIL so the caller can fall back to its hardcoded default.
   This is the hook that lets `bind -T prefix C-Up <cmd>` and `bind -n M-Left
   <cmd>` override the built-in resize/select behaviour."
  (and (%run-bound-string-key session table key-string) t))

(defun %copy-mode-table-or-nil (session)
  "Return the active copy-mode table when COPY-MODE is enabled, otherwise NIL."
  (and (%copy-mode-active-p session)
       (%active-copy-mode-table)))

(defun %try-bound-string-key-in-order (session key-string &rest tables)
  "Try KEY-STRING against TABLES in order until one binding runs."
  (loop for table in tables
        when (and table (%try-bound-string-key session table key-string))
          return t))

(defun %try-bound-string-key-root-then-copy-mode (session key-string)
  "Try ROOT first, then the active copy-mode table when copy mode is enabled."
  (%try-bound-string-key-in-order session key-string
                                  +table-root+
                                  (%copy-mode-table-or-nil session)))

(defun %try-bound-string-key-copy-mode-then-root (session key-string)
  "Try the active copy-mode table first, then ROOT."
  (%try-bound-string-key-in-order session key-string
                                  (%copy-mode-table-or-nil session)
                                  +table-root+))

(defun %prefix-string-entry-result (entry)
  "Return the CPS outcome/state pair for a prefix string-key ENTRY."
  (if (and entry (key-table-repeatable-p entry))
      (values :repeatable #'%after-prefix-input-state)
      (values nil #'%ground-input-state)))

(defun %dispatch-modifier-arrow (session mod-byte final-byte)
  "Handle the modifier+arrow combination inside the 6-byte CSI sequence.
   MOD-BYTE is +byte-csi-mod-ctrl+ (Ctrl) or +byte-csi-mod-meta+ (Meta).
   FINAL-BYTE is the arrow final byte.

   A user binding for the canonical key name (e.g. `bind -T prefix C-Up <cmd>`)
   takes precedence; only when the prefix table has no such binding do we fall
   back to the built-in default — C-arrow resizes 1 cell, M-arrow resizes 5."
  ;; User override in the prefix table wins over the hardcoded default.
  (let ((key (%modifier-arrow-key-name mod-byte final-byte)))
    (or (%run-bound-string-key session +table-prefix+ key)
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
                 (when command (dispatch-command session command nil)))))))
        nil)))

(defun %csi-1-semi-prefix-p (buffer)
  "T when BUFFER[1..3] is the ESC [ 1 ; modifier-key prefix (requires length >= 4)."
  (and (= (aref buffer 1) +byte-csi-bracket+)
       (= (aref buffer 2) +byte-csi-param-1+)
       (= (aref buffer 3) +byte-csi-semi+)))

;;; ── %make-prefix-csi-k branch predicates ────────────────────────────────────
;;;
;;; Each predicate below recognises one shape of the accumulating ESC [ / ESC O
;;; buffer; the matching %handle-* function performs that shape's dispatch and
;;; returns the CPS (values OUTCOME NEXT-STATE) pair.  %make-prefix-csi-k itself
;;; stays a thin cond that pairs each predicate with its handler.

(defun %prefix-ss3-introducer-p (buffer length)
  "T for the 2-byte SS3 introducer ESC O, still awaiting its final byte."
  (and (= length 2) (= (aref buffer 1) +byte-ss3-o+)))

(defun %prefix-ss3-final-p (buffer length)
  "T for a complete 3-byte SS3 sequence ESC O <final>."
  (and (= length 3) (= (aref buffer 1) +byte-ss3-o+)))

(defun %prefix-tilde-key-p (buffer length)
  "T for a complete function/navigation key ESC [ <digits> ~ (F5, PageUp, ...)."
  (and (>= length 4) (= (aref buffer 1) +byte-csi-bracket+)
       (<= +byte-digit-0+ (aref buffer 2) +byte-digit-9+)
       (= (aref buffer (1- length)) +byte-tilde+)))

(defun %prefix-3byte-csi-p (buffer length)
  "T for a complete 3-byte CSI sequence ESC [ FINAL (arrow key or digit start)."
  (and (= length 3) (= (aref buffer 1) +byte-csi-bracket+)))

(defun %prefix-modifier-in-progress-p (buffer length)
  "T while accumulating ESC [ 1 ; [MOD] toward a 6-byte modifier sequence."
  (and (<= 4 length 5) (%csi-1-semi-prefix-p buffer)))

(defun %prefix-6byte-modifier-p (buffer length)
  "T for a complete 6-byte modifier CSI sequence ESC [ 1 ; MOD FINAL."
  (and (= length 6) (%csi-1-semi-prefix-p buffer)))

(defun %prefix-2byte-meta-p (buffer length)
  "T for a 2-byte non-CSI sequence ESC <key> — a prefix meta chord."
  (and (= length 2) (/= (aref buffer 1) +byte-csi-bracket+)))

;;; ── %make-prefix-csi-k branch handlers ──────────────────────────────────────

(defun %handle-ss3-introducer-after-prefix (session buffer)
  "Defer one more byte for the SS3 introducer ESC O so ESC O P/Q/R/S/H/F (F1-F4,
   Home/End) resolve as a unit before the 2-byte meta branch would claim ESC O."
  (values nil (%make-prefix-csi-k session buffer)))

(defun %handle-ss3-after-prefix (session buffer)
  "Resolve a complete SS3 sequence ESC O <final> against the prefix table."
  (let ((key (%ss3-key-name (aref buffer 2))))
    (%prefix-string-entry-result
     (and key (%run-bound-string-key session +table-prefix+ key)))))

(defun %handle-tilde-key-after-prefix (session buffer length)
  "Resolve a complete ESC [ <digits> ~ function/navigation key against the
   prefix table — F5 ESC[15~ ... F12, PageUp ESC[5~, Home ESC[1~, Delete
   ESC[3~ — so `bind F5 <cmd>` / `bind PPage <cmd>` work after the prefix."
  (let ((key (%csi-tilde-key buffer length)))
    (%prefix-string-entry-result
     (and key (%run-bound-string-key session +table-prefix+ key)))))

(defun %handle-3byte-csi-after-prefix (session buffer)
  "Resolve a complete 3-byte CSI sequence ESC [ FINAL: either defer (digit final
   begins a parameterised ESC [ 1 ; MOD FINAL or ESC [ N ~ sequence — was
   limited to '1', which dropped the '~' of ESC [ 5 ~ etc.) or dispatch the
   arrow key, letting a user `bind -T prefix Up <cmd>` override the built-in
   select-pane default."
  (let ((final-byte (aref buffer 2)))
    (if (<= +byte-digit-0+ final-byte +byte-digit-9+)
        (values nil (%make-prefix-csi-k session buffer))
        (let* ((name    (%arrow-final-name final-byte))
               (command (%prefix-csi-arrow-cmd final-byte))
               (entry   (%run-bound-string-key session +table-prefix+ name)))
          (unless entry
            ;; dispatch-command always returns NIL; the when's value is discarded.
            (when command (dispatch-command session command nil)))
          (%prefix-string-entry-result entry)))))

(defun %handle-modifier-in-progress-after-prefix (session buffer)
  "Keep accumulating ESC [ 1 ; [MOD] toward the final modifier letter."
  (values nil (%make-prefix-csi-k session buffer)))

(defun %handle-6byte-modifier-after-prefix (session buffer)
  "Resolve a complete 6-byte modifier CSI sequence ESC [ 1 ; MOD FINAL —
   C-arrow / M-arrow resize, or a user `bind -T prefix C-Up <cmd>` override."
  (let ((entry (%dispatch-modifier-arrow session (aref buffer 4) (aref buffer 5))))
    (setf *dirty* t)
    (%prefix-string-entry-result entry)))

(defun %handle-2byte-meta-after-prefix (session buffer)
  "Resolve a 2-byte non-CSI prefix meta chord (C-b then Alt+key → ESC <key>)
   against `bind M-<key>` in the prefix table; unbound chords are discarded
   (no passthrough after the prefix)."
  (%prefix-string-entry-result
   (%run-bound-string-key session +table-prefix+
                          (%meta-key-name (aref buffer 1)))))

(defun %handle-overflow-after-prefix ()
  "Buffer at capacity (>= 6 bytes) but unrecognised — discard and return to
   ground to avoid permanent stuck-state on malformed CSI sequences."
  (values nil #'%ground-input-state))

(defun %handle-still-accumulating-after-prefix (session buffer)
  "1-5 bytes accumulated so far and no shape matched yet — keep waiting."
  (values nil (%make-prefix-csi-k session buffer)))

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
        ((%prefix-ss3-introducer-p buffer length)
         (%handle-ss3-introducer-after-prefix session buffer))
        ((%prefix-ss3-final-p buffer length)
         (%handle-ss3-after-prefix session buffer))
        ((%prefix-tilde-key-p buffer length)
         (%handle-tilde-key-after-prefix session buffer length))
        ((%prefix-3byte-csi-p buffer length)
         (%handle-3byte-csi-after-prefix session buffer))
        ((%prefix-modifier-in-progress-p buffer length)
         (%handle-modifier-in-progress-after-prefix session buffer))
        ((%prefix-6byte-modifier-p buffer length)
         (%handle-6byte-modifier-after-prefix session buffer))
        ((%prefix-2byte-meta-p buffer length)
         (%handle-2byte-meta-after-prefix session buffer))
        ((>= length 6)
         (%handle-overflow-after-prefix))
        (t (%handle-still-accumulating-after-prefix session buffer))))))

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

;;; ── Root-table repeat mode (bind -n -r) ────────────────────────────────────
;;;
;;; tmux applies repeat mode to the root key table too, not just prefix: when a
;;; `bind -n -r` binding fires, server_client_key_callback sets CLIENT_REPEAT and
;;; keeps the root table active so the key can be pressed again without the prefix
;;; within repeat-time.  %run-root-table-binding arms that state; the repeat state
;;; re-looks-up the next byte in the root table, staying armed for another
;;; repeatable binding and otherwise falling through to normal ground processing.

(defun %run-root-table-binding (session byte)
  "Run the root-table binding matching BYTE (the caller has already confirmed an
   entry exists) and return the CPS (values OUTCOME NEXT-STATE) pair.  A -r
   (repeatable) root binding arms repeat mode: it returns :REPEATABLE plus a
   root-scoped repeat state so the key repeats without the prefix within
   repeat-time, mirroring tmux's CLIENT_REPEAT on the root table."
  (let ((entry (%key-table-entry-by-candidates
                +table-root+ (%single-byte-key-candidates byte))))
    (%run-key-table-binding session entry byte)
    (setf *dirty* t)
    (if (key-table-repeatable-p entry)
        (values :repeatable #'%after-root-repeat-input-state)
        (values nil #'%ground-input-state))))

(define-cps-state %after-root-repeat-input-state (session byte)
  ;; Repeat mode armed by a root (-n -r) binding.  Re-look-up the next byte in the
  ;; root table: another repeatable binding keeps repeat mode armed (and re-stamps
  ;; the timer via the :REPEATABLE outcome), while any other byte exits repeat mode
  ;; and is reprocessed as a normal ground keystroke (root lookup, prefix, escape,
  ;; or pane forward).  Non-repeatable root bindings therefore break the sequence,
  ;; matching tmux server_client_key_callback's CLIENT_REPEAT handling.
  ((%key-table-entry-by-candidates +table-root+ (%single-byte-key-candidates byte))
   (%run-root-table-binding session byte))
  (t
   (%ground-input-state session byte)))
