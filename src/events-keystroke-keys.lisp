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
  (let* ((text      (map 'string #'code-char (subseq buffer 2 (1- length)))) ; drop ESC [ and u
         (semi      (position #\; text))
         (codepoint (if semi
                        (ignore-errors (parse-integer text :end semi))
                        (ignore-errors (parse-integer text))))
         (mod       (or (and semi
                             (ignore-errors (parse-integer text :start (1+ semi)
                                                               :junk-allowed t)))
                        1)))
    (when codepoint (values codepoint mod))))

(defun %control-byte-key-name (byte)
  "Return a printable base key name for a Ctrl BYTE, or NIL."
  (cond
    ((<= 1 byte 26) (string (code-char (+ byte 96)))) ; ^A..^Z -> a..z
    ((<= 27 byte 31) (string (code-char (+ byte 64)))) ; ^[..\^_ -> [..\_
    (t nil)))

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
           (%prefix-string-entry-result
            (and key (%run-bound-string-key session +table-prefix+ key)))))
        ;; ── Function / navigation key after prefix: ESC [ <digits> ~ ─────
        ;; F5 ESC[15~ … F12, PageUp ESC[5~, Home ESC[1~, Delete ESC[3~, so
        ;; `bind F5 <cmd>` / `bind PPage <cmd>` resolve in the prefix table.
        ;; The tilde terminator keeps this disjoint from the ESC[1;MOD arrow
        ;; branches below (those end in a letter).
        ((and (>= length 4) (= (aref buffer 1) +byte-csi-bracket+)
              (<= +byte-digit-0+ (aref buffer 2) +byte-digit-9+)
              (= (aref buffer (1- length)) +byte-tilde+))
         (let ((key (%csi-tilde-key buffer length)))
           (%prefix-string-entry-result
            (and key (%run-bound-string-key session +table-prefix+ key)))))
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
                    (command (%prefix-csi-arrow-cmd final-byte))
                    (entry   nil))
                (setf entry (%run-bound-string-key session +table-prefix+ name))
                (unless entry
                  ;; dispatch-command always returns NIL; the when's value is discarded.
                  (when command (dispatch-command session command nil)))
                (%prefix-string-entry-result entry))))))
        ;; 4-5 byte: ESC [ 1 ; [MOD] — keep accumulating for the final letter
        ((and (<= 4 length 5) (%csi-1-semi-prefix-p buffer))
         (values nil (%make-prefix-csi-k session buffer)))
        ;; Complete 6-byte modifier CSI: ESC [ 1 ; MOD FINAL
        ((and (= length 6) (%csi-1-semi-prefix-p buffer))
         (let ((entry (%dispatch-modifier-arrow session (aref buffer 4) (aref buffer 5))))
           (setf *dirty* t)
           (%prefix-string-entry-result entry)))
        ;; 2-byte non-CSI: a prefix meta chord (C-b then Alt+key → ESC <key>).
        ;; Look up `bind M-<key>` in the prefix table; if unbound, discard as
        ;; before (no passthrough after the prefix).
        ((and (= length 2) (/= (aref buffer 1) +byte-csi-bracket+))
         (%prefix-string-entry-result
          (%run-bound-string-key session +table-prefix+
                                 (%meta-key-name (aref buffer 1)))))
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
