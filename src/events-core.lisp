(in-package #:cl-tmux)

;;;; Event processing — core macros, mouse dispatch, overlay handler.

;;; ── Prompt key handler ──────────────────────────────────────────────────────
;;;
;;; define-prompt-key-rules generates HANDLE-PROMPT-KEY from a declarative
;;; byte-dispatch table, matching the Prolog-like rule style used throughout
;;; the codebase (define-command-handlers, define-csi-rules, define-state).

;;; UTF-8 accumulation state for the prompt (module-level; main-thread-only).
(defvar *prompt-utf8-acc* 0
  "Accumulated code-point bits from UTF-8 lead byte processing.")
(defvar *prompt-utf8-left* 0
  "Number of UTF-8 continuation bytes still expected (0 when idle).")

(defmacro define-prompt-key-rules (&rest rules)
  "Build HANDLE-PROMPT-KEY from a byte-dispatch table.
   Each RULE is (PATTERN &rest BODY) where PATTERN is:
     integer  → exact byte match
     list     → verbatim condition
     t        → default clause
   Always marks *dirty* after dispatching."
  `(defun handle-prompt-key (byte)
     "Route one input BYTE to the active prompt.
      UTF-8 multi-byte sequences are decoded via *prompt-utf8-acc* /
      *prompt-utf8-left* before the dispatch table is consulted."
     (cond
       ,@(mapcar
          (lambda (rule)
            (destructuring-bind (pattern &rest body) rule
              `(,(cond
                   ((eq pattern 't)    't)
                   ((integerp pattern) `(= byte ,pattern))
                   (t                   pattern))
                ,@body)))
          rules))
     (setf *dirty* t)))

;;; ── Vi-mode prompt key dispatch ──────────────────────────────────────────────
;;;
;;; When status-keys = "vi" and the prompt is in vi normal mode,
;;; single-byte commands navigate / edit rather than inserting text.
;;; Returns T when the byte was consumed by vi-normal dispatch.

(defun %handle-vi-normal-key (byte)
  "Dispatch BYTE in vi normal mode.  Returns T when the key was handled.
   Navigation: h (left), l (right), 0/^ (BOL), $ (EOL), w (word-forward),
               b (word-backward).
   Editing:    x (delete char), D (delete to end), dd/d$ (delete to end).
   Mode switch: a/i/A/I return to insert mode; : stays in normal (no-op).
   Enter:      submit (same as insert mode)."
  (let ((p *prompt*))
    (when (and p (prompt-vi-normal-p p))
      (case byte
        ;; Navigation
        (104 (prompt-cursor-back)    t)        ; h — left
        (108 (prompt-cursor-forward) t)        ; l — right
        (48  (prompt-cursor-bol)     t)        ; 0 — beginning of line
        (94  (prompt-cursor-bol)     t)        ; ^ — beginning of line
        (36  (prompt-cursor-eol)     t)        ; $ — end of line
        (119 (prompt-cursor-forward) t)        ; w — word forward (approx: move right)
        (98  (prompt-cursor-back)    t)        ; b — word backward (approx: move left)
        ;; Editing
        (120 (prompt-delete-char)    t)        ; x — delete char under cursor
        (68  (prompt-kill-to-end)    t)        ; D — delete to end of line
        ;; Enter to insert mode
        (97                                    ; a — append (move right, enter insert)
         (prompt-cursor-forward)
         (setf (prompt-vi-normal-p p) nil)
         t)
        (65                                    ; A — append at end
         (prompt-cursor-eol)
         (setf (prompt-vi-normal-p p) nil)
         t)
        (105                                   ; i — insert mode
         (setf (prompt-vi-normal-p p) nil)
         t)
        (73                                    ; I — insert at beginning
         (prompt-cursor-bol)
         (setf (prompt-vi-normal-p p) nil)
         t)
        (13                                    ; Enter — submit
         (let ((active-prompt p))
           (when (prompt-on-submit active-prompt)
             (funcall (prompt-on-submit active-prompt) (prompt-buffer active-prompt)))
           (prompt-clear))
         t)
        (27  (prompt-clear) t)                 ; ESC in normal mode — cancel
        (3   (prompt-clear) t)                 ; C-c — cancel
        (otherwise nil)))))                    ; unhandled — fall through to insert

(define-prompt-key-rules
  (13                                       ; Enter — submit and dismiss
   (setf *prompt-utf8-acc* 0 *prompt-utf8-left* 0)
   (let ((active-prompt *prompt*))
     (when (and active-prompt (prompt-on-submit active-prompt))
       (funcall (prompt-on-submit active-prompt) (prompt-buffer active-prompt)))
     (prompt-clear)))
  (27                                       ; Esc
   (setf *prompt-utf8-acc* 0 *prompt-utf8-left* 0)
   (let ((p *prompt*))
     (cond
       ;; vi mode: ESC enters normal mode (does NOT cancel the prompt).
       ((and p (string-equal (cl-tmux/options:get-option "status-keys" "emacs") "vi")
             (not (prompt-vi-normal-p p)))
        (setf (prompt-vi-normal-p p) t))
       ;; emacs mode or already in vi-normal: cancel.
       (t (prompt-clear)))))
  (3   (setf *prompt-utf8-acc* 0 *prompt-utf8-left* 0) (prompt-clear)) ; C-c — cancel
  (1   (prompt-cursor-bol))                 ; C-a — beginning of line
  (5   (prompt-cursor-eol))                 ; C-e — end of line
  (2   (prompt-cursor-back))                ; C-b — cursor left
  (6   (prompt-cursor-forward))             ; C-f — cursor right
  (11  (prompt-kill-to-end))                ; C-k — kill to end
  (21  (prompt-kill-to-start))              ; C-u — kill to start
  (23  (prompt-kill-word-back))             ; C-w — kill previous word
  ((or (= byte 127) (= byte 8))
   (prompt-backspace))                      ; Backspace / DEL
  ;; Single-key prompt (confirm-before, command-prompt -1): the first printable
  ;; key IS the answer — submit it immediately as a 1-char string, no Enter.
  ;; Control keys (Esc, C-c) are < 32 and fall through to their cancel handlers.
  ((and *prompt* (prompt-single-key *prompt*) (>= byte 32) (< byte 127))
   (setf *prompt-utf8-acc* 0 *prompt-utf8-left* 0)
   (let ((active-prompt *prompt*)
         (ch            (code-char byte)))
     (when (prompt-on-submit active-prompt)
       (funcall (prompt-on-submit active-prompt) (string ch)))
     (prompt-clear)))
  ;; Vi normal mode: intercept printable bytes before insert dispatch.
  ((%handle-vi-normal-key byte) nil)        ; consumed by vi-normal — already handled
  ((and (>= byte 32) (< byte 127))
   (prompt-input (code-char byte)))         ; printable ASCII — insert
  ;; UTF-8 continuation byte: fold into accumulator
  ((= (logand byte #xC0) #x80)
   (when (plusp *prompt-utf8-left*)
     (setf *prompt-utf8-acc*  (logior (ash *prompt-utf8-acc* 6)
                                       (logand byte #x3F)))
     (decf *prompt-utf8-left*)
     (when (zerop *prompt-utf8-left*)
       (let ((code-point *prompt-utf8-acc*))
         (setf *prompt-utf8-acc* 0)
         (let ((character (ignore-errors (code-char code-point))))
           (when character (prompt-input character)))))))
  ;; UTF-8 lead byte: begin multi-byte decode
  ((and (>= byte #xC0) (/= byte #xFF))
   (multiple-value-bind (accumulator bytes-left)
       (cond ((< byte #xE0) (values (logand byte #x1F) 1))
             ((< byte #xF0) (values (logand byte #x0F) 2))
             (t             (values (logand byte #x07) 3)))
     (setf *prompt-utf8-acc*  accumulator
           *prompt-utf8-left* bytes-left)))
  (t nil))                                  ; other control bytes — ignore

;;; ── VT100 escape-sequence byte constants ───────────────────────────────────
(defconstant +byte-esc+         27  "ASCII ESC (0x1B)")
(defconstant +byte-csi-bracket+ 91  "CSI introducer '[' (0x5B)")
(defconstant +byte-arrow-up+    65  "CUU final byte 'A' (0x41)")
(defconstant +byte-arrow-down+  66  "CUD final byte 'B' (0x42)")
(defconstant +byte-arrow-left+  68  "CUB final byte 'D'")
(defconstant +byte-arrow-right+ 67  "CUF final byte 'C'")
(defconstant +byte-q+          113  "Lowercase 'q' for copy-mode exit (0x71)")
(defconstant +byte-j+          106  "Lowercase 'j' — vi down / overlay scroll-down (0x6A)")
(defconstant +byte-k+          107  "Lowercase 'k' — vi up / overlay scroll-up (0x6B)")

;;; ── Additional copy-mode navigation byte constants ──────────────────────────
;;; Named for every raw byte used in the copy-mode dispatch table so that readers
;;; can audit the mapping without cross-referencing ASCII tables.
(defconstant +byte-h+          104  "Lowercase 'h' — vi left (0x68)")
(defconstant +byte-l+          108  "Lowercase 'l' — vi right (0x6C)")
(defconstant +byte-w+          119  "Lowercase 'w' — vi word-forward (0x77)")
(defconstant +byte-b+           98  "Lowercase 'b' — vi word-backward (0x62)")
(defconstant +byte-e+          101  "Lowercase 'e' — vi word-end (0x65)")
(defconstant +byte-f+          102  "Lowercase 'f' — vi jump-forward-to-char (0x66)")
(defconstant +byte-g+          103  "Lowercase 'g' — vi jump-to-top (0x67)")
(defconstant +byte-i+          105  "Lowercase 'i' — exit copy mode (insert, 0x69)")
(defconstant +byte-n+          110  "Lowercase 'n' — search next (0x6E)")
(defconstant +byte-r+          114  "Lowercase 'r' — rectangle select toggle (0x72)")
(defconstant +byte-t+          116  "Lowercase 't' — vi jump-to-before-char (0x74)")
(defconstant +byte-v+          118  "Lowercase 'v' — begin selection (0x76)")
(defconstant +byte-y+          121  "Lowercase 'y' — yank/copy (0x79)")
(defconstant +byte-capital-a+   65  "Uppercase 'A' — append selection (0x41)")
(defconstant +byte-capital-d+   68  "Uppercase 'D' — copy to end of line (0x44)")
(defconstant +byte-capital-f+   70  "Uppercase 'F' — vi jump-backward-to-char (0x46)")
(defconstant +byte-capital-g+   71  "Uppercase 'G' — jump to bottom (0x47)")
(defconstant +byte-capital-h+   72  "Uppercase 'H' — cursor to top of screen (0x48)")
(defconstant +byte-capital-l+   76  "Uppercase 'L' — cursor to bottom of screen (0x4C)")
(defconstant +byte-capital-m+   77  "Uppercase 'M' — cursor to middle of screen (0x4D)")
(defconstant +byte-capital-n+   78  "Uppercase 'N' — search prev (0x4E)")
(defconstant +byte-capital-t+   84  "Uppercase 'T' — vi jump-to-after-prev-char (0x54)")
(defconstant +byte-capital-v+   86  "Uppercase 'V' — begin line selection (0x56)")
(defconstant +byte-capital-y+   89  "Uppercase 'Y' — copy current line (0x59)")
(defconstant +byte-dollar+      36  "Dollar sign '$' — go to line end (0x24)")
(defconstant +byte-slash+       47  "Slash '/' — search forward (0x2F)")
(defconstant +byte-question+    63  "Question mark '?' — search backward (0x3F)")
(defconstant +byte-space+       32  "Space ' ' — begin selection (0x20)")

;;; ── CSI modifier-sequence byte constants ────────────────────────────────────
;;; These appear in ESC [ 1 ; MOD FINAL sequences generated by terminal emulators
;;; for Ctrl-arrow and Meta-arrow key combinations.
(defconstant +byte-csi-param-1+  49  "CSI intermediate parameter '1' (0x31)")
(defconstant +byte-csi-semi+     59  "CSI parameter separator ';' (0x3B)")
(defconstant +byte-csi-mod-ctrl+ 53  "CSI modifier '5' — Ctrl key (0x35)")
(defconstant +byte-csi-mod-meta+ 51  "CSI modifier '3' — Meta/Alt key (0x33)")
(defconstant +byte-tilde+       126  "VT function-key terminator '~' (0x7E)")
(defconstant +byte-sgr-lt+       60  "SGR mouse introducer '<' (0x3C)")
(defconstant +byte-digit-0+      48  "ASCII digit '0' (0x30)")
(defconstant +byte-digit-9+      57  "ASCII digit '9' (0x39)")

;;; ── Function-key parameter constants ────────────────────────────────────────
;;; These are the numeric parameters in ESC [ N ~ sequences.
;;; NOTE: +byte-page-up-param+ (53) has the same numeric value as
;;; +byte-csi-mod-ctrl+ (53) — they are semantically distinct: the former is the
;;; literal digit '5' in ESC [ 5 ~, the latter is the modifier byte in ESC [ 1 ; 5 F.
;;; Both constants are kept to make code at their respective call sites self-documenting.
(defconstant +byte-page-up-param+   53  "ESC [ 5 ~ PageUp parameter byte '5' (0x35)")
(defconstant +byte-page-down-param+ 54  "ESC [ 6 ~ PageDown parameter byte '6' (0x36)")

;;; ── SS3 introducer ──────────────────────────────────────────────────────────
;;; ESC O <final> is the SS3 form xterm/screen/tmux terminals use for F1-F4
;;; (ESC O P/Q/R/S) and Home/End (ESC O H/F).  It collides with Alt+O (a 2-byte
;;; meta chord ESC O), so the decoder defers one byte after ESC O and lets
;;; escape-time disambiguate — exactly as physical terminals do.
(defconstant +byte-ss3-o+ 79 "ESC O SS3 introducer, ASCII 'O' (0x4F).")

;;; Holds the in-progress escape-accumulation buffer (the same object the
;;; make-escape-input-k closure is growing) so %flush-esc-if-timed-out can replay
;;; the FULL partial sequence on an escape-time timeout instead of dropping every
;;; byte after the leading ESC.  NIL when no escape sequence is being accumulated;
;;; in that state the flush falls back to forwarding a lone ESC (vim insert-mode).
(defvar *esc-accum-buffer* nil
  "Octet vector currently accumulating after an ESC, or NIL at ground.")

;;; ── Mouse button-number constants ───────────────────────────────────────────
;;; These are X10-encoded button numbers (raw byte minus 32).
(defconstant +mouse-btn-left+          0  "Left mouse button press (X10 btn 0).")
(defconstant +mouse-btn-middle+        1  "Middle mouse button press (X10 btn 1) — paste.")
(defconstant +mouse-btn-release-x10+   3  "X10 release marker (btn 3+32=35).")
(defconstant +mouse-btn-motion+       32  "Button-1 drag/motion (X10 btn 32).")
(defconstant +mouse-btn-scroll-up+    64  "Scroll-wheel up (X10 btn 64).")
(defconstant +mouse-btn-scroll-down+  65  "Scroll-wheel down (X10 btn 65).")

;;; ── SGR mouse final-byte constants ──────────────────────────────────────────
;;; ASCII 'M' (77) is used as both the X10 mouse-sequence intro final byte and the
;;; SGR press final byte.  A single constant covers both roles; the old duplicate
;;; +byte-sgr-press+ has been removed.
(defconstant +byte-ascii-m+     77  "ASCII 'M' (0x4D) — X10 mouse intro and SGR press final.")
(defconstant +byte-sgr-release+ 109 "ASCII 'm' (0x6D) — SGR mouse release final byte.")
(defconstant +byte-ascii-u+    117 "ASCII 'u' (0x75) — CSI-u (fixterms extended-keys) final byte.")
(defconstant +byte-focus-in+    73 "ASCII 'I' (0x49) — ESC [ I focus-gained report (?1004).")
(defconstant +byte-focus-out+   79 "ASCII 'O' (0x4F) — ESC [ O focus-lost report (?1004).")

;;; ── Escape sequence dispatch macro ─────────────────────────────────────────

(defmacro define-copy-mode-escape-table (&rest rules)
  "Build HANDLE-COPY-MODE-ESCAPE from a declarative table.
   Each RULE is (byte-list &body forms).
   byte-list is a list of constant symbols or integers specifying the sequence.
   The generated function returns T when a sequence is consumed, NIL otherwise."
  `(defun handle-copy-mode-escape (session bytes)
     "Match BYTES against the copy-mode escape table; dispatch and mark dirty.
      Returns T if consumed, NIL otherwise."
     (let ((screen (%active-screen session)))
       (when (and screen (%copy-mode-active-p session))
         (cond
           ,@(mapcar
              (lambda (rule)
                (destructuring-bind (pattern &rest body) rule
                  `((and (= (length bytes) ,(length pattern))
                         ,@(loop for byte in pattern
                                 for i from 0
                                 collect `(= (aref bytes ,i) ,byte)))
                    ,@body
                    (setf *dirty* t)
                    t)))
              rules)
           (t nil))))))

(define-copy-mode-escape-table
  ((+byte-esc+ +byte-csi-bracket+ +byte-arrow-up+)    (copy-mode-move-cursor screen :up))
  ((+byte-esc+ +byte-csi-bracket+ +byte-arrow-down+)  (copy-mode-move-cursor screen :down))
  ((+byte-esc+ +byte-csi-bracket+ +byte-arrow-left+)  (copy-mode-move-cursor screen :left))
  ((+byte-esc+ +byte-csi-bracket+ +byte-arrow-right+) (copy-mode-move-cursor screen :right))
  ((+byte-q+)                                          (copy-mode-exit  screen)))

;;; ── CPS keystroke processing ─────────────────────────────────────────────────
;;;
;;; Each state is a function (SESSION BYTE) → (values OUTCOME NEXT-STATE)
;;; where OUTCOME is :QUIT, :DETACH, or NIL, and NEXT-STATE is the next state
;;; function (or NIL meaning "return to ground state").
;;;
;;; define-cps-state is the session-level analogue of define-state in
;;; terminal/parser.lisp: both express dispatch as Prolog-like ordered clauses.

;;; ── Prolog-like CPS state definition macro ───────────────────────────────────

(defmacro define-cps-state (name (session-var byte-var) &rest rules)
  "Build a (SESSION BYTE) → (values OUTCOME NEXT-STATE) function from
   ordered Prolog-like clauses.  Each RULE is (CONDITION &rest BODY);
   the first matching condition wins (ordered-cut semantics, like cond).
   Both SESSION and BYTE are declared ignorable so rules that need only
   one compile cleanly."
  `(defun ,name (,session-var ,byte-var)
     (declare (ignorable ,session-var ,byte-var))
     (cond
       ,@(mapcar
          (lambda (rule)
            (destructuring-bind (condition &rest body) rule
              `(,condition ,@body)))
          rules))))

(defun %arrow-final-to-ss3-bytes (final-byte)
  "Given a CSI arrow-key final byte (65=A/66=B/67=C/68=D), return the
   corresponding SS3 sequence bytes (ESC O A/B/C/D) as an octet vector,
   or NIL if the final byte is not an arrow key."
  (when (member final-byte '(65 66 67 68))
    (make-array 3 :element-type '(unsigned-byte 8)
                  :initial-contents (list 27 79 final-byte)))) ; ESC O <final>

