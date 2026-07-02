(in-package #:cl-tmux)

;;;; Event processing — core macros, mouse dispatch, overlay handler.

;;; ── Prompt key handler ──────────────────────────────────────────────────────
;;;
;;; define-prompt-key-rules generates HANDLE-PROMPT-KEY from a declarative
;;; byte-dispatch table, matching the Prolog-like rule style used throughout
;;; the codebase (define-command-handlers, define-csi-rules, define-state).

;;; UTF-8 accumulation state for the prompt (module-level; main-thread-only).
;;;
;;; Mirrors terminal/parser.lisp's MAKE-UTF8-K: rather than mutating a raw
;;; accumulator/count pair across calls, a lead byte closes over its decode
;;; state and returns a continuation function.  *PROMPT-UTF8-CONTINUATION*
;;; holds that continuation (or NIL at ground state, i.e. no sequence in
;;; progress) — the same CPS data-flow style %COPY-MODE-ACCUMULATE-DIGIT's
;;; docstring calls for, applied here to eliminate the two former defvars.

(defvar *prompt-utf8-continuation* nil
  "NIL at ground state, or a (LAMBDA (BYTE) ...) continuation returned by
   MAKE-PROMPT-UTF8-K that folds the next UTF-8 continuation byte into the
   in-progress code point.")

(defun %prompt-utf8-lead-decode (byte)
  "Return (values initial-accumulator continuation-bytes-remaining) for a UTF-8
   LEAD BYTE (caller must already know BYTE is a valid lead byte).  Mirrors
   terminal/parser.lisp's UTF8-LEAD-DECODE, expressed with this file's own
   named +BYTE-UTF8-*+ constants rather than raw hex."
  (cond ((< byte +byte-utf8-2byte-lead-max+)
         (values (logand byte +byte-utf8-2byte-lead-data-mask+) 1))
        ((< byte +byte-utf8-3byte-lead-max+)
         (values (logand byte +byte-utf8-3byte-lead-data-mask+) 2))
        (t
         (values (logand byte +byte-utf8-4byte-lead-data-mask+) 3))))

(defun make-prompt-utf8-k (accumulator continuation-bytes-remaining)
  "Return a continuation that collects UTF-8 continuation bytes for the prompt.
   ACCUMULATOR is the code-point bits collected so far (from the lead byte).
   CONTINUATION-BYTES-REMAINING is the count of continuation bytes still needed.
   On the final continuation byte, the assembled code point is inserted into
   the prompt (silently dropped if it does not name a valid character) and
   the returned continuation is NIL (ground state)."
  (lambda (byte)
    (if (= (logand byte +byte-utf8-continuation-tag-mask+) +byte-utf8-continuation-tag+)
        (let ((new-accumulator (logior (ash accumulator 6)
                                        (logand byte +byte-utf8-continuation-data-mask+)))
              (bytes-left      (1- continuation-bytes-remaining)))
          (if (zerop bytes-left)
              (let ((character (ignore-errors (code-char new-accumulator))))
                (when character (prompt-input character))
                nil)
              (make-prompt-utf8-k new-accumulator bytes-left)))
        ;; Malformed: not a continuation byte — drop the in-progress sequence.
        nil)))

(defmacro define-prompt-key-rules (&rest rules)
  "Build HANDLE-PROMPT-KEY from a byte-dispatch table.
   Each RULE is (PATTERN &rest BODY) where PATTERN is:
     integer  → exact byte match
     list     → verbatim condition
     t        → default clause
   Always marks *dirty* after dispatching."
  `(defun handle-prompt-key (byte)
     "Route one input BYTE to the active prompt.
      UTF-8 multi-byte sequences are decoded via the CPS continuation held in
      *PROMPT-UTF8-CONTINUATION* (see MAKE-PROMPT-UTF8-K) before the dispatch
      table is consulted."
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
;;;
;;; define-prompt-vi-key-rules follows the same Prolog-like rule style as
;;; define-copy-mode-vi-rules (events-copy-mode-dispatch.lisp) and
;;; define-prompt-key-rules (above).  It generates %HANDLE-VI-NORMAL-KEY from
;;; a declarative dispatch table of (pattern &rest body) rules.

(defmacro define-prompt-vi-key-rules (&rest rules)
  "Build %HANDLE-VI-NORMAL-KEY from a byte-dispatch table.
   Each RULE is (PATTERN &rest BODY) where PATTERN is a byte literal or constant.
   The generated function returns T when the byte was consumed, NIL otherwise."
  `(defun %handle-vi-normal-key (byte)
     "Dispatch BYTE in vi normal mode.  Returns T when the key was handled.
      Navigation: h (left), l (right), 0/^ (BOL), $ (EOL).
      Editing:    x (delete char), D (delete to end).
      Mode switch: a/i/A/I return to insert mode.
      Enter:      submit; ESC/C-c: cancel."
     (let ((prompt *prompt*))
       (when (and prompt (prompt-vi-normal-p prompt))
         (case byte
           ,@(mapcar
              (lambda (rule)
                (destructuring-bind (pattern &rest body) rule
                  `(,pattern ,@body)))
              rules))))))

(define-prompt-vi-key-rules
  ;; Navigation
  (#.+byte-h+       (prompt-cursor-back)    t)  ; h — left
  (#.+byte-l+       (prompt-cursor-forward) t)  ; l — right
  (#.+byte-digit-0+ (prompt-cursor-bol)     t)  ; 0 — beginning of line
  (#.+byte-caret+   (prompt-cursor-bol)     t)  ; ^ — beginning of line
  (#.+byte-dollar+  (prompt-cursor-eol)     t)  ; $ — end of line
  (#.+byte-w+       (prompt-cursor-forward) t)  ; w — word forward (approx: move right)
  (#.+byte-b+       (prompt-cursor-back)    t)  ; b — word backward (approx: move left)
  ;; Editing
  (#.+byte-capital-d+   (prompt-kill-to-end)  t) ; D — delete to end of line
  (#.+byte-lowercase-x+ (prompt-delete-char)  t) ; x — delete char under cursor
  ;; Enter insert mode
  (#.+byte-lowercase-a+ (prompt-cursor-forward)  ; a — append (move right, enter insert)
       (setf (prompt-vi-normal-p prompt) nil)
       t)
  (#.+byte-capital-a+ (prompt-cursor-eol)        ; A — append at end
       (setf (prompt-vi-normal-p prompt) nil)
       t)
  (#.+byte-i+ (setf (prompt-vi-normal-p prompt) nil)  ; i — insert mode
       t)
  (#.+byte-capital-i+ (prompt-cursor-bol)        ; I — insert at beginning
       (setf (prompt-vi-normal-p prompt) nil)
       t)
  (#.+byte-enter+ (let ((active-prompt prompt))  ; Enter — submit
         (when (prompt-on-submit active-prompt)
           (funcall (prompt-on-submit active-prompt) (prompt-buffer active-prompt)))
         (prompt-clear))
       t)
  (#.+byte-esc+    (prompt-clear) t)             ; ESC in normal mode — cancel
  (#.+byte-ctrl-c+ (prompt-clear) t)              ; C-c — cancel
  (otherwise nil))                    ; unhandled — fall through to insert

(define-prompt-key-rules
  (#.+byte-enter+                           ; Enter — submit and dismiss
   (setf *prompt-utf8-continuation* nil)
   (let ((active-prompt *prompt*))
     (when (and active-prompt (prompt-on-submit active-prompt))
       (funcall (prompt-on-submit active-prompt) (prompt-buffer active-prompt)))
     (prompt-clear)))
  (#.+byte-esc+                             ; Esc
   (setf *prompt-utf8-continuation* nil)
   (let ((p *prompt*))
     (cond
       ;; vi mode: ESC enters normal mode (does NOT cancel the prompt).
       ((and p (string-equal (cl-tmux/options:get-option "status-keys" "emacs") "vi")
             (not (prompt-vi-normal-p p)))
        (setf (prompt-vi-normal-p p) t))
       ;; emacs mode or already in vi-normal: cancel.
       (t (prompt-clear)))))
  (#.+byte-ctrl-c+ (setf *prompt-utf8-continuation* nil)
   (prompt-clear))                          ; C-c — cancel
  (#.+byte-ctrl-a+ (prompt-cursor-bol))      ; C-a — beginning of line
  (#.+byte-ctrl-e+ (prompt-cursor-eol))      ; C-e — end of line
  (#.+byte-ctrl-b+ (prompt-cursor-back))     ; C-b — cursor left
  (#.+byte-ctrl-f+ (prompt-cursor-forward))  ; C-f — cursor right
  (#.+byte-ctrl-k+ (prompt-kill-to-end))     ; C-k — kill to end
  (#.+byte-ctrl-u+ (prompt-kill-to-start))   ; C-u — kill to start
  (#.+byte-ctrl-w+ (prompt-kill-word-back))  ; C-w — kill previous word
  ((or (= byte #.+byte-del+) (= byte #.+byte-backspace+))
   (prompt-backspace))                      ; Backspace / DEL
  ;; Single-key prompt (confirm-before, command-prompt -1): the first printable
  ;; key IS the answer — submit it immediately as a 1-char string, no Enter.
  ;; Control keys (Esc, C-c) are < 32 and fall through to their cancel handlers.
  ((and *prompt* (prompt-single-key *prompt*)
        (>= byte #.+byte-space+) (< byte #.+byte-del+))
   (setf *prompt-utf8-continuation* nil)
   (let ((active-prompt *prompt*)
         (character     (code-char byte)))
     (when (prompt-on-submit active-prompt)
       (funcall (prompt-on-submit active-prompt) (string character)))
     (prompt-clear)))
  ;; Vi normal mode: intercept printable bytes before insert dispatch.
  ((%handle-vi-normal-key byte) nil)        ; consumed by vi-normal — already handled
  ((and (>= byte #.+byte-space+) (< byte #.+byte-del+))
   (prompt-input (code-char byte)))         ; printable ASCII — insert
  ;; UTF-8 continuation byte: fold into the in-progress CPS continuation, if any.
  ((= (logand byte +byte-utf8-continuation-tag-mask+) +byte-utf8-continuation-tag+)
   (when *prompt-utf8-continuation*
     (setf *prompt-utf8-continuation*
           (funcall *prompt-utf8-continuation* byte))))
  ;; UTF-8 lead byte: begin multi-byte decode by arming a fresh continuation.
  ((and (>= byte +byte-utf8-lead-min+) (/= byte +byte-utf8-lead-invalid+))
   (multiple-value-bind (accumulator bytes-left) (%prompt-utf8-lead-decode byte)
     (setf *prompt-utf8-continuation* (make-prompt-utf8-k accumulator bytes-left))))
  (t nil))                                  ; other control bytes — ignore

;;; (byte constants live in events-constants.lisp, loaded before this file)

;;; Holds the in-progress escape-accumulation buffer (the same object the
;;; make-escape-input-k closure is growing) so %flush-esc-if-timed-out can replay
;;; the FULL partial sequence on an escape-time timeout instead of dropping every
;;; byte after the leading ESC.  NIL when no escape sequence is being accumulated;
;;; in that state the flush falls back to forwarding a lone ESC (vim insert-mode).
(defvar *esc-accum-buffer* nil
  "Octet vector currently accumulating after an ESC, or NIL at ground.")

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
  ((+byte-esc+ +byte-csi-bracket+ +byte-arrow-right+) (copy-mode-move-cursor screen :right)))

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
  "Given a CSI arrow-key final byte (+byte-arrow-up+/-down+/-right+/-left+),
   return the corresponding SS3 sequence bytes (ESC O A/B/C/D) as an octet
   vector, or NIL if FINAL-BYTE is not an arrow key."
  (when (member final-byte (list +byte-arrow-up+ +byte-arrow-down+
                                  +byte-arrow-right+ +byte-arrow-left+))
    (make-array 3 :element-type '(unsigned-byte 8)
                  :initial-contents (list +byte-esc+ +byte-ss3-o+ final-byte))))
