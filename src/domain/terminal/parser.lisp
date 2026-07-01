(in-package #:cl-tmux/terminal/parser)

;;;; CPS-based VT100 parser.
;;;;
;;;; Each parser state is a function: (screen byte) → next-state-function
;;;; The screen's (parser) slot holds the current continuation.
;;;; There is no mutable parser state outside of the CPS closures themselves.
;;;;
;;;; States are defined with DEFINE-STATE, a Prolog-like macro that maps
;;;; byte patterns to actions and their continuation (next state).  Each
;;;; rule reads as a Prolog clause: "given this byte, take this action,
;;;; transition to this state."

;;; ── Inline helpers ─────────────────────────────────────────────────────────

(declaim (inline printable-ascii-p utf8-lead-p utf8-continuation-p))

(defun printable-ascii-p (byte)
  "Return T when BYTE is in the printable ASCII range #x20-#x7E (space through tilde)."
  (and (>= byte #x20) (< byte #x7F)))

(defun utf8-lead-p (byte)
  "Return T when BYTE is a UTF-8 multi-byte lead byte (#xC0-#xFE, excluding #xFF)."
  (and (>= byte #xC0) (/= byte #xFF)))

(defun utf8-continuation-p (byte)
  "Return T when BYTE is a UTF-8 continuation byte (#x80-#xBF, high two bits = 10)."
  (= (logand byte #xC0) #x80))

(defun utf8-lead-decode (byte)
  "Return (values initial-accumulator continuation-bytes-remaining)."
  (cond ((< byte #xE0) (values (logand byte #x1F) 1))
        ((< byte #xF0) (values (logand byte #x0F) 2))
        (t             (values (logand byte #x07) 3))))

;;; ── Prolog-like state definition macro ─────────────────────────────────────
;;;
;;; (define-state NAME (SCREEN BYTE) rule...)
;;; Each rule is (PATTERN &rest BODY) where PATTERN is:
;;;   integer  → exact byte match:   (= BYTE integer)
;;;   symbol   → predicate match:    (symbol BYTE)
;;;   t        → default clause
;;;   list     → verbatim condition
;;; The BODY forms are evaluated in order; the last form is the next state.
;;; Both SCREEN and BYTE are declared ignorable so state functions that
;;; discard their arguments (e.g. osc-state, charset-state) compile cleanly.

(defmacro define-state (name (screen-var byte-var) &rest rules)
  "Prolog-like CPS state definition: one rule per parser state clause.
   Expands into a DEFUN named NAME that takes (SCREEN-VAR BYTE-VAR) and
   returns the next CPS continuation function.  A generated docstring is
   injected so the exported state functions are documented at the function
   level, not only via the surrounding block comments."
  `(defun ,name (,screen-var ,byte-var)
     ,(format nil "CPS parser state ~(~A~): (screen byte) -> next-state-function.~%   ~
                   Dispatches on BYTE across ~D rule~:P defined via DEFINE-STATE."
              name (length rules))
     (declare (type screen ,screen-var)
              (type (unsigned-byte 8) ,byte-var)
              (ignorable ,screen-var ,byte-var))
     (cond
       ,@(mapcar
          (lambda (rule)
            (destructuring-bind (pattern &rest body) rule
              `(,(cond
                   ((eq pattern 't)    't)
                   ((integerp pattern) `(= ,byte-var ,pattern))
                   ((symbolp pattern)  `(,pattern ,byte-var))
                   (t                   pattern))
                ,@body)))
          rules))))

;;; ── CSI byte-class constants ────────────────────────────────────────────────
;;;
;;; Named constants for the magic hex literals used in make-csi-k.
;;; The ranges follow ECMA-48 § 5.4 table.

(defconstant +csi-digit-low+   #x30 "Lowest decimal digit byte in a CSI sequence (ASCII '0').")
(defconstant +csi-digit-high+  #x39 "Highest decimal digit byte in a CSI sequence (ASCII '9').")
(defconstant +csi-semicolon+   #x3B "CSI parameter separator ';'.")
(defconstant +csi-colon+       #x3A
  "CSI sub-parameter separator ':' (ISO 8613-6).  Introduces colon-delimited
   sub-parameters within one parameter, e.g. SGR 4:3 (undercurl) or
   38:2::R:G:B (true-colour).  A parameter carrying colon sub-parameters is
   collected into a list (sub0 sub1 …) so apply-sgr can apply colon-form
   extended colour, rather than dropping everything after the leading value.")
(defconstant +csi-dec-marker+  #x3F "DEC private-mode marker '?'.")
(defconstant +csi-sec-da+      #x3E "Secondary DA marker '>'.")
(defconstant +csi-xtpoptitle-marker+ #x3C
  "ECMA-48 private-parameter marker '<' (e.g. CSI < Ps t, XTPOPTITLE).")
(defconstant +csi-tertiary-da-marker+ #x3D
  "ECMA-48 private-parameter marker '=' (e.g. CSI = c, tertiary DA / DA3).")
(defconstant +csi-intermed-low+  #x20 "Lowest CSI intermediate byte (SPACE).")
(defconstant +csi-intermed-high+ #x2F "Highest CSI intermediate byte.")
(defconstant +csi-final-low+   #x40 "Lowest valid CSI final byte '@'.")
(defconstant +csi-final-high+  #x7E "Highest valid CSI final byte '~'.")

(declaim (inline csi-final-byte-before-p csi-final-byte-p))

(defun csi-final-byte-before-p (byte)
  "Return T when BYTE precedes the CSI final-byte range (i.e. still a
   parameter, intermediate, or marker byte — the sequence is incomplete)."
  (< byte +csi-final-low+))

(defun csi-final-byte-p (byte)
  "Return T when BYTE falls within the CSI final-byte range
   (+csi-final-low+ to +csi-final-high+), i.e. it terminates the sequence."
  (<= +csi-final-low+ byte +csi-final-high+))

;;; ── Parameterized state constructors ───────────────────────────────────────

(defun %finish-param (param-accumulator subparams)
  "Combine a parameter's leading PARAM-ACCUMULATOR with its colon SUBPARAMS
   (already-flushed sub-values, in reverse order) into the finished parameter:
   a plain integer when no colon appeared, or a list (sub0 sub1 …) when it did.
   An absent leading value defaults to 0 (matching the semicolon-param rule)."
  (if subparams
      (nreverse (cons (or param-accumulator 0) subparams))
      (or param-accumulator 0)))

(defun %csi-dispatch-final-byte (screen byte intermed private params param-accumulator subparams)
  "Flush the trailing parameter (if any), reverse the collected PARAMS into
   final CSI dispatch order, and call EXECUTE-CSI with the assembled sequence.
   Called by make-csi-k's continuation once a final byte (0x40-0x7E) closes
   the sequence.  Always returns #'GROUND-STATE."
  (let ((all-params (nreverse (if (or param-accumulator subparams)
                                   (cons (%finish-param param-accumulator subparams) params)
                                   params))))
    (execute-csi screen (code-char byte) intermed private all-params))
  #'ground-state)

(defun make-csi-k (&optional (params '()) (param-accumulator nil) (intermed nil)
                             (private nil) (subparams nil))
  "Return a continuation that collects CSI parameters then dispatches.
   Handles the standard VT/ECMA-48 CSI parameter syntax:
     param bytes        +csi-digit-low+ to +csi-digit-high+  (digits 0-9)
     semicolons         +csi-semicolon+                       (parameter separator)
     marker bytes       +csi-dec-marker+ (#\\?) and +csi-sec-da+ (#\\>)
       These are VT convention 'private use' markers that set the intermed slot
       rather than the parameter accumulator.  They are NOT the same as true
       intermediate bytes (#x20-#x2F), even though both affect INTERMED.
     intermediate bytes +csi-intermed-low+ to +csi-intermed-high+  (e.g. SPACE)
       True intermediate bytes such as #x20 (SPACE) select a sub-table of the
       final-byte dispatch (e.g. DECSCUSR uses CSI N SP q).
     final byte         +csi-final-low+  to +csi-final-high+  (dispatch)"
  (lambda (screen byte)
    (declare (type screen screen) (type (unsigned-byte 8) byte))
    (cond
      ;; Digit 0-9: accumulate into the current parameter accumulator — unless we
      ;; are skipping a colon sub-parameter, in which case the digit is consumed
      ;; and discarded (we keep only the parameter's leading value).
      ((and (>= byte +csi-digit-low+) (<= byte +csi-digit-high+))
       (make-csi-k params
                   (+ (* (or param-accumulator 0) 10) (- byte +csi-digit-low+))
                   intermed private subparams))
      ;; Colon: ISO 8613-6 sub-parameter separator.  Flush the leading value
      ;; accumulated so far into SUBPARAMS and begin the next sub-parameter; the
      ;; finished parameter becomes a list so apply-sgr parses colon-form
      ;; extended colour (38:2:R:G:B, 38:5:N, 4:3 undercurl) rather than dropping
      ;; everything after the leading value.
      ((= byte +csi-colon+)
       (make-csi-k params nil intermed private
                   (cons (or param-accumulator 0) subparams)))
      ;; Semicolon: flush the current parameter (combining its colon sub-params,
      ;; if any), start fresh.
      ((= byte +csi-semicolon+)
       (make-csi-k (cons (%finish-param param-accumulator subparams) params)
                   nil intermed private nil))
      ;; ? — DEC private-mode marker byte (selects DEC private sequences).
      ;; Recorded in the PRIVATE slot (separate from a true intermediate) so that
      ;; sequences carrying BOTH — e.g. DECRQM "CSI ? Ps $ p" — keep the ? marker
      ;; even when a #x20-#x2F intermediate ($) follows.
      ((= byte +csi-dec-marker+)
       (make-csi-k params param-accumulator intermed #\? subparams))
      ;; > — secondary DA marker byte (selects secondary device attribute queries).
      ((= byte +csi-sec-da+)
       (make-csi-k params param-accumulator intermed #\> subparams))
      ;; < and = — the remaining ECMA-48 private-parameter markers (0x3C / 0x3D):
      ;; e.g. CSI < Ps t (XTPOPTITLE), CSI = c (tertiary DA / DA3).  Recorded in
      ;; PRIVATE like ? and >.  Without these, the byte hit the catch-all and
      ;; ABORTED the sequence, leaving the final byte to print as a stray char.
      ((= byte +csi-xtpoptitle-marker+)
       (make-csi-k params param-accumulator intermed #\< subparams))
      ((= byte +csi-tertiary-da-marker+)
       (make-csi-k params param-accumulator intermed #\= subparams))
      ;; Intermediate bytes (SPACE through 0x2F): record as intermed.
      ;; SPACE (#x20) is the most common (used by DECSCUSR "CSI N SP q");
      ;; $ (#x24) appears in DECRQM.  Does NOT disturb the private marker.
      ((and (>= byte +csi-intermed-low+) (<= byte +csi-intermed-high+))
       (make-csi-k params param-accumulator (code-char byte) private subparams))
      ;; Final byte (0x40-0x7E): flush accumulator, reverse collected params, dispatch.
      ((csi-final-byte-p byte)
       (%csi-dispatch-final-byte screen byte intermed private params
                                  param-accumulator subparams))
      ;; Anything else: abort CSI (e.g. C0 controls inside a sequence).
      (t #'ground-state))))

(defun make-utf8-k (code-point-accumulator continuation-bytes-remaining)
  "Return a continuation that collects UTF-8 continuation bytes.
   CODE-POINT-ACCUMULATOR is the accumulator built from the lead byte.
   CONTINUATION-BYTES-REMAINING is the count of continuation bytes still needed.
   On the final continuation byte the assembled code point is written to screen."
  (declare (type fixnum code-point-accumulator continuation-bytes-remaining))
  (lambda (screen byte)
    (declare (type screen screen) (type (unsigned-byte 8) byte))
    (if (utf8-continuation-p byte)
        (let ((updated-accumulator (logior (ash code-point-accumulator 6) (logand byte #x3F)))
              (bytes-left          (1- continuation-bytes-remaining)))
          (if (zerop bytes-left)
              (progn (write-codepoint screen updated-accumulator)
                     #'ground-state)
              (make-utf8-k updated-accumulator bytes-left)))
        ;; Malformed: emit U+FFFD, re-process this byte in ground state
        (progn
          (write-codepoint screen #xFFFD)
          (ground-state screen byte)))))

;;; ── Named (non-parameterized) state functions ──────────────────────────────
;;;
;;; Each define-state call reads like Prolog clauses:
;;;   ground-state(Screen, 0x1B) :- next_state(escape_state).
;;;   ground-state(Screen, 0x0D) :- cursor_cr(Screen), next_state(ground_state).

(define-state ground-state (screen byte)
  ;; ── Escape and control ──────────────────────────────────────────────────
  (#x1B  #'escape-state)
  (#x0D  (cursor-cr screen) #'ground-state)
  (#x0A  (cursor-nl screen) #'ground-state)        ; LF — +CR under LNM (mode 20)
  (#x0B  (cursor-nl screen) #'ground-state)        ; VT treated as LF
  (#x0C  (cursor-nl screen) #'ground-state)        ; FF treated as LF
  (#x08  (cursor-bs screen) #'ground-state)
  (#x09  (cursor-ht screen) #'ground-state)
  (#x07  (set-bell-pending screen)
         #'ground-state)                           ; BEL — set pending flag
  (#x7F  #'ground-state)                           ; DEL — ignore
  (#x0E  (invoke-charset screen :g1) #'ground-state) ; SO — invoke G1 (locking shift out)
  (#x0F  (invoke-charset screen :g0) #'ground-state) ; SI — invoke G0 (locking shift in)
  ;; ── Printable ASCII ─────────────────────────────────────────────────────
  (printable-ascii-p
   (write-char-at-cursor screen (code-char byte))
   #'ground-state)
  ;; ── Multi-byte UTF-8 ────────────────────────────────────────────────────
  (utf8-lead-p
   (multiple-value-bind (code-point-accumulator continuation-bytes-remaining)
       (utf8-lead-decode byte)
     (make-utf8-k code-point-accumulator continuation-bytes-remaining)))
  ;; ── Invalid / stray continuation byte ───────────────────────────────────
  ((>= byte #x80)
   (write-codepoint screen #xFFFD)
   #'ground-state)
  ;; ── Unhandled C0 control byte ────────────────────────────────────────────
  (t #'ground-state))

(define-state escape-state (screen byte)
  ;; ── Standard ESC sequences ───────────────────────────────────────────────
  (#x5B  (make-csi-k '() nil nil))                ; ESC [ → CSI
  (#x5D  #'osc-state)                              ; ESC ] → OSC
  (#x50  (make-dcs-k))                             ; ESC P → DCS (tmux passthrough or discard)
  (#x4D  (cursor-ri screen)    #'ground-state)    ; ESC M → RI
  (#x63  (ris-action screen)   #'ground-state)    ; ESC c → RIS
  (#x37  (save-cursor screen)    #'ground-state)  ; ESC 7 → DECSC
  (#x38  (restore-cursor screen) #'ground-state)  ; ESC 8 → DECRC
  (#x44  (cursor-lf  screen)     #'ground-state)  ; ESC D → IND (index: down, no CR)
  (#x45  (cursor-nel screen)     #'ground-state)  ; ESC E → NEL (next line: CR+LF)
  (#x48  (set-tab-stop screen)   #'ground-state)  ; ESC H → HTS (set tab stop)
  ;; ── Charset designators: ESC ( designates G0, ESC ) designates G1 ──────────
  (#x28  (make-charset-designator-k :g0))          ; ESC ( → designate G0
  (#x29  (make-charset-designator-k :g1))          ; ESC ) → designate G1
  (#x2A  (make-ignore-final-byte-k))               ; ESC * → designate G2 (not modeled)
  (#x2B  (make-ignore-final-byte-k))               ; ESC + → designate G3 (not modeled)
  (#x20  (make-ignore-final-byte-k))               ; ESC SP <f> → S7C1T/S8C1T/ANSI level
  (#x25  (make-ignore-final-byte-k))               ; ESC % <f> → charset selection (UTF-8)
  ;; ESC # — DEC line-size / alignment: the next byte selects (8 = DECALN fill,
  ;; 3/4/5/6 = double-height/width/single-width line attrs, accepted+ignored).
  ;; Without this, ESC # aborted and the selector byte printed as a stray char.
  (#x23  (make-hash-line-size-k))                  ; ESC # → DEC line-size selector
  ;; ── All unrecognized ESC sequences → ground (including DECKPAM #x3D, DECKPNM #x3E)
  (t     #'ground-state))

;;; OSC payload accumulator: captures bytes into a buffer, then dispatches
;;; on BEL (#x07) or ST (ESC \) termination.
;;;
;;; The implementation lives in parser-osc.lisp (loaded after this file):
;;;   *osc52-handler* — clipboard callback variable
;;;   %dispatch-osc   — payload parser and side-effect applier
;;;   make-osc-st-k   — bridge continuation waiting for ESC \ backslash
;;;   make-osc-k      — accumulator continuation for OSC payload bytes

(define-state osc-state (screen byte)
  ;; OSC payload: start accumulating into a fresh buffer.
  ;; Bare BEL and bare ESC with empty payload are handled as no-ops.
  (#x07  #'ground-state)                           ; bare BEL with empty payload
  (#x1B  #'osc-st-state)                           ; possible ST = ESC \ with empty payload
  (t     (let ((payload-buffer (make-array 64
                                           :element-type '(unsigned-byte 8)
                                           :fill-pointer 0
                                           :adjustable t)))
           (vector-push-extend byte payload-buffer)
           (make-osc-k payload-buffer))))

;;; osc-st-state is an internal state used to await the backslash of ESC \
;;; (String Terminator) with an empty OSC payload.  It is not exported because
;;; it is an implementation detail of the OSC accumulator state machine.
;;; Contrast with ground-state and escape-state which are exported since callers
;;; may need to reset or inspect the parser's initial state.
(define-state osc-st-state (screen byte)
  (#x5C  #'ground-state)                           ; \ → ST confirmed (empty payload)
  (t     #'osc-state))

;;; DCS (Device Control String) accumulator.
