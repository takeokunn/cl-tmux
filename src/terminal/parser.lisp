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
  (and (>= byte #x20) (< byte #x7F)))

(defun utf8-lead-p (byte)
  (and (>= byte #xC0) (/= byte #xFF)))

(defun utf8-continuation-p (byte)
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
  "Prolog-like CPS state definition: one rule per parser state clause."
  `(defun ,name (,screen-var ,byte-var)
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

;;; ── Parameterized state constructors ───────────────────────────────────────

(defun make-csi-k (&optional (params '()) (cur-param nil) (intermed nil))
  "Return a continuation that collects CSI parameters then dispatches."
  (lambda (screen byte)
    (declare (type screen screen) (type (unsigned-byte 8) byte))
    (cond
      ;; Digit 0-9: accumulate into current parameter
      ((and (>= byte #x30) (<= byte #x39))
       (make-csi-k params
                   (+ (* (or cur-param 0) 10) (- byte #x30))
                   intermed))
      ;; Semicolon: end current param, start next
      ((= byte #x3B)
       (make-csi-k (cons (or cur-param 0) params) nil intermed))
      ;; ? : DEC private marker
      ((= byte #x3F)
       (make-csi-k params cur-param #\?))
      ;; > : secondary DA marker
      ((= byte #x3E)
       (make-csi-k params cur-param #\>))
      ;; Final byte (0x40-0x7E): dispatch
      ((and (>= byte #x40) (<= byte #x7E))
       (let ((all-params (nreverse (if cur-param
                                       (cons cur-param params)
                                       params))))
         (execute-csi screen (code-char byte) intermed all-params))
       #'ground-state)
      ;; Anything else: abort CSI
      (t #'ground-state))))

(defun make-utf8-k (acc remaining)
  "Return a continuation that collects UTF-8 continuation bytes."
  (declare (type fixnum acc remaining))
  (lambda (screen byte)
    (declare (type screen screen) (type (unsigned-byte 8) byte))
    (if (utf8-continuation-p byte)
        (let ((new-acc  (logior (ash acc 6) (logand byte #x3F)))
              (new-left (1- remaining)))
          (if (zerop new-left)
              (progn (write-codepoint screen new-acc)
                     #'ground-state)
              (make-utf8-k new-acc new-left)))
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
  (#x0A  (cursor-lf screen) #'ground-state)
  (#x0B  (cursor-lf screen) #'ground-state)        ; VT treated as LF
  (#x0C  (cursor-lf screen) #'ground-state)        ; FF treated as LF
  (#x08  (cursor-bs screen) #'ground-state)
  (#x09  (cursor-ht screen) #'ground-state)
  (#x07  #'ground-state)                           ; BEL — ignore
  (#x7F  #'ground-state)                           ; DEL — ignore
  (#x0E  #'ground-state)                           ; SO  — charset shift out (ignore)
  (#x0F  #'ground-state)                           ; SI  — charset shift in  (ignore)
  ;; ── Printable ASCII ─────────────────────────────────────────────────────
  (printable-ascii-p
   (write-char-at-cursor screen (code-char byte))
   #'ground-state)
  ;; ── Multi-byte UTF-8 ────────────────────────────────────────────────────
  (utf8-lead-p
   (multiple-value-bind (acc left) (utf8-lead-decode byte)
     (make-utf8-k acc left)))
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
  (#x4D  (cursor-ri screen)    #'ground-state)    ; ESC M → RI
  (#x63  (ris-action screen)   #'ground-state)    ; ESC c → RIS
  (#x37  (save-cursor screen)    #'ground-state)  ; ESC 7 → DECSC
  (#x38  (restore-cursor screen) #'ground-state)  ; ESC 8 → DECRC
  ;; ── Charset designators ──────────────────────────────────────────────────
  (#x28  #'charset-state)                          ; ESC ( → G0
  (#x29  #'charset-state)                          ; ESC ) → G1
  ;; ── All unrecognized ESC sequences → ground (including DECKPAM #x3D, DECKPNM #x3E)
  (t     #'ground-state))

(define-state osc-state (screen byte)
  ;; OSC payload is discarded; screen is unused throughout.
  (#x07  #'ground-state)                           ; BEL terminates OSC
  (#x1B  #'osc-st-state)                           ; possible ST = ESC \
  (t     #'osc-state))

(define-state osc-st-state (screen byte)
  (#x5C  #'ground-state)                           ; \ → ST confirmed
  (t     #'osc-state))

(define-state charset-state (screen byte)
  ;; Consume one designator byte and return to ground; both args unused.
  (t #'ground-state))
