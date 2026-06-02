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
  "Return a continuation that collects CSI parameters then dispatches.
   Handles the standard VT/ECMA-48 CSI parameter syntax:
     param bytes   0x30-0x3F  (digits, semicolon, ?, >)
     intermediate  0x20-0x2F  (e.g. SPACE for DECSCUSR)
     final byte    0x40-0x7E"
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
      ;; ? : DEC private marker (param byte)
      ((= byte #x3F)
       (make-csi-k params cur-param #\?))
      ;; > : secondary DA marker (param byte)
      ((= byte #x3E)
       (make-csi-k params cur-param #\>))
      ;; Intermediate bytes (0x20-0x2F): record as intermed.
      ;; SPACE (#x20) is the most common (used by DECSCUSR "CSI N SP q").
      ((and (>= byte #x20) (<= byte #x2F))
       (make-csi-k params cur-param (code-char byte)))
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
  (#x07  (setf (screen-bell-pending screen) t)
         #'ground-state)                           ; BEL — set pending flag
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
  (#x50  (make-dcs-k))                             ; ESC P → DCS (consume silently)
  (#x4D  (cursor-ri screen)    #'ground-state)    ; ESC M → RI
  (#x63  (ris-action screen)   #'ground-state)    ; ESC c → RIS
  (#x37  (save-cursor screen)    #'ground-state)  ; ESC 7 → DECSC
  (#x38  (restore-cursor screen) #'ground-state)  ; ESC 8 → DECRC
  ;; ── Charset designators ──────────────────────────────────────────────────
  (#x28  #'charset-state)                          ; ESC ( → G0
  (#x29  #'charset-state)                          ; ESC ) → G1
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
  (t     (let ((buf (make-array 64
                                :element-type '(unsigned-byte 8)
                                :fill-pointer 0
                                :adjustable t)))
           (vector-push-extend byte buf)
           (make-osc-k buf))))

(define-state osc-st-state (screen byte)
  (#x5C  #'ground-state)                           ; \ → ST confirmed (empty payload)
  (t     #'osc-state))

;;; DCS (Device Control String) accumulator.
;;; ESC P introduces a DCS; collect bytes until ESC \ (ST).
;;; For now: consume silently (pass-through no-op).

(defun make-dcs-k ()
  "Return a continuation that consumes DCS payload bytes until ST (ESC \\)."
  (lambda (screen byte)
    (declare (type screen screen) (type (unsigned-byte 8) byte))
    (cond
      ((= byte #x1B)
       ;; Possible ESC \ ST — wait for backslash
       (lambda (screen2 byte2)
         (declare (type screen screen2) (type (unsigned-byte 8) byte2))
         (declare (ignore screen2))
         (if (= byte2 #x5C)      ; backslash = ST confirmed
             #'ground-state
             ;; Not ST — keep consuming
             (funcall (make-dcs-k) screen2 byte2))))
      (t
       ;; Continue consuming
       (make-dcs-k)))))

(define-state charset-state (screen byte)
  ;; Consume the designator byte.
  ;; #x30 = '0' → switch to DEC special graphics
  ;; #x42 = 'B' → switch to US ASCII
  ;; All other designators are accepted silently.
  (#x30  (setf (screen-charset screen) :dec-graphics) #'ground-state)
  (#x42  (setf (screen-charset screen) :ascii)        #'ground-state)
  (t     (setf (screen-charset screen) :ascii)        #'ground-state))
