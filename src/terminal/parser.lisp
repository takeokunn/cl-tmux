(in-package #:cl-tmux/terminal/parser)

;;;; CPS-based VT100 parser.
;;;;
;;;; Each parser state is a function: (screen byte) → next-state-function
;;;; The screen's (parser) slot holds the current continuation.
;;;; There is no mutable parser state outside of the CPS closures themselves.

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

;;; All cursor / write / scroll / reset side effects live in
;;; cl-tmux/terminal/actions (the logic layer) and are used here unqualified:
;;; write-codepoint, write-char-at-cursor, cursor-lf, cursor-cr, cursor-bs, cursor-ht,
;;; cursor-ri, save-cursor, restore-cursor, ris-action.

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

(defun ground-state (screen byte)
  "Ground (normal) state: process one byte and return the next state."
  (declare (type screen screen) (type (unsigned-byte 8) byte))
  (cond
    ((= byte #x1B)  #'escape-state)
    ((= byte #x0D)  (cursor-cr screen) #'ground-state)
    ((= byte #x0A)  (cursor-lf screen) #'ground-state)
    ((= byte #x0B)  (cursor-lf screen) #'ground-state)
    ((= byte #x0C)  (cursor-lf screen) #'ground-state)
    ((= byte #x08)  (cursor-bs screen) #'ground-state)
    ((= byte #x09)  (cursor-ht screen) #'ground-state)
    ((= byte #x07)  #'ground-state)    ; BEL — ignore
    ((= byte #x7F)  #'ground-state)    ; DEL — ignore
    ((= byte #x0E)  #'ground-state)    ; SO — charset shift out (ignore)
    ((= byte #x0F)  #'ground-state)    ; SI — charset shift in  (ignore)
    ((printable-ascii-p byte)
     (write-char-at-cursor screen (code-char byte))
     #'ground-state)
    ((utf8-lead-p byte)
     (multiple-value-bind (acc left) (utf8-lead-decode byte)
       (make-utf8-k acc left)))
    ((>= byte #x80)                    ; stray continuation byte / #xFF: invalid UTF-8
     (write-codepoint screen #xFFFD)
     #'ground-state)
    (t #'ground-state)))               ; unhandled C0 control byte: ignore

(defun escape-state (screen byte)
  "ESC received; dispatch on next byte."
  (declare (type screen screen) (type (unsigned-byte 8) byte))
  (cond
    ((= byte #x5B)  (make-csi-k '() nil nil))          ; ESC [ → CSI
    ((= byte #x5D)  #'osc-state)                        ; ESC ] → OSC
    ((= byte #x4D)  (cursor-ri screen) #'ground-state)  ; ESC M → RI
    ((= byte #x63)  (ris-action screen) #'ground-state) ; ESC c → RIS
    ((= byte #x28)  #'charset-state) ; ESC ( → G0 charset designate
    ((= byte #x29)  #'charset-state) ; ESC ) → G1 charset designate
    ((= byte #x3D)  #'ground-state)  ; ESC = → DECKPAM (ignore)
    ((= byte #x3E)  #'ground-state)  ; ESC > → DECKPNM (ignore)
    ((= byte #x37)  (save-cursor screen)    #'ground-state) ; ESC 7 → DECSC
    ((= byte #x38)  (restore-cursor screen) #'ground-state) ; ESC 8 → DECRC
    (t              #'ground-state)))                    ; unknown ESC sequence

(defun osc-state (screen byte)
  "Inside an OSC sequence; discard payload and watch for terminator."
  (declare (type screen screen) (type (unsigned-byte 8) byte)
           (ignore screen))
  (cond
    ((= byte #x07) #'ground-state)   ; BEL terminates OSC
    ((= byte #x1B) #'osc-st-state)  ; might be ST = ESC backslash
    (t             #'osc-state)))

(defun osc-st-state (screen byte)
  "Possible ST terminator inside OSC (ESC already consumed)."
  (declare (type screen screen) (ignore screen))
  (if (= byte #x5C)                  ; \ → ST confirmed
      #'ground-state
      #'osc-state))

(defun charset-state (screen byte)
  "Consume one charset designator byte and return to ground."
  (declare (type screen screen) (type (unsigned-byte 8) byte)
           (ignore screen byte))
  #'ground-state)
