(in-package #:cl-tmux/terminal/parser)

;;;; CPS-based VT100 parser.
;;;;
;;;; Each parser state is a function: (screen byte) → next-state-function
;;;; The screen's (parser) slot holds the current continuation.
;;;; There is no mutable parser state outside of the CPS closures themselves.
;;;;
;;;; States are defined with DEFINE-STATE (parser-core.lisp), a Prolog-like
;;;; macro that maps byte patterns to actions and their continuation.
;;;; Parameterized CSI and UTF-8 continuations live in parser-csi.lisp and
;;;; parser-utf8.lisp so this file remains the state-machine skeleton.

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
  ;; ── Charset designators: ESC ( / ) / * / + designate G0..G3 ────────────────
  (#x28  (make-charset-designator-k :g0))          ; ESC ( → designate G0
  (#x29  (make-charset-designator-k :g1))          ; ESC ) → designate G1
  (#x2A  (make-charset-designator-k :g2))          ; ESC * → designate G2
  (#x2B  (make-charset-designator-k :g3))          ; ESC + → designate G3
  ;; ── Locking shifts LS2/LS3 and single shifts SS2/SS3 ───────────────────────
  (#x6E  (invoke-charset screen :g2) #'ground-state)             ; ESC n → LS2
  (#x6F  (invoke-charset screen :g3) #'ground-state)             ; ESC o → LS3
  (#x4E  (setf (screen-single-shift screen) :g2) #'ground-state) ; ESC N → SS2
  (#x4F  (setf (screen-single-shift screen) :g3) #'ground-state) ; ESC O → SS3
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
