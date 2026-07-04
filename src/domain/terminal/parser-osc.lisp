(in-package #:cl-tmux/terminal/parser)

;;;; OSC accumulator and dispatcher.
;;;;
;;;; Command parsing and OSC handlers live in parser-osc-dispatch.lisp.
;;;; Shared helpers are split across parser-osc-clipboard.lisp,
;;;; parser-osc-uri.lisp and parser-osc-color.lisp.

;;; ── CPS OSC accumulator continuations ──────────────────────────────────────
;;;
;;; make-osc-k builds the accumulator closure that collects raw OSC payload
;;; bytes.  make-osc-st-k builds the single-byte "waiting for backslash" bridge
;;; state used after an ESC inside an OSC sequence (potential ESC \ ST).
;;;
;;; Both continuations receive (screen byte) and return the next state function,
;;; matching the CPS state machine contract defined by define-state.

(defun make-osc-st-k (buffer)
  "Return a continuation waiting for the backslash of ESC \\ (String Terminator).
   BUFFER is the accumulated OSC payload so far.
   On backslash: dispatch the payload and return ground-state.
   On any other byte: return ground-state without dispatching (malformed ST)."
  (lambda (screen-arg byte)
    (declare (type screen screen-arg) (type (unsigned-byte 8) byte))
    (when (= byte #x5C)
      (%dispatch-osc screen-arg buffer))
    #'ground-state))

(defun make-osc-k (buffer)
  "Return a continuation that accumulates OSC payload bytes into BUFFER.
   Dispatches to %DISPATCH-OSC on BEL (#x07) or the start of ESC \\ termination."
  (lambda (screen-arg byte)
    (declare (type screen screen-arg) (type (unsigned-byte 8) byte))
    (cond
      ((= byte #x07)
       ;; BEL: OSC terminated — dispatch and return to ground.
       (%dispatch-osc screen-arg buffer)
       #'ground-state)
      ((= byte #x1B)
       ;; Possible ESC \ (ST) — hand off to the bridge state.
       (make-osc-st-k buffer))
      (t
       ;; Continue accumulating payload bytes.
       (vector-push-extend byte buffer)
       (make-osc-k buffer)))))
