(in-package #:cl-tmux/terminal/parser)

;;;; CSI continuation logic.

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
