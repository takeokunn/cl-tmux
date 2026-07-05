(in-package #:cl-tmux)

;;;; Copy-mode ground-state dispatch.

;;; Extracted from %ground-input-state so the top-level CPS state stays a flat
;;; ordered list of clauses.  %copy-mode-accumulate-digit is itself a small CPS
;;; state function: it accepts the pending BYTE and returns (values COUNT-OR-NIL)
;;; — NIL means "byte consumed into *copy-mode-prefix*, wait for the next byte";
;;; a non-NIL COUNT means "prefix accumulation is complete, dispatch with COUNT".
;;; This expresses the digit accumulator as data flowing through the same
;;; (byte) → outcome protocol as the rest of the keystroke pipeline, rather than
;;; as an ad hoc mutation buried inside the ground-state cond.

(defun %copy-mode-accumulate-digit (byte)
  "Fold BYTE into *copy-mode-prefix* when it continues a numeric prefix.
   Returns NIL when BYTE was consumed as a prefix digit (caller should wait for
   the next byte).  Returns the resolved repeat COUNT (>= 1) and resets
   *copy-mode-prefix* to 0 when BYTE is not a prefix digit — i.e. when the
   accumulated count is ready to be applied to a navigation command.
   '0' with prefix=0 is NOT accumulated (vi convention: bare 0 = beginning of
   line, only 1-9 or a non-zero prefix followed by 0 continue the prefix)."
  (if (and (>= byte +byte-digit-0+) (<= byte +byte-digit-9+)
           (or (> byte +byte-digit-0+) (plusp *copy-mode-prefix*)))
      (progn
        (setf *copy-mode-prefix*
              (+ (* *copy-mode-prefix* 10) (- byte +byte-digit-0+)))
        nil)
      (let ((count (max 1 *copy-mode-prefix*)))
        (setf *copy-mode-prefix* 0)
        count)))

(defparameter +copy-mode-char-argument-handlers+
  '((:copy-mode-jump-forward . copy-mode-jump-forward)
    (:copy-mode-jump-backward . copy-mode-jump-backward)
    (:copy-mode-jump-to . copy-mode-jump-to)
    (:copy-mode-jump-to-backward . copy-mode-jump-to-backward))
  "Copy-mode key-table commands that consume the next byte as a character argument.")

(defun %copy-mode-char-argument-handler (entry)
  "Return the character-argument handler function for ENTRY, or NIL."
  (let ((handler (cdr (assoc (key-table-command entry)
                             +copy-mode-char-argument-handlers+))))
    (and handler (symbol-function handler))))

(defun %copy-mode-char-argument-continuation (screen handler count)
  "Return a CPS continuation that applies HANDLER to the next input byte."
  (lambda (_ignored-session byte2)
    (declare (ignore _ignored-session))
    (loop repeat count
          do (funcall handler screen (code-char byte2)))
    (setf *dirty* t)
    (%ground-values)))

(defun %run-copy-mode-key-table-entry (session byte count)
  "Resolve BYTE against the active copy-mode key table and run the binding.
   Control bytes and single-byte special keys are probed by their canonical
   tmux name (\"C-b\", \"Enter\", \"BSpace\", ...), matching keys stored by
   the key-binding table.  Character-argument commands return a continuation
   that consumes the next byte; otherwise COUNT repeats entries marked
   repeatable by the key-table data and non-repeatable entries run once."
  (let ((entry (%key-table-entry-by-candidates
                (%active-copy-mode-table)
                (%single-byte-key-candidates byte))))
    (when entry
      (let ((char-handler (%copy-mode-char-argument-handler entry)))
        (if char-handler
            (return-from %run-copy-mode-key-table-entry
              (%copy-mode-char-argument-continuation (%active-screen session)
                                                     char-handler
                                                     count))
            (loop repeat (if (key-table-repeatable-p entry) count 1)
                  do (%run-key-table-binding session entry byte))))))
  nil)

(defun %dispatch-copy-mode-ground-byte (session byte)
  "Handle one BYTE of unprefixed copy-mode navigation from ground state.
   Copy mode has its own active table, so ordinary bytes are resolved there.
   Numeric prefix digits accumulate via
   %copy-mode-accumulate-digit; once a non-digit byte resolves the count, the
   byte is resolved via %run-copy-mode-key-table-entry.  Returns
   (values NIL #'%GROUND-INPUT-STATE)."
  (let ((screen (%active-screen session)))
    (when screen
      (let ((count (%copy-mode-accumulate-digit byte)))
        (when count
          (let ((new-state (%run-copy-mode-key-table-entry session byte count)))
            (when new-state
              (setf *dirty* t)
              (return-from %dispatch-copy-mode-ground-byte
                (values nil new-state))))))))
  (setf *dirty* t)
  (values nil #'%ground-input-state))
