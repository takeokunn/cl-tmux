(in-package #:cl-tmux)

;; Forward declarations for the mouse escape parser split into
;; events-keystroke-escape-mouse.lisp.
(declaim (ftype function
                %handle-escape-x10-mouse
                %handle-escape-sgr-mouse
                %parse-sgr-mouse
                %sgr-mouse-sequence-p
                %sgr-mouse-terminated-p))

(defun %csi-u-terminated-p (buffer length)
  "True when BUFFER holds a complete CSI-u sequence ESC [ <digits/;> u — i.e. the
   final byte is 'u' and every parameter byte between '[' and 'u' is a digit or ';'.
   The all-digit/semicolon middle excludes mouse (M / <) and arrow/function-key
   finals, so only genuine extended-keys sequences match."
  (and (>= length 4)
       (= (aref buffer 1) +byte-csi-bracket+)
       (= (aref buffer (1- length)) +byte-ascii-u+)
       (loop for i from 2 below (1- length)
             for b = (aref buffer i)
             always (or (<= +byte-digit-0+ b +byte-digit-9+)
                        (= b +byte-csi-semi+)))))

(defun %csi-u-accumulating-p (buffer length)
  "True when BUFFER is the in-progress prefix of a CSI-u sequence: ESC [ <digit>
   followed only by digits/semicolons, not yet terminated, and under the length
   bound (16 — a max-codepoint chord ESC [ 1114111 ; 8 u is 12 bytes).  The leading
   digit distinguishes it from mouse (buf[2] = M / <) and arrow (buf[2] a letter)
   CSI sequences, so accumulation defers their premature forwarding until the 'u'
   terminator (or a non-CSI-u byte) arrives."
  (and (>= length 3)
       (< length 16)
       (= (aref buffer 1) +byte-csi-bracket+)
       (<= +byte-digit-0+ (aref buffer 2) +byte-digit-9+)
       (let ((last (aref buffer (1- length))))
         (or (<= +byte-digit-0+ last +byte-digit-9+)
             (= last +byte-csi-semi+)))))

;;; ── CPS return helpers ───────────────────────────────────────────────────────

(defun %ground-values ()
  "The standard CPS return for 'sequence consumed, reset to ground state':
   (values NIL #'%ground-input-state).  Named so call sites are self-documenting."
  (values nil #'%ground-input-state))

(defun %prompt-escape-input-continue (buffer)
  (values nil (make-prompt-escape-input-k buffer)))

(defun %escape-input-continue (session buffer)
  (values nil (make-escape-input-k session buffer)))

(defun %handle-escape-csi-u (session buffer length)
  (multiple-value-bind (codepoint mod-value) (%csi-u-parse-params buffer length)
    (let ((key (and codepoint (%csi-u-key-name codepoint mod-value))))
      (cond
        ((null key)
         (unless (%copy-mode-active-p session)
           (%forward-octets-synchronized session (subseq buffer 0 length))))
        ((%try-bound-string-key-copy-mode-then-root session key))
        (t
         (unless (%copy-mode-active-p session)
           (%forward-octets-synchronized session (subseq buffer 0 length))))))))

(defun %escape-ss3-introducer-p (buffer length)
  (and (= length 2)
       (= (aref buffer 1) +byte-ss3-o+)))

(defun %escape-ss3-complete-p (buffer length)
  (and (= length 3)
       (= (aref buffer 1) +byte-ss3-o+)))

(defun %escape-x10-mouse-complete-p (buffer length)
  (and (= length 6)
       (= (aref buffer 1) +byte-csi-bracket+)
       (= (aref buffer 2) +byte-ascii-m+)))

(defun %escape-x10-mouse-accumulating-p (buffer length)
  (and (>= length 3)
       (= (aref buffer 1) +byte-csi-bracket+)
       (= (aref buffer 2) +byte-ascii-m+)
       (< length 6)))

(defun %escape-focus-change-p (buffer length)
  (and (= length 3)
       (= (aref buffer 1) +byte-csi-bracket+)
       (or (= (aref buffer 2) +byte-focus-in+)
           (= (aref buffer 2) +byte-focus-out+))))

(defun %escape-csi-3byte-p (buffer length)
  (and (= length 3)
       (= (aref buffer 1) +byte-csi-bracket+)
       (/= (aref buffer 2) +byte-ascii-m+)
       (/= (aref buffer 2) +byte-sgr-lt+)))

(defun %escape-csi-tilde-p (buffer length)
  (and (>= length 4)
       (= (aref buffer 1) +byte-csi-bracket+)
       (<= +byte-digit-0+ (aref buffer 2) +byte-digit-9+)
       (= (aref buffer (1- length)) +byte-tilde+)))

(defun %escape-modifier-arrow-accumulating-p (buffer length)
  (and (>= length 4)
       (<= length 5)
       (= (aref buffer 1) +byte-csi-bracket+)
       (= (aref buffer 2) +byte-csi-param-1+)
       (= (aref buffer 3) +byte-csi-semi+)))

(defun %escape-modifier-arrow-complete-p (buffer length)
  (and (= length 6)
       (= (aref buffer 1) +byte-csi-bracket+)
       (= (aref buffer 2) +byte-csi-param-1+)
       (= (aref buffer 3) +byte-csi-semi+)))

(defun %escape-digit-leading-csi-accumulating-p (buffer length)
  (and (>= length 4)
       (= (aref buffer 1) +byte-csi-bracket+)
       (<= +byte-digit-0+ (aref buffer 2) +byte-digit-9+)
       (cl-tmux/terminal/parser::csi-final-byte-before-p
        (aref buffer (1- length)))))

(defun %escape-digit-leading-csi-complete-p (buffer length)
  (and (>= length 4)
       (= (aref buffer 1) +byte-csi-bracket+)
       (<= +byte-digit-0+ (aref buffer 2) +byte-digit-9+)
       (cl-tmux/terminal/parser::csi-final-byte-p
        (aref buffer (1- length)))))

(defun %escape-4byte-accumulating-p (buffer length)
  (and (= length 4)
       (= (aref buffer 1) +byte-csi-bracket+)
       (/= (aref buffer 3) +byte-tilde+)))

(defun %escape-two-byte-non-csi-p (buffer length)
  (and (= length 2)
       (/= (aref buffer 1) +byte-csi-bracket+)))

(defun %escape-overflow-p (length)
  (> length 32))

(defun %handle-escape-focus-change (session buffer)
  (%notify-pane-focus (session-active-pane session)
                      (= (aref buffer 2) +byte-focus-in+))
  (%ground-values))

(defun %handle-escape-modifier-arrow (session buffer length)
  (let ((key (%modifier-arrow-key-name (aref buffer 4) (aref buffer 5))))
    (unless (and key (%try-bound-string-key-root-then-copy-mode session key))
      (%forward-unless-copy-mode session buffer length)))
  (%ground-values))

(defun %handle-escape-two-byte-non-csi (session buffer)
  (cond
    ((%copy-mode-active-p session)
     (let* ((byte (aref buffer 1))
            (entry (%key-table-entry-by-candidates
                    (%active-copy-mode-table)
                    (list (%meta-key-name byte)))))
       (if entry
           (%run-key-table-binding session entry nil)
           (let ((screen (%active-screen session)))
             (when screen (copy-mode-clear-selection screen))))
       (setf *dirty* t)))
    ((%try-bound-string-key session +table-root+
                            (%meta-key-name (aref buffer 1))))
    (t
     (%forward-octets-synchronized session (subseq buffer 0 2))))
  (%ground-values))

(defun %handle-escape-overflow (session buffer length)
  (%forward-unless-copy-mode session buffer length)
  (%ground-values))

(defun %escape-input-dispatch-key (buffer length)
  (cond
    ((%escape-ss3-introducer-p buffer length) :ss3-introducer)
    ((%escape-ss3-complete-p buffer length) :ss3-complete)
    ((%escape-x10-mouse-complete-p buffer length) :x10-mouse-complete)
    ((%escape-x10-mouse-accumulating-p buffer length) :x10-mouse-accumulating)
    ((and (%sgr-mouse-sequence-p buffer length)
          (%sgr-mouse-terminated-p buffer length))
     :sgr-mouse-complete)
    ((%sgr-mouse-sequence-p buffer length) :sgr-mouse-accumulating)
    ((%csi-u-terminated-p buffer length) :csi-u-complete)
    ((%csi-u-accumulating-p buffer length) :csi-u-accumulating)
    ((%escape-focus-change-p buffer length) :focus-change)
    ((%escape-csi-3byte-p buffer length) :csi-3byte)
    ((%escape-csi-tilde-p buffer length) :csi-tilde)
    ((%escape-modifier-arrow-accumulating-p buffer length)
     :modifier-arrow-accumulating)
    ((%escape-modifier-arrow-complete-p buffer length)
     :modifier-arrow-complete)
    ((%escape-digit-leading-csi-accumulating-p buffer length)
     :digit-leading-csi-accumulating)
    ((%escape-digit-leading-csi-complete-p buffer length)
     :digit-leading-csi-complete)
    ((%escape-4byte-accumulating-p buffer length) :four-byte-accumulating)
    ((%escape-two-byte-non-csi-p buffer length) :two-byte-non-csi)
    ((%escape-overflow-p length) :overflow)
    (t :continue)))

(defun make-escape-input-k (session buffer)
  "CPS continuation: accumulate an ESC [... sequence one byte at a time.

   X10 mouse: ESC [ M <btn+32> <col+33> <row+33> — 6 bytes total.
     Detected when buf[0]=ESC buf[1]=[ buf[2]=M and we still need 3 more bytes.
     Dispatched via %DISPATCH-MOUSE-EVENT when length reaches 6.

   SGR mouse: ESC [ < Pb ; Px ; Py M|m — variable length, terminated by M or m.
     Detected when buf[2]='<' (60).  Accumulated until final byte M or m arrives.

   Copy-mode 3-byte CSI (ESC [ FINAL): try HANDLE-COPY-MODE-ESCAPE; if not
     handled and not in copy mode, forward to the active pane.

   2-byte non-CSI (ESC X): forward to the active pane.

   Otherwise: keep accumulating."
  ;; SESSION is captured from the make-escape-input-k call; the lambda parameter
  ;; _ignored-session is structurally required by the CPS protocol (SESSION BYTE)
  ;; → values, but is always the same object as the captured SESSION.  We ignore
  ;; the parameter to keep the protocol uniform across all CPS state functions.
  (lambda (_ignored-session byte)
    (declare (ignore _ignored-session))
    (vector-push-extend byte buffer)
    ;; Expose the growing buffer so %flush-esc-if-timed-out can replay the FULL
    ;; partial sequence (e.g. a held ESC O) rather than dropping all but the ESC.
    (setf *esc-accum-buffer* buffer)
    (let ((length (fill-pointer buffer)))
      (case (%escape-input-dispatch-key buffer length)
        (:ss3-introducer (%escape-input-continue session buffer))
        (:ss3-complete (%handle-escape-ss3 session buffer))
        (:x10-mouse-complete (%handle-escape-x10-mouse session buffer))
        (:x10-mouse-accumulating (%escape-input-continue session buffer))
        (:sgr-mouse-complete (%handle-escape-sgr-mouse session buffer length))
        (:sgr-mouse-accumulating (%escape-input-continue session buffer))
        (:csi-u-complete
         (%handle-escape-csi-u session buffer length)
         (%ground-values))
        (:csi-u-accumulating (%escape-input-continue session buffer))
        (:focus-change (%handle-escape-focus-change session buffer))
        (:csi-3byte
         (multiple-value-bind (keep-accumulating next-state)
             (%handle-escape-csi-3byte session buffer)
           (if keep-accumulating
               (%escape-input-continue session buffer)
               (values nil next-state))))
        (:csi-tilde (%handle-escape-csi-tilde session buffer length))
        (:modifier-arrow-accumulating (%escape-input-continue session buffer))
        (:modifier-arrow-complete
         (%handle-escape-modifier-arrow session buffer length))
        (:digit-leading-csi-accumulating
         (%escape-input-continue session buffer))
        (:digit-leading-csi-complete
         (%forward-unless-copy-mode session buffer length)
         (%ground-values))
        (:four-byte-accumulating
         (%forward-unless-copy-mode session buffer length)
         (%ground-values))
        (:two-byte-non-csi (%handle-escape-two-byte-non-csi session buffer))
        (:overflow (%handle-escape-overflow session buffer length))
        (otherwise (%escape-input-continue session buffer)))))
)
