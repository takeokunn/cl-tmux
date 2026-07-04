(in-package #:cl-tmux)

;;;; Prefix-table CSI/SS3 continuation.

(defun %csi-1-semi-prefix-p (buffer)
  "T when BUFFER[1..3] is the ESC [ 1 ; modifier-key prefix."
  (and (= (aref buffer 1) +byte-csi-bracket+)
       (= (aref buffer 2) +byte-csi-param-1+)
       (= (aref buffer 3) +byte-csi-semi+)))

(defun %prefix-ss3-introducer-p (buffer length)
  "T for the 2-byte SS3 introducer ESC O, still awaiting its final byte."
  (and (= length 2) (= (aref buffer 1) +byte-ss3-o+)))

(defun %prefix-ss3-final-p (buffer length)
  "T for a complete 3-byte SS3 sequence ESC O <final>."
  (and (= length 3) (= (aref buffer 1) +byte-ss3-o+)))

(defun %prefix-tilde-key-p (buffer length)
  "T for a complete function/navigation key ESC [ <digits> ~."
  (and (>= length 4) (= (aref buffer 1) +byte-csi-bracket+)
       (<= +byte-digit-0+ (aref buffer 2) +byte-digit-9+)
       (= (aref buffer (1- length)) +byte-tilde+)))

(defun %prefix-3byte-csi-p (buffer length)
  "T for a complete 3-byte CSI sequence ESC [ FINAL."
  (and (= length 3) (= (aref buffer 1) +byte-csi-bracket+)))

(defun %prefix-modifier-in-progress-p (buffer length)
  "T while accumulating ESC [ 1 ; [MOD] toward a 6-byte modifier sequence."
  (and (<= 4 length 5) (%csi-1-semi-prefix-p buffer)))

(defun %prefix-6byte-modifier-p (buffer length)
  "T for a complete 6-byte modifier CSI sequence ESC [ 1 ; MOD FINAL."
  (and (= length 6) (%csi-1-semi-prefix-p buffer)))

(defun %prefix-2byte-meta-p (buffer length)
  "T for a 2-byte non-CSI sequence ESC <key>."
  (and (= length 2) (/= (aref buffer 1) +byte-csi-bracket+)))

(defun %handle-ss3-introducer-after-prefix (session buffer)
  "Defer one more byte for SS3 so ESC O P/Q/R/S/H/F resolves as a unit."
  (values nil (%make-prefix-csi-k session buffer)))

(defun %handle-ss3-after-prefix (session buffer)
  "Resolve a complete SS3 sequence ESC O <final> against the prefix table."
  (let ((key (%ss3-key-name (aref buffer 2))))
    (%prefix-string-entry-result
     (and key (%run-bound-string-key session +table-prefix+ key)))))

(defun %handle-tilde-key-after-prefix (session buffer length)
  "Resolve a complete ESC [ <digits> ~ key against the prefix table."
  (let ((key (%csi-tilde-key buffer length)))
    (%prefix-string-entry-result
     (and key (%run-bound-string-key session +table-prefix+ key)))))

(defun %handle-3byte-csi-after-prefix (session buffer)
  "Resolve a complete 3-byte CSI sequence ESC [ FINAL after prefix."
  (let ((final-byte (aref buffer 2)))
    (if (<= +byte-digit-0+ final-byte +byte-digit-9+)
        (values nil (%make-prefix-csi-k session buffer))
        (let* ((name    (%arrow-final-name final-byte))
               (command (%prefix-csi-arrow-cmd final-byte))
               (entry   (%run-bound-string-key session +table-prefix+ name)))
          (unless entry
            (when command (dispatch-command session command nil)))
          (%prefix-string-entry-result entry)))))

(defun %handle-modifier-in-progress-after-prefix (session buffer)
  "Keep accumulating ESC [ 1 ; [MOD] toward the final modifier letter."
  (values nil (%make-prefix-csi-k session buffer)))

(defun %handle-6byte-modifier-after-prefix (session buffer)
  "Resolve a complete 6-byte modifier CSI sequence after prefix."
  (let ((entry (%dispatch-modifier-arrow session (aref buffer 4) (aref buffer 5))))
    (setf *dirty* t)
    (%prefix-string-entry-result entry)))

(defun %handle-2byte-meta-after-prefix (session buffer)
  "Resolve a 2-byte prefix meta chord against `bind M-<key>`."
  (%prefix-string-entry-result
   (%run-bound-string-key session +table-prefix+
                          (%meta-key-name (aref buffer 1)))))

(defun %handle-overflow-after-prefix ()
  "Discard an unrecognised full prefix escape buffer and return to ground."
  (values nil #'%ground-input-state))

(defun %handle-still-accumulating-after-prefix (session buffer)
  "Keep waiting for the next byte of an incomplete prefix escape sequence."
  (values nil (%make-prefix-csi-k session buffer)))

(defun %make-prefix-csi-k (session buffer)
  "CPS continuation for post-prefix ESC [ / ESC O sequences."
  (lambda (_ignored-session byte)
    (declare (ignore _ignored-session))
    (vector-push-extend byte buffer)
    (setf *esc-accum-buffer* buffer)
    (let ((length (fill-pointer buffer)))
      (cond
        ((%prefix-ss3-introducer-p buffer length)
         (%handle-ss3-introducer-after-prefix session buffer))
        ((%prefix-ss3-final-p buffer length)
         (%handle-ss3-after-prefix session buffer))
        ((%prefix-tilde-key-p buffer length)
         (%handle-tilde-key-after-prefix session buffer length))
        ((%prefix-3byte-csi-p buffer length)
         (%handle-3byte-csi-after-prefix session buffer))
        ((%prefix-modifier-in-progress-p buffer length)
         (%handle-modifier-in-progress-after-prefix session buffer))
        ((%prefix-6byte-modifier-p buffer length)
         (%handle-6byte-modifier-after-prefix session buffer))
        ((%prefix-2byte-meta-p buffer length)
         (%handle-2byte-meta-after-prefix session buffer))
        ((>= length 6)
         (%handle-overflow-after-prefix))
        (t (%handle-still-accumulating-after-prefix session buffer))))))
