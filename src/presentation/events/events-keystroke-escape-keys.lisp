(in-package #:cl-tmux)

(defmacro define-key-lookup-table (fn-name param-var doc &rest specs)
  "Generate a key-lookup function FN-NAME(PARAM-VAR) → key-name-string | nil.
   Integer specs dispatch via EQL; character specs use CHAR-CODE so the generated
   COND stays homogeneous (integer comparisons throughout)."
  `(defun ,fn-name (,param-var)
     ,doc
     (cond ,@(mapcar (lambda (spec)
                       `((eql ,param-var ,(let ((k (first spec)))
                                            (if (characterp k) (char-code k) k)))
                         ,(second spec)))
                     specs)
           (t nil))))

(defun %csi-tilde-digits (buffer start end)
  "Parse BUFFER[START..END) as a base-10 integer, or NIL if any byte is invalid."
  (when (< start end)
    (let ((value 0))
      (loop for i from start below end
            for byte = (aref buffer i)
            if (<= +byte-digit-0+ byte +byte-digit-9+)
              do (setf value (+ (* value 10) (- byte +byte-digit-0+)))
            else return nil
            finally (return value)))))

(defun %csi-tilde-parse (buffer length)
  "Parse an ESC [ <param> [ ; <mod> ] ~ sequence and return its parameter data."
  (let ((semi (position +byte-csi-semi+ buffer :start 2 :end (1- length))))
    (if semi
        (let ((param (%csi-tilde-digits buffer 2 semi))
              (mod   (%csi-tilde-digits buffer (1+ semi) (1- length))))
          (and param mod (values param mod)))
        (let ((param (%csi-tilde-digits buffer 2 (1- length))))
          (when param (values param 1))))))

(defun %csi-tilde-key (buffer length)
  "Return the canonical key name for an ESC [ <param> [;<mod>] ~ sequence."
  (multiple-value-bind (param mod) (%csi-tilde-parse buffer length)
    (let ((base (and param (%csi-tilde-key-name param))))
      (when base (concatenate 'string (%modifier-prefix (or mod 1)) base)))))

(define-key-lookup-table %csi-tilde-key-name param
  "Map the numeric PARAM of an ESC [ <param> ~ sequence to its canonical tmux key
   name, or NIL when PARAM is not a recognised navigation/function key.  Covers
   Home/End/Insert/Delete, PageUp/PageDown, and the vt-style F1-F12 finals."
  (1 "Home") (7 "Home")
  (2 "Insert")
  (3 "Delete")
  (4 "End") (8 "End")
  (5 "PageUp") (6 "PageDown")
  (11 "F1") (12 "F2") (13 "F3") (14 "F4")
  (15 "F5") (17 "F6") (18 "F7") (19 "F8")
  (20 "F9") (21 "F10") (23 "F11") (24 "F12"))

(define-key-lookup-table %ss3-key-name final-byte
  "Map the final byte of an SS3 sequence ESC O <final> to its canonical tmux key
   name, or NIL when it is not a recognised bindable key.  Covers F1-F4
   (ESC O P/Q/R/S, the xterm/screen encoding not carried by the ESC[N~ path)
   and Home/End (ESC O H/F)."
  (#\P "F1") (#\Q "F2") (#\R "F3") (#\S "F4")
  (#\H "Home") (#\F "End"))

(defun %forward-unless-copy-mode (session buffer length)
  "Forward BUFFER[0..LENGTH) to the active pane unless copy mode is active."
  (unless (%copy-mode-active-p session)
    (%forward-octets-synchronized session (subseq buffer 0 length))))

(defun %handle-escape-csi-tilde (session buffer length)
  "Handle a complete ESC [ <param> ~ sequence at root."
  (let ((key (%csi-tilde-key buffer length)))
    (cond
      ((and key (%try-bound-string-key-root-then-copy-mode session key)))
      ((and (member key '("PageUp" "PageDown") :test #'string=)
            (%copy-mode-active-p session))
       (let ((screen (%active-screen session)))
         (when screen
           (copy-mode-scroll screen (if (string= key "PageUp")
                                        (screen-height screen)
                                        (- (screen-height screen))))
           (setf *dirty* t))))
      (t
       (%forward-unless-copy-mode session buffer length))))
  (%ground-values))

(defun %handle-escape-ss3 (session buffer)
  "Handle a complete 3-byte SS3 sequence ESC O <final> from BUFFER."
  (let ((key (%ss3-key-name (aref buffer 2))))
    (unless (and key (%try-bound-string-key session +table-root+ key))
      (%forward-unless-copy-mode session buffer 3)))
  (%ground-values))

(defun %handle-escape-csi-3byte (session buffer)
  "Handle a 3-byte CSI sequence ESC [ FINAL from BUFFER."
  (let ((third-byte (aref buffer 2)))
    (if (and (>= third-byte +byte-digit-0+) (<= third-byte +byte-digit-9+))
        (values t nil)
        (progn
          (unless (handle-copy-mode-escape session buffer)
            (unless (%copy-mode-active-p session)
              (let* ((screen   (%active-screen session))
                     (app-keys (and screen (screen-app-cursor-keys screen)))
                     (ss3-seq  (and app-keys (%arrow-final-to-ss3-bytes third-byte))))
                (if ss3-seq
                    (%forward-octets-synchronized session ss3-seq)
                    (%forward-octets-synchronized session (subseq buffer 0 3))))))
          (%ground-values)))))
