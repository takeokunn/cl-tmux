(in-package #:cl-tmux)

;;; ── Mouse escape parsing ───────────────────────────────────────────────────
;;;
;;; Escape-sequence detection for X10 and SGR mouse input lives here so the
;;; main escape decoder can stay focused on the control-flow around dispatch.

(defun %parse-sgr-mouse (buffer length)
  "Parse an SGR mouse sequence from BUFFER (of LENGTH bytes).
   Expected: ESC [ < Pb ; Px ; Py M|m
   Returns (values btn col row release-p) on success, or (values nil nil nil nil) on failure.
   Coordinates in BUFFER are 1-based; returned col/row are 0-based."
  ;; Minimum: ESC [ < D ; D ; D M = 9 bytes
  (when (and (>= length 9)
             (= (aref buffer 0) +byte-esc+)
             (= (aref buffer 1) +byte-csi-bracket+)
             (= (aref buffer 2) +byte-sgr-lt+))
    (let* ((parameter-string (map 'string #'code-char (subseq buffer 3 length)))
           (final-char        (char parameter-string (1- (length parameter-string))))
           (release-p         (char= final-char #\m))
           (params-str        (subseq parameter-string 0 (1- (length parameter-string))))
           (parts             (loop for start = 0 then (1+ semi)
                                    for semi  = (position #\; params-str :start start)
                                    collect (subseq params-str start (or semi (length params-str)))
                                    while semi)))
      (when (= (length parts) 3)
        (let ((btn (parse-integer (first  parts) :junk-allowed t))
              (col (parse-integer (second parts) :junk-allowed t))
              (row (parse-integer (third  parts) :junk-allowed t)))
          (when (and (integerp btn) (integerp col) (integerp row))
            ;; SGR coords are 1-based; convert to 0-based
            (values btn (1- col) (1- row) release-p)))))))

(defun %sgr-mouse-sequence-p (buffer length)
  "True when BUFFER looks like the start of an SGR mouse sequence: ESC [ <."
  (and (>= length 3)
       (= (aref buffer 0) +byte-esc+)
       (= (aref buffer 1) +byte-csi-bracket+)
       (= (aref buffer 2) +byte-sgr-lt+)))

(defun %sgr-mouse-terminated-p (buffer length)
  "True when BUFFER ends with 'M' (press) or 'm' (release) — SGR mouse final byte."
  (when (> length 3)
    (let ((last-byte (aref buffer (1- length))))
      (or (= last-byte +byte-ascii-m+)
          (= last-byte +byte-sgr-release+)))))

(defun %handle-escape-x10-mouse (session buffer)
  "Decode a complete 6-byte X10 mouse sequence from BUFFER and dispatch it.
   Returns (%ground-values) always."
  (let* ((raw-btn   (aref buffer 3))
         (raw-col   (aref buffer 4))
         (raw-row   (aref buffer 5))
         ;; X10 encoding: btn+32, col/row+33 (1-based → subtract 1 for 0-based)
         (btn       (- raw-btn 32))
         (col       (- raw-col 33))
         (row       (- raw-row 33))
         (release-p (= raw-btn (+ +mouse-btn-release-x10+ 32))))  ; btn 3+32=35 = release in X10
    (%dispatch-mouse-event session btn col row release-p))
  (%ground-values))

(defun %handle-escape-sgr-mouse (session buffer length)
  "Dispatch a completed SGR mouse sequence from BUFFER (LENGTH bytes).
   Returns (%ground-values) always."
  (multiple-value-bind (btn col row release-p)
      (%parse-sgr-mouse buffer length)
    (when btn
      (%dispatch-mouse-event session btn col row release-p)))
  (%ground-values))
