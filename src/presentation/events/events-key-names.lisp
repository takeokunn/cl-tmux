(in-package #:cl-tmux)

;;;; Pure key-name data projections for keyboard event decoding.

;;; One table encodes all three facets of an arrow key:
;;;   arrow_key(byte, "Name", :select-cmd).
;;; %prefix-csi-arrow-cmd and %arrow-final-name are projections of this single
;;; fact table, guaranteeing that adding or renaming an arrow key is a one-line
;;; change and the two functions stay in sync automatically.

(defmacro define-arrow-key-table (&rest specs)
  "Build %PREFIX-CSI-ARROW-CMD and %ARROW-FINAL-NAME from a unified fact table.
   Each SPEC is (final-byte-constant key-name pane-select-command)."
  `(progn
     (defun %prefix-csi-arrow-cmd (final-byte)
       "Map a CSI arrow FINAL-BYTE to a pane-select command keyword, or NIL."
       (cond ,@(mapcar (lambda (spec)
                         `((= final-byte ,(first spec)) ,(third spec)))
                       specs)
             (t nil)))
     (defun %arrow-final-name (final-byte)
       "Canonical tmux key name (\"Up\"/\"Down\"/\"Left\"/\"Right\") for FINAL-BYTE,
        or NIL when not an arrow.  Matches what %parse-key-token stores for
        `bind Up ...` directives, used as key-table lookup keys."
       (cond ,@(mapcar (lambda (spec)
                         `((= final-byte ,(first spec)) ,(second spec)))
                       specs)
             (t nil)))))

(define-arrow-key-table
  (+byte-arrow-up+    "Up"    :select-pane-up)
  (+byte-arrow-down+  "Down"  :select-pane-down)
  (+byte-arrow-right+ "Right" :select-pane-right)
  (+byte-arrow-left+  "Left"  :select-pane-left))

(defun %modifier-prefix (mod-value)
  "Build the canonical C-/M-/S- modifier prefix for a CSI MOD-VALUE.
   MOD-VALUE is 1 + a bitmask where bit0=Shift, bit1=Alt/Meta, bit2=Ctrl.
   Returns \"\" for 1 or any value with no recognised bit."
  (let ((bits (max 0 (- mod-value 1))))
    (concatenate 'string
                 (if (logbitp 2 bits) "C-" "")
                 (if (logbitp 1 bits) "M-" "")
                 (if (logbitp 0 bits) "S-" ""))))

(defun %modifier-arrow-key-name (mod-byte final-byte)
  "Canonical tmux key name for a modifier+arrow CSI sequence.
   MOD-BYTE is the digit byte from ESC [ 1 ; N FINAL.  Returns NIL for a
   non-arrow final byte or a MOD-BYTE carrying no modifier."
  (let ((base   (%arrow-final-name final-byte))
        (prefix (%modifier-prefix (- mod-byte +byte-digit-0+))))
    (when (and base (plusp (length prefix)))
      (concatenate 'string prefix base))))

(defun %csi-u-base-key (codepoint)
  "Base key name for a CSI-u CODEPOINT, or NIL for an unhandled codepoint."
  (case codepoint
    (9   "Tab")
    (13  "Enter")
    (27  "Escape")
    (32  "Space")
    (127 "BSpace")
    (t   (when (<= +byte-first-graphic+ codepoint +byte-last-graphic+)
           (string (code-char codepoint))))))

(defun %csi-u-key-name (codepoint mod-value)
  "Canonical tmux key name for ESC [ CODEPOINT ; MOD-VALUE u."
  (let ((base (%csi-u-base-key codepoint)))
    (when base
      (concatenate 'string (%modifier-prefix mod-value) base))))

(defun %csi-u-parse-params (buffer length)
  "Parse numeric parameters of a u-terminated CSI sequence.
   Returns (values CODEPOINT MOD-VALUE); MOD-VALUE defaults to 1 when omitted."
  (let* ((text      (map 'string #'code-char (subseq buffer 2 (1- length))))
         (semi      (position #\; text))
         (codepoint (if semi
                        (%parse-integer-or-nil text :end semi)
                        (%parse-integer-or-nil text)))
         (mod       (if semi
                        (or (%parse-integer-or-nil text :start (1+ semi)
                                                   :junk-allowed t)
                            1)
                        1)))
    (when codepoint (values codepoint mod))))

(defun %control-byte-key-name (byte)
  "Return a printable base key name for a Ctrl BYTE, or NIL."
  (cond
    ((<= 1 byte 26)
     (string (code-char (+ byte 96))))
    ((<= +byte-esc+ byte 31)
     (string (code-char (+ byte +byte-ctrl-to-upper-offset+))))
    (t nil)))

(defun %meta-key-name (byte)
  "Canonical tmux key name for the Meta/Alt chord that arrives as ESC then BYTE."
  (cond
    ((= byte +byte-space+) "M-Space")
    ((and (> byte +byte-space+) (< byte +byte-del+))
     (concatenate 'string "M-" (string (code-char byte))))
    (t nil)))

(defun %single-byte-key-candidates (byte)
  "Lookup candidates for a single raw input BYTE in the active key table."
  (remove nil
          (list (code-char byte)
                (case byte
                  (9 "Tab")
                  (13 "Enter")
                  (127 "BSpace")
                  (t nil))
                (let ((base (%control-byte-key-name byte)))
                  (and base (concatenate 'string "C-" base))))))
