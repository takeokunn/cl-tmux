(in-package #:cl-tmux/commands)

;;;; send-keys key-name translation and send-keys-to-pane.
;;;;
;;;; Translates tmux key names (Enter, Tab, Up, C-c, M-x, F5, C-Up, S-F5, ...)
;;;; to their byte sequences, using the data tables in commands-keys-data.lisp
;;;; and the shell-style tokeniser in commands-tokenizer.lisp.

(defun %split-key-modifiers (name)
  "Strip leading C-/M-/S- modifier prefixes from NAME.  Returns (values MOD-VALUE
   BASE): MOD-VALUE is the CSI modifier code (1 + Shift + 2·Alt + 4·Ctrl), 1 when
   no modifier prefix is present; BASE is the remaining key name."
  (let ((bits 0) (i 0) (len (length name)))
    (loop while (and (<= (+ i 2) len) (char= (char name (1+ i)) #\-))
          for m = (char-upcase (char name i))
          do (case m
               (#\C (setf bits (logior bits 4)))
               (#\M (setf bits (logior bits 2)))
               (#\S (setf bits (logior bits 1)))
               (otherwise (return)))
             (incf i 2))
    (values (1+ bits) (subseq name i))))

(defun %modified-special-key-string (name)
  "Escape string for a modified special key NAME (C-Up → ESC[1;5A, S-F5 →
   ESC[15;2~, C-M-Left → ESC[1;7D), or NIL when NAME is not a modified special
   key.  Modifiers map through %split-key-modifiers; the base must be a key in
   *modified-send-keys* and at least one modifier must be present."
  (multiple-value-bind (mod-value base) (%split-key-modifiers name)
    (when (> mod-value 1)
      (let ((entry (assoc base *modified-send-keys* :test #'string=)))
        (when entry
          (%escape-sequence
           (ecase (second entry)
             (:letter (format nil "[1;~D~C" mod-value (third entry)))
             (:tilde  (format nil "[~D;~D~~" (third entry) mod-value)))))))))

(defun %key-name-to-bytes (name)
  "Return the octet vector for a tmux key NAME (Enter, Tab, Up, C-c, M-x, F5,
   C-Up, S-F5...), or NIL when NAME is not a recognised key.
   C-<char> → the control byte (logand char #x1f); M-<char> → ESC then <char>;
   <mods>-<special> → the modified CSI sequence (see %modified-special-key-string)."
  (let ((entry    (assoc name *send-key-names* :test #'string=))
        (modified (%modified-special-key-string name)))
    (cond
      (entry
       (babel:string-to-octets (cdr entry) :encoding :utf-8))
      ;; Modified special key (C-Up, S-F5, C-M-Left) before the C-/M-<char> paths.
      (modified
       (babel:string-to-octets modified :encoding :utf-8))
      ;; C-<char>: control byte.  C-a..C-z → 1..26, C-@ → 0, C-[ → 27, ...
      ;; +ctrl-mask+ = #x1f (exported from cl-tmux/config).
      ((and (= (length name) 3) (string= (subseq name 0 2) "C-"))
       (make-array 1 :element-type '(unsigned-byte 8)
                     :initial-element (logand (char-code (char-upcase (char name 2)))
                                              +ctrl-mask+)))
      ;; M-<char>: ESC followed by the character (Alt/Meta).
      ((and (= (length name) 3) (string= (subseq name 0 2) "M-"))
       (babel:string-to-octets
        (concatenate 'string (string (code-char 27)) (subseq name 2))
        :encoding :utf-8))
      (t nil))))

(defun %translate-send-keys (string)
  "Bytes that send-keys should write for the argument string STRING.  STRING is
   tokenised shell-style; each argument naming a tmux key (Enter, C-c, Up, F5,
   M-x, ...) contributes that key's byte sequence and every other argument
   contributes its literal UTF-8 bytes.  Matches tmux: spaces separate arguments
   unless quoted — `send-keys echo hi` sends \"echohi\", whereas
   `send-keys \"echo hi\" Enter` sends \"echo hi\" then CR."
  (let ((args (tokenize-command-string string)))
    (if (null args)
        (babel:string-to-octets string :encoding :utf-8)
        (apply #'concatenate '(vector (unsigned-byte 8))
               (mapcar (lambda (arg)
                         (or (%key-name-to-bytes arg)
                             (babel:string-to-octets arg :encoding :utf-8)))
                       args)))))

(defun send-keys-to-pane (pane string &key literal)
  "Write STRING to PANE's PTY.  STRING is parsed as send-keys arguments: each
   argument naming a tmux key (Enter, Tab, C-c, Up, F5, M-x, ...) is translated
   to its byte sequence, and other arguments are sent as literal UTF-8 text.
   When LITERAL is true (send-keys -l), STRING is written as raw UTF-8 bytes
   with NO key-name interpretation.
   No-op when PANE has no open PTY (fd <= -1)."
  (when (and pane (> (pane-fd pane) -1))
    (pty-write (pane-fd pane)
               (if literal
                   (babel:string-to-octets string :encoding :utf-8)
                   (%translate-send-keys string)))))
