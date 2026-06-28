(in-package #:cl-tmux/commands)

;;;; send-keys key-name translation, command-string tokeniser,
;;;;  send-keys-to-pane, and shell helpers (run-shell, if-shell).

;;; ── send-keys key-name translation ──────────────────────────────────────────
;;;
;;; tmux's send-keys interprets arguments that name a key (Enter, Tab, Up, C-c,
;;; M-x, F5, ...) and sends that key's byte sequence rather than the literal
;;; text.  *send-key-names* is the named-key table; C-<x> (control) and M-<x>
;;; (meta/alt) are handled algorithmically.  Escape sequences use the normal
;;; (non-application) xterm encodings, matching what send-keys emits by default.

;;; ESC as a single-character string.  defparameter rather than defconstant because
;;; string literals are not EQL-comparable and SBCL would signal a redefinition
;;; error on every fasl reload with defconstant.
(defparameter *escape-string* (string (code-char 27))
  "The ESC character (ASCII 27) as a single-character string.")

(defun %escape-sequence (&rest tail)
  "Build a string beginning with ESC followed by TAIL strings concatenated."
  (apply #'concatenate 'string *escape-string* tail))

(defparameter *send-key-names*
  (list
   ;; whitespace / control
   (cons "Enter"  (string #\Return)) (cons "C-m" (string #\Return))
   (cons "Tab"    (string #\Tab))    (cons "C-i" (string #\Tab))
   (cons "Space"  " ")
   (cons "Escape" *escape-string*)   (cons "Esc" *escape-string*)
   (cons "BSpace" (string (code-char 127)))
   (cons "BTab"   (%escape-sequence "[Z"))
   ;; arrows (normal cursor mode)
   (cons "Up"     (%escape-sequence "[A")) (cons "Down"  (%escape-sequence "[B"))
   (cons "Right"  (%escape-sequence "[C")) (cons "Left"  (%escape-sequence "[D"))
   ;; navigation block
   (cons "Home"     (%escape-sequence "[H")) (cons "End"      (%escape-sequence "[F"))
   (cons "PageUp"   (%escape-sequence "[5~")) (cons "PPage"   (%escape-sequence "[5~"))
   (cons "PageDown" (%escape-sequence "[6~")) (cons "NPage"   (%escape-sequence "[6~"))
   (cons "Insert"   (%escape-sequence "[2~")) (cons "IC"      (%escape-sequence "[2~"))
   (cons "Delete"   (%escape-sequence "[3~")) (cons "DC"      (%escape-sequence "[3~"))
   ;; function keys
   (cons "F1"  (%escape-sequence "OP"))   (cons "F2"  (%escape-sequence "OQ"))
   (cons "F3"  (%escape-sequence "OR"))   (cons "F4"  (%escape-sequence "OS"))
   (cons "F5"  (%escape-sequence "[15~")) (cons "F6"  (%escape-sequence "[17~"))
   (cons "F7"  (%escape-sequence "[18~")) (cons "F8"  (%escape-sequence "[19~"))
   (cons "F9"  (%escape-sequence "[20~")) (cons "F10" (%escape-sequence "[21~"))
   (cons "F11" (%escape-sequence "[23~")) (cons "F12" (%escape-sequence "[24~")))
  "Alist mapping tmux key-name strings to their literal byte sequence (as a
   string whose char-codes are the bytes — all < 128).")

(defparameter *modified-send-keys*
  '(;; :letter keys encode as ESC [ 1 ; <mod> <final-char>
    ;; e.g. C-Up → ESC[1;5A  (mod=5=Ctrl), S-Left → ESC[1;2D (mod=2=Shift)
    ("Up" :letter #\A) ("Down" :letter #\B) ("Right" :letter #\C) ("Left" :letter #\D)
    ("Home" :letter #\H) ("End" :letter #\F)
    ("F1" :letter #\P) ("F2" :letter #\Q) ("F3" :letter #\R) ("F4" :letter #\S)
    ;; :tilde keys encode as ESC [ <param> ; <mod> ~
    ;; e.g. C-F5 → ESC[15;5~  (param=15), S-PageUp → ESC[5;2~ (param=5)
    ("F5" :tilde 15) ("F6" :tilde 17) ("F7" :tilde 18) ("F8" :tilde 19)
    ("F9" :tilde 20) ("F10" :tilde 21) ("F11" :tilde 23) ("F12" :tilde 24)
    ("PageUp" :tilde 5) ("PPage" :tilde 5) ("PageDown" :tilde 6) ("NPage" :tilde 6)
    ("Insert" :tilde 2) ("IC" :tilde 2) ("Delete" :tilde 3) ("DC" :tilde 3))
  "Base special keys that accept a CSI modifier prefix, with their byte-sequence
   shape.  The modifier code is 1+Shift+2*Alt+4*Ctrl (so Ctrl=5, Shift=2, etc.).
   The inverse of the event loop's modifier decoding, so send-keys C-Up
   round-trips with `bind -n C-Up`.")

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

;;; ── Command-string tokeniser ────────────────────────────────────────────────
;;;
;;; tmux command arguments are split shell-style: whitespace separates arguments,
;;; '...' is a literal span, "..." allows backslash escapes, and a bare \\ escapes
;;; the next character.  Adjacent spans join into one argument (foo"bar baz" →
;;; foobar baz).  This is the shared lexer behind multi-argument commands such as
;;; send-keys (and, in future, display-message / if-shell).

(defun %consume-single-quoted (string start length accumulator)
  "Consume a single-quoted literal span from STRING beginning at START.
   Writes characters into ACCUMULATOR stream up to the closing quote.
   Returns the index after the closing quote (or LENGTH when unterminated)."
  (let ((index (1+ start)))         ; skip the opening quote
    (loop while (and (< index length)
                     (char/= (char string index) #\'))
          do (write-char (char string index) accumulator)
             (incf index))
    (if (< index length) (1+ index) index))) ; skip closing quote when present

(defun %consume-double-quoted (string start length accumulator)
  "Consume a double-quoted span from STRING beginning at START.
   Inside double quotes a backslash followed by any character is an escape:
   only the escaped character is written.  Other characters are written verbatim.
   Returns the index after the closing quote (or LENGTH when unterminated)."
  (let ((index (1+ start)))         ; skip the opening quote
    (loop while (and (< index length)
                     (char/= (char string index) #\"))
          do (if (and (char= (char string index) #\\) (< (1+ index) length))
                 (progn (write-char (char string (1+ index)) accumulator)
                        (incf index 2))
                 (progn (write-char (char string index) accumulator)
                        (incf index))))
    (if (< index length) (1+ index) index))) ; skip closing quote when present

(defun %flush-tokenized-argument (accumulator arguments in-arg)
  "Flush ACCUMULATOR into ARGUMENTS when IN-ARG is true.
   Returns two values: the updated ARGUMENTS list and the new IN-ARG state."
  (if in-arg
      (values (cons (get-output-stream-string accumulator) arguments) nil)
      (values arguments in-arg)))

(defun tokenize-command-string (string)
  "Split STRING into a list of argument strings, shell-style.
   Whitespace separates arguments; '...' is a literal span; \"...\" allows \\
   escapes; a bare \\ escapes the next character; adjacent spans concatenate.
   Unterminated quotes are tolerated (consumed to end of string).  An explicitly
   quoted empty token (e.g. '') yields an empty-string argument."
  (let ((arguments   nil)
        (accumulator (make-string-output-stream))
        (in-arg      nil)
        (index       0)
        (length      (length string)))
    (loop while (< index length)
          for character = (char string index)
          do (cond
               ((member character '(#\Space #\Tab))
                (multiple-value-bind (new-arguments new-in-arg)
                    (%flush-tokenized-argument accumulator arguments in-arg)
                  (setf arguments new-arguments
                        in-arg new-in-arg))
                (incf index))
               ((char= character #\')
                (setf in-arg t
                      index (%consume-single-quoted string index length accumulator)))
               ((char= character #\")
                (setf in-arg t
                      index (%consume-double-quoted string index length accumulator)))
               ((and (char= character #\\) (< (1+ index) length))
                (setf in-arg t)
                (write-char (char string (1+ index)) accumulator)
                (incf index 2))
               (t
                (setf in-arg t)
                (write-char character accumulator)
                (incf index))))
    (multiple-value-bind (new-arguments new-in-arg)
        (%flush-tokenized-argument accumulator arguments in-arg)
      (declare (ignore new-in-arg))
      (setf arguments new-arguments))
    (nreverse arguments)))

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

;;; ── send-keys-to-pane ───────────────────────────────────────────────────────

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

;;; ── Shell ──────────────────────────────────────────────────────────────────
;;;
;;; run_shell(cmd)            :- subprocess(cmd, timeout=30, output=string).
;;; if_shell(cmd, then, else) :- subprocess(cmd), exit_code=0 -> then ; else.
;;;
;;; Both run-shell and if-shell accept a :timeout keyword (seconds, default
;;; +shell-command-timeout+).  Synchronous callers are bounded by both the Lisp
;;; control path and the subprocess itself; background callers return
;;; immediately but the worker still gives the subprocess a bounded lifetime.
;;;
;;; uiop:run-program is used instead of sb-ext:run-program so the code is
;;; portable across all ASDF-supported implementations.
;;;
;;; if-shell is exported and wired to the :if-shell dispatch key in dispatch.lisp
;;; so it is reachable from the prefix-key handler.

(defconstant +shell-command-timeout+ 30
  "Default wall-clock timeout, in seconds, for shell subprocesses.")

(defun %run-with-timeout (thunk timeout-seconds)
  "Run THUNK in a fresh thread; join it up to TIMEOUT-SECONDS.
   Returns (funcall thunk) result or NIL if the timeout expires."
  (handler-case
      (bt:with-timeout (timeout-seconds)
        (funcall thunk))
    (bt:timeout () nil)))

(defmacro with-shell-timeout ((shell-var timeout) &body body)
  "Bind SHELL-VAR to the active shell binary and run BODY with a TIMEOUT (seconds).
   TIMEOUT is evaluated at macro-expansion call time and passed directly to
   %RUN-WITH-TIMEOUT.  Returns the result of BODY or NIL when the timeout fires."
  `(%run-with-timeout
     (lambda ()
       (let ((,shell-var (or *default-shell* "/bin/sh")))
         ,@body))
     ,timeout))

(defun %run-shell-program (shell command &key output timeout)
  "Run COMMAND through SHELL with an explicit subprocess TIMEOUT."
  (uiop:run-program (list shell "-c" command)
                    :output output
                    :ignore-error-status t
                    :timeout timeout))

(defun run-shell (command &key background (timeout +shell-command-timeout+))
  "Run COMMAND in a subshell.  Returns the output string (stdout) when BACKGROUND
   is nil, or T immediately when BACKGROUND is T.
   Uses *default-shell* for the shell binary.
   TIMEOUT (seconds, default +shell-command-timeout+) limits how long the
   subprocess may run; when the synchronous limit is exceeded NIL is returned."
  (if background
      (progn
        (bt:make-thread
          (lambda ()
            (let ((shell (or *default-shell* "/bin/sh")))
              (ignore-errors
                (%run-shell-program shell command
                                    :output nil
                                    :timeout timeout))))
          :name "shell-bg")
        t)
      (with-shell-timeout (shell timeout)
        (%run-shell-program shell command
                            :output :string
                            :timeout timeout))))

(defun if-shell (command then-fn &key else-fn (timeout +shell-command-timeout+))
  "Run COMMAND; call THEN-FN if exit code is 0, ELSE-FN otherwise.
   THEN-FN and ELSE-FN are zero-argument functions (keyword arguments).
   TIMEOUT (seconds, default +shell-command-timeout+) limits how long the
   command may run; when the limit is exceeded ELSE-FN is called."
  (let ((exit-code
          (with-shell-timeout (shell timeout)
            (multiple-value-bind (output error-output code)
                (%run-shell-program shell command
                                    :output nil
                                    :timeout timeout)
              (declare (ignore output error-output))
              code))))
    (if (and exit-code (zerop exit-code))
        (when then-fn (funcall then-fn))
        (when else-fn (funcall else-fn)))))
