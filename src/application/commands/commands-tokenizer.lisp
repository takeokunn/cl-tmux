(in-package #:cl-tmux/commands)

;;;; Command-string tokeniser: shell-style quote/escape splitting shared by
;;;; multi-argument commands such as send-keys (and, in future,
;;;; display-message / if-shell).
;;;;
;;;; tmux command arguments are split shell-style: whitespace separates arguments,
;;;; '...' is a literal span, "..." allows backslash escapes, and a bare \\ escapes
;;;; the next character.  Adjacent spans join into one argument (foo"bar baz" →
;;;; foobar baz).
;;;;
;;;; Built on cl-parser-kit's tokenizer framework (cl-parser-kit:tokenizer /
;;;; token-rule / tokenize-string): a skipped whitespace rule plus one custom
;;;; :argument rule whose matcher runs the quote/escape-joining scan below and
;;;; reports how many source characters it consumed, exactly the contract
;;;; cl-parser-kit:make-token-rule expects.  That scan is the one piece with
;;;; no off-the-shelf cl-parser-kit rule -- quotes and escapes don't open a
;;;; new token the way they would in a typical language lexer, they extend
;;;; the CURRENT argument -- so it stays hand-written; what cl-parser-kit
;;;; contributes is the rule/skip composition, span tracking, and the same
;;;; resource-limit guards (*maximum-tokenizer-source-length* et al.) every
;;;; other cl-tmux tokenizer built on it gets for free.

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

(defun %argument-token-matcher (source index)
  "cl-parser-kit token-rule matcher for one tmux-style argument: a maximal run
   of plain characters, \\-escaped characters, and '...'/\"...\" spans, joined
   with no separator (foo\"bar baz\" → one token \"foobar baz\").  Stops before
   whitespace or the end of SOURCE.  Returns (values ok consumed-length text
   value) per the cl-parser-kit:make-token-rule matcher contract; (values nil
   index) when INDEX is already whitespace or end of input."
  (let ((length       (length source))
        (accumulator  (make-string-output-stream))
        (position     index))
    (loop while (and (< position length)
                     (not (member (char source position) '(#\Space #\Tab))))
          do (let ((character (char source position)))
               (cond
                 ((char= character #\')
                  (setf position (%consume-single-quoted source position length accumulator)))
                 ((char= character #\")
                  (setf position (%consume-double-quoted source position length accumulator)))
                 ((and (char= character #\\) (< (1+ position) length))
                  (write-char (char source (1+ position)) accumulator)
                  (incf position 2))
                 (t
                  (write-char character accumulator)
                  (incf position)))))
    (if (> position index)
        (let ((text (get-output-stream-string accumulator)))
          (values t (- position index) text text))
        (values nil index))))

(defparameter *command-string-tokenizer*
  (cl-parser-kit:make-tokenizer
   :rules (list
           ;; Only space/tab separate arguments (not every char-whitespace-p
           ;; class member), matching the original hand-rolled scanner: any
           ;; other whitespace, e.g. a literal newline, stays inside its
           ;; argument.
           (cl-parser-kit:make-predicate-rule
            :whitespace (lambda (ch) (member ch '(#\Space #\Tab))) :skip-p t)
           (cl-parser-kit:make-token-rule :type :argument
                                          :matcher #'%argument-token-matcher)))
  "Shared cl-parser-kit tokenizer for tokenize-command-string.  Stateless and
   reusable across calls: every rule matcher is a pure function of (source
   index).")

(defun tokenize-command-string (string)
  "Split STRING into a list of argument strings, shell-style.
   Whitespace separates arguments; '...' is a literal span; \"...\" allows \\
   escapes; a bare \\ escapes the next character; adjacent spans concatenate.
   Unterminated quotes are tolerated (consumed to end of string).  An explicitly
   quoted empty token (e.g. '') yields an empty-string argument."
  (map 'list #'cl-parser-kit:token-text
       (cl-parser-kit:tokenize-string string *command-string-tokenizer*)))
