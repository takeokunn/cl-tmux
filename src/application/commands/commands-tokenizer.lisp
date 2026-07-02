(in-package #:cl-tmux/commands)

;;;; Command-string tokeniser: shell-style quote/escape splitting shared by
;;;; multi-argument commands such as send-keys (and, in future,
;;;; display-message / if-shell).
;;;;
;;;; tmux command arguments are split shell-style: whitespace separates arguments,
;;;; '...' is a literal span, "..." allows backslash escapes, and a bare \\ escapes
;;;; the next character.  Adjacent spans join into one argument (foo"bar baz" →
;;;; foobar baz).

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
