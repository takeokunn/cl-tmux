(in-package #:cl-tmux/format)

;;;; Delimiter scanning and non-brace format dispatch ports.

(defun %matching-close-brace (template start)
  "Index of the } that closes the #{ whose content begins at START, accounting
   for nested #{...}.  Returns NIL when there is no matching close brace.
   For brace-free content this is just the first }, so non-nested formats are
   delimited exactly as before."
  (let ((depth 1) (i start) (n (length template)))
    (loop while (< i n)
          for c = (char template i)
          if (and (char= c #\#) (< (1+ i) n) (char= (char template (1+ i)) #\{))
            do (progn (incf depth) (incf i 2))
          else if (char= c #\})
            do (progn (decf depth)
                      (if (zerop depth) (return i) (incf i)))
          else do (incf i))))

(defun %expand-bracket (template start out)
  "Consume #[attrs] style directive starting at START (just past the '[').
   In real tmux these become SGR sequences; here we pass them through literally
   so the renderer can recognise and convert them (or ignore them safely).
   Writes the full #[...] literally and returns the index just past ']'.
   Emits '#' literally when no closing bracket is found."
  (let ((close (position #\] template :start start)))
    (if (null close)
        (progn (write-char #\# out) (1- start))
        (progn
          (write-char #\# out) (write-char #\[ out)
          (write-string (subseq template start close) out)
          (write-char #\] out)
          (1+ close)))))

(defun %expand-paren (template start out)
  "Expand #(shell-cmd) starting at START (just past the '(').
   Routes the command through the bounded shell-command port and writes stdout to OUT.
   Returns the index just past the closing ')'.
   On any error (no closing paren, command failure) returns safely without
   crashing: missing ')' emits '#' literally; command errors emit empty string."
  (let ((close (position #\) template :start start)))
    (if (null close)
        (progn (write-char #\# out) (1- start))
        (let ((cmd (subseq template start close)))
          (write-string (%run-format-shell-command cmd) out)
          (1+ close)))))
