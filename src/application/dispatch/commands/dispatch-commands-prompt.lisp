(in-package #:cl-tmux)

;;; command-prompt continuation and substitution helpers.

(defun %command-prompt-ask-next (session template prompt-list answers idx num-prompts
                                 single-key initial)
  "Drive the sequential command-prompt -p flow."
  (labels ((finish ()
             (%run-command-line session
                                (%substitute-percent
                                 template
                                 (loop for i below num-prompts
                                       collect (aref answers i)))))
           (advance (i)
             (if (>= i num-prompts)
                 (finish)
                 (let ((label (nth i prompt-list))
                       (seed  (if (zerop i) initial "")))
                   (prompt-start label seed
                                 (lambda (input)
                                   (setf (aref answers i) input)
                                   (advance (1+ i)))
                                 :single-key single-key)))))
    (advance idx)))

(defun %substitute-prompt-response (template input)
  "Expand a single-prompt command-prompt TEMPLATE: tmux replaces both %% and %1
   with the prompt response for a single prompt.  Rewrites each %% pair to %1
   left-to-right, then delegates to %substitute-percent with INPUT as arg 1."
  (let ((rewritten (with-output-to-string (out)
                     (let ((n (length template)) (i 0))
                       (loop while (< i n)
                             do (if (and (char= (char template i) #\%)
                                         (< (1+ i) n)
                                         (char= (char template (1+ i)) #\%))
                                    (progn (write-string "%1" out) (incf i 2))
                                    (progn (write-char (char template i) out)
                                           (incf i))))))))
    (%substitute-percent rewritten (list input))))

(defun %substitute-percent (template args)
  "Expand a command-prompt template: %1..%9 are replaced by the 1st..9th element
   of ARGS (an empty string when that arg is absent, matching tmux), %% is a
   literal percent, and any other %x is left verbatim.  Used by command-prompt -p.
   A single left-to-right pass so %1 never matches inside %10 and %% is not itself
   treated as an argument reference."
  (let ((out (make-string-output-stream))
        (n   (length template))
        (i   0))
    (loop while (< i n)
          for ch = (char template i)
          do (if (and (char= ch #\%) (< (1+ i) n))
                 (let ((next (char template (1+ i))))
                   (cond
                     ((char= next #\%)
                      (write-char #\% out) (incf i 2))
                     ((and (digit-char-p next) (char/= next #\0))
                      (let ((idx (digit-char-p next)))
                        (when (<= idx (length args))
                          (write-string (nth (1- idx) args) out)))
                      (incf i 2))
                     (t
                      (write-char ch out) (incf i))))
                 (progn (write-char ch out) (incf i))))
    (get-output-stream-string out)))
