(in-package #:cl-tmux/config)

;;; if-shell config-time conditional handling.

(defun %if-shell-format-true-p (condition)
  "Return tmux -F truthiness for an if-shell CONDITION."
  (let ((result (ignore-errors (cl-tmux/format:expand-format condition nil))))
    (and result (not (member result '("" "0") :test #'string=)))))

(defun %take-brace-or-command (tokens)
  "Consume one if-shell command unit from TOKENS."
  (cond
    ((null tokens) (values nil nil))
    ((string= (first tokens) "{")
     (let ((depth 1) (inner '()) (rest (rest tokens)))
       (loop for tok = (pop rest)
             while tok do
               (cond ((string= tok "{") (incf depth) (push tok inner))
                     ((string= tok "}") (decf depth)
                      (if (zerop depth) (return) (push tok inner)))
                     (t (push tok inner))))
       (values (%split-on-semicolons (nreverse inner)) rest)))
    (t
     (values (list (%config-tokens (first tokens))) (rest tokens)))))

(defun %apply-if-shell-directive (cmd args)
  "Handle if-shell [-bF] [-t target] CONDITION THEN-CMD [ELSE-CMD]."
  (when (member cmd '("if-shell" "if") :test #'string=)
    (let ((format-mode nil)
          (remaining args))
      (setf remaining
            (%consuming-flags (remaining tok rest)
              ((string= tok "-t") (when rest (setf rest (cdr rest))))
              (t (when (%flag-token-contains-any-p tok '(#\F))
                   (setf format-mode t)))))
      (when (>= (length remaining) 2)
        (let* ((condition (first remaining))
               (truthy-p (if format-mode
                             (%if-shell-format-true-p condition)
                             (eql 0 (nth-value 2
                                      (%run-config-shell-command-safe condition))))))
          (multiple-value-bind (then-cmds after-then)
              (%take-brace-or-command (rest remaining))
            (let ((else-cmds (and after-then
                                  (nth-value 0 (%take-brace-or-command after-then)))))
              (dolist (line (if truthy-p then-cmds else-cmds))
                (apply-config-directive line))))))
      t)))
