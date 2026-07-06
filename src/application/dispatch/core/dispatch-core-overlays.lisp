(in-package #:cl-tmux)

;;;; Declarative command dispatch - overlay rendering helpers.

(defmacro show-built-overlay ((stream) &body body)
  "Show an overlay whose text is built by BODY writing to STREAM."
  `(show-overlay (with-output-to-string (,stream) ,@body)))

(defun %overlay-lines-string (lines &optional (empty ""))
  "Render LINES as newline-separated overlay text, or EMPTY when no lines exist."
  (if lines
      (with-output-to-string (s)
        (loop for line in lines
              for first = t then nil
              do (unless first
                   (terpri s))
                 (princ line s)))
      empty))

(defun %overlayf (control &rest args)
  "Render a formatted one-line overlay from CONTROL and ARGS."
  (show-overlay (apply #'format nil control args)))

(defmacro with-overlay-on-error ((op-label) &body body)
  "Run BODY, reporting any ERROR as an overlay tagged with OP-LABEL."
  `(handler-case (progn ,@body)
     (error (e) (%overlayf "~A error: ~A" ,op-label e))))
