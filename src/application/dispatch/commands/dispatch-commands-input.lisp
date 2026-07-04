(in-package #:cl-tmux)

;;; Shared command-line input parsing for %cmd-* handlers.

(defmacro with-command-flags+pos ((flags pos args &optional (value-flags "")) &body body)
  "Bind FLAGS and POS to the parsed flag alist and positional tokens from ARGS."
  `(multiple-value-bind (,flags ,pos) (%parse-command-flags ,args ,value-flags)
     (declare (ignorable ,flags ,pos))
     ,@body))

(defmacro with-command-input ((flags positionals args &optional (value-flags "") &rest options)
                              &body body)
  "Parse ARGS, validate FLAGS, and optionally constrain POSITIONALS before BODY."
  (let* ((allowed-flags-p (not (null (member :allowed-flags options))))
         (allowed-flags (when allowed-flags-p (getf options :allowed-flags)))
         (min-positionals-p (not (null (member :min-positionals options))))
         (min-positionals (when min-positionals-p (getf options :min-positionals)))
         (max-positionals-p (not (null (member :max-positionals options))))
         (max-positionals (when max-positionals-p (getf options :max-positionals)))
         (message (getf options :message "unsupported argument")))
    `(with-command-flags+pos (,flags ,positionals ,args ,value-flags)
       (if (%command-input-invalid-p ,flags ,positionals
                                     ,allowed-flags-p ,allowed-flags
                                     ,min-positionals-p ,min-positionals
                                     ,max-positionals-p ,max-positionals)
           (progn
             (show-overlay ,message)
             nil)
           (locally ,@body)))))

(defun %maybe-quote-form (form)
  "Return FORM unchanged when it is already quoted, otherwise quote it."
  (if (and (consp form) (eq (car form) 'quote))
      form
      (list 'quote form)))

(defun %command-input-invalid-p (flags positionals allowed-flags-p allowed-flags
                                 min-positionals-p min-positionals
                                 max-positionals-p max-positionals)
  "Return true when parsed command input violates shared flag or arity limits."
  (let ((positional-count (length positionals)))
    (or (and allowed-flags-p
             (find-if-not (lambda (flag)
                            (member (car flag) allowed-flags :test #'char=))
                          flags))
        (and min-positionals-p (< positional-count min-positionals))
        (and max-positionals-p (> positional-count max-positionals)))))

(defmacro define-command-input-handler (name (session args) docstring
                                        (flags positionals value-flags
                                         &rest options)
                                        &body body)
  "Define a %cmd-* handler with shared command-input plumbing."
  (let* ((allowed-flags-p (member :allowed-flags options))
         (allowed-flags (when allowed-flags-p (getf options :allowed-flags)))
         (min-positionals-p (member :min-positionals options))
         (min-positionals (when min-positionals-p (getf options :min-positionals)))
         (max-positionals-p (member :max-positionals options))
         (max-positionals (when max-positionals-p (getf options :max-positionals)))
         (message (getf options :message "unsupported argument")))
    `(defun ,name (,session ,args)
       ,docstring
       (declare (ignorable ,session))
       (with-command-input (,flags ,positionals ,args ,value-flags
                            :allowed-flags ,(%maybe-quote-form allowed-flags)
                            ,@(when min-positionals-p
                                `(:min-positionals ,min-positionals))
                            ,@(when max-positionals-p
                                `(:max-positionals ,max-positionals))
                            :message ,message)
         ,@body))))

(defun %parse-flag-token (token value-flags remaining-tokens)
  "Parse one flag TOKEN into flag entries, supporting clustered boolean flags:
   -ga = -g -a, -gF = -g -F.  Returns (values FLAG-ENTRIES NEW-REMAINING)."
  (let ((entries nil)
        (len     (length token))
        (i       1))
    (loop while (< i len) do
      (let ((ch (char token i)))
        (if (find ch value-flags)
            (let ((attached (when (< (1+ i) len) (subseq token (1+ i)))))
              (if attached
                  (push (cons ch attached) entries)
                  (progn
                    (push (cons ch (if remaining-tokens (first remaining-tokens) ""))
                          entries)
                    (setf remaining-tokens (rest remaining-tokens))))
              (return))
            (progn (push (cons ch t) entries)
                   (incf i)))))
    (values (nreverse entries) remaining-tokens)))

(defun %parse-command-flags (tokens &optional (value-flags ""))
  "Split TOKENS into (values FLAGS POSITIONALS).  -X flags are parsed; those
   whose char is in VALUE-FLAGS consume the next token as their value."
  (loop with flags = nil and positionals = nil and rest = tokens
        while rest
        for token = (first rest)
        do (setf rest (rest rest))
           (if (and (>= (length token) 2)
                    (char= (char token 0) #\-)
                    (char/= (char token 1) #\-))
               (multiple-value-bind (entries new-rest)
                   (%parse-flag-token token value-flags rest)
                 (dolist (e entries) (push e flags))
                 (setf rest new-rest))
               (push token positionals))
        finally (return (values (nreverse flags) (nreverse positionals)))))

(defun %parse-flag-int (flags char)
  "Return the integer value of flag CHAR in FLAGS, or NIL when the flag is absent.
   Uses parse-integer with :junk-allowed t so non-numeric values also return NIL."
  (let ((v (%flag-value flags char)))
    (and (stringp v) (%parse-integer-or-nil v))))
