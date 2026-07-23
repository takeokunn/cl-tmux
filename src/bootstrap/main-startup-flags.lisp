;;; Startup flag parsing helpers.
;;;
;;; This file owns the shared macro used by startup-mode parsers plus the
;;; attach-session flag parser generated from it.

(in-package :cl-tmux)

;;; ── Flag-parser macro ────────────────────────────────────────────────────────
;;;
;;; define-flag-parser generates a parser for a set of boolean and value flags.
;;; Each FLAG-SPEC is one of:
;;;   (:bool  "flag-string"  variable-name)   — sets variable-name to T
;;;   (:value "flag-string"  variable-name)   — sets variable-name to the next arg
;;; The macro generates a loop over the args vector and produces a multi-value
;;; return of all variables in declaration order.
;;;
;;; The generated cond has a final error arm.  Startup flag parsers are strict:
;;; each argument must be declared in FLAG-SPECS, and unknown flags are
;;; rejected instead of being silently treated as positional input.

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun %flag-parser-clause (spec arg-sym args-sym index-sym)
  "Return the COND clause that handles SPEC for a generated flag parser."
  (ecase (first spec)
    (:bool
     (destructuring-bind (_ flag variable) spec
       (declare (ignore _))
       `((string= ,arg-sym ,flag)
         (setf ,variable t)
         (incf ,index-sym))))
    (:value
     (destructuring-bind (_ flag variable) spec
       (declare (ignore _))
       `((string= ,arg-sym ,flag)
         (incf ,index-sym)
         (when (< ,index-sym (length ,args-sym))
           (setf ,variable (nth ,index-sym ,args-sym))
           (incf ,index-sym))))))))

(defmacro define-flag-parser (parser-name (&rest defaults) &rest flag-specs)
  "Define PARSER-NAME as a function (ARGS) → (values ...) that parses FLAGS.
   DEFAULTS is a list of (variable-name default-value) bindings.
   FLAG-SPECS are (:bool FLAG VAR) or (:value FLAG VAR) declarations.
   Unknown flags signal an error; callers must declare every accepted flag."
  (let ((args-sym   (gensym "ARGS"))
        (index-sym  (gensym "INDEX"))
        (arg-sym    (gensym "ARG"))
        (var-names  (mapcar #'first defaults)))
    `(defun ,parser-name (,args-sym)
       ,(format nil "Generated flag parser for: ~{~A~^, ~}"
                (mapcar #'second flag-specs))
         (let (,@defaults
               (,index-sym 0))
           (loop while (< ,index-sym (length ,args-sym)) do
             (let ((,arg-sym (nth ,index-sym ,args-sym)))
               (cond
               ,@(mapcar (lambda (spec)
                           (%flag-parser-clause spec arg-sym args-sym index-sym))
                         flag-specs)
               (t (error "Unknown flag ~A for ~A" ,arg-sym ',parser-name)))))
         (values ,@var-names)))))

(define-flag-parser %parse-attach-flags
    ((name "0") (detach nil) (read-only-p nil))
  (:value "-t" name)
  (:bool  "-d" detach)
  (:bool  "-r" read-only-p))

(define-flag-parser %parse-new-session-flags
    ((name nil) (win-name nil) (detach nil) (start-dir nil))
  (:value "-s" name)
  (:value "-n" win-name)
  (:bool  "-d" detach)
  (:value "-c" start-dir))
