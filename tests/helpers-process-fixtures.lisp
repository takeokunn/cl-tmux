(in-package #:cl-tmux/test)

;;;; Process environment and fdefinition-swap fixtures.

(defmacro with-session-and-env-var ((sess-var name-var env-name env-value) &body body)
  "Bind SESS-VAR to a fresh empty session and NAME-VAR to ENV-NAME.
   Sets ENV-NAME to ENV-VALUE in the real process environment for the duration
   of BODY, then restores the old value (or unsets it if it was absent)."
  `(let ((,sess-var (make-session :id 1 :name "s"))
         (,name-var ,env-name))
     (with-temporary-posix-environment-variable (,name-var ,env-value)
       ,@body)))

(defmacro with-process-env-var ((name-var env-name env-value) &body body)
  "Bind NAME-VAR to ENV-NAME and set ENV-NAME to ENV-VALUE for BODY.
   Restores the original process environment entry after BODY exits."
  `(let ((,name-var ,env-name))
     (with-temporary-posix-environment-variable (,name-var ,env-value)
       ,@body)))

(defmacro with-stubbed-fdefinition ((&rest bindings) &body body)
  "Replace each function cell in BINDINGS with its STUB-FORM for BODY.
   Every original definition is restored even if BODY signals."
  (let ((saved (loop for (symbol) in bindings
                     collect (list symbol (gensym (format nil "ORIG-~A" symbol))))))
    `(let ,(loop for (symbol orig-var) in saved
                collect `(,orig-var (fdefinition ',symbol)))
       (unwind-protect
            (progn
              ,@(loop for (symbol stub-form) in bindings
                     collect `(setf (fdefinition ',symbol) ,stub-form))
              ,@body)
         ,@(loop for (symbol orig-var) in saved
                collect `(setf (fdefinition ',symbol) ,orig-var))))))
