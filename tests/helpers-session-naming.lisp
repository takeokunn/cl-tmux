;;;; Session and window naming helpers for cl-tmux tests.

(in-package #:cl-tmux/test)

(defmacro with-session-name ((session-var name) &body body)
  "Assign NAME to SESSION-VAR and continue with BODY."
  `(progn
     (setf (session-name ,session-var) ,name)
     ,@body))

(defmacro with-window-names ((session-var &rest names) &body body)
  "Assign NAMES to the active windows of SESSION-VAR in order."
  (let ((windows (gensym "WINDOWS")))
    `(let ((,windows (session-windows ,session-var)))
       (loop for window in ,windows
             for name in (list ,@names)
             do (setf (window-name window) name))
       ,@body)))

(defmacro with-session-and-window-names ((session-var session-name
                                         &rest window-names)
                                        &body body)
  "Assign SESSION-NAME and WINDOW-NAMES to SESSION-VAR in one step."
  `(with-session-name (,session-var ,session-name)
     (with-window-names (,session-var ,@window-names)
       ,@body)))

(defmacro with-registered-sessions ((&rest session-bindings) &body body)
  "Bind *SERVER-SESSIONS* from SESSION-BINDINGS data."
  `(let ((cl-tmux::*server-sessions*
          (list ,@(loop for (session-name session-var) in session-bindings
                        collect `(cons ,session-name ,session-var)))))
     ,@body))

(defmacro with-command-line-rejection-cases ((line-var message-var row-token-var cases)
                                             &body body)
  "Iterate over rejection cases as data, keeping the assertions in BODY."
  `(dolist (case ,cases)
     (destructuring-bind (,line-var ,message-var ,row-token-var) case
       ,@body)))
