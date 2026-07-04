;;;; Test isolation helpers for cl-tmux.

(in-package #:cl-tmux/test)

(defmacro with-isolated-hooks (&body body)
  "Run BODY with fresh *hook-registry* and *command-hooks* tables so neither
   lisp-function hooks nor command hooks (set-hook) leak between tests."
  `(let ((cl-tmux/hooks:*hook-registry* (make-hash-table :test #'equal))
         (cl-tmux/hooks:*command-hooks* (make-hash-table :test #'equal)))
     ,@body))

(defmacro with-isolated-config (&body body)
  "Run BODY with the mutable config specials dynamically rebound to copies,
   so directives applied in a test never leak into other suites.
   Isolates: key-tables, default-shell, status-height, prefix-key-code,
             global-options (copy), server-options (copy)."
  `(let ((cl-tmux/config:*key-tables*  (make-hash-table :test #'equal))
         (cl-tmux/config:*default-shell* cl-tmux/config:*default-shell*)
         (cl-tmux/config:*status-height* cl-tmux/config:*status-height*)
         (cl-tmux/config:*prefix-key-code*  cl-tmux/config:*prefix-key-code*)
         (cl-tmux/config:*prefix2-key-code* cl-tmux/config:*prefix2-key-code*)
         (cl-tmux/options:*global-options*
          (let ((h (make-hash-table :test #'equal)))
            (maphash (lambda (k v) (setf (gethash k h) v))
                     cl-tmux/options:*global-options*)
            h))
         (cl-tmux/options:*server-options*
          (let ((h (make-hash-table :test #'equal)))
            (maphash (lambda (k v) (setf (gethash k h) v))
                     cl-tmux/options:*server-options*)
            h)))
     (cl-tmux/config::initialize-default-key-tables)
     (cl-tmux::install-extended-key-bindings)
     ,@body))

(defmacro with-isolated-key-tables (&body body)
  "Run BODY with a fresh *KEY-TABLES* inside WITH-ISOLATED-CONFIG isolation."
  `(let ((cl-tmux/config:*key-tables* (make-hash-table :test #'equal)))
     (with-isolated-config
       ,@body)))

(defmacro with-temp-config-file ((path-var &rest lines) &body body)
  "Write LINES to a temporary config file, bind PATH-VAR, run BODY, then delete it."
  (let ((path-sym (gensym "PATH")))
    `(let ((,path-sym (merge-pathnames
                       (format nil "cl-tmux-test-~D.conf" (random 1000000))
                       (uiop:temporary-directory))))
       (unwind-protect
            (progn
              (with-open-file (out ,path-sym :direction :output
                                             :if-exists :supersede
                                             :if-does-not-exist :create)
                ,@(mapcar (lambda (line) `(write-line ,line out)) lines)
                (finish-output out))
              (let ((,path-var ,path-sym))
                ,@body))
         (when (probe-file ,path-sym)
           (delete-file ,path-sym))))))
