(in-package #:cl-tmux/test)

;;;; Option fixture macros and assertion DSL.

(defmacro with-fresh-options (&body body)
  "Run BODY with empty, isolated option hash tables (no registered specs).
   Neither *global-options* nor *option-registry* carry state from load time."
  `(let ((cl-tmux/options:*global-options*   (make-hash-table :test #'equal))
         (cl-tmux/options:*option-registry*  (make-hash-table :test #'equal)))
     ,@body))

(defmacro with-fresh-global-options (&body body)
  "Run BODY with a copy of *global-options* so mutations do not leak.
   *option-registry* is shared so type coercion continues to work."
  `(let ((cl-tmux/options:*global-options*
          (let ((ht (make-hash-table :test #'equal)))
            (maphash (lambda (k v) (setf (gethash k ht) v))
                     cl-tmux/options:*global-options*)
            ht)))
     ,@body))

(defmacro with-single-option ((name value) &body body)
  "Run BODY with *global-options* bound to a hash-table containing only NAME -> VALUE."
  `(let ((cl-tmux/options:*global-options*
          (let ((ht (make-hash-table :test #'equal)))
            (setf (gethash ,name ht) ,value)
            ht)))
     ,@body))

(defmacro with-fresh-server-options (&body body)
  "Run BODY with an empty, isolated *server-options* hash-table.
   Changes do not leak back to the real *server-options* table."
  `(let ((cl-tmux/options:*server-options* (make-hash-table :test #'equal))
         (cl-tmux/options:*server-option-registry* cl-tmux/options:*server-option-registry*))
     ,@body))

(defmacro with-single-server-option ((name value) &body body)
  "Run BODY with *server-options* bound to a hash-table containing only NAME -> VALUE."
  `(let ((cl-tmux/options:*server-options*
          (let ((ht (make-hash-table :test #'equal)))
            (setf (gethash ,name ht) ,value)
            ht)))
     ,@body))

(defmacro with-isolated-options ((&rest overrides) &body body)
  "Run BODY with a fresh copy of *global-options*, applying OVERRIDES.
   OVERRIDES is a flat list of alternating option-name and value pairs.
   Changes do not leak back to the real *global-options* table."
  (let ((ht-sym (gensym "HT")))
    `(let ((cl-tmux/options:*global-options*
            (let ((,ht-sym (make-hash-table :test #'equal)))
              (maphash (lambda (k v) (setf (gethash k ,ht-sym) v))
                       cl-tmux/options:*global-options*)
              ,@(loop for (k v) on overrides by #'cddr
                      collect `(setf (gethash ,k ,ht-sym) ,v))
              ,ht-sym)))
       ,@body)))

(defmacro assert-set-directive-option-state (form option expected
                                             &key (context "set directive")
                                                  server-p
                                                  (present-p t))
  "Apply FORM and assert that OPTION is present or absent in the selected scope."
  (let ((actual-sym (gensym "ACTUAL")))
    `(progn
       (assert-config-directive-applied ,form ,context)
       (let ((,actual-sym ,(if server-p
                               `(cl-tmux/options:get-server-option ,option nil)
                               `(cl-tmux/options:get-option ,option nil))))
         ,(if present-p
              `(is (equal ,expected ,actual-sym)
                   "~A must store ~S in ~A options, got ~S"
                   ,context ,expected ,(if server-p "server" "global") ,actual-sym)
              `(is (null ,actual-sym)
                   "~A must remove ~S from ~A options, got ~S"
                   ,context ,option ,(if server-p "server" "global") ,actual-sym))))))
