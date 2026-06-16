;;;; Package bootstrap for cl-tmux.
;;;;
;;;; Keep the package declarations in fragment files loaded here so ASDF's
;;;; serial source order can continue to rely on this single entry point.

(defvar *package-fragments-loaded* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless *package-fragments-loaded*
    (let ((base (or *load-pathname* *compile-file-pathname*)))
      (load (merge-pathnames #P"package-version.lisp" base))
      (load (merge-pathnames #P"package-core.lisp" base))
      (load (merge-pathnames #P"package-terminal.lisp" base))
      (load (merge-pathnames #P"package-domain.lisp" base)))
    (setf *package-fragments-loaded* t)))
