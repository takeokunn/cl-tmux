;;;; Package bootstrap for cl-tmux.
;;;;
;;;; Keep the package declarations in fragment files loaded here so ASDF's
;;;; serial source order can continue to rely on this single entry point.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *package-fragments-loaded* nil)
  (unless *package-fragments-loaded*
    ;; ASDF may load the compiled fasl from its cache, so *LOAD-PATHNAME*
    ;; would point at the cache directory instead of the source tree.
    ;; Resolve the system source directory explicitly so fragment loads stay
    ;; anchored to the repository copy.
    (let* ((root (or (ignore-errors (asdf:system-source-directory :cl-tmux))
                     *load-pathname*
                     *compile-file-pathname*))
           (base (merge-pathnames #P"src/" root)))
      (load (merge-pathnames #P"package-version.lisp" base))
      (load (merge-pathnames #P"package-core.lisp" base))
      (load (merge-pathnames #P"package-terminal.lisp" base))
      (load (merge-pathnames #P"package-domain.lisp" base)))
    (setf *package-fragments-loaded* t)))
