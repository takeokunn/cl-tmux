;;;; Package bootstrap for cl-tmux.
;;;;
;;;; Keep the package declarations in fragment files loaded here so ASDF's
;;;; serial source order can continue to rely on this single entry point.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *package-fragments-loaded* nil)
  (unless *package-fragments-loaded*
    ;; ASDF may load a fasl from its cache, so prefer the system source root.
    ;; Direct source loads fall back from src/bootstrap/package.lisp to the
    ;; repository root before resolving fragment paths.
    (let* ((source-path (or *load-pathname* *compile-file-pathname*))
           (root (or (ignore-errors (asdf:system-source-directory :cl-tmux))
                     (and source-path (merge-pathnames #P"../../" source-path))))
           (base (merge-pathnames #P"src/" root)))
      (load (merge-pathnames #P"bootstrap/package-version.lisp" base))
      (load (merge-pathnames #P"bootstrap/package-core.lisp" base))
      (load (merge-pathnames #P"bootstrap/package-terminal.lisp" base))
      (load (merge-pathnames #P"bootstrap/package-domain-ports.lisp" base))
      (load (merge-pathnames #P"bootstrap/package-domain-model.lisp" base))
      (load (merge-pathnames #P"bootstrap/package-presentation.lisp" base))
      (load (merge-pathnames #P"bootstrap/package-application.lisp" base)))
    (setf *package-fragments-loaded* t)))
