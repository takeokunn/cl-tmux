(in-package #:cl-tmux)

;;;; Loader for the rename/select command family.
;;;; Keep the family split in source files without changing ASDF registration.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((root (ignore-errors (asdf:system-source-directory :cl-tmux)))
         (src (or (and root (merge-pathnames #P"src/" root))
                  (and *load-pathname*
                       (uiop:pathname-directory-pathname *load-pathname*))
                  (and *compile-file-pathname*
                       (uiop:pathname-directory-pathname *compile-file-pathname*))
                  *default-pathname-defaults*)))
    (load (merge-pathnames #P"application/dispatch/commands/dispatch-commands-option-pane-window.lisp" src))
    (load (merge-pathnames #P"application/dispatch/commands/dispatch-commands-option-pane-pane.lisp" src))))
