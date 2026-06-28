(in-package #:cl-tmux)

;;;; Shared dispatch command registry data.
;;;; Loaded by dispatch-core-commands.lisp so the registry stays in one place.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((root (or (ignore-errors (asdf:system-source-directory :cl-tmux))
                   *load-pathname*
                   *compile-file-pathname*
                   *default-pathname-defaults*))
         (src (merge-pathnames #P"src/" root)))
    (load (merge-pathnames #p"application/dispatch/core/dispatch-command-specs-common.lisp" src))
    (load (merge-pathnames #p"application/dispatch/core/dispatch-command-specs-core-session.lisp" src))
    (load (merge-pathnames #p"application/dispatch/core/dispatch-command-specs-core-window.lisp" src))
    (load (merge-pathnames #p"application/dispatch/core/dispatch-command-specs-core-pane.lisp" src))
    (load (merge-pathnames #p"application/dispatch/core/dispatch-command-specs-core-misc.lisp" src))))

(defun %dispatch-command-specs-core-from-entries (entries)
  (%dispatch-command-specs-from-entries entries #'%make-dispatch-command-spec))

(defparameter *dispatch-command-specs-core*
  (append (%dispatch-command-specs-core-from-entries
           *dispatch-command-specs-core-session-entries*)
          (%dispatch-command-specs-core-from-entries
           *dispatch-command-specs-core-window-entries*)
          (%dispatch-command-specs-core-from-entries
           *dispatch-command-specs-core-pane-entries*)
          (%dispatch-command-specs-core-from-entries
           *dispatch-command-specs-core-misc-entries*)))
