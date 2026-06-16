(in-package #:cl-tmux)

;;;; Shared dispatch command registry data.
;;;; Loaded by dispatch-core-commands.lisp so the registry stays in one place.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames #p"dispatch-command-specs-common.lisp"
                         *load-pathname*))
  (load (merge-pathnames #p"dispatch-command-specs-core-session.lisp"
                         *load-pathname*))
  (load (merge-pathnames #p"dispatch-command-specs-core-window.lisp"
                         *load-pathname*))
  (load (merge-pathnames #p"dispatch-command-specs-core-pane.lisp"
                         *load-pathname*))
  (load (merge-pathnames #p"dispatch-command-specs-core-misc.lisp"
                         *load-pathname*)))

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
