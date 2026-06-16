(in-package #:cl-tmux)

;;;; Compatibility dispatch command registry data.
;;;; Loaded by dispatch-core-commands.lisp so the registry stays in one place.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames #p"dispatch-command-specs-common.lisp"
                         *load-pathname*))
  (load (merge-pathnames #p"dispatch-command-specs-compat-session.lisp"
                         *load-pathname*))
  (load (merge-pathnames #p"dispatch-command-specs-compat-window.lisp"
                         *load-pathname*))
  (load (merge-pathnames #p"dispatch-command-specs-compat-pane.lisp"
                         *load-pathname*))
  (load (merge-pathnames #p"dispatch-command-specs-compat-misc.lisp"
                         *load-pathname*)))

(defun %dispatch-command-specs-compat-from-entries (entries)
  (%dispatch-command-specs-from-entries
   entries
   (lambda (arg-handler arg-names &key public-name)
     (%make-dispatch-command-spec nil
                                  arg-handler
                                  arg-names
                                  :public-name public-name))))

(defparameter *dispatch-command-specs-compat*
  (append (%dispatch-command-specs-compat-from-entries
           *dispatch-command-specs-compat-session-entries*)
          (%dispatch-command-specs-compat-from-entries
           *dispatch-command-specs-compat-window-entries*)
          (%dispatch-command-specs-compat-from-entries
           *dispatch-command-specs-compat-pane-entries*)
          (%dispatch-command-specs-compat-from-entries
           *dispatch-command-specs-compat-misc-entries*)))
