(in-package #:cl-tmux)

;;;; Shared dispatch command registry data.
;;;; Loaded by dispatch-core-commands.lisp so the registry stays in one place.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames #p"dispatch-command-specs-core.lisp" *load-pathname*))
  (load (merge-pathnames #p"dispatch-command-specs-compat.lisp" *load-pathname*)))

(defparameter *dispatch-command-specs*
  (append *dispatch-command-specs-core*
          *dispatch-command-specs-compat*)
  "Shared dispatch registry data assembled from core and compatibility specs.")

(defun %dispatch-spec-public-name (spec)
  "Return the public command name for SPEC."
  (or (getf spec :public-name)
      (first (getf spec :arg-names))))

(defun %dispatch-public-command-names ()
  "Return tmux public command names derived from the dispatch specs."
  (let ((names (loop for spec in *dispatch-command-specs*
                     for name = (%dispatch-spec-public-name spec)
                     when name collect name)))
    (sort (remove-duplicates names :test #'string=) #'string<)))

(defparameter *tmux-public-command-names* (%dispatch-public-command-names)
  "tmux 3.6a public command names derived from `*dispatch-command-specs*`.
   This ordered list is the public surface for `list-commands`; it is derived
   from the dispatch registry metadata, with explicit overrides for alias-first
   commands.")
