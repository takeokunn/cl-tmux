(in-package #:cl-tmux)

;;;; Shared dispatch command registry data.
;;;; Loaded by dispatch-core-commands.lisp so the registry stays in one place.

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Keep the shared table anchored to the source tree even when ASDF loads
  ;; the compiled fasl from its cache.
  (load (merge-pathnames #p"application/dispatch/core/dispatch-command-specs-core.lisp"
                         (merge-pathnames #P"src/"
                                          (or (ignore-errors (asdf:system-source-directory :cl-tmux))
                                              *load-pathname*
                                              *compile-file-pathname*)))))

(defun %dispatch-spec-public-name (spec)
  "Return the public command name for SPEC."
  (or (getf spec :public-name)
      (first (getf spec :arg-names))))

(defun %dispatch-public-command-names ()
  "Return tmux public command names derived from the core dispatch specs."
  (let ((names (loop for spec in *dispatch-command-specs-core*
                     for name = (%dispatch-spec-public-name spec)
                     when name collect name)))
    (sort (remove-duplicates names :test #'string=) #'string<)))

(defparameter *tmux-public-command-names* (%dispatch-public-command-names)
  "tmux 3.6a public command names derived from `*dispatch-command-specs-core*`.
   This ordered list is the public surface for `list-commands`.")
