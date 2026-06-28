(in-package #:cl-tmux/renderer)

;;;; Copy-mode pane overlay loader.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((source (or *load-truename* *compile-file-truename*))
         (base (and source
                    (make-pathname :name nil :type nil :defaults source))))
    (dolist (name '("renderer-statusbar-layout.lisp"
                    "renderer-pane-copy-mode-overlay.lisp"
                    "renderer-pane-copy-mode-line-number.lisp"))
      (let ((path (and base (merge-pathnames name base))))
        (when (and path (probe-file path))
          (load path))))))
