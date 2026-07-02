(in-package #:cl-tmux/renderer)

;;;; Copy-mode pane overlay loader.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((renderer-source-directory ()
           (or (ignore-errors
                 (let ((root (asdf:system-source-directory :cl-tmux)))
                   (and root
                        (merge-pathnames "src/presentation/renderer/" root))))
               (let* ((source (or *compile-file-truename* *load-truename*)))
                 (and source
                      (make-pathname :name nil :type nil :defaults source))))))
  (let ((base (renderer-source-directory)))
    (dolist (name '("renderer-statusbar-layout.lisp"
                    "renderer-pane-copy-mode-overlay.lisp"
                    "renderer-pane-copy-mode-line-number.lisp"))
      (let ((path (and base (merge-pathnames name base))))
        (when (and path (probe-file path))
          (load path)))))))
