(in-package #:cl-tmux/commands)

;;; Copy-mode navigation loader.
;;;
;;; The actual navigation commands live in smaller files so each concern can
;;; evolve independently:
;;;   commands-copy-mode-nav-line.lisp
;;;   commands-copy-mode-nav-select.lisp
;;;   commands-copy-mode-nav-paragraph.lisp
;;;   commands-copy-mode-nav-jump.lisp
;;;   commands-copy-mode-nav-copy.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((source (or *load-truename* *compile-file-truename*))
         (load-file (lambda (name)
                      (when source
                        (let ((path (merge-pathnames name
                                                     (make-pathname :name nil
                                                                    :type nil
                                                                    :defaults source))))
                          (when (probe-file path)
                            (load path)))))))
    (funcall load-file #P"commands-copy-mode-nav-line.lisp")
    (funcall load-file #P"commands-copy-mode-nav-select.lisp")
    (funcall load-file #P"commands-copy-mode-nav-paragraph.lisp")
    (funcall load-file #P"commands-copy-mode-nav-jump.lisp")
    (funcall load-file #P"commands-copy-mode-nav-copy.lisp")))
