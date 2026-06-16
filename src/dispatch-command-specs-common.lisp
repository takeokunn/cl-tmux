(in-package #:cl-tmux)

;;;; Shared helpers for dispatch command registry construction.

(defun %make-dispatch-command-spec (named-keyword arg-handler arg-names
                                    &key named-names public-name)
  (append (list :named-keyword named-keyword
                :named-names (or named-names
                                 (and named-keyword
                                      (list (string-downcase
                                             (symbol-name named-keyword)))))
                :arg-handler arg-handler
                :arg-names arg-names)
          (when public-name
            (list :public-name public-name))))

(defun %dispatch-command-specs-from-entries (entries maker)
  (mapcar (lambda (entry)
            (apply maker entry))
          entries))
