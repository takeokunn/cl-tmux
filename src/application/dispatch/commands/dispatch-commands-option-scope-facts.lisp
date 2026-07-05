(in-package #:cl-tmux)

;;;; Option scope canonical facts.

(defmacro define-scope-accessor-table (&rest rules)
  "Build %SCOPE-GETTER, %SCOPE-SETTER, and %SCOPE-REMOVER from declarative
scope access rules. Each rule has (SCOPE GETTER-FORM SETTER-FORM REMOVER-FORM).
NAME, VALUE, DEFAULT, and TARGET are intentionally bound by the generated
functions so each fact row stays data-shaped."
  `(progn
     (defun %scope-getter (scope name target &optional default)
       (declare (ignorable target default))
       (ecase scope
         ,@(mapcar (lambda (rule)
                     (destructuring-bind (scope getter-form setter-form remover-form) rule
                       (declare (ignore setter-form remover-form))
                       `(,scope ,getter-form)))
                   rules)))
     (defun %scope-setter (scope name value target)
       (declare (ignorable target))
       (ecase scope
         ,@(mapcar (lambda (rule)
                     (destructuring-bind (scope getter-form setter-form remover-form) rule
                       (declare (ignore getter-form remover-form))
                       `(,scope ,setter-form)))
                   rules)))
     (defun %scope-remover (scope name target)
       (declare (ignorable target))
       (ecase scope
         ,@(mapcar (lambda (rule)
                     (destructuring-bind (scope getter-form setter-form remover-form) rule
                       (declare (ignore getter-form setter-form))
                       `(,scope ,remover-form)))
                   rules)))))

(define-scope-accessor-table
  (:pane
   (cl-tmux/options:get-option-for-pane name target)
   (cl-tmux/options:set-option-for-pane name value target)
   (remhash name (cl-tmux/model:pane-local-options target)))
  (:window
   (cl-tmux/options:get-option-for-window name target)
   (cl-tmux/options:set-option-for-window name value target)
   (remhash name (cl-tmux/model:window-local-options target)))
  (:global
   (cl-tmux/options:get-option name default)
   (cl-tmux/options:set-option name value)
   (remhash name cl-tmux/options:*global-options*))
  (:server
   (cl-tmux/options:get-server-option name default)
   (cl-tmux/options:set-server-option name value)
   (remhash name cl-tmux/options:*server-options*)))
