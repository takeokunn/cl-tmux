(in-package #:cl-tmux)

;;;; Pane layout canonical facts.

(defmacro define-layout-name-table (&rest rows)
  "Build %RESOLVE-LAYOUT-NAME from declarative canonical layout facts."
  `(defun %resolve-layout-name (name)
     "Map NAME to a layout keyword, or NIL when NAME is not canonical."
     (cond
       ,@(mapcar (lambda (row)
                   (destructuring-bind (kw canonical-name) row
                     `((string-equal name ,canonical-name) ,kw)))
                 rows)
       (t nil))))

(define-layout-name-table
  (:even-horizontal "even-horizontal")
  (:even-vertical   "even-vertical")
  (:main-horizontal "main-horizontal")
  (:main-vertical   "main-vertical")
  (:tiled           "tiled"))
