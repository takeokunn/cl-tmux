;;;; Key translation and binding assertion helpers for cl-tmux tests.

(in-package #:cl-tmux/test)

(defun key-name-bytes (name)
  "Return %key-name-to-bytes(NAME) as a list of byte values."
  (coerce (cl-tmux/commands::%key-name-to-bytes name) 'list))

(defun split-key-modifiers-values (name)
  "Return the multiple values from %split-key-modifiers(NAME) as a list."
  (multiple-value-list (cl-tmux/commands::%split-key-modifiers name)))

(defun translate-send-keys-bytes (string)
  "Return %translate-send-keys(STRING) as a list of byte values."
  (coerce (cl-tmux/commands::%translate-send-keys string) 'list))

(defun key-table-command-value (table key)
  "Return the command bound to KEY in TABLE as a list or keyword."
  (let ((command (cl-tmux/config:key-table-command
                  (cl-tmux/config:key-table-lookup table key))))
    (if (and (consp command)
             (eq 'quote (first command))
             (consp (second command))
             (null (cddr command)))
        (second command)
        command)))

(defun copy-mode-x-command-value (name)
  "Return the copy-mode -X command keyword bound to NAME."
  (cdr (assoc name cl-tmux::*copy-mode-x-commands* :test #'string-equal)))

(defun alist-value (key alist &key (test #'eql))
  "Return the value bound to KEY in ALIST."
  (cdr (assoc key alist :test test)))

(defmacro check-copy-mode-bindings (table-name &rest rows)
  "Assert that each (KEY EXPECTED MESSAGE) binding in ROWS is present."
  `(dolist (row (list ,@(mapcar (lambda (row) `(list ,@row)) rows)))
     (destructuring-bind (key expected message) row
       (let ((entry (cl-tmux/config:key-table-lookup ,table-name key)))
         (is (eq expected (cl-tmux/config:key-table-command entry))
             "~A ~A: ~A" ,table-name key message)))))
