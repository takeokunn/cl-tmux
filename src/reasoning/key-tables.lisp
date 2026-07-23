;;;; Projection from the live key-table store into reasoning facts.
;;;;
;;;; `snapshot-key-bindings' walks `cl-tmux/config:*key-tables*' (the same
;;;; store the dispatcher binds/looks up through) and produces the plist facts
;;;; consumed by `build-key-rulebase'.  Because it reads the *live* store, a
;;;; rulebase built from it reflects whatever the config file and runtime
;;;; `bind-key' commands have installed.

(in-package #:cl-tmux/reasoning)

(defun snapshot-key-bindings ()
  "Return the live key tables as a list of binding plists.

Each entry is (:table TABLE :key KEY :command COMMAND :repeatable BOOL
:note NOTE-OR-NIL).  Tables and keys keep their stored representation
(table-name strings; character or key-name-string keys)."
  (let ((facts '()))
    (maphash
     (lambda (table inner)
       (maphash
        (lambda (key entry)
          (push (list :table table
                      :key key
                      :command (cl-tmux/config:key-table-command entry)
                      :repeatable (and (cl-tmux/config:key-table-repeatable-p entry) t)
                      :note (cl-tmux/config:key-table-note entry))
                facts))
        inner))
     cl-tmux/config:*key-tables*)
    (nreverse facts)))

(defun current-key-rulebase ()
  "Build a rulebase from a fresh snapshot of the live key tables."
  (build-key-rulebase (snapshot-key-bindings)))

(defun %display-key (key)
  "A short human spelling of KEY for explanations (mirrors list-keys style)."
  (if (characterp key)
      (let ((code (char-code key)))
        (cond ((< 0 code 27) (format nil "C-~C" (code-char (+ code 96))))
              ((= code 27) "Escape")
              ((= code 32) "Space")
              (t (string key))))
      (princ-to-string key)))

(defun explain-binding (table key &optional (rulebase (current-key-rulebase)))
  "Return a human-readable explanation of KEY in TABLE against RULEBASE.

Reports the resolved command, whether the key also lives in the root table,
and any cross-table conflicts on the same key.  Intended for diagnostics and
REPL introspection, not for the hot dispatch path."
  (multiple-value-bind (command found) (key-command rulebase table key)
    (with-output-to-string (out)
      (format out "~A ~A" table (%display-key key))
      (if found
          (format out " -> ~A" command)
          (format out " -> (unbound)"))
      (let ((shadows (member (cons table key) (shadowing-bindings rulebase)
                             :test #'equal)))
        (when shadows
          (format out "~%  also bound in root")))
      (let ((conflicts
              (remove-if-not
               (lambda (entry) (equal (getf entry :key) key))
               (binding-conflicts rulebase))))
        (dolist (entry conflicts)
          (destructuring-bind (&key key tables commands) entry
            (declare (ignore key))
            (format out "~%  conflict: ~A -> ~A vs ~A -> ~A"
                    (first tables) (first commands)
                    (second tables) (second commands))))))))
