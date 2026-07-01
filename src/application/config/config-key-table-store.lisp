(in-package #:cl-tmux/config)

;;; ── Key-table system ──────────────────────────────────────────────────────
;;;
;;; *key-tables* maps table-name (string) → hash-table of chord → (keyword . flags).
;;; Flags is a plist; :repeatable T means the prefix stays active after dispatch.
;;; Standard table names: +table-prefix+, +table-root+,
;;; +table-copy-mode+, +table-copy-mode-vi+.
;;;
;;; The prefix table is the main dispatch path; callers should mutate it
;;; through key-table-bind / key-table-unbind directly.
;;;
;;; This file owns the storage primitives only (create/bind/lookup/unbind).
;;; The declarative default-bindings DATA and the install-* functions that
;;; populate the tables at load time live in config.lisp, which loads after
;;; this file.

(defparameter *key-tables*
  (make-hash-table :test #'equal)
  "Hash-table mapping table-name string → inner hash-table of chord → (keyword . flags).")

(defun ensure-key-table (name)
  "Return the inner hash-table for key-table NAME, creating it if absent.
   Idempotent: repeated calls with the same NAME return the same object."
  (or (gethash name *key-tables*)
      (setf (gethash name *key-tables*)
            (make-hash-table :test #'equal))))

(defun key-table-bind (table key command &key repeatable note)
  "Add a binding for KEY → COMMAND in TABLE (a table-name string).
   :REPEATABLE T marks the binding so the prefix table stays active after dispatch.
   :NOTE is an optional description string (from `bind -N`) shown by list-keys."
  (let ((inner (ensure-key-table table)))
    (setf (gethash key inner)
          (cons command (list :repeatable repeatable :note note)))))

(defun key-table-unbind (table key)
  "Remove any binding for KEY from TABLE and return T when TABLE existed."
  (let ((tbl (gethash table *key-tables*)))
    (when tbl
      (remhash key tbl)
      t)))

(defun key-table-lookup (table key)
  "Return the (command . flags) cons for KEY in TABLE, or NIL."
  (let ((inner (gethash table *key-tables*)))
    (when inner (gethash key inner))))

(defun key-table-command (entry)
  "Extract the command keyword from a key-table entry (car)."
  (car entry))

(defun key-table-repeatable-p (entry)
  "Return T if the key-table entry is marked repeatable.
   Safe to call with NIL (returns NIL without signaling)."
  (and entry (getf (cdr entry) :repeatable)))

(defun key-table-note (entry)
  "Return the -N description string for a key-table ENTRY, or NIL.
   Safe to call with NIL (returns NIL without signaling)."
  (and entry (getf (cdr entry) :note)))
