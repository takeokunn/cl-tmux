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

(defun key-display-string (key)
  "Human-readable spelling of a key-table KEY (a character or key-name string)."
  (if (characterp key)
      (let ((code (char-code key)))
        (cond ((< 0 code 27) (format nil "C-~C" (code-char (+ code 96))))
              ((= code 27)   "Escape")
              ((= code 32)   "Space")
              (t             (string key))))
      (princ-to-string key)))

(defun describe-key-binding-notes (table-name include-unnoted-p)
  "list-keys -N: list per-binding notes.  Bindings carrying a bind -N note are
   listed as 'TABLE KEY  NOTE'; INCLUDE-UNNOTED-P (-a) also lists un-noted
   bindings with their command as the description, like tmux.  Restricted to
   TABLE-NAME when non-NIL; tables and keys are sorted for stable output."
  (with-output-to-string (out)
    (let (tables)
      (maphash (lambda (name inner) (push (cons name inner) tables)) *key-tables*)
      (dolist (entry (sort tables #'string< :key #'car))
        (destructuring-bind (tname . inner) entry
          (when (or (null table-name) (string= tname table-name))
            (let (rows)
              (maphash
               (lambda (key binding)
                 (let ((note (getf (cdr binding) :note)))
                   (when (or note include-unnoted-p)
                     (push (list (key-display-string key)
                                 (or note (format nil "~(~A~)" (car binding))))
                           rows))))
               inner)
              (dolist (row (sort rows #'string< :key #'first))
                (format out "~A ~A  ~A~%" tname (first row) (second row))))))))))

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
