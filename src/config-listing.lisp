(in-package #:cl-tmux/config)

;;; ── Key-binding accessors (thin wrappers over key-tables) ─────────────────

(defun lookup-key-binding (key)
  "Return the command keyword bound to KEY (a character or string), or NIL.
   Looks up the prefix key-table."
  (let ((entry (key-table-lookup +table-prefix+ key)))
    (and entry (key-table-command entry))))

(defun key-label (key)
  "Return a display string for KEY: characters become single-character strings,
   strings are returned as-is."
  (if (characterp key) (string key) key))

(defun %binding-label (command)
  "Human-readable label for a binding's COMMAND value: a reconstructed command
   line for a token list (`bind key cmd args`), or the lowercased keyword name
   for a built-in command."
  (if (consp command)
      (format nil "~{~A~^ ~}" command)
      (format nil "~(~A~)" command)))

(defun %sorted-table-names ()
  "Return all key-table names from *KEY-TABLES* in alphabetical order."
  (sort (loop for name being the hash-keys of *key-tables* collect name)
        #'string<))

(defun %table-binding-alist (inner)
  "Build an alist of (key . entry) from INNER (an inner key-table hash-table).
   The whole entry (command . flags) is kept so callers can read the -N note."
  (loop for key being the hash-keys of inner
        using (hash-value entry)
        collect (cons key entry)))

(defun %format-binding-line (table-name key entry)
  "Format one `bind-key -T TABLE-NAME [-N note] KEY COMMAND` line.
   When ENTRY is repeatable, emit `-r`; when it carries an -N note, emit
   `-N \"note\"` before the key, matching tmux's list-keys -N output."
  (let ((note (key-table-note entry))
        (repeatable (key-table-repeatable-p entry)))
    (format nil "bind-key -T ~A ~:[~;-r ~]~@[-N \"~A\" ~]~A ~A~%"
            table-name
            repeatable
            note
            (key-label key)
            (%binding-label (key-table-command entry)))))

(defun %describe-one-table (out table-name)
  "Write bind-key lines for TABLE-NAME to OUT stream.  No-op when table is absent."
  (let* ((inner (gethash table-name *key-tables*)))
    (when inner
      (let ((bindings (sort (%table-binding-alist inner)
                            #'string< :key (lambda (b) (key-label (car b))))))
        (dolist (binding bindings)
          (write-string (%format-binding-line table-name (car binding) (cdr binding))
                        out))))))

(defun describe-key-bindings ()
  "Return bind-key -T table key command lines for all key tables.
   Output format matches real tmux list-keys: one binding per line,
   sorted by table name then by key within each table."
  (with-output-to-string (out)
    (dolist (table-name (%sorted-table-names))
      (%describe-one-table out table-name))))

(defun describe-key-bindings-for-table (table-name)
  "Return bind-key lines for TABLE-NAME only.
   When TABLE-NAME is NIL, returns all tables (same as DESCRIBE-KEY-BINDINGS).
   Returns an empty string when TABLE-NAME names a non-existent table."
  (if (null table-name)
      (describe-key-bindings)
      (with-output-to-string (out)
        (%describe-one-table out table-name))))

(defun describe-key-bindings-for-key (table-name key)
  "Return bind-key lines matching KEY, optionally limited to TABLE-NAME.
   KEY is compared against the display label shown by LIST-KEYS, so both character
   keys like \"c\" and named keys like \"C-Right\" work."
  (let ((tables (if table-name (list table-name) (%sorted-table-names))))
    (with-output-to-string (out)
      (dolist (name tables)
        (let ((inner (gethash name *key-tables*)))
          (when inner
            (dolist (binding (sort (%table-binding-alist inner)
                                   #'string< :key (lambda (b) (key-label (car b)))))
              (when (string= key (key-label (car binding)))
                (write-string (%format-binding-line name (car binding) (cdr binding))
                              out)))))))))
