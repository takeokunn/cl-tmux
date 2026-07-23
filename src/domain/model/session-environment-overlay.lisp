(in-package #:cl-tmux/model)

;;; ── Session overlay environment ──────────────────────────────────────────────
;;;
;;; This file owns the in-session environment overlay: parsing NAME=VALUE
;;; strings, validating names, and reading/writing the session's overlay tables.

;;; ── NAME=VALUE string pair helpers ──────────────────────────────────────────
;;;
;;; %ENVIRONMENT-ENTRY-NAME lives in session-environment-process.lisp (loaded
;;; before this file, and itself a caller) rather than here, to avoid the two
;;; files each defining an identical name-parsing helper.

(defun %environment-entry-value (entry)
  "Return the VALUE component of a NAME=VALUE environment ENTRY string, or NIL."
  (let ((eq-pos (position #\= entry)))
    (when eq-pos
      (subseq entry (1+ eq-pos)))))

(defun %environment-strings-to-table (entries)
  "Convert a list of NAME=VALUE ENTRIES into a hash table keyed by name.
   Entries without '=' are silently skipped."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (entry entries table)
      (let ((name  (%environment-entry-name  entry))
            (value (%environment-entry-value entry)))
        (when (and name value)
          (setf (gethash name table) value))))))

(defun %environment-table-to-list (table)
  "Convert TABLE into a sorted list of NAME=VALUE strings (sorted by name)."
  (let (entries)
    (maphash (lambda (name value)
               (push (format nil "~A=~A" name value) entries))
             table)
    (sort entries #'string< :key #'%environment-entry-name)))

;;; ── %assert-environment-variable-name ───────────────────────────────────────

(defun %assert-environment-variable-name (name)
  "Signal an error when NAME is not a valid POSIX environment variable name.
   A valid name is a non-empty string containing no '=' character."
  (unless (and (stringp name)
               (plusp (length name))
               (not (find #\= name)))
    (error "Invalid environment variable name: ~S" name)))

;;; ── Session overlay access ───────────────────────────────────────────────────

(defun session-environment-value (session name)
  "Return SESSION's effective value for NAME.
   Returns two values: (value source-keyword) where source-keyword is one of:
     :unset   — explicitly removed from the session overlay
     :session — set in the session overlay
     :process — inherited from the current process environment
     NIL      — not present anywhere relevant."
  (cond
    ((member name (session-environment-unsets session) :test #'string=)
     (values nil :unset))
    (t
     (multiple-value-bind (value present-p)
         (gethash name (session-environment session))
       (if present-p
           (values value :session)
           ;; Fall back to the live process environment (tmux inherits unset
           ;; session vars from the server's environment).  The fallback lives
           ;; in the else-branch so it stays reachable — a `(t ...)` middle
           ;; cond clause would make it dead code.
           (let ((process-value (process-environment-value name)))
             (if process-value
                 (values process-value :process)
                 (values nil nil))))))))

(defun session-environment-names (session)
  "Return the sorted list of environment names relevant to SESSION.
   Includes: update-environment vars that are currently set in the process,
   all names in the session overlay (set and explicitly unset)."
  (let ((names (mapcar #'car (get-update-environment-vars))))
    (maphash (lambda (name value)
               (declare (ignore value))
               (pushnew name names :test #'string=))
             (session-environment session))
    (dolist (name (session-environment-unsets session))
      (pushnew name names :test #'string=))
    (sort names #'string<)))

(defun session-set-environment (session name value &key hidden)
  "Store NAME=VALUE in SESSION's environment overlay.
   Removes NAME from the unset list if it was explicitly unset before.
   HIDDEN marks the variable hidden (tmux set-environment -h: excluded from
   plain show-environment and from child environments); a plain set clears an
   existing hidden mark, matching tmux's environ_set with no flags.
   Returns SESSION."
  (setf (session-environment-unsets session)
        (delete name (session-environment-unsets session) :test #'string=))
  (if hidden
      (pushnew name (session-environment-hidden session) :test #'string=)
      (setf (session-environment-hidden session)
            (delete name (session-environment-hidden session) :test #'string=)))
  (setf (gethash name (session-environment session)) value)
  session)

(defun session-unset-environment (session name)
  "Record NAME as explicitly unset in SESSION's environment overlay.
   Removes NAME from the set table (and the hidden list) and adds it to the
   unset list.  Returns SESSION."
  (remhash name (session-environment session))
  (setf (session-environment-hidden session)
        (delete name (session-environment-hidden session) :test #'string=))
  (pushnew name (session-environment-unsets session) :test #'string=)
  session)

(defvar *global-hidden-environment-names* nil
  "Names marked hidden via set-environment -hg (tmux ENVIRON_HIDDEN on the
   global environment).  cl-tmux maps the global environment onto the real
   process environment, so hidden globals are tracked here and stripped from
   child-process environments and plain show-environment listings.")
