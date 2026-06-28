(in-package #:cl-tmux/model)

;;; ── Environment management ──────────────────────────────────────────────────
;;;
;;; This file holds all session environment management code split from session.lisp.
;;;
;;; Concerns:
;;;   process env     — read/write the server process's own POSIX environment
;;;   update-env      — *update-environment* list of names propagated to new panes
;;;   session overlay — per-session set/unset tables in the session struct
;;;   child env       — the merged snapshot passed to shell spawn via RUN-PROGRAM
;;;
;;; Data/logic separation:
;;;   Pure helpers (%environment-entry-name, %environment-strings-to-table, etc.)
;;;   operate only on their arguments; callers supply the live data sources.
;;;   %with-posix-env-op centralises the set/unset pattern.
;;;   %apply-session-overlay and %apply-extra-env isolate the session and
;;;   extra-env merge steps from the orchestrating session-child-environment.

;;; ── update-environment defaults ─────────────────────────────────────────────

(defparameter +default-update-environment+
  '("DISPLAY" "SSH_AUTH_SOCK" "SSH_CONNECTION" "XAUTHORITY")
  "Default update-environment variable names used for new sessions and
   as the reset target when the option is unset.")

(defparameter *update-environment*
  (copy-list +default-update-environment+)
  "List of environment variable names to propagate into new panes.
   Mirrors tmux's update-environment server option.  Used as a fallback when
   the option string has not been set.")

;;; ── Process-level POSIX environment helpers ──────────────────────────────────

(defun get-update-environment-vars ()
  "Return an alist of (name . value) for each variable in the update-environment
   option that is set in the current process environment.  Unset vars are omitted."
  (loop for name in *update-environment*
        for value = (ignore-errors (sb-ext:posix-getenv name))
        when value collect (cons name value)))

(defun %process-posix-fn (name)
  "Look up the symbol named NAME in SB-POSIX lazily.
   Returns the symbol (callable as a function) when SB-POSIX is available,
   or NIL when the package has not been loaded yet."
  (let ((pkg (find-package "SB-POSIX")))
    (and pkg (find-symbol name pkg))))

(defun process-environment-value (name)
  "Return the current process environment value for NAME, or NIL when unset."
  (ignore-errors (sb-ext:posix-getenv name)))

(defun process-environment-names ()
  "Return the sorted list of variable names present in the current process environment."
  (let ((names nil))
    (dolist (entry (ignore-errors (sb-ext:posix-environ)))
      (let ((name (%environment-entry-name entry)))
        (when name
          (pushnew name names :test #'string=))))
    (sort names #'string<)))

;;; ── %with-posix-env-op — shared skeleton for set/unset ──────────────────────
;;;
;;; Both process-set-environment and process-unset-environment share an identical
;;; three-step skeleton: (1) validate name, (2) call the SB-POSIX function,
;;; (3) update the hidden-names tracking list.  The macro captures this once.

(defmacro %with-posix-env-op ((name posix-fn-name) &body call-args)
  "Assert NAME is a non-empty string, then call the SB-POSIX function named
   POSIX-FN-NAME (looked up lazily) with CALL-ARGS, ignoring errors.
   Expands to a progn so callers can append their own return form."
  `(progn
     (%assert-environment-variable-name ,name)
     (let ((%posix-fn (%process-posix-fn ,posix-fn-name)))
       (when %posix-fn
         (ignore-errors (funcall %posix-fn ,@call-args))))))

(defun process-set-environment (name value)
  "Set NAME=VALUE in the current process environment when SB-POSIX is available.
   NAME must be a non-empty string without '='.  Returns VALUE."
  (%with-posix-env-op (name "SETENV") name value 1)
  value)

(defun process-unset-environment (name)
  "Remove NAME from the current process environment when SB-POSIX is available.
   NAME must be a non-empty string without '='.  Returns NAME."
  (%with-posix-env-op (name "UNSETENV") name)
  name)

;;; ── NAME=VALUE string pair helpers ──────────────────────────────────────────

(defun %environment-entry-name (entry)
  "Return the NAME component of a NAME=VALUE environment ENTRY string, or NIL."
  (let ((eq-pos (position #\= entry)))
    (when eq-pos
      (subseq entry 0 eq-pos))))

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

(defun %env-name-matches-visibility-p (session name hidden)
  "Return T when NAME in SESSION's overlay matches the HIDDEN visibility filter.
   HIDDEN T selects names that are in the hidden set; NIL selects visible names.
   Used by session-environment-names to partition names."
  (declare (ignore hidden))
  ;; Currently all names exposed via the overlay are treated equally visible.
  ;; This hook exists for future per-name visibility tracking.
  (declare (ignore session name))
  t)

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

(defun session-set-environment (session name value)
  "Store NAME=VALUE in SESSION's environment overlay.
   Removes NAME from the unset list if it was explicitly unset before.
   Returns SESSION."
  (setf (session-environment-unsets session)
        (delete name (session-environment-unsets session) :test #'string=))
  (setf (gethash name (session-environment session)) value)
  session)

(defun session-unset-environment (session name)
  "Record NAME as explicitly unset in SESSION's environment overlay.
   Removes NAME from the set table and adds it to the unset list.
   Returns SESSION."
  (remhash name (session-environment session))
  (pushnew name (session-environment-unsets session) :test #'string=)
  session)

;;; ── Child environment snapshot ───────────────────────────────────────────────
;;;
;;; session-child-environment merges 5 sources in order.  The two inner merge
;;; steps (%apply-session-overlay and %apply-extra-env) are extracted into named
;;; helpers so each concern is independently testable.

(defvar *suppress-update-environment* nil
  "When non-NIL, session-child-environment SKIPS applying the update-environment
   variables (merge step 2).  Bound to T around new-session -E so the created
   session — including its initial pane — does not pick up update-environment,
   matching tmux's `new-session -E`.")

(defun %apply-session-overlay (session table)
  "Merge SESSION's environment overlay into TABLE (mutates TABLE in place).
   Applies the set table first, then removes explicitly unset names.
   When SESSION is NIL this is a no-op (bootstrap / geometry-only callers)."
  (when session
    (maphash (lambda (name value)
               (setf (gethash name table) value))
             (session-environment session))
    (dolist (name (session-environment-unsets session))
      (remhash name table))))

(defun %apply-extra-env (extra-env table)
  "Merge EXTRA-ENV (an alist of (NAME . VALUE) conses) into TABLE.
   Entries that are not proper (string . string) conses are silently skipped."
  (dolist (pair extra-env)
    (when (and (consp pair)
               (stringp (car pair))
               (stringp (cdr pair)))
      (setf (gethash (car pair) table) (cdr pair)))))

(defun session-child-environment (session &key term extra-env)
  "Return a full child environment snapshot for SESSION as a list of NAME=VALUE strings.
   The merge order is:
     1. current process environment (base)
     2. update-environment variables from the current process
        (skipped when *suppress-update-environment* is non-NIL — new-session -E)
     3. SESSION overlay sets and unsets
     4. TERM override, when TERM is a non-empty string
     5. EXTRA-ENV alist of (NAME . VALUE), when supplied
   SESSION may be NIL for bootstrap or pure geometry helpers — step 3 is skipped.
   The result is suitable for passing as :environment to sb-ext:run-program."
  ;; Step 1: base from current process.
  (let ((table (%environment-strings-to-table (sb-ext:posix-environ))))
    ;; Step 2: update-environment propagation (new-session -E suppresses this).
    (unless *suppress-update-environment*
      (dolist (pair (get-update-environment-vars))
        (setf (gethash (car pair) table) (cdr pair))))
    ;; Step 3: session overlay.
    (%apply-session-overlay session table)
    ;; Step 4: TERM override.
    (when (and term (plusp (length term)))
      (setf (gethash "TERM" table) term))
    ;; Step 5: extra per-call environment.
    (%apply-extra-env extra-env table)
    (%environment-table-to-list table)))
