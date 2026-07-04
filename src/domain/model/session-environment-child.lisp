(in-package #:cl-tmux/model)

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
   Applies the set table first, then removes explicitly unset names and any
   hidden names (tmux: hidden variables are not passed to new processes).
   When SESSION is NIL this is a no-op (bootstrap / geometry-only callers)."
  (when session
    (maphash (lambda (name value)
               (setf (gethash name table) value))
             (session-environment session))
    (dolist (name (session-environment-unsets session))
      (remhash name table))
    (dolist (name (session-environment-hidden session))
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
    ;; Hidden globals (set-environment -hg) never reach child processes; they
    ;; live in the real process environment (step 1) so strip them here.
    (dolist (name *global-hidden-environment-names*)
      (remhash name table))
    ;; Step 3: session overlay.
    (%apply-session-overlay session table)
    ;; Step 4: TERM override.
    (when (and term (plusp (length term)))
      (setf (gethash "TERM" table) term))
    ;; Step 5: extra per-call environment.
    (%apply-extra-env extra-env table)
    (%environment-table-to-list table)))
