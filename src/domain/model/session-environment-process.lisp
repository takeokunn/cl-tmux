(in-package #:cl-tmux/model)

;;; ── Process environment ─────────────────────────────────────────────────────
;;;
;;; Process-level environment access is separated from the session overlay and
;;; child-environment assembly concerns so each step stays independently testable.

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

(defun process-environment-value (name)
  "Return NAME's value from the live process environment, or NIL when unset."
  (%assert-environment-variable-name name)
  (ignore-errors (sb-ext:posix-getenv name)))

(defun %environment-entry-name (entry)
  "Return the NAME component of a NAME=VALUE environment ENTRY string, or NIL.
   Shared with session-environment-overlay.lisp (loaded after this file),
   which also defines the matching %ENVIRONMENT-ENTRY-VALUE."
  (let ((eq-pos (position #\= entry)))
    (when eq-pos
      (subseq entry 0 eq-pos))))

(defun process-environment-names ()
  "Return sorted names from the live process environment."
  (let (names)
    (dolist (entry (ignore-errors (sb-ext:posix-environ)))
      (let ((name (%environment-entry-name entry)))
        (when name
          (pushnew name names :test #'string=))))
    (sort names #'string<)))

(defun get-update-environment-vars ()
  "Return an alist of (name . value) for each variable in the update-environment
   option that is set in the current process environment.  Unset vars are omitted."
  (loop for name in *update-environment*
        for value = (ignore-errors (sb-ext:posix-getenv name))
        when value collect (cons name value)))

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
     (let ((%posix-fn (find-posix-function ,posix-fn-name)))
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
