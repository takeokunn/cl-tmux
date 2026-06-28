(in-package #:cl-tmux/options)

;;;; Scope-resolution helpers for the option API.
;;;;
;;;; This file owns the mapping from SCOPE keyword (:server / nil / :window)
;;;; to the concrete hash-tables, plus all array-option name parsing and
;;;; spec-lookup logic.  It is loaded before options-api.lisp, which depends
;;;; on the helpers defined here.

;;; ── User-option predicate + error helper ─────────────────────────────────

(defun %user-option-name-p (name)
  "Return true for tmux user options, whose names begin with @."
  (and (stringp name)
       (plusp (length name))
       (char= #\@ (char name 0))))

(defun %invalid-option-error (name)
  "Signal tmux-compatible invalid option error for NAME."
  (error "invalid option: ~A" name))

;;; ── Scope dispatch helpers ───────────────────────────────────────────────
;;;
;;; The :server / nil (session) duality appears in three parallel pairs of
;;; hash-tables.  All callers should go through these one-liners so that
;;; adding a third scope only changes these four functions.

(defun %scope-options (scope)
  "Return the runtime option table for SCOPE (:server or session/nil)."
  (if (eq scope :server) *server-options* *global-options*))

(defun %scope-registry (scope)
  "Return the runtime spec registry for SCOPE (:server or session/nil)."
  (if (eq scope :server) *server-option-registry* *option-registry*))

(defun %scope-known-registry (scope)
  "Return the stable tmux 3.6a spec registry for SCOPE (:server or session/nil)."
  (if (eq scope :server) *known-server-option-registry* *known-option-registry*))

(defun %scope-triple (scope)
  "Return three values: (options registry known-registry) for SCOPE.
   Eliminates the repeated triple-call pattern seen in %option-spec-for-name callers."
  (values (%scope-options scope)
          (%scope-registry scope)
          (%scope-known-registry scope)))

;;; ── Array-option name parsing ─────────────────────────────────────────────
;;;
;;; tmux array options use the BASE[N] naming convention (e.g. \"command-alias[0]\").
;;; These predicates and parsers are the single authoritative layer for that syntax.

(defun %decimal-digits-p (string start end)
  "Return T when STRING[START..END) is a non-empty decimal digit sequence."
  (and (< start end)
       (loop for index from start below end
             always (digit-char-p (char string index)))))

(defun %array-entry-index-for-base (base name)
  "Return the numeric index when NAME is BASE[N], otherwise NIL."
  (let* ((prefix (concatenate 'string base "["))
         (prefix-len (length prefix))
         (name-len (length name)))
    (when (and (stringp base)
               (stringp name)
               (> name-len prefix-len)
               (string= prefix name
                        :start1 0 :end1 prefix-len
                        :start2 0 :end2 prefix-len)
               (char= (char name (1- name-len)) #\])
               (%decimal-digits-p name prefix-len (1- name-len)))
      (cl-tmux::%parse-integer-or-nil name
                                      :start prefix-len
                                      :end (1- name-len)))))

(defun %array-entry-base-name (name)
  "Return BASE when NAME is BASE[N], otherwise NIL."
  (when (stringp name)
    (let ((open  (position #\[ name :from-end t))
          (close (position #\] name :from-end t)))
      (when (and open
                 close
                 (= close (1- (length name)))
                 (< open close)
                 (%decimal-digits-p name (1+ open) close))
        (subseq name 0 open)))))

(defun %table-has-array-base-p (base table)
  "Return T when TABLE contains at least one key of the form BASE[N].
   Works for both spec-registry hash-tables and runtime-options hash-tables."
  (let ((foundp nil))
    (maphash (lambda (name _value)
               (declare (ignore _value))
               (when (%array-entry-index-for-base base name)
                 (setf foundp t)))
             table)
    foundp))

(defun array-option-indexed-name-p (name &optional scope)
  "Return true when NAME is an indexed array option entry (e.g. \"foo[0]\").
   The indexed form is determined structurally by the BASE[N] suffix; SCOPE is
   accepted for API symmetry but the structural check is sufficient.
   Used by %scope-unset to decide whether to blank the entry (indexed) or remove it."
  (declare (ignore scope))
  (not (null (%array-entry-base-name name))))

(defun %array-option-p (name scope)
  "Return true when NAME is a tmux array-option base (not an indexed entry) in SCOPE.
   Checks registered specs and the runtime options table."
  (and (stringp name)
       (not (%array-entry-base-name name))
       (or (%table-has-array-base-p name (%scope-registry scope))
           (%table-has-array-base-p name (%scope-known-registry scope))
           (%table-has-array-base-p name (%scope-options scope)))))

;;; ── Spec lookup helpers ───────────────────────────────────────────────────

(defun %exact-option-spec-for-scope (name scope)
  "Return only an exact spec match for NAME in SCOPE, or NIL."
  (or (gethash name (%scope-registry scope))
      (gethash name (%scope-known-registry scope))))

(defun %find-spec-by-array-prefix (base table)
  "Return the first spec in TABLE whose key has the form BASE[N], or NIL."
  (let ((found nil))
    (maphash (lambda (registered-name registered-spec)
               (when (and (null found)
                          (%array-entry-index-for-base base registered-name))
                 (setf found registered-spec)))
             table)
    found))

(defun %array-template-spec-for-name (name registry known-registry)
  "Return the spec used to type-check an array entry NAME.
   Tries (in order): exact base key in registry, exact base in known-registry,
   then a sibling BASE[N] entry in registry, then one in known-registry."
  (let ((base (%array-entry-base-name name)))
    (when base
      (or (gethash base registry)
          (gethash base known-registry)
          (%find-spec-by-array-prefix base registry)
          (%find-spec-by-array-prefix base known-registry)))))

(defun %option-spec-for-name (name registry known-registry)
  "Return the exact spec for NAME, or an array-entry template spec, or NIL."
  (or (gethash name registry)
      (gethash name known-registry)
      (%array-template-spec-for-name name registry known-registry)))

;;; ── Presence / settability predicates ────────────────────────────────────

(defun option-present-for-scope-p (name &optional scope)
  "Return true when NAME is valid or present in SCOPE (for set-option routing)."
  (or (%user-option-name-p name)
      (nth-value 1 (gethash name (%scope-options scope)))
      (%exact-option-spec-for-scope name scope)
      (%array-option-p name scope)
      (and (%array-entry-base-name name)
           (%option-spec-for-name name
                                  (%scope-registry scope)
                                  (%scope-known-registry scope)))))

(defun option-present-for-display-p (name &optional scope)
  "Return true when NAME may be shown by show-option/show-options in SCOPE."
  (if (%user-option-name-p name)
      (nth-value 1 (gethash name (%scope-options scope)))
      (option-present-for-scope-p name scope)))

(defun option-settable-for-scope-p (name &optional scope)
  "Return true when NAME is a valid option that may be SET at SCOPE.
   :server scope accepts server-registry options only; nil/session scope
   accepts session/window registry options.  User options (@name) are
   always accepted in any scope."
  (or (%user-option-name-p name)
      (not (null (%option-spec-for-name name
                                        (%scope-registry scope)
                                        (%scope-known-registry scope))))))
