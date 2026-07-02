(in-package #:cl-tmux/options)

;;; Global option storage

(defvar *global-options* (make-hash-table :test #'equal)
  "Hash-table mapping option name strings to their current values.")

(defvar *server-options* (make-hash-table :test #'equal)
  "Hash-table for server-scoped options (set-option -s).
   Keys: escape-time, exit-empty, exit-unattached.")

;;; Option specification

(defstruct option-spec
  "Describes one tmux option: its name, type keyword, and default value."
  name
  type
  default)

(defvar *option-registry* (make-hash-table :test #'equal)
  "Hash-table mapping option name strings to OPTION-SPEC instances.
   This is the mutable runtime registry; keys may be removed via set-option -u.")

(defvar *known-option-registry* (make-hash-table :test #'equal)
  "Read-only registry of built-in tmux option specs (populated by define-tmux-options).
   Used as a fallback source of defaults when a key is absent from *option-registry*,
   mirroring tmux's options_remove_or_default behaviour for `set-option -u`.")

;;; ── Unified option-table registration macro ───────────────────────────────
;;;
;;; define-option-table encapsulates the two-phase expand pattern shared by
;;; define-tmux-options and define-server-options: registering spec metadata
;;; (immutable after load) and initialising runtime default values.
;;;
;;; Parameters:
;;;   REGISTRY-VAR  — the *-option-registry* hash-table to receive specs
;;;   STORAGE-VAR   — the *-options* hash-table to receive runtime defaults
;;;   SPECS         — list of (name type default) triples

(defmacro define-option-table (registry-var storage-var &rest specs)
  "Register option specs in REGISTRY-VAR and initialise STORAGE-VAR with defaults.
   Each SPEC has the form (name type default) where TYPE is :boolean, :integer,
   or :string.  Phase 1 stores spec metadata; phase 2 stores runtime defaults."
  `(progn
     ;; Phase 1: register spec metadata (immutable after load)
     ,@(mapcar (lambda (spec)
                 (destructuring-bind (name type default) spec
                   `(setf (gethash ,name ,registry-var)
                          (make-option-spec :name ,name
                                            :type ,type
                                            :default ,default))))
               specs)
     ;; Phase 2: initialise runtime default values
     ,@(mapcar (lambda (spec)
                 (destructuring-bind (name _type default) spec
                   (declare (ignore _type))
                   `(setf (gethash ,name ,storage-var) ,default)))
               specs)))

;;; ── Convenience wrappers for the two standard option tables ───────────────

(defmacro define-tmux-options (&rest specs)
  "Register tmux options in *OPTION-REGISTRY* and *KNOWN-OPTION-REGISTRY* (spec
   metadata) and initialise *GLOBAL-OPTIONS* with their defaults (runtime state).
   Each SPEC has the form (name type default) where TYPE is :boolean, :integer,
   or :string.  *KNOWN-OPTION-REGISTRY* is the immutable fallback used by
   get-option when a key has been removed from *OPTION-REGISTRY* via set-option -u."
  `(progn
     (define-option-table *option-registry* *global-options* ,@specs)
     ;; Populate the known (immutable) registry so set-option -u can fall back to defaults.
     ,@(mapcar (lambda (spec)
                 (destructuring-bind (name type default) spec
                   `(setf (gethash ,name *known-option-registry*)
                          (make-option-spec :name ,name :type ,type :default ,default))))
               specs)))

(defmacro define-server-options (&rest specs)
  "Register server options in *SERVER-OPTION-REGISTRY* and *KNOWN-SERVER-OPTION-REGISTRY*
   (spec metadata) and initialise *SERVER-OPTIONS* with their defaults.  Each SPEC has
   the form (name type default).  *KNOWN-SERVER-OPTION-REGISTRY* is the immutable fallback."
  `(progn
     (define-option-table *server-option-registry* *server-options* ,@specs)
     ;; Populate the known (immutable) registry.
     ,@(mapcar (lambda (spec)
                 (destructuring-bind (name type default) spec
                   `(setf (gethash ,name *known-server-option-registry*)
                          (make-option-spec :name ,name :type ,type :default ,default))))
               specs)))

;;; Server-option registry and defaults

(defvar *server-option-registry* (make-hash-table :test #'equal)
  "Mutable runtime registry for server-scoped option specs (set with set-option -s).")

(defvar *known-server-option-registry* (make-hash-table :test #'equal)
  "Read-only registry of built-in tmux server option specs (populated by define-server-options).
   Used as a fallback source of defaults when a key is absent from *server-option-registry*.")

;;; The registered-option data tables (define-tmux-options / define-server-options
;;; invocations) live in options-registry-data.lisp, loaded immediately after this
;;; file so the macros above are already defined.
