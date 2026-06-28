(in-package #:cl-tmux/repository)

;;;; Session Repository — domain-side protocol for session persistence.
;;;;
;;;; The domain defines what operations a session store must support.
;;;; Infrastructure (bootstrap/session-registry.lisp, in the cl-tmux package)
;;;; implements the protocol using an in-memory alist backed by *server-sessions*.
;;;;
;;;; DDD pattern:
;;;;   Repository interface (here, domain layer) — "what operations exist"
;;;;   Repository implementation (bootstrap) — "how they are implemented"
;;;;
;;;; Aggregate root: Session is the root of the Session aggregate.
;;;; Windows and Panes are accessed only through their owning Session.
;;;; External code that needs a Window or Pane first resolves the Session.

;;; ── Protocol ─────────────────────────────────────────────────────────────────
;;;
;;; These generic functions define the contract.  The in-memory implementation
;;; in cl-tmux::server-* functions satisfies this contract.

(defgeneric repo-find-session (store name)
  (:documentation
   "Return the session named NAME from STORE, or NIL when not found.
    Supports exact name, $N id-notation, and prefix-match — same semantics
    as the tmux -t flag for sessions."))

(defgeneric repo-add-session (store session)
  (:documentation
   "Register SESSION in STORE under (session-name session).
    Replaces any prior entry with the same name."))

(defgeneric repo-remove-session (store name)
  (:documentation
   "Remove the session named NAME from STORE.  No-op when not found."))

(defgeneric repo-all-sessions (store)
  (:documentation
   "Return a list of all sessions currently in STORE."))

(defgeneric repo-current-session (store)
  (:documentation
   "Return the most recently active session (highest session-last-active).
    Returns NIL when STORE is empty."))

;;; ── Active repository ────────────────────────────────────────────────────────
;;;
;;; Domain code that needs the repository calls through *session-repo*.
;;; The composition root (server startup) sets this to the concrete implementation.

(defvar *session-repo* nil
  "The active session repository.  Set by install-session-repository at server
   startup to a concrete store object.  Domain queries go through repo-* generics
   on this value.")
