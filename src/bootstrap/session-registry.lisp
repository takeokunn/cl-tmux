(in-package #:cl-tmux)

;;;; Session registry and session-group management.
;;;;
;;;; *server-sessions* is the authoritative registry of all live sessions
;;;; (defvar lives in runtime.lisp so dispatch.lisp can reference it before
;;;; server loads).  run-server initialises it with the single initial session;
;;;; new-session (in dispatch.lisp) adds further sessions; kill-session removes
;;;; them.
;;;;
;;;; Session groups allow multiple sessions to share the same window list.
;;;; Sessions in the same group see the same windows; switching a window in one
;;;; automatically switches all others in the group.

;;; ── In-memory session store (concrete repository) ────────────────────────────
;;;
;;; Implements the cl-tmux/repository protocol by delegating to the global
;;; *server-sessions* alist already maintained by server-* functions.
;;; Wired into *session-repo* at server startup via install-session-repository.

(defstruct in-memory-session-store
  "Concrete Repository implementation backed by *server-sessions*.")

(defmethod cl-tmux/repository:repo-find-session
    ((store in-memory-session-store) name)
  (server-find-session name))

(defmethod cl-tmux/repository:repo-add-session
    ((store in-memory-session-store) session)
  (server-add-session session))

(defmethod cl-tmux/repository:repo-remove-session
    ((store in-memory-session-store) name)
  (server-remove-session name))

(defmethod cl-tmux/repository:repo-all-sessions
    ((store in-memory-session-store))
  (server-all-sessions))

(defmethod cl-tmux/repository:repo-current-session
    ((store in-memory-session-store))
  (server-current-session))

(defun install-session-repository ()
  "Register the in-memory session store as the active cl-tmux/repository adapter."
  (setf cl-tmux/repository:*session-repo* (make-in-memory-session-store)))

;;; ── Session registry ──────────────────────────────────────────────────────────

(defun server-add-session (session)
  "Register SESSION in *server-sessions* keyed by (session-name session).
   If a session with the same name already exists it is replaced."
  (setf *server-sessions*
        (cons (cons (session-name session) session)
              (remove (session-name session) *server-sessions*
                      :key #'car :test #'string=))))

(defun server-find-session (name)
  "Find a session by NAME in *server-sessions*.
   Match order:
     1. Exact name match
     2. $N notation (session id)
     3. Name prefix match (first matching session wins)
   Returns the session or NIL."
  (when (and name (plusp (length name)))
    (or
     ;; 1. Exact name match
     (cdr (assoc name *server-sessions* :test #'string=))
     ;; 2. $N: match by session id
     (when (char= (char name 0) #\$)
       (let ((id (%parse-integer-or-nil (subseq name 1))))
         (when id
           (find id (mapcar #'cdr *server-sessions*) :key #'session-id))))
     ;; 3. Name prefix match
     (loop for (key . sess) in *server-sessions*
           when (and (stringp key)
                     (>= (length key) (length name))
                     (string= name key :end2 (length name)))
             return sess))))

(defun server-current-session ()
  "Return the most recently active session (highest session-last-active).
   Returns NIL when no sessions are registered."
  (let ((sessions (mapcar #'cdr *server-sessions*)))
    (when sessions
      (reduce (lambda (a b)
                (if (> (session-last-active b) (session-last-active a)) b a))
              sessions))))

(defun server-remove-session (name)
  "Remove the session named NAME from *server-sessions*."
  (setf *server-sessions*
        (remove name *server-sessions* :key #'car :test #'string=)))

(defun server-all-sessions ()
  "Return a list of all active sessions."
  (mapcar #'cdr *server-sessions*))

;;; ── Session groups ────────────────────────────────────────────────────────────
;;;
;;; A session group is a set of sessions that share the same window list.
;;; Sessions in the same group see the same windows; switching window in one
;;; automatically switches all others in the group.

(defparameter *session-groups* nil
  "Alist mapping group-id to list of sessions in that group.")

(defvar *group-id-counter* 0
  "Monotonic counter for session group ids. Never decremented — id reuse is impossible.")

(defun %next-group-id ()
  "Allocate a fresh group-id via a monotonic counter (never derives from alist length)."
  (incf *group-id-counter*))

;;; ── Session group helpers (data / logic decomposed) ──────────────────────────
;;;
;;; server-new-session-in-group has four distinct phases:
;;;   1. Allocate/reuse a group-id   (pure: id derivation)
;;;   2. Link session structs         (mutation: windows + group slot — data only)
;;;   3. Sync active window           (logic: policy that new session mirrors existing)
;;;   4. Update the registry alist    (mutation: *session-groups*)
;;; Each phase is extracted into a named sub-function so it is independently
;;; readable and testable.

(defun %resolve-group-id (session)
  "Return SESSION's existing group-id, or allocate a fresh one and install it."
  (or (session-group session)
      (let ((group-id (%next-group-id)))
        (setf (session-group session) group-id)
        group-id)))

(defun %link-session-to-group (new-session existing-session group-id)
  "Share EXISTING-SESSION's windows into NEW-SESSION and assign GROUP-ID.
   Pure data wiring only — does not select the active window (see %sync-active-window)."
  (setf (session-windows new-session) (session-windows existing-session)
        (session-group   new-session) group-id))

(defun %sync-active-window (new-session existing-session)
  "Mirror EXISTING-SESSION's active window selection into NEW-SESSION.
   This is the policy step (new session follows existing session's view)
   and is intentionally separate from the data-linkage in %link-session-to-group."
  (let ((active-window (session-active-window existing-session)))
    (when active-window
      (session-select-window new-session active-window))))

(defun %register-in-group-alist (session group-id)
  "Add SESSION to the *session-groups* alist under GROUP-ID."
  (let ((entry (assoc group-id *session-groups*)))
    (if entry
        (pushnew session (cdr entry))
        (push (list group-id session) *session-groups*))))

(defun server-new-session-in-group (new-session existing-session)
  "Add NEW-SESSION to the same session group as EXISTING-SESSION.
   If EXISTING-SESSION is not yet in a group, a new group is created.
   Both sessions will share the same window list."
  (let ((group-id (%resolve-group-id existing-session)))
    (%link-session-to-group new-session existing-session group-id)
    (%sync-active-window new-session existing-session)
    (%register-in-group-alist existing-session group-id)
    (%register-in-group-alist new-session group-id)))
