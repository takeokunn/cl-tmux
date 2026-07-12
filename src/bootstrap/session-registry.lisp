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

;;; ── Session registry ──────────────────────────────────────────────────────────

(defun server-add-session (session)
  "Register SESSION in *server-sessions* keyed by (session-name session).
   If a session with the same name already exists it is replaced."
  (setf *server-sessions*
        (cons (cons (session-name session) session)
              (remove (session-name session) *server-sessions*
                      :key #'car :test #'string=))))

(defun %find-session-by-exact-name (name)
  "Return the session whose registry key exactly matches NAME, or NIL."
  (cdr (assoc name *server-sessions* :test #'string=)))

(defun %find-session-by-id-notation (name)
  "Return the session referenced by $N notation in NAME, or NIL."
  (when (char= (char name 0) #\$)
    (let ((id (%parse-integer-or-nil (subseq name 1))))
      (when id
        (find id (mapcar #'cdr *server-sessions*) :key #'session-id)))))

(defun %find-session-by-prefix (name)
  "Return the first session whose registry key has NAME as a prefix, or NIL."
  (loop for (key . sess) in *server-sessions*
        when (and (stringp key)
                  (>= (length key) (length name))
                  (string= name key :end2 (length name)))
          return sess))

(defun server-find-session (name)
  "Find a session by NAME in *server-sessions*.
   Match order:
     1. Exact name match
     2. $N notation (session id)
     3. Name prefix match (first matching session wins)
   Returns the session or NIL."
  (when (and name (plusp (length name)))
    (or
     (%find-session-by-exact-name name)
     (%find-session-by-id-notation name)
     (%find-session-by-prefix name))))

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

(defun %sync-group-session-windows (session)
  "Mirror SESSION's window list to every other live session in its group.
   tmux session groups share ONE window set: creating, killing, moving, or
   renumbering a window in any grouped session must be reflected in all of them.
   The mirror is a plain slot copy (no session-windows-changed re-entry, so no
   recursion).  A peer whose active window vanished from the shared set falls
   back to the first remaining window — a pure focus repair, deliberately not
   session-select-window (no timestamp/flag side effects on a passive peer)."
  (let* ((group-id (cl-tmux/model:session-group session))
         (entry    (and group-id (assoc group-id *session-groups*))))
    (when entry
      (dolist (peer (cdr entry))
        (%sync-peer-session-windows peer session)))))

;;; Install the group fan-out as the model layer's window-sync policy.  Unit
;;; tests that build sessions directly are unaffected: sessions without a group
;;; (or absent from *session-groups*) make the sync a no-op.
(defun %sync-peer-session-windows (peer session)
  (unless (eq peer session)
    (setf (session-windows peer) (session-windows session))
    (unless (member (cl-tmux/model:session-active peer)
                    (session-windows peer))
      (setf (cl-tmux/model:session-active peer)
            (first (session-windows peer))))))

(setf cl-tmux/model:*session-windows-sync-function* #'%sync-group-session-windows)
