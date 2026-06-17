(in-package #:cl-tmux)

;;;; server-access ACL commands.

;;; ── server-access ──────────────────────────────────────────────────────────
;;; tmux's server-access maintains an access-control list for the (multi-user)
;;; server socket.  cl-tmux is single-user and does not share its server over a
;;; socket, so the list gates nothing — but modelling it faithfully lets a
;;; `.tmux.conf` `server-access` directive load and round-trip, and lets the
;;; behaviour be verified (add/delete/modify/list) like any other command.

(defvar *server-access-list* nil
  "Alist of (username . permission), permission being :read-write or :read-only.
   The server access-control list managed by the `server-access` command.  Front
   of the list is the most-recently-added user; %format-server-access-list emits
   them in insertion order (oldest first).")

(defun %format-server-access-list ()
  "Render *server-access-list* as one `name: permission` line per entry, in
   insertion order.  Empty list yields a single explanatory line."
  (if (null *server-access-list*)
      "server-access: no entries"
      (with-output-to-string (s)
        (loop for (name . perm) in (reverse *server-access-list*)
              do (format s "~A: ~(~A~)~%" name perm)))))

(defun %server-access-delete-user (user)
  "Remove USER from *server-access-list* and emit the standard overlay."
  (setf *server-access-list*
        (remove user *server-access-list* :key #'car :test #'string=))
  (%overlayf "server-access: removed ~A" user)
  t)

(defun %server-access-upsert-user (user perm addp)
  "Modify USER if present, add it when ADDP is true, or report an unknown user."
  (let ((entry (assoc user *server-access-list* :test #'string=)))
    (cond
      (entry
       (when perm
         (setf (cdr entry) perm))
       t)
      (addp
       (push (cons user (or perm :read-write)) *server-access-list*)
       t)
      (t
       (%overlayf "server-access: unknown user ~A" user)
       nil))))

(defun %cmd-server-access (session args)
  "server-access [-l] [-a|-d] [-r|-w] [user]: manage the server access list.
   -l       list the current access entries (name -> permission); also the
            default when no user and no -a/-d is given.
   -a user  add USER (read-write by default, read-only when -r is also given).
   -d user  remove USER from the access list.
   -r / -w  set the permission to read-only / read-write when adding or modifying.
   A bare `server-access -r user` (no -a/-d) modifies an existing entry; modifying
   an unknown user is an error, matching tmux.  See *server-access-list*."
  (declare (ignore session))
  (with-command-input (flags positionals args ""
                       :allowed-flags '(#\l #\a #\d #\r #\w)
                       :max-positionals 1
                       :message "server-access: unsupported argument")
    (let* ((listp (assoc #\l flags))
           (addp  (assoc #\a flags))
           (delp  (assoc #\d flags))
           (perm  (cond ((assoc #\r flags) :read-only)
                        ((assoc #\w flags) :read-write)
                        (t nil)))
           (user  (first positionals)))
      (cond
        ;; -l, or no actionable arguments: list.
        ((or listp (and (null addp) (null delp) (null user)))
         (show-overlay (%format-server-access-list))
         t)
        ;; -d user: remove (a no-op if absent, like tmux).
        ((and delp user)
         (%server-access-delete-user user))
        ;; -a user, or bare `user` with -r/-w: add or modify.
        (user
         (when (%server-access-upsert-user user perm addp)
           (%overlayf "server-access: ~A -> ~(~A~)" user
                      (cdr (assoc user *server-access-list* :test #'string=)))
           t))
        (t nil)))))
