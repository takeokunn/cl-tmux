(in-package #:cl-tmux)

;;;; server-access ACL commands and customize-mode tree browser.

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

;;; ── customize-mode ─────────────────────────────────────────────────────────
;;; tmux's customize-mode opens an interactive tree of every option / hook / key
;;; binding for editing in place.  cl-tmux renders it as a read-only customize
;;; tree overlay — the same depth as :choose-tree / :list-keys (the other "mode"
;;; commands here are informational overlays, not j/k-navigable panes); values
;;; are changed with set-option / bind-key.  The grouping (Server / Session+Window
;;; Options / Key Bindings) mirrors tmux's customize-mode categories so the same
;;; mental model and the same names appear.

(defun %customize-match-p (str filter)
  "T when FILTER is NIL or a case-insensitive substring of STR.  Used to filter
   the customize tree by customize-mode's -f option."
  (or (null filter)
      (search (string-downcase filter) (string-downcase str))))

(defun %customize-split-lines (text)
  "Split TEXT into a list of its lines (newline-separated), dropping the trailing
   empty line.  Used to filter pre-rendered multi-line blocks (key bindings)."
  (with-input-from-string (in text)
    (loop for line = (read-line in nil nil) while line collect line)))

(defun %customize-value-string (value)
  "Render an option VALUE for the customize tree: T->on, NIL->off, strings as-is,
   everything else via princ (mirrors cl-tmux/options' show-options formatter)."
  (cond ((eq value t) "on")
        ((null value) "off")
        ((stringp value) value)
        (t (princ-to-string value))))

(defun %customize-emit-options (s title ht-pairs &optional filter)
  "Write one customize-tree section to S, preserving the existing name sort and
   filtering behavior for options and hooks."
  (let ((shown (sort (remove-if-not
                      (lambda (p) (%customize-match-p (car p) filter))
                      ht-pairs)
                     #'string< :key #'car)))
    (when shown
      (format s "~A:~%" title)
      (dolist (p shown)
        (format s "  ~A: ~A~%"
                (car p) (%customize-value-string (cdr p)))))))

(defun %format-customize-tree (&optional filter)
  "Render the customize tree as an overlay string: Server Options, then
   Session/Window Options (each `  name: value`, name-sorted), then Key Bindings,
   restricted to entries matching FILTER (substring, case-insensitive).  A group
   with no surviving entries is omitted entirely."
  (with-output-to-string (s)
    (let (server-pairs)
      (maphash (lambda (k v) (push (cons k v) server-pairs))
               cl-tmux/options::*server-options*)
      (%customize-emit-options s "Server Options" server-pairs filter))
    (%customize-emit-options s "Session/Window Options"
                             (cl-tmux/options:all-options) filter)
    ;; Key bindings: filter the pre-rendered describe-key-bindings block by line.
    (let ((lines (remove-if
                  (lambda (l) (or (string= l "")
                                  (not (%customize-match-p l filter))))
                  (%customize-split-lines (cl-tmux/config:describe-key-bindings)))))
      (when lines
        (format s "Key Bindings:~%")
        (dolist (l lines) (format s "  ~A~%" l))))))

(defun %cmd-customize-mode (session args)
  "customize-mode [-f filter]: show the customize tree (options + key bindings)
   in an overlay.  -f FILTER limits the tree to entries whose name/line contains
   FILTER (case-insensitive substring).  cl-tmux's tree is read-only; edit with
   set-option / bind-key."
  (declare (ignore session))
  (with-command-input (flags positionals args "f"
                       :allowed-flags '(#\f)
                       :max-positionals 0
                       :message "customize-mode: unsupported argument")
    (show-overlay (%format-customize-tree (cdr (assoc #\f flags))))
    t))
