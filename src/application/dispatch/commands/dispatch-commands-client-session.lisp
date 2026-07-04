(in-package #:cl-tmux)

;;; -- Client session commands ---------------------------------------------------

(defun %switch-client-target-session (session flags)
  "Resolve the session selected by SWITCH-CLIENT flags, or NIL when none was
   requested.  -t takes priority over -n/-p/-l, matching tmux's command-line
   precedence."
  (let ((sessions (mapcar #'cdr *server-sessions*)))
    (cond
      ((%flag-present-p flags #\t)
       (server-find-session (%flag-value flags #\t)))
      ((%flag-present-p flags #\n)
       (and sessions (next-cyclic sessions session)))
      ((%flag-present-p flags #\p)
       (and sessions (prev-cyclic sessions session)))
      ((%flag-present-p flags #\l)
       (second (sort (copy-list sessions) #'> :key #'session-last-active)))
      (t nil))))

(defun %switch-client-apply-key-table (flags)
  "Apply SWITCH-CLIENT's -T key table side effect."
  (let ((table (%flag-value flags #\T)))
    (when table
      (setf *key-table* (if (string= table +table-root+) nil table)))))

(defun %switch-client-handle-refresh (flags)
  "Handle SWITCH-CLIENT's -r refresh path when no session move is requested."
  (when (%flag-present-p flags #\r)
    (setf *dirty* t)
    t))

(defun %cmd-switch-client (session args)
  "switch-client [-lnpr] [-t target] [-T key-table]:
   control the client's session and key table.
     -T <table>  set the active custom key table (modal keymaps); `-T root` (or no
                 -T) returns to the normal root/prefix flow.
     -t <name>   switch the client to the named session.
     -n / -p     switch to the next / previous session (cyclic over the registry).
     -l          switch to the last (most-recently-active-but-one) session.
     -r          refresh the client display when no session switch is requested.
   -T is independent of the session flags, so `switch-client -t foo -T copy-mode`
   both moves the client and arms a key table.  Mirrors the keybinding handlers
   :switch-client / :switch-client-next/-prev / :last-session, reusing the same
   session-touch primitive."
  (with-command-input (flags positionals args "Tt"
                             :allowed-flags '(#\T #\t #\n #\p #\l #\r)
                             :max-positionals 0
                             :message "switch-client: unsupported argument")
    (declare (ignore positionals))
    (%switch-client-apply-key-table flags)
    (or (%switch-to-session (%switch-client-target-session session flags))
        (%switch-client-handle-refresh flags))))

(defun %cmd-attach-session-arg (session args)
  "attach-session [-c working-dir] [-t target]: in an already attached client,
   switch this client to the target session, or to the current server session
   when no target is given.
   -c working-dir: set the target session's working directory (used as the
     default start directory for new windows), matching tmux attach-session -c."
  (declare (ignore session))
  (with-command-input (flags positionals args "ct"
                             :allowed-flags '(#\c #\t)
                             :max-positionals 0
                             :message "attach-session: unsupported argument")
    (declare (ignore positionals))
    (let* ((target-name (%flag-value flags #\t))
           (work-dir    (%flag-value flags #\c))
           (target      (if target-name
                            (server-find-session target-name)
                            (server-current-session))))
      (when (and target work-dir (plusp (length work-dir)))
        (setf (session-start-directory target) work-dir))
      (%switch-to-session target))))

(defun %cmd-detach-arg (session args)
  "detach-client / detach: detach the active client."
  (with-command-input (flags positionals args ""
                             :allowed-flags '()
                             :max-positionals 0
                             :message "detach: unsupported argument")
    (declare (ignore flags positionals))
    (dispatch-command session :detach nil)))
