(in-package #:cl-tmux)

;;; -- Session/client lifecycle commands -----------------------------------------

(defun %current-session (&optional fallback)
  "The session the standalone client is currently viewing: the most-recently-
   touched (highest session-last-active) session in *server-sessions*, or FALLBACK
   when the registry is empty.  This is how session-switch commands (switch-client,
   choose-tree, last-session) change the displayed session -- they session-touch
   their target, and the event loop re-resolves the current session through here on
   every iteration, so the display follows the switch.  Delegates to the registry's
   server-current-session (highest last-active), adding the FALLBACK for the empty
   registry -- ties (same-second stamps) resolve there; deliberate switches are
   seconds apart in practice."
  (or (server-current-session) fallback))

(defun %switch-to-session (target)
  "Make TARGET the client's active session by bumping its last-active stamp (the
   renderer follows the most-recently-touched session via %current-session) and
   marking the screen dirty.  No-op when TARGET is NIL.  Returns TARGET when a switch
   happened, else NIL -- the single chokepoint every session move routes through.
   When destroy-unattached is on, the session the client was viewing becomes
   unattached on the switch and is destroyed (tmux's destroy-unattached)."
  (when target
    (let ((old (server-current-session)))   ; the session being left, if any
      (session-touch target)
      (setf *dirty* t)
      (when (and old (not (eq old target))
                 (cl-tmux/options:get-option "destroy-unattached"))
        (%destroy-session old))
      target)))

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
  (with-command-input (flags positionals args "Ttc"
                             :allowed-flags '(#\T #\t #\n #\p #\l #\r)
                             :max-positionals 0
                             :message "switch-client: unsupported argument")
    (declare (ignore positionals))
    (%switch-client-apply-key-table flags)
    (or (%switch-to-session (%switch-client-target-session session flags))
        (%switch-client-handle-refresh flags))))

(defun %cmd-attach-session-arg (session args)
  "attach-session [-t target]: in an already attached client, switch this client
   to the target session, or to the current server session when no target is
   given."
  (declare (ignore session))
    (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\t)
                             :max-positionals 0
                             :message "attach-session: unsupported argument")
    (declare (ignore positionals))
    (let ((target-name (%flag-value flags #\t)))
      (%switch-to-session
       (if target-name
           (server-find-session target-name)
           (server-current-session))))))

(defun %parse-wxh (str)
  "Parse a \"WxH\" size string (e.g. the default-size option \"80x24\") into
   (values W H), or NIL when STR is not of that form or either dimension
   is not a positive integer."
  (when (stringp str)
    (let* ((x (position #\x str :test #'char-equal))
           (w (and x (%parse-integer-or-nil str :end x :junk-allowed t)))
           (h (and x (%parse-integer-or-nil str :start (1+ x) :junk-allowed t))))
      (when (and w h (plusp w) (plusp h))
        (values w h)))))

(defun %next-free-session-name ()
  "Return the lowest positive-integer string not already in use as a session name."
  (loop for i from 1
        for candidate = (format nil "~D" i)
        unless (server-find-session candidate) return candidate))

(defun %new-session-name-from-flags (flags)
  "Return the requested session name, or the next auto-generated one."
  (or (%flag-value flags #\s)
      (format nil "~D" (1+ (length *server-sessions*)))))

(defun %default-size-dimensions (detach-p)
  "Return the default detached session dimensions, or NIL when attached."
  (when detach-p
    (multiple-value-bind (cols rows)
        (%parse-wxh (cl-tmux/options:get-option "default-size" "80x24"))
      (values cols rows))))

(defun %new-session-dimensions-from-flags (flags detach-p)
  "Return the initial session dimensions selected by X/Y flags and defaults."
  (multiple-value-bind (default-cols default-rows)
      (%default-size-dimensions detach-p)
    (values (or (%parse-flag-int flags #\x)
                default-cols
                *term-cols*)
            (or (%parse-flag-int flags #\y)
                default-rows
                (- *term-rows* *status-height*)))))

(defun %new-session-return-existing (name detach-p)
  "Return the already-existing session NAME for new-session -A, touching it."
  (let ((existing (server-find-session name)))
    (when existing
      (session-touch existing)
      (unless detach-p
        (setf *dirty* t))
      existing)))

(defun %new-session-resolve-name (name attach-if-exists flags)
  "Return the final session name, or NIL when an explicit duplicate is refused."
  (if (and (not attach-if-exists)
           (server-find-session name))
      (if (%flag-present-p flags #\s)
          (progn
            (%overlayf "duplicate session: ~A" name)
            nil)
          (%next-free-session-name))
      name))

(defun %new-session-create-grouped (name group-target detach-p)
  "Create a grouped session that shares windows with GROUP-TARGET."
  (let ((target (server-find-session group-target)))
    (unless target
      (%overlayf "can't find session: ~A" group-target)
      (return-from %new-session-create-grouped nil))
    (let ((grouped (make-session :id (incf *session-id-counter*)
                                 :name name
                                 :last-active (get-universal-time))))
      (server-add-session grouped)
      (server-new-session-in-group grouped target)
      (when (not detach-p)
        (setf *dirty* t)
        (show-transient-overlay
         (format nil "new session: ~A" (session-name grouped))))
      grouped)))

(defun %new-session-finalize (new-sess win-name detach-p)
  "Apply post-creation window naming and overlays to NEW-SESS."
  (when (and win-name new-sess)
    (let ((win (session-active-window new-sess)))
      (when win
        (rename-window win win-name))))
  (when (and new-sess (not detach-p))
    (show-transient-overlay
     (format nil "new session: ~A" (session-name new-sess))))
  new-sess)

(defun %cmd-new-session-arg (session args)
  "new-session [-A] [-d] [-s name] [-n window-name] [-c start-dir] [-x width] [-y height]: create a new session.
   -A: if a session named NAME already exists, attach to it instead of creating a new one.
  -d: create detached (do not switch to the new session).
  -s name: session name.
  -n name: initial window name.
  -c dir: start directory for the initial window's shell.
   -x width: initial columns (default: terminal width, or default-size when -d).
   -y height: initial rows (default: terminal height minus status bar, or
     default-size when -d).
  A DETACHED session (-d) has no client to size it, so — like tmux — it uses the
   default-size option (\"WxH\", default 80x24) when -x/-y are not given."
  (with-command-flags+pos (flags positionals args "sncxyt")
    (declare (ignore positionals))
    (let* ((name            (%new-session-name-from-flags flags))
           (attach-if-exists (%flag-present-p flags #\A))
           (detach-p         (%flag-present-p flags #\d))
           (win-name         (%flag-value flags #\n))
           ;; -t <group>: the new session JOINS an existing session's group,
           ;; sharing its window list (tmux "grouped sessions").
           (group-target     (%flag-value flags #\t))
           (start-dir        (%flag-value flags #\c)))
      (multiple-value-bind (cols rows)
          (%new-session-dimensions-from-flags flags detach-p)
        (when attach-if-exists
          (return-from %cmd-new-session-arg
            (%new-session-return-existing name detach-p)))
        (setf name (%new-session-resolve-name name attach-if-exists flags))
        (when (null name)
          (return-from %cmd-new-session-arg nil))
        (when group-target
          (return-from %cmd-new-session-arg
            (%new-session-create-grouped name group-target detach-p)))
        (%new-session-finalize
         (new-session name rows cols :start-dir start-dir)
         win-name
         detach-p)))))

(defun %destroy-session (session)
  "Tear down SESSION: close its panes' PTYs, remove it from the server registry,
   and fire the session-closed hook.  The single chokepoint for session
   DESTRUCTION (every kill-session path routes through here) -- deliberately
   distinct from rename-session, which also removes+re-adds the registry entry but
   must NOT fire session-closed.  Returns the session name.

   PTY teardown is REFERENCE-COUNTED: grouped/linked sessions share the SAME window
   structs (session-registry %link-session-to-group aliases the window list), so a
   window still referenced by another live session must keep its PTYs open or the
   survivors lose the panes they display.  SESSION is still in *server-sessions*
   here, so an UNSHARED window has %window-session-count = 1 (close it) and a SHARED
   window has >= 2 (leave it) -- identical to the old unconditional close for the
   common single-session case."
  (when session
    (let ((name (session-name session)))
      (dolist (win (session-windows session))
        (when (<= (%window-session-count win) 1)
          (dolist (pane (window-panes win))
            (ignore-errors (pty-close (pane-fd pane) (pane-pid pane))))))
      (server-remove-session name)
      (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-session-closed+ session)
      name)))

(defun %alphabetical-neighbour (name dir)
  "The surviving session whose name is alphabetically just after (DIR +1) or
   before (DIR -1) NAME (the destroyed session's name, no longer in the registry),
   wrapping around.  Returns NIL when no sessions survive.  Backs detach-on-destroy
   previous/next."
  (let ((sorted (sort (mapcar #'cdr *server-sessions*) #'string< :key #'session-name)))
    (when sorted
      (if (plusp dir)
          (or (find-if (lambda (s) (string< name (session-name s))) sorted)
              (first sorted))
          (or (find-if (lambda (s) (string< (session-name s) name)) (reverse sorted))
              (car (last sorted)))))))

(defun %detach-on-destroy-action (destroyed-name)
  "Decide the standalone client's fate after the session it was viewing (named
   DESTROYED-NAME) is destroyed, per the detach-on-destroy option
   (off / on (default) / no-detached / previous / next).  Returns :QUIT when the
   client should detach -- which in the single-client standalone model means exit --
   or NIL when it switches to a surviving session (the event loop then follows the
   new current session).  No survivors -> always :QUIT.  off/no-detached fall to the
   most-recent survivor (the loop's natural choice); previous/next touch the
   alphabetical neighbour of DESTROYED-NAME so the loop moves there."
  (if (null *server-sessions*)
      :quit
      (let ((mode (or (cl-tmux/options:get-option "detach-on-destroy") "on")))
        (cond
          ((string= mode "on") :quit)
          ((string= mode "previous")
           (%switch-to-session (%alphabetical-neighbour destroyed-name -1)) nil)
          ((string= mode "next")
           (%switch-to-session (%alphabetical-neighbour destroyed-name 1)) nil)
          (t nil)))))   ; off / no-detached -> most-recent survivor (loop auto-follows)

(defun %cmd-kill-session-arg (session args)
  "kill-session [-a] [-C] [-t name]: kill session(s), or clear their alerts.
   -a: kill all sessions EXCEPT the one named by -t (or current session).
   -C: do NOT kill; instead clear the alert flags (activity/silence) on every
       window of the target session, matching tmux's `kill-session -C`.
   -t name: the target session (default: current session)."
  (with-command-flags+pos (flags positionals args "t")
    (declare (ignore positionals))
    (let* ((clear-alerts    (%flag-present-p flags #\C))
           (kill-all-others (%flag-present-p flags #\a))
           (target-name     (%flag-value flags #\t))
           (target-sess     (or (and target-name (server-find-session target-name))
                                session)))
      (cond
        ;; -C: clear alerts instead of killing.  tmux resets every window's
        ;; activity/silence (and bell) flags in the target session; cl-tmux does
        ;; not model bell separately, so clearing activity+silence is the faithful
        ;; subset.
        (clear-alerts
         (when target-sess
           (dolist (win (session-windows target-sess))
             (setf (window-activity-flag win) nil
                   (window-silence-flag  win) nil))))
        ;; -a: kill all sessions except target-sess (the "keep" session).
        (kill-all-others
         (dolist (entry (remove-if (lambda (e) (eq (cdr e) target-sess))
                                   *server-sessions*))
           (%destroy-session (cdr entry))))
        ;; No -a/-C: kill target-sess.
        (t
         (when target-sess
           (let ((name        (session-name target-sess))
                 (was-current (eq target-sess session)))
             (%destroy-session target-sess)
             ;; Killing the session the client is viewing -> detach-on-destroy.
             (when (and was-current
                        (eq :quit (%detach-on-destroy-action name)))
               (setf *running* nil)))))))))

(defun %cmd-resize-window-arg (session args)
  "resize-window [-x cols] [-y rows] [-t target-window]: resize a window.
   Sets the window to exactly COLS x ROWS; without flags prompts interactively."
  (with-command-input (flags positionals args "xyt"
                             :allowed-flags '(#\x #\y #\t)
                             :max-positionals 0
                             :message "resize-window: unsupported argument")
    (declare (ignore positionals))
    (let* ((cols     (%parse-flag-int flags #\x))
           (rows     (%parse-flag-int flags #\y))
           (target   (%flag-value flags #\t))
           (win      (if target
                         (%resolve-window-target session target)
                         (session-active-window session))))
      (when (and win cols rows (> cols 0) (> rows 0))
        (window-relayout win rows cols)))))

(defun %cmd-detach-arg (session args)
  "detach: detach the active client.
   cl-tmux does not implement target selection, print commands, or shell hooks
   for detach; argument forms are rejected instead of silently collapsed onto
   the active client."
  (with-command-input (flags positionals args ""
                             :allowed-flags '()
                             :max-positionals 0
                             :message "detach: unsupported argument")
    (declare (ignore flags positionals))
    (dispatch-command session :detach nil)))
