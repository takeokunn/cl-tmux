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
      ((assoc #\t flags)
       (server-find-session (cdr (assoc #\t flags))))
      ((assoc #\n flags)
       (and sessions (next-cyclic sessions session)))
      ((assoc #\p flags)
       (and sessions (prev-cyclic sessions session)))
      ((assoc #\l flags)
       (second (sort (copy-list sessions) #'> :key #'session-last-active)))
      (t nil))))

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
    ;; -T key table (modal keymap) -- orthogonal to the session move below.
    (let ((table (cdr (assoc #\T flags))))
      (when table
        (setf *key-table* (if (string= table +table-root+) nil table))))
    (let ((result (%switch-to-session (%switch-client-target-session session flags))))
      (cond
        (result result)
        ((assoc #\r flags)
         (setf *dirty* t)
         t)))))

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
    (let ((target-name (cdr (assoc #\t flags))))
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
           (w (and x (parse-integer str :end x :junk-allowed t)))
           (h (and x (parse-integer str :start (1+ x) :junk-allowed t))))
      (when (and w h (plusp w) (plusp h))
        (values w h)))))

(defun %next-free-session-name ()
  "Return the lowest positive-integer string not already in use as a session name."
  (loop for i from 1
        for candidate = (format nil "~D" i)
        unless (server-find-session candidate) return candidate))

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
    (let* ((name            (or (cdr (assoc #\s flags))
                                (format nil "~D" (1+ (length *server-sessions*)))))
           (attach-if-exists (assoc #\A flags))
           (detach-p         (assoc #\d flags))
           (win-name         (cdr (assoc #\n flags)))
           ;; -t <group>: the new session JOINS an existing session's group,
           ;; sharing its window list (tmux "grouped sessions").
           (group-target     (cdr (assoc #\t flags)))
           (start-dir        (cdr (assoc #\c flags)))
           ;; Detached sessions have no client → fall back to default-size, not the
           ;; current terminal size.  NIL for attached sessions (use the terminal).
           (default-wxh      (and detach-p
                                  (cl-tmux/options:get-option "default-size" "80x24")))
           ;; -x/-y override everything when given (junk-allowed).
           (cols             (or (%parse-flag-int flags #\x)
                                 (and default-wxh (nth-value 0 (%parse-wxh default-wxh)))
                                 *term-cols*))
           (rows             (or (%parse-flag-int flags #\y)
                                 (and default-wxh (nth-value 1 (%parse-wxh default-wxh)))
                                 (- *term-rows* *status-height*))))
      ;; -A: attach to existing session if it exists
      (when attach-if-exists
        (let ((existing (server-find-session name)))
          (when existing
            (session-touch existing)
            (unless detach-p (setf *dirty* t))
            (return-from %cmd-new-session-arg existing))))
      ;; Without -A, a name already in use cannot be taken over (server-add-session
      ;; would orphan the existing session): an EXPLICIT -s duplicate is refused
      ;; (tmux's "duplicate session"); an AUTO name bumps to the next free number.
      (when (and (not attach-if-exists) (server-find-session name))
        (if (cdr (assoc #\s flags))
            (progn
              (%overlayf "duplicate session: ~A" name)
              (return-from %cmd-new-session-arg nil))
            (setf name (%next-free-session-name))))
      ;; -t <group>: join an existing session's group instead of spawning a new
      ;; pane.  The grouped session SHARES the target's window list (and thus the
      ;; live PTYs + reader threads already attached to those panes), so it must
      ;; be built with a bare make-session — NOT new-session, which would spawn an
      ;; initial PTY + reader thread that %link-session-to-group then orphans.
      (when group-target
        (let ((target (server-find-session group-target)))
          (unless target
            (%overlayf "can't find session: ~A" group-target)
            (return-from %cmd-new-session-arg nil))
          (let ((grouped (make-session :id (incf *session-id-counter*)
                                       :name name
                                       :last-active (get-universal-time))))
            (server-add-session grouped)
            (server-new-session-in-group grouped target)
            (when (not detach-p)
              (setf *dirty* t)
              (show-transient-overlay
               (format nil "new session: ~A" (session-name grouped))))
            (return-from %cmd-new-session-arg grouped))))
      (let ((new-sess (new-session name rows cols :start-dir start-dir)))
        ;; Apply window name if given
        (when (and win-name new-sess)
          (let ((win (session-active-window new-sess)))
            (when win (rename-window win win-name))))
        ;; Without -d, show an overlay confirming the new session was created.
        ;; With -d, the session is created in background and SESSION (the calling
        ;; session) remains the active display — no dirty flag, no visual switch.
        (when (and new-sess (not detach-p))
          (show-transient-overlay
           (format nil "new session: ~A" (session-name new-sess))))
        new-sess))))

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
  "kill-session [-a] [-t name]: kill session(s).
   -a: kill all sessions EXCEPT the one named by -t (or current session).
   -t name: the target session (default: current session)."
  (with-command-flags+pos (flags positionals args "t")
    (declare (ignore positionals))
    (let* ((kill-all-others (assoc #\a flags))
           (target-name     (cdr (assoc #\t flags)))
           (target-sess     (or (and target-name (server-find-session target-name))
                                session)))
      (if kill-all-others
          ;; -a: kill all sessions except target-sess (the "keep" session)
          (dolist (entry (remove-if (lambda (e) (eq (cdr e) target-sess))
                                    *server-sessions*))
            (%destroy-session (cdr entry)))
          ;; No -a: kill target-sess
          (when target-sess
            (let ((name        (session-name target-sess))
                  (was-current (eq target-sess session)))
              (%destroy-session target-sess)
              ;; Killing the session the client is viewing -> apply detach-on-destroy.
              (when (and was-current
                         (eq :quit (%detach-on-destroy-action name)))
                (setf *running* nil))))))))

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
           (target   (cdr (assoc #\t flags)))
           (win      (if target
                         (%resolve-window-target session target)
                         (session-active-window session))))
      (when (and win cols rows (> cols 0) (> rows 0))
        (window-relayout win rows cols)))))

(defun %cmd-detach-client-arg (session args)
  "detach-client: detach the active client.
   cl-tmux does not implement target selection, print commands, or shell hooks
   for detach-client; argument forms are rejected instead of silently collapsed
   onto the active client."
  (with-command-input (flags positionals args ""
                             :allowed-flags '()
                             :max-positionals 0
                             :message "detach-client: unsupported argument")
    (declare (ignore flags positionals))
    (dispatch-command session :detach nil)))
