(in-package #:cl-tmux)

;;; -- Window/pane/session structural commands ----------------------------------------
;;;
;;; Named-layout macro, select-layout, list-panes, new-window, split-window,
;;; new-session, switch-client, destroy-session, kill-session, resize-window,
;;; detach-client, and the copy-mode -X command table (for send-keys -X).

;;; -- Layout name → keyword dispatch macro ------------------------------------
;;;
;;; Each row is (aliases... keyword), Prolog-style: one fact per layout name.
;;; The macro generates a flat cond of (member name aliases :test #'string-equal)
;;; checks so adding a new layout requires appending one line here.

(defmacro define-layout-name-table (&rest rows)
  "Build %RESOLVE-LAYOUT-NAME from a declarative aliases→keyword table.
   Each ROW is (keyword alias-string...).  Generates a function that maps a
   layout name string to the corresponding keyword, or NIL for unknown names."
  `(defun %resolve-layout-name (name)
     "Map NAME (a string) to a layout keyword, or NIL when unrecognised."
     (cond
       ,@(mapcar (lambda (row)
                   (destructuring-bind (kw &rest aliases) row
                     `((member name ',aliases :test #'string-equal) ,kw)))
                 rows)
       (t nil))))

(define-layout-name-table
  (:even-horizontal "even-horizontal" "even-h")
  (:even-vertical   "even-vertical"   "even-v")
  (:main-horizontal "main-horizontal" "main-h")
  (:main-vertical   "main-vertical"   "main-v")
  (:tiled           "tiled"))

(defun %cmd-select-layout (session args)
  "select-layout <name>: apply the named layout to the active window.
   Accepted names: even-horizontal (even-h), even-vertical (even-v),
   main-horizontal (main-h), main-vertical (main-v), tiled."
  (let* ((name (first args))
         (kw   (and name (%resolve-layout-name name))))
    (when kw
      (%apply-named-layout-to-session session kw))))

(defun %cmd-list-panes (session args)
  "list-panes: list all panes in the active window (mirrors display-panes)."
  (declare (ignore args))
  (with-active-window (win session)
    (let ((panes (window-panes win)))
      (show-overlay
       (if panes
           (with-output-to-string (stream)
             (dolist (p panes)
               (format stream "~D: ~Dx~D at (~D,~D)~A~%"
                       (pane-id p)
                       (pane-width p) (pane-height p)
                       (pane-x p) (pane-y p)
                       (if (eq p (window-active-pane win)) " [active]" ""))))
           "(no panes)")))))

(defun %format-pane-info (session win pane)
  "Return a short pane info string: session:window.pane geometry.
   Used by -P flag in new-window and split-window."
  (format nil "~A:~A.~A: [~Dx~D]"
          (session-name session)
          (if win (window-id win) "?")
          (if pane (pane-id pane) "?")
          (if pane (pane-width pane) 0)
          (if pane (pane-height pane) 0)))

(defun %expand-start-dir (session raw-dir)
  "Expand #{...} format variables in RAW-DIR using the current active pane
   as the format context.  Returns the expanded string, or NIL when RAW-DIR is NIL."
  (when raw-dir
    (let* ((win  (session-active-window session))
           (pane (and win (window-active-pane win)))
           (ctx  (cl-tmux/format:format-context-from-session session win pane)))
      (cl-tmux/format:expand-format raw-dir ctx))))

(defun %show-pane-info-overlay (session win pane print-fmt)
  "Show a transient pane-info overlay for the -P flag.
   Uses PRINT-FMT if given, otherwise the default session:window.pane summary."
  (show-transient-overlay
   (if print-fmt
       (cl-tmux/format:expand-format
        print-fmt
        (cl-tmux/format:format-context-from-session session win pane))
       (%format-pane-info session win pane))))

(defun %cmd-new-window-arg (session args)
  "new-window [-d] [-k] [-P] [-n name] [-t target-window] [-a] [-c start-dir] [-e VAR=val].
   -d: create the window but do not make it active (detached).
   -k: kill any existing window at the target index before creating the new one.
   -P: print the new pane's details (session:window.pane [WxH]) to overlay.
   -F format: with -P, the format string for the printed info (instead of the
     default session:window.pane [WxH]) — e.g. `new-window -dP -F '#{window_id}'`.
   -n name: name the new window.
   -t idx: insert at specific index (assigned as the window id).
   -a: insert after the current window.
   -c dir: start directory for the new pane's shell (format strings expanded).
   -e VAR=val: set environment variable in the new pane (repeatable)."
  (with-command-flags+pos (flags positionals args "ntceF")
    (declare (ignore positionals))
    (let* ((extra-env  (%collect-env-flags flags))
           (name       (cdr (assoc #\n flags)))
           (detach-p   (assoc #\d flags))
           (kill-p     (assoc #\k flags))
           (print-p    (assoc #\P flags))
           (print-fmt  (cdr (assoc #\F flags)))
           (after-p    (assoc #\a flags))
           (raw-dir    (cdr (assoc #\c flags)))
           (start-dir  (%expand-start-dir session raw-dir))
           (at-idx     (%parse-flag-int flags #\t)))
      ;; -k: if a window with the target index already exists, kill it first.
      (when (and kill-p at-idx)
        (let ((existing (find at-idx (session-windows session) :key #'window-id)))
          (when existing
            (%handle-kill-result (kill-window session existing)))))
      ;; Inject -e VAR=val pairs via *pane-extra-env* so %fork-pane picks them up.
      (when extra-env
        (setf *pane-extra-env* extra-env))
      (let ((new-win (%cmd-new-window session
                                      :name name
                                      :start-dir start-dir
                                      :detach (and detach-p t)
                                      :at-index at-idx
                                      :after-current (and after-p t))))
        ;; -P: print new pane details to overlay.
        (when (and print-p new-win)
          (%show-pane-info-overlay session new-win (window-active-pane new-win) print-fmt))
        new-win))))

(defun %parse-split-size (lines-str)
  "Parse a split-window/-l value: \"30\" → 30 (absolute cells, an integer), \"30%\"
   → 0.30 (a real fraction of the parent, equivalent to -p 30).  Modern tmux folds
   the deprecated -p into `-l N%`.  Returns NIL for a missing or non-numeric value."
  (when lines-str
    (let ((pct-pos (position #\% lines-str)))
      (if pct-pos
          (let ((n (parse-integer lines-str :end pct-pos :junk-allowed t)))
            (and n (/ n 100.0)))
          (parse-integer lines-str :junk-allowed t)))))

(defun %cmd-split-window (session args)
  "split-window [-h|-v] [-b] [-f] [-d] [-t target] [-p percent] [-l size] [-c start-dir] [-e VAR=val].
   -h: horizontal split (new pane to the right; side-by-side).
   -v: vertical split (new pane below — default).
   -b: insert before the active pane (left of / above) instead of after.
   -f: full-window split — the new pane spans the whole window dimension (the split
       is inserted at the layout root) instead of subdividing the active pane.
   -d: split but do not change focus (detached mode).
   -t target: split the target pane instead of the active pane.
   -p N: size as a percentage of the parent pane (0-100).
   -l N: size in lines/columns (absolute integer), or -l N% as a percentage
     (modern tmux folds the deprecated -p into -l N%).
   -c dir: start directory for the new pane's shell (format strings expanded).
   -e VAR=val: set environment variable in the new pane (repeatable).
   -P: print the new pane's details to overlay.
   -F format: with -P, the format string for the printed info (instead of the
     default session:window.pane [WxH]) — e.g. `split-window -dP -F '#{pane_id}'`."
  (with-command-flags+pos (flags positionals args "plcetF")
    (declare (ignore positionals))
    (let* ((extra-env    (%collect-env-flags flags))
           (horizontal-p (assoc #\h flags))
           (before-p     (assoc #\b flags))
           (full-p       (assoc #\f flags))
           (detach-p     (assoc #\d flags))
           (target-str   (cdr (assoc #\t flags)))
           (lines-str    (cdr (assoc #\l flags)))
           (raw-dir      (cdr (assoc #\c flags)))
           (start-dir    (%expand-start-dir session raw-dir))
           ;; -l N → N cells; -l N% → fraction (modern tmux); -p N → fraction.
           (size         (let ((pct (%parse-flag-int flags #\p)))
                           (or (and pct (/ pct 100.0)) (%parse-split-size lines-str)))))
      ;; -t target: temporarily make the target pane active so %cmd-split
      ;; operates on it.  Restore the previous active pane afterwards if -d.
      (let* ((prev-win  (session-active-window session))
             (prev-pane (and prev-win (window-active-pane prev-win))))
        (when target-str
          (multiple-value-bind (_sess target-win target-pane)
              (resolve-target *server-sessions* target-str
                              :current-session session
                              :current-window prev-win
                              :current-pane prev-pane)
            (declare (ignore _sess))
            (when (and target-win target-pane)
              ;; Switch active window and pane to the target for the split.
              (session-select-window session target-win)
              (window-select-pane target-win target-pane))))
        ;; Inject -e VAR=val pairs via *pane-extra-env* so %fork-pane picks them up.
        (when extra-env
          (setf *pane-extra-env* extra-env))
        (let* ((print-p (assoc #\P flags))
               (result  (%cmd-split session (if horizontal-p :h :v)
                                    :size size :no-focus (and detach-p t)
                                    :start-dir start-dir :before (and before-p t)
                                    :full (and full-p t))))
          ;; Restore original focus when -d (detach).
          (when (and detach-p target-str prev-win)
            (session-select-window session prev-win)
            (when prev-pane (window-select-pane prev-win prev-pane)))
          ;; -P: print the new pane's details.
          (when (and print-p result)
            (%show-pane-info-overlay session (pane-window result) result
                                     (cdr (assoc #\F flags))))
          result)))))

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
              (show-overlay (format nil "duplicate session: ~A" name))
              (return-from %cmd-new-session-arg nil))
            (setf name (%next-free-session-name))))
      ;; -t <group>: join an existing session's group instead of forking a new
      ;; pane.  The grouped session SHARES the target's window list (and thus the
      ;; live PTYs + reader threads already attached to those panes), so it must
      ;; be built with a bare make-session — NOT new-session, which would fork an
      ;; initial PTY + reader thread that %link-session-to-group then orphans.
      (when group-target
        (let ((target (server-find-session group-target)))
          (unless target
            (show-overlay (format nil "can't find session: ~A" group-target))
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
      ;; Create a new session
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

(defvar *key-table* nil
  "The client's active custom key table (a table-name string), or NIL for the
   normal root/prefix flow.  Set by `switch-client -T <table>`; while non-NIL the
   ground input state looks keys up in this table (modal keymaps).  Defined here
   (dispatch-core loads before events-keystroke) so it is declared special before
   either %cmd-switch-client or %ground-input-state references it.")

(defun %current-session (&optional fallback)
  "The session the standalone client is currently viewing: the most-recently-
   touched (highest session-last-active) session in *server-sessions*, or FALLBACK
   when the registry is empty.  This is how session-switch commands (switch-client,
   choose-tree, last-session) change the displayed session — they session-touch
   their target, and the event loop re-resolves the current session through here on
   every iteration, so the display follows the switch.  Delegates to the registry's
   server-current-session (highest last-active), adding the FALLBACK for the empty
   registry — ties (same-second stamps) resolve there; deliberate switches are
   seconds apart in practice."
  (or (server-current-session) fallback))

(defun %switch-to-session (target)
  "Make TARGET the client's active session by bumping its last-active stamp (the
   renderer follows the most-recently-touched session via %current-session) and
   marking the screen dirty.  No-op when TARGET is NIL.  Returns TARGET when a switch
   happened, else NIL — the single chokepoint every session move routes through.
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

(defun %cmd-switch-client (session args)
  "switch-client [-T key-table] [-t target] [-n] [-p] [-l]: control the client's
   session and key table.
     -T <table>  set the active custom key table (modal keymaps); `-T root` (or no
                 -T) returns to the normal root/prefix flow.
     -t <name>   switch the client to the named session.
     -n / -p     switch to the next / previous session (cyclic over the registry).
     -l          switch to the last (most-recently-active-but-one) session.
   -T is independent of the session flags, so `switch-client -t foo -T copy-mode`
   both moves the client and arms a key table.  Mirrors the keybinding handlers
   :switch-client / :switch-client-next/-prev / :last-session, reusing the same
   session-touch primitive."
  (with-command-flags (flags args "Tt")
    ;; -T key table (modal keymap) — orthogonal to the session move below.
    (let ((table (cdr (assoc #\T flags))))
      (when table
        (setf *key-table* (if (equal table +table-root+) nil table))))
    ;; Session selection: -t named, else -n/-p cyclic, else -l last-active.
    (let ((sessions (mapcar #'cdr *server-sessions*)))
      (cond
        ((assoc #\t flags)
         (%switch-to-session (server-find-session (cdr (assoc #\t flags)))))
        ((assoc #\n flags)
         (%switch-to-session (and sessions (next-cyclic sessions session))))
        ((assoc #\p flags)
         (%switch-to-session (and sessions (prev-cyclic sessions session))))
        ((assoc #\l flags)
         (%switch-to-session
          (second (sort (copy-list sessions) #'> :key #'session-last-active))))))))

(defun %destroy-session (session)
  "Tear down SESSION: close its panes' PTYs, remove it from the server registry,
   and fire the session-closed hook.  The single chokepoint for session
   DESTRUCTION (every kill-session path routes through here) — deliberately
   distinct from rename-session, which also removes+re-adds the registry entry but
   must NOT fire session-closed.  Returns the session name.

   PTY teardown is REFERENCE-COUNTED: grouped/linked sessions share the SAME window
   structs (session-registry %link-session-to-group aliases the window list), so a
   window still referenced by another live session must keep its PTYs open or the
   survivors lose the panes they display.  SESSION is still in *server-sessions*
   here, so an UNSHARED window has %window-session-count = 1 (close it) and a SHARED
   window has >= 2 (leave it) — identical to the old unconditional close for the
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
   client should detach — which in the single-client standalone model means exit —
   or NIL when it switches to a surviving session (the event loop then follows the
   new current session).  No survivors → always :QUIT.  off/no-detached fall to the
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
          (t nil)))))   ; off / no-detached → most-recent survivor (loop auto-follows)

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
              ;; Killing the session the client is viewing → apply detach-on-destroy.
              (when (and was-current
                         (eq :quit (%detach-on-destroy-action name)))
                (setf *running* nil))))))))

(defun %cmd-resize-window-arg (session args)
  "resize-window [-x cols] [-y rows] [-t target-window]: resize a window.
   Sets the window to exactly COLS × ROWS; without flags prompts interactively."
  (with-command-flags+pos (flags positionals args "xyt")
    (declare (ignore positionals))
    (let* ((cols     (%parse-flag-int flags #\x))
           (rows     (%parse-flag-int flags #\y))
           (win      (session-active-window session)))
      (when (and win cols rows (> cols 0) (> rows 0))
        (window-relayout win rows cols)))))

(defun %cmd-detach-client-arg (session args)
  "detach-client [-a] [-t target-session]: detach from a session.
   In standalone mode, both the -a (all clients) form and the no-flag form
   stop the event loop.  SESSION and ARGS are not used."
  (declare (ignore session args))
  (setf *running* nil))

