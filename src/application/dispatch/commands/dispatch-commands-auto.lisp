(in-package #:cl-tmux)

;;; -- Window navigation and session management commands ----------------------
;;;

(defun %window-string-matches-p (pattern string &key regex-p (ignore-case t))
  "T when STRING matches PATTERN either as a substring or a regex."
  (and string
       (if regex-p
           (cl-tmux/format::%regex-match-p pattern string ignore-case)
           (search pattern string :test (if ignore-case #'char-equal #'char=)))))

(defun %window-matches-pattern-p (window pattern &key (search-name-p t)
                                                  (search-title-p t)
                                                  (search-content-p t)
                                                  regex-p (ignore-case t))
  "T when WINDOW matches PATTERN against its name, pane title, screen title, or
   visible content.  The default search spans name/title/content so the interactive
   :find-window binding keeps its existing behavior."
  (or (and search-name-p
           (%window-string-matches-p pattern (window-name window)
                                     :regex-p regex-p :ignore-case ignore-case))
      (some (lambda (pane)
              (or (and search-title-p
                       (let ((title (cl-tmux/model:pane-title pane))
                             (screen (cl-tmux/model:pane-screen pane)))
                         (or (%window-string-matches-p pattern title
                                                       :regex-p regex-p
                                                       :ignore-case ignore-case)
                             (%window-string-matches-p
                              pattern
                              (and screen (cl-tmux/terminal:screen-title screen))
                              :regex-p regex-p
                              :ignore-case ignore-case))))
                  (and search-content-p
                       (some (lambda (line)
                               (cl-tmux/format::%content-search-match-p
                                pattern line regex-p ignore-case))
                             (cl-tmux/format::%pane-visible-lines pane)))))
            (cl-tmux/model:window-panes window))))

(defun %window-has-live-panes-p (window)
  "T when WINDOW contains at least one live pane."
  (some #'cl-tmux/model:pane-live-p (cl-tmux/model:window-panes window)))

(defun %cmd-find-window-arg (session args)
  "find-window [-N] match-string: find the window whose name
   (or, unless -N, a pane title/content) matches MATCH-STRING and select it.  With
   several matches, the first is selected.  The match is case-insensitive substring
   (as in the interactive find-window).  This is the scriptable form; the
   interactive :find-window binding (which lists matches in an overlay) is
   unchanged."
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\C #\N #\T #\i #\r #\t)
                             :max-positionals 1
                             :message "find-window: unsupported argument")
    (let* ((target-str   (%flag-value flags #\t))
           (pattern      (first positionals))
           (name-only    (%flag-present-p flags #\N))
           (title-only   (%flag-present-p flags #\T))
           (content-only (%flag-present-p flags #\C))
           (regex-p      (%flag-present-p flags #\r))
           (ignore-case  t)
           (selector-p   (or name-only title-only content-only))
           (search-name-p (or name-only (not selector-p)))
           (search-title-p (or title-only (not selector-p)))
           (search-content-p (or content-only (not selector-p)))
           (session-to-search session))
      (when target-str
        (with-target-context (resolved-session target-window target-pane session target-str)
          (declare (ignore target-window target-pane))
          (setf session-to-search resolved-session)))
      (when (and pattern (plusp (length pattern)))
        (let ((match (find-if (lambda (w)
                                (%window-matches-pattern-p w pattern
                                                            :search-name-p search-name-p
                                                            :search-title-p search-title-p
                                                            :search-content-p search-content-p
                                                            :regex-p regex-p
                                                            :ignore-case ignore-case))
                               (session-windows session-to-search))))
          (when match
            (session-select-window session-to-search match)
            (setf *dirty* t)
            t))))))

(defun %window-has-alert-p (win)
  "T when WIN has a pending alert — activity (monitor-activity) or silence
   (monitor-silence).  These are the windows next-window/previous-window -a jumps
   between (cl-tmux tracks activity + silence at the window level)."
  (and win (or (cl-tmux/model:window-activity-flag win)
               (cl-tmux/model:window-silence-flag win))))

(defun %cycle-to-alert-window (session cycler)
  "Select the next/prev window (via CYCLER) that has an alert, scanning from the
   active window and wrapping once.  Checks only the OTHER windows (never re-selects
   the current one) and is a no-op when none of them has an alert."
  (let* ((windows (session-windows session))
         (start   (session-active-window session)))
    (when (and windows start (> (length windows) 1))
      (loop with cur = start
            repeat (1- (length windows))      ; visit every OTHER window, at most once
            do (setf cur (funcall cycler windows cur))
               (when (%window-has-alert-p cur)
                 (%with-window-focus-transition (session)
                   (session-select-window session cur))
                 (return t))
            finally (return nil)))))

(defun %cycle-window-in-target (session args cycler command-name)
  "Resolve -t to a target session (default SESSION) and cycle its active window
   with CYCLER (next-cyclic / prev-cyclic).  Shared by the scriptable
   next-window / previous-window commands.  -a cycles to the next/prev window with
   an alert (activity or silence); without -a, plain window cycling."
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\a #\t)
                             :max-positionals 0
                             :message (format nil "~A: unsupported argument" command-name))
    (let ((target-str (%flag-value flags #\t)))
      (with-target-session (target-session target-str session
                                :on-missing :current)
        (let ((cycled (if (%flag-present-p flags #\a)
                          (%cycle-to-alert-window target-session cycler)
                          (%cmd-cycle-window target-session cycler))))
          (when cycled
            (setf *dirty* t)
            t))))))

(defun %cmd-next-window-arg (session args)
  "next-window [-a] [-t target-session]: select the next window in the target
   session (default: the current session).  Scriptable form; the interactive
   :next-window binding (current session) is unchanged."
  (%cycle-window-in-target session args #'next-cyclic "next-window"))

(defun %cmd-previous-window-arg (session args)
  "previous-window [-a] [-t target-session]: select the previous window in the
   target session (default: the current session)."
  (%cycle-window-in-target session args #'prev-cyclic "previous-window"))

(defun %cmd-last-window-arg (session args)
  "last-window [-t target-session]: select the previously active window in the
   target session (default: the current session)."
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\t)
                             :max-positionals 0
                             :message "last-window: unsupported argument")
    (let ((target-str (%flag-value flags #\t)))
      (with-target-session (target-session target-str session
                                :on-missing :current)
        (let ((prev (session-last-window target-session)))
          (when prev
            (%with-window-focus-transition (target-session)
              (session-select-window target-session prev))
            (setf *dirty* t)
            t))))))

(defun %cmd-refresh-client-arg (session args)
  "refresh-client [-cDLRSU] [-A pane] [-B sub] [-C size] [-f flags] [-l target]
   [-t target-client]: refresh / redraw the client.
   In the standalone single-client model every form collapses onto a redraw:
     -S            redraw the status line only (cl-tmux redraws the whole frame).
     -L/-R/-U/-D   pan the visible window — accepted; the full-screen single
                   client is never larger than the terminal, so it is a no-op.
     -c            reset panning to cursor tracking (no-op, see above).
     -f / -F       set client flags: a comma-separated list; a '!' prefix
                   removes a flag.  Stored in *client-flags* (single-client
                   model) and shown by #{client_flags}.
     -l            request the host clipboard via OSC 52 — accepted; there is no
                   outer xterm client to query in the standalone model.
     -C WxH        set the client size: updates *term-rows*/*term-cols* and
                   relayouts the active window (tmux control-mode clients use
                   this to drive the session size independent of the tty).
     -A / -B       passthrough / subscription control (accepted, no-op).
     -t            target client (single-client model).
   Unknown flags are still rejected so invalid config surfaces an error."
  (with-command-input (flags positionals args "ABCfFlt"
                             :allowed-flags '(#\A #\B #\C #\D #\L #\R #\S #\U
                                              #\c #\f #\F #\l #\t)
                             :max-positionals 0
                             :message "refresh-client: unsupported argument")
    (declare (ignore positionals))
    ;; -f/-F: apply the comma-separated client flag list ('!' removes).
    (let ((spec (or (%flag-value flags #\f) (%flag-value flags #\F))))
      (when spec
        (dolist (flag (uiop:split-string spec :separator ","))
          (let ((name (string-trim " " flag)))
            (cond
              ((zerop (length name)))
              ((char= (char name 0) #\!)
               (setf *client-flags*
                     (delete (subseq name 1) *client-flags* :test #'string=)))
              (t (pushnew name *client-flags* :test #'string=)))))))
    (multiple-value-bind (cols rows) (%parse-client-size (%flag-value flags #\C))
      (when (and cols rows)
        (setf *term-cols* cols *term-rows* rows)
        (let ((win (session-active-window session)))
          (when win
            (window-relayout win (- rows *status-height*) cols)))))
    (setf *dirty* t)
    t))

(defun %parse-client-size (spec)
  "Parse a refresh-client -C size SPEC (\"WIDTHxHEIGHT\", e.g. \"80x24\").
   Returns (values COLS ROWS), or NIL when SPEC is missing or malformed."
  (let ((x (and spec (position #\x spec))))
    (when x
      (let ((cols (%parse-integer-or-nil (subseq spec 0 x)))
            (rows (%parse-integer-or-nil (subseq spec (1+ x)))))
        (when (and cols rows (plusp cols) (plusp rows))
          (values cols rows))))))

(defun %cmd-lock-client-arg (session args)
  "lock-client: lock the active session."
  (with-command-input (flags positionals args ""
                             :allowed-flags '()
                             :max-positionals 0
                             :message "lock-client: unsupported argument")
    (dispatch-command session :lock-client nil)
    t))

(defun %cmd-lock-session-arg (session args)
  "lock-session [-t target-session]: lock a session."
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\t)
                             :max-positionals 0
                             :message "lock-session: unsupported argument")
    (let ((target-str (%flag-value flags #\t)))
      (with-target-session (target-session target-str session
                                :on-missing :current)
        (dispatch-command target-session :lock-session nil)
        t))))

;;; -- Runtime hook and key-binding commands ----------------------------------

(defun %cmd-set-hook (session args)
  "set-hook [-g] [-a] [-R] [-u] event [command]: register or unset a command hook
   at runtime (the same backend the .tmux.conf `set-hook` directive uses, now
   reachable from command-prompt / key bindings / control mode).
     -u  unset all command hooks for EVENT.
     -R  run EVENT's hooks immediately (after setting, if a command is also given).
     -a  append the command to EVENT's hook list (preserving prior hooks); without
         -a, set-hook REPLACES the event's hook, matching tmux.
     -g  accepted (cl-tmux keeps a flat, server-wide command-hook table).
   Without -u, the tokens after EVENT are joined into one command line and stored
   as a raw string, expanded at hook-fire time via %run-command-line."
  (with-command-flags+pos (flags positionals args "")
    (let ((event (first positionals)))
      (when event
        (%delegate-config-directive "set-hook" args)
        (unless (%flag-present-p flags #\u)
          (when (%flag-present-p flags #\R)
            (run-command-hooks event session)))))))

(defun %delegate-config-directive (directive args)
  "Dispatch DIRECTIVE with ARGS through the config directive interpreter."
  (cl-tmux/config:apply-config-directive (cons directive args)))

(defun %cmd-bind-arg (session args)
  "bind [-n] [-r] [-T table] [-N note] key command...: bind a key at runtime
   (command-prompt / key binding / control mode).  Delegates to the config
   directive logic so the full flag set is honoured — the same path .tmux.conf
   uses.  The no-arg form falls through to the interactive bind prompt."
  (declare (ignore session))
  (%delegate-config-directive "bind" args))

(defun %cmd-unbind-arg (session args)
  "unbind [-a] [-n] [-T table] [key]: unbind a key (or, with -a, every key in
   a table) at runtime, delegating to the config directive logic."
  (declare (ignore session))
  (%delegate-config-directive "unbind" args))
