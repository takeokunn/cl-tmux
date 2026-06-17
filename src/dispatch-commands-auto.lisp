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

(defun %cmd-find-window-arg (session args)
  "find-window [-N] match-string: find the window whose name
   (or, unless -N, a pane title/content) matches MATCH-STRING and select it.  With
   several matches, the first is selected.  The match is case-insensitive substring
   (as in the interactive find-window).  This is the scriptable form; the
   interactive :find-window binding (which lists matches in an overlay) is
   unchanged."
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\C #\N #\T #\Z #\i #\r #\t)
                             :max-positionals 1
                             :message "find-window: unsupported argument")
    (let* ((target-str   (cdr (assoc #\t flags)))
           (pattern      (first positionals))
           (name-only    (assoc #\N flags))
           (title-only   (assoc #\T flags))
           (content-only (assoc #\C flags))
           (regex-p      (assoc #\r flags))
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
    (let ((target-str (cdr (assoc #\t flags))))
      (with-target-session (target-session target-str session
                                :on-missing :current)
        (let ((cycled (if (assoc #\a flags)
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
    (let ((target-str (cdr (assoc #\t flags))))
      (with-target-session (target-session target-str session
                                :on-missing :current)
        (let ((prev (session-last-window target-session)))
          (when prev
            (%with-window-focus-transition (target-session)
              (session-select-window target-session prev))
            (setf *dirty* t)
            t))))))

(defun %cmd-refresh-client-arg (session args)
  "refresh-client [-S] [-t target-client]: force a full redraw.
   cl-tmux refreshes the active client immediately; status-only and target
   arguments are accepted for tmux syntax parity."
  (declare (ignore session))
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\S #\t)
                             :max-positionals 0
                             :message "refresh-client: unsupported argument")
    (setf *dirty* t)
    t))

(defun %cmd-lock-client-arg (session args)
  "lock-client [-t target-client]: lock the active client/session.
   cl-tmux locks the active session immediately; target arguments are accepted
   for tmux syntax parity."
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\t)
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
    (let ((target-str (cdr (assoc #\t flags))))
      (with-target-session (target-session target-str session
                                :on-missing :current)
        (dispatch-command target-session :lock-session nil)
        t))))

(defun %send-keys-hex-to-string (hex)
  "Convert a send-keys -H argument (a hexadecimal character code like \"1b\" or
   \"41\") to the one-character string it names, or NIL when HEX is not a valid
   in-range code.  Mirrors tmux's send-keys -H (strtol base 16 → key).  Extracted
   as a named helper so the hex→byte logic is unit-testable without a live PTY
   (send-keys-to-pane no-ops on fd -1), matching the send-keys -l test pattern."
  (let ((code (parse-integer hex :radix 16 :junk-allowed t)))
    (when (and code (<= 0 code (1- char-code-limit)))
      (string (code-char code)))))

(defun %send-keys-reset-target-pane-terminal-state (flags target-pane)
  "Apply send-keys -R to TARGET-PANE when requested by FLAGS."
  (when (and (assoc #\R flags) target-pane (pane-screen target-pane))
    (cl-tmux/terminal/actions:ris-action (pane-screen target-pane))
    (setf *dirty* t)))

(defun %cmd-send-keys-arg (session args)
  "send-keys [-lHMR] [-N count] [-t target-pane] [-X] [key ...]: send keys or a
   copy-mode command.
   -X: the first positional is a named copy-mode command (begin-selection,
       scroll-up, etc.) dispatched to the target pane's copy mode.  -X is a
       BOOLEAN flag — the command is a positional, not -X's value.
   -N count: repeat count.  With -X, the copy-mode command runs COUNT times
       (e.g. `send -X -N 5 scroll-up`); with regular keys, the whole key sequence
       is sent COUNT times.  Default 1.
   -t: target a specific pane by pane-id or 'session:window.pane' syntax.
   -l: send each positional literally (no key-name translation).
   -H: each positional is a hexadecimal character code (e.g. `send -H 1b 5b 41`).
   -M: forward the current mouse event to the target pane.
   -R: reset the target pane's terminal state (RIS) before sending any keys.
   Without -X: each positional is a key name or literal string typed into the pane."
  (with-command-input (flags positionals args "tN"
                             :allowed-flags '(#\l #\H #\M #\R #\X #\N #\t)
                             :message "send-keys: unsupported argument")
    (let* ((target-str (cdr (assoc #\t flags)))
           (literal-p  (and (assoc #\l flags) t))
           (hex-p      (and (assoc #\H flags) t))
           (m-p        (and (assoc #\M flags) t))
           (x-p        (and (assoc #\X flags) t))
           (count      (let ((n (cdr (assoc #\N flags))))
                         (max 1 (or (and n (parse-integer n :junk-allowed t)) 1)))))
      (with-target-context (target-session target-win target-pane session target-str)
        (declare (ignore target-win))
        (let ((session target-session))
          (%send-keys-reset-target-pane-terminal-state flags target-pane)
          (cond
            (m-p
             (if *current-mouse-event*
                 (%forward-current-mouse-event-to-pane target-pane)
                 (show-overlay "send-keys: no current mouse event")))
            ;; -X: dispatch the copy-mode command (first positional) COUNT times.
            (x-p
             (when (first positionals)
               (dotimes (_ count)
                 (%dispatch-send-keys-X session (first positionals) target-pane
                                        target-win (rest positionals)))))
            ;; Regular keys: send the whole positional sequence COUNT times. With
            ;; -H each positional is a hex code -> the literal character it names.
            ((and positionals target-pane)
             (dotimes (_ count)
               (dolist (key positionals)
                 (if hex-p
                     (let ((str (%send-keys-hex-to-string key)))
                       (when str (send-keys-to-pane target-pane str :literal t)))
                     (send-keys-to-pane target-pane key :literal literal-p)))))))))))

(defun %cmd-send-prefix-arg (session args)
  "send-prefix [-2] [-t target-pane]: send the configured prefix key to a pane.
   -2 sends the secondary prefix key instead of the primary prefix.  -t targets a
   specific pane by pane-id or 'session:window.pane' syntax."
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\2 #\t)
                             :max-positionals 0
                             :message "send-prefix: unsupported argument")
    (let* ((target-str (cdr (assoc #\t flags)))
           (target-pane nil)
           (prefix-byte (if (assoc #\2 flags)
                            cl-tmux/config:*prefix2-key-code*
                            cl-tmux/config:*prefix-key-code*)))
      (with-target-context (target-session target-window pane session target-str)
        (declare (ignore target-session target-window))
        (setf target-pane pane))
      (when (and prefix-byte
                 target-pane
                 (not *client-read-only*)
                 (> (pane-fd target-pane) 0))
        (cl-tmux/pty:pty-write
         (pane-fd target-pane)
         (make-array 1 :element-type '(unsigned-byte 8)
                     :initial-element prefix-byte))
        t))))

(defun %cmd-respawn-pane-arg (session args)
  "respawn-pane [-k] [-t target-pane]: restart
   the target pane's process (default: the active pane).
   -k: kill the existing process first.  WITHOUT -k, respawning a pane whose process
   is still running is an error (tmux behaviour) — use -k to force it.
   This is the scriptable form; the interactive :respawn-pane binding is unchanged."
  (with-command-input (flags positionals args "cet"
                             :allowed-flags '(#\k #\t #\c #\e)
                             :message "respawn-pane: unsupported argument")
    (let* ((target-str (cdr (assoc #\t flags)))
           (kill-p (assoc #\k flags))
           (raw-dir (cdr (assoc #\c flags)))
           (start-dir (%expand-start-dir session raw-dir))
           (extra-env (%collect-env-flags flags))
           (default-command (format nil "~{~A~^ ~}" positionals)))
      (with-target-context (target-session win pane session target-str)
        (declare (ignore target-session))
        (when (and win pane)
          (if (and (not kill-p) (> (cl-tmux/model:pane-fd pane) 0))
              ;; tmux: respawn-pane without -k on a still-running pane is an error.
              (show-overlay "respawn-pane: pane is active (use -k to force respawn)")
              (let ((new-pane (respawn-pane session pane
                                            :start-dir start-dir
                                            :default-command (and (plusp (length default-command))
                                                                  default-command)
                                            :extra-env extra-env)))
                (when new-pane
                  (start-reader-thread new-pane)
                  (setf *dirty* t)
                  t))))))))

(defun %cmd-respawn-window-arg (session args)
  "respawn-window [-k] [-t target-window]:
   restart every pane's process in the target window (default: the active window).
   -k: kill the existing processes first.  WITHOUT -k, respawning when ANY pane is
   still running is an error (tmux behaviour) — use -k to force it.  Scriptable form; the
   interactive :respawn-window binding is unchanged."
  (with-command-input (flags positionals args "cet"
                             :allowed-flags '(#\k #\t #\c #\e)
                             :message "respawn-window: unsupported argument")
    (let* ((target-str (cdr (assoc #\t flags)))
           (win nil)
           (kill-p (assoc #\k flags))
           (raw-dir (cdr (assoc #\c flags)))
           (start-dir (%expand-start-dir session raw-dir))
           (extra-env (%collect-env-flags flags))
           (default-command (let ((command (format nil "~{~A~^ ~}" positionals)))
                              (and (plusp (length command)) command))))
      (with-target-context (target-session resolved-win target-pane session target-str)
        (declare (ignore target-session target-pane))
        (setf win resolved-win))
      (when win
        (if (and (not kill-p)
                 (some (lambda (p) (> (cl-tmux/model:pane-fd p) 0))
                       (cl-tmux/model:window-panes win)))
            ;; tmux: respawn-window without -k while panes are running is an error.
            (show-overlay "respawn-window: window has active panes (use -k to force)")
            (progn
              (dolist (pane (cl-tmux/model:window-panes win))
                (let ((new-pane (respawn-pane session pane
                                              :start-dir start-dir
                                              :default-command default-command
                                              :extra-env extra-env)))
                  (when new-pane (start-reader-thread new-pane))))
              (setf *dirty* t)
              t))))))

(defun %cmd-pipe-pane-arg (session args)
  "pipe-pane [-o] [-I] [-O] [-t target-pane] [command]: open or close a pipe
   for the target pane (default: the active pane).
   -o: only open a pipe if none is currently open (no-op when one already is).
   -I: route command stdout back into the pane.
   -O: route pane output into the command stdin.
   Without -I or -O, the default is -O.
   -t target: the pane to pipe (pane-id in the active window; bare ids and %N
   are accepted).
   Without a command: close any open pipe on the target pane."
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\I #\O #\o #\t)
                             :message "pipe-pane: unsupported argument")
    (let* ((only-open (assoc #\o flags))
           (pipe-in (assoc #\I flags))
           (pipe-out (assoc #\O flags))
           (command   (format nil "~{~A~^ ~}" positionals))
           (target-str (cdr (assoc #\t flags))))
      (let* ((win  (session-active-window session))
             (pane (if target-str
                       (%resolve-pane-in-window win target-str)
                       (session-active-pane session)))
             (pane-output-to-command-p (or pipe-out (and (not pipe-in) (not pipe-out))))
             (command-output-to-pane-p pipe-in))
        (when pane
          (cond
            ;; No command: close existing pipe.
            ((zerop (length command))
             (when (pane-pipe-active-p pane)
               (pipe-pane-close pane)))
            ;; -o: skip if already piped.
            ((and only-open (pane-pipe-active-p pane)) nil)
            ;; Open the pipe.
            (t (pipe-pane-open pane command
                               :pane-output-to-command-p pane-output-to-command-p
                               :command-output-to-pane-p command-output-to-pane-p))))))))

(defun %call-sbcl-posix (name &rest args)
  "Call the SB-POSIX function named NAME with ARGS, ignoring errors.
   Safe no-op on non-SBCL implementations or when SB-POSIX is unavailable."
  (ignore-errors
    (let ((fn (find-symbol name "SB-POSIX")))
      (when fn (apply fn args)))))

(defun %shell-single-quote (value)
  (with-output-to-string (out)
    (write-char #\' out)
    (loop for ch across value
          do (if (char= ch #\')
                 (write-string "'\\''" out)
                 (write-char ch out)))
    (write-char #\' out)))

(defun %format-show-environment-entry (name value shell-p)
  (if shell-p
      (if value
          (format nil "~A=~A; export ~A" name (%shell-single-quote value) name)
          (format nil "unset ~A" name))
      (if value
          (format nil "~A=~A" name value)
          (format nil "-~A" name))))

(defun %show-environment-name-overlay (session name shell-p)
  (multiple-value-bind (value source)
      (cl-tmux/model:session-environment-value session name)
    (declare (ignore source))
    (show-overlay
     (%format-show-environment-entry name value shell-p))))

(defun %cmd-show-environment-arg (session args)
  "show-environment [-g] [-s] [-t target] [NAME]: show environment variables.
   With NAME, show that variable.  -s prints shell assignment/unset syntax."
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\g #\s #\t)
                             :max-positionals 1
                             :message "show-environment: unsupported argument")
    (let ((shell-p (assoc #\s flags))
          (name    (first positionals))
          (global-p (assoc #\g flags))
          (target-p (assoc #\t flags)))
      (cond
        ((and global-p target-p)
         (show-overlay "show-environment: -g and -t are mutually exclusive"))
        (global-p
         (if name
             (let ((value (cl-tmux/model:process-environment-value name)))
               (show-overlay (%format-show-environment-entry name value shell-p)))
             (show-built-overlay (s)
               (format s "environment~%")
               (dolist (name (cl-tmux/model:process-environment-names))
                 (let ((value (cl-tmux/model:process-environment-value name)))
                   (if shell-p
                       (format s "~A~%" (%format-show-environment-entry name value t))
                       (format s "  ~A=~A~%" name value)))))))
        (target-p
         (let ((target-str (cdr (assoc #\t flags))))
           (with-target-session (target-session target-str session
                                  :message "show-environment: no such session: ~A"
                                  :on-missing :error)
             (if name
                 (%show-environment-name-overlay target-session name shell-p)
                 (show-built-overlay (s)
                   (format s "environment~%")
                   (dolist (name (cl-tmux/model:session-environment-names target-session))
                     (multiple-value-bind (value source)
                         (cl-tmux/model:session-environment-value target-session name)
                       (declare (ignore source))
                       (if shell-p
                           (format s "~A~%" (%format-show-environment-entry name value t))
                           (format s "  ~A=~A~%" name value)))))))))
        (t
         (if name
             (%show-environment-name-overlay session name shell-p)
             (show-built-overlay (s)
               (format s "environment~%")
               (dolist (name (cl-tmux/model:session-environment-names session))
                 (multiple-value-bind (value source)
                     (cl-tmux/model:session-environment-value session name)
                   (declare (ignore source))
                   (if shell-p
                       (format s "~A~%" (%format-show-environment-entry name value t))
                       (format s "  ~A=~A~%" name value)))))))))))

(defun %cmd-set-environment-prompt (session args)
  "set-environment [-g] [-u|-r] [-t target] NAME [VALUE]: set or unset an environment variable.
   -u (tmux's unset flag) or -r unsets the variable.  Otherwise VALUE is required."
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\g #\r #\t #\u)
                             :message "set-environment: unsupported argument")
    (let* ((remove-p (or (assoc #\u flags) (assoc #\r flags)))
           (name     (first positionals))
           (value    (format nil "~{~A~^ ~}" (rest positionals)))
           (global-p (assoc #\g flags))
           (target-p (assoc #\t flags))
           (target-str (cdr target-p)))
      (cond
        ((and global-p target-p)
         (show-overlay "set-environment: -g and -t are mutually exclusive"))
        (global-p
         (when (and name (plusp (length name)))
           (if remove-p
               (cl-tmux/model:process-unset-environment name)
               (cl-tmux/model:process-set-environment name value))))
        (target-p
         (with-target-session (target-session target-str session
                                  :message "set-environment: no such session: ~A"
                                  :on-missing :error)
           (when (and name (plusp (length name)))
             (if remove-p
                 (cl-tmux/model:session-unset-environment target-session name)
                 (cl-tmux/model:session-set-environment target-session name value)))))
        (t
         (when (and name (plusp (length name)))
           (if remove-p
               (cl-tmux/model:session-unset-environment session name)
               (cl-tmux/model:session-set-environment session name value))))))))

(defun %cmd-set-hook (session args)
  "set-hook [-g] [-a] [-R] [-u] event [command]: register or unset a command hook
   at runtime (the same backend the .tmux.conf `set-hook` directive uses, now
   reachable from command-prompt / key bindings / control mode).
     -u  unset all command hooks for EVENT.
     -R  run EVENT's hooks immediately (after setting, if a command is also given).
     -g / -a  accepted (cl-tmux keeps a flat command-hook table — global/append are
              the only behaviours), so `set-hook -g ...` works as written.
   Without -u, the tokens after EVENT are joined into one command line and stored
   as a raw string, expanded at hook-fire time via %run-command-line."
  (with-command-flags+pos (flags positionals args "")
    (let ((event   (first positionals))
          (cmd-str (when (rest positionals)
                     (format nil "~{~A~^ ~}" (rest positionals)))))
      (when event
        (cond
          ((assoc #\u flags)
           (cl-tmux/hooks:clear-command-hooks event))
          (t
           (when cmd-str
             (cl-tmux/hooks:set-command-hook event cmd-str))
           ;; -R: fire the event's hooks now.
           (when (assoc #\R flags)
             (run-command-hooks event session))))))))

(defun %cmd-bind-arg (session args)
  "bind [-n] [-r] [-T table] [-N note] key command...: bind a key at runtime
   (command-prompt / key binding / control mode).  Delegates to the config
   directive logic so the full flag set is honoured — the same path .tmux.conf
   uses.  The no-arg form falls through to the interactive bind prompt."
  (declare (ignore session))
  (cl-tmux/config:apply-config-directive (cons "bind" args)))

(defun %cmd-unbind-arg (session args)
  "unbind [-a] [-n] [-T table] [key]: unbind a key (or, with -a, every key in
   a table) at runtime, delegating to the config directive logic."
  (declare (ignore session))
  (cl-tmux/config:apply-config-directive (cons "unbind" args)))
