(in-package #:cl-tmux)

;;; -- Window navigation and session management commands ----------------------
;;;
;;; find-window, next/previous-window (alert cycling), send-keys (-H/-l/-N),
;;; list-sessions/windows/panes, respawn-pane/window, pipe-pane,
;;; set-environment, set-hook, bind-key, unbind-key, list-commands,
;;; server-access, customize-mode.
(defun %window-matches-pattern-p (window pattern &key name-only)
  "T when WINDOW matches PATTERN by case-insensitive substring.  The window name
   is always searched; unless NAME-ONLY, each pane's title and screen title are
   searched too (approximating find-window's name/title/content scan).  Shared by
   the scriptable find-window command and the interactive :find-window binding."
  (or (search pattern (window-name window) :test #'char-equal)
      (and (not name-only)
           (some (lambda (p)
                   (let ((title  (cl-tmux/model:pane-title p))
                         (screen (cl-tmux/model:pane-screen p)))
                     (or (and (plusp (length title))
                              (search pattern title :test #'char-equal))
                         (and screen
                              (search pattern (cl-tmux/terminal:screen-title screen)
                                      :test #'char-equal)))))
                 (cl-tmux/model:window-panes window)))))

(defun %cmd-find-window-arg (session args)
  "find-window [-CNiTrZ] [-t target-pane] match-string: find the window whose name
   (or, unless -N, a pane title/content) matches MATCH-STRING and select it.  With
   several matches, the first is selected.  The match is case-insensitive substring
   (as in the interactive find-window); -i/-r/-C/-T/-Z are accepted.  This is the
   scriptable form; the interactive :find-window binding (which lists matches in an
   overlay) is unchanged."
  (with-command-flags+pos (flags positionals args "t")
    (let* ((name-only (assoc #\N flags))
           (pattern   (first positionals)))
      (when (and pattern (plusp (length pattern)))
        (let ((match (find-if (lambda (w)
                                (%window-matches-pattern-p w pattern :name-only name-only))
                              (session-windows session))))
          (when match
            (session-select-window session match)
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

(defun %cycle-window-in-target (session args cycler)
  "Resolve -t to a target session (default SESSION) and cycle its active window
   with CYCLER (next-cyclic / prev-cyclic).  Shared by the scriptable
   next-window / previous-window commands.  -a cycles to the next/prev window with
   an alert (activity or silence); without -a, plain window cycling."
  (with-command-flags+pos (flags positionals args "t")
    (declare (ignore positionals))
    (let* ((target-str (cdr (assoc #\t flags)))
           (target     (or (and target-str
                                 (find-session-by-target *server-sessions* target-str))
                           session)))
      (if (assoc #\a flags)
          (%cycle-to-alert-window target cycler)
          (%cmd-cycle-window target cycler))
      (setf *dirty* t)
      t)))

(defun %cmd-next-window-arg (session args)
  "next-window [-a] [-t target-session]: select the next window in the target
   session (default: the current session).  Scriptable form; the interactive
   :next-window binding (current session) is unchanged."
  (%cycle-window-in-target session args #'next-cyclic))

(defun %cmd-previous-window-arg (session args)
  "previous-window [-a] [-t target-session]: select the previous window in the
   target session (default: the current session)."
  (%cycle-window-in-target session args #'prev-cyclic))

(defun %send-keys-hex-to-string (hex)
  "Convert a send-keys -H argument (a hexadecimal character code like \"1b\" or
   \"41\") to the one-character string it names, or NIL when HEX is not a valid
   in-range code.  Mirrors tmux's send-keys -H (strtol base 16 → key).  Extracted
   as a named helper so the hex→byte logic is unit-testable without a live PTY
   (send-keys-to-pane no-ops on fd -1), matching the send-keys -l test pattern."
  (let ((code (parse-integer hex :radix 16 :junk-allowed t)))
    (when (and code (<= 0 code (1- char-code-limit)))
      (string (code-char code)))))

(defun %cmd-send-keys-arg (session args)
  "send-keys [-lHR] [-N count] [-t target-pane] [-X] [key ...]: send keys or a
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
   -R: reset the target pane's terminal state (RIS) before sending any keys.
   -M (mouse passthrough) is accepted but not acted on (needs mouse-event context).
   Without -X: each positional is a key name or literal string typed into the pane."
  (with-command-flags+pos (flags positionals args "tN")
    (let* ((target-str (cdr (assoc #\t flags)))
           (literal-p  (and (assoc #\l flags) t))
           (hex-p      (and (assoc #\H flags) t))
           (x-p        (and (assoc #\X flags) t))
           (count      (let ((n (cdr (assoc #\N flags))))
                         (max 1 (or (and n (parse-integer n :junk-allowed t)) 1))))
           ;; Resolve -t to a specific window+pane; fall back to the active ones.
           ;; The window is needed so a copy-mode -X command can be routed to a
           ;; non-active target pane (see %dispatch-send-keys-X).
           (target-resolved (and target-str
                                 (multiple-value-list
                                  (resolve-target *server-sessions* target-str
                                                  :current-session session
                                                  :current-window  (session-active-window session)
                                                  :current-pane    (session-active-pane session)))))
           (target-win  (if target-str (second target-resolved)
                            (session-active-window session)))
           (target-pane (if target-str (third target-resolved)
                            (session-active-pane session))))
      ;; -R: reset the target pane's terminal state (RIS — clears the grid, homes
      ;; the cursor, resets SGR + modes) so a pane left in a confused state by a
      ;; crashed full-screen app recovers.  Runs before any keys are sent.
      (when (and (assoc #\R flags) target-pane (pane-screen target-pane))
        (cl-tmux/terminal/actions:ris-action (pane-screen target-pane))
        (setf *dirty* t))
      (cond
        ;; -X: dispatch the copy-mode command (first positional) COUNT times.
        (x-p
         (when (first positionals)
           (dotimes (_ count)
             (%dispatch-send-keys-X session (first positionals) target-pane target-win
                                    (rest positionals)))))
        ;; Regular keys: send the whole positional sequence COUNT times.  With -H
        ;; each positional is a hex code → the literal character it names.
        ((and positionals target-pane)
         (dotimes (_ count)
           (dolist (key positionals)
             (if hex-p
                 (let ((str (%send-keys-hex-to-string key)))
                   (when str (send-keys-to-pane target-pane str :literal t)))
                 (send-keys-to-pane target-pane key :literal literal-p)))))))))

(defun %cmd-list-sessions-arg (session args)
  "list-sessions [-F format]: list sessions.
   -F format: custom format string (default: shows name, windows, attached).
   Shows overlay in standalone mode."
  (with-command-flags (flags args "F")
    (let ((fmt (cdr (assoc #\F flags))))
      (if fmt
          (show-built-overlay (s)
             (dolist (sess (or (mapcar #'cdr *server-sessions*) (list session)))
               (let* ((ctx (cl-tmux/format:format-context-from-session
                            sess (session-active-window sess) nil)))
                 (format s "~A~%" (cl-tmux/format:expand-format fmt ctx)))))
          (show-overlay (%format-session-list session))))))

(defun %cmd-list-windows-arg (session args)
  "list-windows [-F format] [-a] [-t session]: list windows.
   -F format: custom format string.
   -a: list windows in all sessions."
  (with-command-flags (flags args "Ft")
    (let* ((fmt    (cdr (assoc #\F flags)))
           (all-p  (assoc #\a flags))
           (sessions (if (and all-p *server-sessions*)
                         (mapcar #'cdr *server-sessions*)
                         (list session))))
      (show-built-overlay (s)
        (dolist (sess sessions)
          (dolist (win (session-windows sess))
            (if fmt
                (let ((ctx (cl-tmux/format:format-context-from-window sess win)))
                  (format s "~A~%" (cl-tmux/format:expand-format fmt ctx)))
                (format s "~A: ~A (~Dx~D) [~D pane~:P]~A~%"
                        (window-id win) (window-name win)
                        (window-width win) (window-height win)
                        (length (window-panes win))
                        (if (eq win (session-active-window sess)) " [active]" ""))))))))

(defun %cmd-list-panes-arg-full (session args)
  "list-panes [-F format] [-a] [-t target]: list panes.
   -F format: custom format string."
  (with-command-flags (flags args "Ft")
    (let* ((fmt   (cdr (assoc #\F flags)))
           (win   (session-active-window session)))
      (show-built-overlay (s)
        (when win
          (dolist (pane (window-panes win))
            (if fmt
                (let ((ctx (cl-tmux/format:format-context-from-session
                             session win pane)))
                  (format s "~A~%" (cl-tmux/format:expand-format fmt ctx)))
                (format s "~D: [~Dx~D] [~D,~D] pane ~D~A~%"
                        (pane-id pane)
                        (pane-width pane) (pane-height pane)
                        (pane-x pane) (pane-y pane)
                        (pane-id pane)
                        (if (eq pane (window-active-pane win)) " (active)" ""))))))))

(defun %cmd-respawn-pane-arg (session args)
  "respawn-pane [-k] [-c start-dir] [-e VAR=val] [-t target-pane] [command]: restart
   the target pane's process (default: the active pane).
   -k: kill the existing process first.  WITHOUT -k, respawning a pane whose process
   is still running is an error (tmux behaviour) — use -k to force it.
   -c/-e/command are accepted for compatibility; the respawn currently reuses the
   pane's default shell (start-dir/env/command override is not yet modelled).
   This is the scriptable form; the interactive :respawn-pane binding is unchanged."
  (with-command-flags+pos (flags positionals args "cet")
    (declare (ignore positionals))
    (let* ((win    (session-active-window session))
           (pane   (%resolve-pane-in-window win (cdr (assoc #\t flags))))
           (kill-p (assoc #\k flags)))
      (when pane
        (if (and (not kill-p) (> (cl-tmux/model:pane-fd pane) 0))
            ;; tmux: respawn-pane without -k on a still-running pane is an error.
            (show-overlay "respawn-pane: pane is active (use -k to force respawn)")
            (let ((new-pane (respawn-pane pane)))
              (when new-pane
                (start-reader-thread new-pane)
                (setf *dirty* t)
                t)))))))

(defun %cmd-respawn-window-arg (session args)
  "respawn-window [-k] [-c start-dir] [-e VAR=val] [-t target-window] [command]:
   restart every pane's process in the target window (default: the active window).
   -k: kill the existing processes first.  WITHOUT -k, respawning when ANY pane is
   still running is an error (tmux behaviour) — use -k to force it.  -c/-e/command
   are accepted for compatibility (override not yet modelled).  Scriptable form; the
   interactive :respawn-window binding is unchanged."
  (with-command-flags+pos (flags positionals args "cet")
    (declare (ignore positionals))
    (let* ((target-str (cdr (assoc #\t flags)))
           (win    (if target-str
                       (%resolve-window-target session target-str)
                       (session-active-window session)))
           (kill-p (assoc #\k flags)))
      (when win
        (if (and (not kill-p)
                 (some (lambda (p) (> (cl-tmux/model:pane-fd p) 0))
                       (cl-tmux/model:window-panes win)))
            ;; tmux: respawn-window without -k while panes are running is an error.
            (show-overlay "respawn-window: window has active panes (use -k to force)")
            (progn
              (dolist (pane (cl-tmux/model:window-panes win))
                (let ((new-pane (respawn-pane pane)))
                  (when new-pane (start-reader-thread new-pane))))
              (setf *dirty* t)
              t))))))

(defun %cmd-pipe-pane-arg (session args)
  "pipe-pane [-IOo] [-t target-pane] [command]: open or close a pipe for the
   target pane (default: the active pane).
   -o: only open a pipe if none is currently open (no-op when one already is).
   -t target: the pane to pipe (pane-id in the active window).
   -I/-O (pipe pane input / output) are accepted; cl-tmux pipes pane OUTPUT.
   Without a command: close any open pipe on the target pane."
  (with-command-flags+pos (flags positionals args "t")
    (let* ((only-open (assoc #\o flags))
           (command   (format nil "~{~A~^ ~}" positionals))
           (win       (session-active-window session))
           (pane      (%resolve-pane-in-window win (cdr (assoc #\t flags)))))
      (when pane
        (cond
          ;; No command: close existing pipe
          ((zerop (length command))
           (when (pane-pipe-fd pane) (pipe-pane-close pane)))
          ;; -o: skip if already piped
          ((and only-open (pane-pipe-fd pane)) nil)
          ;; Open the pipe
          (t (pipe-pane-open pane command)))))))

(defun %call-sbcl-posix (name &rest args)
  "Call the SB-POSIX function named NAME with ARGS, ignoring errors.
   Safe no-op on non-SBCL implementations or when SB-POSIX is unavailable."
  (ignore-errors
    (let ((fn (find-symbol name "SB-POSIX")))
      (when fn (apply fn args)))))

(defun %cmd-set-environment-prompt (session args)
  "set-environment [-u|-r] NAME [VALUE]: set or unset a process environment variable.
   -u (tmux's unset flag) or -r unsets the variable.  Otherwise VALUE is required."
  (declare (ignore session))
  (with-command-flags+pos (flags positionals args "")
    (let* ((remove-p (or (assoc #\u flags) (assoc #\r flags)))
           (name     (first positionals))
           (value    (format nil "~{~A~^ ~}" (rest positionals))))
      (when (and name (plusp (length name)))
        (if remove-p
            (%call-sbcl-posix "UNSETENV" name)
            (%call-sbcl-posix "SETENV" name value 1))))))

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

(defun %cmd-bind-key-arg (session args)
  "bind / bind-key [-n] [-r] [-T table] [-N note] key command...: bind a key at
   runtime (command-prompt / key binding / control mode).  Delegates to the config
   directive logic so the full flag set is honoured — the same path .tmux.conf
   uses.  The no-arg form falls through to the interactive :bind-key prompt."
  (declare (ignore session))
  (cl-tmux/config:apply-config-directive (cons "bind" args)))

(defun %cmd-unbind-key-arg (session args)
  "unbind / unbind-key [-a] [-n] [-T table] [key]: unbind a key (or, with -a, every
   key in a table) at runtime, delegating to the config directive logic."
  (declare (ignore session))
  (cl-tmux/config:apply-config-directive (cons "unbind" args)))

(defun %cmd-list-commands-arg (session args)
  "list-commands [command]: list the recognised commands one per line; with a
   COMMAND name, show only that command (tmux's `list-commands <name>`).  Without a
   name this matches the interactive :list-commands binding (the full list)."
  (declare (ignore session))
  (let* ((name     (first args))
         (cmds     (sort (copy-list cl-tmux/config::*bindable-commands*)
                         #'string< :key #'symbol-name))
         (filtered (if (and name (plusp (length name)))
                       (remove-if-not
                        (lambda (c) (string-equal (symbol-name c) name)) cmds)
                       cmds)))
    (show-built-overlay (s)
      (dolist (cmd filtered)
        (format s "~(~A~)~%" cmd)))))

(defun %cmd-wait-for-arg (session args)
  "wait-for [-SLU] channel: channel synchronization.
   Bare: block the calling thread until CHANNEL is signaled (or timeout elapses).
   -S: signal (unblock) all threads waiting on CHANNEL.
   -L: lock CHANNEL so subsequent signal calls are suppressed.
   -U: unlock CHANNEL, re-enabling signal-channel."
  (declare (ignore session))
  (with-command-flags+pos (flags positionals args)  ; S/L/U are boolean (no value)
    (let ((channel (first positionals)))
      (when (and channel (plusp (length channel)))
        (cond
          ((assoc #\S flags) (signal-channel channel))
          ((assoc #\L flags) (lock-channel   channel))
          ((assoc #\U flags) (unlock-channel channel))
          (t                 (wait-for-channel channel)))))))

