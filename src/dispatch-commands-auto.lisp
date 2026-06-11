(in-package #:cl-tmux)

;;; -- Automation and scripting commands -----------------------------------------------
;;;
;;; send-keys (-X copy-mode / -H hex / -l literal), run-shell, if-shell (-F),
;;; capture-pane, resize-pane, join-pane, break-pane, clear-history,
;;; rotate-window, find-window, next/previous-window, list-sessions/windows/panes,
;;; respawn-pane/window, pipe-pane, set-environment, set-hook, bind-key,
;;; unbind-key, list-commands, server-access, customize-mode.

(defun %dispatch-send-keys-X (session command-name &optional target-pane target-window extra-args)
  "Dispatch a send-keys -X COMMAND-NAME against TARGET-PANE's copy mode (default:
   the active pane).  Copy-mode -X commands act on the session's ACTIVE screen, so
   when TARGET-PANE is a non-active pane it is TEMPORARILY focused — a raw
   active-slot swap (session-active + window-active), restored via unwind-protect,
   with NO focus events or last-active updates — so the command operates on the
   target while leaving the real focus unchanged.  Returns T when COMMAND-NAME is
   a recognised copy-mode command.
   EXTRA-ARGS (a list of strings) holds any positional arguments after the command
   name; used by copy-pipe / copy-pipe-and-cancel to carry the pipe-command string."
  ;; jump-forward/backward/to/to-backward: require a char argument (the target).
  ;; `send -X jump-forward a` passes "a" as the first extra-arg.
  (when (and extra-args
             (member command-name '("jump-forward" "jump-backward"
                                    "jump-to" "jump-to-backward")
                                 :test #'string-equal))
    (let* ((char-arg (first extra-args))
           (target-ch (and char-arg (plusp (length char-arg)) (char char-arg 0)))
           (pane   (or target-pane (session-active-pane session)))
           (screen (and pane (cl-tmux/model:pane-screen pane))))
      (when (and screen target-ch)
        (cond
          ((string-equal command-name "jump-forward")
           (copy-mode-jump-forward screen target-ch))
          ((string-equal command-name "jump-backward")
           (copy-mode-jump-backward screen target-ch))
          ((string-equal command-name "jump-to")
           (copy-mode-jump-to screen target-ch))
          ((string-equal command-name "jump-to-backward")
           (copy-mode-jump-to-backward screen target-ch))))
      (return-from %dispatch-send-keys-X (and screen target-ch t))))
  ;; goto-line N: requires a numeric line-number argument.
  ;; `send -X goto-line 42` passes "42" as the first extra-arg.
  (when (string-equal command-name "goto-line")
    (let* ((pane   (or target-pane (session-active-pane session)))
           (screen (and pane (cl-tmux/model:pane-screen pane)))
           (n-str  (first extra-args))
           (n      (and n-str (plusp (length n-str))
                        (ignore-errors (parse-integer n-str)))))
      (when (and screen (integerp n))
        (copy-mode-goto-line screen n))
      (return-from %dispatch-send-keys-X (and screen (integerp n) t))))
  ;; search-forward-text / search-backward-text: scripted search with the text
  ;; passed as extra-args (no interactive prompt).  `send -X search-forward-text "foo"`
  (when (and extra-args
             (member command-name '("search-forward-text" "search-backward-text")
                                 :test #'string-equal))
    (let* ((pane   (or target-pane (session-active-pane session)))
           (screen (and pane (cl-tmux/model:pane-screen pane)))
           (term   (first extra-args)))
      (when (and screen term (plusp (length term)))
        (if (string-equal command-name "search-forward-text")
            (copy-mode-search-forward  screen term)
            (copy-mode-search-backward screen term)))
      (return-from %dispatch-send-keys-X (and screen term t))))
  ;; copy-pipe / copy-pipe-and-cancel with an explicit pipe-command argument:
  ;; bypass the keyword table (which cannot carry per-invocation args) and call
  ;; the copy-mode function directly with the argument string.
  (when (and extra-args
             (member command-name '("copy-pipe" "copy-pipe-and-cancel"
                                    "pipe" "pipe-and-cancel" "pipe-no-clear")
                                 :test #'string-equal))
    (let* ((pane   (or target-pane (session-active-pane session)))
           (screen (and pane (cl-tmux/model:pane-screen pane))))
      (when screen
        (if (member command-name '("copy-pipe-and-cancel" "pipe-and-cancel")
                                 :test #'string-equal)
            (copy-mode-copy-pipe          screen (first extra-args))
            (copy-mode-copy-pipe-no-cancel screen (first extra-args))))
      (return-from %dispatch-send-keys-X (and screen t))))
  ;; Standard keyword dispatch.
  (let ((kw (cdr (assoc command-name *copy-mode-x-commands* :test #'string-equal))))
    (when kw
      (if (and target-pane target-window
               (not (eq target-pane (session-active-pane session))))
          (let ((prev-win  (cl-tmux/model:session-active session))
                (prev-pane (cl-tmux/model:window-active target-window)))
            (unwind-protect
                 (progn
                   (setf (cl-tmux/model:session-active session)      target-window
                         (cl-tmux/model:window-active   target-window) target-pane)
                   (dispatch-command session kw nil))
              (setf (cl-tmux/model:session-active session)       prev-win
                    (cl-tmux/model:window-active  target-window) prev-pane)))
          (dispatch-command session kw nil))
      t)))

(defun %cmd-run-shell-arg (session args)
  "run-shell [-b] command: run COMMAND in a shell and show the output.
   -b: run in background (fire-and-forget, no output shown).
   The command is run via /bin/sh -c."
  (multiple-value-bind (flags positionals) (%parse-command-flags args "")
    (let* ((bg-p    (assoc #\b flags))
           (command (format nil "~{~A~^ ~}" positionals)))
      (when (plusp (length command))
        (if bg-p
            (run-shell command :background t)
            (let ((output (run-shell command)))
              ;; Show output when non-empty; show "(no output)" when empty
              ;; so users know the command ran successfully.
              (let ((text (if (and output (plusp (length output)))
                              output
                              "(run-shell: no output)")))
                (show-overlay text))))))))

(defun %cmd-if-shell-arg (session args)
  "if-shell [-F] condition [then-cmd] [else-cmd]: conditional command execution.
   -F: treat condition as a format string (#{var}) instead of a shell command.
   Without -F: runs condition as shell; exit 0 = truthy."
  (multiple-value-bind (flags positionals) (%parse-command-flags args "")
    (let* ((format-p (assoc #\F flags))
           (cond-str (first positionals))
           (then-str (second positionals))
           (else-str (third positionals)))
      (when cond-str
        (if format-p
            ;; -F: expand the condition as a format string
            (let* ((win    (session-active-window session))
                   (pane   (session-active-pane session))
                   (ctx    (cl-tmux/format:format-context-from-session session win pane))
                   (result (cl-tmux/format:expand-format cond-str ctx))
                   (truthy (and result (plusp (length result)) (not (string= result "0")))))
              (when truthy (when then-str (%run-command-line session then-str)))
              (unless truthy (when else-str (%run-command-line session else-str))))
            ;; Plain shell: run condition and check exit code
            (if-shell cond-str
                      (lambda () (when then-str (%run-command-line session then-str)))
                      :else-fn (lambda () (when else-str (%run-command-line session else-str)))))))))

(defun %cmd-capture-pane-arg (session args)
  "capture-pane [-p] [-S start] [-E end] [-b buffer] [-JeNaP] [-t target]: capture
   the pane's content.
   Default (no -p): SAVE the captured text to a paste buffer (retrievable with
     paste-buffer) — tmux's default behaviour, and the canonical capture→paste
     workflow.  Silent (no overlay).
   -p: print to stdout (shown as an overlay in standalone mode) instead of saving.
   -S start: include scrollback.  A line number or '-' (start of history) both
     include the full scrollback above the visible region.
   -E end: accepted (end line); the visible bottom is the end here.
   -b name: store the capture in the buffer named NAME (retrievable with
     paste-buffer -b NAME); without -b an automatic name is assigned.
   -e: include SGR escape sequences so captured colours/attributes are preserved.
   -J: preserve trailing spaces AND rejoin lines that wrapped at the right margin
     into one logical line (default strips trailing spaces and keeps every row a
     separate line, like tmux).  Joining uses the screen's per-row wrap flags and
     applies to the visible region (scrollback rows are not joined).
   -N: preserve trailing spaces WITHOUT joining wrapped lines (the difference from -J).
   -a / -P: accepted but not specially handled.
   -t target: target pane (standalone uses the active pane)."
  (multiple-value-bind (flags _positionals) (%parse-command-flags args "tSEb")
    (declare (ignore _positionals))
    (let* ((print-p (assoc #\p flags))
           (include-scrollback (assoc #\S flags))
           (escapes  (assoc #\e flags))      ; -e: keep SGR colour/attr escapes
           (join     (assoc #\J flags))      ; -J: preserve trailing spaces + join wraps
           (preserve (assoc #\N flags))      ; -N: preserve trailing spaces, no join
           (pane (session-active-pane session))
           (content (and pane (capture-pane pane
                                            :include-scrollback (and include-scrollback t)
                                            :escapes (and escapes t)
                                            :join    (and join t)
                                            :preserve-trailing (and preserve t)))))
      (when content
        (if print-p
            ;; -p: stdout equivalent — show the content in an overlay.
            (show-overlay content)
            ;; Default: save to a paste buffer (silent), like tmux.  -b names it.
            (cl-tmux/buffer:add-paste-buffer content (cdr (assoc #\b flags))))))))

(defun %cmd-resize-pane-arg (session args)
  "resize-pane [-t target] [-L|-R|-U|-D|-Z] [-x width] [-y height] [amount]: resize a pane.
   -t target: target pane by pane-id or 'session:window.pane' (default: active pane).
   -L/-R/-U/-D: resize by AMOUNT (default 5) in the given direction.
   -x N / -y N: resize to an ABSOLUTE width/height of N cells (computed as a delta
   from the pane's current size and applied via the :right/:down border move; both
   may be given together).
   -Z: zoom-toggle the target pane."
  (multiple-value-bind (flags positionals) (%parse-command-flags args "txy")
    (let* ((amount-str (first positionals))
           (amount     (or (and amount-str (parse-integer amount-str :junk-allowed t)) 5))
           (x-str      (cdr (assoc #\x flags)))
           (y-str      (cdr (assoc #\y flags)))
           (x-val      (and x-str (parse-integer x-str :junk-allowed t)))
           (y-val      (and y-str (parse-integer y-str :junk-allowed t)))
           ;; Resolve target pane; fall back to active window for resize operations.
           (target-str (cdr (assoc #\t flags)))
           (win        (if target-str
                           ;; Resolve target to its window; resize operates on the window.
                           (multiple-value-bind (_s target-win _p)
                               (resolve-target *server-sessions* target-str
                                               :current-session session
                                               :current-window  (session-active-window session))
                             (declare (ignore _s _p))
                             target-win)
                           (session-active-window session))))
      (cond
        ((assoc #\Z flags)
         (when win (window-zoom-toggle win)))
        ;; -x/-y: absolute resize.  Move the relevant border by (target - current);
        ;; a signed delta grows (positive) or shrinks (negative) the active pane.
        ((or x-val y-val)
         (when win
           (let ((ap (window-active-pane win)))
             (when ap
               (when x-val
                 (let ((dx (- x-val (cl-tmux/model:pane-width ap))))
                   (unless (zerop dx) (resize-pane win :right dx))))
               (when y-val
                 (let ((dy (- y-val (cl-tmux/model:pane-height ap))))
                   (unless (zerop dy) (resize-pane win :down dy))))))))
        ((assoc #\L flags) (when win (resize-pane win :left  amount)))
        ((assoc #\R flags) (when win (resize-pane win :right amount)))
        ((assoc #\U flags) (when win (resize-pane win :up    amount)))
        ((assoc #\D flags) (when win (resize-pane win :down  amount)))))))

(defun %cmd-join-pane-arg (session args)
  "join-pane / move-pane [-bdhv] [-l size] [-s src-pane] [-t dst-pane]: move
   SRC-PANE out of its window and into DST-PANE's window as a new split.
   -h splits left/right; -v (the default, as for split-window) splits top/bottom.
   -s source pane (default: the active pane); -t destination pane, whose WINDOW
   receives the split (default: the active window).
   -d keeps the current pane active (no switch to the joined pane).  -b (insert
   before) and -l (size) are accepted for compatibility; the split uses the
   model's default placement and even sizing.
   No-op when source and destination resolve to the same window (nothing to move).
   This is the scriptable form; the interactive :join-pane / :move-pane keybindings
   (which prompt for a window index) are unchanged."
  (multiple-value-bind (flags positionals) (%parse-command-flags args "stl")
    (declare (ignore positionals))
    (let* ((src-str  (cdr (assoc #\s flags)))
           (dst-str  (cdr (assoc #\t flags)))
           (dir      (if (assoc #\h flags) :h :v))
           (cur-win  (session-active-window session))
           (src-win  (if *server-marked-pane*
                         (pane-window *server-marked-pane*)
                         cur-win))
           (src-pane (or *server-marked-pane*
                         (and cur-win (window-active-pane cur-win))))
           (dst-win  cur-win))
      ;; -s: resolve the source pane (and its window).  When the target names a
      ;; window but no pane, take THAT window's active pane (not the current
      ;; window's, which is resolve-target's current-pane default).
      (when src-str
        (multiple-value-bind (s w p)
            (resolve-target *server-sessions* src-str
                            :current-session session :current-window cur-win
                            :current-pane src-pane)
          (declare (ignore s))
          (when w
            (setf src-win  w
                  src-pane (if (and p (member p (window-panes w))) p
                               (window-active-pane w))))))
      ;; -t: resolve the destination — only its WINDOW matters (the split host).
      (when dst-str
        (multiple-value-bind (s w p)
            (resolve-target *server-sessions* dst-str
                            :current-session session :current-window cur-win)
          (declare (ignore s p))
          (setf dst-win w)))
      (when (and src-win src-pane dst-win (not (eq src-win dst-win))
                 (join-pane session src-win src-pane dst-win dir))
        ;; tmux makes the joined pane active unless -d.
        (unless (assoc #\d flags)
          (window-select-pane dst-win src-pane))
        (setf *dirty* t)
        t))))

(defun %cmd-break-pane-arg (session args)
  "break-pane [-dP] [-n window-name] [-s src-pane] [-t dst-window] [-F format]:
   move a pane out of its window into a new window of its own.
   -d: don't switch to the new window (stay on the current one).
   -n name: name the new window (default: the shell basename).
   -s src-pane: the pane to break out (default: the active pane).
   -t/-F/-P/-a: accepted for compatibility; target position and the -P/-F print
   form are not specially handled (the new window goes at the next free id).
   No-op when the source window has fewer than two panes.  This is the scriptable
   form; the interactive :break-pane keybinding is unchanged."
  (multiple-value-bind (flags positionals) (%parse-command-flags args "nstF")
    (declare (ignore positionals))
    (let* ((detach   (assoc #\d flags))
           (name     (cdr (assoc #\n flags)))
           (src-str  (cdr (assoc #\s flags)))
           (cur-win  (session-active-window session))
           (src-win  cur-win)
           (src-pane (and cur-win (window-active-pane cur-win))))
      ;; -s: resolve the source pane (and its window); a window-only target uses
      ;; that window's own active pane (not the current window's).
      (when src-str
        (multiple-value-bind (s w p)
            (resolve-target *server-sessions* src-str
                            :current-session session :current-window cur-win
                            :current-pane src-pane)
          (declare (ignore s))
          (when w
            (setf src-win  w
                  src-pane (if (and p (member p (window-panes w))) p
                               (window-active-pane w))))))
      (let ((new-win (cl-tmux/commands:break-pane
                      session :src-window src-win :pane src-pane
                              :name name :select (not detach))))
        (when new-win
          (setf *dirty* t)
          t)))))

(defun %cmd-clear-history-arg (session args)
  "clear-history [-H] [-t target-pane]: clear a pane's scrollback history.
   -t target-pane: the pane to clear (default: the active pane); a window-only
   target clears that window's active pane.
   -H: accepted (tmux also drops the alternate-screen history); cl-tmux clears the
   pane's scrollback regardless.  This is the scriptable form; the interactive
   :clear-history keybinding (active pane) is unchanged."
  (multiple-value-bind (flags positionals) (%parse-command-flags args "t")
    (declare (ignore positionals))
    (let* ((target-str (cdr (assoc #\t flags)))
           (cur-win    (session-active-window session))
           (pane       (and cur-win (window-active-pane cur-win))))
      (when target-str
        (multiple-value-bind (s w p)
            (resolve-target *server-sessions* target-str
                            :current-session session :current-window cur-win
                            :current-pane pane)
          (declare (ignore s))
          (when w
            (setf pane (if (and p (member p (window-panes w))) p
                           (window-active-pane w))))))
      (when pane
        (cl-tmux/terminal/actions:clear-scrollback (pane-screen pane))
        (setf *dirty* t)
        t))))

(defun %cmd-rotate-window-arg (session args)
  "rotate-window [-DUZ] [-t target-window]: rotate the pane order in a window.
   -U (the default) rotates forward (the first pane moves to the end); -D rotates
   backward.  -t target-window: the window to rotate (default: the active window).
   -Z (keep zoom) is accepted but not specially handled.  This is the scriptable
   form; the interactive :rotate-window / :rotate-window-reverse bindings are
   unchanged."
  (multiple-value-bind (flags positionals) (%parse-command-flags args "t")
    (declare (ignore positionals))
    (let* ((target-str (cdr (assoc #\t flags)))
           (dir        (if (assoc #\D flags) :down :up))
           (cur-win    (session-active-window session))
           (win        cur-win))
      (when target-str
        (multiple-value-bind (s w p)
            (resolve-target *server-sessions* target-str
                            :current-session session :current-window cur-win)
          (declare (ignore s p))
          (when w (setf win w))))
      (when win
        (window-rotate win dir)
        (setf *dirty* t)
        t))))

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
  (multiple-value-bind (flags positionals) (%parse-command-flags args "t")
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
  (multiple-value-bind (flags positionals) (%parse-command-flags args "t")
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
  (multiple-value-bind (flags positionals) (%parse-command-flags args "tN")
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
  (multiple-value-bind (flags _positionals) (%parse-command-flags args "F")
    (declare (ignore _positionals))
    (let* ((fmt (cdr (assoc #\F flags))))
      (if fmt
          ;; Custom format: expand for each session
          (show-overlay
           (with-output-to-string (s)
             (if *server-sessions*
                 (dolist (entry *server-sessions*)
                   (let ((sess (cdr entry)))
                     (let ((ctx (cl-tmux/format:format-context-from-session
                                 sess (session-active-window sess) nil)))
                       (format s "~A~%" (cl-tmux/format:expand-format fmt ctx)))))
                 (let ((ctx (cl-tmux/format:format-context-from-session
                             session (session-active-window session) nil)))
                   (format s "~A~%" (cl-tmux/format:expand-format fmt ctx))))))
          ;; Default format
          (show-overlay (%format-session-list session))))))

(defun %cmd-list-windows-arg (session args)
  "list-windows [-F format] [-a] [-t session]: list windows.
   -F format: custom format string.
   -a: list windows in all sessions."
  (multiple-value-bind (flags _positionals) (%parse-command-flags args "Ft")
    (declare (ignore _positionals))
    (let* ((fmt    (cdr (assoc #\F flags)))
           (all-p  (assoc #\a flags))
           (sessions (if (and all-p *server-sessions*)
                         (mapcar #'cdr *server-sessions*)
                         (list session))))
      (show-overlay
       (with-output-to-string (s)
         (dolist (sess sessions)
           (dolist (win (session-windows sess))
             (if fmt
                 (let ((ctx (cl-tmux/format:format-context-from-window sess win)))
                   (format s "~A~%" (cl-tmux/format:expand-format fmt ctx)))
                 (format s "~A: ~A (~Dx~D) [~D pane~:P]~A~%"
                         (window-id win) (window-name win)
                         (window-width win) (window-height win)
                         (length (window-panes win))
                         (if (eq win (session-active-window sess)) " [active]" ""))))))))))

(defun %cmd-list-panes-arg-full (session args)
  "list-panes [-F format] [-a] [-t target]: list panes.
   -F format: custom format string."
  (multiple-value-bind (flags _positionals) (%parse-command-flags args "Ft")
    (declare (ignore _positionals))
    (let* ((fmt   (cdr (assoc #\F flags)))
           (win   (session-active-window session)))
      (show-overlay
       (with-output-to-string (s)
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
                         (if (eq pane (window-active-pane win)) " (active)" ""))))))))))

(defun %cmd-respawn-pane-arg (session args)
  "respawn-pane [-k] [-c start-dir] [-e VAR=val] [-t target-pane] [command]: restart
   the target pane's process (default: the active pane).
   -k: kill the existing process first.  WITHOUT -k, respawning a pane whose process
   is still running is an error (tmux behaviour) — use -k to force it.
   -c/-e/command are accepted for compatibility; the respawn currently reuses the
   pane's default shell (start-dir/env/command override is not yet modelled).
   This is the scriptable form; the interactive :respawn-pane binding is unchanged."
  (multiple-value-bind (flags positionals) (%parse-command-flags args "cet")
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
  (multiple-value-bind (flags positionals) (%parse-command-flags args "cet")
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
  (multiple-value-bind (flags positionals) (%parse-command-flags args "t")
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

(defun %cmd-set-environment-prompt (session args)
  "set-environment [-u|-r] NAME [VALUE]: set or unset a process environment variable.
   -u (tmux's unset flag) or -r unsets the variable.  Otherwise VALUE is required."
  (declare (ignore session))
  (multiple-value-bind (flags positionals) (%parse-command-flags args "")
    (let* ((remove-p (or (assoc #\u flags) (assoc #\r flags)))
           (name     (first positionals))
           (value    (format nil "~{~A~^ ~}" (rest positionals))))
      (when (and name (plusp (length name)))
        (if remove-p
            (ignore-errors
              (let ((fn (find-symbol "UNSETENV" (find-package "SB-POSIX"))))
                (when fn (funcall fn name))))
            (ignore-errors
              (let ((fn (find-symbol "SETENV" (find-package "SB-POSIX"))))
                (when fn (funcall fn name value 1)))))))))

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
  (multiple-value-bind (flags positionals) (%parse-command-flags args "")
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
    (show-overlay
     (with-output-to-string (s)
       (dolist (cmd filtered)
         (format s "~(~A~)~%" cmd))))))

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
        (dolist (entry (reverse *server-access-list*))
          (format s "~A: ~(~A~)~%" (car entry) (cdr entry))))))

(defun %cmd-server-access (session args)
  "server-access [-l] [-a|-d] [-r|-w] [-k] [user]: manage the server access list.
   -l       list the current access entries (name -> permission); also the
            default when no user and no -a/-d is given.
   -a user  add USER (read-write by default, read-only when -r is also given).
   -d user  remove USER from the access list.
   -r / -w  set the permission to read-only / read-write when adding or modifying.
   -k       kill USER's clients — accepted for compatibility; single-user cl-tmux
            has no remote clients to kill, so it is a no-op.
   A bare `server-access -r user` (no -a/-d) modifies an existing entry; modifying
   an unknown user is an error, matching tmux.  See *server-access-list*."
  (declare (ignore session))
  (multiple-value-bind (flags positionals) (%parse-command-flags args "")
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
         (setf *server-access-list*
               (remove user *server-access-list* :key #'car :test #'string=))
         (show-overlay (format nil "server-access: removed ~A" user))
         t)
        ;; -a user, or bare `user` with -r/-w: add or modify.
        (user
         (let ((entry (assoc user *server-access-list* :test #'string=)))
           (cond
             (entry (when perm (setf (cdr entry) perm)))
             (addp  (push (cons user (or perm :read-write)) *server-access-list*))
             (t (show-overlay (format nil "server-access: unknown user ~A" user))
                (return-from %cmd-server-access nil))))
         (show-overlay
          (format nil "server-access: ~A -> ~(~A~)" user
                  (cdr (assoc user *server-access-list* :test #'string=))))
         t)
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

(defun %format-customize-tree (&optional filter)
  "Render the customize tree as an overlay string: Server Options, then
   Session/Window Options (each `  name: value`, name-sorted), then Key Bindings,
   restricted to entries matching FILTER (substring, case-insensitive).  A group
   with no surviving entries is omitted entirely."
  (with-output-to-string (s)
    (flet ((emit-options (title ht-pairs)
             (let ((shown (sort (remove-if-not
                                 (lambda (p) (%customize-match-p (car p) filter))
                                 ht-pairs)
                                #'string< :key #'car)))
               (when shown
                 (format s "~A:~%" title)
                 (dolist (p shown)
                   (format s "  ~A: ~A~%"
                           (car p) (%customize-value-string (cdr p))))))))
      (let (server-pairs)
        (maphash (lambda (k v) (push (cons k v) server-pairs))
                 cl-tmux/options::*server-options*)
        (emit-options "Server Options" server-pairs))
      (emit-options "Session/Window Options" (cl-tmux/options:all-options)))
    ;; Key bindings: filter the pre-rendered describe-key-bindings block by line.
    (let ((lines (remove-if
                  (lambda (l) (or (string= l "")
                                  (not (%customize-match-p l filter))))
                  (%customize-split-lines (cl-tmux/config:describe-key-bindings)))))
      (when lines
        (format s "Key Bindings:~%")
        (dolist (l lines) (format s "  ~A~%" l))))))

(defun %cmd-customize-mode (session args)
  "customize-mode [-N] [-F format] [-f filter] [-t target-pane]: show the
   customize tree (options + key bindings) in an overlay.  -f FILTER limits the
   tree to entries whose name/line contains FILTER (case-insensitive substring).
   -F format, -N (numeric/no-preview), and -t target-pane are accepted for tmux
   compatibility and otherwise ignored — cl-tmux's tree is read-only (edit with
   set-option / bind-key)."
  (declare (ignore session))
  (multiple-value-bind (flags positionals) (%parse-command-flags args "Fft")
    (declare (ignore positionals))
    (show-overlay (%format-customize-tree (cdr (assoc #\f flags))))
    t))

