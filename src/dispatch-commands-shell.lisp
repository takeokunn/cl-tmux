(in-package #:cl-tmux)

;;; -- Shell execution and pane manipulation commands -------------------------
;;;
;;; send-keys-X copy-mode dispatch, run-shell, if-shell, capture-pane,
;;; resize-pane, join-pane, break-pane, clear-history, rotate-window.

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
  (let* ((pane   (or target-pane (session-active-pane session)))
         (screen (and pane (cl-tmux/model:pane-screen pane))))
    (cond
      ;; jump-forward/backward/to/to-backward: require a char argument (the target).
      ;; `send -X jump-forward a` passes "a" as the first extra-arg.
      ((and extra-args
            (member command-name '("jump-forward" "jump-backward"
                                   "jump-to" "jump-to-backward")
                                :test #'string-equal))
       (let* ((char-arg  (first extra-args))
              (target-ch (and char-arg (plusp (length char-arg)) (char char-arg 0))))
         (when (and screen target-ch)
           (cond
             ((string-equal command-name "jump-forward")   (copy-mode-jump-forward  screen target-ch))
             ((string-equal command-name "jump-backward")  (copy-mode-jump-backward screen target-ch))
             ((string-equal command-name "jump-to")        (copy-mode-jump-to       screen target-ch))
             ((string-equal command-name "jump-to-backward") (copy-mode-jump-to-backward screen target-ch))))
         (and screen target-ch t)))
      ;; goto-line N: requires a numeric line-number argument.
      ;; `send -X goto-line 42` passes "42" as the first extra-arg.
      ((string-equal command-name "goto-line")
       (let* ((n-str (first extra-args))
              (n     (and n-str (plusp (length n-str))
                          (ignore-errors (parse-integer n-str)))))
         (when (and screen (integerp n))
           (copy-mode-goto-line screen n))
         (and screen (integerp n) t)))
      ;; search-forward-text / search-backward-text: scripted search with the text
      ;; passed as extra-args (no interactive prompt).
      ((and extra-args
            (member command-name '("search-forward-text" "search-backward-text")
                                :test #'string-equal))
       (let ((term (first extra-args)))
         (when (and screen term (plusp (length term)))
           (if (string-equal command-name "search-forward-text")
               (copy-mode-search-forward  screen term)
               (copy-mode-search-backward screen term)))
         (and screen term t)))
      ;; copy-pipe / copy-pipe-and-cancel with an explicit pipe-command argument:
      ;; bypass the keyword table (which cannot carry per-invocation args) and call
      ;; the copy-mode function directly with the argument string.
      ((and extra-args
            (member command-name '("copy-pipe" "copy-pipe-and-cancel"
                                   "copy-pipe-end-of-line-and-cancel"
                                   "pipe" "pipe-and-cancel" "pipe-no-clear")
                                :test #'string-equal))
       (when screen
         (cond
           ((string-equal command-name "copy-pipe-end-of-line-and-cancel")
            (copy-mode-copy-pipe-end-of-line screen (first extra-args)))
           ((member command-name '("copy-pipe-and-cancel" "pipe-and-cancel")
                    :test #'string-equal)
            (copy-mode-copy-pipe           screen (first extra-args)))
           (t
            (copy-mode-copy-pipe-no-cancel screen (first extra-args)))))
       (and screen t))
      ;; Standard keyword dispatch.
      (t
       (let ((kw (cdr (assoc command-name *copy-mode-x-commands* :test #'string-equal))))
         (when kw
           (if (and target-pane target-window
                    (not (eq target-pane (session-active-pane session))))
               (let ((prev-win  (cl-tmux/model:session-active session))
                     (prev-pane (cl-tmux/model:window-active target-window)))
                 (unwind-protect
                      (progn
                        (setf (cl-tmux/model:session-active session)       target-window
                              (cl-tmux/model:window-active  target-window) target-pane)
                        (dispatch-command session kw nil))
                   (setf (cl-tmux/model:session-active session)       prev-win
                         (cl-tmux/model:window-active  target-window) prev-pane)))
               (dispatch-command session kw nil))
           t))))))

(defun %cmd-run-shell-arg (session args)
  "run-shell [-bC] command:
   run COMMAND in a shell and show the output.
   -b: run in background (fire-and-forget, no output shown).
   -C executes COMMAND as a tmux command instead of a shell command."
  (with-command-input (flags positionals args ""
                             :allowed-flags '(#\b #\C)
                             :message "run-shell: unsupported argument")
    (let* ((bg-p    (assoc #\b flags))
           (cmd-p   (assoc #\C flags))
           (command (format nil "~{~A~^ ~}" positionals)))
      (when (plusp (length command))
        (cond
          (cmd-p
           (%run-command-line session command))
          (bg-p
           (run-shell command :background t))
          (t
           (let ((output (run-shell command)))
             ;; Show output when non-empty; show "(no output)" when empty
             ;; so users know the command ran successfully.
             (show-overlay (or (and output (plusp (length output)) output)
                               "(run-shell: no output)")))))))))

(defun %cmd-if-shell-arg (session args)
  "if-shell [-bF] [-t target-pane] condition [then-cmd] [else-cmd]: conditional command execution.
   -F: treat condition as a format string (#{var}) instead of a shell command.
   -b, -t: accepted for tmux command-line compatibility.
   Without -F: runs condition as shell; exit 0 = truthy."
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\b #\F #\t)
                             :max-positionals 3
                             :message "if-shell: unsupported argument")
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
                   (result (cl-tmux/format:expand-format cond-str ctx)))
              (if (not (member result '("" "0") :test #'string=))
                  (when then-str (%run-command-line session then-str))
                  (when else-str (%run-command-line session else-str))))
            ;; Plain shell: run condition and check exit code
            (if-shell cond-str
                      (lambda () (when then-str (%run-command-line session then-str)))
                      :else-fn (lambda () (when else-str (%run-command-line session else-str)))))))))

(defun %cmd-capture-pane-arg (session args)
  "capture-pane [-p] [-S start] [-E end] [-b buffer] [-JeN] [-t target]: capture
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
  -t target: target pane by id (for example %2) or session:window.pane."
  (with-command-input (flags positionals args "tSEb"
                             :max-positionals 0
                             :allowed-flags '(#\p #\S #\E #\b #\e #\J #\N #\t)
                             :message "capture-pane: unsupported argument")
    (multiple-value-bind (cur-win cur-pane) (%active-window-pane session)
      (let* ((print-p (assoc #\p flags))
             (include-scrollback (assoc #\S flags))
             (escapes  (assoc #\e flags))      ; -e: keep SGR colour/attr escapes
             (join     (assoc #\J flags))      ; -J: preserve trailing spaces + join wraps
             (preserve (assoc #\N flags))      ; -N: preserve trailing spaces, no join
             (target-str (cdr (assoc #\t flags)))
             (pane (nth-value 1 (%resolve-target-window-pane session target-str cur-win cur-pane)))
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
              (cl-tmux/buffer:add-paste-buffer content (cdr (assoc #\b flags)))))))))

(defun %cmd-resize-pane-arg (session args)
  "resize-pane [-t target] [-L|-R|-U|-D|-Z] [-x width] [-y height] [amount]: resize a pane.
   -t target: target pane by pane-id or 'session:window.pane' (default: active pane).
   -L/-R/-U/-D: resize by AMOUNT (default 5) in the given direction.
   -x N / -y N: resize to an ABSOLUTE width/height of N cells (computed as a delta
   from the pane's current size and applied via the :right/:down border move; both
   may be given together).
   -Z: zoom-toggle the target pane."
  (with-command-flags+pos (flags positionals args "txy")
    (multiple-value-bind (cur-win cur-pane) (%active-window-pane session)
      (let* ((amount-str (first positionals))
             (amount     (or (and amount-str (parse-integer amount-str :junk-allowed t)) 5))
             (x-val      (%parse-flag-int flags #\x))
             (y-val      (%parse-flag-int flags #\y))
             ;; Resolve target pane; fall back to active window for resize operations.
             (target-str (cdr (assoc #\t flags)))
             (win        (multiple-value-bind (target-win target-pane)
                             (%resolve-target-window-pane session target-str cur-win cur-pane)
                           (declare (ignore target-pane))
                           target-win)))
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
          ((assoc #\D flags) (when win (resize-pane win :down  amount))))))))

(defun %cmd-join-pane-arg (session args)
  "join-pane / move-pane [-dhv] [-s src-pane] [-t dst-pane]: move
   SRC-PANE out of its window and into DST-PANE's window as a new split.
   -h splits left/right; -v (the default, as for split-window) splits top/bottom.
   -s source pane (default: the active pane); -t destination pane, whose WINDOW
     receives the split (default: the active window).
   -d keeps the current pane active (no switch to the joined pane).
   No-op when source and destination resolve to the same window (nothing to move).
   This is the scriptable form; the interactive :join-pane / :move-pane keybindings
   (which prompt for a window index) are unchanged."
  (with-command-input (flags positionals args "stl"
                               :allowed-flags '(#\d #\h #\v #\s #\t)
                               :max-positionals 0
                               :message "join-pane: unsupported argument")
    (declare (ignore positionals))
    (multiple-value-bind (cur-win cur-pane) (%active-window-pane session)
      (let* ((src-str  (cdr (assoc #\s flags)))
             (dst-str  (cdr (assoc #\t flags)))
             (dir      (if (assoc #\h flags) :h :v))
             (src-win  (if *server-marked-pane*
                           (pane-window *server-marked-pane*)
                           cur-win))
             (src-pane (or *server-marked-pane*
                           cur-pane))
             (dst-win  cur-win))
        ;; -s: resolve the source pane (and its window).  When the target names a
        ;; window but no pane, take THAT window's active pane (not the current
        ;; window's, which is resolve-target's current-pane default).
        (multiple-value-setq (src-win src-pane)
          (%resolve-target-window-pane session src-str src-win src-pane))
        ;; -t: resolve the destination — only its WINDOW matters (the split host).
        (setf dst-win (multiple-value-bind (target-win target-pane)
                           (%resolve-target-window-pane session dst-str cur-win cur-pane)
                         (declare (ignore target-pane))
                         target-win))
        (when (and src-win src-pane dst-win (not (eq src-win dst-win))
                   (join-pane session src-win src-pane dst-win dir))
          ;; tmux makes the joined pane active unless -d.
          (unless (assoc #\d flags)
            (window-select-pane dst-win src-pane))
          (setf *dirty* t)
          t)))))

(defun %cmd-break-pane-arg (session args)
  "break-pane [-d] [-n window-name] [-s src-pane]:
   move a pane out of its window into a new window of its own.
   -d: don't switch to the new window (stay on the current one).
   -n name: name the new window (default: the shell basename).
   -s src-pane: the pane to break out (default: the active pane).
   No-op when the source window has fewer than two panes.  This is the scriptable
   form; the interactive :break-pane keybinding is unchanged."
  (with-command-input (flags positionals args "nstF"
                               :allowed-flags '(#\d #\n #\s)
                               :max-positionals 0
                               :message "break-pane: unsupported argument")
    (multiple-value-bind (cur-win cur-pane) (%active-window-pane session)
      (let* ((detach   (assoc #\d flags))
             (name     (cdr (assoc #\n flags)))
             (src-str  (cdr (assoc #\s flags)))
             (src-win  cur-win)
             (src-pane cur-pane))
        ;; -s: resolve the source pane (and its window); a window-only target uses
        ;; that window's own active pane (not the current window's).
        (multiple-value-setq (src-win src-pane)
          (%resolve-target-window-pane session src-str cur-win src-pane))
        (let ((new-win (cl-tmux/commands:break-pane
                        session :src-window src-win :pane src-pane
                                :name name :select (not detach))))
          (when new-win
            (setf *dirty* t)
            t))))))

(defun %cmd-clear-history-arg (session args)
  "clear-history [-H] [-t target-pane]: clear a pane's scrollback history.
   -t target-pane: the pane to clear (default: the active pane); a window-only
   target clears that window's active pane.
   -H: accepted (tmux also drops the alternate-screen history); cl-tmux clears the
   pane's scrollback regardless.  This is the scriptable form; the interactive
   :clear-history keybinding (active pane) is unchanged."
  (with-command-flags+pos (flags positionals args "t")
    (declare (ignore positionals))
    (multiple-value-bind (cur-win cur-pane) (%active-window-pane session)
      (let* ((target-str (cdr (assoc #\t flags)))
             (pane       cur-pane))
        (multiple-value-bind (_resolved-window resolved-pane)
            (%resolve-target-window-pane session target-str cur-win pane)
          (declare (ignore _resolved-window))
          (setf pane resolved-pane))
        (when pane
          (cl-tmux/terminal/actions:clear-scrollback (pane-screen pane))
          (setf *dirty* t)
          t)))))

(defun %cmd-rotate-window-arg (session args)
  "rotate-window [-DU] [-t target-window]: rotate the pane order in a window.
   -U (the default) rotates forward (the first pane moves to the end); -D rotates
   backward.  -t target-window: the window to rotate (default: the active window).
   This is the scriptable form; the interactive :rotate-window /
   :rotate-window-reverse bindings are unchanged."
  (with-command-input (flags positionals args "t"
                               :allowed-flags '(#\D #\U #\t)
                               :max-positionals 0
                               :message "rotate-window: unsupported argument")
    (declare (ignore positionals))
    (multiple-value-bind (cur-win cur-pane) (%active-window-pane session)
      (let* ((target-str (cdr (assoc #\t flags)))
             (dir        (if (assoc #\D flags) :down :up))
             (win        (multiple-value-bind (target-win target-pane)
                             (%resolve-target-window-pane session target-str cur-win cur-pane)
                           (declare (ignore target-pane))
                           target-win)))
        (when win
          (window-rotate win dir)
          (setf *dirty* t)
          t)))))
