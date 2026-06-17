(in-package #:cl-tmux)

;;; -- Shell execution and pane manipulation commands -------------------------
;;;
;;; send-keys-X copy-mode dispatch, run-shell, if-shell, capture-pane,
;;; resize-pane, join-pane, break-pane, clear-history, rotate-window.

;;; Flat records keep explicit-arg lookup and coercion separate:
;;;   (command-name kind handler)
(defparameter +send-keys-x-explicit-arg-specs+
  '(("jump-forward"                  :char copy-mode-jump-forward)
    ("jump-backward"                 :char copy-mode-jump-backward)
    ("jump-to"                       :char copy-mode-jump-to)
    ("jump-to-backward"              :char copy-mode-jump-to-backward)
    ("goto-line"                     :line copy-mode-goto-line)
    ("search-forward-text"           :text copy-mode-search-forward)
    ("search-backward-text"          :text copy-mode-search-backward)
    ("copy-pipe"                     :text copy-mode-copy-pipe-no-cancel)
    ("copy-pipe-and-cancel"          :text copy-mode-copy-pipe)
    ("copy-pipe-end-of-line-and-cancel"
     :text copy-mode-copy-pipe-end-of-line)))

(defun %send-keys-x-explicit-arg-spec (command-name)
  "Return the explicit-argument spec for COMMAND-NAME."
  (dolist (spec +send-keys-x-explicit-arg-specs+)
    (destructuring-bind (name kind handler) spec
      (when (string-equal command-name name)
        (return (values kind handler))))))

(defun %send-keys-x-explicit-arg-string (kind extra-args)
  "Return the explicit argument string for KIND from EXTRA-ARGS."
  (ecase kind
    ((:char :line) (first extra-args))
    (:text (format nil "~{~A~^ ~}" extra-args))))

(defun %send-keys-x-coerce-explicit-arg (kind handler screen arg)
  "Apply KIND-specific coercion to ARG and call HANDLER on SCREEN."
  (when (and screen arg (plusp (length arg)))
    (ecase kind
      (:char (funcall handler screen (char arg 0)))
      (:line (let ((line-number (ignore-errors (parse-integer arg))))
               (when line-number
                 (funcall handler screen line-number)
                 t)))
      (:text (funcall handler screen arg) t))))

(defun %dispatch-send-keys-x-explicit-arg (screen command-name extra-args)
  "Dispatch COMMAND-NAME with an explicit positional argument when it has one."
  (multiple-value-bind (kind handler)
      (%send-keys-x-explicit-arg-spec command-name)
    (when handler
      (%send-keys-x-coerce-explicit-arg kind handler screen
                                        (%send-keys-x-explicit-arg-string kind
                                                                         extra-args)))))

(defun %dispatch-send-keys-x-with-temporary-focus (session target-pane target-window thunk)
  "Run THUNK while TARGET-PANE is temporarily focused in TARGET-WINDOW.
   Restores the real session/window focus afterward without delivering focus
   events or updating recency metadata."
  (let ((prev-win  (session-active-window session))
        (prev-pane (and target-window (window-active-pane target-window))))
    (unwind-protect
         (progn
           (setf (session-active session) target-window
                 (window-active target-window) target-pane)
           (funcall thunk))
      (when target-window
        (setf (window-active target-window) prev-pane))
      (setf (session-active session) prev-win))))

(defun %dispatch-send-keys-X (session command-name &optional target-pane target-window extra-args)
  "Dispatch a send-keys -X COMMAND-NAME against TARGET-PANE's copy mode (default:
   the active pane).  Copy-mode -X commands act on the session's ACTIVE screen, so
   when TARGET-PANE is a non-active pane the command runs with a temporary focus
   swap so it operates on the target while leaving the real focus unchanged.
   Returns T when COMMAND-NAME is a recognised copy-mode command.
   EXTRA-ARGS (a list of strings) holds any positional arguments after the command
   name; used by the copy-pipe commands to carry the pipe-command string."
  (let* ((pane   (or target-pane (session-active-pane session)))
         (screen (and pane (cl-tmux/model:pane-screen pane))))
    (cond
      ((and extra-args
            (%dispatch-send-keys-x-explicit-arg screen command-name extra-args))
       t)
      ;; Standard keyword dispatch.
       (t
       (let ((kw (cdr (assoc command-name *copy-mode-x-commands* :test #'string-equal))))
         (when kw
           (if (and target-pane target-window
                    (not (eq target-pane (session-active-pane session))))
               (%dispatch-send-keys-x-with-temporary-focus
                session target-pane target-window
                (lambda ()
                  (dispatch-command session kw nil)))
               (dispatch-command session kw nil))
           t))))))

(defun %cmd-run-shell-arg (session args)
  "run-shell [-bCdt] command:
   run COMMAND in a shell and show the output.
   -b: run in background (fire-and-forget, no output shown).
   -C executes COMMAND as a tmux command instead of a shell command.
   -t and -d are accepted for parity with tmux but ignored."
  (with-command-input (flags positionals args "dt"
                             :allowed-flags '(#\b #\C #\d #\t)
                             :message "run-shell: unsupported argument")
    (let* ((command (format nil "~{~A~^ ~}" positionals)))
      (when (plusp (length command))
        (cond
          ((%run-shell-tmux-command-p flags)
           (%run-command-line session command))
          ((%run-shell-background-p flags)
           (run-shell command :background t))
          (t
           (let ((output (run-shell command)))
             ;; Show output when non-empty; show "(no output)" when empty
             ;; so users know the command ran successfully.
             (show-overlay (or (and output (plusp (length output)) output)
                               "(run-shell: no output)")))))))))

(defun %if-shell-run-branch (session then-str else-str truthy-p)
  "Run the THEN-STR or ELSE-STR command line for IF-SHELL depending on TRUTHY-P."
  (if truthy-p
      (when then-str (%run-command-line session then-str))
      (when else-str (%run-command-line session else-str))))

(defun %if-shell-format-result-truthy-p (result)
  "Treat a formatted IF-SHELL result as truthy when it is neither empty nor 0."
  (not (member result '("" "0") :test #'string=)))

(defun %run-shell-background-p (flags)
  "True when RUN-SHELL was called with the background flag."
  (assoc #\b flags))

(defun %run-shell-tmux-command-p (flags)
  "True when RUN-SHELL should route COMMAND through tmux instead of the shell."
  (assoc #\C flags))

(defun %if-shell-format-p (flags)
  "True when IF-SHELL should expand its condition as a format string."
  (assoc #\F flags))

(defun %cmd-if-shell-format-arg (session target-session target-window target-pane
                                  cond-str then-str else-str)
  "Handle IF-SHELL when -F is present by expanding the condition as a format."
  (let* ((ctx    (cl-tmux/format:format-context-from-session
                  target-session target-window target-pane))
         (result (cl-tmux/format:expand-format cond-str ctx)))
    (%if-shell-run-branch session then-str else-str
                          (%if-shell-format-result-truthy-p result))))

(defun %cmd-if-shell-shell-arg (session cond-str then-str else-str)
  "Handle IF-SHELL without -F by delegating to the shell exit status."
  (if-shell cond-str
            (lambda () (when then-str (%run-command-line session then-str)))
            :else-fn (lambda () (when else-str (%run-command-line session else-str)))))

(defun %cmd-if-shell-arg (session args)
  "if-shell [-bF] [-t target-pane] condition [then-cmd] [else-cmd]: conditional command execution.
   -F: treat condition as a format string (#{var}) instead of a shell command.
   -b, -t: supported flags.
   Without -F: runs condition as shell; exit 0 = truthy."
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\b #\F #\t)
                             :max-positionals 3
                             :message "if-shell: unsupported argument")
    (let* ((format-p (%if-shell-format-p flags))
           (target-str (cdr (assoc #\t flags)))
           (cond-str (first positionals))
           (then-str (second positionals))
           (else-str (third positionals)))
      (when cond-str
        (with-target-context (target-session target-window target-pane session target-str)
          (if format-p
              (%cmd-if-shell-format-arg session target-session target-window target-pane
                                        cond-str then-str else-str)
              (%cmd-if-shell-shell-arg session cond-str then-str else-str)))))))

(defun %resolve-active-target-window-pane (session target-str)
  "Resolve TARGET-STR relative to SESSION's active window and pane."
  (multiple-value-bind (cur-win cur-pane) (%active-window-pane session)
    (%resolve-target-window-pane session target-str cur-win cur-pane)))

(defun %capture-pane-options-from-flags (flags)
  "Decode capture-pane flags into a plist used by the command handler."
  (list :print-p (assoc #\p flags)
        :include-scrollback (assoc #\S flags)
        :escapes (assoc #\e flags)
        :join (assoc #\J flags)
        :preserve (assoc #\N flags)
        :target-str (cdr (assoc #\t flags))
        :buffer-name (cdr (assoc #\b flags))))

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
    (let* ((options (%capture-pane-options-from-flags flags))
           (print-p (getf options :print-p))
           (include-scrollback (getf options :include-scrollback))
           (escapes (getf options :escapes))
           (join (getf options :join))
           (preserve (getf options :preserve))
           (target-str (getf options :target-str))
           (pane (nth-value 1 (%resolve-active-target-window-pane session target-str)))
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
            (cl-tmux/buffer:add-paste-buffer content (getf options :buffer-name)))))))

(defun %resize-pane-to-absolute-dimension (win pane target-size size-fn direction)
  (let ((delta (- target-size (funcall size-fn pane))))
    (unless (zerop delta)
      (resize-pane win direction delta))))

(defparameter +resize-pane-direction-specs+
  '((#\L :left)
    (#\R :right)
    (#\U :up)
    (#\D :down)))

(defun %resize-pane-apply-relative-directions (flags win amount)
  (dolist (spec +resize-pane-direction-specs+)
    (destructuring-bind (flag direction) spec
      (when (assoc flag flags)
        (when win
          (resize-pane win direction amount))))))

(defun %cmd-resize-pane-arg (session args)
  "resize-pane [-t target] [-L|-R|-U|-D|-Z] [-x width] [-y height] [amount]: resize a pane.
   -t target: target pane by pane-id or 'session:window.pane' (default: active pane).
   -L/-R/-U/-D: resize by AMOUNT (default 5) in the given direction.
   -x N / -y N: resize to an ABSOLUTE width/height of N cells (computed as a delta
   from the pane's current size and applied via the :right/:down border move; both
   may be given together).
   -Z: zoom-toggle the target pane."
  (with-command-flags+pos (flags positionals args "txy")
    (let* ((amount-str (first positionals))
           (amount     (or (and amount-str (parse-integer amount-str :junk-allowed t)) 5))
           (x-val      (%parse-flag-int flags #\x))
           (y-val      (%parse-flag-int flags #\y))
           ;; Resolve target pane; fall back to active window for resize operations.
           (target-str (cdr (assoc #\t flags)))
           (win        (nth-value 0 (%resolve-active-target-window-pane session target-str))))
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
                 (%resize-pane-to-absolute-dimension win ap x-val
                                                      #'cl-tmux/model:pane-width
                                                      :right))
               (when y-val
                 (%resize-pane-to-absolute-dimension win ap y-val
                                                      #'cl-tmux/model:pane-height
                                                      :down))))))
        ((some (lambda (spec) (assoc (first spec) flags))
               +resize-pane-direction-specs+)
         (%resize-pane-apply-relative-directions flags win amount))))))

(defun %cmd-resize-pane (session args)
  "Compatibility wrapper for resize-pane command dispatch."
  (%cmd-resize-pane-arg session args))

(defun %cmd-join-pane-arg (session args)
  "join-pane / move-pane [-bdfhv] [-l size] [-s src-pane] [-t dst-pane]: move
   SRC-PANE out of its window and into DST-PANE's window as a new split.
   -h splits left/right; -v (the default, as for split-window) splits top/bottom.
   -b inserts the moved pane before/above the destination pane.
   -f makes the split span the full window dimension along the split axis.
   -l size sets the new pane size (cells or percentage, tmux split-window syntax).
   -s source pane (default: the active pane); -t destination pane, whose WINDOW
     receives the split (default: the active window).
   -d keeps the current pane active (no switch to the joined pane).
   No-op when source and destination resolve to the same window (nothing to move).
   This is the scriptable form; the interactive :join-pane / :move-pane keybindings
    (which prompt for a window index) are unchanged."
  (with-command-input (flags positionals args "stl"
                               :allowed-flags '(#\b #\d #\f #\h #\l #\s #\t #\v)
                               :max-positionals 0
                               :message "join-pane: unsupported argument")
    (declare (ignore positionals))
    (multiple-value-bind (cur-win cur-pane) (%active-window-pane session)
      (let* ((src-str  (cdr (assoc #\s flags)))
            (dst-str  (cdr (assoc #\t flags)))
            (dir      (if (assoc #\h flags) :h :v))
            (before   (assoc #\b flags))
            (full     (assoc #\f flags))
            (size-str (cdr (assoc #\l flags)))
            (size     (and size-str (%parse-split-size size-str)))
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
                   (join-pane session src-win src-pane dst-win dir
                              :before before
                              :full full
                              :size size))
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
    (let* ((target-str (cdr (assoc #\t flags)))
           (pane       (nth-value 1 (%resolve-active-target-window-pane session target-str))))
      (when pane
        (cl-tmux/terminal/actions:clear-scrollback (pane-screen pane))
        (setf *dirty* t)
        t))))

(defun %cmd-rotate-window-arg (session args)
  "rotate-window [-DUZ] [-t target-window]: rotate the pane order in a window.
   -U (the default) rotates forward (the first pane moves to the end); -D rotates
   backward.  -Z keeps a zoomed window zoomed and rotates the saved layout.
   -t target-window: the window to rotate (default: the active window).
   This is the scriptable form; the interactive :rotate-window /
   :rotate-window-reverse bindings are unchanged."
  (with-command-input (flags positionals args "t"
                               :allowed-flags '(#\D #\U #\Z #\t)
                               :max-positionals 0
                               :message "rotate-window: unsupported argument")
    (declare (ignore positionals))
    (let* ((target-str (cdr (assoc #\t flags)))
           (dir        (if (assoc #\D flags) :down :up))
           (win        (nth-value 0 (%resolve-active-target-window-pane session target-str))))
      (when win
        (window-rotate win dir)
        (setf *dirty* t)
        t))))
