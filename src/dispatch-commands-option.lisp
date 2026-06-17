(in-package #:cl-tmux)

;;; -- set-option scope helpers + rename/select %cmd-* handlers ----------------
;;;
;;; %with-option-scope (CPS), %cmd-set-option, %cmd-set-window-option,
;;; %cmd-rename-window, %cmd-rename-session, %cmd-select-window, %cmd-select-pane.

;;; ── set-option scope helpers (CPS + data-logic separation) ─────────────────
;;;
;;; %cmd-set-option decomposes into three concerns:
;;;   1. Value expansion (-F flag) — data transformation before storage.
;;;   2. Scope resolution (-g/-w/-p/-t) — which store to use.
;;;   3. Operation dispatch (-u unset / -a append / -o guard / normal set).
;;;
;;; %with-option-scope resolves the scope ONCE and passes (scope target) to a
;;; continuation K.  The three %scope-* functions are pure scope→effect transforms
;;; with ecase — exhaustive, so the compiler warns on any missing scope kind.

(defun %expand-F-flag (flags session raw-value)
  "Expand RAW-VALUE as a format string when FLAGS contains -F; else return as-is."
  (if (assoc #\F flags)
      (cl-tmux/format:expand-format
       raw-value
       (cl-tmux/format:format-context-from-session
        session (session-active-window session) (session-active-pane session)))
      raw-value))

(defun %with-option-scope (session flags target-str k)
  "Resolve the option scope from FLAGS / TARGET-STR, then call K with (scope target).
   SCOPE is :pane, :window, or :global; TARGET is the resolved pane/window (NIL for
   :global).  Falls back to :global when -p/-w resolves to a NIL target."
  (let ((globalp (and (assoc #\g flags) t)))
    (cond
      ((and (assoc #\p flags) (not globalp))
       (let ((pane (if target-str
                       (%resolve-pane-in-window (session-active-window session) target-str)
                       (session-active-pane session))))
         (funcall k (if pane :pane :global) pane)))
      ((and (assoc #\w flags) (not globalp))
       (let ((win (%resolve-window-target-or-active session target-str)))
         (funcall k (if win :window :global) win)))
      (t
       (funcall k :global nil)))))

(defun %scope-option-accessors (scope target)
  "Return getter, setter, and remover closures for SCOPE / TARGET."
  (ecase scope
    (:pane
     (values (lambda (name &optional default)
               (declare (ignore default))
               (cl-tmux/options:get-option-for-pane name target))
             (lambda (name value)
               (cl-tmux/options:set-option-for-pane name value target))
             (lambda (name)
               (remhash name (cl-tmux/model:pane-local-options target)))))
    (:window
     (values (lambda (name &optional default)
               (declare (ignore default))
               (cl-tmux/options:get-option-for-window name target))
             (lambda (name value)
               (cl-tmux/options:set-option-for-window name value target))
             (lambda (name)
               (remhash name (cl-tmux/model:window-local-options target)))))
    (:global
     (values (lambda (name &optional default)
               (cl-tmux/options:get-option name default))
             (lambda (name value)
               (cl-tmux/options:set-option name value))
             (lambda (name)
               (remhash name cl-tmux/options:*global-options*))))))

(defun %scope-append (name value scope target)
  "Append VALUE to option NAME in the store identified by SCOPE / TARGET.
   Style options (e.g. status-style) join with ',' via append-option-value."
  (multiple-value-bind (getter setter remover)
      (%scope-option-accessors scope target)
    (declare (ignore remover))
    (funcall setter
             name
             (cl-tmux/options:append-option-value name (funcall getter name nil) value))))

(defun %scope-set (name value scope target)
  "Store VALUE for option NAME in the store identified by SCOPE / TARGET."
  (multiple-value-bind (getter setter remover)
      (%scope-option-accessors scope target)
    (declare (ignore getter remover))
    (funcall setter name value)))

(defun %scope-unset (name scope target)
  "Remove NAME from the option store identified by SCOPE / TARGET."
  (multiple-value-bind (getter setter remover)
      (%scope-option-accessors scope target)
    (declare (ignore getter setter))
    (funcall remover name)))

(defun %cmd-set-option (session args)
  "set-option [-aFgopsuw] [-t target] <name> <value...>: set an option.
   Scope: -p pane-local, -w window-local, -g global (default), -s → global.
   Operation: -u unset, -a append, -o only-if-unset, default: set.
   -F expands #{...} in VALUE before storage (one-shot format resolution)."
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\a #\F #\g #\o #\p #\s #\u #\w #\t)
                             :message "set-option: unsupported argument")
    (let* ((name       (first positionals))
           (raw-value  (format nil "~{~A~^ ~}" (rest positionals)))
           (target-str (cdr (assoc #\t flags))))
      (cond
        ((null name) nil)
        ((cl-tmux/config::%unsupported-set-option-p name)
         (%overlayf "set-option: unsupported option ~A" name)
         nil)
        (t
         (let ((value (%expand-F-flag flags session raw-value)))
           (%with-option-scope session flags target-str
             (lambda (scope target)
               (cond
                 ((assoc #\u flags)
                  (%scope-unset name scope target))
                 ((assoc #\a flags)
                  (%scope-append name value scope target))
                 ((and (assoc #\o flags)
                       (nth-value 1 (gethash name cl-tmux/options:*global-options*)))
                  nil)
                 (t
                  (%scope-set name value scope target)))
               ;; Side-effects for special options (prefix/status/escape-time etc.)
               ;; always run after the operation, even when -o skips the write.
               ;; Passes RAW value — side-effect parsers expect strings, not coerced types.
               (cl-tmux/config:%apply-option-side-effects name value (assoc #\u flags))))))))))

(defun %cmd-set-window-option (session args)
  "set-window-option: like set-option but defaults to WINDOW scope.  Prepends
   -w so a bare `set-window-option mode-keys vi` is window-local; an explicit
   -g still wins (global), since %cmd-set-option's (and windowp (not globalp))
   gate lets -g override the injected -w."
  (%cmd-set-option session (cons "-w" args)))

(defun %show-options-scope (flags default-scope)
  "Resolve show-options scope flags.  The current option store models session and
   window options through the global table; server options are separate."
  (cond
    ((assoc #\s flags) :server)
    ((eq default-scope :server) :server)
    (t nil)))

(defun %show-option-value-only (name scope)
  "Return only NAME's value for `show-options -v`, or NIL when NAME is unset."
  (let* ((line (cl-tmux/options:show-option name scope))
         (prefix (format nil "~A " name)))
    (when (and (not (search "(not set)" line))
               (>= (length line) (length prefix))
               (string= prefix line :end2 (length prefix)))
      (string-right-trim '(#\Newline #\Return)
                         (subseq line (length prefix))))))

(defun %show-options-with-hooks (text flags)
  "Append command-hook listings to TEXT when FLAGS contains -H.
   tmux's -H flag includes hooks in show-options output; this implementation
   reuses the existing command-hook formatter so the display path stays aligned."
  (if (assoc #\H flags)
      (if (plusp (length text))
          (format nil "~A~%~A" text (cl-tmux/hooks:describe-command-hooks))
          (cl-tmux/hooks:describe-command-hooks))
      text))

(defun %cmd-show-options* (session args default-scope)
  "show-options argument form.
     Supports the common scriptable subset: -g/-w/-s scope flags, -t target
     consumption, -q quiet missing options, -v value-only, -A inherited options,
     -H hooks, and an optional option NAME positional.  Targets are consumed by
     the scriptable tmux syntax; option storage is currently global/server-scoped."
  (declare (ignore session))
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\A #\H #\g #\w #\s #\t #\q #\v)
                             :max-positionals 1
                             :message "show-options: unsupported argument")
    (let* ((scope (%show-options-scope flags default-scope))
           (name (first positionals))
           (quietp (assoc #\q flags))
           (value-only-p (assoc #\v flags)))
      (cond
        ((and name value-only-p)
         (let ((value (%show-option-value-only name scope)))
           (when (or value (not quietp))
             (show-overlay (or value "")))))
        (name
         (let ((out (cl-tmux/options:show-option name scope)))
           (unless (and quietp (search "(not set)" out))
             (show-overlay (%show-options-with-hooks out flags)))))
        (t
         (show-overlay (%show-options-with-hooks
                        (cl-tmux/options:show-options scope)
                        flags)))))))

(defun %cmd-show-options-arg (session args)
  "show-options with arguments."
  (%cmd-show-options* session args nil))

(defun %cmd-show-window-options-arg (session args)
  "show-window-options with arguments; consumes tmux flags."
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\A #\H #\g #\w #\s #\t #\q #\v)
                             :max-positionals 1
                             :message "show-options: unsupported argument")
    (let* ((name (first positionals))
           (quietp (assoc #\q flags))
           (value-only-p (assoc #\v flags))
           (target-str (cdr (assoc #\t flags)))
           (win (%resolve-window-target-or-active session target-str)))
      (cond
        ((and name value-only-p)
         (let ((value (cl-tmux/options:show-window-option
                       name win :value-only-p t)))
           (when (or value (not quietp))
             (show-overlay (or value "")))))
        (name
         (let ((out (cl-tmux/options:show-window-option
                     name win :inherited-p (assoc #\A flags))))
           (unless (and quietp (search "(not set)" out))
             (show-overlay (%show-options-with-hooks out flags)))))
        (t
         (show-overlay (%show-options-with-hooks
                        (cl-tmux/options:show-window-options
                         win :inherited-p (assoc #\A flags))
                        flags)))))))

(defun %cmd-show-session-options-arg (session args)
  "show-session-options with arguments; consumes tmux flags."
  (%cmd-show-options* session args nil))

(defun %cmd-show-server-options-arg (session args)
  "show-server-options with arguments; defaults to the server option store."
  (%cmd-show-options* session args :server))

;;; -- -e VAR=val environment flag parser ----------------------------------------
;;;
;;; new-window and split-window accept repeated -e VAR=val flags to set
;;; environment variables in the new pane.  This helper collects them from
;;; an already-parsed flags alist (produced by %parse-command-flags with "e"
;;; in value-flags) into an alist suitable for %fork-pane's :extra-env.

(defun %collect-env-flags (flags-alist)
  "Extract all (-e . \"VAR=val\") entries from FLAGS-ALIST and return an alist
   of (\"VAR\" . \"val\") pairs.  Entries without \"=\" are included as (\"NAME\" . \"\").
   Multiple -e flags are supported; all are collected."
  (loop for (char . value) in flags-alist
        when (and (char= char #\e) (stringp value))
        collect (let ((eq-pos (position #\= value)))
                  (if eq-pos
                      (cons (subseq value 0 eq-pos)
                            (subseq value (1+ eq-pos)))
                      (cons value "")))))

(defun %cmd-rename-window (session args)
  "rename-window [-t target-window] <name...>: rename the target window (default:
   the active window) to the joined remaining ARGS.  Without -t parsing, a bare
   `rename-window -t @2 foo` would fold the flag tokens into the name and rename
   the wrong (active) window."
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\t)
                             :message "rename-window: unsupported argument")
    (let* ((target-str (cdr (assoc #\t flags)))
           (win        (%resolve-window-target-or-active session target-str))
           (name       (format nil "~{~A~^ ~}" positionals)))
      (when win
        (if (plusp (length name))
            (rename-window win name)
            (prompt-start "rename-window" (window-name win)
                          (lambda (new-name)
                            (rename-window win new-name))))))))

(defun %rename-session-checked (session new-name)
  "Rename SESSION to NEW-NAME, keeping *server-sessions* keyed by the new name and
   firing +hook-session-renamed+.  REFUSES (returns NIL) when NEW-NAME is empty or
   already used by a DIFFERENT session — tmux rejects a rename onto an existing name
   (`duplicate session`) rather than silently orphaning the other session; renaming
   to the session's CURRENT name is a harmless no-op that still succeeds.  The single
   chokepoint both rename paths (arg command + interactive prompt) route through.
   Returns T on success."
  (when (and new-name (plusp (length new-name)))
    (let ((existing (server-find-session new-name)))
      (unless (and existing (not (eq existing session)))   ; a different session owns it
        (server-remove-session (session-name session))
        (rename-session session new-name)
        (server-add-session session)
        (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-session-renamed+ session)
        t))))

(defun %cmd-rename-session (session args)
  "rename-session [-t target-session] <name...>: rename the target session (default:
   the current one) to the joined remaining ARGS, updating the registry key.
   Refuses a name already used by another session (see %rename-session-checked).
   Without -t parsing, `rename-session -t old new` would fold the flag tokens into
   the name and rename the wrong (current) session."
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\t)
                             :message "rename-session: unsupported argument")
    (let ((target-str (cdr (assoc #\t flags)))
          (name       (format nil "~{~A~^ ~}" positionals)))
      (with-target-session (target-session target-str session
                                :on-missing :current)
        (if (plusp (length name))
            (%rename-session-checked target-session name)
            (prompt-start "rename-session" (session-name target-session)
                          (lambda (new-name)
                            (%rename-session-checked target-session new-name))))))))

(defun %cmd-select-window (session args)
  "select-window [-t target] [-l] [-n] [-p] [-T]: select a window.
   -t target: window-id, name, or special shorthand (:! last, :+ next, :- prev).
   -l: select the last (previously active) window (same as C-b l).
   -n: select the next window.
   -p: select the previous window.
   -T: toggle — when the target is ALREADY the current window, behave like
       last-window instead (the `bind Tab select-window -T` two-window toggle).
   Delivers ?1004 focus events on the switch."
  (with-command-input (flags positionals args "t"
                             :allowed-flags '(#\l #\n #\p #\T #\t)
                             :max-positionals 0
                             :message "select-window: unsupported argument")
    (cond
      ((assoc #\l flags)
       (%select-window-select-last session))
      ((assoc #\n flags)
       (%select-window-cycle-next session))
      ((assoc #\p flags)
       (%select-window-cycle-prev session))
      (t
       (%select-window-select-target session
                                     (cdr (assoc #\t flags))
                                     (assoc #\T flags))))
    ;; after-select-window: tmux's per-command hook (run-hooks now fires both the
    ;; add-hook and the .tmux.conf set-hook registries).
    (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-select-window+ session)))

(defun %select-window-select-last (session)
  (let ((prev (session-last-window session)))
    (when prev
      (%with-window-focus-transition (session)
        (session-select-window session prev)))))

(defun %select-window-cycle-next (session)
  (%cmd-cycle-window session #'next-cyclic))

(defun %select-window-cycle-prev (session)
  (%cmd-cycle-window session #'prev-cyclic))

(defun %select-window-select-target (session target toggle-p)
  (when target
    (%with-window-focus-transition (session)
      (let ((win (%resolve-window-target session target)))
        (when win
          ;; -T toggle: already on the target → jump to last window instead.
          (if (and toggle-p
                   (eq win (session-active-window session))
                   (session-last-window session))
              (session-select-window session (session-last-window session))
              (session-select-window session win)))))))

(defun %select-pane-disable-input (pane disabled-p)
  (when pane
    (setf (pane-input-disabled pane) disabled-p)))

(defun %select-pane-set-title (pane title)
  (when (and pane title)
    (setf (pane-title pane) title)
    (let ((screen (pane-screen pane)))
      (when screen
        (cl-tmux/terminal/actions:set-screen-title screen title)))))

(defun %select-pane-mark (window pane)
  (when (and window pane)
    (dolist (p (window-panes window))
      (setf (pane-marked p) nil))
    (setf (pane-marked pane) t)))

(defun %select-pane-clear-mark (window)
  (when window
    (dolist (p (window-panes window))
      (setf (pane-marked p) nil))))

(defun %select-pane-select-last (window)
  (when window
    (let ((last (window-last-active window)))
      (when last
        (%select-pane-with-focus window last)))))

(defun %select-pane-select-target (window pane)
  (when (and window pane (not (eq pane (window-active-pane window))))
    (%select-pane-with-focus window pane)))

(defun %select-pane-move-in-direction (session flags)
  (cond
    ((assoc #\L flags) (%select-pane-in-direction session :left))
    ((assoc #\R flags) (%select-pane-in-direction session :right))
    ((assoc #\U flags) (%select-pane-in-direction session :up))
    ((assoc #\D flags) (%select-pane-in-direction session :down))))

(defun %select-pane-configure-target (window target-pane flags)
  (cond
    ;; -d/-e: disable / enable input to the target pane.
    ((assoc #\d flags) (%select-pane-disable-input target-pane t))
    ((assoc #\e flags) (%select-pane-disable-input target-pane nil))
    ;; -T title: set the target pane's title (and its screen title so
    ;; #{pane_title} reflects it).
    ((assoc #\T flags)
     (%select-pane-set-title target-pane (cdr (assoc #\T flags))))
    ;; -m: mark the target pane (unmark the others in its window first).
    ((assoc #\m flags) (%select-pane-mark window target-pane))
    ;; -M: clear the marked pane (unmark all panes in the active window).
    ((assoc #\M flags) (%select-pane-clear-mark window))
    ;; -l: select the previously active (last) pane in the active window.
    ((assoc #\l flags) (%select-pane-select-last window))
    ;; Default: select the target pane (no-op when it is already active).
    (t (%select-pane-select-target window target-pane))))

(defun %cmd-select-pane (session args)
  "select-pane [-L|-R|-U|-D|-l|-d|-e|-m|-M] [-t target] [-T title]: select or configure a pane.
   -L/-R/-U/-D: move in the given direction (relative to the active pane).
   -l: select the previously active (last) pane.
   -d/-e: disable / re-enable keyboard input to the TARGET pane.
   -T title: set the TARGET pane's title.
   -m: mark the TARGET pane; -M: clear the marked pane (unmark all).
   -t target: pane-id within the active window (default: the active pane).  The
     pane-configuring forms (-d/-e/-T/-m) and plain selection all act on -t's pane,
     not unconditionally the active one."
  (with-command-input (flags positionals args "tT"
                             :allowed-flags '(#\L #\R #\U #\D #\l #\d #\e #\m #\M #\t #\T)
                             :max-positionals 0
                             :message "select-pane: unsupported argument")
    (let* ((win    (session-active-window session))
           ;; Resolve -t to a pane-id within the active window; default = active pane.
           (target-pane (%resolve-pane-in-window win (cdr (assoc #\t flags)))))
      (cond
        ((or (assoc #\L flags)
             (assoc #\R flags)
             (assoc #\U flags)
             (assoc #\D flags))
         (%select-pane-move-in-direction session flags))
        (t
         (%select-pane-configure-target win target-pane flags)))
      ;; after-select-pane fires once after the command (run-hooks now fires both
      ;; the add-hook and the .tmux.conf set-hook registries).
      (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-select-pane+ session))))
