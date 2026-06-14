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
       (let ((win (if target-str
                      (%resolve-window-target session target-str)
                      (session-active-window session))))
         (funcall k (if win :window :global) win)))
      (t
       (funcall k :global nil)))))

(defun %scope-unset (name scope target)
  "Remove NAME from the option store identified by SCOPE / TARGET."
  (ecase scope
    (:pane   (remhash name (cl-tmux/model:pane-local-options target)))
    (:window (remhash name (cl-tmux/model:window-local-options target)))
    (:global (remhash name cl-tmux/options:*global-options*))))

(defun %scope-append (name value scope target)
  "Append VALUE to option NAME in the store identified by SCOPE / TARGET.
   Style options (e.g. status-style) join with ',' via append-option-value."
  (flet ((cur (v) (cl-tmux/options:append-option-value name v value)))
    (ecase scope
      (:pane
       (cl-tmux/options:set-option-for-pane
        name (cur (cl-tmux/options:get-option-for-pane name target)) target))
      (:window
       (cl-tmux/options:set-option-for-window
        name (cur (cl-tmux/options:get-option-for-window name target)) target))
      (:global
       (cl-tmux/options:set-option name (cur (cl-tmux/options:get-option name nil)))))))

(defun %scope-set (name value scope target)
  "Store VALUE for option NAME in the store identified by SCOPE / TARGET."
  (ecase scope
    (:pane   (cl-tmux/options:set-option-for-pane name value target))
    (:window (cl-tmux/options:set-option-for-window name value target))
    (:global (cl-tmux/options:set-option name value))))

(defun %cmd-set-option (session args)
  "set / set-option [-aFgopsuw] [-t target] <name> <value...>: set an option.
   Scope: -p pane-local, -w window-local, -g global (default), -s → global.
   Operation: -u unset, -a append, -o only-if-unset, default: set.
   -F expands #{...} in VALUE before storage (one-shot format resolution)."
  (with-command-flags+pos (flags positionals args "t")
    (let* ((name       (first positionals))
           (raw-value  (format nil "~{~A~^ ~}" (rest positionals)))
           (value      (%expand-F-flag flags session raw-value))
           (target-str (cdr (assoc #\t flags))))
      (when name
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
            (cl-tmux/config:%apply-option-side-effects name value)))))))

(defun %cmd-set-window-option (session args)
  "set-window-option / setw: like set-option but defaults to WINDOW scope (tmux
   `setw` is `set -w`).  Prepends -w so a bare `setw mode-keys vi` is window-local;
   an explicit -g still wins (global), since %cmd-set-option's (and windowp (not
   globalp)) gate lets -g override the injected -w."
  (%cmd-set-option session (cons "-w" args)))

(defun %show-options-scope (flags default-scope)
  "Resolve show-options scope flags.  The current option store models session and
   window options through the global table; server options are separate."
  (cond
    ((assoc #\s flags) :server)
    ((eq default-scope :server) :server)
    (t nil)))

(defun %show-option-value-only (name scope)
  "Return only NAME's value for `show-option -v`, or NIL when NAME is unset."
  (let* ((line (cl-tmux/options:show-option name scope))
         (prefix (format nil "~A " name)))
    (when (and (not (search "(not set)" line))
               (>= (length line) (length prefix))
               (string= prefix line :end2 (length prefix)))
      (string-right-trim '(#\Newline #\Return)
                         (subseq line (length prefix))))))

(defun %cmd-show-options* (session args default-scope)
  "show-options/show-option argument form.
   Supports the common scriptable subset: -g/-w/-s scope flags, -t target
   consumption, -q quiet missing options, -v value-only, and an optional option
   NAME positional.  Targets are consumed for tmux-compatible syntax; option
   storage is currently global/server-scoped."
  (declare (ignore session))
  (with-command-flags+pos (flags positionals args "t")
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
             (show-overlay out))))
        (t
         (show-overlay (cl-tmux/options:show-options scope)))))))

(defun %cmd-show-options-arg (session args)
  "show-options / show / show-option with arguments."
  (%cmd-show-options* session args nil))

(defun %cmd-show-window-options-arg (session args)
  "show-window-options / showw with arguments; consumes tmux flags."
  (%cmd-show-options* session args nil))

(defun %cmd-show-session-options-arg (session args)
  "show-session-options / shows with arguments; consumes tmux flags."
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
  (with-command-flags+pos (flags positionals args "t")
    (let* ((target-str (cdr (assoc #\t flags)))
           (win        (if target-str
                           (%resolve-window-target session target-str)
                           (session-active-window session)))
           (name       (format nil "~{~A~^ ~}" positionals)))
      (when (and win (plusp (length name)))
        (rename-window win name)))))

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
  (with-command-flags+pos (flags positionals args "t")
    (let* ((target-str (cdr (assoc #\t flags)))
           (target     (if target-str
                           (or (find-session-by-target *server-sessions* target-str)
                               session)
                           session))
           (name       (format nil "~{~A~^ ~}" positionals)))
      (when (plusp (length name))
        (%rename-session-checked target name)))))

(defun %cmd-select-window (session args)
  "select-window [-t target] [-l] [-n] [-p] [-T]: select a window.
   -t target: window-id, name, or special shorthand (:! last, :+ next, :- prev).
   -l: select the last (previously active) window (same as C-b l).
   -n: select the next window.
   -p: select the previous window.
   -T: toggle — when the target is ALREADY the current window, behave like
       last-window instead (the `bind Tab select-window -T` two-window toggle).
   Delivers ?1004 focus events on the switch."
  (with-command-flags (flags args "t")
    (cond
      ((assoc #\l flags)
       ;; -l: last window
       (let ((prev (session-last-window session)))
         (when prev
           (%with-window-focus-transition (session)
             (session-select-window session prev)))))
      ((assoc #\n flags)
       ;; -n: next window
       (%cmd-cycle-window session #'next-cyclic))
      ((assoc #\p flags)
       ;; -p: previous window
       (%cmd-cycle-window session #'prev-cyclic))
      (t
       ;; -t target or bare target
       (let ((target (cdr (assoc #\t flags))))
         (when target
           (%with-window-focus-transition (session)
             (let ((win (%resolve-window-target session target)))
               (when win
                 ;; -T toggle: already on the target → jump to last window instead.
                 (if (and (assoc #\T flags)
                          (eq win (session-active-window session))
                          (session-last-window session))
                     (session-select-window session (session-last-window session))
                     (session-select-window session win)))))))))
    ;; after-select-window: tmux's per-command hook (run-hooks now fires both the
    ;; add-hook and the .tmux.conf set-hook registries).
    (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-select-window+ session)))

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
  (with-command-flags (flags args "tT")
    (let* ((win    (session-active-window session))
           ;; Resolve -t to a pane-id within the active window; default = active pane.
           (target-pane (%resolve-pane-in-window win (cdr (assoc #\t flags)))))
      (cond
        ((assoc #\L flags) (%select-pane-in-direction session :left))
        ((assoc #\R flags) (%select-pane-in-direction session :right))
        ((assoc #\U flags) (%select-pane-in-direction session :up))
        ((assoc #\D flags) (%select-pane-in-direction session :down))
        ;; -d/-e: disable / enable input to the target pane.
        ((assoc #\d flags) (when target-pane (setf (pane-input-disabled target-pane) t)))
        ((assoc #\e flags) (when target-pane (setf (pane-input-disabled target-pane) nil)))
        ;; -T title: set the target pane's title (and its screen title so
        ;; #{pane_title} reflects it).
        ((assoc #\T flags)
         (let ((title (cdr (assoc #\T flags))))
           (when (and target-pane title)
             (setf (pane-title target-pane) title)
             (let ((screen (pane-screen target-pane)))
               (when screen
                 (cl-tmux/terminal/actions:set-screen-title screen title))))))
        ;; -m: mark the target pane (unmark the others in its window first).
        ((assoc #\m flags)
         (when (and win target-pane)
           (dolist (p (window-panes win)) (setf (pane-marked p) nil))
           (setf (pane-marked target-pane) t)))
        ;; -M: clear the marked pane (unmark all panes in the active window).
        ((assoc #\M flags)
         (when win (dolist (p (window-panes win)) (setf (pane-marked p) nil))))
        ;; -l: select the previously active (last) pane in the active window.
        ((assoc #\l flags)
         (when win
           (let ((last (window-last-active win)))
             (when last (%select-pane-with-focus win last)))))
        ;; Default: select the target pane (no-op when it is already active).
        (t
         (when (and win target-pane (not (eq target-pane (window-active-pane win))))
           (%select-pane-with-focus win target-pane))))
      ;; after-select-pane fires once after the command (run-hooks now fires both
      ;; the add-hook and the .tmux.conf set-hook registries).
      (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-select-pane+ session))))
