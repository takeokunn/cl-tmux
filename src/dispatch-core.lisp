(in-package #:cl-tmux)

;;;; Declarative command dispatch — macros, helpers, and core dispatch logic.
;;;;
;;;; This file contains:
;;;;   - Cyclic navigator macro (next-cyclic, prev-cyclic)
;;;;   - Active-pane/window guard macros (with-active-pane, with-active-window)
;;;;   - Private command helper functions (%cmd-new-window, %cmd-cycle-window, etc.)
;;;;   - Copy-mode key override macro + table
;;;;   - Session/window/menu format helpers
;;;;   - New-session factory
;;;;   - Named-command table macro + table
;;;;   - dispatch-prefix-command entry point
;;;;
;;;; The actual command handler rules live in dispatch-handlers.lisp.

;;; ── Cyclic navigation macro ─────────────────────────────────────────────────
;;;
;;; next-cyclic and prev-cyclic are the same modular-arithmetic pattern with
;;; the step direction as the only difference.  A Prolog-like fact table:
;;;   navigate(next, List, Current) :- idx + 1.
;;;   navigate(prev, List, Current) :- idx - 1.

(defmacro define-cyclic-navigators (&rest specs)
  "Build cyclic list navigator functions from a declarative step table.
   Each SPEC is (name step docstring)."
  `(progn
     ,@(mapcar
        (lambda (spec)
          (destructuring-bind (name step docstring) spec
            `(defun ,name (list current)
               ,docstring
               (let ((idx (or (position current list) 0)))
                 (nth (mod (+ idx ,step) (length list)) list)))))
        specs)))

(define-cyclic-navigators
  (next-cyclic  1  "Element after CURRENT in LIST, wrapping around.")
  (prev-cyclic -1  "Element before CURRENT in LIST, wrapping around."))

;;; ── Active-pane access macro ─────────────────────────────────────────────────
;;;
;;; The pattern (let ((ap (session-active-pane session))) (when ap body))
;;; appears in %active-screen and %forward-octets.
;;; with-active-pane names the intent directly.

(defmacro with-active-pane ((pane-var session) &body body)
  "Bind PANE-VAR to SESSION's active pane and evaluate BODY.
   Returns NIL when no active pane is present (no-op guard)."
  `(let ((,pane-var (session-active-pane ,session)))
     (when ,pane-var ,@body)))

;;; -- Kill-result helper -------------------------------------------------------
;;;
;;; Both :kill-pane and :kill-window check the result and set *running* nil on
;;; :quit.  %handle-kill-result centralises that one-liner.

(defun %handle-kill-result (result)
  "Set *running* nil when RESULT is :quit, then return RESULT."
  (when (eq result :quit) (setf *running* nil))
  result)

;;; -- Active-window guard macro ------------------------------------------------
;;;
;;; Several handlers obtain the active window and do nothing when it is NIL.
;;; with-active-window names that guard directly.

(defmacro with-active-window ((win-var session) &body body)
  "Bind WIN-VAR to SESSION's active window and evaluate BODY only when present."
  `(let ((,win-var (session-active-window ,session)))
     (when ,win-var ,@body)))

;;; -- Swap-active-pane helper --------------------------------------------------
;;;
;;; :swap-pane-forward and :swap-pane-backward share the same shape.

(defun %swap-active-pane (session direction)
  "Swap the active pane of SESSION in DIRECTION (:left or :right)."
  (with-active-window (win session)
    (swap-pane win direction)))

;;; -- Focus event delivery (?1004) -------------------------------------------
;;;
;;; When a pane's application has enabled focus events, switching the active pane
;;; must deliver ESC[O (focus lost) to the pane being left and ESC[I (focus
;;; gained) to the pane being entered.  focus-event-report (terminal layer) owns
;;; the byte sequence; here we perform the PTY write.  Both are guarded by a live
;;; fd, so panes without a PTY (fd <= 0, e.g. in tests) are a harmless no-op.

(defun %notify-pane-focus (pane focused-p)
  "Send PANE's application its focus-tracking report (ESC[I gained / ESC[O lost)
   when it enabled focus events and PANE has a live PTY.  A safe no-op otherwise."
  (when (and pane (> (pane-fd pane) 0))
    (let ((seq (cl-tmux/terminal/actions:focus-event-report
                (pane-screen pane) focused-p)))
      (when seq
        (pty-write (pane-fd pane) (babel:string-to-octets seq :encoding :utf-8))))))

(defun %select-pane-with-focus (win new-pane)
  "Make NEW-PANE the active pane of WIN, delivering focus-out to the previously
   active pane and focus-in to NEW-PANE (for panes that enabled ?1004).  Used by
   every interactive pane-switch path so focus tracking stays transparent."
  (let ((old (window-active-pane win)))
    (window-select-pane win new-pane)
    (unless (eq old new-pane)
      (%notify-pane-focus old nil)
      (%notify-pane-focus new-pane t))))

(defmacro %with-window-focus-transition ((session) &body body)
  "Run BODY (which may change SESSION's active window by any means) and then
   deliver focus-out to the previously active window's pane and focus-in to the
   newly active window's pane.  Captures the active window/pane BEFORE BODY and
   diffs AFTER, so it works for direct session-select-window calls and for
   lookup-based switches (select-window-by-number, find-window) alike.  Returns
   BODY's primary value."
  (let ((sess (gensym "SESSION")) (old-win (gensym "OLD-WIN"))
        (old-pane (gensym "OLD-PANE")) (new-win (gensym "NEW-WIN")))
    `(let* ((,sess     ,session)
            (,old-win  (session-active-window ,sess))
            (,old-pane (and ,old-win (window-active-pane ,old-win))))
       (prog1 (progn ,@body)
         (let ((,new-win (session-active-window ,sess)))
           (unless (eq ,old-win ,new-win)
             (%notify-pane-focus ,old-pane nil)
             (%notify-pane-focus (and ,new-win (window-active-pane ,new-win)) t)))))))

;;; -- Private command helpers ------------------------------------------------

(defun run-command-hooks (event-name session)
  "Dispatch every command registered for hook EVENT-NAME (via the `set-hook`
   directive) on SESSION.  A no-op when no command hooks are set, so calling it
   next to each hook's run-hooks at a fire site is free for the common case.
   NOTE: command hooks that dispatch a pane-forking command (:new-window,
   :split-*) hit the single-thread fork constraint when reader threads are live;
   non-forking commands (select/rename/layout) are the supported case."
  (dolist (keyword (cl-tmux/hooks:command-hooks event-name))
    (dispatch-command session keyword 0)))

;; Install run-command-hooks as the command-hook runner so lower layers
;; (cl-tmux/commands kill-pane / kill-window) can fire command hooks too.
(setf cl-tmux/hooks:*command-hook-runner* #'run-command-hooks)

(defun %cmd-new-window (session)
  "Create a new window in SESSION and start a reader thread for it.
   The window name defaults to the shell basename (e.g. \"bash\"), matching
   real tmux; the id is assigned by session-new-window as the lowest free slot."
  (let* ((rows (- *term-rows* *status-height*))
         (cols *term-cols*)
         (name (%shell-basename))
         (win  (session-new-window session name rows cols)))
    (start-reader-thread (window-active-pane win))
    (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-new-window+ win)
    (run-command-hooks cl-tmux/hooks:+hook-after-new-window+ session)))

(defun %cmd-cycle-window (session cycler)
  "Switch the active window using CYCLER (next-cyclic or prev-cyclic)."
  (let ((w (funcall cycler
                    (session-windows session)
                    (session-active-window session))))
    (when w
      (%with-window-focus-transition (session)
        (session-select-window session w)))))

(defun %cmd-cycle-pane (session cycler)
  "Switch the active pane within the active window using CYCLER."
  (let* ((win   (session-active-window session))
         (panes (window-panes win))
         (next  (funcall cycler panes (window-active-pane win))))
    (when next (%select-pane-with-focus win next))))

(defun %cmd-split (session orient &key no-focus size)
  "Split the active pane of SESSION's active window in tree ORIENT (:h left/right,
   :v top/bottom).  Returns NIL when the pane is too small and no shell is forked.
   NO-FOCUS T skips focus change.  SIZE hints the new pane's extent."
  (let* ((win (session-active-window session))
         (new (window-split win orient :no-focus no-focus :size size)))
    (when new
      (start-reader-thread new)
      (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-split-window+ new)
      (run-command-hooks cl-tmux/hooks:+hook-after-split-window+ session))
    new))

(defun %active-screen (session)
  "Return SESSION's active-pane screen, or NIL when there is no active pane."
  (with-active-pane (ap session)
    (pane-screen ap)))

;;; -- %copy-mode-active-p ---------------------------------------------------
;;;
;;; Internal predicate — not part of the public API.  Tests access it via the
;;; double-colon path.  The % prefix signals that callers outside this file
;;; should treat it as an implementation detail.

(defun %copy-mode-active-p (session)
  "Return T when the active pane's screen is in copy mode."
  (let* ((win (session-active-window session))
         (ap  (and win (window-active-pane win))))
    (and ap
         (screen-copy-mode-p (pane-screen ap)))))

;;; -- Directional pane selection helper ------------------------------------
;;;
;;; The four :select-pane-left/right/up/down handlers share the same shape:
;;; obtain the active window and pane, then walk to the neighbor in DIRECTION.

(defun %select-pane-in-direction (session direction)
  "Select the pane adjacent to the active pane in DIRECTION."
  (let* ((win (session-active-window session))
         (ap  (and win (window-active-pane win))))
    (when (and win ap)
      (let ((nb (pane-neighbor win ap direction)))
        (when nb (%select-pane-with-focus win nb))))))

;;; -- Named-layout application helper --------------------------------------
;;;
;;; The three :select-layout-* handlers share the same shape: apply a named
;;; layout to the active window and recompute geometry.

(defun %apply-named-layout-to-session (session layout-name)
  "Apply LAYOUT-NAME to SESSION's active window and reassign geometry."
  (let ((win (session-active-window session)))
    (when win
      (cl-tmux/model:apply-named-layout win layout-name)
      (layout-assign (window-tree win) 0 0 (window-width win) (window-height win)))))

;;; -- Copy-mode dispatch helper --------------------------------------------
;;;
;;; The copy-mode command handlers share the pattern:
;;; obtain the active screen and invoke a copy-mode function when present.
;;; NOTE: This helper guards only on the presence of an active screen, not
;;; on whether copy mode is currently on.  The caller is responsible for
;;; gating on copy-mode state when required.

(defun %copy-mode-call (session fn)
  "Call FN on SESSION's active screen when one exists.
   Does NOT check whether copy mode is currently active; the caller must
   guard on that separately if needed."
  (let ((screen (%active-screen session)))
    (when screen (funcall fn screen))))

;;; -- Window list formatter ------------------------------------------------

(defun %format-window-list (session)
  "Return a formatted string listing all windows in SESSION.
   Format: INDEX: NAME (WxH) [active marker]
   INDEX is the window's stored id, not its 0-based list position."
  (let* ((win  (session-active-window session))
         (wins (session-windows session)))
    (with-output-to-string (s)
      (dolist (w wins)
        (format s "~A~A: ~A (~Dx~D) [~D pane~:P]~A~%"
                (if (eq w win) "*" " ")
                (window-id w)
                (window-name w)
                (window-width w)
                (window-height w)
                (length (window-panes w))
                (if (eq w win) " [active]" ""))))))

;;; ── Copy-mode key overrides macro ────────────────────────────────────────────

(defmacro define-copy-mode-key-overrides (&rest rules)
  "Build a copy-mode key-lookup function from a declarative override table.
   Each RULE is (char keyword). When in copy mode, CH is checked against the
   override table before the normal key-binding lookup.
   Generates %COPY-MODE-CMD that returns the override or the normal binding."
  `(defun %copy-mode-cmd (ch)
     "Return the command for CH when copy mode is active."
     (cond
       ,@(mapcar (lambda (rule)
                   (destructuring-bind (char kw) rule
                     `((and ch (char= ch ,char)) ,kw)))
                 rules)
       (t (and ch (lookup-key-binding ch))))))

(define-copy-mode-key-overrides
  (#\q :copy-mode-exit)
  (#\i :copy-mode-exit)
  (#\Space :copy-mode-begin-selection)
  (#\v :copy-mode-begin-selection)
  (#\V :copy-mode-begin-line-selection)
  (#\y :copy-mode-yank)
  (#\w :copy-mode-word-forward)
  (#\b :copy-mode-word-backward)
  (#\e :copy-mode-word-end)
  (#\0 :copy-mode-line-start)
  (#\$ :copy-mode-line-end)
  (#\g :copy-mode-top)
  (#\G :copy-mode-bottom)
  (#\H :copy-mode-high)
  (#\M :copy-mode-middle)
  (#\L :copy-mode-low)
  (#\D :copy-mode-copy-end-of-line)
  (#\Y :copy-mode-copy-line)
  (#\n :copy-mode-search-next)
  (#\N :copy-mode-search-prev)
  (#\/ :copy-mode-search-forward-prompt)
  (#\? :copy-mode-search-backward-prompt)
  (#\= :copy-mode-choose-buffer))

;;; -- Session list formatter helper -------------------------------------------
;;;
;;; :list-sessions, :list-sessions-full, :choose-session, and :choose-tree all
;;; produce the same "* N: name (W window[s])" line format.  A single helper
;;; keeps the loop in one place.

(defun %format-session-list (current-session)
  "Return a formatted string listing all sessions in *server-sessions*.
   The session matching CURRENT-SESSION is marked with an asterisk.
   Falls back to a one-line entry when *server-sessions* is empty."
  (with-output-to-string (s)
    (if *server-sessions*
        (loop for (name . sess) in *server-sessions*
              for i from 0
              do (format s "~A~A: ~A (~D window~:P)~%"
                         (if (string= name (session-name current-session)) "*" " ")
                         i name
                         (length (session-windows sess))))
        (format s "  0: ~A (1 window)~%" (session-name current-session)))))

;;; -- Choose-tree entry formatter helper --------------------------------------
;;;
;;; :choose-tree needs to render one session + its windows.  Both the
;;; *server-sessions* branch and the fallback branch share this logic.

(defun %format-tree-entry (stream session-name current-session-name windows active-window)
  "Write one session entry (SESSION-NAME + window list) to STREAM.
   Current session is marked with an asterisk.  ACTIVE-WINDOW marks the active
   window within that session."
  (format stream "~A~A~%"
          (if (string= session-name current-session-name) "* " "  ")
          session-name)
  (dolist (win windows)
    (format stream "    ~A~A: ~A~%"
            (if (eq win active-window) "*" " ")
            (window-id win)
            (window-name win))))

;;; -- Menu formatter helper ---------------------------------------------------

(defun %format-menu (menu)
  "Format a MENU struct into a displayable overlay string."
  (let ((title (menu-title menu))
        (items (menu-items menu))
        (sel   (menu-selected-index menu)))
    (with-output-to-string (s)
      (format s "┌─ ~A ─┐~%" title)
      (loop for (label . _cmd) in items
            for i from 0
            do (format s "~A ~A~%"
                       (if (= i sel) "▶" " ")
                       label))
      (format s "└~A┘"
              (make-string (+ 4 (length title)) :initial-element #\─)))))

;;; -- new-session -------------------------------------------------------------

(defun new-session (name rows cols)
  "Create a new session named NAME with a full-screen window of ROWS x COLS.
   Registers the session in *server-sessions* and starts reader threads."
  (let ((session (create-initial-session rows cols)))
    (setf (session-name session) name)
    (session-touch session)
    (server-add-session session)
    (dolist (pane (all-panes session))
      (start-reader-thread pane))
    (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-session-created+ session)
    (run-command-hooks cl-tmux/hooks:+hook-session-created+ session)
    session))

;;; -- Signal-channel prompt helper --------------------------------------------
;;;
;;; :wait-for and :wait-for-signal had identical bodies; %signal-channel-prompt
;;; factors out the common logic so a single form removes the duplication.

(defun %signal-channel-prompt (prompt-label)
  "Open a prompt labelled PROMPT-LABEL; on submit signal the named channel
   and show a confirmation overlay."
  (prompt-start prompt-label ""
                (lambda (name)
                  (unless (string= name "")
                    (signal-channel name)
                    (show-overlay (format nil "signaled channel: ~A" name))))))

;;; -- Toggle-synchronize-panes helper -----------------------------------------
;;;
;;; The :synchronize-panes handler mutates an option and shows an overlay.
;;; Extracting it as a named function keeps the handler table declarative and
;;; places the option-mutation logic where it belongs (a named function),
;;; separating it from the dispatch-layer rule.

(defun %toggle-synchronize-panes ()
  "Toggle the 'synchronize-panes' option and show a status overlay."
  (let ((current (cl-tmux/options:get-option "synchronize-panes")))
    (cl-tmux/options:set-option "synchronize-panes" (not current))
    (show-overlay (if (not current)
                      "synchronize-panes: ON"
                      "synchronize-panes: OFF"))))

;;; -- Option prompt helper -----------------------------------------------------
;;;
;;; :set-window-option and :set-session-option share the exact same body.
;;; %set-option-from-prompt factors out the common prompt+parse logic.

(defun %set-option-from-prompt (prompt-label)
  "Open a prompt labelled PROMPT-LABEL; on submit parse 'name value' and call set-option."
  (prompt-start prompt-label ""
                (lambda (input)
                  (unless (string= input "")
                    (let* ((parts (uiop:split-string input :separator " "))
                           (name  (first parts))
                           (value (second parts)))
                      (when (and name value)
                        (cl-tmux/options:set-option name value)))))))

;;; -- Paste helper --------------------------------------------------------------
;;;
;;; :paste-buffer and :choose-buffer both need to write text to the active pane's
;;; PTY, honouring bracketed-paste mode.  %paste-to-pane factors that out.

(defun %paste-to-pane (pane text)
  "Write TEXT to PANE's PTY, wrapping in bracketed-paste sequences when enabled."
  (when (and text pane (> (pane-fd pane) 0))
    (let* ((screen    (pane-screen pane))
           (bracketed (screen-bracketed-paste screen))
           (prefix    (when bracketed (format nil "~C[200~~" #\Escape)))
           (suffix    (when bracketed (format nil "~C[201~~" #\Escape))))
      (when prefix
        (pty-write (pane-fd pane) (babel:string-to-octets prefix :encoding :utf-8)))
      (pty-write (pane-fd pane) (babel:string-to-octets text :encoding :utf-8))
      (when suffix
        (pty-write (pane-fd pane) (babel:string-to-octets suffix :encoding :utf-8))))))

;;; -- Named-command table macro -----------------------------------------------
;;;
;;; Maps string command names (as typed in the command-prompt) to dispatch
;;; keywords.  The table is expressed as Prolog-like facts so new entries can
;;; be added by appending a single line rather than editing a cond chain.

(defmacro define-named-command-table (&rest entries)
  "Build %DISPATCH-NAMED-COMMAND from a declarative string→keyword table.
   Each ENTRY is (\"command-name\" keyword).  The generated function maps
   CMD-NAME to a keyword and executes it, or shows an unknown-command overlay."
  `(defun %dispatch-named-command (session cmd-name)
     "Map CMD-NAME (a string) to a dispatch keyword and execute it on SESSION.
      Shows an error overlay for unknown command names."
     (let ((kw (cond
                 ,@(mapcar (lambda (entry)
                             (destructuring-bind (name kw) entry
                               `((string-equal cmd-name ,name) ,kw)))
                           entries)
                 (t nil))))
       (if kw
           (dispatch-command session kw nil)
           (show-overlay (format nil "unknown command: ~A" cmd-name))))))

(define-named-command-table
  ("new-window"    :new-window)
  ("new-session"   :new-session)
  ("kill-pane"     :kill-pane)
  ("kill-window"   :kill-window)
  ("kill-session"  :kill-session)
  ("detach"        :detach)
  ("detach-client" :detach)
  ("next-window"   :next-window)
  ("prev-window"   :prev-window)
  ("split-window"  :split-horizontal)
  ("rename-window" :rename-window)
  ("rename-session":rename-session)
  ("list-windows"  :list-windows)
  ("list-sessions" :list-sessions)
  ("list-keys"     :list-keys)
  ("copy-mode"     :copy-mode-enter)
  ("paste-buffer"  :paste-buffer)
  ("list-buffers"  :list-buffers)
  ("show-buffer"   :show-buffer)
  ("choose-buffer" :choose-buffer)
  ("delete-buffer" :delete-buffer)
  ("save-buffer"   :save-buffer)
  ("load-buffer"   :load-buffer)
  ("zoom-toggle"   :zoom-toggle)
  ("choose-tree"   :choose-tree)
  ("choose-session":choose-session)
  ("choose-window" :choose-window)
  ("display-panes" :display-panes)
  ("show-messages" :show-messages)
  ("show-hooks"    :show-hooks)
  ("capture-pane"  :capture-pane)
  ("clear-history" :clear-history)
  ("respawn-pane"  :respawn-pane)
  ("send-keys"     :send-keys)
  ("clock-mode"    :clock-mode)
  ("source-file"   :source-file)
  ("run-shell"     :run-shell)
  ("if-shell"      :if-shell)
  ("show-options"  :show-options)
  ("show-option"   :show-option)
  ("display-info"  :display-info)
  ("mark-pane"     :mark-pane)
  ("clear-mark"    :clear-mark)
  ("next-layout"   :next-layout)
  ("bind-key"       :bind-key)
  ("unbind-key"     :unbind-key)
  ("choose-client"  :choose-client)
  ("move-window"    :move-window-prompt)
  ("refresh-client" :refresh-client)
  ;; Commands that had key bindings + handlers but were not reachable by name
  ;; from the C-b : prompt until now (no-argument forms).
  ("break-pane"      :break-pane)
  ("swap-pane"       :swap-pane-forward)
  ("last-pane"       :last-pane)
  ("last-window"     :last-window)
  ("find-window"     :find-window)
  ("previous-window" :prev-window)
  ("command-prompt"  :command-prompt)
  ("rotate-window"   :rotate-window)
  ;; No-arg forms open a prompt; the arg form (rename-window <name>) is handled
  ;; by %run-command-line before reaching this table.
  ("rename-window"   :rename-window)
  ("rename-session"  :rename-session)
  ;; No-arg kill acts on the active window/pane; the -t arg form is intercepted
  ;; by %run-command-line first.
  ("kill-window"     :kill-window)
  ("kill-pane"       :kill-pane))

;;; -- Arg-aware command-line runner -------------------------------------------
;;;
;;; The C-b : prompt may name a command WITH arguments (e.g.
;;; "display-message #{session_name}").  %run-command-line tokenises the line
;;; (shared shell-style lexer), routes arg-taking commands to their handlers, and
;;; falls through to the no-argument name table for everything else.

(defun %cmd-display-message (session args)
  "display-message <fmt...>: expand the space-joined ARGS as a format string
   against the active session/window/pane, then log and show the result.
   This is what makes #{session_name} etc. resolve instead of printing literally."
  (let* ((win  (session-active-window session))
         (pane (session-active-pane session))
         (ctx  (cl-tmux/format:format-context-from-session session win pane))
         (text (cl-tmux/format:expand-format
                (format nil "~{~A~^ ~}" args) ctx)))
    (add-message-log text)
    (show-overlay text)))

(defparameter *set-option-command-names*
  '("set" "set-option" "setw" "set-window-option" "sets" "set-session-option")
  "Command names that all forward to the global option store, mirroring the
   set-option family of config directives.")

(defun %cmd-set-option (session args)
  "set / set-option [-g|-s|-w|-o] [-a] [-u] <name> <value...>: set a global option.
   Scope flags (-g global, -s server, -w window, -o only-if-unset) are accepted
   and treated as the global store (cl-tmux keeps a flat option table); -a
   appends VALUE to the option's current value; -u unsets the option (removes
   the override, reverting to the registered default).
   SESSION is unused.  NOTE: this fixes `set -g status off`, which previously set
   an option literally named \"-g\"."
  (declare (ignore session))
  (multiple-value-bind (flags positionals) (%parse-command-flags args "")
    (let ((name  (first positionals))
          (value (format nil "~{~A~^ ~}" (rest positionals))))
      (when name
        (cond
          ;; -u: unset option — remove from hash table so the default is used.
          ((assoc #\u flags)
           (remhash name cl-tmux/options:*global-options*))
          ;; -a: append value to existing.
          ((assoc #\a flags)
           (cl-tmux/options:set-option
            name (concatenate 'string
                              (princ-to-string
                               (or (cl-tmux/options:get-option name nil) ""))
                              value)))
          ;; normal set.
          (t (cl-tmux/options:set-option name value)))))))

(defun %cmd-rename-window (session args)
  "rename-window <name...>: rename SESSION's active window to the joined ARGS."
  (let ((win (session-active-window session)))
    (when win (rename-window win (format nil "~{~A~^ ~}" args)))))

(defun %cmd-rename-session (session args)
  "rename-session <name...>: rename SESSION to the joined ARGS."
  (rename-session session (format nil "~{~A~^ ~}" args)))

;;; -- Flag parser (-t target, boolean flags) ----------------------------------
;;;
;;; Many tmux commands take a -t target plus boolean flags (-d, -p, ...).  This
;;; splits a token list into (alist-of-flags . positionals).  Flags whose char is
;;; in VALUE-FLAGS consume the next token (or an attached -Xvalue) as their value;
;;; the rest are boolean (T).  Used by select-window/-pane and any future -t cmd.

(defun %parse-command-flags (tokens &optional (value-flags ""))
  "Split TOKENS into (values FLAGS POSITIONALS).  A -X token is a flag; when X is
   in VALUE-FLAGS it consumes the next token (or the attached -Xvalue) as its
   value, otherwise it is boolean (T).  FLAGS is an alist of (flag-char . value)
   (look up with ASSOC, which uses EQL on the character); POSITIONALS is the
   remaining non-flag tokens in order."
  (let ((flags nil) (positionals nil) (rest tokens))
    (loop while rest do
      (let ((tok (pop rest)))
        (cond
          ((and (>= (length tok) 2)
                (char= (char tok 0) #\-)
                (char/= (char tok 1) #\-))
           (let ((fc (char tok 1)))
             (if (find fc value-flags)
                 (push (cons fc (if (> (length tok) 2)
                                    (subseq tok 2)
                                    (if rest (pop rest) "")))
                       flags)
                 (push (cons fc t) flags))))
          (t (push tok positionals)))))
    (values (nreverse flags) (nreverse positionals))))

(defun %resolve-window-target (session target-str)
  "Resolve TARGET-STR to a window in SESSION: by window-id when TARGET-STR is
   numeric, otherwise by window-name.  Returns NIL when nothing matches."
  (let ((n (parse-integer target-str :junk-allowed t)))
    (if n
        (find n (session-windows session) :key #'window-id)
        (find target-str (session-windows session)
              :key #'window-name :test #'string-equal))))

(defun %cmd-select-window (session args)
  "select-window -t <target>: select the window whose number (window-id) or name
   is the -t value.  Delivers ?1004 focus events on the switch."
  (let ((target (cdr (assoc #\t (%parse-command-flags args "t")))))
    (when target
      (%with-window-focus-transition (session)
        (let ((win (%resolve-window-target session target)))
          (when win (session-select-window session win)))))))

(defun %cmd-select-pane (session args)
  "select-pane -t <target>: select the pane with pane-id <target> in the active
   window, delivering focus events."
  (let* ((target (cdr (assoc #\t (%parse-command-flags args "t"))))
         (n      (and target (parse-integer target :junk-allowed t)))
         (win    (session-active-window session)))
    (when (and n win)
      (let ((pane (find n (window-panes win) :key #'pane-id)))
        (when pane (%select-pane-with-focus win pane))))))

(defun %cmd-kill-window (session args)
  "kill-window [-t target]: kill the window named by -t (window-id or name), or
   the active window when no -t is given.  Quits when the last window is killed."
  (let* ((target-str (cdr (assoc #\t (%parse-command-flags args "t"))))
         (win (if target-str
                  (%resolve-window-target session target-str)
                  (session-active-window session))))
    (when win
      (%handle-kill-result (kill-window session win)))))

(defun %cmd-kill-pane (session args)
  "kill-pane [-t target]: kill the pane with pane-id -t in the active window, or
   the active pane when no -t is given.  A -t target that matches nothing is a
   no-op (the active pane is NOT killed by accident)."
  (let* ((target-str (cdr (assoc #\t (%parse-command-flags args "t"))))
         (n    (and target-str (parse-integer target-str :junk-allowed t)))
         (win  (session-active-window session))
         (pane (and n win (find n (window-panes win) :key #'pane-id))))
    (when (or pane (null target-str))
      (%handle-kill-result (kill-pane session pane)))))

(defun %cmd-swap-window (session args)
  "swap-window [-s src] -t dst: exchange two windows in the session's list.  SRC
   and DST are window-id/name targets; with no -s the active window is the source.
   First command to use two value flags (-s and -t) at once."
  (multiple-value-bind (flags positionals) (%parse-command-flags args "st")
    (declare (ignore positionals))
    (let* ((src-str (cdr (assoc #\s flags)))
           (dst-str (cdr (assoc #\t flags)))
           (src     (if src-str
                        (%resolve-window-target session src-str)
                        (session-active-window session)))
           (dst     (and dst-str (%resolve-window-target session dst-str)))
           (wins    (session-windows session)))
      (when (and src dst (not (eq src dst)))
        (session-swap-windows session (position src wins) (position dst wins))))))

(defun %cmd-source-file (session args)
  "source-file <path>: load the tmux config file at <path>.  Enables the
   canonical reload binding (bind r source-file ~/.tmux.conf).  SESSION unused."
  (declare (ignore session))
  (let ((path (first args)))
    (when path
      ;; A missing file or parse error must not crash the session (tmux shows an
      ;; error but keeps running).
      (ignore-errors (cl-tmux/config:load-config-file path)))))

(defun %cmd-move-window (session args)
  "move-window -t <n>: renumber the active window to window-id <n>.  A no-op when
   <n> is already taken by a DIFFERENT window (tmux would error; we keep running)."
  (let* ((target (cdr (assoc #\t (%parse-command-flags args "t"))))
         (n      (and target (parse-integer target :junk-allowed t)))
         (win    (session-active-window session)))
    (when (and n win
               (let ((holder (find n (session-windows session) :key #'window-id)))
                 (or (null holder) (eq holder win))))
      (setf (window-id win) n))))

(defun %cmd-if-shell (session args)
  "if-shell -F <cond> <then> [<else>]: when the format CONDITION expands to a
   truthy value (non-empty and not \"0\"), run the THEN command line; otherwise
   run ELSE if given.  Only the -F (format, no shell fork) form is handled; a
   plain shell-condition if-shell is a no-op here (would require a fork)."
  (multiple-value-bind (flags positionals) (%parse-command-flags args "")
    (when (assoc #\F flags)
      (let ((cond-str (first  positionals))
            (then     (second positionals))
            (else     (third  positionals)))
        (when cond-str
          (let* ((win  (session-active-window session))
                 (pane (session-active-pane session))
                 (ctx  (cl-tmux/format:format-context-from-session session win pane))
                 (val  (cl-tmux/format:expand-format cond-str ctx)))
            (if (and (plusp (length val)) (not (string= val "0")))
                (when then (%run-command-line session then))
                (when else (%run-command-line session else)))))))))

(defun %cmd-select-layout (session args)
  "select-layout <name>: apply the named layout to the active window.
   Accepted names: even-horizontal (even-h), even-vertical (even-v),
   main-horizontal (main-h), main-vertical (main-v), tiled."
  (let* ((name (first args))
         (kw   (and name
                    (cond
                      ((member name '("even-horizontal" "even-h")
                               :test #'string-equal) :even-horizontal)
                      ((member name '("even-vertical" "even-v")
                               :test #'string-equal) :even-vertical)
                      ((member name '("main-horizontal" "main-h")
                               :test #'string-equal) :main-horizontal)
                      ((member name '("main-vertical" "main-v")
                               :test #'string-equal) :main-vertical)
                      ((string-equal name "tiled") :tiled)
                      (t nil)))))
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

(defun %cmd-new-window-arg (session args)
  "new-window [-n name]: create a new window, optionally with a given name."
  (multiple-value-bind (flags positionals) (%parse-command-flags args "n")
    (declare (ignore positionals))
    (let ((name (cdr (assoc #\n flags))))
      (%cmd-new-window session)
      (when name
        (let ((win (session-active-window session)))
          (when win (rename-window win name)))))))

(defparameter *arg-command-table*
  (list
   (cons '("display-message" "display") #'%cmd-display-message)
   (cons *set-option-command-names*     #'%cmd-set-option)
   (cons '("rename-window")             #'%cmd-rename-window)
   (cons '("rename-session")            #'%cmd-rename-session)
   (cons '("select-window" "selectw")   #'%cmd-select-window)
   (cons '("select-pane")               #'%cmd-select-pane)
   (cons '("kill-window" "killw")       #'%cmd-kill-window)
   (cons '("kill-pane")                 #'%cmd-kill-pane)
   (cons '("swap-window" "swapw")       #'%cmd-swap-window)
   (cons '("move-window" "movew")       #'%cmd-move-window)
   (cons '("if-shell" "if")             #'%cmd-if-shell)
   (cons '("source-file" "source")      #'%cmd-source-file)
   (cons '("select-layout" "selectl")   #'%cmd-select-layout)
   (cons '("list-panes" "lsp")          #'%cmd-list-panes)
   (cons '("new-window" "neww")         #'%cmd-new-window-arg))
  "Arg-taking commands: (list-of-names . handler), handler a function of
   (SESSION ARGS).  Consulted by %run-command-line before the no-argument
   %dispatch-named-command name table.")

(defun %run-command-tokens (session tokens)
  "Run a command line given as an already-tokenised TOKENS list (first = command
   name, rest = arguments).  Arg-taking commands (found in *arg-command-table*)
   consume their arguments; everything else dispatches by name via
   %dispatch-named-command (no args).  Taking pre-split tokens lets arg-bearing
   key bindings store and run their command without a lossy re-tokenisation."
  (let ((cmd  (first tokens))
        (rest (rest tokens)))
    (cond
      ((null cmd) nil)
      (t (let ((entry (and rest
                           (find-if (lambda (e)
                                      (member cmd (car e) :test #'string-equal))
                                    *arg-command-table*))))
           (if entry
               (funcall (cdr entry) session rest)
               (%dispatch-named-command session cmd)))))))

(defun %run-command-line (session input)
  "Tokenise INPUT (one command line, shell-style) and run it via
   %run-command-tokens."
  (%run-command-tokens session (cl-tmux/commands:tokenize-command-string input)))

;;; -- dispatch-prefix-command -----------------------------------------------

(defun dispatch-prefix-command (session byte)
  "Handle one byte received after the prefix key.
   Copy mode intercepts [ ] q before the normal binding table.  A binding whose
   value is a token LIST (from `bind key command args...`) runs as a command
   line; a keyword value dispatches as a built-in command."
  (let* ((ch  (and byte (code-char byte)))
         (cmd (if (%copy-mode-active-p session)
                  (%copy-mode-cmd ch)
                  (and ch (lookup-key-binding ch)))))
    (if (consp cmd)
        (%run-command-tokens session cmd)
        (dispatch-command session cmd byte))))
