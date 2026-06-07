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

(defun %cmd-new-window (session &key name start-dir detach at-index after-current)
  "Create a new window in SESSION and start a reader thread for it.
   NAME: window name (defaults to shell basename).
   START-DIR: start directory for the new pane's shell.
   DETACH: when T, do not make the new window active.
   AT-INDEX: when an integer, try to assign that specific window id.
   AFTER-CURRENT: when T, insert after the current window's id.
   Returns the new window."
  (let* ((rows (- *term-rows* *status-height*))
         (cols *term-cols*)
         (win-name (or name (cl-tmux/model::%shell-basename)))
         (prev-win (session-active-window session))
         ;; Determine base-index for id assignment.
         (base (cond
                 ((and at-index (integerp at-index)) at-index)
                 ((and after-current prev-win)
                  (1+ (window-id prev-win)))
                 (t (or (cl-tmux/options:get-option "base-index") 0))))
         (win  (session-new-window session win-name rows cols base start-dir)))
    (start-reader-thread (window-active-pane win))
    (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-new-window+ win)
    (run-command-hooks cl-tmux/hooks:+hook-after-new-window+ session)
    (when (and detach prev-win)
      (session-select-window session prev-win))
    win))

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

(defun %cmd-split (session orient &key no-focus size start-dir)
  "Split the active pane of SESSION's active window in tree ORIENT (:h left/right,
   :v top/bottom).  Returns NIL when the pane is too small and no shell is forked.
   NO-FOCUS T skips focus change.  SIZE hints the new pane's extent.
   START-DIR: when non-NIL, the new pane's shell starts in that directory."
  (let* ((win (session-active-window session))
         (new (window-split win orient :no-focus no-focus :size size
                                       :start-dir start-dir)))
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

(defun new-session (name rows cols &key start-dir)
  "Create a new session named NAME with a full-screen window of ROWS x COLS.
   START-DIR: when non-NIL, the initial shell starts in that directory.
   Registers the session in *server-sessions* and starts reader threads."
  (let ((session (create-initial-session rows cols :start-dir start-dir)))
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
  ("show-options"         :show-options)
  ("show-option"          :show-option)
  ("show-window-options"  :show-window-options)
  ("showw"                :show-window-options)
  ("show-session-options" :show-session-options)
  ("shows"                :show-session-options)
  ("show-server-options"  :show-server-options)
  ("display-info"         :display-info)
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
  ("break-pane"        :break-pane)
  ("join-pane"         :join-pane)
  ("swap-pane"         :swap-pane-forward)
  ("last-pane"         :last-pane)
  ("last-window"       :last-window)
  ("find-window"       :find-window)
  ("previous-window"   :prev-window)
  ("command-prompt"    :command-prompt)
  ("rotate-window"     :rotate-window)
  ("synchronize-panes" :synchronize-panes)
  ("lock-session"      :lock-session)
  ("unlock-session"    :unlock-session)
  ("has-session"       :has-session)
  ("wait-for"          :wait-for)
  ("pipe-pane"         :pipe-pane)
  ("display-popup"     :display-popup)
  ;; Server management
  ("server-info"       :server-info)
  ("list-clients"      :list-clients)
  ("lsc"               :list-clients)
  ("suspend-client"    :suspend-client)
  ("suspendc"          :suspend-client)
  ("lock-server"       :lock-server)
  ;; Window management (additional)
  ("resize-window"     :resize-window)
  ("resizew"           :resize-window)
  ("respawn-window"    :respawn-window)
  ("attach-session"    :attach-session)
  ("attach"            :attach-session)
  ("move-pane"         :move-pane)
  ;; Environment
  ("show-environment"  :show-environment)
  ("showenv"           :show-environment)
  ("set-environment"   :set-environment)
  ("setenv"            :set-environment)
  ;; Prompt history
  ("show-prompt-history"  :show-prompt-history)
  ("clear-prompt-history" :clear-prompt-history)
  ;; Detach all clients (no-arg form; the interactive :detach handler covers
  ;; the common single-client case; this name dispatches :detach-all-clients).
  ("detach-all-clients"   :detach-all-clients))

;;; -- Arg-aware command-line runner -------------------------------------------
;;;
;;; The C-b : prompt may name a command WITH arguments (e.g.
;;; "display-message #{session_name}").  %run-command-line tokenises the line
;;; (shared shell-style lexer), routes arg-taking commands to their handlers, and
;;; falls through to the no-argument name table for everything else.

(defun %cmd-display-message (session args)
  "display-message <fmt...>: expand the space-joined ARGS as a format string
   against the active session/window/pane, then log and show the result.
   Uses show-transient-overlay so display-time ms auto-dismisses it."
  (let* ((win  (session-active-window session))
         (pane (session-active-pane session))
         (ctx  (cl-tmux/format:format-context-from-session session win pane))
         (text (cl-tmux/format:expand-format
                (format nil "~{~A~^ ~}" args) ctx)))
    (add-message-log text)
    (show-transient-overlay text)))

;;; *set-option-command-names* removed — inlined into *arg-command-table* below.

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
  "rename-session <name...>: rename SESSION to the joined ARGS.
   Updates *server-sessions* so the registry stays consistent with the
   new name (same invariant enforced by the interactive :rename-session handler)."
  (let ((new-name (format nil "~{~A~^ ~}" args)))
    (unless (string= new-name "")
      (server-remove-session (session-name session))
      (rename-session session new-name)
      (server-add-session session))))

;;; -- Flag parser (-t target, boolean flags) ----------------------------------
;;;
;;; Many tmux commands take a -t target plus boolean flags (-d, -p, ...).  This
;;; splits a token list into (alist-of-flags . positionals).  Flags whose char is
;;; in VALUE-FLAGS consume the next token (or an attached -Xvalue) as their value;
;;; the rest are boolean (T).  Used by select-window/-pane and any future -t cmd.

(defun %parse-flag-token (token value-flags remaining-tokens)
  "Parse one flag TOKEN whose first char is #\\-.
   Returns (values flag-entry new-remaining) where FLAG-ENTRY is (char . value)
   and NEW-REMAINING is the residual token list after consuming a value argument
   when the flag char is in VALUE-FLAGS.
   TOKEN must have length >= 2 and token[0] = #\\-."
  (let ((flag-char (char token 1)))
    (if (find flag-char value-flags)
        (let ((attached (when (> (length token) 2) (subseq token 2))))
          (if attached
              (values (cons flag-char attached) remaining-tokens)
              (values (cons flag-char (if remaining-tokens
                                          (first remaining-tokens)
                                          ""))
                      (if remaining-tokens (rest remaining-tokens) nil))))
        (values (cons flag-char t) remaining-tokens))))

(defun %parse-command-flags (tokens &optional (value-flags ""))
  "Split TOKENS into (values FLAGS POSITIONALS).  A -X token is a flag; when X is
   in VALUE-FLAGS it consumes the next token (or the attached -Xvalue) as its
   value, otherwise it is boolean (T).  FLAGS is an alist of (flag-char . value)
   (look up with ASSOC, which uses EQL on the character); POSITIONALS is the
   remaining non-flag tokens in order."
  (loop with flags = nil and positionals = nil and rest = tokens
        while rest
        for token = (first rest)
        do (setf rest (rest rest))
           (if (and (>= (length token) 2)
                    (char= (char token 0) #\-)
                    (char/= (char token 1) #\-))
               (multiple-value-bind (entry new-rest)
                   (%parse-flag-token token value-flags rest)
                 (push entry flags)
                 (setf rest new-rest))
               (push token positionals))
        finally (return (values (nreverse flags) (nreverse positionals)))))

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
  "select-pane [-L|-R|-U|-D|-m] [-t target]: select a pane by direction or id.
   -L/-R/-U/-D: move in the given direction (same as prefix C-b arrow keys).
   -t target: select pane by pane-id in the active window.
   -m: mark the selected pane."
  (multiple-value-bind (flags _positionals) (%parse-command-flags args "t")
    (declare (ignore _positionals))
    (cond
      ((assoc #\L flags) (%select-pane-in-direction session :left))
      ((assoc #\R flags) (%select-pane-in-direction session :right))
      ((assoc #\U flags) (%select-pane-in-direction session :up))
      ((assoc #\D flags) (%select-pane-in-direction session :down))
      ((assoc #\m flags)
       ;; -m: mark the active pane
       (with-active-pane (ap session)
         (with-active-window (win session)
           (dolist (p (window-panes win)) (setf (pane-marked p) nil)))
         (setf (pane-marked ap) t)))
      (t
       ;; Default: select by pane-id via -t
       (let* ((target (cdr (assoc #\t flags)))
              (n      (and target (parse-integer target :junk-allowed t)))
              (win    (session-active-window session)))
         (when (and n win)
           (let ((pane (find n (window-panes win) :key #'pane-id)))
             (when pane (%select-pane-with-focus win pane)))))))))

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

(defun %cmd-new-window-arg (session args)
  "new-window [-d] [-n name] [-t target-window] [-a] [-c start-dir]: create a new window.
   -d: create the window but do not make it active (detached).
   -n name: name the new window.
   -t idx: insert at specific index (assigned as the window id).
   -a: insert after the current window.
   -c dir: start directory for the new pane's shell (format strings expanded)."
  (multiple-value-bind (flags positionals) (%parse-command-flags args "ntc")
    (declare (ignore positionals))
    (let* ((name       (cdr (assoc #\n flags)))
           (detach-p   (assoc #\d flags))
           (after-p    (assoc #\a flags))
           (target-str (cdr (assoc #\t flags)))
           (raw-dir    (cdr (assoc #\c flags)))
           ;; Expand #{...} format variables in the -c argument.
           (start-dir  (when raw-dir
                         (let* ((win  (session-active-window session))
                                (pane (and win (window-active-pane win)))
                                (ctx  (cl-tmux/format:format-context-from-session
                                       session win pane)))
                           (cl-tmux/format:expand-format raw-dir ctx))))
           (at-idx     (and target-str (parse-integer target-str :junk-allowed t))))
      (%cmd-new-window session
                       :name name
                       :start-dir start-dir
                       :detach (and detach-p t)
                       :at-index at-idx
                       :after-current (and after-p t)))))

(defun %cmd-split-window (session args)
  "split-window [-h|-v] [-d] [-p percent] [-l size] [-c start-dir]: split the active pane.
   -h: horizontal split (new pane to the right; side-by-side).
   -v: vertical split (new pane below — default).
   -d: split but do not change focus (detached mode).
   -p N: size as a percentage of the parent pane (0-100).
   -l N: size in lines/columns (absolute integer).
   -c dir: start directory for the new pane's shell (format strings expanded)."
  (multiple-value-bind (flags positionals) (%parse-command-flags args "plc")
    (declare (ignore positionals))
    (let* ((horizontal-p (assoc #\h flags))
           (detach-p     (assoc #\d flags))
           (pct-str      (cdr (assoc #\p flags)))
           (lines-str    (cdr (assoc #\l flags)))
           (raw-dir      (cdr (assoc #\c flags)))
           ;; Expand #{...} format variables in the -c argument so that
           ;; 'split-window -c "#{pane_current_path}"' opens in the right dir.
           (start-dir    (when raw-dir
                           (let* ((win  (session-active-window session))
                                  (pane (and win (window-active-pane win)))
                                  (ctx  (cl-tmux/format:format-context-from-session
                                         session win pane)))
                             (cl-tmux/format:expand-format raw-dir ctx))))
           (pct          (and pct-str (parse-integer pct-str :junk-allowed t)))
           (lines        (and lines-str (parse-integer lines-str :junk-allowed t)))
           (size         (or (and pct (/ pct 100.0)) lines)))
      (if horizontal-p
          (%cmd-split session :h :size size :no-focus (and detach-p t)
                              :start-dir start-dir)
          (%cmd-split session :v :size size :no-focus (and detach-p t)
                              :start-dir start-dir)))))

(defun %cmd-new-session-arg (session args)
  "new-session [-A] [-s name] [-n window-name] [-c start-dir] [-d]: create a new session.
   -A: if a session named NAME already exists, attach to it instead of creating a new one.
   -s name: session name.
   -n name: initial window name.
   -c dir: start directory for the initial window's shell.
   -d: do not switch to the new session (stay in current session)."
  (declare (ignore session))
  (multiple-value-bind (flags positionals) (%parse-command-flags args "snc")
    (declare (ignore positionals))
    (let* ((name      (or (cdr (assoc #\s flags))
                          (format nil "~D" (1+ (length *server-sessions*)))))
           (attach-if-exists (assoc #\A flags))
           (win-name  (cdr (assoc #\n flags)))
           (start-dir (cdr (assoc #\c flags)))
           (rows      (- *term-rows* *status-height*))
           (cols      *term-cols*))
      ;; -A: attach to existing session if it exists
      (when attach-if-exists
        (let ((existing (server-find-session name)))
          (when existing
            (session-touch existing)
            (setf *dirty* t)
            (return-from %cmd-new-session-arg existing))))
      ;; No existing session: create a new one
      (let ((new-sess (new-session name rows cols :start-dir start-dir)))
        ;; Apply window name if given
        (when (and win-name new-sess)
          (let ((win (session-active-window new-sess)))
            (when win (rename-window win win-name))))
        new-sess))))

(defun %cmd-kill-session-arg (session args)
  "kill-session [-a] [-t name]: kill session(s).
   -a: kill all sessions EXCEPT the one named by -t (or current session).
   -t name: the target session (default: current session)."
  (multiple-value-bind (flags positionals) (%parse-command-flags args "t")
    (declare (ignore positionals))
    (let* ((kill-all-others (assoc #\a flags))
           (target-name     (cdr (assoc #\t flags)))
           (keep-sess       (if target-name
                                (cdr (assoc target-name *server-sessions*
                                            :test #'equal))
                                session)))
      (if kill-all-others
          ;; -a: kill all sessions except keep-sess
          (let ((to-kill (loop for (name . sess) in *server-sessions*
                               unless (eq sess keep-sess)
                               collect (cons name sess))))
            (dolist (entry to-kill)
              (let ((name (car entry))
                    (sess (cdr entry)))
                (dolist (pane (all-panes sess))
                  (ignore-errors (pty-close (pane-fd pane) (pane-pid pane))))
                (server-remove-session name))))
          ;; No -a: kill the target session
          (let ((target-sess (or (and target-name
                                      (cdr (assoc target-name *server-sessions*
                                                  :test #'equal)))
                                 session)))
            (when target-sess
              (let ((name (session-name target-sess)))
                (dolist (pane (all-panes target-sess))
                  (ignore-errors (pty-close (pane-fd pane) (pane-pid pane))))
                (server-remove-session name)
                (when (and (eq target-sess session) (null *server-sessions*))
                  (setf *running* nil))))))))))

(defun %cmd-resize-window-arg (session args)
  "resize-window [-x cols] [-y rows] [-t target-window]: resize a window.
   Sets the window to exactly COLS × ROWS; without flags prompts interactively."
  (multiple-value-bind (flags positionals) (%parse-command-flags args "xyt")
    (declare (ignore positionals))
    (let* ((cols-str (cdr (assoc #\x flags)))
           (rows-str (cdr (assoc #\y flags)))
           (cols     (and cols-str (parse-integer cols-str :junk-allowed t)))
           (rows     (and rows-str (parse-integer rows-str :junk-allowed t)))
           (win      (session-active-window session)))
      (when (and win cols rows (> cols 0) (> rows 0))
        (window-relayout win rows cols)))))

(defun %cmd-detach-client-arg (session args)
  "detach-client [-a] [-t target-session]: detach from a session.
   In standalone mode, both the -a (all clients) form and the no-flag form
   stop the event loop.  SESSION and ARGS are not used."
  (declare (ignore session args))
  (setf *running* nil))

;;; ── Copy-mode command name table (for send-keys -X) ─────────────────────────
;;;
;;; Real tmux's `send-keys -X <name>` dispatches a named copy-mode action.
;;; This table maps name strings to dispatch command keywords so that
;;; `bind -T copy-mode-vi v send-keys -X begin-selection` works.

(defparameter *copy-mode-x-commands*
  '(("begin-selection"           . :copy-mode-begin-selection)
    ("begin-selection-line"      . :copy-mode-begin-line-selection)
    ("begin-line-selection"      . :copy-mode-begin-line-selection)
    ("copy-selection"            . :copy-mode-yank)
    ("copy-selection-and-cancel" . :copy-mode-yank)
    ("cancel"                    . :copy-mode-exit)
    ("cursor-up"                 . :copy-mode-scroll-up-line)
    ("cursor-down"               . :copy-mode-scroll-down-line)
    ("cursor-left"               . :copy-mode-begin-selection)   ; approximate
    ("cursor-right"              . :copy-mode-begin-selection)   ; approximate
    ("page-up"                   . :copy-mode-page-up)
    ("page-down"                 . :copy-mode-page-down)
    ("halfpage-up"               . :copy-mode-half-page-up)
    ("halfpage-down"             . :copy-mode-half-page-down)
    ("search-again"              . :copy-mode-search-next)
    ("search-reverse"            . :copy-mode-search-prev)
    ("search-forward"            . :copy-mode-search-forward-prompt)
    ("search-backward"           . :copy-mode-search-backward-prompt)
    ("top-line"                  . :copy-mode-top)
    ("bottom-line"               . :copy-mode-bottom)
    ("history-top"               . :copy-mode-top)
    ("history-bottom"            . :copy-mode-bottom)
    ("next-word"                 . :copy-mode-word-forward)
    ("previous-word"             . :copy-mode-word-backward)
    ("next-word-end"             . :copy-mode-word-end)
    ("rectangle-toggle"          . :copy-mode-begin-selection)  ; approximate
    ("copy-end-of-line"          . :copy-mode-copy-end-of-line)
    ("copy-line"                 . :copy-mode-copy-line)
    ("append-selection"          . :copy-mode-yank)
    ("back-to-indentation"       . :copy-mode-line-start)
    ("start-of-line"             . :copy-mode-line-start)
    ("end-of-line"               . :copy-mode-line-end)
    ;; scroll variants
    ("scroll-up"                 . :copy-mode-scroll-up-line)
    ("scroll-down"               . :copy-mode-scroll-down-line)
    ("scroll-up-half-page"       . :copy-mode-half-page-up)
    ("scroll-down-half-page"     . :copy-mode-half-page-down)
    ;; emacs-style names
    ("select-word"               . :copy-mode-begin-selection)
    ("copy-pipe"                 . :copy-mode-yank)
    ("copy-pipe-and-cancel"      . :copy-mode-yank)
    ;; mouse-wheel support
    ("scroll-mouse"              . :copy-mode-scroll-up-line)
    ;; vi-style movement
    ("previous-paragraph"        . :copy-mode-page-up)
    ("next-paragraph"            . :copy-mode-page-down)
    ("jump-to-mark"              . :copy-mode-line-start)
    ("toggle-position"           . :copy-mode-begin-selection)
    ;; pipe operations
    ("pipe"                      . :copy-mode-yank)
    ("pipe-and-cancel"           . :copy-mode-yank))
  "Alist mapping send-keys -X command names to copy-mode dispatch keywords.")

(defun %dispatch-send-keys-X (session command-name)
  "Dispatch a send-keys -X COMMAND-NAME against the active pane's copy mode.
   Returns T when handled, NIL otherwise."
  (let ((kw (cdr (assoc command-name *copy-mode-x-commands* :test #'string-equal))))
    (when kw
      (dispatch-command session kw nil)
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
              (when (plusp (length (or output "")))
                (show-overlay output))))))))

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
  "capture-pane [-p] [-S start-line] [-E end-line] [-t target]: capture pane content.
   -p: print to stdout (shows overlay in standalone mode).
   -S N: start from scrollback line N (negative = N lines above visible).
   -t: target pane (not fully supported in standalone mode, uses active pane).
   Without -p: shows in overlay."
  (multiple-value-bind (flags _positionals) (%parse-command-flags args "tSE")
    (declare (ignore _positionals))
    (let* ((print-p (assoc #\p flags))
           (include-scrollback (assoc #\S flags))
           (pane (session-active-pane session))
           (content (and pane (capture-pane pane :include-scrollback (and include-scrollback t)))))
      (when content
        (if print-p
            (show-overlay content)
            (show-overlay content))))))

(defun %cmd-resize-pane-arg (session args)
  "resize-pane [-L|-R|-U|-D|-Z] [amount]: resize the active pane.
   -L/-R/-U/-D: resize by AMOUNT (default 5) in the given direction.
   -Z: zoom-toggle the active pane."
  (multiple-value-bind (flags positionals) (%parse-command-flags args "")
    (let* ((amount-str (first positionals))
           (amount     (or (and amount-str (parse-integer amount-str :junk-allowed t)) 5))
           (win        (session-active-window session)))
      (cond
        ((assoc #\Z flags) (with-active-window (w session) (window-zoom-toggle w)))
        ((assoc #\L flags) (resize-pane win :left  amount))
        ((assoc #\R flags) (resize-pane win :right amount))
        ((assoc #\U flags) (resize-pane win :up    amount))
        ((assoc #\D flags) (resize-pane win :down  amount))))))

(defun %cmd-send-keys-arg (session args)
  "send-keys [-t target-pane] [-X copy-mode-cmd] [key ...]: send keys or copy-mode commands.
   -X: dispatch a named copy-mode command (begin-selection, copy-selection, etc.)
   Without -X: each positional is a key name or literal string typed into the active pane.
   No -t targeting in standalone mode."
  (multiple-value-bind (flags positionals) (%parse-command-flags args "t")
    (declare (ignore flags))
    ;; Check for -X flag: it consumes the next token as a copy-mode command name.
    (let ((x-pos (position "-X" args :test #'string=)))
      (if (and x-pos (nth (1+ x-pos) args))
          ;; -X command: dispatch to copy-mode
          (%dispatch-send-keys-X session (nth (1+ x-pos) args))
          ;; Normal: send key strings to the active pane
          (when positionals
            (with-active-pane (ap session)
              (dolist (key positionals)
                (send-keys-to-pane ap key))))))))

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

(defun %cmd-pipe-pane-arg (session args)
  "pipe-pane [-o] [command]: open or close pipe-pane for the active pane.
   -o: only open a pipe if no current pipe is open (no-op when pipe already open).
   Without command: close any open pipe.
   With command: open a pipe to COMMAND."
  (multiple-value-bind (flags positionals) (%parse-command-flags args "")
    (let* ((only-open (assoc #\o flags))
           (command   (format nil "~{~A~^ ~}" positionals)))
      (with-active-pane (ap session)
        (cond
          ;; No command: close existing pipe
          ((zerop (length command))
           (when (pane-pipe-fd ap) (pipe-pane-close ap)))
          ;; -o: skip if already piped
          ((and only-open (pane-pipe-fd ap)) nil)
          ;; Open the pipe
          (t (pipe-pane-open ap command))))))

(defun %cmd-set-environment-prompt (session args)
  "set-environment [-r] NAME [VALUE]: set or unset a process environment variable.
   -r: unset the variable.  Without -r, VALUE is required."
  (declare (ignore session))
  (multiple-value-bind (flags positionals) (%parse-command-flags args "")
    (let* ((remove-p (assoc #\r flags))
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

(defparameter *arg-command-table*
  (list
   (cons '("display-message" "display") #'%cmd-display-message)
   (cons '("set" "set-option" "setw" "set-window-option" "sets" "set-session-option")
         #'%cmd-set-option)
   (cons '("rename-window")             #'%cmd-rename-window)
   (cons '("rename-session")            #'%cmd-rename-session)
   (cons '("select-window" "selectw")   #'%cmd-select-window)
   (cons '("select-pane")               #'%cmd-select-pane)
   (cons '("kill-window" "killw")       #'%cmd-kill-window)
   (cons '("kill-pane")                 #'%cmd-kill-pane)
   (cons '("kill-session")              #'%cmd-kill-session-arg)
   (cons '("swap-window" "swapw")       #'%cmd-swap-window)
   (cons '("move-window" "movew")       #'%cmd-move-window)
   (cons '("if-shell" "if")             #'%cmd-if-shell)
   (cons '("source-file" "source")      #'%cmd-source-file)
   (cons '("select-layout" "selectl")   #'%cmd-select-layout)
   (cons '("list-panes" "lsp")          #'%cmd-list-panes)
   (cons '("new-window" "neww")         #'%cmd-new-window-arg)
   (cons '("split-window" "splitw")     #'%cmd-split-window)
   (cons '("new-session" "new")         #'%cmd-new-session-arg)
   (cons '("set-environment" "setenv")  #'%cmd-set-environment-prompt)
   (cons '("resize-window" "resizew")   #'%cmd-resize-window-arg)
   (cons '("detach-client" "detachc")   #'%cmd-detach-client-arg)
   (cons '("send-keys" "send-key")      #'%cmd-send-keys-arg)
   (cons '("resize-pane" "resizep")     #'%cmd-resize-pane-arg)
   (cons '("capture-pane" "capturep")   #'%cmd-capture-pane-arg)
   (cons '("run-shell" "run")           #'%cmd-run-shell-arg)
   (cons '("if-shell" "if")             #'%cmd-if-shell-arg)
   (cons '("pipe-pane" "pipep")         #'%cmd-pipe-pane-arg)
   (cons '("list-sessions" "ls")        #'%cmd-list-sessions-arg)
   (cons '("list-windows" "lsw")        #'%cmd-list-windows-arg)
   (cons '("list-panes" "lsp")          #'%cmd-list-panes-arg-full))
  "Arg-taking commands: (list-of-names . handler), handler a function of
   (SESSION ARGS).  Consulted by %run-command-line before the no-argument
   %dispatch-named-command name table.")

(defun %run-command-tokens (session tokens)
  "Run a command line given as an already-tokenised TOKENS list (first = command
   name, rest = arguments).  Dispatch order:
   1. command-alias lookup (expand alias + append remaining tokens)
   2. arg-taking commands in *arg-command-table* (consume their arguments)
   3. no-arg named commands via %dispatch-named-command
   Taking pre-split tokens lets arg-bearing key bindings run without lossy
   re-tokenisation.  Returns the handler's return value."
  (let ((cmd  (first tokens))
        (rest (rest tokens)))
    (when cmd
      ;; 1. Command alias: expand and re-dispatch with remaining args appended.
      (let ((alias-exp (cl-tmux/options:lookup-command-alias cmd)))
        (if alias-exp
            (%run-command-line session
                               (format nil "~A~@[ ~{~A~^ ~}~]" alias-exp rest))
            ;; 2. Arg-taking commands (only when there are arguments to consume).
            (let ((entry (and rest
                              (find-if (lambda (e)
                                         (member cmd (car e) :test #'string-equal))
                                       *arg-command-table*))))
              (if entry
                  (funcall (cdr entry) session rest)
                  ;; 3. No-arg named commands (includes arg-cmds invoked with no args).
                  (%dispatch-named-command session cmd))))))))

(defun %run-command-line (session input)
  "Tokenise INPUT (one command line, shell-style) and run it.
   When the tokenised line contains \";\" tokens, splits into multiple commands
   and runs each in sequence, matching tmux's command-prompt behaviour."
  (let* ((tokens    (cl-tmux/commands:tokenize-command-string input))
         (sequences (cl-tmux/config::%split-on-semicolons tokens)))
    (if (= (length sequences) 1)
        (%run-command-tokens session (first sequences))
        (dolist (subcmd sequences)
          (%run-command-tokens session subcmd)))))

;;; -- dispatch-prefix-command -----------------------------------------------

(defun dispatch-prefix-command (session byte)
  "Handle one byte received after the prefix key.
   Copy mode intercepts [ ] q before the normal binding table.  A binding whose
   value is a token LIST (from `bind key command args...`) runs as a command
   line; a keyword value dispatches as a built-in command.
   Returns :REPEATABLE when the binding had the -r (repeatable) flag set, so
   the caller can stay in after-prefix state for the next key."
  (let* ((ch  (and byte (code-char byte)))
         (entry (if (%copy-mode-active-p session)
                    nil
                    (and ch (key-table-lookup +table-prefix+ ch))))
         (repeatable-p (and entry (key-table-repeatable-p entry)))
         (cmd (if (%copy-mode-active-p session)
                  (%copy-mode-cmd ch)
                  (and ch (lookup-key-binding ch))))
         (result (cond
                   ;; (:sequence cmd1 cmd2 ...) — run each sub-command in order.
                   ((and (consp cmd) (eq (car cmd) :sequence))
                    (let (last-result)
                      (dolist (subcmd (cdr cmd) last-result)
                        (setf last-result (%run-command-tokens session subcmd)))))
                   ;; Token list (arg-bearing command).
                   ((consp cmd)
                    (%run-command-tokens session cmd))
                   ;; Built-in command keyword.
                   (t
                    (dispatch-command session cmd byte)))))
    ;; Propagate :quit/:detach outcomes to the caller.  For other outcomes,
    ;; signal :repeatable when the binding had the -r flag so the caller can
    ;; stay in after-prefix state (resize without re-pressing the prefix key).
    (or (and (member result '(:quit :detach)) result)
        (and repeatable-p :repeatable))))
