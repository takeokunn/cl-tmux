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
  "Notify PANE of a focus change: fire the pane-focus-in / pane-focus-out hook
   (independent of ?1004), then send the application its focus-tracking report
   (ESC[I gained / ESC[O lost) when it enabled focus events and PANE has a live
   PTY.  A safe no-op when PANE is NIL."
  (when pane
    ;; Hook fires on every focus transition, regardless of whether the app
    ;; enabled ?1004 focus reporting (matches tmux's pane-focus-in/out hooks).
    (cl-tmux/hooks:run-hooks (if focused-p
                                 cl-tmux/hooks:+hook-pane-focus-in+
                                 cl-tmux/hooks:+hook-pane-focus-out+)
                             pane))
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
   directive) on SESSION.  A no-op when no command hooks are set.
   Hooks may be stored as keywords (legacy) OR as strings (from set-hook in
   .tmux.conf, e.g. 'display-message #{session_name}').  String hooks are
   run via %run-command-line so format expansion and argument parsing work."
  (dolist (entry (cl-tmux/hooks:command-hooks event-name))
    (cond
      ((stringp entry)
       ;; String hook from set-hook directive: run as a command line with full
       ;; format expansion and argument parsing.
       (ignore-errors (%run-command-line session entry)))
      ((keywordp entry)
       ;; Keyword hook from programmatic add-hook or legacy set-command-hook.
       (dispatch-command session entry 0)))))

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

(defun %cmd-split (session orient &key no-focus size start-dir before)
  "Split the active pane of SESSION's active window in tree ORIENT (:h left/right,
   :v top/bottom).  Returns NIL when the pane is too small and no shell is forked.
   NO-FOCUS T skips focus change.  SIZE hints the new pane's extent.
   BEFORE T inserts the new pane before the active pane (split-window -b).
   START-DIR: when non-NIL, the new pane's shell starts in that directory."
  (let* ((win (session-active-window session))
         (new (window-split win orient :no-focus no-focus :size size
                                       :start-dir start-dir :before before)))
    (when new
      (start-reader-thread new)
      (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-split-window+ new)
      ;; A split creates a new pane — fire after-new-pane too (was defined but
      ;; never fired).
      (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-new-pane+ new)
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
  "Apply LAYOUT-NAME to SESSION's active window and reassign geometry.
   Reads the main-pane-width/-height and other-pane-width/-height options here (the
   cl-tmux layer, above options) and threads them into apply-named-layout (model
   layer, below options) so main-horizontal / main-vertical honour their configured
   main / other pane sizes."
  (let ((win (session-active-window session)))
    (when win
      (cl-tmux/model:apply-named-layout
       win layout-name
       (or (cl-tmux/options:get-option "main-pane-width") 80)
       (or (cl-tmux/options:get-option "main-pane-height") 24)
       (or (cl-tmux/options:get-option "other-pane-width") 0)
       (or (cl-tmux/options:get-option "other-pane-height") 0))
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
           ;; Unknown name: show the error overlay AND return the :unknown-command
           ;; sentinel so callers (e.g. control mode's %error framing) can detect
           ;; the failure — the overlay value alone is not a reliable signal.
           (progn (show-overlay (format nil "unknown command: ~A" cmd-name))
                  :unknown-command)))))

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
  ("link-window"    :link-window)
  ("unlink-window"  :unlink-window)
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
  ("popup"             :display-popup)   ; documented alias (man tmux ALIASES)
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
  ("detach-all-clients"   :detach-all-clients)
  ;; ── Standard tmux command abbreviations (see man tmux "ALIASES") ──────────
  ;; The no-argument / fall-through forms; arg-bearing abbreviations (killp -t,
  ;; selectp -t, send, has, rename, renamew) are aliased in *arg-command-table*.
  ;; previous-layout and lock-client were dispatchable by keyword but had no name
  ;; entry at all — add both the canonical name and its abbreviation here.
  ("breakp"    :break-pane)
  ("clearhist" :clear-history)
  ("displayp"  :display-panes)
  ("findw"     :find-window)
  ("joinp"     :join-pane)
  ("killp"     :kill-pane)
  ("last"      :last-window)
  ("loadb"     :load-buffer)
  ("lock"      :lock-server)
  ("locks"     :lock-session)
  ("lock-client" :lock-client)
  ("lockc"     :lock-client)
  ("lsb"       :list-buffers)
  ("movep"     :move-pane)
  ("next"      :next-window)
  ("nextl"     :next-layout)
  ("pasteb"    :paste-buffer)
  ("prev"      :prev-window)
  ("previous-layout" :previous-layout)
  ("prevl"     :previous-layout)
  ("refresh"   :refresh-client)
  ("respawnp"  :respawn-pane)
  ("respawnw"  :respawn-window)
  ("rotatew"   :rotate-window)
  ("saveb"     :save-buffer)
  ("showb"     :show-buffer)
  ("showmsgs"  :show-messages)
  ("show"      :show-options))

;;; -- Arg-aware command-line runner -------------------------------------------
;;;
;;; The C-b : prompt may name a command WITH arguments (e.g.
;;; "display-message #{session_name}").  %run-command-line tokenises the line
;;; (shared shell-style lexer), routes arg-taking commands to their handlers, and
;;; falls through to the no-argument name table for everything else.

(defun %cmd-display-message (session args)
  "display-message [-d ms] [-t target] <fmt...>: expand the space-joined ARGS as a format string
   against the target (or active) session/window/pane, then log and show the result.
   -d ms: display duration in milliseconds (overrides display-time option).
   -t target: build the format context from the target's session/window/pane.
   Uses show-transient-overlay so it auto-dismisses after the configured duration."
  (multiple-value-bind (flags positionals) (%parse-command-flags args "dt")
    (let* ((delay-str  (cdr (assoc #\d flags)))
           (delay-ms   (and delay-str (parse-integer delay-str :junk-allowed t)))
           (target-str (cdr (assoc #\t flags)))
           ;; -t: resolve to a target session/window/pane; fall back to active.
           (tgt-session session)
           (tgt-win    (session-active-window session))
           (tgt-pane   (session-active-pane session)))
      (when target-str
        (multiple-value-bind (rs rw rp)
            (resolve-target *server-sessions* target-str
                            :current-session session
                            :current-window  (session-active-window session)
                            :current-pane    (session-active-pane session))
          (when rs (setf tgt-session rs))
          (when rw (setf tgt-win rw))
          (when rp (setf tgt-pane rp))))
    (let* ((win       tgt-win)
           (pane      tgt-pane)
           (ctx       (cl-tmux/format:format-context-from-session tgt-session win pane))
           (text      (cl-tmux/format:expand-format
                       (format nil "~{~A~^ ~}" positionals) ctx)))
      (add-message-log text)
      (if delay-ms
          ;; Custom delay: temporarily override display-time for this message.
          (let ((saved (cl-tmux/options:get-option "display-time" 750)))
            (cl-tmux/options:set-option "display-time" delay-ms)
            (show-transient-overlay text)
            (cl-tmux/options:set-option "display-time" saved))
          (show-transient-overlay text))))))

(defun %cmd-swap-pane-arg (session args)
  "swap-pane [-U|-D|-L|-R] [-Z]: swap the active pane with an adjacent one.
   -U: swap with pane above.  -D: swap below.
   -L: swap with pane to the left.  -R: swap with pane to the right.
   Without direction flags: swap forward (same as C-b })."
  (multiple-value-bind (flags _positionals) (%parse-command-flags args "")
    (declare (ignore _positionals))
    (with-active-window (win session)
      (cond
        ((assoc #\U flags) (swap-pane win :up))
        ((assoc #\D flags) (swap-pane win :down))
        ((assoc #\L flags) (swap-pane win :left))
        ((assoc #\R flags) (swap-pane win :right))
        ;; No direction: swap forward (default tmux behavior with no -d/-s flags)
        (t (swap-pane win :right))))))

(defun %cmd-command-prompt-arg (session args)
  "command-prompt [-p prompts] [template]: open a command prompt with optional args.
   -p prompts: comma-separated list of prompt labels; each label becomes a
     separate sequential prompt.  On completion, each response replaces %%1, %%2,
     etc. in TEMPLATE and the expanded command is executed.
   Without -p: single prompt ':' that runs the typed command line (same as C-b :).
   Without TEMPLATE: input is executed directly as a command line."
  (multiple-value-bind (flags positionals) (%parse-command-flags args "p")
    (let* ((prompts-str (cdr (assoc #\p flags)))
           (template    (format nil "~{~A~^ ~}" positionals))
           (prompt-list (when prompts-str
                          (mapcar (lambda (s) (string-trim " " s))
                                  (uiop:split-string prompts-str :separator ","))))
           (num-prompts (length prompt-list)))
      (cond
        ;; -p with template: multi-prompt with %%N substitution
        ((and prompt-list (plusp (length template)))
         (let ((answers (make-array num-prompts :initial-element "")))
           (labels ((ask-prompt (idx)
                      (if (>= idx num-prompts)
                          ;; All prompts answered — substitute %%N → answer and run
                          (let ((cmd (%substitute-percent
                                      template
                                      (loop for i below num-prompts collect (aref answers i)))))
                            (%run-command-line session cmd))
                          ;; Ask next prompt
                          (let ((label (nth idx prompt-list)))
                            (prompt-start label "" (lambda (input)
                                                     (setf (aref answers idx) input)
                                                     (ask-prompt (1+ idx))))))))
             (ask-prompt 0))))
        ;; -p without template: each prompt result is concatenated
        (prompt-list
         (let ((label (first prompt-list)))
           (prompt-start (or label ": ") ""
                         (lambda (input)
                           (unless (string= input "")
                             (add-prompt-history input)
                             (%run-command-line session input))))))
        ;; No -p: standard C-b : interactive prompt
        (t
         (prompt-start ": " ""
                       (lambda (input)
                         (unless (string= input "")
                           (add-prompt-history input)
                           (%run-command-line session input)))))))))

(defun %substitute-percent (template args)
  "Replace %%1, %%2, ... in TEMPLATE with ARGS list elements.
   Used by command-prompt -p substitution."
  (let ((result template))
    (loop for val in args
          for i from 1
          for pat = (format nil "%%~D" i)
          do (let ((out (make-string-output-stream)))
               (let ((start 0))
                 (loop for pos = (search pat result :start2 start)
                       while pos
                       do (write-string result out :start start :end pos)
                          (write-string val out)
                          (setf start (+ pos (length pat)))
                       finally (write-string result out :start start)))
               (setf result (get-output-stream-string out))))
    result))

(defun %cmd-last-pane-arg (session args)
  "last-pane [-Z]: jump to the previously active pane.
   -Z: zoom/unzoom the pane after selecting it (toggle zoom state)."
  (multiple-value-bind (flags _pos) (%parse-command-flags args "")
    (declare (ignore _pos))
    (let* ((win  (session-active-window session))
           (last (and win (window-last-active win))))
      (when last
        (%select-pane-with-focus win last)
        ;; -Z: toggle zoom on the newly selected pane's window.
        (when (assoc #\Z flags)
          (with-active-window (w session)
            (window-zoom-toggle w)))))))

(defun %cmd-has-session-arg (session args)
  "has-session [-t name]: check if a named session exists.
   Shows a transient overlay: 'has-session: yes' or 'has-session: no'.
   Without -t: checks if there is any session in *server-sessions*."
  (declare (ignore session))
  (multiple-value-bind (flags _positionals) (%parse-command-flags args "t")
    (declare (ignore _positionals))
    (let* ((target-name (cdr (assoc #\t flags)))
           (found       (if target-name
                            (server-find-session target-name)
                            (not (null *server-sessions*)))))
      (show-transient-overlay
       (if found
           (format nil "has-session ~A: yes" (or target-name ""))
           (format nil "has-session ~A: no"  (or target-name "")))))))

;;; ── Named paste-buffer commands (set/paste/delete/show -b name) ──────────────
;;;
;;; tmux's set-buffer/paste-buffer/delete-buffer/show-buffer all accept -b <name>
;;; to target a specific named buffer.  These arg-bearing handlers (registered in
;;; *arg-command-table*) layer over cl-tmux/buffer's named-buffer API; the no-arg
;;; keyword handlers (:set-buffer etc. in dispatch-handlers) remain for the C-b
;;; interactive bindings.

(defun %cmd-set-buffer-arg (session args)
  "set-buffer [-a] [-b name] [-t target] data: set a paste buffer's contents.
   -b name: name the buffer (retrievable via paste-buffer -b name, etc.); without
     -b an automatic name (bufferN) is assigned.
   -a: append DATA to the existing buffer (named NAME, or the most recent)."
  (declare (ignore session))
  (multiple-value-bind (flags positionals) (%parse-command-flags args "bt")
    (let* ((name     (cdr (assoc #\b flags)))
           (append-p (and (assoc #\a flags) t))
           (data     (format nil "~{~A~^ ~}" positionals)))
      (when positionals
        (if append-p
            (let ((existing (or (if name
                                    (cl-tmux/buffer:get-buffer-by-name name)
                                    (cl-tmux/buffer:get-paste-buffer 0))
                                "")))
              (cl-tmux/buffer:add-paste-buffer
               (concatenate 'string existing data) name))
            (cl-tmux/buffer:add-paste-buffer data name))))))

(defun %cmd-paste-buffer-arg (session args)
  "paste-buffer [-d] [-p] [-r] [-b name] [-s sep] [-t target]: paste a buffer into
   the target pane.  -b name pastes the named buffer (else the most recent); -d
   deletes the buffer after pasting.  -p (bracketed) / -r (no LF→CR) / -s sep are
   accepted but not specially handled."
  (multiple-value-bind (flags _positionals) (%parse-command-flags args "bst")
    (declare (ignore _positionals))
    (let* ((name       (cdr (assoc #\b flags)))
           (delete-p   (and (assoc #\d flags) t))
           (target-str (cdr (assoc #\t flags)))
           (text       (if name
                           (cl-tmux/buffer:get-buffer-by-name name)
                           (cl-tmux/buffer:get-paste-buffer 0)))
           (target-pane (if target-str
                            (nth-value 2 (resolve-target
                                          *server-sessions* target-str
                                          :current-session session
                                          :current-window (session-active-window session)
                                          :current-pane (session-active-pane session)))
                            (session-active-pane session))))
      (when text
        (%paste-to-pane target-pane text)
        (when delete-p
          (if name
              (cl-tmux/buffer:delete-buffer-by-name name)
              (cl-tmux/buffer:delete-paste-buffer 0)))))))

(defun %cmd-delete-buffer-arg (session args)
  "delete-buffer [-b name]: delete the named buffer (or the most recent)."
  (declare (ignore session))
  (multiple-value-bind (flags _positionals) (%parse-command-flags args "b")
    (declare (ignore _positionals))
    (let ((name (cdr (assoc #\b flags))))
      (if name
          (cl-tmux/buffer:delete-buffer-by-name name)
          (cl-tmux/buffer:delete-paste-buffer 0)))))

(defun %cmd-show-buffer-arg (session args)
  "show-buffer [-b name]: show the named buffer's contents (or the most recent)."
  (declare (ignore session))
  (multiple-value-bind (flags _positionals) (%parse-command-flags args "b")
    (declare (ignore _positionals))
    (let* ((name (cdr (assoc #\b flags)))
           (text (if name
                     (cl-tmux/buffer:get-buffer-by-name name)
                     (cl-tmux/buffer:get-paste-buffer 0))))
      (show-overlay (or text "(no buffer)")))))

;;; ── Popup overlay constants + formatter ─────────────────────────────────────
;;;
;;; Moved here from dispatch-handlers so BOTH the arg-bearing %cmd-display-popup
;;; (below, registered in *arg-command-table*) and the legacy :display-popup
;;; keyword handler (in dispatch-handlers, which loads after dispatch-core) can
;;; share them.  These bounds cap the overlay geometry to the terminal size.

(defconstant +popup-max-width+  60 "Maximum column width of a popup overlay.")
(defconstant +popup-max-height+ 15 "Maximum row height of a popup overlay.")
(defconstant +popup-margin+      4 "Row margin subtracted from terminal height for popups.")

(defun %popup-border-chars ()
  "Return (values TOP-LEFT TOP-RIGHT BOTTOM-LEFT BOTTOM-RIGHT HORIZONTAL) box-
   drawing characters for popup-border-lines.  Delegates to the single source
   cl-tmux/renderer:%popup-border-charset (the text overlay has no sides, so the
   vertical character it also returns is dropped here)."
  (multiple-value-bind (tl tr bl br h v) (cl-tmux/renderer:%popup-border-charset)
    (declare (ignore v))
    (values tl tr bl br h)))

(defun %format-popup-overlay (title output)
  "Format a popup overlay string with box-drawing borders whose characters follow
   the popup-border-lines option.  TITLE is the header; OUTPUT is the body."
  (multiple-value-bind (tl tr bl br h) (%popup-border-chars)
    (format nil "~C~C ~A ~C~C~%~A~%~C~A~C"
            tl h title h tr
            (or output "")
            bl (make-string (+ 2 (length title)) :initial-element h) br)))

(defun %popup-dimension (spec axis-total fallback)
  "Resolve a popup -w/-h dimension SPEC against AXIS-TOTAL (the terminal width or
   height).  SPEC may be NIL (use FALLBACK), an integer string (absolute cells),
   or an N% string (percentage of AXIS-TOTAL, which tmux accepts, e.g. -w 80%).
   Returns a positive integer clamped to [1, AXIS-TOTAL]."
  (let ((n (cond
             ((null spec) fallback)
             ((and (plusp (length spec))
                   (char= (char spec (1- (length spec))) #\%))
              (let ((pct (parse-integer spec :end (1- (length spec)) :junk-allowed t)))
                (if pct (max 1 (floor (* axis-total pct) 100)) fallback)))
             (t (or (parse-integer spec :junk-allowed t) fallback)))))
    (max 1 (min n axis-total))))

(defun %cmd-display-popup (session args)
  "display-popup (alias: popup) [-E] [-w width] [-h height] [-x col] [-y row]
   [-d dir] [-t target] [-c client] [-b border] [-T title] [command]: show a popup.

   With a COMMAND (the common `bind C-p popup -E \"cmd\"` form), run it in a shell
   and display its output in the popup directly — no prompt.  With NO command, open
   the interactive popup-command prompt (the legacy :display-popup behaviour).
   -w/-h accept absolute cells or an N% of the terminal; -E/-EE and
   -x/-y/-d/-t/-c/-b are parsed and tolerated.  Geometry is clamped to the overlay
   bounds (cl-tmux popups render command output, not a live embedded terminal)."
  (declare (ignore session))
  (multiple-value-bind (flags positionals) (%parse-command-flags args "whxydtcbT")
    (let* ((title   (or (cdr (assoc #\T flags)) ""))
           (command (when positionals (format nil "~{~A~^ ~}" positionals)))
           (width   (%popup-dimension (cdr (assoc #\w flags)) *term-cols* +popup-max-width+))
           (height  (%popup-dimension (cdr (assoc #\h flags)) *term-rows*
                                      (min +popup-max-height+ (- *term-rows* +popup-margin+))))
           (clamp-w (min width  *term-cols*))
           (clamp-h (min height (max 1 (- *term-rows* +popup-margin+)))))
      (flet ((render (cmd)
               (let ((label  (if (plusp (length title)) title cmd))
                     (output (run-shell cmd)))
                 (show-popup (make-popup :title label :width clamp-w :height clamp-h
                                         :screen nil :pane nil))
                 (show-overlay (%format-popup-overlay label output)))))
        (if command
            (render command)
            ;; No command: fall back to the interactive popup-command prompt.
            (prompt-start "popup command" ""
                          (lambda (cmd)
                            (unless (string= cmd "") (render cmd)))))))))

(defun %cmd-display-menu-arg (session args)
  "display-menu [-T title] [-x x] [-y y] [label key command ...]: show an interactive menu.
   -T title: menu title (default: 'Menu').
   -x col / -y row: screen position (default: centred).  Clamped on screen.
   Item triples: label key command.  Empty label '' creates a visual separator.
   When selected, command is run via %run-command-line.
   Preconfigured commands as keyword tokens run directly (for compatibility)."
  (declare (ignore session))  ; session used via closure in item command
  (multiple-value-bind (flags positionals) (%parse-command-flags args "Txy")
    (let* ((title (or (cdr (assoc #\T flags)) "Menu"))
           (x-str (cdr (assoc #\x flags)))
           (y-str (cdr (assoc #\y flags)))
           (menu-x (and x-str (parse-integer x-str :junk-allowed t)))
           (menu-y (and y-str (parse-integer y-str :junk-allowed t)))
           ;; Build items from consecutive (label key command) triples.
           ;; Silently skip incomplete triples (real tmux shows an error).
           (items (loop for (label key cmd) on positionals by #'cdddr
                        when (and label key cmd)
                        collect (cons (if (and (plusp (length label))
                                               (plusp (length key)))
                                          (format nil "~A [~A]" label key)
                                          label)
                                      cmd))))
      (when items
        (show-menu (make-menu :title title :items items :selected-index 0
                              :x menu-x :y menu-y))
        (show-overlay (%format-menu *active-menu*))))))

(defun %cmd-confirm-before-arg (session args)
  "confirm-before [-p prompt] command: prompt before running COMMAND.
   -p prompt: custom prompt text (default: 'command? (y/n)').
   COMMAND is the remaining positional tokens as a command line.
   Only executes COMMAND when the user confirms with 'y' or 'Y'."
  (multiple-value-bind (flags positionals) (%parse-command-flags args "p")
    (let* ((custom-prompt (cdr (assoc #\p flags)))
           (cmd-line      (format nil "~{~A~^ ~}" positionals))
           (prompt-text   (or custom-prompt
                              (format nil "~A? (y/n)" cmd-line))))
      (when (plusp (length cmd-line))
        (prompt-start prompt-text ""
                      (lambda (input)
                        (when (member input '("y" "Y") :test #'string=)
                          (%run-command-line session cmd-line))))))))

(defun %cmd-list-keys-arg (session args)
  "list-keys [-T table] [-1] [key]: list key bindings.
   -T table: show bindings for TABLE only (e.g. prefix, root, copy-mode-vi).
   Without -T: show all tables.  Additional positionals and flags (-1) are accepted
   but ignored for simplicity (cl-tmux shows the full table always)."
  (declare (ignore session))
  (multiple-value-bind (flags _positionals) (%parse-command-flags args "T")
    (declare (ignore _positionals))
    (let* ((table-name (cdr (assoc #\T flags)))
           (output     (cl-tmux/config:describe-key-bindings-for-table table-name)))
      (show-overlay (if (plusp (length output))
                        output
                        (format nil "(no bindings in table ~A)"
                                (or table-name "all")))))))

(defun %cmd-copy-mode-arg (session args)
  "copy-mode [-e] [-u]: enter copy mode.
   -u: pre-scroll to the oldest scrollback content (e.g. bind PageUp copy-mode -u).
   -e: exit copy mode automatically when the viewport is scrolled back down to
       the live bottom (offset 0).  Standard for mouse-wheel copy-mode entry:
       `bind -n WheelUpPane copy-mode -e` enters copy mode on scroll-up and
       leaves it once the user scrolls back to the live output."
  (let* ((flags (nth-value 0 (%parse-command-flags args "")))
         (scroll-to-top  (and (assoc #\u flags) t))
         (exit-on-bottom (and (assoc #\e flags) t))
         (screen (%active-screen session)))
    (when screen
      (copy-mode-enter screen :scroll-to-top scroll-to-top
                              :exit-on-bottom exit-on-bottom)
      (setf *dirty* t))))

;;; *set-option-command-names* removed — inlined into *arg-command-table* below.

(defun %cmd-set-option (session args)
  "set / set-option [-g|-s|-w|-p|-o] [-a] [-u] <name> <value...>: set an option.
   Scope flags route a normal set to the matching store:
     -p  pane-local   — stores on SESSION's active pane (falls back to global
                        when there is no active pane).
     -w  window-local — stores on SESSION's active window (falls back to global
                        when there is no active window).
     -g  global / (default) — stores in the flat global option table.
   -p and -w are ignored when -g is also present (explicit global wins).
   -s (server) and -o (only-if-unset) are accepted and treated as the global
   store (cl-tmux keeps a flat option table).
   -a appends VALUE to the option's current global value; -u unsets the global
   option (removes the override, reverting to the registered default).  -a/-u
   always operate on the GLOBAL store regardless of -w/-p.
   NOTE: this fixes `set -g status off`, which previously set an option literally
   named \"-g\"."
  (multiple-value-bind (flags positionals) (%parse-command-flags args "")
    (let ((name  (first positionals))
          (value (format nil "~{~A~^ ~}" (rest positionals)))
          (globalp (and (assoc #\g flags) t))
          (windowp (and (assoc #\w flags) t))
          (panep   (and (assoc #\p flags) t)))
      (when name
        (cond
          ;; -u: unset option — remove from hash table so the default is used.
          ((assoc #\u flags)
           (remhash name cl-tmux/options:*global-options*))
          ;; -a: append value to existing (global store).
          ((assoc #\a flags)
           (cl-tmux/options:set-option
            name (concatenate 'string
                              (princ-to-string
                               (or (cl-tmux/options:get-option name nil) ""))
                              value)))
          ;; normal set: route by scope flag.
          (t
           (let ((pane   (and panep   (not globalp) (session-active-pane session)))
                 (window (and windowp (not globalp) (session-active-window session))))
             (cond
               ;; -p (and not -g): pane-local when an active pane exists.
               (pane
                (cl-tmux/options:set-option-for-pane name value pane))
               ;; -w (and not -g): window-local when an active window exists.
               (window
                (cl-tmux/options:set-option-for-window name value window))
               ;; global (default, includes -g and the no-active-pane/window fallback).
               (t
                (cl-tmux/options:set-option name value))))))))))

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
  "rename-window <name...>: rename SESSION's active window to the joined ARGS."
  (let ((win (session-active-window session)))
    (when win (rename-window win (format nil "~{~A~^ ~}" args)))))

(defun %rename-session-checked (session new-name)
  "Rename SESSION to NEW-NAME, keeping *server-sessions* keyed by the new name and
   firing +hook-session-renamed+.  REFUSES (returns NIL) when NEW-NAME is empty or
   already used by a DIFFERENT session — tmux rejects a rename onto an existing name
   (`duplicate session`) rather than silently orphaning the other session; renaming
   to the session's CURRENT name is a harmless no-op that still succeeds.  The single
   chokepoint both rename paths (arg command + interactive prompt) route through.
   Returns T on success."
  (when (and new-name (not (string= new-name "")))
    (let ((existing (server-find-session new-name)))
      (unless (and existing (not (eq existing session)))   ; a different session owns it
        (server-remove-session (session-name session))
        (rename-session session new-name)
        (server-add-session session)
        (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-session-renamed+ session)
        t))))

(defun %cmd-rename-session (session args)
  "rename-session <name...>: rename SESSION to the joined ARGS, updating the
   registry key.  Refuses a name already used by another session (see
   %rename-session-checked)."
  (%rename-session-checked session (format nil "~{~A~^ ~}" args)))

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
  "Resolve TARGET-STR to a window in SESSION.
   Supports special shorthands:
     :!  — last (previously active) window
     :+  — next window (wraps)
     :-  — previous window (wraps)
     :^  — first window
     :$  — last window
   Also accepts window-id (numeric) or window-name (string)."
  (let* ((wins (session-windows session))
         (act  (session-active-window session)))
    (cond
      ;; Special shorthands (with or without leading colon)
      ((member target-str '(":!" "!") :test #'string=)
       (session-last-window session))
      ((member target-str '(":+" "+") :test #'string=)
       (when wins
         (let ((idx (or (position act wins) 0)))
           (nth (mod (1+ idx) (length wins)) wins))))
      ((member target-str '(":-" "-") :test #'string=)
       (when wins
         (let ((idx (or (position act wins) 0)))
           (nth (mod (1- idx) (length wins)) wins))))
      ((member target-str '(":^" "^") :test #'string=)
       (first wins))
      ((member target-str '(":$" "$") :test #'string=)
       (car (last wins)))
      ;; Numeric window-id
      (t
       (let ((n (parse-integer target-str :junk-allowed t)))
         (if n
             (find n wins :key #'window-id)
             (find target-str wins :key #'window-name :test #'string-equal)))))))

(defun %cmd-select-window (session args)
  "select-window [-t target] [-l] [-n] [-p]: select a window.
   -t target: window-id, name, or special shorthand (:! last, :+ next, :- prev).
   -l: select the last (previously active) window (same as C-b l).
   -n: select the next window.
   -p: select the previous window.
   Delivers ?1004 focus events on the switch."
  (multiple-value-bind (flags _pos) (%parse-command-flags args "t")
    (declare (ignore _pos))
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
               (when win (session-select-window session win))))))))))

(defun %cmd-select-pane (session args)
  "select-pane [-L|-R|-U|-D|-d|-e|-m] [-t target] [-T title]: select or configure a pane.
   -L/-R/-U/-D: move in the given direction.
   -d: disable keyboard input to the target pane (pane-input-disabled t).
   -e: re-enable keyboard input to the target pane (pane-input-disabled nil).
   -t target: select pane by pane-id in the active window.
   -T title: set the title of the target (or active) pane.
   -m: mark the selected pane."
  (multiple-value-bind (flags _positionals) (%parse-command-flags args "tT")
    (declare (ignore _positionals))
    (cond
      ((assoc #\L flags) (%select-pane-in-direction session :left))
      ((assoc #\R flags) (%select-pane-in-direction session :right))
      ((assoc #\U flags) (%select-pane-in-direction session :up))
      ((assoc #\D flags) (%select-pane-in-direction session :down))
      ;; -d: disable pane input (keystrokes will be swallowed rather than sent)
      ((assoc #\d flags)
       (with-active-pane (ap session)
         (setf (pane-input-disabled ap) t)))
      ;; -e: enable pane input (re-enable after -d)
      ((assoc #\e flags)
       (with-active-pane (ap session)
         (setf (pane-input-disabled ap) nil)))
      ;; -T title: set the pane title (equivalent to OSC 0/2 renaming)
      ((assoc #\T flags)
       (let* ((title (cdr (assoc #\T flags)))
              (pane  (session-active-pane session)))
         (when (and pane title)
           (setf (pane-title pane) title)
           ;; Also update the screen title so #{pane_title} reflects the change.
           (let ((screen (pane-screen pane)))
             (when screen
               (cl-tmux/terminal/actions:set-screen-title screen title))))))
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
             (when pane (%select-pane-with-focus win pane))))))))
  ;; after-select-pane fires once after the select-pane command, regardless of
  ;; which form (-L/-R/.../-t/-T/-m) it took.
  (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-after-select-pane+ session))

(defun %cmd-kill-window (session args)
  "kill-window [-a] [-t target]: kill a window or all windows except the current.
   -a: kill ALL windows in the session EXCEPT the target (or active) window.
   -t target: target window by id or name.
   No flags: kill the active window."
  (multiple-value-bind (flags _pos) (%parse-command-flags args "t")
    (declare (ignore _pos))
    (let* ((target-str  (cdr (assoc #\t flags)))
           (kill-others (assoc #\a flags))
           (ref-win     (if target-str
                            (%resolve-window-target session target-str)
                            (session-active-window session))))
      (if kill-others
          ;; -a: kill all EXCEPT the reference window
          (let ((to-kill (remove ref-win (session-windows session))))
            (dolist (w to-kill)
              (%handle-kill-result (kill-window session w))))
          ;; Normal: kill the target window
          (when ref-win
            (%handle-kill-result (kill-window session ref-win)))))))

(defun %window-session-count (window)
  "Number of sessions in *server-sessions* whose window list contains WINDOW.
   Used by unlink-window to avoid orphaning a window that is only in one session."
  (count-if (lambda (entry)
              (member window (session-windows (cdr entry))))
            *server-sessions*))

(defun %cmd-link-window (session args)
  "link-window [-s src] -t dst [-k]: share a window into another session.
   -s src: source window target (session:window); default is the active window.
   -t dst: destination session (session or session:window).
   -k: kill any window already occupying the destination index first.
   The window object is SHARED — it appears in both sessions at the same index
   (cl-tmux stores the index in the window struct, so linked windows share it)."
  (multiple-value-bind (flags _pos) (%parse-command-flags args "st")
    (declare (ignore _pos))
    (let* ((src-str (cdr (assoc #\s flags)))
           (dst-str (cdr (assoc #\t flags)))
           (kill-p  (assoc #\k flags))
           ;; Resolve source window (default: active window of current session).
           (src-win (if src-str
                        (nth-value 1 (resolve-target *server-sessions* src-str
                                                     :current-session session
                                                     :current-window (session-active-window session)))
                        (session-active-window session)))
           ;; Resolve destination session.
           (dst-sess (and dst-str
                          (nth-value 0 (resolve-target *server-sessions* dst-str
                                                       :current-session session)))))
      (cond
        ((not (and src-win dst-sess))
         (show-overlay "link-window: source window or destination session not found"))
        ;; Already linked there — nothing to do.
        ((member src-win (session-windows dst-sess))
         (show-overlay "link-window: window already linked in destination"))
        (t
         (let ((collision (find (window-id src-win) (session-windows dst-sess)
                                :key #'window-id)))
           (cond
             ((and collision kill-p)
              (kill-window dst-sess collision)
              (session-insert-window dst-sess src-win)
              (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-window-linked+ src-win)
              (show-overlay "link-window: linked (replaced existing)"))
             (collision
              (show-overlay "link-window: target index in use (add -k to replace)"))
             (t
              (session-insert-window dst-sess src-win)
              (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-window-linked+ src-win)
              (show-overlay "link-window: linked")))))))))

(defun %cmd-unlink-window (session args)
  "unlink-window [-t target] [-k]: remove a window's link from its session.
   -t target: window to unlink (default: active window).
   The window is removed from the resolved session only when it is also linked in
   at least one OTHER session (so it is not orphaned).  When it exists in only
   one session, -k is required to actually destroy it (matches tmux)."
  (multiple-value-bind (flags _pos) (%parse-command-flags args "t")
    (declare (ignore _pos))
    (let* ((target-str (cdr (assoc #\t flags)))
           (kill-p     (assoc #\k flags))
           (win        (if target-str
                           (%resolve-window-target session target-str)
                           (session-active-window session))))
      (cond
        ((null win)
         (show-overlay "unlink-window: window not found"))
        ((> (%window-session-count win) 1)
         ;; Linked elsewhere — safe to drop from this session only.
         (let ((was-active (eq (session-active-window session) win)))
           (setf (session-windows session) (remove win (session-windows session)))
           ;; Reselect a remaining window if we just removed the active one.
           (when (and was-active (session-windows session))
             (session-select-window session (first (session-windows session)))))
         (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-window-unlinked+ win)
         (show-overlay "unlink-window: unlinked"))
        (kill-p
         ;; Only in this session and -k given — destroy it.
         (%handle-kill-result (kill-window session win))
         (show-overlay "unlink-window: killed (last link)"))
        (t
         (show-overlay "unlink-window: window only in this session (add -k to kill)"))))))

(defun %cmd-kill-pane (session args)
  "kill-pane [-a] [-t target]: kill the target pane, or all except target with -a.
   -a: kill all panes in the active window EXCEPT the target (or active) pane.
   -t target: target pane by pane-id.
   No -t: target is the active pane.  A -t that matches nothing is a no-op."
  (multiple-value-bind (flags _pos) (%parse-command-flags args "t")
    (declare (ignore _pos))
    (let* ((target-str (cdr (assoc #\t flags)))
           (kill-all   (assoc #\a flags))
           (n          (and target-str (parse-integer target-str :junk-allowed t)))
           (win        (session-active-window session))
           ;; Determine the pane to KEEP (when -a) or KILL.
           (ref-pane   (if (and n win)
                           (find n (window-panes win) :key #'pane-id)
                           (session-active-pane session))))
      (cond
        ;; -a: kill all panes EXCEPT the reference pane.
        (kill-all
         (when (and win ref-pane)
           (dolist (p (copy-list (window-panes win)))
             (unless (eq p ref-pane)
               (%handle-kill-result (kill-pane session p))))))
        ;; Normal: kill the target (or active) pane.
        ((or ref-pane (null target-str))
         (%handle-kill-result (kill-pane session ref-pane)))))))

(defun %swap-window-ids (session win-a win-b)
  "Exchange the index numbers (window-id) of WIN-A and WIN-B and re-sort the
   session's window list by id — tmux's swap-window, which trades the two windows'
   INDICES (so #{window_index}, the status bar, and select-window -t follow the
   content).  This is distinct from a list-position swap, which would leave the
   indices out of order.  No-op when either window is NIL or they are the same.
   Returns T when a swap occurred."
  (when (and win-a win-b (not (eq win-a win-b)))
    (rotatef (window-id win-a) (window-id win-b))
    (setf (session-windows session)
          (sort (copy-list (session-windows session)) #'< :key #'window-id))
    t))

(defun %cmd-swap-window (session args)
  "swap-window [-s src] -t dst: exchange the index numbers of two windows.  SRC and
   DST are window-id/name targets; with no -s the active window is the source.
   First command to use two value flags (-s and -t) at once."
  (multiple-value-bind (flags positionals) (%parse-command-flags args "st")
    (declare (ignore positionals))
    (let ((src (if (cdr (assoc #\s flags))
                   (%resolve-window-target session (cdr (assoc #\s flags)))
                   (session-active-window session)))
          (dst (and (cdr (assoc #\t flags))
                    (%resolve-window-target session (cdr (assoc #\t flags))))))
      (%swap-window-ids session src dst))))

(defun %cmd-source-file (session args)
  "source-file [-q] [-n] [-v] path...: load the tmux config file(s) at the given
   path(s), expanding ~ and shell globs (* ? []).  Enables the canonical reload
   binding (bind r source-file ~/.tmux.conf).  A missing file or parse error never
   crashes the session.  SESSION unused."
  (declare (ignore session))
  (cl-tmux/config:source-files args))

(defun %window-id-occupied-p (session id exclude)
  "T when some window OTHER than EXCLUDE in SESSION already has window-id ID."
  (loop for w in (session-windows session)
        thereis (and (not (eq w exclude)) (= (window-id w) id))))

(defun %shuffle-windows-up (session dst exclude)
  "Make room at index DST by shifting windows up — tmux's winlink_shuffle_up.
   Finds the first free index >= DST (ignoring EXCLUDE) and increments the id of
   every other window in [DST, free) by one, highest-id first so no two windows
   collide mid-shift."
  (let ((free dst))
    (loop while (%window-id-occupied-p session free exclude) do (incf free))
    (dolist (w (sort (remove exclude (copy-list (session-windows session)))
                     #'> :key #'window-id))
      (when (<= dst (window-id w) (1- free))
        (incf (window-id w))))))

(defun %cmd-move-window (session args)
  "move-window [-s src-window] [-t dst-index] [-r] [-a]: move/renumber a window.
   -s src: source window (name or id); default is the active window.
   -t n: destination window-id (numeric index to assign to the window).
   -r: renumber all windows sequentially from base-index (repack gaps).
   -a: insert after the current window (used with -t for relative positioning.
   Without -s/-t: prompts interactively (no-op in arg-command path)."
  (multiple-value-bind (flags positionals) (%parse-command-flags args "st")
    (declare (ignore positionals))
    (let* ((src-str (cdr (assoc #\s flags)))
           (dst-str (cdr (assoc #\t flags)))
           (repack  (assoc #\r flags))
           (after   (assoc #\a flags))
           (src-win (if src-str
                        (%resolve-window-target session src-str)
                        (session-active-window session)))
           (dst-n   (and dst-str (parse-integer dst-str :junk-allowed t))))
      (cond
        ;; -r: repack all windows sequentially from base-index
        (repack
         (let* ((base (or (cl-tmux/options:get-option "base-index") 0))
                (sorted (sort (copy-list (session-windows session))
                              #'< :key #'window-id)))
           (loop for win in sorted
                 for i from base
                 do (setf (window-id win) i))
           (setf (session-windows session) sorted)))
        ;; -t n (with optional -s src / -a): move the window to index n.  -a
        ;; inserts AFTER index n (n+1); the default/-b inserts AT n.  When the
        ;; target index is occupied by ANOTHER window, the windows at and above it
        ;; shift up to make room (tmux's winlink_shuffle_up) rather than the move
        ;; being silently dropped or another window orphaned.
        ((and src-win dst-n)
         (let ((target (if after (1+ dst-n) dst-n)))
           (when (%window-id-occupied-p session target src-win)
             (%shuffle-windows-up session target src-win))
           (setf (window-id src-win) target
                 (session-windows session)
                 (sort (copy-list (session-windows session)) #'< :key #'window-id))))))))

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

(defun %format-pane-info (session win pane)
  "Return a short pane info string: session:window.pane geometry.
   Used by -P flag in new-window and split-window."
  (format nil "~A:~A.~A: [~Dx~D]"
          (session-name session)
          (if win (window-id win) "?")
          (if pane (pane-id pane) "?")
          (if pane (pane-width pane) 0)
          (if pane (pane-height pane) 0)))

(defun %cmd-new-window-arg (session args)
  "new-window [-d] [-k] [-P] [-n name] [-t target-window] [-a] [-c start-dir] [-e VAR=val].
   -d: create the window but do not make it active (detached).
   -k: kill any existing window at the target index before creating the new one.
   -P: print the new pane's details (session:window.pane [WxH]) to overlay.
   -n name: name the new window.
   -t idx: insert at specific index (assigned as the window id).
   -a: insert after the current window.
   -c dir: start directory for the new pane's shell (format strings expanded).
   -e VAR=val: set environment variable in the new pane (repeatable)."
  (multiple-value-bind (flags positionals) (%parse-command-flags args "ntce")
    (declare (ignore positionals))
    (let* ((extra-env  (%collect-env-flags flags))
           (name       (cdr (assoc #\n flags)))
           (detach-p   (assoc #\d flags))
           (kill-p     (assoc #\k flags))
           (print-p    (assoc #\P flags))
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
      ;; -k: if a window with the target index already exists, kill it first.
      (when (and kill-p at-idx)
        (let ((existing (find at-idx (session-windows session) :key #'window-id)))
          (when existing
            (%handle-kill-result (kill-window session existing)))))
      ;; Inject -e VAR=val pairs via *pane-extra-env* so %fork-pane picks them up.
      (when extra-env
        (setf *pane-extra-env* extra-env))
      (let ((new-win (%cmd-new-window session
                                      :name name
                                      :start-dir start-dir
                                      :detach (and detach-p t)
                                      :at-index at-idx
                                      :after-current (and after-p t))))
        ;; -P: print new pane details to overlay.
        (when (and print-p new-win)
          (show-transient-overlay
           (%format-pane-info session new-win (window-active-pane new-win))))
        new-win))))

(defun %cmd-split-window (session args)
  "split-window [-h|-v] [-b] [-d] [-t target] [-p percent] [-l size] [-c start-dir] [-e VAR=val].
   -h: horizontal split (new pane to the right; side-by-side).
   -v: vertical split (new pane below — default).
   -b: insert before the active pane (left of / above) instead of after.
   -d: split but do not change focus (detached mode).
   -t target: split the target pane instead of the active pane.
   -p N: size as a percentage of the parent pane (0-100).
   -l N: size in lines/columns (absolute integer).
   -c dir: start directory for the new pane's shell (format strings expanded).
   -e VAR=val: set environment variable in the new pane (repeatable)."
  (multiple-value-bind (flags positionals) (%parse-command-flags args "plcet")
    (declare (ignore positionals))
    (let* ((extra-env    (%collect-env-flags flags))
           (horizontal-p (assoc #\h flags))
           (before-p     (assoc #\b flags))
           (detach-p     (assoc #\d flags))
           (target-str   (cdr (assoc #\t flags)))
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
      ;; -t target: temporarily make the target pane active so %cmd-split
      ;; operates on it.  Restore the previous active pane afterwards if -d.
      (let* ((prev-win  (session-active-window session))
             (prev-pane (and prev-win (window-active-pane prev-win))))
        (when target-str
          (multiple-value-bind (_sess target-win target-pane)
              (resolve-target *server-sessions* target-str
                              :current-session session
                              :current-window prev-win
                              :current-pane prev-pane)
            (declare (ignore _sess))
            (when (and target-win target-pane)
              ;; Switch active window and pane to the target for the split.
              (session-select-window session target-win)
              (window-select-pane target-win target-pane))))
        ;; Inject -e VAR=val pairs via *pane-extra-env* so %fork-pane picks them up.
        (when extra-env
          (setf *pane-extra-env* extra-env))
        (let* ((print-p (assoc #\P flags))
               (result
                (if horizontal-p
                    (%cmd-split session :h :size size :no-focus (and detach-p t)
                                        :start-dir start-dir :before (and before-p t))
                    (%cmd-split session :v :size size :no-focus (and detach-p t)
                                        :start-dir start-dir :before (and before-p t)))))
          ;; Restore original focus when -d (detach): the target had focus switched
          ;; transiently for the split but the user wants to stay in the prior window.
          (when (and detach-p target-str prev-win)
            (session-select-window session prev-win)
            (when prev-pane (window-select-pane prev-win prev-pane)))
          ;; -P: print the new pane's details.
          (when (and print-p result)
            (let ((win (pane-window result)))
              (show-transient-overlay (%format-pane-info session win result))))
          result)))))

(defun %parse-wxh (str)
  "Parse a \"WxH\" size string (e.g. the default-size option \"80x24\") into
   (values W H), or (values NIL NIL) when STR is not of that form or either
   dimension is not a positive integer."
  (when (stringp str)
    (let ((x (position #\x str :test #'char-equal)))
      (when x
        (let ((w (parse-integer str :end x :junk-allowed t))
              (h (parse-integer str :start (1+ x) :junk-allowed t)))
          (when (and w h (plusp w) (plusp h))
            (return-from %parse-wxh (values w h))))))
    (values nil nil)))

(defun %cmd-new-session-arg (session args)
  "new-session [-A] [-d] [-s name] [-n window-name] [-c start-dir] [-x width] [-y height]: create a new session.
   -A: if a session named NAME already exists, attach to it instead of creating a new one.
   -d: create detached (do not switch to the new session).
   -s name: session name.
   -n name: initial window name.
   -c dir: start directory for the initial window's shell.
   -x width: initial columns (default: terminal width, or default-size when -d).
   -y height: initial rows (default: terminal height minus status bar, or
     default-size when -d).
   A DETACHED session (-d) has no client to size it, so — like tmux — it uses the
   default-size option (\"WxH\", default 80x24) when -x/-y are not given."
  (multiple-value-bind (flags positionals) (%parse-command-flags args "sncxy")
    (declare (ignore positionals))
    (let* ((name            (or (cdr (assoc #\s flags))
                                (format nil "~D" (1+ (length *server-sessions*)))))
           (attach-if-exists (assoc #\A flags))
           (detach-p         (assoc #\d flags))
           (win-name         (cdr (assoc #\n flags)))
           (start-dir        (cdr (assoc #\c flags)))
           (x-str            (cdr (assoc #\x flags)))
           (y-str            (cdr (assoc #\y flags)))
           ;; Detached sessions have no client → fall back to default-size, not the
           ;; current terminal size.  NIL for attached sessions (use the terminal).
           (default-wxh      (and detach-p
                                  (cl-tmux/options:get-option "default-size" "80x24")))
           ;; -x/-y override everything when given (junk-allowed).
           (cols             (or (and x-str (parse-integer x-str :junk-allowed t))
                                 (and default-wxh (nth-value 0 (%parse-wxh default-wxh)))
                                 *term-cols*))
           (rows             (or (and y-str (parse-integer y-str :junk-allowed t))
                                 (and default-wxh (nth-value 1 (%parse-wxh default-wxh)))
                                 (- *term-rows* *status-height*))))
      ;; -A: attach to existing session if it exists
      (when attach-if-exists
        (let ((existing (server-find-session name)))
          (when existing
            (session-touch existing)
            (unless detach-p (setf *dirty* t))
            (return-from %cmd-new-session-arg existing))))
      ;; Without -A, a name already in use cannot be taken over (server-add-session
      ;; would orphan the existing session): an EXPLICIT -s duplicate is refused
      ;; (tmux's "duplicate session"); an AUTO name bumps to the next free number.
      (when (and (not attach-if-exists) (server-find-session name))
        (if (cdr (assoc #\s flags))
            (progn
              (show-overlay (format nil "duplicate session: ~A" name))
              (return-from %cmd-new-session-arg nil))
            (setf name (loop for i from 1
                             for candidate = (format nil "~D" i)
                             unless (server-find-session candidate) return candidate))))
      ;; Create a new session
      (let ((new-sess (new-session name rows cols :start-dir start-dir)))
        ;; Apply window name if given
        (when (and win-name new-sess)
          (let ((win (session-active-window new-sess)))
            (when win (rename-window win win-name))))
        ;; Without -d, show an overlay confirming the new session was created.
        ;; With -d, the session is created in background and SESSION (the calling
        ;; session) remains the active display — no dirty flag, no visual switch.
        (when (and new-sess (not detach-p))
          (show-transient-overlay
           (format nil "new session: ~A" (session-name new-sess))))
        new-sess))))

(defvar *key-table* nil
  "The client's active custom key table (a table-name string), or NIL for the
   normal root/prefix flow.  Set by `switch-client -T <table>`; while non-NIL the
   ground input state looks keys up in this table (modal keymaps).  Defined here
   (dispatch-core loads before events-keystroke) so it is declared special before
   either %cmd-switch-client or %ground-input-state references it.")

(defun %current-session (&optional fallback)
  "The session the standalone client is currently viewing: the most-recently-
   touched (highest session-last-active) session in *server-sessions*, or FALLBACK
   when the registry is empty.  This is how session-switch commands (switch-client,
   choose-tree, last-session) change the displayed session — they session-touch
   their target, and the event loop re-resolves the current session through here on
   every iteration, so the display follows the switch.  Delegates to the registry's
   server-current-session (highest last-active), adding the FALLBACK for the empty
   registry — ties (same-second stamps) resolve there; deliberate switches are
   seconds apart in practice."
  (or (server-current-session) fallback))

(defun %switch-to-session (target)
  "Make TARGET the client's active session by bumping its last-active stamp (the
   renderer follows the most-recently-touched session via %current-session) and
   marking the screen dirty.  No-op when TARGET is NIL.  Returns TARGET when a switch
   happened, else NIL — the single chokepoint every session move routes through.
   When destroy-unattached is on, the session the client was viewing becomes
   unattached on the switch and is destroyed (tmux's destroy-unattached)."
  (when target
    (let ((old (server-current-session)))   ; the session being left, if any
      (session-touch target)
      (setf *dirty* t)
      (when (and old (not (eq old target))
                 (cl-tmux/options:get-option "destroy-unattached"))
        (%destroy-session old))
      target)))

(defun %cmd-switch-client (session args)
  "switch-client [-T key-table] [-t target] [-n] [-p] [-l]: control the client's
   session and key table.
     -T <table>  set the active custom key table (modal keymaps); `-T root` (or no
                 -T) returns to the normal root/prefix flow.
     -t <name>   switch the client to the named session.
     -n / -p     switch to the next / previous session (cyclic over the registry).
     -l          switch to the last (most-recently-active-but-one) session.
   -T is independent of the session flags, so `switch-client -t foo -T copy-mode`
   both moves the client and arms a key table.  Mirrors the keybinding handlers
   :switch-client / :switch-client-next/-prev / :last-session, reusing the same
   session-touch primitive."
  (multiple-value-bind (flags _pos) (%parse-command-flags args "Tt")
    (declare (ignore _pos))
    ;; -T key table (modal keymap) — orthogonal to the session move below.
    (let ((table (cdr (assoc #\T flags))))
      (when table
        (setf *key-table* (if (equal table +table-root+) nil table))))
    ;; Session selection: -t named, else -n/-p cyclic, else -l last-active.
    (let ((sessions (mapcar #'cdr *server-sessions*)))
      (cond
        ((assoc #\t flags)
         (%switch-to-session (server-find-session (cdr (assoc #\t flags)))))
        ((assoc #\n flags)
         (%switch-to-session (and sessions (next-cyclic sessions session))))
        ((assoc #\p flags)
         (%switch-to-session (and sessions (prev-cyclic sessions session))))
        ((assoc #\l flags)
         (%switch-to-session
          (second (sort (copy-list sessions) #'> :key #'session-last-active))))))))

(defun %destroy-session (session)
  "Tear down SESSION: close every pane's PTY, remove it from the server registry,
   and fire the session-closed hook.  The single chokepoint for session
   DESTRUCTION (every kill-session path routes through here) — deliberately
   distinct from rename-session, which also removes+re-adds the registry entry but
   must NOT fire session-closed.  Returns the session name."
  (when session
    (let ((name (session-name session)))
      (dolist (pane (all-panes session))
        (ignore-errors (pty-close (pane-fd pane) (pane-pid pane))))
      (server-remove-session name)
      (cl-tmux/hooks:run-hooks cl-tmux/hooks:+hook-session-closed+ session)
      name)))

(defun %alphabetical-neighbour (name dir)
  "The surviving session whose name is alphabetically just after (DIR +1) or
   before (DIR -1) NAME (the destroyed session's name, no longer in the registry),
   wrapping around.  Returns NIL when no sessions survive.  Backs detach-on-destroy
   previous/next."
  (let ((sorted (sort (mapcar #'cdr *server-sessions*) #'string< :key #'session-name)))
    (when sorted
      (if (plusp dir)
          (or (find-if (lambda (s) (string< name (session-name s))) sorted)
              (first sorted))
          (or (find-if (lambda (s) (string< (session-name s) name)) (reverse sorted))
              (car (last sorted)))))))

(defun %detach-on-destroy-action (destroyed-name)
  "Decide the standalone client's fate after the session it was viewing (named
   DESTROYED-NAME) is destroyed, per the detach-on-destroy option
   (off / on (default) / no-detached / previous / next).  Returns :QUIT when the
   client should detach — which in the single-client standalone model means exit —
   or NIL when it switches to a surviving session (the event loop then follows the
   new current session).  No survivors → always :QUIT.  off/no-detached fall to the
   most-recent survivor (the loop's natural choice); previous/next touch the
   alphabetical neighbour of DESTROYED-NAME so the loop moves there."
  (if (null *server-sessions*)
      :quit
      (let ((mode (or (cl-tmux/options:get-option "detach-on-destroy") "on")))
        (cond
          ((string= mode "on") :quit)
          ((string= mode "previous")
           (%switch-to-session (%alphabetical-neighbour destroyed-name -1)) nil)
          ((string= mode "next")
           (%switch-to-session (%alphabetical-neighbour destroyed-name 1)) nil)
          (t nil)))))   ; off / no-detached → most-recent survivor (loop auto-follows)

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
              (%destroy-session (cdr entry))))
          ;; No -a: kill the target session
          (let ((target-sess (or (and target-name
                                      (cdr (assoc target-name *server-sessions*
                                                  :test #'equal)))
                                 session)))
            (when target-sess
              (let ((name        (session-name target-sess))
                    (was-current (eq target-sess session)))
                (%destroy-session target-sess)
                ;; Killing the session the client is viewing → apply detach-on-destroy.
                (when (and was-current
                           (eq :quit (%detach-on-destroy-action name)))
                  (setf *running* nil)))))))))

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
    ("cursor-up"                 . :copy-mode-cursor-up)
    ("cursor-down"               . :copy-mode-cursor-down)
    ("cursor-left"               . :copy-mode-cursor-left)
    ("cursor-right"              . :copy-mode-cursor-right)
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
    ("rectangle-toggle"          . :copy-mode-rectangle-toggle)
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
    ("select-word"               . :copy-mode-select-word)
    ("copy-pipe"                 . :copy-mode-yank)
    ("copy-pipe-and-cancel"      . :copy-mode-yank)
    ;; mouse-wheel support
    ("scroll-mouse"              . :copy-mode-scroll-up-line)
    ;; vi-style movement
    ("previous-paragraph"        . :copy-mode-page-up)
    ("next-paragraph"            . :copy-mode-page-down)
    ("jump-to-mark"              . :copy-mode-line-start)
    ;; other-end / toggle-position: swap the two ends of the selection (vi `o`).
    ("other-end"                 . :copy-mode-other-end)
    ("toggle-position"           . :copy-mode-other-end)
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
   -J (join wrapped lines) / -e (escapes) / -N (trailing spaces) / -a / -P:
     accepted but not specially handled.
   -t target: target pane (standalone uses the active pane)."
  (multiple-value-bind (flags _positionals) (%parse-command-flags args "tSEb")
    (declare (ignore _positionals))
    (let* ((print-p (assoc #\p flags))
           (include-scrollback (assoc #\S flags))
           (pane (session-active-pane session))
           (content (and pane (capture-pane pane :include-scrollback
                                            (and include-scrollback t)))))
      (when content
        (if print-p
            ;; -p: stdout equivalent — show the content in an overlay.
            (show-overlay content)
            ;; Default: save to a paste buffer (silent), like tmux.  -b names it.
            (cl-tmux/buffer:add-paste-buffer content (cdr (assoc #\b flags))))))))

(defun %cmd-resize-pane-arg (session args)
  "resize-pane [-t target] [-L|-R|-U|-D|-Z] [amount]: resize a pane.
   -t target: target pane by pane-id or 'session:window.pane' (default: active pane).
   -L/-R/-U/-D: resize by AMOUNT (default 5) in the given direction.
   -Z: zoom-toggle the target pane."
  (multiple-value-bind (flags positionals) (%parse-command-flags args "t")
    (let* ((amount-str (first positionals))
           (amount     (or (and amount-str (parse-integer amount-str :junk-allowed t)) 5))
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
        ((assoc #\L flags) (when win (resize-pane win :left  amount)))
        ((assoc #\R flags) (when win (resize-pane win :right amount)))
        ((assoc #\U flags) (when win (resize-pane win :up    amount)))
        ((assoc #\D flags) (when win (resize-pane win :down  amount)))))))

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
   -R (reset terminal state) and -M (mouse passthrough) are accepted but not acted
   on — they are rare in .tmux.conf and need pane terminal-reset / mouse-bind
   context respectively.
   Without -X: each positional is a key name or literal string typed into the pane."
  (multiple-value-bind (flags positionals) (%parse-command-flags args "tN")
    (let* ((target-str (cdr (assoc #\t flags)))
           (literal-p  (and (assoc #\l flags) t))
           (hex-p      (and (assoc #\H flags) t))
           (x-p        (and (assoc #\X flags) t))
           (count      (let ((n (cdr (assoc #\N flags))))
                         (max 1 (or (and n (parse-integer n :junk-allowed t)) 1))))
           ;; Resolve -t to a specific pane; fall back to the active pane.
           (target-pane
            (if target-str
                (multiple-value-bind (_s _w pane)
                    (resolve-target *server-sessions* target-str
                                    :current-session session
                                    :current-window  (session-active-window session)
                                    :current-pane    (session-active-pane session))
                  (declare (ignore _s _w))
                  pane)
                (session-active-pane session))))
      (cond
        ;; -X: dispatch the copy-mode command (first positional) COUNT times.
        (x-p
         (when (first positionals)
           (dotimes (_ count)
             (%dispatch-send-keys-X session (first positionals)))))
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
          (t (pipe-pane-open ap command)))))))

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
   (cons '("rename-window" "renamew")   #'%cmd-rename-window)
   (cons '("rename-session" "rename")   #'%cmd-rename-session)
   (cons '("select-window" "selectw")   #'%cmd-select-window)
   (cons '("select-pane" "selectp")     #'%cmd-select-pane)
   (cons '("kill-window" "killw")       #'%cmd-kill-window)
   (cons '("kill-pane" "killp")         #'%cmd-kill-pane)
   (cons '("kill-session")              #'%cmd-kill-session-arg)
   (cons '("swap-window" "swapw")       #'%cmd-swap-window)
   (cons '("move-window" "movew")       #'%cmd-move-window)
   (cons '("link-window" "linkw")       #'%cmd-link-window)
   (cons '("unlink-window" "unlinkw")   #'%cmd-unlink-window)
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
   (cons '("send-keys" "send-key" "send") #'%cmd-send-keys-arg)
   (cons '("resize-pane" "resizep")     #'%cmd-resize-pane-arg)
   (cons '("capture-pane" "capturep")   #'%cmd-capture-pane-arg)
   (cons '("run-shell" "run")           #'%cmd-run-shell-arg)
   (cons '("if-shell" "if")             #'%cmd-if-shell-arg)
   (cons '("pipe-pane" "pipep")         #'%cmd-pipe-pane-arg)
   (cons '("list-sessions" "ls")        #'%cmd-list-sessions-arg)
   (cons '("list-windows" "lsw")        #'%cmd-list-windows-arg)
   (cons '("list-panes" "lsp")          #'%cmd-list-panes-arg-full)
   ;; copy-mode [-u]: -u flag pre-scrolls to oldest content on entry.
   (cons '("copy-mode")                 #'%cmd-copy-mode-arg)
   ;; list-keys [-T table]: filter by key table when -T is supplied.
   (cons '("list-keys" "lsk")           #'%cmd-list-keys-arg)
   ;; swap-pane [-U|-D|-L|-R]: directional swap including up/down.
   (cons '("swap-pane" "swapp")         #'%cmd-swap-pane-arg)
   ;; confirm-before [-p prompt] cmd: gate COMMAND behind a y/n prompt.
   (cons '("confirm-before" "confirm")  #'%cmd-confirm-before-arg)
   ;; command-prompt [-p prompts] [template]: interactive prompt with substitution.
   (cons '("command-prompt" "commandp") #'%cmd-command-prompt-arg)
   ;; display-menu [-T title] [-x x] [-y y] [label key cmd ...]: interactive menu.
   (cons '("display-menu" "menu")       #'%cmd-display-menu-arg)
   ;; display-popup [-E] [-w W] [-h H] [-T title] [cmd]: run cmd, show output in a
   ;; popup (the `bind C-p popup -E "cmd"` form); no cmd opens the prompt.
   (cons '("display-popup" "popup")     #'%cmd-display-popup)
   ;; Named paste-buffer commands: -b <name> targets a specific named buffer.
   (cons '("set-buffer" "setb")         #'%cmd-set-buffer-arg)
   (cons '("paste-buffer" "pasteb")     #'%cmd-paste-buffer-arg)
   (cons '("delete-buffer" "deleteb")   #'%cmd-delete-buffer-arg)
   (cons '("show-buffer" "showb")       #'%cmd-show-buffer-arg)
   ;; has-session [-t name]: check if a named session exists (0 = yes, 1 = no).
   (cons '("has-session" "has")         #'%cmd-has-session-arg)
   ;; switch-client -T <key-table>: activate a custom key table (modal keymaps).
   (cons '("switch-client" "switchc")   #'%cmd-switch-client)
   ;; last-pane [-Z]: select last pane, optionally toggling zoom.
   (cons '("last-pane" "lastp")         #'%cmd-last-pane-arg))
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

;;; -- Control mode (-C) REPL ------------------------------------------------
;;;
;;; A control client sends commands as text lines; each is run via %run-command-line
;;; and the reply is framed by cl-tmux/control.  A command's textual output (what it
;;; would show in an overlay — list-sessions, display-message, ...) is captured by
;;; binding *overlay* around the run and reading it back into the reply body.

(defun %control-run-command (session line number)
  "Run control-mode command LINE for SESSION as command NUMBER and return the
   framed %begin/.../%end reply string.  The command's overlay text is captured as
   the reply body; a signalled error closes the block with %error."
  (let ((cl-tmux/prompt:*overlay* nil)
        (success t))
    (handler-case
        ;; An unknown command returns the :unknown-command sentinel → %error.
        (when (eq :unknown-command (%run-command-line session line))
          (setf success nil))
      (error () (setf success nil)))
    (cl-tmux/control:control-format-reply
     number (or cl-tmux/prompt:*overlay* "") :success success)))

(defun %install-control-notifications (output)
  "Register hook callbacks that write control-mode (-C) %-notifications to OUTPUT as
   windows/sessions change (the asynchronous half of control mode).  Returns the
   list of (event . callback) pairs so %remove-control-notifications can unregister
   them when the client detaches.  Each callback is variadic so it tolerates the
   hook's argument list; the changed object is the first argument."
  (labels ((emit (line) (write-line line output) (force-output output))
           ;; after-resize-pane fires with a WINDOW, after-split-window with a PANE;
           ;; coerce either to its window so we can serialise its layout.
           (window-of (obj)
             (if (cl-tmux/model::pane-p obj) (cl-tmux/model:pane-window obj) obj))
           (emit-layout (obj)
             (let ((win (window-of obj)))
               (when win
                 (let ((layout (cl-tmux/model:layout->string win)))
                   (emit (cl-tmux/control:control-layout-change
                          (window-id win) layout layout
                          (if (cl-tmux/model:window-zoom-p win) "Z" "*"))))))))
    (let ((handlers
            (list
             (cons cl-tmux/hooks:+hook-after-new-window+
                   (lambda (&rest a)
                     (emit (cl-tmux/control:control-window-add (window-id (first a))))))
             (cons cl-tmux/hooks:+hook-after-kill-window+
                   (lambda (&rest a)
                     (emit (cl-tmux/control:control-window-close (window-id (first a))))))
             (cons cl-tmux/hooks:+hook-window-renamed+
                   (lambda (&rest a)
                     (emit (cl-tmux/control:control-window-renamed
                            (window-id (first a)) (window-name (first a))))))
             (cons cl-tmux/hooks:+hook-session-renamed+
                   (lambda (&rest a)
                     (emit (cl-tmux/control:control-session-renamed
                            (session-id (first a)) (session-name (first a))))))
             ;; Layout changes: resize fires with the window, split with the pane.
             (cons cl-tmux/hooks:+hook-after-resize-pane+
                   (lambda (&rest a) (emit-layout (first a))))
             (cons cl-tmux/hooks:+hook-after-split-window+
                   (lambda (&rest a) (emit-layout (first a)))))))
      (dolist (h handlers) (cl-tmux/hooks:add-hook (car h) (cdr h)))
      handlers)))

(defun %remove-control-notifications (handlers)
  "Unregister the control-mode notification callbacks installed by
   %install-control-notifications (HANDLERS is its return value)."
  (dolist (h handlers) (cl-tmux/hooks:remove-hook (car h) (cdr h))))

(defun control-mode-loop (session input output)
  "The control-mode (-C) REPL: read command lines from INPUT, run each as the next
   numbered command, write its framed reply to OUTPUT, until EOF — then emit %exit.
   While the loop runs, %-notifications are emitted to OUTPUT as windows/sessions
   change (installed/removed around the loop).  Streams are parameters so the loop
   is testable without a real tty."
  (let ((handlers (%install-control-notifications output)))
    (unwind-protect
         (loop with number = 0
               for line = (read-line input nil nil)
               while line
               unless (string= "" (string-trim '(#\Space #\Tab #\Return) line))
                 do (incf number)
                    (write-line (%control-run-command session line number) output)
                    (force-output output))
      (%remove-control-notifications handlers)))
  (write-line (cl-tmux/control:control-exit) output)
  (force-output output))

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
