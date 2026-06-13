(in-package #:cl-tmux)

;;;; Copy-mode key overrides, format helpers, new-session factory,
;;;;  and named-command table (C-b : prompt resolution).

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
  (#\W :copy-mode-space-forward)
  (#\B :copy-mode-space-backward)
  (#\E :copy-mode-space-end)
  (#\0 :copy-mode-line-start)
  (#\^ :copy-mode-back-to-indentation)
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
   Falls back to a single entry for CURRENT-SESSION when the registry is empty."
  (let ((current-name (session-name current-session)))
    (with-output-to-string (s)
      (loop for (name . sess) in (or *server-sessions*
                                     (list (cons current-name current-session)))
            for i from 0
            do (format s "~A~A: ~A (~D window~:P)~%"
                       (if (string= name current-name) "*" " ")
                       i name
                       (length (session-windows sess)))))))

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
  ;; Bare no-arg forms of two arg-commands whose bare invocation is meaningful in
  ;; tmux: `list-panes` lists the current window's panes, `list-commands` lists all
  ;; commands.  Their flag forms (-F/-a/-t, filter NAME) live in *arg-command-table*,
  ;; but that table is only consulted WITH args — so the bare names need an entry
  ;; here too or they error as "unknown command".
  ("list-panes"    :list-panes)
  ("lsp"           :list-panes)
  ("list-commands" :list-commands)
  ("lscm"          :list-commands)
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
  ;; Server lifecycle: handlers exist (dispatch-handlers.lisp) but were never
  ;; reachable by name from the C-b : prompt or control mode until registered here.
  ("kill-server"       :kill-server)
  ("start-server"      :start-server)
  ;; send-prefix: forward the prefix key to the active pane (the literal C-b form).
  ("send-prefix"       :send-prefix)
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
  ;; customize-mode no-arg form (the -f/-F/-t form is in *arg-command-table*).
  ("customize-mode"       :customize-mode)
  ("customize"            :customize-mode)
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

