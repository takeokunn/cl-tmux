;;; ── Domain Ports (Dependency Inversion) ─────────────────────────────────────
;;;
;;; cl-tmux/ports defines the abstract interface for PTY operations.
;;; Domain code (cl-tmux/model) calls these port functions.
;;; Infrastructure (cl-tmux/pty) installs concrete implementations at startup.
;;; This enforces the Dependency Inversion Principle:
;;;   high-level module (domain) → abstraction (ports)
;;;   low-level module (infra)   → implements abstraction

(defpackage #:cl-tmux/ports
  (:use #:cl)
  (:export
   ;; PTY port variables (set by install-pty-port at server startup)
   #:*spawn-pty*
   #:*write-pty*
   #:*resize-pty*
   #:*close-pty*
   ;; PTY port functions (called by domain model)
   #:spawn-pty
   #:write-pty
   #:resize-pty
   #:close-pty))

;;; ── Session Repository (DDD Repository Pattern) ──────────────────────────────
;;;
;;; cl-tmux/repository defines the abstract repository interface for sessions.
;;; Domain code declares what operations exist (generic functions).
;;; The composition root (bootstrap) provides the concrete in-memory implementation.

(defpackage #:cl-tmux/repository
  (:use #:cl)
  (:export
   ;; Repository protocol (generic functions)
   #:repo-find-session
   #:repo-add-session
   #:repo-remove-session
   #:repo-all-sessions
   #:repo-current-session
   ;; Active repository instance
   #:*session-repo*))

;; cl-tmux/prompt package: command-line prompt state, dismissible overlays,
;; popups and menus (all presentation-layer, PTY-independent).
(defpackage #:cl-tmux/prompt
  (:use #:cl)
  (:export
   #:prompt #:make-prompt #:prompt-p
   #:prompt-label #:prompt-buffer #:prompt-cursor-index #:prompt-on-submit
   #:prompt-on-change #:prompt-on-cancel #:prompt-numeric-only
   #:prompt-close-on-focus-out #:prompt-clear
   #:prompt-vi-normal-p #:prompt-single-key
   #:with-active-prompt
   #:*prompt* #:prompt-active-p #:prompt-start
   #:prompt-input #:prompt-backspace #:prompt-clear #:prompt-text
   #:prompt-notify-change
   ;; Cursor navigation
   #:prompt-cursor-bol #:prompt-cursor-eol
   #:prompt-cursor-back #:prompt-cursor-forward
   ;; Kill commands
   #:prompt-kill-to-end #:prompt-kill-to-start #:prompt-kill-word-back
   ;; Prompt history navigation
   #:prompt-history-prev #:prompt-history-next
   ;; Vi-mode character deletion (vi x)
   #:prompt-delete-char
   ;; Dismissible overlay (list-keys help, …)
   #:*overlay* #:*overlay-scroll-offset* #:*display-panes-active*
   #:overlay-active-p #:overlay-shown-at #:show-overlay #:show-transient-overlay
   #:show-display-panes-overlay
   #:clear-overlay #:overlay-lines
   #:overlay-scroll #:*overlay-shown-at*
   ;; Popup overlay
   #:+default-popup-width+ #:+default-popup-height+
   #:popup #:make-popup #:popup-p
   #:popup-width #:popup-height
   #:popup-screen #:popup-pane #:popup-title #:popup-close-on-exit
   #:*active-popup*
   #:show-popup #:close-popup #:popup-active-p
   ;; Menu overlay
   #:menu #:make-menu #:menu-p
   #:menu-title #:menu-items #:menu-selected-index
   #:menu-x #:menu-y
   #:menu-keep-open
   #:*active-menu*
   #:show-menu #:close-menu #:menu-active-p))

;;; ── Model / renderer / input ─────────────────────────────────────────────

(defpackage #:cl-tmux/model
  (:use #:cl #:bordeaux-threads
        #:cl-tmux/config #:cl-tmux/ports #:cl-tmux/terminal)
  (:export
   ;; Pane
   #:pane
   #:make-pane
   #:pane-id
   #:pane-x #:pane-y
   #:pane-width #:pane-height
   #:pane-screen
   #:pane-fd
   #:pane-pid
   #:pane-feed
   #:pane-pipe-fd
   #:pane-pipe-active-p
   #:pane-live-p
   #:pane-pipe-output-stream
   #:pane-pipe-output-thread
   #:pane-pipe-process
   #:pane-window
   #:pane-marked
   #:pane-title
   #:pane-tty
   ;; Spawn record for #{pane_start_command} / #{pane_start_path}
   #:pane-start-command
   #:pane-start-path
   ;; Death record for remain-on-exit / #{pane_dead_status} family
   #:pane-dead-status
   #:pane-dead-signal
   #:pane-dead-time
   #:pane-input-disabled
   #:pane-local-options
   #:respawn-pane
   ;; Window
   #:window
   #:make-window
   #:window-id
   #:window-name
   #:window-width #:window-height
   #:window-panes
   #:window-tree
   #:window-active-pane
   #:window-active
   #:window-select-pane
   #:window-split
   #:window-relayout
   #:window-relayout-current
   #:%assign-window-tree    ; (window w h &optional top-offset) → layout-assign with y-offset
   #:%status-top-offset     ; () → rows reserved at top for a top status bar
   #:window-remove-pane
   #:window-resize-active
   #:window-refresh-panes
   #:ensure-window-fits
   #:pane-reposition
   ;; Layout split-tree
   #:+pane-min-width+ #:+pane-min-height+
   #:layout-leaf #:make-layout-leaf #:layout-leaf-p #:layout-leaf-pane
   #:layout-split #:make-layout-split #:layout-split-p
   #:layout-split-orientation #:layout-split-first #:layout-split-second
   #:layout-split-ratio
   #:layout-leaves #:layout-find-leaf #:layout-find-parent
   #:layout-min-extent #:layout-assign #:layout-split-axis-extent
   #:resize-find-split #:resize-direction-orientation
   #:split-child-geometry #:next-pane-id
   #:pane-neighbor
   ;; Layout persistence
   #:layout->string
   ;; Zoom
   #:window-zoom-p
   #:window-zoom-tree
   #:window-zoom-toggle
   #:window-lock
   ;; Last-active pane (for C-b ;)
   #:window-last-active
   ;; Last-active time (for C-b l last-window)
   #:window-last-active-time
   ;; Per-window local options
   #:window-local-options
   ;; Automatic-rename (OSC 0/2 updates window-name)
   #:window-automatic-rename-p
   ;; Activity flag for monitor-activity / #{window_activity_flag}
   #:window-activity-flag
   ;; Sticky bell flag for monitor-bell / #{window_bell_flag}
   #:window-bell-flag
   ;; Silence tracking for monitor-silence
   #:window-last-output-time
   #:window-silence-flag
   ;; Per-pane-spawn extra environment injection (new-window -e / split-window -e)
   #:*pane-extra-env*
   ;; Layout cycle index (for C-b Space next-layout)
   #:window-layout-cycle-index
   #:window-last-layout-tree
   ;; Rotate-window
   #:window-rotate
   ;; Session
   #:make-session
   #:*session-id-counter*
   #:session-id
   #:session-name
   #:session-windows
   #:session-active-window
   #:session-active
   #:session-select-window
   #:session-insert-window
   #:session-new-window
   ;; Session-group window-set sharing (policy hook + notifier)
   #:*session-windows-sync-function*
   #:session-windows-changed
   #:session-active-pane
   #:session-last-active
   #:session-created
   #:session-window-stack
   ;; Per-session winlink indexes (link-window into a different slot)
   #:session-window-index
   #:set-session-window-index
   #:session-window-index-map
   #:session-windows-in-index-order
   #:session-clients
   #:session-locked-p
   #:session-group
   #:session-start-directory
   #:session-touch
   ;; Pane hit testing
   #:pane-at-position
   ;; Named layouts
   #:apply-named-layout
   ;; Window reordering
   #:session-move-window
   #:session-swap-windows
   #:session-last-window
   ;; Global state
   #:create-initial-session
   #:all-panes
   #:process-environment-value
   #:process-environment-names
   #:process-set-environment
   #:process-unset-environment
   #:session-environment
   #:session-environment-value
   #:session-environment-names
   #:session-set-environment
   #:session-unset-environment
   ;; Hidden environment variables (set-environment -h / ENVIRON_HIDDEN)
   #:session-environment-hidden
   #:*global-hidden-environment-names*
   #:session-child-environment
   #:*suppress-update-environment*
   ;; update-environment
   #:+default-update-environment+
   #:*update-environment*
   #:get-update-environment-vars))

;; cl-tmux/format package: #{...} format-string expansion for status lines,
;; window/pane titles, and command output.
(defpackage #:cl-tmux/format
  (:use #:cl #:cl-tmux/model)
  (:export #:expand-format #:expand-format-safe
           #:format-context-from-session #:format-context-from-window))

;; cl-tmux/buffer package: paste-buffer storage (tmux copy-mode "buffers"),
;; including OSC 52 clipboard integration.
(defpackage #:cl-tmux/buffer
  (:use #:cl)
  (:export #:+default-buffer-limit+
           #:*paste-buffers* #:*buffer-auto-index*
           #:add-paste-buffer #:rename-paste-buffer #:get-paste-buffer #:set-named-buffer
           #:get-named-buffer #:buffer-names
           #:initialize-osc52-handler
           #:list-paste-buffers #:list-paste-buffers-with-names
           #:delete-paste-buffer #:delete-buffer-by-name #:clear-paste-buffers))

(defpackage #:cl-tmux/control
  (:use #:cl)
  ;; control-error is a standard CL condition type; we define our own line
  ;; formatter of that name, so shadow the inherited symbol.
  (:shadow #:control-error)
  (:documentation "tmux control mode (-C) wire-protocol line formatters.")
  (:export #:control-begin #:control-end #:control-error #:control-format-reply
           #:control-escape-output #:control-output
           #:control-session-changed #:control-session-renamed
           #:control-window-add #:control-window-close #:control-window-renamed
           #:control-layout-change #:control-unlinked-window-add
           #:control-window-pane-changed #:control-session-window-changed
           #:control-client-session-changed #:control-exit))

;; cl-tmux/hooks package: named-event registry (set-hook / hooks option) plus
;; the tmux command-hook dispatch used by define-msg-dispatch et al.
(defpackage #:cl-tmux/hooks
  (:use #:cl)
  (:export
   ;; Event string constants
   #:+hook-after-new-window+
   #:+hook-after-new-pane+
   #:+hook-pane-exited+
   #:+hook-after-rename-window+
   #:+hook-session-created+
   #:+hook-after-kill-pane+
   #:+hook-after-kill-window+
   #:+hook-after-split-window+
   #:+hook-client-attached+
   #:+hook-client-detached+
   #:+hook-alert-bell+
   #:+hook-alert-activity+
   #:+hook-alert-silence+
   #:+hook-pane-focus-in+
   #:+hook-pane-focus-out+
   #:+hook-after-select-pane+
   #:+hook-after-select-window+
   #:+hook-session-window-changed+
   #:+hook-window-pane-changed+
   #:+hook-window-renamed+
   #:+hook-session-renamed+
   #:+hook-after-resize-pane+
   #:+hook-client-resized+
   #:+hook-window-linked+
   #:+hook-window-unlinked+
   #:+hook-session-closed+
   #:+hook-pane-output+
   #:+hook-pane-died+
   ;; Registry and dispatch
   #:*hook-registry*
   #:add-hook
   #:remove-hook
   #:run-hooks
   #:clear-hooks
   #:list-hooks
   #:*command-hooks*
   #:set-command-hook
   #:scoped-hook-entry-p
   #:append-command-hook
   #:command-hooks
   #:clear-command-hooks
   #:list-command-hooks
   #:describe-command-hooks
   #:*command-hook-runner*
   #:run-command-hooks-via-runner))

;; cl-tmux/options package: server/session/window/pane option tables (set-option
;; / show-options), including scoped accessors and display formatting.
(defpackage #:cl-tmux/options
  (:use #:cl)
  (:export #:*global-options* #:*option-registry*
           #:option-spec #:make-option-spec
           #:option-spec-name #:option-spec-type #:option-spec-default
           ;; Registration macros
           #:define-option-table
           #:define-tmux-options
           #:get-option #:set-option
           #:option-defined-p #:all-options
           #:option-scope-from-name
           #:style-option-p #:append-option-value
           ;; Server options
           #:*server-options* #:*server-option-registry*
           #:define-server-options
           #:get-server-option #:set-server-option
           ;; Scoped accessors (per-window / per-pane)
           #:get-option-for-window #:set-option-for-window
           #:get-option-for-pane   #:set-option-for-pane
           #:get-option-for-context
           ;; Scope/presence predicates (options-scope.lisp)
           #:option-present-for-scope-p
           #:option-present-for-display-p
           ;; Display helpers (options-display.lisp)
           #:show-options #:show-option
           #:show-window-options #:show-window-option
           #:window-option-present-for-display-p
           ))

;; cl-tmux/renderer package: composes model + terminal + prompt state into a
;; single output frame (mouse/focus/extended-keys reporting, style/SGR helpers).
(defpackage #:cl-tmux/renderer
  (:use #:cl #:bordeaux-threads
        #:cl-tmux/model #:cl-tmux/terminal #:cl-tmux/prompt)
  (:export
   #:render-session            ; (session rows cols) → write a frame to stdout
   #:render-session-to-string  ; (session rows cols) → frame string (server uses this)
   #:clear-display
   #:enable-mouse-reporting    ; () → emit ?1000h/?1002h/?1006h to outer terminal
   #:disable-mouse-reporting   ; () → emit ?1000l/?1002l/?1006l to outer terminal
   #:extended-keys-level       ; (option-value) → modifyOtherKeys level 1/2 or NIL
   #:enable-extended-keys      ; (option-value) → emit CSI >4;Nm; returns level or NIL
   #:disable-extended-keys     ; () → emit CSI >4;0m to reset extended-keys reporting
   #:enable-focus-reporting    ; () → emit ?1004h to enable focus events on outer term
   #:disable-focus-reporting   ; () → emit ?1004l to disable focus events on outer term
   #:parse-style-string        ; (style-str) → plist :fg :bg :bold :reverse etc.
   #:style-to-sgr              ; (parsed-style) → escape-sequence string
   #:%popup-border-charset))   ; () → (values tl tr bl br h v) for popup-border-lines

;; cl-tmux/input package: raw-mode terminal control and non-blocking byte
;; reads from the outer terminal (client-side keystroke ingestion).
(defpackage #:cl-tmux/input
  (:use #:cl #:cffi
        #:cl-tmux/config #:cl-tmux/pty)
  (:export
   #:with-raw-mode        ; macro — raw mode for body, restores on exit
   #:read-byte-nonblock)) ; (&optional timeout-us) → byte or nil

;;; ── High-level commands ──────────────────────────────────────────────────

(defpackage #:cl-tmux/commands
  (:use #:cl #:bordeaux-threads
        #:cl-tmux/config
        #:cl-tmux/pty
        #:cl-tmux/terminal
        #:cl-tmux/model
        #:cl-tmux/hooks)
  (:export
   #:kill-pane
   #:kill-window
   #:resize-pane
   #:rename-window
   #:select-window-by-number
   #:copy-mode-enter
   #:copy-mode-exit
   #:copy-mode-copy-selection-no-cancel
   #:copy-mode-copy-selection-no-clear
   #:copy-mode-pipe-no-cancel #:copy-mode-pipe-no-clear #:copy-mode-pipe-and-cancel
   #:copy-mode-copy-pipe-no-clear #:copy-mode-copy-pipe-line
   #:copy-mode-copy-pipe-line-and-cancel
   #:copy-mode-rectangle-on #:copy-mode-rectangle-off
   #:copy-mode-cursor-centre-vertical #:copy-mode-cursor-centre-horizontal
   #:copy-mode-scroll
   #:copy-mode-move-cursor
   #:copy-mode-set-cursor
   #:copy-mode-begin-selection
   #:copy-mode-cancel-selection
   #:copy-mode-other-end
   #:copy-mode-jump-to-mark
   #:copy-mode-clear-selection
   #:copy-mode-select-word
   #:copy-mode-yank
   ;; Word navigation
   #:copy-mode-word-forward
   #:copy-mode-word-backward
   #:copy-mode-word-end
   #:copy-mode-space-forward
   #:copy-mode-space-backward
   #:copy-mode-space-end
   ;; Line navigation
   #:copy-mode-line-start
   #:copy-mode-back-to-indentation
   #:copy-mode-line-end
   ;; Jump to top/bottom
   #:copy-mode-top
   #:copy-mode-bottom
   ;; Screen position jumps
   #:copy-mode-high
   #:copy-mode-middle
   #:copy-mode-low
   ;; Page scrolling
   #:copy-mode-page-up
   #:copy-mode-page-down
   #:copy-mode-half-page-up
   #:copy-mode-half-page-down
   #:copy-mode-scroll-up-line
   #:copy-mode-scroll-down-line
   #:copy-mode-scroll-middle
   ;; *-and-cancel / selection-mode / scroll-to-mouse (send-keys -X)
   #:copy-mode-scroll-down-and-cancel
   #:copy-mode-page-down-and-cancel
   #:copy-mode-cursor-down-and-cancel
   #:copy-mode-selection-mode
   #:copy-mode-scroll-to-mouse
   #:copy-mode-previous-paragraph
   #:copy-mode-next-paragraph
   ;; Line selection (V)
   #:copy-mode-begin-line-selection
   ;; Goto absolute line number (send-keys -X goto-line N)
   #:copy-mode-goto-line
   ;; Copy variants
   #:copy-mode-copy-end-of-line
   #:copy-mode-copy-end-of-line-and-cancel
   #:copy-mode-copy-line
   #:copy-mode-copy-line-and-cancel
   ;; Jump-to-char (vi f/F/t/T/;/,)
   #:copy-mode-jump-forward
   #:copy-mode-jump-backward
   #:copy-mode-jump-to
   #:copy-mode-jump-to-backward
   #:copy-mode-jump-again
   #:copy-mode-jump-reverse
   #:*copy-mode-last-jump*
   ;; Search
   #:copy-mode-search-forward
   #:copy-mode-search-backward
   #:copy-mode-search-next
   #:copy-mode-search-prev
   ;; Incremental search (C-s / C-r in copy-mode-vi and copy-mode)
   #:copy-mode-search-forward-incremental
   #:copy-mode-search-backward-incremental
   ;; Bracket matching (vi %)
   #:copy-mode-next-matching-bracket
   #:copy-mode-previous-matching-bracket
   ;; Rectangle select
   #:copy-mode-toggle-rectangle
   ;; Mark management
   #:copy-mode-set-mark
   #:copy-mode-stop-selection
   #:copy-mode-toggle-position
   #:copy-mode-previous-prompt
   #:copy-mode-next-prompt
   #:copy-mode-half-page-down-and-cancel
   #:copy-mode-copy-pipe-end-of-line-no-cancel
   ;; Append selection
   #:copy-mode-append-selection
   #:copy-mode-append-selection-and-cancel
   ;; Copy-pipe (yank + pipe to shell command)
   #:copy-mode-copy-pipe
   #:copy-mode-copy-pipe-no-cancel
   #:copy-mode-copy-pipe-end-of-line
   #:rename-session
   #:run-shell
   #:if-shell
   #:swap-pane
   #:swap-two-panes
   #:capture-pane
   ;; Advanced pane commands
   #:break-pane
   #:join-pane
   #:pipe-pane-open
   #:pipe-pane-close
   #:pipe-pane-write
   ;; send-keys
   #:send-keys-to-pane
   ;; command-string tokeniser (shared lexer for multi-arg commands)
   #:tokenize-command-string))

;;; ── Top-level entry point ────────────────────────────────────────────────

(defpackage #:cl-tmux
  (:use #:cl #:bordeaux-threads
        #:cl-tmux/config
        #:cl-tmux/pty
        #:cl-tmux/terminal
        #:cl-tmux/model
        #:cl-tmux/renderer
        #:cl-tmux/input
        #:cl-tmux/commands
        #:cl-tmux/prompt
        #:cl-tmux/protocol
        #:cl-tmux/transport
        #:cl-tmux/net)
  (:export
   #:main
   ;; Session registry (server)
   #:*server-sessions*
   #:*server-marked-pane*
   #:*client-read-only*
   #:server-add-session
   #:server-find-session
   #:server-remove-session
   #:server-all-sessions
   #:server-current-session
   ;; Repository adapter
   #:install-session-repository
   #:in-memory-session-store
   ;; Session groups
   #:*session-groups*
   #:*group-id-counter*
   #:server-new-session-in-group
   ;; Multi-session commands
   #:new-session
   ;; Session/window/pane targeting (-t flag)
   #:resolve-target
   #:resolve-target-context
   #:find-session-by-target
   #:find-window-by-target
   #:find-pane-by-target
   ;; Wait-for channel synchronization
   #:*wait-channels*
   #:%ensure-channel
   #:wait-for-channel
   #:signal-channel
   #:lock-channel
   #:unlock-channel
   ;; Message log (for :show-messages)
   #:*message-log*
   #:add-message-log
   ;; Prompt history (for :show-prompt-history / :clear-prompt-history)
   #:*prompt-history*
   #:add-prompt-history
   #:save-prompt-history       ; persist *prompt-history* to history-file
   #:load-prompt-history       ; load *prompt-history* from history-file at startup
   ;; Clock mode (for :clock-mode)
   #:*clock-mode-pane-id*
   ;; Reader thread lifecycle
   #:stop-reader-threads
   ;; Status interval timer
   #:start-status-timer
   ;; Named constants
   #:+max-message-log-entries+
   #:+max-prompt-history+))
