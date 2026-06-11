;;;; Package definitions for cl-tmux.
;;;; All package declarations live here so cross-package dependencies are explicit.

(defpackage #:cl-tmux/config
  (:use #:cl)
  (:export
   #:+prefix-key-code+
   #:+ctrl-mask+
   ;; Standard key-table name constants
   #:+table-prefix+
   #:+table-root+
   #:+table-copy-mode+
   #:*default-shell*
   #:*status-height*
   #:+pty-buf-size+
   #:+max-scrollback-lines+
   #:+poll-timeout-us+
   #:+accept-timeout-us+
   #:+pty-poll-timeout-us+
   #:define-initial-key-bindings
   #:lookup-key-binding
   #:describe-key-bindings
   #:describe-key-bindings-for-table
   #:set-key-binding
   #:remove-key-binding
   ;; Key-table system
   #:*key-tables*
   #:ensure-key-table
   #:key-table-bind
   #:key-table-lookup
   #:key-table-command
   #:key-table-repeatable-p
   #:key-table-note
   #:initialize-default-key-tables
   #:load-config-file
   #:load-config-from-stream
   #:load-config-from-string
   #:source-files
   #:apply-config-directive
   #:%apply-option-side-effects
   #:config-file-path
   ;; %if condition evaluator hook (set by top-level package)
   #:*config-condition-evaluator*
   ;; Mouse-reporting callback hook (set by orchestrate/events-loop layer)
   #:*mouse-reporting-hook*
   ;; Dynamic prefix key (primary and secondary)
   #:*prefix-key-code*
   #:*prefix2-key-code*
   #:%parse-prefix-key
   ;; ORCHESTRATE-layer shell initializer
   #:init-default-shell))

(defpackage #:cl-tmux/pty
  (:use #:cl #:cffi)
  (:export
   ;; PTY lifecycle
   #:forkpty-with-shell    ; (rows cols) → (values master-fd child-pid slave-path)
   #:pty-write             ; (fd data)   — write octets/string to PTY
   #:pty-read-blocking     ; (fd size)   → octet-vector or nil on EOF
   #:pty-close             ; (fd pid)
   #:set-pty-size          ; (fd rows cols)
   ;; Terminal raw mode
   #:enable-raw-mode!      ; (fd)
   #:disable-raw-mode!     ; (fd)
   ;; Multiplexed I/O
   #:select-fds            ; (fds timeout-us) → ready-fd-list
   ;; Terminal geometry
   #:terminal-size))       ; () → (values rows cols)

;;; ── Client/server wire protocol ──────────────────────────────────────────

(defpackage #:cl-tmux/protocol
  (:use #:cl)
  (:export
   ;; Message type tags + header size
   #:+msg-attach+ #:+msg-key+ #:+msg-resize+ #:+msg-detach+ #:+msg-frame+ #:+msg-bye+
   #:+msg-command+ #:+msg-reply+
   #:+header-size+
   ;; Frame codec
   #:encode-frame #:decode-frame
   ;; Typed message constructors
   #:msg-attach #:msg-key #:msg-resize #:msg-detach #:msg-frame #:msg-bye
   #:msg-command #:msg-reply
   ;; Command message codec
   #:encode-command-payload #:decode-command-payload #:target-field-p
   ;; Command payload helpers — exported as stable API so tests use single-colon access
   #:split-on-nul-bytes #:command-name-to-string
   #:assemble-command-fields #:encode-fields-to-buffer
   ;; Payload decoders + octet helpers
   #:decode-size #:decode-text #:to-octets
   ;; Integer codec helpers (exported so tests can use single-colon access)
   #:u16-octets #:u32-octets #:u16-octets-pair #:read-u16 #:read-u32))

;;; ── Client/server stream transport ───────────────────────────────────────

(defpackage #:cl-tmux/transport
  (:use #:cl #:cl-tmux/protocol)
  (:export
   #:send-frame            ; (stream octets)          — write one frame + flush
   #:read-frame            ; (stream) → (values type payload) or NIL at EOF
   #:with-incoming-frame)) ; macro — read + Prolog-dispatch one frame from a stream

(defpackage #:cl-tmux/net
  (:use #:cl)
  (:export
   #:make-listener #:accept-connection #:connect-to
   #:socket-stream #:socket-fd #:close-socket
   #:unix-socket-available-p))

;;; ── Terminal sub-packages ────────────────────────────────────────────────

(defpackage #:cl-tmux/terminal/types
  (:use #:cl #:bordeaux-threads)
  (:export
   ;; Attribute bit constants (LSB first, matching bit layout in cell.lisp)
   #:+attr-bold+
   #:+attr-dim+
   #:+attr-reverse+
   #:+attr-underline+
   #:+attr-blink+
   #:+attr-italic+
   #:+attr-conceal+
   #:+attr-strikethrough+
   ;; Extended attribute bits (attrs2 slot: double-underline, overline)
   #:+attr2-double-underline+
   #:+attr2-overline+
   ;; True-colour encoding sentinel (bit 24 of a colour slot)
   #:+true-color-flag+
   ;; XTPUSHTITLE/XTPOPTITLE stack depth limit (matches xterm)
   #:+title-stack-max-depth+
   ;; Grid allocation helper
   #:%make-blank-cells
   ;; Cell struct + helpers
   #:cell
   #:make-cell
   #:cell-p
   #:cell-char
   #:cell-fg
   #:cell-bg
   #:cell-attrs
   #:cell-attrs2
   #:cell-ul-color
   #:cell-combining
   #:cell-width
   #:cell-hyperlink
   #:blank-cell
   #:clamp
   #:safe-code-char
   #:char-width
   #:define-wide-char-ranges
   ;; Screen struct + constructors
   #:screen
   #:%make-screen
   #:make-screen
   #:screen-p
   ;; Geometry / cursor / SGR-state accessors
   #:screen-width
   #:screen-height
   #:screen-cells
   #:screen-cursor-x
   #:screen-cursor-y
   #:screen-cur-fg
   #:screen-cur-bg
   #:screen-cur-attrs
   #:screen-cur-attrs2
   #:screen-cur-ul-color
   #:screen-scroll-top
   #:screen-scroll-bottom
   #:screen-parser
   #:screen-dirty-p
   #:screen-lock
   ;; Alternate-screen save slots
   #:screen-alt-cells
   #:screen-alt-cursor-x
   #:screen-alt-cursor-y
   ;; DECSC/DECRC saved cursor
   #:screen-saved-cursor
   ;; DECTCEM cursor visibility
   #:screen-cursor-visible
   ;; Copy / scrollback mode
   #:screen-copy-mode-p
   #:screen-copy-offset
   #:screen-scrollback
   ;; Copy-mode selection state
   #:screen-copy-mark
   #:screen-copy-mark-offset
   #:screen-copy-cursor
   #:screen-copy-selecting
   ;; copy-mode -e: auto-exit when scrolled to live bottom
   #:screen-copy-exit-on-bottom
   ;; REP (repeat preceding char) support
   #:screen-last-char
   ;; DECSCUSR cursor shape
   #:screen-cursor-shape
   ;; BEL pending flag
   #:screen-bell-pending
   ;; Copy-mode search term (/ ? n N)
   #:screen-copy-search-term
   ;; Copy-mode line-selection flag (V)
   #:screen-copy-line-selection-p
   ;; Copy-mode rectangle-select flag (r)
   #:screen-copy-rect-select-p
   ;; IRM insert/replace mode
   #:screen-insert-mode
   #:screen-newline-mode
   #:screen-reverse-screen
   ;; Bracketed paste mode
   #:screen-bracketed-paste
   ;; Application cursor keys
   #:screen-app-cursor-keys
   ;; OSC 0/2 window title + XTPUSHTITLE/XTPOPTITLE stack
   #:screen-title
   #:screen-title-stack
   ;; OSC 7 current working directory
   #:screen-cwd
   ;; Mouse reporting mode
   #:screen-mouse-mode
   #:screen-mouse-sgr-mode
   ;; Auto-wrap mode (?7h / ?7l)
   #:screen-autowrap
   #:screen-pending-wrap
   #:screen-origin-mode
   ;; Focus event reporting (?1004h / ?1004l)
   #:screen-focus-events
   ;; Active character set (:ascii / :dec-graphics) + VT100 G0/G1 + SO/SI state
   #:screen-charset
   #:screen-g0-charset
   #:screen-g1-charset
   #:screen-active-g
   #:screen-tab-stops
   ;; Response queue for DA1/DA2 and similar replies
   #:screen-response-queue
   #:screen-passthrough-queue
   #:screen-clipboard-queue
   ;; OSC 10/11 default foreground/background colours
   #:screen-osc-default-fg
   #:screen-osc-default-bg
   ;; OSC 8 current hyperlink
   #:screen-current-hyperlink
   ;; Line-wrap flags (capture-pane -J)
   #:screen-wrapped-rows
   #:%mark-line-wrapped
   #:%line-wrapped-p
   #:%clear-line-wrapped
   #:%clear-all-line-wrapped
   #:%shift-line-wrapped-up
   ;; Grid helpers
   #:screen-cell
   #:screen-clear-dirty
   #:screen-resize
   ;; Bell consumption (logic layer — consume and clear bell-pending atomically)
   #:screen-consume-bell
   ;; SGR pen reset (canonical, data layer; shared by actions and sgr layers)
   #:reset-sgr-pen))

(defpackage #:cl-tmux/terminal/actions
  (:use #:cl #:cl-tmux/terminal/types)
  (:export
   ;; Cursor movement
   #:cursor-up
   #:cursor-down
   #:cursor-right
   #:cursor-left
   #:set-cursor
   #:cursor-lf
   #:cursor-nl
   #:cursor-ht
   #:cursor-cht
   #:cursor-cbt
   #:set-tab-stop
   #:clear-tab-stops
   #:cursor-bs
   #:cursor-ri
   #:cursor-cr
   #:cursor-nel
   #:cursor-down/scroll
   ;; Character writing
   #:write-char-at-cursor
   #:write-codepoint
   #:combining-char-p
   ;; Scroll
   #:scroll-up-one
   #:scroll-down-one
   #:scroll-screen-to-history
   #:trim-scroll-history
   #:clear-scrollback
   #:*history-limit-function*
   #:*alternate-screen-enabled-function*
   #:*scroll-on-clear-function*
   ;; Focus event reporting (?1004)
   #:focus-event-report
   ;; Erase
   #:erase-region
   #:erase-display
   #:erase-line
   ;; DEC Rectangle operations (DECERA/DECFRA/DECCRA)
   #:decera
   #:decfra
   #:deccra
   ;; Edit (insert/delete characters and lines)
   #:delete-chars
   #:insert-chars
   #:insert-lines
   #:delete-lines
   ;; Scroll region + DEC private modes + reset
   #:decstbm
   #:dec-pm-set
   #:dec-pm-reset
   #:enter-alt-screen
   #:exit-alt-screen
   #:reset-terminal-modes
   #:ris-action
   #:decstr-action
   #:decaln-action
   ;; DECSC / DECRC cursor save & restore
   #:save-cursor
   #:restore-cursor
   ;; Display projection (copy-mode scrollback)
   #:screen-display-cell
   ;; Terminal state action helpers (DISPATCH layer calls these instead of mutating structs)
   #:set-cursor-shape
   #:set-bell-pending
   #:set-ansi-mode
   #:reset-ansi-mode
   #:set-charset
   #:designate-charset
   #:invoke-charset
   #:screen-invoked-charset
   #:set-screen-title
   #:set-screen-cwd))

;; sgr package: apply-sgr + the inverse %pen-to-sgr-params (DECRQSS reports)
(defpackage #:cl-tmux/terminal/sgr
  (:use #:cl #:cl-tmux/terminal/types)
  (:export
   #:%dispatch-sgr-code
   #:apply-sgr
   #:%pen-to-sgr-params))

(defpackage #:cl-tmux/terminal/csi
  (:use #:cl
        #:cl-tmux/terminal/types
        #:cl-tmux/terminal/actions
        #:cl-tmux/terminal/sgr)
  (:export
   #:execute-csi))

(defpackage #:cl-tmux/terminal/parser
  (:use #:cl
        #:cl-tmux/terminal/types
        #:cl-tmux/terminal/actions
        #:cl-tmux/terminal/csi)
  ;; cl-tmux/buffer is used for OSC 52 clipboard paste storage.
  ;; We reference it by qualified name to avoid circular deps.
  (:export
   #:ground-state
   #:escape-state
   #:make-csi-k
   #:make-utf8-k
   #:osc-state
   #:make-charset-designator-k
   #:*osc52-handler*
   #:osc52-clipboard-sequence))

(defpackage #:cl-tmux/terminal/emulator
  (:use #:cl
        #:cl-tmux/terminal/types)
  (:export
   #:screen-process-bytes))

;;; ── Terminal umbrella (re-export facade) ─────────────────────────────────

(defpackage #:cl-tmux/terminal
  (:use #:cl #:bordeaux-threads
        #:cl-tmux/terminal/types
        #:cl-tmux/terminal/actions
        #:cl-tmux/terminal/sgr
        #:cl-tmux/terminal/csi
        #:cl-tmux/terminal/parser
        #:cl-tmux/terminal/emulator)
  (:export
   ;; Construction
   #:make-screen
   ;; Geometry
   #:screen-width
   #:screen-height
   ;; Cursor
   #:screen-cursor-x
   #:screen-cursor-y
   ;; Dirty flag
   #:screen-dirty-p
   #:screen-clear-dirty
   ;; DECTCEM cursor visibility
   #:screen-cursor-visible
   ;; DECSCUSR cursor shape
   #:screen-cursor-shape
   ;; IRM insert/replace mode
   #:screen-insert-mode
   #:screen-newline-mode
   #:screen-reverse-screen
   ;; Bracketed paste mode
   #:screen-bracketed-paste
   ;; Application cursor keys
   #:screen-app-cursor-keys
   ;; OSC 0/2 window title
   #:screen-title
   ;; OSC 7 current working directory
   #:screen-cwd
   ;; Mouse reporting mode
   #:screen-mouse-mode
   #:screen-mouse-sgr-mode
   ;; Lock (for renderer <-> reader-thread synchronisation)
   #:screen-lock
   ;; Resize the grid in place
   #:screen-resize
   ;; Feed raw PTY bytes into the emulator
   #:screen-process-bytes
   ;; Grid access
   #:screen-cell
   ;; Viewport projection honoring copy-mode scrollback
   #:screen-display-cell
   ;; Copy / scrollback mode (used by renderer status bar + commands)
   #:screen-copy-mode-p
   #:screen-copy-offset
   #:screen-scrollback
   ;; Copy-mode selection state
   #:screen-copy-mark
   #:screen-copy-mark-offset
   #:screen-copy-cursor
   #:screen-copy-selecting
   ;; copy-mode -e: auto-exit when scrolled to live bottom
   #:screen-copy-exit-on-bottom
   ;; Cell accessors
   #:cell-char
   #:cell-fg
   #:cell-bg
   #:cell-attrs
   #:cell-attrs2
   #:cell-ul-color
   #:cell-combining
   #:cell-width
   #:cell-hyperlink
   ;; Auto-wrap mode
   #:screen-autowrap
   #:screen-pending-wrap
   ;; Active character set
   #:screen-charset
   ;; SGR pen extras
   #:screen-cur-attrs2
   #:screen-cur-ul-color
   ;; Response queue
   #:screen-response-queue
   #:screen-passthrough-queue
   #:screen-clipboard-queue
   ;; Combining char predicate
   #:combining-char-p
   ;; BEL pending flag
   #:screen-bell-pending
   ;; Bell consumption (logic layer)
   #:screen-consume-bell
   ;; Copy-mode search term
   #:screen-copy-search-term
   ;; Copy-mode line-selection flag
   #:screen-copy-line-selection-p
   ;; Copy-mode rectangle-select flag
   #:screen-copy-rect-select-p
   ;; Scroll history limit callback (injected at startup)
   #:*history-limit-function*
   #:*alternate-screen-enabled-function*
   #:*scroll-on-clear-function*))

(defpackage #:cl-tmux/prompt
  (:use #:cl)
  (:export
   #:prompt #:make-prompt #:prompt-p
   #:prompt-label #:prompt-buffer #:prompt-cursor-index #:prompt-on-submit
   #:prompt-vi-normal-p #:prompt-single-key
   #:*prompt* #:prompt-active-p #:prompt-start
   #:prompt-input #:prompt-backspace #:prompt-clear #:prompt-text
   #:prompt-notify-change
   ;; Cursor navigation
   #:prompt-cursor-bol #:prompt-cursor-eol
   #:prompt-cursor-back #:prompt-cursor-forward
   ;; Kill commands
   #:prompt-kill-to-end #:prompt-kill-to-start #:prompt-kill-word-back
   ;; Vi-mode character deletion (vi x)
   #:prompt-delete-char
   ;; Dismissible overlay (list-keys help, …)
   #:*overlay* #:*overlay-scroll-offset* #:*display-panes-active*
   #:overlay-active-p #:overlay-shown-at #:show-overlay #:show-transient-overlay
   #:clear-overlay #:overlay-lines
   #:overlay-scroll #:*overlay-shown-at*
   ;; Popup overlay
   #:+default-popup-width+ #:+default-popup-height+
   #:popup #:make-popup #:popup-p
   #:popup-x #:popup-y #:popup-width #:popup-height
   #:popup-screen #:popup-pane #:popup-title #:popup-close-on-exit
   #:*active-popup*
   #:show-popup #:close-popup #:popup-active-p
   ;; Menu overlay
   #:menu #:make-menu #:menu-p
   #:menu-title #:menu-items #:menu-selected-index
   #:menu-x #:menu-y
   #:*active-menu*
   #:show-menu #:close-menu #:menu-active-p))

;;; ── Model / renderer / input ─────────────────────────────────────────────

(defpackage #:cl-tmux/model
  (:use #:cl #:bordeaux-threads
        #:cl-tmux/config #:cl-tmux/pty #:cl-tmux/terminal)
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
   #:pane-window
   #:pane-marked
   #:pane-title
   #:pane-tty
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
   #:%assign-window-tree    ; (window w h) → layout-assign with top-status y-offset
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
   #:layout->string #:string->layout
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
   ;; Silence tracking for monitor-silence
   #:window-last-output-time
   #:window-silence-flag
   ;; Per-fork extra environment injection (new-window -e / split-window -e)
   #:*pane-extra-env*
   ;; Layout cycle index (for C-b Space next-layout)
   #:window-layout-cycle-index
   ;; Rotate-window
   #:window-rotate
   ;; Session
   #:session
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
   #:session-active-pane
   #:session-last-active
   #:session-clients
   #:session-locked-p
   #:session-group
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
   ;; update-environment
   #:*update-environment*
   #:get-update-environment-vars))

(defpackage #:cl-tmux/format
  (:use #:cl #:cl-tmux/model)
  (:export #:expand-format #:format-context-from-session #:format-context-from-window))

(defpackage #:cl-tmux/buffer
  (:use #:cl)
  (:export #:+default-buffer-limit+
           #:*paste-buffers* #:*buffer-auto-index*
           #:add-paste-buffer #:get-paste-buffer #:set-named-buffer
           #:get-buffer-by-name #:buffer-names
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
   ;; Registry and dispatch
   #:*hook-registry*
   #:add-hook
   #:remove-hook
   #:run-hooks
   #:clear-hooks
   #:list-hooks
   #:*command-hooks*
   #:set-command-hook
   #:command-hooks
   #:clear-command-hooks
   #:list-command-hooks
   #:describe-command-hooks
   #:*command-hook-runner*
   #:run-command-hooks-via-runner))

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
           #:style-option-p #:append-option-value
           ;; Server options
           #:*server-options* #:*server-option-registry*
           #:define-server-options
           #:get-server-option #:set-server-option
           ;; Scoped accessors (per-window / per-pane)
           #:get-option-for-window #:set-option-for-window
           #:get-option-for-pane   #:set-option-for-pane
           #:get-option-for-context
           ;; show-options helpers
           #:show-options #:show-option
           ;; Command-alias registry
           #:*command-aliases*
           #:register-command-alias
           #:lookup-command-alias
           #:list-command-aliases))

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
   #:copy-mode-previous-paragraph
   #:copy-mode-next-paragraph
   ;; Line selection (V)
   #:copy-mode-begin-line-selection
   ;; Goto absolute line number (send-keys -X goto-line N)
   #:copy-mode-goto-line
   ;; Copy variants
   #:copy-mode-copy-end-of-line
   #:copy-mode-copy-line
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
   ;; Rectangle select
   #:copy-mode-toggle-rectangle
   ;; Mark management
   #:copy-mode-set-mark
   ;; Append selection
   #:copy-mode-append-selection
   #:copy-mode-append-selection-and-cancel
   ;; Copy-pipe (yank + pipe to shell command)
   #:copy-mode-copy-pipe
   #:copy-mode-copy-pipe-no-cancel
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
   ;; Session groups
   #:*session-groups*
   #:*group-id-counter*
   #:server-new-session-in-group
   ;; Multi-session commands
   #:new-session
   ;; Session/window/pane targeting (-t flag)
   #:resolve-target
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
