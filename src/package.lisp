;;;; Package definitions for cl-tmux.
;;;; All package declarations live here so cross-package dependencies are explicit.

(defpackage #:cl-tmux/config
  (:use #:cl)
  (:export
   #:+prefix-key-code+
   #:*default-shell*
   #:*status-height*
   #:+pty-buf-size+
   #:+max-scrollback-lines+
   #:+poll-timeout-us+
   #:+accept-timeout-us+
   #:+pty-poll-timeout-us+
   #:define-initial-key-bindings
   #:*key-bindings*
   #:lookup-key-binding
   #:describe-key-bindings
   #:set-key-binding
   #:remove-key-binding
   #:load-config-file
   #:load-config-from-stream
   #:load-config-from-string
   #:apply-config-directive
   #:config-file-path))

(defpackage #:cl-tmux/pty
  (:use #:cl #:cffi)
  (:export
   ;; PTY lifecycle
   #:forkpty-with-shell    ; (rows cols) → (values master-fd child-pid)
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
   #:terminal-size         ; () → (values rows cols)
   ;; Availability probe
   #:pty-available-p))     ; () → boolean

;;; ── Client/server wire protocol ──────────────────────────────────────────

(defpackage #:cl-tmux/protocol
  (:use #:cl)
  (:export
   ;; Message type tags + header size
   #:+msg-attach+ #:+msg-key+ #:+msg-resize+ #:+msg-detach+ #:+msg-frame+ #:+msg-bye+
   #:+header-size+
   ;; Frame codec
   #:encode-frame #:decode-frame
   ;; Typed message constructors
   #:msg-attach #:msg-key #:msg-resize #:msg-detach #:msg-frame #:msg-bye
   ;; Payload decoders + octet helpers
   #:decode-size #:decode-text #:to-octets #:read-u32))

;;; ── Client/server stream transport ───────────────────────────────────────

(defpackage #:cl-tmux/transport
  (:use #:cl #:cl-tmux/protocol)
  (:export
   #:send-frame          ; (stream octets)        — write one frame + flush
   #:read-frame))        ; (stream) → (values type payload) or NIL at EOF

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
   ;; Grid allocation helper
   #:%make-blank-cells
   ;; Cell struct + helpers
   #:cell
   #:make-cell
   #:cell-char
   #:cell-fg
   #:cell-bg
   #:cell-attrs
   #:cell-width
   #:blank-cell
   #:clamp
   #:safe-code-char
   #:char-width
   ;; Screen struct + constructors
   #:screen
   #:%make-screen
   #:make-screen
   ;; Geometry / cursor / SGR-state accessors
   #:screen-width
   #:screen-height
   #:screen-cells
   #:screen-cx
   #:screen-cy
   #:screen-cur-fg
   #:screen-cur-bg
   #:screen-cur-attrs
   #:screen-scroll-top
   #:screen-scroll-bottom
   #:screen-parser
   #:screen-dirty-p
   #:screen-lock
   ;; Alternate-screen save slots
   #:screen-alt-cells
   #:screen-alt-cx
   #:screen-alt-cy
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
   #:screen-copy-cursor
   #:screen-copy-selecting
   ;; REP (repeat preceding char) support
   #:screen-last-char
   ;; DECSCUSR cursor shape
   #:screen-cursor-shape
   ;; Bracketed paste mode
   #:screen-bracketed-paste
   ;; Application cursor keys
   #:screen-app-cursor-keys
   ;; OSC 0/2 window title
   #:screen-title
   ;; Mouse reporting mode
   #:screen-mouse-mode
   #:screen-mouse-sgr-mode
   ;; Cursor wrappers + grid helpers
   #:screen-cursor-x
   #:screen-cursor-y
   #:screen-cell
   #:screen-clear-dirty
   #:screen-resize))

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
   #:cursor-ht
   #:cursor-bs
   #:cursor-ri
   #:cursor-cr
   #:cursor-down/scroll
   ;; Character writing
   #:write-char-at-cursor
   #:write-codepoint
   ;; Scroll
   #:scroll-up-one
   #:scroll-down-one
   ;; Erase
   #:erase-region
   #:erase-display
   #:erase-line
   ;; Edit (insert/delete characters and lines)
   #:delete-chars
   #:insert-chars
   #:insert-lines
   #:delete-lines
   ;; Scroll region + DEC private modes + reset
   #:decstbm
   #:dec-pm-set
   #:dec-pm-reset
   #:ris-action
   ;; DECSC / DECRC cursor save & restore
   #:save-cursor
   #:restore-cursor
   ;; Display projection (copy-mode scrollback)
   #:screen-display-cell))

(defpackage #:cl-tmux/terminal/sgr
  (:use #:cl #:cl-tmux/terminal/types)
  (:export
   #:define-sgr-rules
   #:%dispatch-sgr-code
   #:apply-sgr))

(defpackage #:cl-tmux/terminal/csi
  (:use #:cl
        #:cl-tmux/terminal/types
        #:cl-tmux/terminal/actions
        #:cl-tmux/terminal/sgr)
  (:export
   #:define-csi-rules
   #:execute-csi))

(defpackage #:cl-tmux/terminal/parser
  (:use #:cl
        #:cl-tmux/terminal/types
        #:cl-tmux/terminal/actions
        #:cl-tmux/terminal/csi)
  (:export
   #:ground-state
   #:escape-state
   #:make-csi-k
   #:make-utf8-k
   #:osc-state
   #:charset-state))

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
   ;; Bracketed paste mode
   #:screen-bracketed-paste
   ;; Application cursor keys
   #:screen-app-cursor-keys
   ;; OSC 0/2 window title
   #:screen-title
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
   #:screen-copy-cursor
   #:screen-copy-selecting
   ;; Cell accessors
   #:cell-char
   #:cell-fg
   #:cell-bg
   #:cell-attrs
   #:cell-width))

(defpackage #:cl-tmux/prompt
  (:use #:cl)
  (:export
   #:prompt #:make-prompt #:prompt-p
   #:prompt-label #:prompt-buffer #:prompt-on-submit
   #:*prompt* #:prompt-active-p #:prompt-start
   #:prompt-input #:prompt-backspace #:prompt-clear #:prompt-text
   ;; Dismissible overlay (list-keys help, …)
   #:*overlay* #:overlay-active-p #:show-overlay #:clear-overlay #:overlay-lines))

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
   ;; Window
   #:window
   #:make-window
   #:window-id
   #:window-name
   #:window-width #:window-height
   #:window-panes
   #:window-tree
   #:window-active-pane
   #:window-select-pane
   #:window-split
   #:window-relayout
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
   ;; Zoom
   #:window-zoom-p
   #:window-zoom-tree
   #:window-zoom-toggle
   #:window-lock
   ;; Last-active pane (for C-b ;)
   #:window-last-active
   ;; Session
   #:session
   #:make-session
   #:session-id
   #:session-name
   #:session-windows
   #:session-active-window
   #:session-select-window
   #:session-new-window
   #:session-active-pane
   ;; Pane hit testing
   #:pane-at-position
   ;; Named layouts
   #:apply-named-layout
   ;; Global state
   #:create-initial-session
   #:all-panes))

(defpackage #:cl-tmux/format
  (:use #:cl #:cl-tmux/model)
  (:export #:expand-format #:format-context-from-session))

(defpackage #:cl-tmux/buffer
  (:use #:cl)
  (:export #:*paste-buffers* #:add-paste-buffer #:get-paste-buffer
           #:list-paste-buffers #:delete-paste-buffer #:clear-paste-buffers))

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
   ;; Macro
   #:define-hook-events
   ;; Registry and dispatch
   #:*hook-registry*
   #:add-hook
   #:remove-hook
   #:run-hooks
   #:clear-hooks
   #:list-hooks))

(defpackage #:cl-tmux/options
  (:use #:cl)
  (:export #:*global-options* #:*option-registry*
           #:option-spec #:make-option-spec
           #:option-spec-name #:option-spec-type #:option-spec-default
           #:define-tmux-options #:get-option #:set-option
           #:option-defined-p #:all-options))

(defpackage #:cl-tmux/renderer
  (:use #:cl #:bordeaux-threads
        #:cl-tmux/model #:cl-tmux/terminal #:cl-tmux/prompt)
  (:export
   #:render-session            ; (session rows cols) → write a frame to stdout
   #:render-session-to-string  ; (session rows cols) → frame string (server uses this)
   #:clear-display))

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
   #:copy-mode-begin-selection
   #:copy-mode-cancel-selection
   #:copy-mode-yank
   #:rename-session
   #:run-shell
   #:if-shell
   #:swap-pane
   #:capture-pane))

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
   #:server-add-session
   #:server-find-session
   #:server-remove-session
   #:server-all-sessions
   ;; Multi-session commands
   #:new-session))
