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
   ;; Default-colour sentinel (SGR 39/49; distinct from palette 7/0)
   #:+default-color+
   ;; XTPUSHTITLE/XTPOPTITLE stack depth limit (matches xterm)
   #:+title-stack-max-depth+
   ;; Default terminal geometry constants (VT100 standard)
   #:+default-screen-width+
   #:+default-screen-height+
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
   ;; Mouse-entered copy-mode suppresses gutter line numbers
   #:screen-copy-mode-entered-by-mouse-p
   ;; REP (repeat preceding char) support
   #:screen-last-char
   ;; DECSCUSR cursor shape
   #:screen-cursor-shape
   ;; BEL pending flag
   #:screen-bell-pending
   ;; Copy-mode search term (/ ? n N)
   #:screen-copy-search-term
   #:screen-copy-search-direction
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
   ;; OSC 10/11 default foreground/background colour constants (data layer)
   #:+osc-default-fg+
   #:+osc-default-bg+
   ;; OSC 10/11 default foreground/background colours
   #:screen-osc-default-fg
   #:screen-osc-default-bg
   ;; OSC 8 current hyperlink
   #:screen-current-hyperlink
   ;; OSC 4 / OSC 104 custom palette overrides
   #:screen-palette-overrides
   #:%palette-override-get
   #:%palette-override-set
   #:%palette-override-clear
   #:%palette-override-clear-all
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
   #:push-title-stack
   #:pop-title-stack
   #:reset-osc-default-fg
   #:reset-osc-default-bg
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
   ;; Mouse-entered copy-mode suppresses gutter line numbers
   #:screen-copy-mode-entered-by-mouse-p
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
   #:screen-copy-search-direction
   ;; Copy-mode line-selection flag
   #:screen-copy-line-selection-p
   ;; Copy-mode rectangle-select flag
   #:screen-copy-rect-select-p
   ;; Scroll history limit callback (injected at startup)
   #:*history-limit-function*
   #:*alternate-screen-enabled-function*
   #:*scroll-on-clear-function*))
