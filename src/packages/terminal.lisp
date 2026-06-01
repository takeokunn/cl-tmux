;;;; Terminal emulator sub-packages.

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
   ;; Extended attribute bits (attrs2 slot)
   #:+attr2-double-underline+
   #:+attr2-overline+
   ;; Grid allocation helper
   #:%make-blank-cells
   ;; Cell struct + helpers
   #:cell
   #:make-cell
   #:cell-char
   #:cell-fg
   #:cell-bg
   #:cell-attrs
   #:cell-attrs2
   #:cell-ul-color
   #:cell-combining
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
   ;; Auto-wrap mode (?7h / ?7l)
   #:screen-autowrap
   ;; Active character set (:ascii / :dec-graphics)
   #:screen-charset
   ;; Underline color pen
   #:screen-cur-ul-color
   ;; Extended attribute pen
   #:screen-cur-attrs2
   ;; Response queue (DA1/DA2 and similar replies)
   #:screen-response-queue
   ;; BEL pending flag
   #:screen-bell-pending
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
   #:cursor-cht
   #:cursor-cbt
   #:cursor-bs
   #:cursor-ri
   #:cursor-cr
   #:cursor-down/scroll
   ;; Character writing
   #:write-char-at-cursor
   #:write-codepoint
   #:combining-char-p
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
   ;; Auto-wrap mode
   #:screen-autowrap
   ;; Active character set
   #:screen-charset
   ;; Underline color pen
   #:screen-cur-ul-color
   ;; Extended attribute pen
   #:screen-cur-attrs2
   ;; Response queue (DA1/DA2 etc.)
   #:screen-response-queue
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
   #:cell-attrs2
   #:cell-ul-color
   #:cell-combining
   #:cell-width))
