;;;; Package definitions for cl-tmux.
;;;; All package declarations live here so cross-package dependencies are explicit.

(defpackage #:cl-tmux/config
  (:use #:cl)
  (:export
   #:+prefix-key-code+
   #:*default-shell*
   #:*status-height*
   #:+pty-buf-size+
   #:*key-bindings*))

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
   #:terminal-size))       ; () → (values rows cols)

(defpackage #:cl-tmux/terminal
  (:use #:cl #:bordeaux-threads)
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
   ;; Lock (for renderer ↔ reader-thread synchronisation)
   #:screen-lock
   ;; Feed raw PTY bytes into the emulator
   #:screen-process-bytes
   ;; Grid access
   #:screen-cell
   ;; Cell accessors
   #:cell-char
   #:cell-fg
   #:cell-bg
   #:cell-attrs))

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
   #:window-active-pane
   #:window-select-pane
   #:window-split
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
   ;; Global state
   #:*current-session*
   #:create-initial-session
   #:all-panes))

(defpackage #:cl-tmux/renderer
  (:use #:cl #:bordeaux-threads
        #:cl-tmux/model #:cl-tmux/terminal)
  (:export
   #:render-session       ; (session rows cols)
   #:clear-display))

(defpackage #:cl-tmux/input
  (:use #:cl #:cffi
        #:cl-tmux/config #:cl-tmux/pty)
  (:export
   #:with-raw-mode        ; macro — raw mode for body, restores on exit
   #:read-byte-nonblock)) ; (&optional timeout-us) → byte or nil

(defpackage #:cl-tmux
  (:use #:cl #:bordeaux-threads
        #:cl-tmux/config
        #:cl-tmux/pty
        #:cl-tmux/terminal
        #:cl-tmux/model
        #:cl-tmux/renderer
        #:cl-tmux/input)
  (:export
   #:main))
