;;;; Test package for cl-tmux.

(defpackage #:cl-tmux/test
  (:use #:cl #:fiveam)
  (:import-from #:cl-tmux/terminal
                #:make-screen
                #:screen-resize
                #:screen-process-bytes
                #:screen-cell
                #:screen-display-cell
                #:screen-cursor-x
                #:screen-cursor-y
                #:screen-width
                #:screen-height
                #:screen-clear-dirty
                #:cell-char
                #:cell-fg
                #:cell-bg
                #:cell-attrs
                #:cell-width)
  (:import-from #:cl-tmux/terminal/types
                #:screen-copy-mode-p
                #:screen-copy-offset
                #:screen-scrollback
                #:screen-copy-selecting
                #:screen-copy-mark
                #:screen-copy-cursor
                #:screen-mouse-mode
                #:screen-mouse-sgr-mode
                #:screen-title
                #:char-width)
  (:import-from #:cl-tmux/model
                #:create-initial-session
                #:session-windows
                #:session-active-window
                #:session-select-window
                #:session-new-window
                #:session-active-pane
                #:window-panes
                #:window-active-pane
                #:window-select-pane
                #:window-split
                #:window-relayout
                #:window-remove-pane
                #:window-resize-active
                #:window-refresh-panes
                #:window-tree
                #:make-layout-leaf
                #:make-layout-split
                #:layout-leaf-pane
                #:layout-leaves
                #:layout-find-leaf
                #:layout-find-parent
                #:all-panes
                #:make-pane
                #:make-window
                #:make-session
                #:pane-feed
                #:pane-screen
                #:window-id
                #:window-name
                #:window-width #:window-height
                #:session-name
                #:pane-id
                #:pane-x #:pane-y #:pane-width #:pane-height #:pane-fd #:pane-pid
                #:pane-neighbor
                #:pane-at-position
                #:apply-named-layout
                #:window-lock
                ;; New window management
                #:window-last-active-time
                #:window-automatic-rename-p
                #:window-rotate
                #:session-last-window
                #:session-move-window
                #:session-swap-windows
                ;; Pane management
                #:window-last-active
                #:respawn-pane
                ;; New pane slots
                #:pane-pipe-fd
                #:pane-window
                #:pane-marked
                ;; New session slots
                #:session-locked-p
                #:session-group
                #:session-clients
                ;; Window layout-cycle-index slot
                #:window-layout-cycle-index
                ;; Layout persistence
                #:layout->string
                #:string->layout
                ;; update-environment
                #:*update-environment*
                #:get-update-environment-vars
                ;; Session name / id
                #:session-id
                #:session-last-active
                #:session-touch
                ;; Pane geometry (direct reposition)
                #:pane-reposition
                ;; Session window management
                #:session-insert-window)
  (:import-from #:cl-tmux
                ;; Session groups
                #:*session-groups*
                #:server-new-session-in-group
                ;; Runtime state (needed by tests)
                #:*server-sessions*)
  (:import-from #:cl-tmux/renderer
                #:render-session-to-string
                #:render-session
                #:clear-display)
  (:import-from #:cl-tmux/protocol
                #:+msg-attach+ #:+msg-key+ #:+msg-resize+
                #:+msg-detach+ #:+msg-frame+ #:+msg-bye+ #:+msg-command+ #:+header-size+
                #:encode-frame #:decode-frame
                #:msg-attach #:msg-key #:msg-resize #:msg-detach #:msg-frame #:msg-bye
                #:msg-command
                #:decode-size #:decode-text #:to-octets)
  (:import-from #:cl-tmux/transport
                #:send-frame #:read-frame #:with-incoming-frame)
  (:import-from #:cl-tmux/net
                #:make-listener #:accept-connection #:connect-to
                #:socket-stream #:socket-fd #:close-socket
                #:unix-socket-available-p)
  (:import-from #:cl-tmux/config
                #:*status-height*
                #:+max-scrollback-lines+
                #:lookup-key-binding
                #:define-initial-key-bindings
                #:key-table-bind
                #:key-table-command
                #:*key-tables*)
  (:import-from #:cl-tmux/commands
                #:kill-pane
                #:kill-window
                #:rename-window
                #:resize-pane
                #:select-window-by-number
                #:swap-pane
                #:capture-pane
                ;; Advanced pane commands
                #:break-pane
                #:join-pane
                #:pipe-pane-open
                #:pipe-pane-close
                #:pipe-pane-write)
  (:import-from #:cl-tmux/prompt
                #:*prompt*
                #:prompt-active-p
                #:prompt-start
                #:prompt-input
                #:prompt-backspace
                #:prompt-clear
                #:prompt-text
                #:*overlay*
                #:*overlay-scroll-offset*
                #:overlay-active-p
                #:show-overlay
                #:clear-overlay
                #:overlay-lines
                #:overlay-scroll
                #:popup #:make-popup #:popup-p
                #:popup-x #:popup-y #:popup-width #:popup-height
                #:popup-screen #:popup-pane #:popup-title #:popup-close-on-exit
                #:*active-popup*
                #:menu #:make-menu #:menu-p
                #:menu-title #:menu-items #:menu-selected-index
                #:*active-menu*
                #:prompt-label
                #:prompt-buffer
                #:prompt-on-submit
                #:prompt-cursor-index
                #:prompt-cursor-bol
                #:prompt-cursor-eol
                #:prompt-cursor-back
                #:prompt-cursor-forward
                #:prompt-kill-to-end
                #:prompt-kill-to-start
                #:prompt-kill-word-back)
  (:import-from #:cl-tmux/pty
                #:forkpty-with-shell
                #:pty-write
                #:pty-read-blocking
                #:pty-close
                #:select-fds)
  (:export #:run-tests
           #:cl-tmux-suite))
