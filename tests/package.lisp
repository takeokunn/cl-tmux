;;;; Test package for cl-tmux.

(defpackage #:cl-tmux/test
  ;; The test framework is cl-weave, used natively throughout: every file
  ;; registers its own top-level (describe "name" (it "case" ...) ...) block.
  (:use #:cl)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave
                #:it #:it-only #:it-concurrent #:it-sequential
                #:describe-only #:describe-concurrent #:describe-sequential
                #:expect #:expect-not
                #:signals #:finishes #:fail #:skip
                #:before-each #:after-each #:before-all #:after-all #:around-each
                #:make-mock-function #:with-mocked-functions #:mock-calls
                #:it-property #:gen-integer #:gen-list #:gen-boolean #:gen-string
                #:gen-member #:gen-one-of
                #:defmatcher)
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
                #:screen-copy-mark-offset
                #:screen-copy-cursor
                #:screen-mouse-mode
                #:screen-mouse-sgr-mode
                #:screen-title
                #:screen-copy-line-selection-p
                #:screen-copy-rect-select-p
                #:screen-app-cursor-keys
                #:screen-dirty-p
                #:char-width
                #:screen-p)
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
                #:window-relayout-current
                #:window-remove-pane
                #:window-resize-active
                #:window-refresh-panes
                #:session-environment
                #:session-environment-value
                #:session-environment-names
                #:session-set-environment
                #:session-unset-environment
                #:session-child-environment
                #:window-tree
                #:make-layout-leaf
                #:make-layout-split
                #:layout-leaf-pane
                #:layout-leaves
                #:layout-node-bounding-box
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
                #:pane-live-p
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
                #:pane-pipe-active-p
                #:pane-pipe-output-stream
                #:pane-pipe-output-thread
                #:pane-pipe-process
                #:pane-window
                #:pane-marked
                #:pane-title
                #:pane-local-options
                ;; Window options
                #:window-local-options
                ;; New session slots
                #:session-locked-p
                #:session-group
                #:session-clients
                ;; Window layout-cycle-index slot
                #:window-layout-cycle-index
                ;; Layout persistence
                #:layout->string
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
                #:session-insert-window
                ;; Pane liveness check
                #:pane-live-p)
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
                #:+msg-detach+ #:+msg-frame+ #:+msg-bye+ #:+msg-command+ #:+msg-reply+
                #:+header-size+
                #:encode-frame #:decode-frame
                #:msg-attach #:msg-key #:msg-resize #:msg-detach #:msg-frame #:msg-bye
                #:msg-command #:msg-reply #:decode-attach-flags #:+attach-flag-read-only+
                #:encode-command-payload #:decode-command-payload
                #:u16-octets-pair
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
                #:key-table-unbind
                #:key-table-command
                #:apply-config-directive
                #:*key-tables*)
  (:import-from #:cl-tmux/commands
                #:kill-pane
                #:kill-window
                #:rename-window
                #:resize-pane
                #:select-window-by-number
                #:swap-pane
                #:swap-two-panes
                #:capture-pane
                ;; Advanced pane commands
                #:break-pane
                #:join-pane
                #:pipe-pane-open
                #:pipe-pane-close
                #:pipe-pane-write)
  (:import-from #:cl-tmux/prompt
                #:prompt #:make-prompt #:prompt-p
                #:*prompt*
                #:prompt-active-p
                #:prompt-start
                #:prompt-input
                #:prompt-backspace
                #:prompt-history-prev
                #:prompt-history-next
                #:prompt-clear
                #:prompt-text
                #:prompt-vi-normal-p
                #:prompt-notify-change
                #:prompt-delete-char
                #:with-active-prompt
                #:*overlay*
                #:*overlay-scroll-offset*
                #:*overlay-shown-at*
                #:overlay-shown-at
                #:*display-panes-active*
                #:overlay-active-p
                #:show-overlay
                #:show-transient-overlay
                #:show-display-panes-overlay
                #:clear-overlay
                #:overlay-lines
                #:overlay-scroll
                #:+default-popup-width+ #:+default-popup-height+
                #:popup #:make-popup #:popup-p
                #:popup-width #:popup-height
                #:popup-screen #:popup-pane #:popup-title #:popup-close-on-exit
                #:*active-popup*
                #:show-popup #:close-popup #:popup-active-p
                #:show-menu #:close-menu #:menu-active-p
                #:menu #:make-menu #:menu-p
                #:menu-title #:menu-items #:menu-selected-index
                #:menu-x #:menu-y
                #:*active-menu*
                #:prompt-label
                #:prompt-buffer
                #:prompt-on-submit
                #:prompt-on-change
                #:prompt-on-cancel
                #:prompt-single-key
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
