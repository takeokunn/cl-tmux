;;;; Test package for cl-tmux.

(defpackage #:cl-tmux/test
  (:use #:cl #:fiveam)
  (:import-from #:cl-tmux/terminal
                #:make-screen
                #:screen-resize
                #:screen-process-bytes
                #:screen-cell
                #:screen-cursor-x
                #:screen-cursor-y
                #:screen-width
                #:screen-height
                #:cell-char
                #:cell-fg
                #:cell-bg
                #:cell-attrs)
  (:import-from #:cl-tmux/model
                #:divide-window
                #:create-initial-session
                #:session-active-window
                #:window-panes
                #:window-split
                #:window-relayout
                #:all-panes
                #:pane-x #:pane-y #:pane-width #:pane-height #:pane-fd #:pane-pid)
  (:import-from #:cl-tmux/config
                #:*status-height*)
  (:import-from #:cl-tmux/pty
                #:forkpty-with-shell
                #:pty-write
                #:pty-read-blocking
                #:pty-close
                #:select-fds)
  (:export #:run-tests
           #:cl-tmux-suite))
