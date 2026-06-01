;;;; Application model packages: prompt, model, format, buffer, hooks, options.

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
