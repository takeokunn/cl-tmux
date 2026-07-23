;;; Presentation and input packages.

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
   #:prompt-cursor-bol #:prompt-cursor-eol
   #:prompt-cursor-back #:prompt-cursor-forward
   #:prompt-kill-to-end #:prompt-kill-to-start #:prompt-kill-word-back
   #:prompt-history-prev #:prompt-history-next
   #:prompt-delete-char
   #:*overlay* #:*overlay-scroll-offset* #:*display-panes-active*
   #:overlay-active-p #:overlay-shown-at #:show-overlay #:show-transient-overlay
   #:show-display-panes-overlay
   #:clear-overlay #:overlay-lines
   #:overlay-scroll #:*overlay-shown-at*
   #:+default-popup-width+ #:+default-popup-height+
   #:popup #:make-popup #:popup-p
   #:popup-width #:popup-height
   #:popup-screen #:popup-pane #:popup-title #:popup-close-on-exit
   #:*active-popup*
   #:show-popup #:close-popup #:popup-active-p
   #:menu #:make-menu #:menu-p
   #:menu-title #:menu-items #:menu-selected-index
   #:menu-x #:menu-y
   #:menu-keep-open
   #:*active-menu*
   #:show-menu #:close-menu #:menu-active-p))

(defpackage #:cl-tmux/renderer
  (:use #:cl #:bordeaux-threads
        #:cl-tmux/model #:cl-tmux/terminal #:cl-tmux/prompt)
  (:export
   #:render-session
   #:render-session-to-string
   #:clear-display
   #:enable-mouse-reporting
   #:disable-mouse-reporting
   #:extended-keys-level
   #:enable-extended-keys
   #:disable-extended-keys
   #:enable-focus-reporting
   #:disable-focus-reporting
   #:parse-style-string
   #:style-to-sgr
   #:%popup-border-charset
   ;; Terminal colour-capability downsampling hook, set from the -2 startup
   ;; flag (main-startup-flags.lisp); see renderer-format.lisp.
   #:*color-downsample-fn*
   #:%rgb-int-to-256))

(defpackage #:cl-tmux/input
  (:use #:cl #:cffi
        #:cl-tmux/config #:cl-tmux/pty)
  (:export
   #:with-raw-mode
   #:read-byte-nonblock))
