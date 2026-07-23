(defpackage #:cl-tmux/config
  (:use #:cl)
  (:export
   #:+prefix-key-code+
   #:+ctrl-mask+
   ;; Standard key-table name constants
   #:+table-prefix+
   #:+table-root+
   #:+table-copy-mode+
   #:+table-copy-mode-vi+
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
   #:describe-key-bindings-for-key
   ;; Key-table system
   #:*key-tables*
   #:ensure-key-table
   #:key-table-bind
   #:describe-key-binding-notes
   #:key-table-unbind
   #:key-table-lookup
   #:key-table-command
   #:key-table-repeatable-p
   #:key-table-note
   #:copy-mode-count-command-p
   #:initialize-default-key-tables
   #:load-config-file
   #:load-config-from-stream
   #:load-config-from-string
   #:source-files
   #:apply-config-directive
   #:apply-option-side-effects
   #:config-file-path
   ;; -f startup-flag override (set by the cl-cli global-option parser in
   ;; main-startup-flags.lisp), consulted first by config-file-path.
   #:*config-file-override*
   ;; cl-boundary-kit process boundary shared by config-time shell directives
   ;; and run-shell/if-shell (commands-shell.lisp); see
   ;; config-directives-runtime-services.lisp.
   #:*process-boundary*
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
   #:pty-child-exit-status ; (fd) → (values code :exited|:signaled) or NIL
   #:set-pty-size          ; (fd rows cols)
   ;; Terminal raw mode
   #:enable-raw-mode!      ; (fd)
   #:disable-raw-mode!     ; (fd)
   ;; Multiplexed I/O
   #:select-fds            ; (fds timeout-us) → ready-fd-list
   ;; Terminal geometry
   #:terminal-size         ; () → (values rows cols)
   #:+default-term-rows+   ; fallback terminal rows when ioctl fails/is unavailable
   #:+default-term-cols+   ; fallback terminal columns when ioctl fails/is unavailable
   ;; Port adapter (installs cl-tmux/ports vars at server startup)
   #:install-pty-port))

;;; ── Client/server wire protocol ──────────────────────────────────────────

(defpackage #:cl-tmux/protocol
  (:use #:cl)
  (:export
   ;; Message type tags + header size
   #:+msg-attach+ #:+msg-key+ #:+msg-resize+ #:+msg-detach+ #:+msg-frame+ #:+msg-bye+
   #:+msg-command+ #:+msg-reply+
   #:+header-size+
   ;; Frame layout constants
   #:+payload-length-offset+
   #:+attach-flag-read-only+
   #:+attach-flags-offset+
   #:+cols-offset-in-size-payload+
   ;; Frame codec
   #:encode-frame #:decode-frame
   ;; Typed message constructors
   #:msg-attach #:msg-key #:msg-resize #:msg-detach #:msg-frame #:msg-bye
   #:msg-command #:msg-reply #:decode-attach-flags
   ;; Command message codec (protocol-command.lisp, same package)
   #:+field-delimiter+
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
