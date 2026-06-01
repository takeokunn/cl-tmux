;;;; Wire-level packages: config, PTY, protocol, transport, networking.

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
