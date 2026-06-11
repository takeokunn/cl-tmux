(defsystem "cl-tmux"
  :description "A tmux-compatible terminal multiplexer in Common Lisp"
  :version "0.1.0"
  :author "motoki317 <motoki317@gmail.com>"
  :license "MIT"
  :depends-on (:cffi           ; C foreign-function interface
               :bordeaux-threads ; portable threads + locks
               :babel            ; string↔octet encoding
               :cl-ppcre)        ; Perl-compatible regular expressions
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:file "config")
     (:file "config-tokenizer")    ; config tokenizer + key/command parse tables
     (:file "config-directives")   ; directive handlers + option side-effects + loader
     (:file "pty-ffi")       ; FFI declarations and platform constants
     (:file "pty-rawmode")   ; terminal raw mode management
     (:file "pty")           ; PTY lifecycle, I/O, multiplexing
     (:file "protocol")
     (:file "transport")
     (:file "net")
     (:module "terminal"
      :serial t
      :components
      ((:file "cell")      ; immutable cell type, char-width table
       (:file "screen")    ; mutable screen struct and grid operations
       (:file "scroll")    ; row helpers + scroll-up/down + decstbm (loads before cursor/erase/edit)
       (:file "erase")     ; erase-region, erase-display, erase-line rule tables
       (:file "edit")      ; delete/insert chars+lines (uses %copy-row, %clear-row from scroll)
       (:file "cursor")    ; cursor movement, character writing (uses scroll-up-one)
       (:file "modes")     ; DEC modes, cursor save/restore, display projection
       (:file "sgr")
       (:file "csi")
       (:file "parser")
       (:file "parser-osc")    ; OSC accumulator, Base64 decoder, *osc52-handler*
       (:file "emulator")))
     (:file "pane")             ; leaf PTY data and wiring (loaded first: layout needs pane-reposition)
     (:file "layout")             ; tree structure + traversal (uses pane-reposition)
     (:file "layout-persistence") ; layout string serialization/deserialization
     (:file "layout-geometry")    ; rectangle assignment + resize helpers (uses pane-id, pane-x/y/w/h)
     (:file "window")          ; window struct + core operations (split/relayout/resize)
     (:file "window-neighbor") ; directional pane navigation (uses window-panes)
     (:file "window-layout")   ; named layouts (apply-named-layout, uses window accessors)
     (:file "session")  ; session management (uses window)
     (:file "format")         ; tmux-style format string expansion (engine only)
     (:file "format-context") ; context builder: model objects → expand-format plist
     (:file "target")   ; session/window/pane target resolution (-t flag)
     (:file "options")  ; global option registry
     (:file "buffer")   ; paste-buffer ring (uses options for buffer-limit)
     (:file "control-mode")  ; control mode (-C) wire-protocol formatters
     (:file "hooks")    ; user-defined hook registry
     (:file "prompt")
     (:file "overlay")              ; overlay, popup, menu state (used by dispatch/events/renderer)
     (:file "commands-core")
     (:file "commands-copy-mode")
     (:file "commands-copy-mode-nav")    ; word/line navigation, page/half-page scroll, copy-D/Y
     (:file "commands-copy-mode-search") ; search-forward/backward, search-next/prev
     (:file "commands")
     (:file "renderer-format")     ; ANSI primitives
     (:file "renderer-style")     ; style-string parsing + SGR dispatch tables
     (:file "renderer-pane")      ; pane + border rendering
     (:file "renderer-overlay")   ; popup and menu box-drawing
     (:file "renderer-statusbar") ; status bar composition
     (:file "renderer-compose")   ; session frame compositing + entry points
     (:file "renderer")           ; documentation stub (intentionally empty)
     (:file "input")
     (:file "runtime")
     (:file "dispatch-core")            ; dispatch macros, focus helpers, named-command table
     (:file "dispatch-commands")         ; display/buffer/popup/option/rename %cmd-* handlers
     (:file "dispatch-commands-pane")   ; layout/window/pane/session %cmd-* + copy-mode -X table
     (:file "dispatch-commands-auto")   ; automation %cmd-* (send-keys, run-shell, capture, etc.)
     (:file "dispatch-commands-runner") ; *arg-command-table* + %run-command-tokens + %run-command-line
     (:file "dispatch-control")         ; control-mode REPL + dispatch-prefix-command
     (:file "dispatch-handlers")        ; command handler rule table (define-command-handlers)
     (:file "dispatch-handlers-buffer") ; paste-buffer command handler helpers
     (:file "events-core")
     (:file "events-keystroke")
     (:file "events-loop")
     (:file "session-registry")  ; session registry + group management
     (:file "server")
     (:file "server-multi")  ; multi-client select-multiplexed serve loop
     (:file "client")
     (:file "main"))))
  ;; Build a standalone binary: (asdf:make :cl-tmux)
  :build-operation "program-op"
  :build-pathname "cl-tmux"
  :entry-point "cl-tmux:main"
  :in-order-to ((test-op (test-op "cl-tmux/test"))))

(defsystem "cl-tmux/test"
  :description "Test suite for cl-tmux"
  :depends-on (:cl-tmux :fiveam)
  :components
  ((:module "test"
    :serial t
    :components
    ((:file "package")
     (:file "helpers")
     (:module "unit"
      :serial t
      :components
      ((:module "terminal"
        :serial t
        :components
        ((:file "cell-tests")     ; declares terminal-suite parent; double-width sub-suite
         (:file "screen-tests")   ; resize sub-suite
         (:file "cursor-tests")
         (:file "scroll-tests")
         (:file "modes-tests")
         (:file "sgr-tests")
         (:file "csi-tests")
         (:file "parser-tests")
         (:file "emulator-tests")))
       (:file "layout-tests")
       (:file "layout-geometry-tests")
       (:file "pane-tests")
       (:file "window-tests")
       (:file "session-tests")
       (:file "format-tests")
       (:file "target-tests")
       (:file "buffer-tests")
       (:file "control-mode-tests")
       (:file "options-tests")
       (:file "hooks-tests")
       (:file "config-tests")
       (:file "config-directives-tests")
       (:file "renderer-format-tests")
       (:file "renderer-pane-tests")
       (:file "renderer-tests")
       (:file "dispatch-tests")
       (:file "events-tests")
       (:file "mouse-tests")
       (:file "commands-tests")
       (:file "overlay-tests")
       (:file "prompt-tests")
       (:file "protocol-tests")
       (:file "transport-tests")
       (:file "net-tests")
       (:file "server-tests")
       (:file "server-multi-tests")
       (:file "pty-ffi-tests")
       (:file "pty-rawmode-tests")
       (:file "pty-tests")
       (:file "input-tests")
       (:file "runtime-tests")
       (:file "client-tests")
       (:file "main-tests")
       (:file "advanced-tests")))
     (:file "suite"))))
  ;; Run with: (asdf:test-system :cl-tmux)
  :perform (test-op (op c)
             (symbol-call :cl-tmux/test :run-tests)))
