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
     (:file "config-directives")         ; directive macros + bind/unbind parsing + key dispatch
     (:file "config-directives-set")     ; fixed-arity table + set-option flag handling + side effects
     (:file "config-directives-runtime") ; set-environment, if-shell, run-shell, source-file
     (:file "config-loader")       ; apply-config-directive + preprocessor + load-config-file
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
     (:file "format-helpers")  ; tmux-style format: pure data helpers + shorthand/arithmetic tables
     (:file "format-strftime") ; strftime support (#{t:format}): %strftime-letter-p + formatting engine
     (:file "format")         ; format modifier helpers, glob/regex, iteration expanders
     (:file "format-engine")  ; core %expand-brace, bracket/paren expanders, CPS processor, expand-format
     (:file "format-context") ; context builder: model objects → expand-format plist
     (:file "target")   ; session/window/pane target resolution (-t flag)
     (:file "options")     ; global option registry: hash tables + define-tmux/server-options data
     (:file "options-api") ; option accessor API: type coercions, get/set, scoped overrides, show-options
     (:file "buffer")   ; paste-buffer ring (uses options for buffer-limit)
     (:file "control-mode")  ; control mode (-C) wire-protocol formatters
     (:file "hooks")    ; user-defined hook registry
     (:file "prompt")
     (:file "overlay")              ; overlay, popup, menu state (used by dispatch/events/renderer)
     (:file "commands-core")
     (:file "commands-copy-mode")      ; copy-mode core: enter/exit, scroll, cursor, selection
     (:file "commands-copy-mode-clip") ; rectangle selection text, yank, copy-pipe, append-selection
     (:file "commands-copy-mode-nav")    ; word/line navigation, page/half-page scroll, copy-D/Y
     (:file "commands-copy-mode-search") ; search-forward/backward, search-next/prev
     (:file "commands")
     (:file "commands-keys")           ; send-keys translation, tokenizer, shell helpers
     (:file "renderer-format")     ; ANSI primitives
     (:file "renderer-style")     ; style-string parsing + SGR dispatch tables
     (:file "renderer-pane")      ; pane cell rendering (clock, selection, copy-mode highlights)
     (:file "renderer-borders")   ; split-tree separators + pane border rendering
     (:file "renderer-overlay")   ; popup and menu box-drawing
     (:file "renderer-statusbar") ; status bar composition
     (:file "renderer-compose")   ; session frame compositing + entry points
     (:file "renderer")           ; documentation stub (intentionally empty)
     (:file "input")
     (:file "runtime")       ; shared state + channel sync + prompt history + PTY reader threads
     (:file "runtime-timer") ; status interval timer, lock-after-time, monitor-silence
     (:file "dispatch-core")            ; dispatch macros, focus helpers, core dispatch infrastructure
     (:file "dispatch-core-commands")   ; copy-mode table, format helpers, new-session, named-command table
     (:file "dispatch-commands")          ; flag-parser utils + display/prompt/pane %cmd-* handlers
     (:file "dispatch-commands-buffer")   ; paste-buffer + overlay popup/menu %cmd-* handlers
     (:file "dispatch-commands-option")   ; set-option (CPS) + rename/select %cmd-* handlers
     (:file "dispatch-commands-lifecycle") ; kill/link/unlink/swap/move/source-file/if-shell %cmd-*
     (:file "dispatch-commands-pane")   ; layout/window/pane/session %cmd-*
     (:file "dispatch-commands-pane-x") ; copy-mode -X command name table (send-keys -X dispatch)
     (:file "dispatch-commands-shell")   ; shell/pane-ops %cmd-* (run-shell, if-shell, capture, resize, join, break, clear, rotate)
     (:file "dispatch-commands-auto")   ; window-nav/session-mgmt %cmd-* (find-window, send-keys, list-*, respawn, pipe-pane)
     (:file "dispatch-commands-server") ; server-access ACL + customize-mode tree browser
     (:file "dispatch-commands-runner") ; *arg-command-table* + %run-command-tokens + %run-command-line
     (:file "dispatch-control")         ; control-mode REPL + dispatch-prefix-command
     (:file "dispatch-handlers")        ; command handler rule table part I (detach through wait-for)
     (:file "dispatch-handlers-b")     ; command handler rule table part II (popup/menu through detach-client)
     (:file "dispatch-handlers-buffer") ; paste-buffer command handler helpers
     (:file "events-core")
     (:file "events-mouse")   ; mouse event dispatch + overlay pager escape handler
     (:file "events-keystroke-escape")  ; escape/mouse sequence decoder + make-escape-input-k
     (:file "events-keystroke")          ; CPS state functions: ground-state, after-prefix-state
     (:file "events-keystroke-keys")    ; arrow-key table, modifier/CSI-u helpers, %make-prefix-csi-k
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
         (:file "scroll-tests")    ; scroll-ops/erase/scroll-region/delete-insert-chars — part I
         (:file "scroll-tests-b")  ; direct-row-primitives, direct-action-erase — part II
         (:file "modes-tests")    ; RIS/alt-screen/DECSC/mouse/bracketed-paste/focus — part I
         (:file "modes-tests-b")  ; set-cursor-shape/bell-pending/charset/title/reset-modes — part II
         (:file "sgr-tests")
         (:file "csi-tests")    ; cursor-movement/DECSCUSR/CBT/SU-SD/REP/DA-response — part I
         (:file "csi-tests-b")  ; ECH/DECRQM/XTWINOPS/CPR/REP-0/VPR/ICH/DCH/ED/EL/IL/DL — part II
         (:file "parser-tests")    ; utf8/special/basic-text/CPS parser suites — part I
         (:file "parser-tests-b")  ; combining-chars/ACS/DCS/OSC suites — part II
         (:file "emulator-tests")))
       (:file "layout-tests")      ; layout-tree core: leaves/split/resize/collapse/persistence — part I
       (:file "layout-tests-b")    ; named-layout helpers, apply-named-layout, persistence internals — part II
       (:file "layout-geometry-tests")
       (:file "pane-tests")
       (:file "window-tests")      ; window-relayout/split/resize/zoom/lock/pane-neighbor — part I
       (:file "window-tests-b")    ; apply-named-layout/move/swap/rotate/find/format/auto-rename — part II
       (:file "session-tests")
       (:file "format-tests")            ; format expansion — part I (shorthands, context, modifiers, brace/conditional up to line 972)
       (:file "format-tests-b")          ; format expansion — part II (path/substitute/nested/strftime/context/glob/regex)
       (:file "format-tests-c")          ; format expansion — part III (arithmetic/variables/geometry/content-search/direct-unit-tests)
       (:file "target-tests")
       (:file "buffer-tests")
       (:file "control-mode-tests")
       (:file "options-tests")    ; option registry, coercions, boolean defaults, make-option-spec — part I
       (:file "options-tests-b")  ; define-option-accessor, type-coercions, scoped overrides, show-options — part II
       (:file "hooks-tests")
       (:file "config-tests")
       (:file "config-directives-tests")   ; directive parsing — part I (key-table, bind/unbind, load-config up to line 993)
       (:file "config-directives-tests-b") ; directive parsing — part II (parse-bind-args, tokenizer edge cases, set/source/run/preproc/if-shell)
       (:file "renderer-format-tests")
       (:file "renderer-pane-tests")    ; render-pane/borders/clock/in-sel/display-panes — part I
       (:file "renderer-pane-tests-b")  ; %clock-digit-rows, %render-v-separator, border/pane edge cases — part II
       (:file "renderer-tests")            ; renderer — part I (fixtures, status-bar, render-session, clear-display, window-list up to line 867)
       (:file "renderer-tests-b")          ; renderer — part II (status-bar, parse-style, render-popup/menu)
       (:file "renderer-tests-c")          ; renderer — part III (mouse/focus/keys, lock-screen, justify, cursor-shape, overlay, inline-style)
       (:file "dispatch-tests")               ; core dispatch — part I (cyclic helpers, window/pane select, copy-mode, detach, prefix)
       (:file "dispatch-tests-b")            ; core dispatch — part II (swap-pane, kill-pane-confirm, select-layout, zoom, resize, rotate)
       (:file "dispatch-tests-commands")     ; arg-taking command tests: flag parsing, kill/swap/move/-t, confirm
       (:file "dispatch-tests-commands-b")   ; named-command handlers, display/buffer/session dispatch
       (:file "dispatch-tests-commands-c")   ; helper tests, on-submit paths, cyclic nav, break/join/run/if
       (:file "dispatch-tests-session")    ; copy-mode paging dispatch tests
       (:file "dispatch-tests-session-b")  ; coverage: untested handlers, send-keys, capture-pane, paste-buffer
       (:file "dispatch-tests-session-c")  ; options, session management, control mode, server-lifecycle
       (:file "events-tests")            ; keystroke pipeline — part I (escape, process-byte, mouse, key-table)
       (:file "events-tests-b")          ; locked-session, drag/modifier, copy-mode cursor, vi nav — part II
       (:file "events-tests-c")          ; app-cursor-keys, prompt-key, copy-mode nav, SGR, border-check — part III
       (:file "events-tests-e")          ; status-col, SGR-nil, copy-nav, flush-esc, reset-repeat, CSI-u — part IV
       (:file "events-tests-d")          ; app-cursor-keys (ss3), new bindings, :mark-pane, root table, fn-keys — part V
       (:file "mouse-tests")
       (:file "commands-tests")          ; resize-pane, scroll, kill-pane, select/rename, begin-sel/yank/other-end — part I
       (:file "commands-tests-e")        ; copy-mode-clear-sel, WORD-motion, select-word, move-cursor — part II
       (:file "commands-tests-f")        ; rename-window, kill-window, run/if-shell, selection-text, swap/capture-pane — part III
       (:file "commands-tests-b")        ; copy-mode line/page/word motions, top/bottom, search — part IV
       (:file "commands-tests-g")        ; send-keys, key-name, tokenize, kill-window-mru, join-pane — part V
       (:file "commands-tests-h")        ; copy-mode-exit, break-pane, clear-history, rotate, find, next/prev-win — part VI
       (:file "commands-tests-c")        ; pipe-pane, virtual-row, timeout, scroll helpers, extract-chars — part VII
       (:file "commands-tests-d")        ; rename hooks, server-access, customize-mode, copy-mode-toggle/append/copy-pipe — part VIII
       (:file "commands-tests-i")        ; rectangle-sel, run-copy-cmd, set-cursor, send-keys-l, jump-to-char, goto-line, search-incr — part IX
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
