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
       (:file "modes")     ; DEC modes — alt-screen + DEC PM rule table (parts I-II)
       (:file "modes-d")   ; DEC modes — focus, DECSC, reset, ANSI SM/RM, charset (parts III-IV)
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
     (:file "helpers-b")
     (:module "unit"
      :serial t
      :components
      ((:module "terminal"
        :serial t
        :components
        ((:file "cell-tests")     ; declares terminal-suite parent; double-width sub-suite
         (:file "screen-tests")   ; construction/p/cell-access/cursor/resize/dirty/sgr-pen/bell — part I
         (:file "screen-tests-b") ; copy-mode slots, alt-screen, mouse-sgr, response-queue, origin-mode, tab-stops, lock, cells/parser — part II
         (:file "screen-tests-c") ; screen-clear-dirty, reset-sgr-pen, bell-pending, screen-consume-bell, slots, copy-mode extra slots — part III
         (:file "cursor-tests")   ; scroll-region-clamp, set-cursor, direct-action, tab-stops, ri, nel, wide-char, advance — part I
         (:file "cursor-tests-b") ; %place-wide-char, table-driven, combining-char-p, write-char combining, DEC graphics — part II
         (:file "cursor-tests-c") ; cursor-ri, cursor-nel, write-char-at-cursor wide, %advance-cursor no-wrap, movement behavioral — part III
         (:file "scroll-tests")    ; scroll-ops/erase/scroll-region/delete-insert-chars — part I
         (:file "scroll-tests-b")  ; direct-row-primitives, direct-action-erase, constrained-scroll, history-limit — part II
         (:file "scroll-tests-c")  ; direct-line-edit (il/dl), scroll-screen-to-history, DEC-rect (DECERA/DECFRA/DECCRA) — part III
         (:file "modes-tests")    ; RIS/alt-screen/DECSC/mouse/bracketed-paste/focus — part I
         (:file "modes-tests-b")  ; set-cursor-shape/bell-pending/set-charset/set-screen-title/reset-modes/alt-screen-direct — part II
         (:file "modes-tests-c")  ; screen-invoked-charset/G0-G1, set-screen-cwd, erase-display-mode-3, IRM, LNM, DECSCNM, DECSTR — part III
         (:file "modes-tests-d")  ; mouse DEC private modes, bracketed paste, focus events, app-cursor, auto-wrap, reset-sgr-pen, display-cell — part IV
         (:file "sgr-tests")    ; sgr suite: fg/bg tables, truecolor, colon SGR, pen-to-sgr-params — part I
         (:file "sgr-tests-b")  ; direct-action-sgr, sgr-extended, extra codes, define-sgr-rules, consume-256-color — part II
         (:file "csi-tests")    ; cursor-movement/DECSCUSR/CBT/SU-SD — part I
         (:file "csi-tests-d")  ; REP/da-response/DECRQM/XTWINOPS/CPR/DA-table/REP-count-zero — part IV
         (:file "csi-tests-b")  ; ECH/DSR/ich-dch/decstbm/execute-csi-direct/%csi-decstbm-params — part II
         (:file "csi-tests-c")  ; csi-unknown-sequences/DECOM/cup-row/enqueue/XTPUSHTITLE/DEC-rect — part III
         (:file "parser-tests")    ; utf8/special/OSC/ESC-hash/private/dec-pm — part I
         (:file "parser-tests-b")  ; combining-chars/ACS/dcs-parsing/xtgettcap/decrqss/ground-state/direct-dcs/direct-osc — part II
         (:file "parser-tests-d")  ; osc-dispatch-edge-cases/osc52/osc7/parser-suite/base64/csi-colon — part IV
         (:file "parser-tests-c")  ; basic-text, inline-predicates, CPS state functions, define-state — part III
         (:file "emulator-tests")))
       (:file "layout-tests")      ; layout-tree core: leaves/split/resize/collapse/persistence — part I
       (:file "layout-tests-b")    ; named-layout helpers, apply-named-layout — part II
       (:file "layout-tests-c")    ; layout persistence internals: split-bounding-box, node-to-string, read-digits, round-trips — part III
       (:file "layout-geometry-tests")  ; orientation helpers, layout-assign, resize-find-split, pane-at-position, split-child — part I
       (:file "layout-geometry-tests-b") ; %ranges-overlap-p, pane-center, closest-to-center, define-axis-rules, nested min-extent — part II
       (:file "pane-tests")
       (:file "window-tests")      ; window-relayout/split/resize/zoom/lock/pane-neighbor — part I
       (:file "window-tests-b")    ; apply-named-layout (5 layouts), last-window/move/swap/rotate — part II
       (:file "window-tests-c")    ; find-window-by-name, list-windows-format, auto-rename-from-osc — part III
       (:file "session-tests")
       (:file "format-tests")            ; format expansion — part I (shorthands, brace/conditional, context, window_flags, helpers)
       (:file "format-tests-d")          ; format expansion — part IV (shorthand-table, %expand-brace, %truthy-p, pane/client vars, structural, modifiers)
       (:file "format-tests-b")          ; format expansion — part II (path/substitute/nested/strftime/context/glob/regex)
       (:file "format-tests-c")          ; format expansion — part III (arithmetic/vars/geometry/pane_at_edges/pane-synchronized)
       (:file "format-tests-e")          ; format expansion — part V (content-search, glob-match-p, pane-visible-lines, apply-pad-modifier, window-raw-flags)
       (:file "format-tests-f")          ; format expansion — part VI (new context keys, modifier chaining, glob/regex match, format variables)
       (:file "target-tests")      ; parse-session/window/pane/target, find-by-target, resolve-target — part I
       (:file "target-tests-b")    ; %sigil-id, %name-prefix-p, edge cases, table-driven parse-target, multi-digit ids — part II
       (:file "buffer-tests")
       (:file "control-mode-tests")
       (:file "options-tests")    ; option registry, coercions, boolean defaults, make-option-spec — part I
       (:file "options-tests-b")  ; define-option-accessor, type-coercions, scoped overrides, show-options — part II
       (:file "options-tests-c")  ; type-coercion dispatch, option-table macro, spec accessors, server options, show-option sorting — part III
       (:file "hooks-tests")              ; hook-event-constants, hook-registry, add/run/remove/clear/list-hooks — part I
       (:file "hooks-tests-b")            ; command hooks (set-hook), set-hook -u, list-command-hooks, runtime set-hook, show-hooks — part II
       (:file "config-tests")
       (:file "config-directives-tests")   ; directive parsing — part I (bindable-cmds, apply-directive, set flags, bind/unbind, load-config-from-stream)
       (:file "config-directives-tests-c") ; directive parsing — part III (load-config-file, command-keyword, parse-bind-args, key-table edge cases)
       (:file "config-directives-tests-b") ; directive parsing — part II (%parse-bind-args, tokenizer, source-file, run-shell, %expand-tilde, if/elif, unbind-all)
       (:file "config-directives-tests-d") ; directive parsing — part IV (set-g-status-off, bind-key-n, load-config, %elif, line-continuation, source-file-glob, if-shell)
       (:file "config-directives-tests-e") ; directive parsing — part V (macro registry, env-set-p, key-table edge cases, remaining bind/set directives)
       (:file "renderer-format-tests")           ; SGR codes, style tokens, border-color, cursor-shape, palette bounds — part I
       (:file "renderer-format-tests-b")         ; all-attrs table, attrs2, ul-color, style-token/emit remaining, parse-style, border-charset — part II
       (:file "renderer-pane-tests")    ; render-pane content/borders/window-style — part I
       (:file "renderer-pane-tests-b")  ; %clock-digit-rows, %render-v-separator, border/pane edge cases — part II
       (:file "renderer-pane-tests-c")  ; %apply-border-style branches, draw-clock, render-pane-clock-mode, draw-pane-number, in-sel-branch — part III
       (:file "renderer-tests")            ; renderer — part I (status-bar, render-session, clear-display, status-indicators, window-list)
       (:file "renderer-tests-d")          ; renderer — part IV (per-window options, alert-tab-styles, status-bar-line, overlay, DECTCEM)
       (:file "renderer-tests-b")          ; renderer — part II (status-bar, status-position, BEL rendering, status-left-expanded)
       (:file "renderer-tests-f")          ; renderer — part VI (parse-style-string, style-to-sgr, status-length, window-status-format, render-popup/menu)
       (:file "renderer-tests-c")          ; renderer — part III (mouse/focus/keys, lock-screen, justify, cursor-shape, zoom-suppression)
       (:file "renderer-tests-e")          ; renderer — part V (%clamp-status-segment, cursor-shape in output, status-bar-line gap, inline-style, bell relay)
       (:file "dispatch-tests")               ; core dispatch — part I (cyclic helpers, window/pane select, copy-mode, detach, prefix)
       (:file "dispatch-tests-c")             ; core dispatch — part III (focus events window-switch, list-keys, select-pane, zoom, list-windows/sessions)
       (:file "dispatch-tests-b")            ; core dispatch — part II (swap-pane, kill-pane-confirm, command-prompt, run-command-line, set-option)
       (:file "dispatch-tests-d")            ; core dispatch — part IV (%cmd-set-option scope, side-effects, bind/unbind/rename, set-hook)
       (:file "dispatch-tests-commands")     ; flag-parse, select-window/pane, kill, swap-window, source-file, move-window, if-shell-F, dispatch-named-command, show-messages — part I
       (:file "dispatch-tests-commands-f")  ; display-message-logs, clock-mode, capture-pane, send-keys, choose-tree, confirm-before, paste-to-pane, format-tree-entry — part VI
       (:file "dispatch-tests-commands-b")  ; synchronize, lock, last-window, show-options, respawn, pipe-pane, last-pane, format helpers, popup/menu — part II
       (:file "dispatch-tests-commands-e")  ; switch-client, last-session, new-session, kill-session, mark-pane, next-layout, bind/unbind-key, list/choose-buffer, wait-for — part V
       (:file "dispatch-tests-commands-c")   ; helper tests, on-submit paths, cyclic nav, break/join/run/if — part III
       (:file "dispatch-tests-commands-d")   ; has-session, find-window/select-window-prompt, move/swap-window, bind/unbind-key, kill-pane, split, new-window — part IV
       (:file "dispatch-tests-session")    ; copy-mode paging, with-active-pane, format-menu, named-layout, kill-result, command-dispatch-outcome — part I
       (:file "dispatch-tests-session-e")  ; named-command table, select-layout, set-option -u, split-window, new-window, server-management, prefix, aliases — part V
       (:file "dispatch-tests-session-b")  ; coverage: untested handlers, parse-flag-token, rename/new-window/split-window flags — part II
       (:file "dispatch-tests-session-c")  ; options, move-window, new-session -s/-A/-t, control-mode REPL — part III
       (:file "dispatch-tests-session-f")  ; new-session duplicate, grouped sessions, control-mode notifications, server-lifecycle, %output relay — part VI
       (:file "dispatch-tests-session-d")  ; display-popup, send-keys -N/-H, capture-pane, named paste-buffer, join-pane, wait-for-arg — part IV
       (:file "events-tests")            ; keystroke pipeline — part I (escape, process-byte, mouse, key-table, copy-mode vi-nav)
       (:file "events-tests-f")          ; keystroke pipeline — part VI (PageUp/Down, prefix-arrow, send-prefix, modifier+arrow, meta/alt)
       (:file "events-tests-b")          ; locked-session, drag/modifier, copy-mode cursor, vi nav — part II
       (:file "events-tests-h")          ; byte-constants, make-input-state, forward-octets, maybe-rename-window — part VIII
       (:file "events-tests-c")          ; app-cursor-keys, prompt-key, copy-mode nav, SGR, border-check — part III
       (:file "events-tests-e")          ; status-col, SGR-nil, copy-nav, flush-esc, reset-repeat, CSI-u — part IV
       (:file "events-tests-d")          ; app-cursor-keys (ss3), new bindings, :mark-pane, root table, fn-keys — part V
       (:file "events-tests-g")          ; select-layout-spread, new key bindings, choose-window, mouse-reporting, tmux defaults — part VII
       (:file "events-tests-i")          ; copy-mode v-select, middle-cursor-jump, mouse X10, CSI-tilde outside mode, CSI-3byte — part IX
       (:file "mouse-tests")
       (:file "commands-tests")          ; resize-pane, scroll, kill-pane, select/rename, begin-sel/yank/other-end — part I
       (:file "commands-tests-e")        ; copy-mode-clear-sel, WORD-motion, select-word, move-cursor — part II
       (:file "commands-tests-f")        ; rename-window, kill-window, run/if-shell, selection-text, swap-pane — part III
       (:file "commands-tests-m")        ; swap-pane (cont), capture-pane, shift-line-wrapped, copy-mode scroll, resize-pane, split-window — part XIII
       (:file "commands-tests-n")        ; copy-mode-begin-selection multi-row, yank, other-end — part XIV
       (:file "commands-tests-b")        ; copy-mode line-start/end, high/middle/low, scroll noop guards, word motions, top/bottom — part IV
       (:file "commands-tests-k")        ; begin-line-selection, copy-end-of-line (D), copy-line (Y), search-forward/backward, wrap-search — part XI
       (:file "commands-tests-g")        ; send-keys, key-name, tokenize, kill-window-mru, join-pane — part V
       (:file "commands-tests-h")        ; copy-mode-exit, break-pane, clear-history, rotate, find, next/prev-win — part VI
       (:file "commands-tests-c")        ; pipe-pane, virtual-row, timeout, scroll helpers, word/paragraph nav — part VII
       (:file "commands-tests-o")        ; selection-bounds scrollback, word/paragraph nav, scroll-middle — part XV
       (:file "commands-tests-j")        ; join-pane helpers, resize-pane up, noop guards, search, scroll, extract-chars, row-string — part X
       (:file "commands-tests-d")        ; rename/select hooks, server-access, customize-mode, begin-line-selection multi-row — part VIII
       (:file "commands-tests-l")        ; copy-mode copy-line/copy-end-of-line, with-shell-timeout, kill hooks, toggle-rect, append-sel, copy-pipe, renumber-windows — part XII
       (:file "commands-tests-i")        ; rectangle-sel, run-copy-cmd, set-cursor, send-keys-l, jump-to-char, goto-line, search-incr — part IX
       (:file "overlay-tests")
       (:file "prompt-tests")
       (:file "protocol-tests")            ; octets/frame-header/round-trips/msg-command — part I
       (:file "protocol-tests-b")          ; read-u32, split-on-nul, encode/decode-command-payload, target-field-p — part II
       (:file "transport-tests")
       (:file "net-tests")
       (:file "server-tests")
       (:file "server-tests-b")          ; list-sessions, rename-session, switch-client, last-session — part II
       (:file "server-multi-tests")
       (:file "pty-ffi-tests")
       (:file "pty-rawmode-tests")
       (:file "pty-tests")
       (:file "input-tests")
       (:file "runtime-tests")             ; globals, pane-reader-loop, monitor-activity/silence, prompt-history, alert-action — part I
       (:file "runtime-tests-c")          ; stop-reader-threads, add-message-log, add-prompt-history, wait-for-channel — part III
       (:file "runtime-tests-b")          ; add-message-log table-driven, add-prompt-history, wait-for-channel — part II
       (:file "client-tests")
       (:file "main-tests")
       (:file "advanced-tests")))
     (:file "suite"))))
  ;; Run with: (asdf:test-system :cl-tmux)
  :perform (test-op (op c)
             (symbol-call :cl-tmux/test :run-tests)))
