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
     ((:module "bootstrap-packages"
       :pathname "bootstrap"
       :serial t
       :components ((:file "package")))  ; loads package-* fragments; defines all packages
     (:module "application/config"
      :serial t
      :components
      ((:file "config-key-table-store") ; key-table storage primitives (bind/unbind/lookup)
       (:file "config")
       (:file "config-tokenizer")    ; config tokenizer + key/command parse tables
       (:file "config-directives-macro")   ; generic directive-dispatch macro infra + posix/tilde/flag helpers
       (:file "config-bind-parsing")        ; bind/unbind-specific parsing + key-table dispatch
       (:file "config-directives-set")     ; fixed-arity table + set-option flag handling/routing
       (:file "config-option-side-effects") ; option runtime side effects + set-hook directive
       (:file "config-directives-runtime") ; set-environment, if-shell, run-shell, source-file
       (:file "config-loader")        ; directive dispatch + comment stripping + apply-config-line
       (:file "config-preprocessor")  ; %if/%elif/%else/%endif state machine + brace/continuation joining
       (:file "config-paths")))       ; config-file path resolution + load-config-file
     (:module "domain/ports"
      :serial t
      :components
      ((:file "pty-port")))   ; PTY port abstraction: port vars + port fns (loads before infra so defvars are declared)
     (:module "infrastructure/pty"
      :serial t
      :components
      ((:file "pty-ffi")       ; FFI declarations and platform constants
       (:file "pty-rawmode")   ; terminal raw mode management
       (:file "pty")))         ; PTY lifecycle + install-pty-port adapter (references cl-tmux/ports vars)
     (:module "infrastructure/net"
      :serial t
      :components
      ((:file "protocol")
       (:file "protocol-command")  ; +msg-command+ payload codec (same package as protocol)
       (:file "transport")
       (:file "net")))
     (:module "domain/terminal"
      :serial t
      :components
      ((:file "cell")         ; immutable cell type, char-width table
       (:file "screen")       ; screen struct (DATA layer): defstruct, grid helpers, resize
       (:file "screen-logic") ; screen mutation helpers (LOGIC layer): screen-clear-dirty, screen-consume-bell, screen-drain-queue, reset-sgr-pen
       (:file "scroll")    ; row helpers + scroll-up/down + decstbm (loads before cursor/erase/edit)
       (:file "erase")     ; erase-region, erase-display, erase-line rule tables
       (:file "edit")      ; delete/insert chars+lines (uses %copy-row, %clear-row from scroll)
       (:file "cursor")    ; cursor movement (uses scroll-up-one)
       (:file "char-write") ; combining chars, DEC graphics, wide/normal cell writes (uses cursor-down/scroll, insert-chars)
       (:file "modes-alt-screen") ; DEC modes — alt-screen enter/exit helpers (part I)
       (:file "modes-dec-pm")     ; DEC modes — DEC PM rule-table macro + dispatch table (part II)
       (:file "modes-d")   ; DEC modes — focus, DECSC, reset, ANSI SM/RM, charset (parts III-IV)
       (:file "sgr")
       (:file "csi-replies")   ; CSI reply-queue helpers (DSR/DA/CPR/DECRQM/XTWINOPS); loads before csi
       (:file "csi")
       (:file "parser-dcs")    ; DCS passthrough/XTGETTCAP/DECRQSS helpers (loads before parser)
       (:file "parser")
       (:file "parser-osc-helpers") ; OSC helper layer: Base64, hex, OSC 7/52
       (:file "parser-osc")    ; OSC accumulator + dispatcher state machine
       (:file "emulator")))
     (:module "domain/model"
      :serial t
      :components
      ((:file "pane")             ; leaf PTY data and wiring (loaded first: layout needs pane-reposition)
       (:file "layout")             ; tree structure + traversal (uses pane-reposition)
       (:file "layout-persistence") ; layout string serialization
       (:file "layout-geometry")    ; rectangle assignment + resize helpers (uses pane-id, pane-x/y/w/h)
       (:file "window")             ; window struct + core ops (split/relayout/constants)
       (:file "window-operations")  ; window resize/rotate/zoom (uses window + layout helpers)
       (:file "window-neighbor") ; directional pane navigation (uses window-panes)
       (:file "window-layout")   ; named layouts (apply-named-layout, uses window accessors)
       (:file "session")             ; session lifecycle: struct + windows + touch + all-panes
       (:file "session-environment"))) ; environment management: update-env/overlay/child-env
     (:module "domain/format"
      :serial t
      :components
      ((:file "format-helpers")    ; tmux-style format: pure data helpers + shorthand/arithmetic tables
       (:file "format-strftime")   ; strftime support (#{t:format}): %strftime-letter-p + formatting engine
       (:file "format-modifiers")  ; value-modifiers (#{b:}/#{d:}/#{=N:}/#{pN:}/#{s///:}/#{q:}/#{E:})
       (:file "format-search")     ; glob/regex matching + pane content search (#{m:}/#{m/r:}/#{C:})
       (:file "format-operators")  ; comparison and logical operators (#{==:}/#{!=:}/#{||:}/#{&&:})
       (:file "format-iteration")  ; W:/S:/P: window/session/pane iteration expanders
       (:file "format-engine")     ; core %expand-brace, bracket/paren expanders, CPS processor, expand-format
       (:file "format-context-os-probe") ; OS probes (pgrep/ps/lsof/proc) for pane_current_command/pane_current_path
       (:file "format-context-screen") ; pane-geometry/screen/client section builders (mechanical getter tables)
       (:file "format-context")))  ; context builder: model objects → expand-format plist
     (:module "domain/repository"
      :serial t
      :components
      ((:file "session-repository"))) ; Repository pattern: session store protocol + *session-repo* var
     ;; target resolution is a domain/model service; placed in the model directory
     ;; via :pathname so its load slot (after format) stays byte-identical.
     (:module "domain-model-target"
      :pathname "domain/model"
      :serial t
      :components
      ((:file "target")))   ; session/window/pane target resolution (-t flag)
     (:module "domain/options"
      :serial t
      :components
      ((:file "options")             ; global option registry: hash-tables + define-option-table macros
       (:file "options-registry-data") ; define-tmux-options/define-server-options DATA tables
       (:file "options-scope")  ; scope dispatch + array-name parsing + spec lookup + presence predicates
       (:file "options-api")    ; type coercions, define-option-accessor, public get/set API, scoped overrides
       (:file "options-display"))) ; option display/rendering helpers (show-options, show-option-values)
     (:module "domain/buffer"
      :serial t
      :components
      ((:file "buffer")))   ; paste-buffer ring (uses options for buffer-limit)
     (:module "infrastructure/control-mode"
      :serial t
      :components
      ((:file "control-mode")))  ; control mode (-C) wire-protocol formatters
     (:module "domain/hooks"
      :serial t
      :components
      ((:file "hooks")))    ; user-defined hook registry
     (:module "presentation/prompt"
      :serial t
      :components
      ((:file "prompt")
       (:file "overlay")))              ; overlay, popup, menu state (used by dispatch/events/renderer)
     ;; commands context: general pane/window ops + the cohesive copy-mode
     ;; cluster (its own sub-area). commands-core loads first, then copy-mode,
     ;; then commands/commands-keys-data/commands-tokenizer/commands-keys/
     ;; commands-shell (split back to root via :pathname).
     (:module "application/commands"
      :serial t
      :components
      ((:file "commands-core")))
     (:module "application/commands/copy-mode"
      :serial t
      :components
      ((:file "commands-copy-mode")      ; copy-mode core: enter/exit, scroll, cursor, selection
       (:file "commands-copy-mode-selection") ; selection bounds and text extraction helpers
       (:file "commands-copy-mode-word") ; word/WORD motion helpers shared by navigation/search
       (:file "commands-copy-mode-nav-line") ; line-start/end, cursor-jump macros, scroll wrappers
       (:file "commands-copy-mode-nav-select") ; begin-line-selection
       (:file "commands-copy-mode-nav-paragraph") ; paragraph boundaries
       (:file "commands-copy-mode-nav-jump") ; jump-to-char and goto-line commands
       (:file "commands-copy-mode-nav-copy") ; copy-end-of-line, copy-line helpers
       (:file "commands-copy-mode-clip") ; rectangle selection text, yank, copy-pipe, append-selection
       (:file "commands-copy-mode-virtual") ; virtual-row helpers shared by search/brackets
       (:file "commands-copy-mode-brackets") ; bracket matching commands
       (:file "commands-copy-mode-search"))) ; search-forward/backward, search-next/prev
     (:module "application-commands-2"
      :pathname "application/commands"
      :serial t
      :components
      ((:file "commands")               ; loads commands-capture-pane fragment
       (:file "commands-keys-data")      ; send-keys key-name data tables
       (:file "commands-tokenizer")      ; shell-style command-string tokeniser
       (:file "commands-keys")           ; send-keys key-name translation logic
       (:file "commands-shell")))        ; run-shell / if-shell subprocess execution
     (:module "presentation/renderer"
      :serial t
      :components
      ((:file "renderer-format")     ; ANSI primitives
       (:file "renderer-style-data") ; declarative style/SGR/border-charset dispatch tables
       (:file "renderer-style")     ; style-string parsing + SGR emission logic
       (:file "renderer-pane-selection") ; selection bounds helpers
       (:file "renderer-pane-clock")     ; big digits + display-panes clock overlay
       (:file "renderer-pane-search")    ; pane content search match ranges
       (:file "renderer-pane-copy-mode") ; copy-mode pane overlay rendering
       (:file "renderer-pane")           ; pane cell rendering (selection, copy-mode highlights)
       (:file "renderer-borders")        ; split-tree separators + pane border rendering
       (:file "renderer-overlay")        ; popup and menu box-drawing
       (:file "renderer-statusbar-layout"); status bar layout helpers
       (:file "renderer-statusbar")      ; status bar composition
       (:file "renderer-compose-protocols") ; terminal protocol toggles
       (:file "renderer-compose-overlay")   ; overlay rendering + mouse mode sequences
       (:file "renderer-compose-effects")   ; bell / cursor / queue drain effects
       (:file "renderer-compose")        ; session frame compositing + entry points
       (:file "renderer")))         ; documentation stub (intentionally empty)
     (:module "infrastructure/input"
      :serial t
      :components
      ((:file "input")))
     (:module "bootstrap-runtime"
      :pathname "bootstrap"
      :serial t
      :components
      ((:file "runtime")        ; shared state + channel sync + prompt history + SIGWINCH
       (:file "runtime-reader") ; PTY reader CPS state machine + alert-action dispatch table
       (:file "runtime-timer"))) ; status interval timer, lock-after-time, monitor-silence
     ;; dispatch context, subdivided into cohesive sub-areas. Load order is
     ;; byte-identical to the old flat module; handlers split early (support)
     ;; / late (rest) via the :pathname trick (dispatch-handlers-2).
     (:module "application/dispatch/core"
      :serial t
      :components
      ((:file "dispatch-core")            ; dispatch macros + core dispatch infrastructure
       (:file "dispatch-core-targets")    ; target-string resolution helpers
       (:file "dispatch-core-hooks")      ; command-hook dispatch helpers
       (:file "dispatch-core-window-cmds") ; window/pane/split command factories
       (:file "dispatch-core-focus")      ; focus event delivery helpers
       (:file "dispatch-core-commands"))) ; copy-mode table, format helpers, new-session, named-command table (loads dispatch-command-specs* fragments)
     (:module "application/dispatch/handlers"
      :serial t
      :components
      ((:file "dispatch-handlers-support"))) ; shared prompt/menu helpers for dispatch handlers
     (:module "application/dispatch/commands"
      :serial t
      :components
      ((:file "dispatch-commands")          ; flag-parser utils + display/prompt/pane %cmd-* handlers
       (:file "dispatch-commands-buffer")   ; paste-buffer + overlay popup/menu %cmd-* handlers
       (:file "dispatch-commands-option")   ; set-option (CPS) + show-options %cmd-*
       (:file "dispatch-commands-option-pane") ; rename/select %cmd-* handlers (loads option-pane-window/pane fragments)
       (:file "dispatch-commands-lifecycle") ; kill/link/unlink/swap/move/source-file %cmd-*
       (:file "dispatch-commands-pane")   ; layout/window/pane helpers + *key-table*
       (:file "dispatch-commands-pane-session") ; session/client lifecycle %cmd-*
       (:file "dispatch-commands-pane-x") ; copy-mode -X command name table (send-keys -X dispatch)
       (:file "dispatch-commands-shell")   ; shell/pane-ops %cmd-* (run-shell, if-shell, capture, resize, join, break, clear, rotate)
       (:file "dispatch-commands-list-data") ; *command-usage-table* pure data (canonical-name → usage-flags)
       (:file "dispatch-commands-list")    ; list-sessions/windows/panes/clients arg parsing + overlay handlers
       (:file "dispatch-commands-list-commands") ; list-commands + wait-for arg parsing/handlers
       (:file "dispatch-commands-auto")   ; window-nav/session-mgmt %cmd-* (find-window, refresh/lock, hooks, bind)
       (:file "dispatch-commands-auto-env") ; show-environment/set-environment helpers + %cmd-* handlers
       (:file "dispatch-commands-auto-pane") ; pane input/prefix runtime commands %cmd-* (send-keys, send-prefix)
       (:file "dispatch-commands-auto-pane-process") ; pane process/pipe runtime commands %cmd-* (respawn, pipe-pane)
       (:file "dispatch-commands-server") ; server-access ACL
       (:file "dispatch-commands-server-customize") ; customize-mode tree browser
       (:file "dispatch-commands-runner"))) ; *arg-command-table* + %run-command-tokens + %run-command-line
     (:module "application/dispatch/control"
      :serial t
      :components
      ((:file "dispatch-control")))       ; control-mode REPL + dispatch-prefix-command
     (:module "dispatch-handlers-2"
      :pathname "application/dispatch/handlers"
      :serial t
      :components
      ((:file "dispatch-handlers")        ; command handler rule table part I (detach through wait-for)
       (:file "dispatch-handlers-copy-mode") ; copy-mode command handler rule table
       (:file "dispatch-handlers-b-menu") ; popup/menu overlays
       (:file "dispatch-handlers-b-server") ; server/env/prompt-history handlers
       (:file "dispatch-handlers-b-prompt") ; prompt-driven dispatch handlers
       (:file "dispatch-handlers-b")     ; command handler rule table part II (break/join through mark/layout)
       (:file "dispatch-handlers-b-tail") ; session/window/misc handlers
       (:file "dispatch-handlers-buffer"))) ; paste-buffer command handler helpers
     (:module "presentation/events"
      :serial t
      :components
      ((:file "events-constants")  ; VT100 / mouse / CSI byte constants (pure data, no logic)
       (:file "events-core")
       (:file "events-loop-bindings") ; extended prefix key-binding table installation
       (:file "events-mouse-status") ; status bar mouse handling
       (:file "events-mouse")   ; mouse event dispatch
       (:file "events-overlay-pager") ; overlay pager escape handler
       (:file "events-keystroke-escape")  ; escape decoder coordinator + CSI-u helpers
       (:file "events-keystroke-escape-mouse") ; X10/SGR mouse escape parsing
       (:file "events-keystroke-escape-prompt") ; prompt-local ESC sequences
       (:file "events-keystroke-escape-keys") ; SS3 / CSI-tilde key-name resolution
       (:file "events-keystroke")          ; CPS state functions: ground-state, after-prefix-state
       (:file "events-copy-mode-dispatch") ; define-copy-mode-vi-rules macro + %dispatch-copy-mode-byte
       (:file "events-keystroke-keys")    ; arrow-key table, modifier/CSI-u helpers, %make-prefix-csi-k
       (:file "events-loop-timers") ; CPS process-byte + escape/repeat timer plumbing + synchronize-panes
       (:file "events-loop")))
     (:module "bootstrap-server"
      :pathname "bootstrap"
      :serial t
      :components
      ((:file "session-registry")  ; session registry + group management
       (:file "server")
       (:file "server-multi")  ; multi-client select-multiplexed serve loop
       (:file "client")
       (:file "main")
       (:file "main-startup"))))))
  ;; Build a standalone binary: (asdf:make :cl-tmux)
  :build-operation "program-op"
  :build-pathname "cl-tmux"
  :entry-point "cl-tmux:main"
  :in-order-to ((test-op (test-op "cl-tmux/test"))))

(defsystem "cl-tmux/test"
  :description "Test suite for cl-tmux"
  :depends-on (:cl-tmux :fiveam)
  :components
  ((:module "tests"
    :serial t
    :components
    ((:file "package")
     (:file "helpers-isolation")
     (:file "helpers-terminal-builders")
     (:file "helpers-render-output")
     (:file "helpers-key-bindings")
     (:file "helpers-overlay-assertions")
     (:file "helpers-session-naming")
     (:file "helpers-pane-fixtures")
     (:file "helpers-pty-runtime")
     (:file "helpers-network-listener")
     (:file "helpers-net-protocol")
     (:file "helpers-options")
     (:file "helpers-process-fixtures")
     (:file "helpers-screen-assertions")
     (:file "helpers-loop-fixtures")
     (:file "helpers-mouse-fixtures")
     (:file "helpers-layout-fixtures")
     (:file "helpers-renderer-fixtures")
     (:file "helpers-session-fixtures")
     (:file "helpers-input-fixtures")
     (:file "helpers-pipe-fixtures")
     (:module "unit"
      :serial t
      :components
      (
       (:module "domain/terminal"
        :serial t
        :components
        ((:file "cell-tests")  ; declares terminal-suite parent; double-width sub-suite
         (:file "screen-tests")  ; construction/p/cell-access/cursor/resize/dirty/sgr-pen/bell — part I
         (:file "screen-tests-b")  ; copy-mode slots, alt-screen, mouse-sgr, response-queue, origin-mode, tab-stops, lock, cells/parser — part II
         (:file "screen-tests-c")  ; screen-clear-dirty, reset-sgr-pen, bell-pending, screen-consume-bell, slots, copy-mode extra slots — part III
         (:file "screen-tests-d")  ; title-stack, cwd, pending-wrap, focus-events, g0/g1/active-g, boolean-slot macro — part IV
         (:file "screen-queue-palette-tests")  ; passthrough/clipboard queues and palette override storage
         (:file "screen-wrap-copy-tests")  ; wrapped rows, ANSI boolean modes, copy-search-dir, rect-select
         (:file "cursor-tests")  ; scroll-region-clamp, set-cursor, direct-action, tab-stops, ri, nel, wide-char, advance — part I
         (:file "cursor-tests-b")  ; %place-wide-char, table-driven, combining-char-p, write-char combining, DEC graphics — part II
         (:file "cursor-tests-c")  ; cursor-ri, cursor-nel, write-char-at-cursor wide, %advance-cursor no-wrap, movement behavioral — part III
         (:file "cursor-tests-d")  ; cursor-lf direct, cursor-nl newline-mode, %materialize-tab-stops, BCE background, boundary table — part IV
         (:file "cursor-tests-e")  ; custom multi-stop %next-tab-stop/%prev-tab-stop via HTS/TBC, table-driven regression — part V
         (:file "scroll-tests")  ; scroll-ops/erase/scroll-region/delete-insert-chars — part I
         (:file "scroll-tests-b")  ; direct-row-primitives, direct-action-erase, constrained-scroll, history-limit — part II
         (:file "scroll-tests-c")  ; direct-line-edit (il/dl), scroll-screen-to-history, DEC-rect (DECERA/DECFRA/DECCRA) — part III
         (:file "scroll-tests-d")  ; clear-scrollback, BCE background via %erase-cell, *scroll-on-clear-function* edge cases — part IV
         (:file "modes-tests")  ; RIS/alt-screen/DECSC/mouse/bracketed-paste/focus — part I
         (:file "modes-tests-b")  ; set-cursor-shape/bell-pending/set-charset/set-screen-title/reset-modes/alt-screen-direct — part II
         (:file "modes-tests-c")  ; screen-invoked-charset/G0-G1, set-screen-cwd, erase-display-mode-3, IRM, LNM, DECSCNM, DECSTR — part III
         (:file "modes-tests-d")  ; mouse DEC private modes, bracketed paste, focus events, app-cursor, auto-wrap, reset-sgr-pen, display-cell — part IV
         (:file "modes-tests-e")  ; decstr-action/decaln-action direct calls, set-ansi-mode/reset-ansi-mode direct calls — part V
         (:file "sgr-tests")  ; sgr suite: fg/bg tables, truecolor, colon SGR, pen-to-sgr-params — part I
         (:file "sgr-tests-b")  ; direct-action-sgr, sgr-extended, extra codes, define-sgr-rules, consume-256-color — part II
         (:file "csi-tests")  ; cursor-movement/DECSCUSR/CBT/SU-SD — part I
         (:file "csi-tests-d")  ; REP/da-response/DECRQM/XTWINOPS/CPR/DA-table/REP-count-zero — part IV
         (:file "csi-tests-b")  ; ECH/DSR/ich-dch/decstbm/execute-csi-direct/%csi-decstbm-params — part II
         (:file "csi-tests-c")  ; csi-unknown-sequences/DECOM/cup-row/enqueue/XTPUSHTITLE/DEC-rect — part III
         (:file "parser-tests")  ; utf8/special/OSC/ESC-hash/private/dec-pm — part I
         (:file "parser-tests-b")  ; combining-chars/ACS/dcs-parsing/xtgettcap/decrqss/ground-state/direct-dcs/direct-osc — part II
         (:file "parser-tests-d")  ; osc-dispatch-edge-cases/osc52/osc7/parser-suite/base64/csi-colon — part IV
         (:file "parser-tests-c")  ; basic-text, inline-predicates, CPS state functions, define-state — part III
         (:file "emulator-tests")))
       (:module "domain/model"
        :serial t
        :components
        ((:file "layout-tests")  ; layout-tree core: leaves/split/resize/collapse/persistence — part I
         (:file "layout-tests-b")  ; named-layout helpers, apply-named-layout — part II
         (:file "layout-tests-c")  ; layout persistence internals: split-bounding-box, node-to-string, read-digits, round-trips — part III
         (:file "layout-tests-d")  ; main-pane-extent table, layout-split defaults, checksum constants, zoomed pane-neighbor guard — part IV
         (:file "layout-geometry-tests")  ; orientation helpers, layout-assign, resize-find-split, pane-at-position, split-child — part I
         (:file "layout-geometry-tests-b")  ; %ranges-overlap-p, pane-center, closest-to-center, define-axis-rules, nested min-extent — part II
         (:file "pane-tests")
         (:file "window-tests")  ; window-relayout/split/resize/zoom/lock/pane-neighbor — part I
         (:file "window-tests-b")  ; apply-named-layout (5 layouts), last-window/move/swap/rotate — part II
         (:file "window-tests-c")  ; find-window-by-name, list-windows-format, auto-rename-from-osc — part III
         (:file "session-tests")
         (:file "session-tests-b")))  ; start-directory, suppress-update-environment, environment helpers, all-panes ordering
       (:module "domain/format"
        :serial t
        :components
        ((:file "format-tests")  ; format expansion — part I (shorthands, brace/conditional, context, window_flags, helpers)
         (:file "format-tests-d")  ; format expansion — part IV (shorthand-table, %expand-brace, %truthy-p, pane/client vars, structural, modifiers)
         (:file "format-tests-b")  ; format expansion — part II (path/substitute/nested/strftime/context/glob/regex)
         (:file "format-tests-c")  ; format expansion — part III (arithmetic/vars/geometry/pane_at_edges/pane-synchronized)
         (:file "format-tests-e")  ; format expansion — part V (content-search, glob-match-p, pane-visible-lines, apply-pad-modifier, window-raw-flags)
         (:file "format-tests-f")))  ; format expansion — part VI (new context keys, modifier chaining, glob/regex match, format variables)
       (:module "domain/model-2"
        :pathname "domain/model"
        :serial t
        :components
        ((:file "target-tests")  ; parse-session/window/pane/target, find-by-target, resolve-target — part I
         (:file "target-tests-b")))  ; %sigil-id, %name-prefix-p, edge cases, table-driven parse-target, multi-digit ids — part II
       (:module "domain/buffer"
        :serial t
        :components
        ((:file "buffer-tests")))
       (:module "infrastructure/control-mode"
        :serial t
        :components
        ((:file "control-mode-tests")))
       (:module "domain/options"
        :serial t
        :components
        ((:file "options-tests")  ; option registry, coercions, boolean defaults, make-option-spec — part I
         (:file "options-tests-b")  ; define-option-accessor, type-coercions, scoped overrides, show-options — part II
         (:file "options-tests-c")))  ; type-coercion dispatch, option-table macro, spec accessors, server options, show-option sorting — part III
       (:module "domain/hooks"
        :serial t
        :components
        ((:file "hooks-tests")  ; hook-event-constants, hook-registry, add/run/remove/clear/list-hooks — part I
         (:file "hooks-tests-b")))  ; command hooks (set-hook), set-hook -u, list-command-hooks, runtime set-hook, show-hooks — part II
      (:module "application/config"
       :serial t
       :components
       ((:file "config-tests")
         (:file "config-key-description-tests")
         (:file "config-key-table-runtime-tests")
         (:file "config-directives-tests")  ; directive parsing — part I (suite, bindable commands, basic apply/set directives)
         (:file "config-load-tests")  ; directive parsing — load strings/streams/files and config paths
         (:file "config-bind-directive-tests")  ; directive parsing — bind/unbind, notes, brace blocks, sequences
         (:file "config-key-token-tests")  ; directive parsing — key tokens, command names, listing labels
         (:file "config-directives-tests-c")  ; directive parsing — part III (load-config-file, command-keyword, parse-bind-args, key-table edge cases)
         (:file "config-directives-tests-b")  ; directive parsing — part II (%parse-bind-args, tokenizer, set aliases, server flag, terminal option routing)
         (:file "config-source-run-tests")  ; directive parsing — source-file, run-shell, path expansion
         (:file "config-source-file-tests")  ; directive parsing — source-file flags, glob expansion, missing diagnostics
         (:file "config-preprocessor-environment-tests")  ; directive parsing — preprocessor, environment, key-table side effects
         (:file "config-directives-tests-d")  ; directive parsing — part IV (set-g-status-off, bind-key-n, load-config, %elif, line-continuation, if-shell)
         (:file "config-directives-tests-e")))  ; directive parsing — part V (macro registry, env-set-p, key-table edge cases, remaining bind/set directives)
       (:module "presentation/renderer"
        :serial t
        :components
        ((:file "renderer-format-tests")  ; SGR codes, style tokens, border-color, cursor-shape, palette bounds — part I
         (:file "renderer-format-tests-b")  ; all-attrs table, attrs2, ul-color, style-token/emit remaining, parse-style, border-charset — part II
         (:file "renderer-pane-tests")  ; render-pane content/borders/window-style — part I
         (:file "renderer-pane-tests-b")  ; %clock-digit-rows, %render-v-separator, border/pane edge cases — part II
         (:file "renderer-pane-tests-c")  ; %apply-border-style branches, draw-clock, render-pane-clock-mode, draw-pane-number, in-sel-branch — part III
         (:file "renderer-tests")  ; renderer — part I (status-bar, render-session, clear-display, status-indicators, window-list)
         (:file "renderer-tests-d")  ; renderer — part IV (per-window options, alert-tab-styles, status-bar-line, overlay, DECTCEM)
         (:file "renderer-tests-b")  ; renderer — part II (status-bar, status-position, BEL rendering, status-left-expanded)
         (:file "renderer-tests-f")  ; renderer — part VI (parse-style-string, style-to-sgr, status-length, window-status-format, render-popup/menu)
         (:file "renderer-tests-c")  ; renderer — part III (mouse/focus/keys, lock-screen, justify, cursor-shape, zoom-suppression)
         (:file "renderer-tests-e")  ; renderer — part V (%clamp-status-segment, cursor-shape in output, status-bar-line gap, inline-style, bell relay)
         (:file "renderer-tests-g")))  ; renderer — part VII (%split-align-attr, %status-align-buckets, %status-bar-default-segments, %content-search-match-p flag matrix)
       (:module "application/dispatch"
        :serial t
        :components
        ((:file "dispatch-suite-support")  ; dispatch-suite definition and shared support macros
         (:file "dispatch-tests-core-navigation") ; core dispatch navigation helpers and window/pane select
         (:file "dispatch-tests-window-rename-prompt")  ; rename-window prompt dispatch
         (:file "dispatch-tests-copy-mode-send-keys")  ; copy-mode and send-keys -X dispatch
         (:file "dispatch-tests-detach-kill-prefix")  ; detach, kill, and prefix routing
         (:file "dispatch-tests-core-screen-helpers")  ; %active-screen, active window/pane, session/window helpers, overlay
         (:file "dispatch-tests-core-command-runtime")  ; run-command-hooks, command table, handlers, target-context, window cycling helpers
         (:file "dispatch-tests-c")  ; core dispatch — part V (focus events window-switch, list-keys, select-pane, zoom, list-windows/sessions)
         (:file "dispatch-tests-pane-window-prefix")  ; pane/window/prefix dispatch: swap, confirm, send-prefix, paste-buffer
         (:file "dispatch-tests-command-prompt-runtime")  ; command-prompt basic flow and display-message command-line runtime
         (:file "dispatch-tests-option-command-line")  ; set-option command-line parsing and mutation
         (:file "dispatch-tests-command-prompt-templates")  ; command-prompt templates, copy-mode source pane, focus behavior
         (:file "dispatch-tests-option-scope-runtime")  ; set-option scope routing and runtime side effects
         (:file "dispatch-tests-runtime-rename-respawn")  ; runtime bind/unbind, rename, and respawn
         (:file "dispatch-tests-hooks-command-listing")  ; set-hook dispatch and list-commands rendering
         (:file "dispatch-tests-commands-targeting")  ; flag parsing, select/kill/link/swap/move target commands, source-file
         (:file "dispatch-tests-commands-shell-messages")  ; if-shell -F, named dispatch helper, show-messages
         (:file "dispatch-tests-commands-f")  ; display-message-logs, clock-mode, capture-pane, send-keys, choose-tree, confirm-before, paste-to-pane, format-tree-entry — part VI
         (:file "dispatch-tests-state-option-navigation")  ; state toggles, show-options, last-window/last-pane, respawn/pipe-pane
         (:file "dispatch-tests-list-format-helpers")  ; window/session list formatting, copy-mode-call, kill-result helper
         (:file "dispatch-tests-popup-menu-runtime")  ; popup overlay, display-popup, display-menu runtime
         (:file "dispatch-tests-session-presence")  ; has-session prompt and argument validation
         (:file "dispatch-tests-pane-zoom-resize")  ; pane zoom, navigation, and resize dispatch cases
         (:file "dispatch-tests-copy-mode-mouse-x")  ; copy-mode mouse entry, indicator, and -X prompt cases
         (:file "dispatch-tests-display-menu-list-keys")  ; display-menu selection and list-keys notes
         (:file "dispatch-tests-format-vars")  ; session/pane/window/client format variables
         (:file "dispatch-tests-capture-terminal-rendering")  ; capture-pane and terminal line-size rendering
         (:file "dispatch-tests-layout-winlink-order")  ; select-layout undo and per-session winlink ordering
         (:file "dispatch-tests-commands-e")  ; switch-client, last-session, new-session, kill-session, mark-pane, next-layout, bind/unbind-key, list/choose-buffer, wait-for — part V
         (:file "dispatch-tests-commands-c")  ; helper tests, on-submit paths, cyclic nav, break/join/run/if — part III
         (:file "dispatch-tests-commands-d")  ; has-session, find-window/select-window-prompt, move/swap-window, bind/unbind-key, kill-pane, split, new-window — part IV
         (:file "dispatch-tests-session")  ; copy-mode paging, with-active-pane, format-menu, named-layout, kill-result, command-dispatch-outcome — part I
         (:file "dispatch-tests-session-listing")  ; named-command table, select-layout, list clients/sessions/panes/windows, list-commands
         (:file "dispatch-tests-session-window-lifecycle")  ; split-window, new-window, server commands, prefix, detached variants
         (:file "dispatch-tests-session-environment-hooks")  ; hidden environment variables, client size, scoped hooks
         (:file "dispatch-tests-client-session-control")  ; attach, detach, refresh, lock, and move-pane dispatch
         (:file "dispatch-tests-window-resize-lifecycle")  ; resize-window, respawn-window, and select-layout dispatch
         (:file "dispatch-tests-environment-set")  ; set-environment dispatch
         (:file "dispatch-tests-environment-show-overlays")  ; show-environment and overlay dispatch
         (:file "dispatch-tests-session-flag-targets")  ; flag parsers, target resolvers, new-window/split-window/new-session command cases
         (:file "dispatch-tests-session-b-tail")  ; layout names, target resolvers, display-message, new-session size parsing — part IIb
         (:file "dispatch-tests-session-c")  ; options, move-window, new-session -s/-A/-t, control-mode REPL — part III
         (:file "dispatch-tests-session-f")  ; new-session duplicate, grouped sessions, control-mode notifications, server-lifecycle, %output relay — part VI
         (:file "dispatch-tests-session-d")  ; display-popup, send-keys -N/-H, capture-pane — part IV
         (:file "dispatch-tests-session-d-tail")))  ; named paste-buffer, join-pane, wait-for-arg — part IVb
       (:module "presentation/events"
        :serial t
        :components
        ((:file "events-tests")  ; keystroke pipeline — part I (suite, escape, process-byte, prompt, copy-mode vi-nav)
         (:file "events-mouse-tests")  ; mouse dispatch, X10, middle-click paste, defaults
         (:file "events-tests-f")  ; keystroke pipeline — part VI (PageUp/Down, prefix-arrow, send-prefix, modifier+arrow, meta/alt)
         (:file "events-tests-b")  ; locked-session, drag/modifier, copy-mode cursor, vi nav — part II
         (:file "events-tests-h")  ; byte-constants, make-input-state, forward-octets, maybe-rename-window — part VIII
         (:file "events-tests-c")  ; app-cursor-keys, prompt-key, copy-mode nav, SGR, border-check — part III
         (:file "events-tests-e")  ; status-col, SGR-nil, copy-nav, flush-esc, reset-repeat, mouse/key-table — part IV
         (:file "events-csi-u-tests")  ; CSI-u extended key parsing and dispatch
         (:file "events-tests-d")  ; app-cursor-keys (ss3), new bindings, :mark-pane, root table, fn-keys — part V
         (:file "events-tests-g")  ; select-layout-spread, new key bindings, choose-window, mouse-reporting, tmux defaults — part VII
         (:file "events-tests-i")  ; copy-mode v-select, middle-cursor-jump, mouse X10, CSI-tilde outside mode, CSI-3byte — part IX
         (:file "events-tests-j")  ; vi-normal-key dispatch, %dispatch-menu-key, %rename-from-osc-title — part X
         (:file "mouse-tests")))
       (:module "application/commands"
        :serial t
        :components
        ((:file "commands-tests")  ; resize-pane, scroll, kill-pane, select/rename, begin-sel/yank/other-end — part I
         (:file "commands-tests-e")  ; copy-mode-clear-sel, WORD-motion, select-word, move-cursor — part II
         (:file "commands-tests-f")  ; rename-window, kill-window, run/if-shell, selection-text, swap-pane — part III
         (:file "commands-tests-m")  ; swap-pane (cont), capture-pane, shift-line-wrapped, copy-mode scroll, resize-pane, split-window — part XIII
         (:file "commands-tests-n")  ; copy-mode-begin-selection multi-row, yank, other-end — part XIV
         (:file "commands-tests-b")  ; copy-mode line-start/end, high/middle/low, scroll noop guards, word motions, top/bottom — part IV
         (:file "commands-tests-k")  ; begin-line-selection, copy-end-of-line (D), copy-line (Y), search-forward/backward, wrap-search — part XI
         (:file "commands-tests-g")  ; send-keys, key-name, tokenize, kill-window-mru, join-pane — part V
         (:file "commands-tests-h")  ; copy-mode-exit, break-pane, clear-history, rotate — part VI
         (:file "commands-window-navigation-tests")  ; find-window and next/previous/last-window command behavior
         (:file "commands-tests-c")  ; pipe-pane, virtual-row, timeout, scroll helpers, word/paragraph nav — part VII
         (:file "commands-tests-o")  ; selection-bounds scrollback, word/paragraph nav, scroll-middle — part XV
         (:file "commands-tests-j")  ; join-pane helpers, resize-pane up, noop guards, search, scroll, extract-chars, row-string — part X
         (:file "commands-tests-d")  ; rename/select hooks, server-access, customize-mode, begin-line-selection multi-row — part VIII
         (:file "commands-tests-l")  ; copy-mode copy-line/copy-end-of-line, with-shell-timeout, kill hooks, toggle-rect, append-sel, copy-pipe, renumber-windows — part XII
         (:file "commands-tests-i")  ; rectangle-sel, run-copy-cmd, set-cursor, send-keys-l, jump-to-char, goto-line, search-incr — part IX
         (:file "commands-tests-p")))  ; copy-selection-no-cancel/no-clear, pipe family, copy-pipe-no-clear/line/-and-cancel, rectangle-on/off, cursor-down-and-cancel, scroll-to-mouse, copy-*-and-cancel, last-jump — part XVII
       (:module "presentation/prompt"
        :serial t
        :components
        ((:file "overlay-tests")
         (:file "prompt-tests")
         (:file "prompt-tests-wiring")))
       (:module "application/config-2"
        :pathname "application/config"
        :serial t
        :components
        ((:file "config-tests-defaults")))
       (:module "infrastructure/net"
        :serial t
        :components
        ((:file "protocol-tests")  ; octets/frame-header/round-trips/msg-command — part I
         (:file "protocol-tests-b")  ; read-u32, split-on-nul, encode/decode-command-payload, target-field-p — part II
         (:file "transport-tests")  ; round-trips, with-incoming-frame, %read-exact — part I
         (:file "transport-tests-b")))  ; validation, security boundaries, CPS-phase direct coverage — part II
       (:module "bootstrap"
        :serial t
        :components
        ((:file "server-tests")
         (:file "server-tests-b")))  ; list-sessions, rename-session, switch-client, last-session — part II
       (:module "infrastructure/pty"
        :serial t
        :components
        ((:file "pty-ffi-tests")
         (:file "pty-rawmode-tests")
         (:file "pty-tests")))  ; PTY argument-assembly unit tests (spawn helpers)
       (:module "infrastructure/input"
        :serial t
        :components
        ((:file "input-tests")))
       (:module "bootstrap-2"
        :pathname "bootstrap"
        :serial t
        :components
        ((:file "runtime-tests")  ; globals, pane-reader-loop, monitor-activity/silence, alert-action — part I
         (:file "runtime-prompt-history-io-tests")
         (:file "runtime-message-log-core-tests")
         (:file "runtime-tests-c")  ; stop-reader-threads, add-message-log, add-prompt-history, wait-for-channel — part III
         (:file "runtime-tests-b")  ; add-message-log table-driven, add-prompt-history, wait-for-channel — part II
         (:file "main-tests")
         (:file "main-entry-tests")
         (:file "main-environment-tests")
         (:file "main-command-argument-tests")))
       (:module "feature"
        :serial t
        :components
        ((:file "advanced-tests")))  ; cross-layer acceptance suite (break/pipe/sync, layout, lock, session-groups)
       ))
     
     (:module "integration"
      :serial t
      :components
      ((:file "net-tests")
       (:file "server-multi-tests")
       (:file "server-multi-command-client-tests")
       (:file "pty-tests")
       (:file "client-tests")))
     (:file "suite"))))
  ;; Run with: (asdf:test-system :cl-tmux)
  :perform (test-op (op c)
             (symbol-call :cl-tmux/test :run-tests)))
