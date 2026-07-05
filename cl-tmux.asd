#.(progn
    (load (merge-pathnames "system/asdf-test-components.lisp" *load-truename*))
    nil)

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
         (:file "config-directives-bind-sequences") ; brace/semicolon splitting for bind payloads
         (:file "config-directives-bind-parse") ; bind-key argument parsing + command resolution
         (:file "config-directives-bind-dispatch") ; bind/unbind directive dispatch
         (:file "config-directives-set")     ; fixed-arity table + set-option flag handling/routing
       (:file "config-option-side-effects") ; option runtime side effects + set-hook directive
       (:file "config-directives-runtime-services") ; shared shell execution services
       (:file "config-directives-environment") ; set-environment handler
       (:file "config-directives-if-shell") ; if-shell handler
       (:file "config-directives-run-shell") ; run-shell handler
       (:file "config-directives-source-file") ; source-file handler
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
       (:file "screen")       ; screen struct (DATA layer): defstruct, grid helpers
       (:file "screen-metadata") ; screen capture/palette metadata mutation helpers
       (:file "screen-resize") ; screen resize logic; depends on metadata reset helpers
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
       (:file "csi-replies")    ; CSI reply-queue helpers (DSR/DA/CPR/DECRQM/XTWINOPS); loads before csi
       (:file "csi-parameters") ; CSI parameter-to-domain-value translation
       (:file "csi-dispatch")   ; DEFINE-CSI-RULES macro that emits EXECUTE-CSI
       (:file "csi")            ; declarative CSI action rule table
       (:file "parser-dcs")    ; DCS passthrough/XTGETTCAP/DECRQSS helpers (loads before parser)
       (:file "parser-core")   ; parser byte predicates + Prolog-like DEFINE-STATE macro
       (:file "parser-csi")    ; CSI continuation builder and byte-class predicates
       (:file "parser-utf8")   ; UTF-8 continuation builder and byte predicates
       (:file "parser")        ; named CPS state-machine skeleton
       (:file "parser-osc-clipboard") ; OSC 52 Base64 helpers + clipboard callback
       (:file "parser-osc-uri")       ; OSC 7/8 URI decoding helpers
       (:file "parser-osc-color")      ; OSC color and palette helpers
       (:file "parser-osc-dispatch")   ; OSC command parsing + dispatch rules
       (:file "parser-osc")            ; OSC accumulator + dispatcher state machine
       (:file "emulator")))
     (:module "domain/model"
      :serial t
      :components
      ((:file "pane-core")         ; leaf PTY data and feed helpers
       (:file "pane-geometry")     ; geometry update + PTY/screen resize helpers
       (:file "layout")            ; tree structure + traversal (uses pane-reposition)
       (:file "layout-persistence") ; layout string serialization
       (:file "layout-geometry")    ; rectangle assignment + resize helpers (uses pane-id, pane-x/y/w/h)
       (:file "window-core")        ; window struct + core ops (split/constants)
       (:file "window-tree")        ; tree mutation + relayout/remove helpers
       (:file "window-operations")  ; window resize/rotate/zoom (uses window + layout helpers)
       (:file "window-neighbor") ; directional pane navigation (uses window-panes)
       (:file "window-layout")   ; named layouts (apply-named-layout, uses window accessors)
       (:file "session")             ; session lifecycle: struct + windows + touch + all-panes
       (:file "session-environment-process")   ; update-env defaults + process env helpers
       (:file "session-environment-overlay")    ; session overlay tables and env access
       (:file "session-environment-child")      ; child env snapshot assembly
       (:file "pane-spawn")))                   ; PTY-backed pane factory + respawn
     (:module "domain/format"
      :serial t
      :components
      ((:file "format-helpers")    ; tmux-style format: pure data helpers + shorthand/arithmetic tables
       (:file "format-strftime")   ; strftime support (#{t:format}): %strftime-letter-p + formatting engine
       (:file "format-modifiers")  ; value-modifiers (#{b:}/#{d:}/#{=N:}/#{pN:}/#{s///:}/#{q:}/#{E:})
       (:file "format-search")     ; glob/regex matching + pane content search (#{m:}/#{m/r:}/#{C:})
       (:file "format-operators")  ; comparison and logical operators (#{==:}/#{!=:}/#{||:}/#{&&:})
       (:file "format-iteration")  ; W:/S:/P: window/session/pane iteration expanders
       (:file "format-shell-command") ; bounded shell-command port for #(command) expansion
       (:file "format-delimiters") ; delimiter scanning plus #[...] and #(command) ports
       (:file "format-brace")      ; core #{...} modifier/operator expansion
       (:file "format-engine")     ; CPS processor and expand-format public entry points
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
      ((:file "commands-copy-mode")      ; copy-mode core: enter/exit, scroll, prompts, selection state
       (:file "commands-copy-mode-cursor") ; cursor movement and viewport edge scrolling
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
      ((:file "commands")               ; shared command execution helpers
       (:file "commands-capture-pane")  ; capture-pane snapshot/rendering
       (:file "commands-pipe-pane")     ; pipe-pane process I/O lifecycle
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
      ((:file "dispatch-commands-input")    ; shared flag parser and command-input macros
       (:file "dispatch-commands-target")   ; shared target resolution helpers
       (:file "dispatch-commands-prompt")   ; command-prompt substitution/CPS helpers
       (:file "dispatch-commands")          ; display/prompt/pane %cmd-* handlers
       (:file "dispatch-commands-flag-accessors") ; generated command flag accessors
       (:file "dispatch-commands-buffer")   ; paste-buffer %cmd-* handlers
       (:file "dispatch-commands-buffer-ui") ; popup/menu/confirm/list-keys %cmd-* handlers
       (:file "dispatch-commands-copy-mode-entry") ; copy-mode entry %cmd-* handler
       (:file "dispatch-commands-option")   ; set-option (CPS) + show-options %cmd-*
       (:file "dispatch-commands-option-pane") ; rename/select %cmd-* handlers (loads option-pane-window/pane fragments)
       (:file "dispatch-commands-lifecycle") ; kill/link/unlink/swap/move/source-file %cmd-*
       (:file "dispatch-commands-pane")   ; layout/window/pane helpers + *key-table*
       (:file "dispatch-commands-session-service") ; session switching/destruction services
       (:file "dispatch-commands-client-session") ; switch/attach/detach %cmd-* handlers
       (:file "dispatch-commands-session-create") ; new-session %cmd-* handler
       (:file "dispatch-commands-session-destroy") ; kill-session %cmd-* handler
       (:file "dispatch-commands-window-resize") ; resize-window %cmd-* handler
       (:file "dispatch-commands-pane-x-facts") ; copy-mode -X canonical fact tables
       (:file "dispatch-commands-pane-x") ; send-keys -X dispatch logic
       (:file "dispatch-commands-shell")   ; run-shell and if-shell %cmd-* handlers
       (:file "dispatch-commands-capture-pane") ; capture-pane %cmd-* handler
       (:file "dispatch-commands-pane-ops") ; resize/join/break/clear/rotate %cmd-* handlers
       (:file "dispatch-commands-list-data") ; *command-usage-table* pure data (canonical-name → usage-flags)
       (:file "dispatch-commands-list-registry") ; list-commands registry projection
       (:file "dispatch-commands-list-overlay") ; list overlay presentation helpers
       (:file "dispatch-commands-list-query") ; list read-model queries and formatters
       (:file "dispatch-commands-list-parser") ; list-* tmux-compatible arg parser
       (:file "dispatch-commands-list")    ; list-sessions/windows/panes/clients %cmd-* handlers
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
       (:file "events-mouse-state") ; mouse dispatch dynamic state and pure counters
       (:file "events-mouse-layout") ; pane-border hit testing and drag resize
       (:file "events-mouse-bindings") ; mouse key names, actions, and context
       (:file "events-mouse-passthrough") ; pane X10/SGR mouse passthrough
       (:file "events-mouse-actions") ; built-in mouse actions
       (:file "events-mouse-dispatch") ; mouse event dispatch coordinator
       (:file "events-overlay-pager") ; overlay pager escape handler
       (:file "events-key-names") ; arrow/key-name fact tables and CSI-u parsing
       (:file "events-key-bindings") ; key-table lookup and binding execution
       (:file "events-keystroke-escape")  ; escape decoder coordinator + CSI-u helpers
       (:file "events-keystroke-escape-mouse") ; X10/SGR mouse escape parsing
       (:file "events-keystroke-escape-prompt") ; prompt-local ESC sequences
       (:file "events-keystroke-escape-keys") ; SS3 / CSI-tilde key-name resolution
       (:file "events-keystroke")          ; CPS state functions: ground-state, after-prefix-state
       (:file "events-prefix-csi-continuation") ; post-prefix CSI/SS3 CPS continuation
       (:file "events-keystroke-repeat-states") ; prefix/root repeat CPS states
       (:file "events-loop-timers") ; CPS process-byte + escape/repeat timer plumbing + synchronize-panes
       (:file "events-loop")))
     (:module "bootstrap-server"
      :pathname "bootstrap"
      :serial t
      :components
      ((:file "session-registry")  ; session registry + group management
       (:file "server")
       (:file "server-multi")  ; multi-client client registry + dispatch helpers
       (:file "server-multi-loop") ; multi-client select-multiplexed serve loop
       (:file "client")
       (:file "main")
       (:file "main-startup-socket") ; socket discovery + server auto-start helpers
       (:file "main-startup-forwarding") ; command-client forwarding helpers + generated commands
       (:file "main-startup"))))))
  ;; Build a standalone binary: (asdf:make :cl-tmux)
  :build-operation "program-op"
  :build-pathname "cl-tmux"
  :entry-point "cl-tmux:main"
  :in-order-to ((test-op (test-op "cl-tmux/test"))))

(defsystem "cl-tmux/test"
  :description "Test suite for cl-tmux"
  :depends-on (:cl-tmux :fiveam)
  :components #.(symbol-value (find-symbol "*CL-TMUX-TEST-COMPONENTS*" :cl-user))
  ;; Run with: (asdf:test-system :cl-tmux)
  :perform (test-op (op c)
             (symbol-call :cl-tmux/test :run-tests)))
