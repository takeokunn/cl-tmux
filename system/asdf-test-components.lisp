(in-package #:cl-user)

(defparameter *cl-tmux-test-components*
  '((:module "tests"
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
       ((:module "domain/terminal"
         :serial t
         :components
         ((:file "cell-tests") ; declares terminal-suite parent; cell data, width, combining
          (:file "cell-display-tests") ; DEC graphics, BCE, constants, hyperlink
          (:file "screen-tests") ; construction/p/cell-access/cursor/resize/dirty/sgr-pen/bell - part I
          (:file "screen-tests-b") ; copy-mode slots, alt-screen, mouse-sgr, response-queue, origin-mode, tab-stops, lock, cells/parser - part II
          (:file "screen-tests-c") ; screen-clear-dirty, reset-sgr-pen, bell-pending, screen-consume-bell, slots, copy-mode extra slots - part III
          (:file "screen-tests-d") ; title-stack, cwd, pending-wrap, focus-events, g0/g1/active-g, boolean-slot macro - part IV
          (:file "screen-queue-palette-tests") ; passthrough/clipboard queues and palette override storage
          (:file "screen-wrap-copy-tests") ; wrapped rows, ANSI boolean modes, copy-search-dir, rect-select
          (:file "cursor-tests") ; scroll-region-clamp, set-cursor, direct-action, tab-stops, ri, nel, wide-char, advance - part I
          (:file "cursor-tests-b") ; %place-wide-char, table-driven, combining-char-p, write-char combining, DEC graphics - part II
          (:file "cursor-tests-c") ; cursor-ri, cursor-nel, write-char-at-cursor wide, %advance-cursor no-wrap, movement behavioral - part III
          (:file "cursor-tests-d") ; cursor-lf direct, cursor-nl newline-mode, %materialize-tab-stops, BCE background, boundary table - part IV
          (:file "cursor-tests-e") ; custom multi-stop %next-tab-stop/%prev-tab-stop via HTS/TBC, table-driven regression - part V
          (:file "scroll-tests") ; scroll-ops/erase/scroll-region/delete-insert-chars - part I
          (:file "scroll-tests-b") ; direct-row-primitives, direct-action-erase, constrained-scroll, history-limit - part II
          (:file "scroll-tests-c") ; direct-line-edit (il/dl), scroll-screen-to-history, DEC-rect (DECERA/DECFRA/DECCRA) - part III
          (:file "scroll-tests-d") ; clear-scrollback, BCE background via %erase-cell, *scroll-on-clear-function* edge cases - part IV
          (:file "modes-tests") ; RIS/alt-screen/DECSC/mouse/bracketed-paste/focus - part I
          (:file "modes-tests-b") ; set-cursor-shape/bell-pending/set-charset/set-screen-title/reset-modes/alt-screen-direct - part II
          (:file "modes-tests-c") ; screen-invoked-charset/G0-G1, set-screen-cwd, erase-display-mode-3, IRM, LNM, DECSCNM, DECSTR - part III
          (:file "modes-tests-d") ; mouse DEC private modes, bracketed paste, focus events, app-cursor, auto-wrap, reset-sgr-pen, display-cell - part IV
          (:file "modes-tests-e") ; decstr-action/decaln-action direct calls, set-ansi-mode/reset-ansi-mode direct calls - part V
          (:file "sgr-tests") ; sgr suite: fg/bg tables, truecolor, colon SGR, pen-to-sgr-params - part I
          (:file "sgr-tests-b") ; direct-action-sgr, sgr-extended, extra codes, define-sgr-rules, consume-256-color - part II
          (:file "csi-tests") ; cursor-movement/DECSCUSR/CBT/SU-SD - part I
          (:file "csi-tests-d") ; REP/da-response/DECRQM/XTWINOPS/CPR/DA-table/REP-count-zero - part IV
          (:file "csi-tests-b") ; ECH/DSR/ich-dch/decstbm/execute-csi-direct/%csi-decstbm-params - part II
          (:file "csi-tests-c") ; csi-unknown-sequences/DECOM/cup-row/enqueue/XTPUSHTITLE/DEC-rect - part III
          (:file "parser-tests") ; utf8/special/OSC/ESC-hash/private/dec-pm - part I
          (:file "parser-tests-b") ; combining-chars/ACS/dcs-parsing/xtgettcap/decrqss - part II
          (:file "parser-control-state-tests") ; ground-state/escape-state/direct-dcs/G2-G3 shifts
          (:file "parser-tests-d") ; osc-dispatch-edge-cases/osc52/osc7/parser-suite/base64/csi-colon - part IV
          (:file "parser-tests-c") ; basic-text, inline-predicates, CPS state functions, define-state - part III
          (:file "emulator-tests")))
        (:module "domain/model"
         :serial t
         :components
         ((:file "layout-tests") ; layout-tree core: leaves/split/resize/collapse/persistence - part I
          (:file "layout-tests-b") ; named-layout helpers, apply-named-layout - part II
          (:file "layout-tests-c") ; layout persistence internals: split-bounding-box, node-to-string, read-digits, round-trips - part III
          (:file "layout-tests-d") ; main-pane-extent table, layout-split defaults, checksum constants, zoomed pane-neighbor guard - part IV
          (:file "layout-geometry-tests") ; orientation helpers, layout-assign, resize-find-split, pane-at-position, split-child - part I
          (:file "layout-geometry-tests-b") ; %ranges-overlap-p, pane-center, closest-to-center, define-axis-rules, nested min-extent - part II
          (:file "pane-tests")
          (:file "window-tests") ; window-relayout/split/resize/private tree helpers - part I
          (:file "window-neighbor-tests") ; pane-neighbor directional lookup
          (:file "window-zoom-tests") ; even-layout, zoom toggle, lock slot
          (:file "window-tests-b") ; apply-named-layout (5 layouts), last-window/move/swap/rotate - part II
          (:file "window-tests-c") ; find-window-by-name, list-windows-format, auto-rename-from-osc - part III
          (:file "session-tests")
          (:file "session-tests-b"))) ; start-directory, suppress-update-environment, environment helpers, all-panes ordering
        (:module "domain/format"
         :serial t
         :components
         ((:file "format-tests") ; format expansion - part I (shorthands, brace/conditional, context, window_flags, helpers)
          (:file "format-tests-d") ; format expansion - part IV (shorthand-table, %expand-brace, %truthy-p, pane/client vars)
          (:file "format-structural-tests") ; structural pane/session/window/terminal format variables
          (:file "format-modifier-tests") ; truncation/logical/quote/char/path modifiers
          (:file "format-tests-b") ; format expansion - part II (path/substitute/nested/strftime/context/glob/regex)
          (:file "format-tests-c") ; format expansion - part III (arithmetic/vars/geometry/pane_at_edges/pane-synchronized)
          (:file "format-tests-e") ; format expansion - part V (content-search, glob-match-p, pane-visible-lines, apply-pad-modifier, window-raw-flags)
          (:file "format-tests-f"))) ; format expansion - part VI (new context keys, modifier chaining, glob/regex match, format variables)
        (:module "domain/model-2"
         :pathname "domain/model"
         :serial t
         :components
         ((:file "target-tests") ; parse-session/window/pane/target, find-by-target, resolve-target - part I
          (:file "target-tests-b"))) ; %sigil-id, %name-prefix-p, edge cases, table-driven parse-target, multi-digit ids - part II
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
         ((:file "options-tests") ; option registry, coercions, boolean defaults, make-option-spec - part I
          (:file "options-display-tests") ; scope/display presence, array names, value display quoting
          (:file "options-tests-b") ; define-option-accessor, type-coercions, scoped overrides, show-options - part II
          (:file "options-tests-c"))) ; type-coercion dispatch, option-table macro, spec accessors, server options, show-option sorting - part III
        (:module "domain/hooks"
         :serial t
         :components
         ((:file "hooks-tests") ; hook-event-constants, hook-registry, add/run/remove/clear/list-hooks - part I
          (:file "hooks-tests-b"))) ; command hooks (set-hook), set-hook -u, list-command-hooks, runtime set-hook, show-hooks - part II
        (:module "application/config"
         :serial t
         :components
         ((:file "config-tests")
          (:file "config-key-description-tests")
          (:file "config-key-table-runtime-tests")
          (:file "config-directives-tests") ; directive parsing - part I (suite, bindable commands, basic apply/set directives)
          (:file "config-load-tests") ; directive parsing - load strings/streams/files and config paths
          (:file "config-bind-directive-tests") ; directive parsing - bind/unbind, notes, brace blocks, sequences
          (:file "config-key-token-tests") ; directive parsing - key tokens, command names, listing labels
          (:file "config-directives-tests-c") ; directive parsing - part III (load-config-file, command-keyword, parse-bind-args, key-table edge cases)
          (:file "config-directives-tests-b") ; directive parsing - part II (%parse-bind-args, tokenizer, set aliases, server flag, terminal option routing)
          (:file "config-source-run-tests") ; directive parsing - source-file, run-shell, path expansion
          (:file "config-source-file-tests") ; directive parsing - source-file flags, glob expansion, missing diagnostics
          (:file "config-preprocessor-environment-tests") ; directive parsing - preprocessor, environment, key-table side effects
          (:file "config-directives-tests-d") ; directive parsing - part IV (set-g-status-off, bind-key-n, load-config, %elif, line-continuation, if-shell)
          (:file "config-directives-real-world-tests") ; directive parsing - real-world config fixture
          (:file "config-directives-tests-e"))) ; directive parsing - part V (macro registry, env-set-p, key-table edge cases, remaining bind/set directives)
        (:module "presentation/renderer"
         :serial t
         :components
         ((:file "renderer-format-tests") ; SGR codes, style tokens, border-color, cursor-shape, palette bounds - part I
          (:file "renderer-format-tests-b") ; all-attrs table, attrs2, ul-color, style-token/emit remaining, parse-style, border-charset - part II
          (:file "renderer-pane-tests") ; render-pane content/borders/window-style - part I
          (:file "renderer-pane-tests-b") ; %clock-digit-rows, %render-v-separator, border/pane edge cases - part II
          (:file "renderer-pane-tests-c") ; %apply-border-style branches, draw-clock, render-pane-clock-mode, draw-pane-number, in-sel-branch - part III
          (:file "renderer-tests") ; renderer - part I (status-bar, render-session, clear-display, status-indicators, window-list)
          (:file "renderer-tests-d") ; renderer - part IV (per-window options, alert-tab-styles, status-bar-line, overlay, DECTCEM)
          (:file "renderer-tests-b") ; renderer - part II (status-bar, status-position, BEL rendering, status-left-expanded)
          (:file "renderer-tests-f") ; renderer - part VI (parse-style-string, style-to-sgr, status-length, window-status-format, render-popup/menu)
          (:file "renderer-tests-c") ; renderer - part III (mouse/focus/keys, lock-screen, justify, cursor-shape, zoom-suppression)
          (:file "renderer-tests-e") ; renderer - part V (%clamp-status-segment, cursor-shape in output, status-bar-line gap, inline-style, bell relay)
          (:file "renderer-tests-g"))) ; renderer - part VII (%split-align-attr, %status-align-buckets, %status-bar-default-segments, %content-search-match-p flag matrix)
        (:module "application/dispatch"
         :serial t
         :components
         ((:file "dispatch-suite-support") ; dispatch-suite definition and shared support macros
          (:file "dispatch-tests-core-navigation") ; core dispatch navigation helpers and window/pane select
          (:file "dispatch-tests-window-rename-prompt") ; rename-window prompt dispatch
          (:file "dispatch-tests-copy-mode-send-keys") ; copy-mode and send-keys -X dispatch
          (:file "dispatch-tests-detach-kill-prefix") ; detach, kill, and prefix routing
          (:file "dispatch-tests-core-screen-helpers") ; %active-screen, active window/pane, session/window helpers, overlay
          (:file "dispatch-tests-core-command-runtime") ; run-command-hooks, command table, handlers, target-context, window cycling helpers
          (:file "dispatch-tests-c") ; core dispatch - part V (focus events window-switch, list-keys, select-pane, zoom, list-windows/sessions)
          (:file "dispatch-tests-pane-window-prefix") ; pane/window/prefix dispatch: swap, confirm, send-prefix, paste-buffer
          (:file "dispatch-tests-command-prompt-runtime") ; command-prompt basic flow and display-message command-line runtime
          (:file "dispatch-tests-option-command-line") ; set-option command-line parsing and mutation
          (:file "dispatch-tests-command-prompt-templates") ; command-prompt templates, copy-mode source pane, focus behavior
          (:file "dispatch-tests-option-scope-runtime") ; set-option scope routing and runtime side effects
          (:file "dispatch-tests-runtime-rename-respawn") ; runtime bind/unbind, rename, and respawn
          (:file "dispatch-tests-hooks-command-listing") ; set-hook dispatch and list-commands rendering
          (:file "dispatch-tests-commands-targeting") ; flag parsing, select/kill/link/swap/move target commands, source-file
          (:file "dispatch-tests-commands-shell-messages") ; if-shell -F, named dispatch helper, show-messages
          (:file "dispatch-tests-commands-f") ; display-message-logs, clock-mode, capture-pane, send-keys, choose-tree, confirm-before, paste-to-pane, format-tree-entry - part VI
          (:file "dispatch-tests-state-option-navigation") ; state toggles, show-options, last-window/last-pane, respawn/pipe-pane
          (:file "dispatch-tests-list-format-helpers") ; window/session list formatting, copy-mode-call, kill-result helper
          (:file "dispatch-tests-popup-menu-runtime") ; popup overlay, display-popup, display-menu runtime
          (:file "dispatch-tests-session-presence") ; has-session prompt and argument validation
          (:file "dispatch-tests-pane-zoom-resize") ; pane zoom, navigation, and resize dispatch cases
          (:file "dispatch-tests-copy-mode-mouse-x") ; copy-mode mouse entry, indicator, and -X prompt cases
          (:file "dispatch-tests-display-menu-list-keys") ; display-menu selection and list-keys notes
          (:file "dispatch-tests-format-vars") ; session/pane/window/client format variables
          (:file "dispatch-tests-capture-terminal-rendering") ; capture-pane and terminal line-size rendering
          (:file "dispatch-tests-layout-winlink-order") ; select-layout undo and per-session winlink ordering
          (:file "dispatch-tests-commands-e") ; switch-client, last-session, new-session, kill-session, mark-pane, next-layout, bind/unbind-key, list/choose-buffer, wait-for - part V
          (:file "dispatch-tests-commands-c") ; helper tests, on-submit paths, cyclic nav, break/join/run/if - part III
          (:file "dispatch-tests-commands-d") ; has-session, find-window/select-window-prompt, move/swap-window, bind/unbind-key, kill-pane, split, new-window - part IV
          (:file "dispatch-tests-session") ; copy-mode paging, with-active-pane, format-menu, named-layout, kill-result, command-dispatch-outcome - part I
          (:file "dispatch-tests-session-listing") ; named-command table, select-layout, list clients/sessions/panes/windows, list-commands
          (:file "dispatch-tests-session-window-lifecycle") ; split-window, new-window, server commands, prefix, detached variants
          (:file "dispatch-tests-session-environment-hooks") ; hidden environment variables, client size, scoped hooks
          (:file "dispatch-tests-client-session-control") ; attach, detach, refresh, lock, and move-pane dispatch
          (:file "dispatch-tests-window-resize-lifecycle") ; resize-window, respawn-window, and select-layout dispatch
          (:file "dispatch-tests-environment-set") ; set-environment dispatch
          (:file "dispatch-tests-environment-show-overlays") ; show-environment and overlay dispatch
          (:file "dispatch-tests-session-flag-targets") ; flag parsers, target resolvers, new-window/split-window/new-session command cases
          (:file "dispatch-tests-session-b-tail") ; layout names, target resolvers, display-message, new-session size parsing - part IIb
          (:file "dispatch-tests-session-c") ; options, move-window, new-session -s/-A/-t, control-mode REPL - part III
          (:file "dispatch-tests-session-f") ; new-session duplicate, grouped sessions, control-mode notifications, server-lifecycle, %output relay - part VI
          (:file "dispatch-tests-session-d") ; display-popup, send-keys -N/-H, capture-pane - part IV
          (:file "dispatch-tests-session-d-tail") ; named paste-buffer and join-pane marked-pane - part IVb
          (:file "dispatch-tests-wait-for"))) ; wait-for command channel state and argument validation
        (:module "presentation/events"
         :serial t
         :components
         ((:file "events-tests") ; keystroke pipeline - part I (suite, escape, process-byte, prompt, copy-mode vi-nav)
          (:file "events-copy-mode-repeat-tests") ; copy-mode numeric-prefix repeat counts
          (:file "events-runtime-tests") ; prompt UTF-8, event-loop cycle, automatic rename
          (:file "events-mouse-tests") ; mouse dispatch, X10, middle-click paste, defaults
          (:file "events-tests-f") ; keystroke pipeline - part VI (PageUp/Down, prefix-arrow, send-prefix, modifier+arrow, meta/alt)
          (:file "events-switch-client-tests") ; custom key tables, switch-client, default meta/layout bindings
          (:file "events-tests-b") ; locked-session, drag/modifier, copy-mode cursor, vi nav - part II
          (:file "events-tests-h") ; byte-constants, make-input-state, forward-octets, maybe-rename-window - part VIII
          (:file "events-tests-c") ; app-cursor-keys, prompt-key, copy-mode nav, SGR, border-check - part III
          (:file "events-tests-e") ; status-col, SGR-nil, copy-nav, flush-esc, reset-repeat, mouse/key-table - part IV
          (:file "events-csi-u-tests") ; CSI-u extended key parsing and dispatch
          (:file "events-tests-d") ; app-cursor-keys (ss3), new bindings, :mark-pane, root table, fn-keys - part V
          (:file "events-tests-g") ; select-layout-spread, new key bindings, choose-window, mouse-reporting, tmux defaults - part VII
          (:file "events-tests-i") ; copy-mode v-select, middle-cursor-jump, mouse X10, CSI-tilde outside mode, CSI-3byte - part IX
          (:file "events-tests-j") ; vi-normal-key dispatch, %dispatch-menu-key, %rename-from-osc-title - part X
          (:file "mouse-tests")))
        (:module "application/commands"
         :serial t
         :components
         ((:file "commands-tests") ; resize-pane, scroll, kill-pane, select/rename, begin-sel/yank/other-end - part I
          (:file "commands-tests-e") ; copy-mode-clear-sel, WORD-motion, select-word, move-cursor - part II
          (:file "commands-tests-f") ; rename-window, kill-window, run/if-shell, selection-text, swap-pane - part III
          (:file "commands-tests-m") ; swap-pane (cont), capture-pane, shift-line-wrapped, copy-mode scroll, resize-pane, split-window - part XIII
          (:file "commands-tests-n") ; copy-mode-begin-selection multi-row, yank, other-end - part XIV
          (:file "commands-tests-b") ; copy-mode line-start/end, high/middle/low, scroll noop guards, word motions, top/bottom - part IV
          (:file "commands-tests-k") ; begin-line-selection, copy-end-of-line (D), copy-line (Y), search-forward/backward, wrap-search - part XI
          (:file "commands-tests-g") ; send-keys, key-name, tokenize, kill-window-mru, join-pane - part V
          (:file "commands-tests-h") ; copy-mode-exit, break-pane, clear-history, rotate - part VI
          (:file "commands-window-navigation-tests") ; find-window and next/previous/last-window command behavior
          (:file "commands-tests-c") ; pipe-pane, virtual-row, timeout, scroll helpers, word/paragraph nav - part VII
          (:file "commands-tests-o") ; selection-bounds scrollback, word/paragraph nav, scroll-middle - part XV
          (:file "commands-tests-j") ; join-pane helpers, resize-pane up, noop guards, search, scroll, extract-chars, row-string - part X
          (:file "commands-tests-d") ; rename/select hooks, server-access, customize-mode, begin-line-selection multi-row - part VIII
          (:file "commands-tests-l") ; copy-mode copy-line/copy-end-of-line, with-shell-timeout, kill hooks, toggle-rect, append-sel, copy-pipe, renumber-windows - part XII
          (:file "commands-tests-i") ; rectangle-sel, run-copy-cmd, set-cursor, send-keys-l - part IX
          (:file "commands-copy-navigation-tests") ; jump-to-char, goto-line, search-incr, bracket navigation
          (:file "commands-tests-p"))) ; copy-selection-no-cancel/no-clear, pipe family, copy-pipe-no-clear/line/-and-cancel, rectangle-on/off, cursor-down-and-cancel, scroll-to-mouse, copy-*-and-cancel, last-jump - part XVII
        (:module "presentation/prompt"
         :serial t
         :components
         ((:file "overlay-tests")
          (:file "overlay-popup-menu-tests")
          (:file "overlay-transient-tests")
          (:file "prompt-tests")
          (:file "prompt-editing-tests")
          (:file "prompt-tests-wiring")))
        (:module "application/config-2"
         :pathname "application/config"
         :serial t
         :components
         ((:file "config-tests-defaults")))
        (:module "infrastructure/net"
         :serial t
         :components
         ((:file "protocol-tests") ; octets/frame-header/round-trips/msg-command - part I
          (:file "protocol-tests-b") ; read-u32, split-on-nul, encode/decode-command-payload, target-field-p - part II
          (:file "transport-tests") ; round-trips, with-incoming-frame, %read-exact - part I
          (:file "transport-tests-b"))) ; validation, security boundaries, CPS-phase direct coverage - part II
        (:module "bootstrap"
         :serial t
         :components
         ((:file "server-tests")
          (:file "server-tests-b") ; list-sessions, rename-session, switch-client, session groups
          (:file "server-socket-cps-tests"))) ; socket paths, client key CPS, runtime registry
        (:module "infrastructure/pty"
         :serial t
         :components
         ((:file "pty-ffi-tests")
          (:file "pty-rawmode-tests")
          (:file "pty-tests"))) ; PTY argument-assembly unit tests (spawn helpers)
        (:module "infrastructure/input"
         :serial t
         :components
         ((:file "input-tests")))
        (:module "bootstrap-2"
         :pathname "bootstrap"
         :serial t
         :components
         ((:file "runtime-tests") ; globals, pane-reader-loop, monitor-activity/silence, alert-action - part I
          (:file "runtime-prompt-history-io-tests")
          (:file "runtime-message-log-core-tests")
          (:file "runtime-tests-c") ; stop-reader-threads, add-message-log, add-prompt-history, wait-for-channel - part III
          (:file "runtime-tests-b") ; add-message-log table-driven, add-prompt-history, wait-for-channel - part II
          (:file "main-tests")
          (:file "main-entry-tests")
          (:file "main-environment-tests")
          (:file "main-command-argument-tests")))
        (:module "feature"
         :serial t
         :components
         ((:file "advanced-tests"))))) ; cross-layer acceptance suite (break/pipe/sync, layout, lock, session-groups)
      (:module "integration"
       :serial t
       :components
       ((:file "net-tests")
        (:file "server-multi-tests")
        (:file "server-multi-command-client-tests")
        (:file "pty-tests")
        (:file "client-tests")
        (:file "client-receive-tests")))
      (:file "suite")))))
