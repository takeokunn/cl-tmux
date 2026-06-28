# cl-tmux Implementation Roadmap: 100% tmux Feature Coverage

## 1. Executive Summary

**Current implementation coverage:** the old 12–15% estimate is no longer used. Several items that were previously tracked as blockers are already implemented in the current tree, so this roadmap focuses on confirmed gaps, regression coverage, and parity checks.

Implemented and working:
- Single session with multiple windows and panes (binary split tree)
- Horizontal/vertical pane splitting (C-b % and C-b ")
- Basic key bindings via `*key-bindings*` hash table
- Terminal emulator core: SGR 0–7/22–27, CSI cursor movement, alternate screen
- Config file loading with `set-shell` and `set-status-height` directives
- Unix socket client/server with message framing
- Copy mode (scroll, selection, and paste buffer)
- Status bar (format-driven)
- PTY integration via cffi
- Session targeting / target resolution
- Multi-session server and session registry
- Format-string expansion for status bars and message rendering
- General option infrastructure (`set-option`, `set-window-option`, `show-options`)

**Remaining work:**

1. **Parity checks only** — the previously tracked blockers are implemented or intentionally unsupported; keep recording any newly discovered tmux behavior differences here.
2. **Behavioral parity audits** — keep verifying edge cases where the code already has an implementation path but tmux semantics may still differ.

---

## 2. Sprint 0 — P0 Blockers (fix before anything else)

These are correctness defects that break existing functionality or make the multiplexer unusable for real work.

### S0-1: SGR italic fix [implemented]

**What:** SGR 3/23 already map italic on/off in `src/domain/terminal/sgr.lisp`. Conceal, strikethrough, and overline are also already implemented in the current tree.

**Files:** `src/domain/terminal/sgr.lisp`, `src/presentation/renderer/renderer-format.lisp`, `tests/unit/terminal/sgr-tests.lisp`, `tests/unit/renderer-tests-d.lisp`

**Status:** implemented in the current codebase; keep this section as a parity reference only.

---

### S0-2: 256-color and true-color SGR [implemented]

**Status:** implemented.

**Implemented in:** `src/domain/terminal/sgr.lisp`, `src/presentation/renderer/renderer-format.lisp`, `src/domain/terminal/cell.lisp`, `src/domain/terminal/screen.lisp`

**Tests:** `tests/unit/terminal/sgr-tests.lisp`, `tests/unit/terminal/sgr-tests-b.lisp`, `tests/unit/renderer-format-tests.lisp`, `tests/unit/renderer-format-tests-b.lisp`, `tests/unit/commands-tests-m.lisp`, `tests/unit/terminal/parser-tests.lisp`

**Notes:** `38;5;n` and `38;2;r;g;b` are already parsed and rendered. Keep this section only as a reminder to re-check tmux parity for any edge-case color sequences we have not yet mirrored.

---

### S0-3: Cursor visibility (DECTCEM) [implemented]

**What:** DECTCEM cursor visibility (`\e[?25l` / `\e[?25h`) is already tracked in terminal state and honored by the renderer.

**Files:** `src/domain/terminal/csi.lisp`, `src/presentation/renderer/renderer-format.lisp`, `tests/unit/renderer-tests-d.lisp`

**Status:** implemented in the current codebase; keep this section as a parity reference only.

---

## 3. Sprint 1 — P1 High-Value Features

These are the features needed to make cl-tmux useful as a daily driver. Implement in the order listed, as later items depend on earlier ones.

### S1-1: Session targeting (-t flag resolution) [implemented]

**Priority:** already implemented in the current tree; keep this section as the canonical reference for the target-resolution path and regression coverage.

**Files:** `src/domain/model/target.lisp`, `src/application/dispatch/dispatch-core.lisp`, `src/application/dispatch/dispatch-commands*.lisp`, `src/bootstrap/package.lisp`, `tests/unit/target-tests*.lisp`

**Current implementation notes:**
- `resolve-target` and the helper lookups live in `src/domain/model/target.lisp`.
- Dispatch handlers route through the target-resolution helpers before mutating sessions, windows, or panes.
- The target tests already exercise name/id/index matching and the runtime paths that consume the resolution result.

**Tests:** `tests/unit/target-tests.lisp` and `tests/unit/target-tests-b.lisp` already cover the target lookup and command-routing surfaces.

---

### S1-2: Multi-session server [implemented]

**Priority:** already implemented in the current tree; keep this section as the canonical reference for session registry behavior and regression coverage.

**Files:** `src/bootstrap/session-registry.lisp`, `src/application/dispatch/dispatch-commands-pane-session.lisp`, `src/bootstrap/runtime.lisp`, `src/bootstrap/main.lisp`, `src/bootstrap/package.lisp`, `tests/unit/server-tests*.lisp`

**Current implementation notes:**
- The runtime already maintains a multi-session registry and session lifecycle helpers.
- `run-server` and the dispatch layer already consult the current session / registry state rather than assuming a single hard-coded session.
- `new-session`, `kill-session`, `list-sessions`, and `switch-client` are already wired through the dispatch command tables.

**Tests:** `tests/unit/server-tests.lisp` and related dispatch tests already cover session registry and selection behavior.

---

### S1-3: new-session and kill-session commands [implemented]

**Status:** implemented.

**Implemented in:** `src/bootstrap/main-startup.lisp`, `src/application/dispatch/dispatch-command-specs-core-session.lisp`, `src/application/dispatch/dispatch-handlers.lisp`, `src/application/dispatch/dispatch-commands-pane.lisp`, `src/bootstrap/main.lisp`, `tests/unit/main-tests.lisp`, `tests/unit/server-tests.lisp`, `tests/unit/dispatch-tests-session-f.lisp`, `tests/unit/dispatch-tests-commands-e.lisp`, `tests/integration/server-multi-tests.lisp`

**Notes:** `run-new-session` already handles startup forwarding and `%ensure-server-running`; `:new-session` / `:kill-session` are wired in dispatch and covered by tests.

---

### S1-4: Directional pane navigation (select-pane -L/-R/-U/-D) [implemented]

**Status:** implemented.

**Implemented in:** `src/application/dispatch/dispatch-commands-option.lisp`, `src/application/dispatch/dispatch-handlers.lisp`, `src/application/dispatch/dispatch-core.lisp`, `src/domain/model/window-neighbor.lisp`, `src/application/config/config.lisp`

**Tests:** `tests/unit/dispatch-tests-c.lisp`, `tests/unit/dispatch-tests-commands-d.lisp`, `tests/unit/events-tests-b.lisp`, `tests/unit/config-tests.lisp`

**Notes:** `select-pane -L/-R/-U/-D` already routes through directional neighbor lookup and the prefix arrow-key bindings are already wired. Keep this section only for parity audits against tmux edge cases.

---

### S1-5: Pane zoom toggle (resize-pane -Z, C-b z) [implemented]

**Status:** implemented.

**Implemented in:** `src/presentation/events/events-loop.lisp`, `src/application/config/config.lisp`, `src/domain/model/window.lisp`, `src/application/dispatch/dispatch-commands.lisp`, `src/application/dispatch/dispatch-handlers.lisp`, `tests/unit/events-tests-g.lisp`, `tests/unit/dispatch-tests-c.lisp`, `tests/unit/window-tests.lisp`, `tests/unit/config-tests.lisp`

**Notes:** `C-b z` / `#\z` and the zoom-toggle command path already route through `window-zoom-toggle`.

---

### S1-6: General set-option infrastructure [implemented]

**Files:** `src/domain/options/options-api.lisp`, `src/application/config/config-directives-set.lisp`, `src/application/dispatch/dispatch-commands-option.lisp`, `src/domain/model/session.lisp`, `src/domain/model/window.lisp`, `tests/unit/config-tests.lisp`

**Current implementation notes:**
- `set-option`, `set-option-for-window`, and `set-option-for-pane` already exist in the options API.
- The config directive layer already routes `set`, `set-window-option`, and related aliases to the option machinery.
- `show-options` and `set-option` are already exposed through the dispatch command registry and exercised by tests.

**Tests:** `tests/unit/config-tests.lisp` already covers option lookup and directive behavior.

---

### S1-7: Format-string expansion engine [implemented]

**Priority:** already implemented in the current tree; keep this section as the canonical reference for status-bar formatting and message expansion behavior.

**Files:** `src/domain/format/format-engine.lisp`, `src/domain/format/format-context.lisp`, `src/presentation/renderer/renderer-statusbar.lisp`, `src/application/dispatch/dispatch-commands-buffer.lisp`, `src/application/dispatch/dispatch-commands-server.lisp`, `tests/unit/format-tests*.lisp`

**Current implementation notes:**
- `expand-format` and `format-context-from-session` already exist and are used by the renderer and message paths.
- Status bar rendering already builds its text from the session/window/pane context rather than hard-coded literals.
- The format tests already exercise `#S`, `#{session_name}`, and conditional expansion paths.

**Tests:** `tests/unit/format-tests.lisp` and `tests/unit/format-tests-f.lisp` already cover the format engine and context assembly.

---

### S1-8: Copy-mode text selection and paste buffer [implemented]

**Status:** implemented.

**Implemented in:** `src/application/commands/commands-copy-mode.lisp`, `src/application/commands/commands-copy-mode-clip.lisp`, `src/domain/buffer/buffer.lisp`, `src/presentation/events/events-mouse.lisp`, `src/application/dispatch/dispatch-commands-pane-x.lisp`, `tests/unit/commands-tests-e.lisp`, `tests/unit/commands-tests-n.lisp`, `tests/unit/commands-tests-l.lisp`, `tests/unit/dispatch-tests-commands-c.lisp`

**Notes:** `copy-mode-select-word`, `copy-mode-yank`, and the paste-buffer plumbing already exist; this section is now a parity audit reference only.

---

### S1-9: Server auto-start on client connect [implemented]

**Status:** implemented.

**Implemented in:** `src/bootstrap/main-startup.lisp`, `src/bootstrap/client.lisp`, `src/bootstrap/main.lisp`, `tests/unit/main-tests.lisp`, `tests/integration/client-tests.lisp`

**Notes:** startup auto-forwarding / `ensure-server-running` behavior is already wired; keep this section only for parity audits.

---

## 4. Sprint 2 — P2 Medium-Value Features

### Session management
- **Implemented**: `rename-session`, `list-sessions`, `switch-client`, `has-session`, `last-session`, `source-file`, and `display-message` already exist in the dispatch/runtime path. The current codebase also handles `attach-session` targets and the `-d` / `-r` attach flags.
- **Relevant code**: `src/bootstrap/main-startup.lisp`, `src/presentation/events/events-loop.lisp`, `src/bootstrap/session-registry.lisp`, `src/domain/model/session.lisp`, `src/application/dispatch/dispatch-commands-lifecycle.lisp`, `src/application/dispatch/dispatch-command-specs-core-session.lisp`, `src/application/dispatch/dispatch-handlers.lisp`, `src/bootstrap/main.lisp`, `src/bootstrap/client.lisp`.
- **Tests**: session-management behavior is covered in `tests/unit/dispatch-tests-session-e.lisp`, `tests/unit/dispatch-tests-commands-c.lisp`, `tests/unit/main-tests.lisp`, and `tests/integration/server-multi-tests.lisp`.

### Window management
- **Implemented**: `last-window`, `move-window`, `swap-window`, `list-windows` / `choose-window`, `find-window`, `rotate-window`, and automatic window renaming are already present.
- **Relevant code**: `src/presentation/events/events-loop.lisp`, `src/application/dispatch/dispatch-command-specs-core-window.lisp`, `src/application/dispatch/dispatch-commands-window.lisp`, `src/domain/model/window.lisp`, `src/domain/terminal/parser.lisp`, `src/domain/options/options.lisp`, `src/presentation/renderer/renderer.lisp`.
- **Tests**: window management is covered by `tests/unit/window-tests*.lisp` and dispatch tests.

### Pane management
- **Implemented**: `split-window -d`, `split-window -p/-l size`, `swap-pane`, `display-panes`, `capture-pane`, `last-pane`, `pane-border-style`, and `respawn-pane` are already wired through the dispatch/runtime path.
- **Relevant code**: `src/application/dispatch/dispatch-core.lisp`, `src/application/dispatch/dispatch-commands-pane.lisp`, `src/domain/model/window.lisp`, `src/presentation/renderer/renderer.lisp`, `src/presentation/renderer/renderer-borders.lisp`, `src/domain/options/options.lisp`, `src/domain/terminal/screen.lisp`, `src/application/dispatch/dispatch-commands-buffer.lisp`.
- **Tests**: pane management is covered by `tests/unit/window-tests*.lisp`, `tests/unit/dispatch-tests-commands-*.lisp`, and integration coverage for PTY-oriented behavior.

### Terminal emulation
- **Implemented**: cursor shape (`DECSCUSR`), bracketed paste, application cursor keys, auto-wrap, CBT/CHT, OSC title/clipboard handling, and combining characters are already present.
- **Relevant code**: `src/domain/terminal/parser.lisp`, `src/domain/terminal/modes.lisp`, `src/domain/terminal/cursor.lisp`, `src/domain/terminal/csi.lisp`, `src/domain/terminal/parser-osc.lisp`, `src/domain/buffer/buffer.lisp`.
- **Tests**: terminal emulation coverage lives under `tests/unit/terminal/*.lisp`.

### Config / options
- **Implemented**: `bind-key -n`, `bind-key -r`, `bind-key -T`, `show-options`, and `server-options` are already implemented.
- **Remaining follow-up**: the config file tokenizer deserves a separate pass if quote/escape fidelity needs to be improved further.
- **Relevant code**: `src/application/config/config.lisp`, `src/application/config/config-directives.lisp`, `src/domain/options/options.lisp`.

### Status bar
- **Implemented**: status on/off, status position, status style, status interval, and status justification are already present in the renderer/options path.
- **Relevant code**: `src/domain/options/options.lisp`, `src/presentation/renderer/renderer-statusbar.lisp`, `src/presentation/renderer/renderer-style.lisp`, `src/bootstrap/runtime.lisp`.
- **Tests**: status bar behavior is covered by renderer and integration tests.

---

## 5. Sprint 3 — P3 Completeness Features

### Session groups and advanced session management
- **Session groups** (`new-session -t`): implemented via session `group` slots and registry-managed group ids; sessions in the same group share windows.
- **lock-session / lock-client**: implemented in `src/application/dispatch/dispatch-command-specs-core-session.lisp`, `src/application/dispatch/dispatch-handlers-b.lisp`, and `src/presentation/renderer/renderer-lock.lisp`; the renderer overlays a lock screen and accepts a passphrase via command prompt.
- **update-environment**: implemented via `src/application/config/config-directives-set.lisp` and `src/domain/model/session.lisp`; session creation copies the configured client environment variables into server-spawned PTY environments.

### Advanced window/pane operations
- **link-window / unlink-window**: implemented in `src/application/dispatch/dispatch-commands-lifecycle.lisp`, `src/application/dispatch/dispatch-command-specs-core-window.lisp`, and `src/application/dispatch/dispatch-handlers-b.lisp`; windows can be shared across sessions at the window level.
- **break-pane (C-b !)**: implemented in `src/application/commands/commands.lisp`; detaches the active pane into a new window while preserving layout-tree consistency.
- **join-pane / move-pane**: implemented in `src/application/commands/commands.lisp` and dispatch handlers; moves panes across windows and reinserts them into the current layout tree.
- **pipe-pane**: implemented in `src/application/commands/commands.lisp`, `src/application/dispatch/dispatch-handlers-b.lisp`, and `src/application/dispatch/dispatch-command-specs-core-window.lisp`; subprocess teeing and pane pipe lifecycle are already wired.
- **synchronize-panes** window option: implemented in `src/presentation/events/events-loop.lisp` and `src/presentation/events/events-keystroke.lisp`; when enabled, active-pane input is also written to the other panes in the window.

### Built-in named layouts
- **even-horizontal, even-vertical, main-horizontal, main-vertical, tiled**: implemented in `src/domain/model/window-layout.lisp`; named layouts rebuild the layout tree with the expected split rules.
- **select-layout and C-b Space / C-b M-1–M-5**: implemented via `src/application/dispatch/dispatch-command-specs-core-window.lisp`, `src/application/dispatch/dispatch-handlers-b.lisp`, and the key-binding tables; `cmd-select-layout` cycles through or jumps to a named layout.
- **layout-persistence (layout string)**: implemented in `src/domain/model/layout-persistence.lisp`; `layout->string` and `string->layout` encode and decode persisted layouts.

### Mouse support
- **Mouse reporting modes (?1000h/1002h/1003h/1006h)**: implemented in `src/domain/terminal/modes.lisp`, `src/presentation/events/events-mouse.lisp`, `src/presentation/renderer/renderer-compose-protocols.lisp`, and `src/presentation/renderer/renderer-compose-overlay.lisp`; these rows are runtime-covered and gated by the session `mouse` option.
- **Mouse pane selection**: implemented in `src/presentation/events/events-mouse.lisp`; `MouseDown1Pane` uses `pane-at-position` to find the clicked pane and activate it.
- **Mouse pane border resize**: implemented in `src/presentation/events/events-mouse.lisp`; border clicks enter drag mode and update the split ratio in the layout tree.
- **Mouse wheel scrollback**: implemented in `src/presentation/events/events-mouse.lisp`; wheel events translate to copy-mode scroll up/down.
- **Mouse text selection**: implemented in `src/presentation/events/events-mouse.lisp`; drag enters copy mode, updates the copy cursor, and yanks on button release.
- **Mouse status bar click**: implemented in `src/presentation/events/events-mouse.lisp`; status-bar clicks map the column to a window and call `:select-window`.
- All mouse features above are gated behind `(get-option session "mouse")` and exercised by `tests/unit/mouse-tests.lisp`.

### Scripting and hooks
- **hooks system**: implemented in `src/domain/hooks/hooks.lisp`, `src/application/dispatch/dispatch-core.lisp`, and the dispatch handler files; hook names are stored in the runtime and executed as command strings.
- **run-shell**: implemented in `src/application/commands/commands-keys.lisp` and `src/application/config/config-directives-runtime.lisp`; it runs a shell command and captures the result for command execution.
- **if-shell**: implemented in `src/application/commands/commands-keys.lisp` and `src/application/config/config-directives-runtime.lisp`; it runs a shell command and branches on the exit code.
- **confirm-before**: implemented in `src/application/dispatch/dispatch-handlers.lisp`, `src/application/dispatch/dispatch-command-specs-core-window.lisp`, and `src/application/dispatch/dispatch-commands-buffer.lisp`; tests already cover the confirmation prompt and wrapped dispatch behavior.
- **wait-for**: implemented in `src/application/dispatch/dispatch-commands-list.lisp`; `wait-for foo` blocks until the matching signal arrives.
- **display-popup**: implemented in `src/application/dispatch/dispatch-commands-buffer.lisp`, `src/application/dispatch/dispatch-command-specs-core-window.lisp`, `src/presentation/prompt/overlay.lisp`, and `src/presentation/renderer/renderer-overlay.lisp`; popups render as floating overlays instead of separate PTY-backed screens.
- **display-menu**: implemented in `src/application/dispatch/dispatch-handlers-b-menu.lisp`, `src/application/dispatch/dispatch-command-specs-core-misc.lisp`, `src/presentation/prompt/overlay.lisp`, and `src/presentation/renderer/renderer-overlay.lisp`; the unit tests already exercise menu placement and selection.
- **key-tables**: implemented in `src/application/config/config.lisp`, `src/application/config/config-directives.lisp`, and the keystroke dispatch files; `copy-mode` / `prefix` / `root` tables and repeatable bindings are already present.

### Control mode and advanced client/server
- **control mode (tmux -C)**: implemented in `src/infrastructure/control-mode/control-mode.lisp`, `src/bootstrap/main.lisp`, and `src/application/dispatch/dispatch-control.lisp`; notifications are emitted as `%begin`/`%end`-delimited blocks and covered by the control-mode unit tests.
- **concurrent multi-client**: implemented in `src/bootstrap/server-multi.lisp`; the server event loop already broadcasts frame diffs to connected clients, with integration coverage in `tests/integration/server-multi-tests.lisp`.
- **command protocol over socket**: implemented in `src/bootstrap/client.lisp` and `src/bootstrap/server-multi.lisp`; `run-command-client` is covered by `tests/integration/client-tests.lisp` and `tests/integration/server-multi-tests.lisp`.
- **read-only client**: implemented via `*client-read-only*` in `src/bootstrap/runtime.lisp`, `src/presentation/events/events-loop.lisp`, `src/presentation/events/events-mouse.lisp`, `src/application/dispatch/dispatch-commands-auto.lisp`, and `src/application/dispatch/dispatch-handlers.lisp`; input forwarding is skipped for read-only clients.

### Additional terminal emulation
- **Line drawing / ACS**: ESC `(0` / ESC `(B` character set switching and ACS remapping are already implemented in `src/domain/terminal/parser.lisp`, `src/domain/terminal/modes.lisp`, and `src/domain/terminal/cursor.lisp`; keep the remaining coverage/docs aligned with that implementation.
- **DCS sequences**: implemented in `src/domain/terminal/parser.lisp` as DCS passthrough / XTGETTCAP / DECRQSS handling, with parser coverage in `tests/unit/terminal/parser-tests-b.lisp`.
- **Device Attributes (DA1/DA2)**: implemented in `src/domain/terminal/csi.lisp`; respond with `\e[?1;2c` (VT100 with AVO) and `\e[>1;10;0c` for DA2.
- **SGR double-underline (21), rapid-blink (26), underline-color (58/59)**: implemented in `src/domain/terminal/sgr.lisp` with renderer support in `src/presentation/renderer/renderer-format.lisp` and unit coverage in `tests/unit/terminal/sgr-tests-b.lisp`.

---

## 6. Architecture Notes — Cross-Cutting Concerns

### Mouse support (Sprint 3) spans terminal, renderer, and event dispatch
Mouse handling is implemented across:
- `src/domain/terminal/modes.lisp` — DEC modes 1000/1002/1003/1006
- `src/domain/terminal/screen.lisp` — `mouse-mode` / `mouse-sgr-mode` state
- `src/presentation/events/events-mouse.lisp` — parse X10/SGR mouse sequences, handle passthrough, click counts, drag-resize, scrollback, and `%dispatch-mouse-event`
- `src/presentation/renderer/renderer-compose-overlay.lisp` — emit outer-terminal mouse-tracking sequences from session and pane state
- `src/domain/terminal/csi.lisp` — DECRQM reporting for the mouse modes
- `src/application/config/config-directives.lisp` / `src/domain/options/options.lisp` — `mouse` boolean option and the option-change hook

### Format strings (Sprint 1-7) touches renderer, config, and all command output
The format pipeline is split across `src/domain/format/format-helpers.lisp`, `src/domain/format/format-strftime.lisp`, `src/domain/format/format.lisp`, `src/domain/format/format-engine.lisp`, and `src/domain/format/format-context.lisp`. Keep those files ahead of renderer/dispatch layers in `cl-tmux.asd`, and treat `format-context-from-session` as the builder that turns session/window/pane state into the plist consumed by `expand-format`.

### Multi-session server and protocol framing are already target-aware
`src/infrastructure/net/protocol.lisp` already encodes and decodes target-bearing commands, and `src/bootstrap/server-multi.lisp` reconstructs the command line with `-t <target>` before dispatch. Keep new wire messages target-aware so they remain compatible with multi-session routing.

### Key-table system (Sprint 2 config, Sprint 3 scripting)
The key-table system is already implemented. `src/application/config/config.lisp` defines named tables and repeatable bindings, `src/application/config/config-directives.lisp` parses `bind -n`, `bind -r`, `bind -T`, and the keystroke dispatch path resolves the active table at runtime. Keep this section as a reminder that future bindings should reuse the existing table model instead of flattening it again.

### Colors require asd component ordering
The color type change in `src/domain/terminal/screen.lisp` (Sprint 0-2) will break `src/domain/terminal/sgr.lisp`, `src/presentation/renderer/renderer.lisp`, and `src/presentation/renderer/renderer-pane.lisp` if they are compiled before the new color struct is defined. `screen.lisp` is already first in the terminal subsystem ordering — verify the `.asd` `:depends-on` or `:serial t` ordering is `screen → cursor → modes → sgr → csi → parser`.

### Binary split tree must support zoom invariant
The `layout.lisp` zoom-in operation (Sprint 1-5) replaces the window's layout tree with a single-leaf tree. All functions in `layout-geometry.lisp` that walk the tree (including the new `pane-neighbor`) must handle single-leaf trees without error. Add a guard: if `(window-zoomed win)` is T, `pane-neighbor` returns NIL (no neighbors when zoomed).

### Renderer purity and incremental redraw
`render-session-to-string` is documented as pure. As features like zoom overlays, copy-mode highlighting, display-panes numbering, and popup windows add stateful overlays, keep all overlay state in the model (window/pane structs) rather than in the renderer. The renderer reads state; it does not mutate it.

---

## 7. Testing Strategy

The test suite is organized by scope:

- `tests/unit/`: fast in-process tests for parsing, dispatch, rendering helpers, options, and in-memory data structures.
- `tests/integration/`: PTY/socket/client-server tests that exercise runtime behavior across process boundaries.
  - Current files: `tests/integration/client-tests.lisp`, `tests/integration/net-tests.lisp`, `tests/integration/pty-tests.lisp`, `tests/integration/server-multi-tests.lisp`
- `tests/e2e/`: end-to-end smoke checks that run against a built binary.
  - Current file: `tests/e2e/e2e-smoke.lisp`

Unit and integration tests are wired through `cl-tmux.asd` and `tests/suite.lisp`. E2E checks are intentionally kept out of the ASDF `cl-tmux/test` system and run separately.

Run the full test suite with:

```sh
nix build .#checks
```
