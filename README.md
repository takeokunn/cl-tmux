# cl-tmux

[![CI](https://github.com/takeokunn/cl-tmux/actions/workflows/ci.yml/badge.svg)](https://github.com/takeokunn/cl-tmux/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

A tmux-compatible terminal multiplexer written entirely in Common Lisp.

cl-tmux reimplements tmux's behavior — commands, options, format strings,
copy mode, hooks, mouse, client/server — on top of SBCL, with no custom C
code. Every verified behavior is pinned by a regression suite of **11,000+
checks** that runs hermetically through Nix.

Built on:

- **SBCL** — the Lisp implementation (PTYs via `sb-ext:run-program`, POSIX via `sb-posix`)
- **CFFI** — for the handful of libc calls sb-posix doesn't cover (`select`, `ioctl`)
- **bordeaux-threads** — one reader thread per PTY pane
- **babel** / **cl-ppcre** — UTF-8 codecs and regexes (format `s///` and `m/r:` matching)

## Feature highlights

- **Commands** — every primary command name in tmux's command table resolves
  (~100 commands: `split-window`, `send-keys`, `capture-pane`, `display-menu`,
  `display-popup`, `command-prompt`, `choose-tree`, `if-shell`, …), with
  flag-level behavior closed against upstream tmux across repeated audits.
- **Terminal emulation** — VT100/ANSI with 16/256/true color, alternate
  screen, scroll regions, origin mode, G0–G3 charsets with line-drawing
  remap, DECDHL/DECDWL double-size lines, bracketed paste, SGR mouse,
  OSC 52 clipboard, OSC 133 prompt marks, and UTF-8 with wide (CJK) cells.
- **Copy mode** — vi-style navigation, selection (including rectangle),
  incremental search, prompt jumping, and 90+ `send-keys -X` commands.
- **Format strings** — the full `#{...}` modifier set
  (`b: d: U: L: n: =N: pN: s/// E: t: m: C: a: q: l:`, comparison/boolean
  operators, `W:`/`S:`/`P:` iteration) over 160+ format variables.
- **Options & hooks** — 120+ options across server/session/window/pane
  scopes, 28 hook events with `set-hook` scoping, key tables, and
  `bind-key -N` notes.
- **Client/server** — detach/attach over per-user Unix sockets
  (`-L`/`-S`, `$TMUX_TMPDIR`), multiple sessions, session groups sharing one
  window set, and control mode (`-C`) for tools like tmuxp/libtmux-style
  automation.
- **Configuration** — real `.tmux.conf` syntax: `%if`/`%elif`/`%else`,
  `%hidden`, variable assignments, `source-file`, brace blocks, and tmux
  quoting rules.

See [docs/COMPATIBILITY.md](docs/COMPATIBILITY.md) for the precise
compatibility statement — what is implemented, what is deliberately
different, and where the remaining risk lives.

## Quick start

With Nix (the only supported build path — it pins SBCL and all Lisp deps):

```bash
nix run github:takeokunn/cl-tmux    # run directly
# or, from a checkout:
nix build .                          # → ./result/bin/cl-tmux
./result/bin/cl-tmux
```

Development shell:

```bash
nix develop
sbcl --eval '(require :asdf)' \
     --eval '(push (truename ".") asdf:*central-registry*)' \
     --eval '(asdf:load-system :cl-tmux)' \
     --eval '(cl-tmux:main)'
```

## Usage

```bash
cl-tmux                          # standalone session (no server)
cl-tmux new-session -s work      # create session "work" on the server
cl-tmux attach -t work           # attach; C-b d detaches, server keeps running
cl-tmux attach -t work -r        # read-only attach
cl-tmux list-sessions            # what's running
cl-tmux kill-server              # stop everything
cl-tmux -C                       # control mode (text protocol on stdin/stdout)
cl-tmux -V                       # print version; --help prints a usage summary
```

Socket selection works like tmux: `-L <name>` picks a named socket in the
per-user directory (created `0700` under `$TMUX_TMPDIR`, falling back to the
system temp dir), and `-S <path>` uses an explicit path.

### Default key bindings

The prefix is **`C-b`**. Common defaults (see `C-b ?` / `list-keys` for the
full table, including `-N` notes):

| Key | Action |
|---|---|
| `c` / `n` / `p` / digits | New / next / previous / select window |
| `"` / `%` | Split pane horizontally / vertically |
| `o`, arrow keys | Move between panes |
| `C-arrows` / `M-arrows` | Resize pane by 1 / 5 (repeatable) |
| `[` / `]` | Enter copy mode / paste buffer |
| `x` / `&` | Kill pane / window (with confirmation) |
| `,` / `$` | Rename window / session |
| `d` | Detach |

All bindings are re-bindable with `bind-key` / `unbind-key`, in the config
file or at the command prompt, including custom key tables.

## Configuration

cl-tmux reads a tmux-style config at startup. Path resolution order:

1. `$CL_TMUX_CONF` if set,
2. `$XDG_CONFIG_HOME/cl-tmux/cl-tmux.conf` (default `~/.config/cl-tmux/cl-tmux.conf`),
3. your existing tmux config as a fallback: `$XDG_CONFIG_HOME/tmux/tmux.conf`,
   `~/.config/tmux/tmux.conf`, or `~/.tmux.conf`.

A missing file is not an error. The syntax is tmux's:

```tmux
# prefix and splits
set -g prefix C-a
bind | split-window -h
bind - split-window -v

# status line with format strings
set -g status-left "#[bold]#{session_name} "
set -g status-right "#{pane_current_command} %H:%M"

# conditionals and variables (tmux 3.2+)
%if "#{==:#{host},worklaptop}"
set -g status-style bg=blue
%endif
MYCOLOR=red
set -g message-style "bg=#{MYCOLOR}"

# hooks and shell integration
set-hook -g after-new-window 'display-message "window created"'
if-shell 'test -f ~/.tmux.local' 'source-file ~/.tmux.local'
```

One deliberate difference from tmux: **only canonical command names are
accepted** — short aliases (`neww`, `splitw`, `killp`, …) are rejected rather
than silently supported, so typos fail loudly. Spell commands out in configs
you share between tmux and cl-tmux.

## Testing

```bash
nix flake check -L    # build + full cl-weave suite (same as CI)
```

The suite (290+ test files, 11,000+ checks) runs on
[`cl-weave`](https://github.com/takeokunn/cl-weave) and covers the VT100 emulator,
layout geometry, command dispatch, format engine, options/hooks, copy mode,
the client/server protocol, and live PTY integration against a real shell.
PTY tests self-skip where `/dev/ptmx` is unavailable, so sandboxed runs stay
meaningful. The runner is deliberately sequential — tests share global
session/socket/PTY state. There is also an end-to-end smoke test that drives
the real binary inside a PTY:

```bash
nix build .
sbcl --no-sysinit --no-userinit --script tests/e2e/e2e-smoke.lisp result/bin/cl-tmux
```

## Project structure

```
cl-tmux/
├── flake.nix               # Nix build + checks (pure Lisp, no C compilation)
├── cl-tmux.asd             # ASDF system + test system
├── src/
│   ├── bootstrap/          # packages, entry point, runtime, server/client loops
│   ├── domain/             # pure model + logic (no I/O)
│   │   ├── terminal/       #   VT100/ANSI emulator (data structs ⁄ logic split)
│   │   ├── model/          #   session → window → pane tree, layouts
│   │   ├── format/         #   #{...} format-string engine
│   │   ├── options/        #   option registry + scopes
│   │   ├── hooks/          #   hook registry + firing
│   │   ├── buffer/         #   paste buffers
│   │   └── ports/          #   port variables (PTY, repository interfaces)
│   ├── application/        # use cases: command dispatch, config loading
│   │   ├── commands/       #   command implementations; tokenizer on cl-parser-kit
│   │   ├── config/         #   tmux.conf directives; shell calls on cl-boundary-kit
│   │   └── dispatch/       #   command table, handlers, control mode
│   ├── infrastructure/     # adapters: PTY (cl-tty-kit spawn/IO; CFFI select+ioctl), sockets, input, control mode
│   ├── presentation/       # renderer (cl-tty-kit colour downsampling), events, prompt
│   ├── reasoning/          # cl-prolog cold-path read-models (keys, commands)
│   └── dataflow/           # cl-dataflow cold-path read-model (copy-mode lifecycle)
└── tests/
    ├── unit/               # 250+ feature-focused spec files
    ├── integration/        # PTY/socket/runtime integration specs
    ├── weave/              # cl-weave suite for the reasoning read-model
    ├── dataflow/           # cl-weave suite for the copy-mode lifecycle read-model
    └── e2e/                # binary-level smoke test
```

### Cold-path reasoning with cl-prolog

`src/reasoning/` is a declarative read-model built on
[`cl-prolog`](https://github.com/takeokunn/cl-prolog), a dependency-free
Common Lisp Prolog engine that is a **core dependency** of cl-tmux (compiled
into the binary). It projects cl-tmux's declarative tables into Prolog
rulebases and answers relational questions the flat tables cannot express
directly. It is used strictly on **cold paths** (introspection, validation,
diagnostics) — never the hot per-keystroke dispatch loop, which stays
imperative for speed.

Two domains ship today — key bindings and the canonical command table:

```lisp
(let ((rb (cl-tmux/reasoning:current-key-rulebase)))
  (cl-tmux/reasoning:key-command rb "prefix" #\c)   ; => :NEW-WINDOW, T
  (cl-tmux/reasoning:keys-running rb :new-window)   ; => (("prefix" . #\c))
  (cl-tmux/reasoning:binding-conflicts rb))         ; keys bound differently across tables

(let ((rb (cl-tmux/reasoning:current-command-rulebase)))
  (cl-tmux/reasoning:command-accepts-flag-p rb "bind-key" "T") ; => T
  (cl-tmux/reasoning:commands-with-flag rb "t")               ; commands taking -t target
  (cl-tmux/reasoning:scriptable-commands rb))                 ; commands taking no arguments
```

Its regression suite (`cl-tmux/weave`) uses
[`cl-weave`](https://github.com/takeokunn/cl-weave) — custom matchers,
`around-each` fixtures, a property test, and `cl-prolog`'s own
`deftest-queries` bridge — and runs as the `weave` flake check
(`nix build .#checks.<system>.weave`).

### Dogfooded sibling libraries

Beyond `cl-prolog` / `cl-weave` above, cl-tmux is a testbed for four more
dependency-light [`nerima-lisp`](https://github.com/nerima-lisp) libraries,
each adopted where it is a genuine fit for something cl-tmux already does by
hand — not bolted on beside it:

- [`cl-cli`](https://github.com/nerima-lisp/cl-cli) parses the top-level
  `cl-tmux [flags] [command [flags]]` global flags (`main-startup-flags.lisp`
  `*cli-app*`), replacing the old ad hoc `-L`/`-S`-only scanner with real
  tmux(1) flag parity — flags may now appear in any order before the command
  word.
- [`cl-boundary-kit`](https://github.com/nerima-lisp/cl-boundary-kit)
  supplies the process boundary (`cl-tmux/config:*process-boundary*`) that
  `run-shell` / `if-shell` and config-time shell directives run through, so
  tests can swap in a fake process without shelling out for real.
- [`cl-dataflow`](https://github.com/nerima-lisp/cl-dataflow) models the
  copy-mode lifecycle as an inspectable state machine (`src/dataflow/`), the
  cl-dataflow counterpart to `src/reasoning/` above — same cold-path-only
  rule, same `nix build .#checks.<system>.dataflow` pattern.
- [`cl-tty-kit`](https://github.com/nerima-lisp/cl-tty-kit) backs the PTY
  layer — pane spawn, byte-transparent master-fd read/write, raw mode, and
  terminal-size queries all delegate to it (`src/infrastructure/pty/`) — and
  contributes `rgb-to-256` for `-2` (force-256-colour) true-colour downsampling
  in `renderer-format.lisp`, cl-tmux's first outer-terminal colour-capability
  negotiation.  cl-tmux keeps its own `select(2)` fd-multiplexing loop, SIGHUP
  `pty-close`, and `set-pty-size` ioctl on top.
- [`cl-parser-kit`](https://github.com/nerima-lisp/cl-parser-kit) is the
  tokenizer framework `commands-tokenizer.lisp`'s shell-style argument
  splitter runs on — one custom rule for the quote/escape-joining scan (no
  generic library has tmux's "quotes extend the current argument" grammar
  built in) plus a whitespace-skip rule, composed through
  `cl-parser-kit:tokenize-string`.

The layering rule: `domain` has no I/O; `application` orchestrates domain
logic through port variables; `infrastructure` provides the real PTY/socket
adapters; `presentation` turns model state into escape codes. Terminal code
further separates data (`types`) from logic (`actions`, `csi`, `sgr`,
CPS parser).

## Architecture

```
stdin ──► main thread ──► key tables / dispatch ──► pty-write(active pane fd)
                  ↑
               select(50ms timeout)
                  │
             render when dirty
                  │
          ┌───────┴────────┐
          │  active window │
          │  ┌───────────┐ │
          │  │   pane 0  │◄──── reader thread 0: blocking read(fd0)
          │  │  screen 0 │      → screen-process-bytes → *dirty* = T
          │  └───────────┘ │
          │  ┌───────────┐ │
          │  │   pane 1  │◄──── reader thread 1: blocking read(fd1)
          │  │  screen 1 │      → screen-process-bytes → *dirty* = T
          │  └───────────┘ │
          └────────────────┘
```

The renderer composites all pane screens into a single buffered write to
minimize flicker. Terminal resizes arrive via `SIGWINCH`, which flags a
one-shot relayout (geometry is never polled per frame, so a transient bad
`ioctl` read can't trigger a resize storm). In client/server mode the same
`process-byte` pipeline runs server-side; clients forward keystrokes and
resizes as length-prefixed frames and paint rendered frames back.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for the build/test workflow and
project-specific rules (the flake only sees git-tracked files; tests must
use the isolation helpers; behavior changes need a tmux reference).
Security reports: see [SECURITY.md](SECURITY.md).

## License

[MIT](LICENSE) © takeokunn
