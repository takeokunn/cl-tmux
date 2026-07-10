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
- **CFFI** — for the handful of libc calls sb-posix doesn't cover (`select`, `ioctl`, termios)
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
nix flake check -L    # build + full FiveAM suite (same as CI)
```

The suite (290+ test files, 11,000+ checks) covers the VT100 emulator,
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
│   │   ├── commands/       #   command implementations (copy mode, panes, …)
│   │   ├── config/         #   tmux.conf tokenizer/preprocessor/directives
│   │   └── dispatch/       #   command table, handlers, control mode
│   ├── infrastructure/     # adapters: PTY (CFFI), sockets, input, control mode
│   └── presentation/       # renderer (escape-code frame composer), events, prompt
└── tests/
    ├── unit/               # 250+ feature-focused spec files
    ├── integration/        # PTY/socket/runtime integration specs
    └── e2e/                # binary-level smoke test
```

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
