# cl-tmux

A tmux-compatible terminal multiplexer written entirely in Common Lisp.

Built on top of:
- **SBCL** — the Lisp implementation
- **sb-posix** — POSIX bindings (fork, exec, termios, …) built into SBCL
- **CFFI** — for the handful of libc calls sb-posix doesn't cover (posix_openpt, select, ioctl)
- **bordeaux-threads** — portable threading (one reader thread per PTY pane)
- No custom C source files.

## Features (v0.1)

| Feature | Status |
|---|---|
| Single pane, full-screen shell | ✅ |
| Horizontal pane split (`"`) | ✅ |
| Vertical pane split (`%`) | ✅ |
| Multiple windows (`c` / `n` / `p`) | ✅ |
| Pane focus cycling (`o`) | ✅ |
| Detach (`d`) | ✅ |
| VT100 / ANSI terminal emulation | ✅ |
| UTF-8 decoding (multi-byte, split across reads) | ✅ |
| Pane separators (│ / ─) | ✅ |
| Terminal resize handling (SIGWINCH → relayout) | ✅ |
| Status bar with window list & clock | ✅ |
| Test suite (unit + PTY integration + e2e) | ✅ |
| Scrollback buffer (1000 lines) | ✅ |
| Copy mode (scroll back through history) | ✅ |
| Alternate screen (`?1049h`/`?1049l`) | ✅ |
| Cursor save/restore (`ESC 7`/`ESC 8`, DECSC/DECRC) | ✅ |
| Double-width (CJK) cell rendering | ✅ |
| Pane resize (`H`/`J`/`K`/`L`) | ✅ |
| Config file (XDG: `~/.config/cl-tmux/cl-tmux.conf`) | ✅ |
| Client-server detach/attach (Unix socket) | ✅ |

## Key bindings

All commands require the prefix key **`Ctrl-B`** first.

| Key | Action |
|---|---|
| `c` | New window |
| `n` | Next window |
| `p` | Previous window |
| `"` | Split pane horizontally (top/bottom) |
| `%` | Split pane vertically (left/right) |
| `o` | Focus next pane |
| `,` | Rename current window (opens a status-bar prompt: type, Enter applies, Esc cancels, Backspace edits) |
| `H` / `J` / `K` / `L` | Resize active pane left / down / up / right |
| `[` | Enter copy mode (then arrows / `[` `]` scroll, `q` exits) |
| `x` | Kill active pane |
| `&` | Kill active window |
| `d` | Detach (exit) |
| `?` | List keys |

## Configuration

On startup cl-tmux reads an optional config file (if present) and applies its
directives before the first session is created. The path follows the XDG Base
Directory spec:

1. `$CL_TMUX_CONF` if that environment variable is set, otherwise
2. `$XDG_CONFIG_HOME/cl-tmux/cl-tmux.conf` (with `$XDG_CONFIG_HOME` defaulting to
   `~/.config`), i.e. `~/.config/cl-tmux/cl-tmux.conf`.

A missing file is not an error — cl-tmux starts with its defaults.

One directive per line; blank lines and lines beginning with `#` are ignored.
Tokens are whitespace-separated, with double quotes and backslash escapes
supported inside a token and single quotes treated literally.

| Directive | Arguments | Effect |
|---|---|---|
| `bind` | `<key> <command>` | Bind a prefix key to a command |
| `unbind` | `<key>` | Remove a prefix-key binding |
| `set-shell` | `<path>` | Shell launched for new panes (default `$SHELL`, else `/bin/sh`) |
| `set-status-height` | `<n>` | Rows reserved for the status bar (default `1`) |

`<key>` is a single character (e.g. `c`, `"`) or a multi-character token such as
`M-1`. `<command>` is one of the command names from the Key bindings table
(e.g. `new-window`, `split-vertical`, `resize-left`); an unrecognized command is
ignored.

```conf
# ~/.config/cl-tmux/cl-tmux.conf — rebind splits to be more mnemonic
bind | split-vertical
bind - split-horizontal
unbind "

# use fish for new panes, and a taller status bar
set-shell /run/current-system/sw/bin/fish
set-status-height 2
```

## Building with Nix

```bash
# Run directly
nix run .

# Build and inspect
nix build .
./result/bin/cl-tmux

# Development shell (SBCL + all deps on PATH)
nix develop
sbcl --load cl-tmux.asd \
     --eval "(asdf:load-system :cl-tmux)" \
     --eval "(cl-tmux:main)"
```

## Client-server (detach / attach)

By default `cl-tmux` runs standalone (in-process). It can also run as a headless
server that a thin client attaches to over a Unix socket, so the session
survives detaching:

```bash
cl-tmux server work     # headless server owning session "work"
cl-tmux attach work     # attach a client; C-b d detaches (server keeps running)
cl-tmux attach work     # …re-attach later
```

The socket lives at `$TMPDIR/cl-tmux-<name>.sock`. The client forwards keystrokes
and resizes as length-prefixed protocol frames and paints the frames the server
renders back; all prefix/copy-mode/prompt handling happens server-side through
the same `process-byte` pipeline the standalone loop uses.

## Testing

```bash
# Unit + integration suite (FiveAM). PTY tests self-skip where
# /dev/ptmx is unavailable, so this also works in sandboxed builds:
nix flake check                     # runs the suite as a Nix check
# or, in the dev shell:
sbcl --eval '(require :asdf)' \
     --eval '(push (truename ".") asdf:*central-registry*)' \
     --eval '(asdf:test-system :cl-tmux)' \
     --quit

# End-to-end smoke test: drives the *real* binary inside a PTY,
# types a command, and verifies cl-tmux renders the output.
nix build .
sbcl --no-sysinit --no-userinit --script tests/e2e/e2e-smoke.lisp result/bin/cl-tmux
```

The suite covers three layers: the VT100 emulator (cursor, erase, SGR,
scrolling, UTF-8, resize), pane-layout geometry (no overlap, in-bounds), and
the live PTY pipeline (fork/exec/read/write/select against a real shell).

## Project structure

```
cl-tmux/
├── flake.nix          # Nix build + `checks` (pure Lisp, no C compilation step)
├── cl-tmux.asd        # ASDF system + test system
├── src/
│   ├── package.lisp   # All defpackage declarations
│   ├── config.lisp    # Prefix key, key bindings, config-file loading
│   ├── pty.lisp       # PTY + raw-mode (CFFI/sb-posix, no custom C)
│   ├── protocol.lisp  # Client/server wire protocol (length-prefixed frames)
│   ├── transport.lisp # Frame send/read over a binary stream
│   ├── net.lisp       # Unix-domain socket primitives (sb-bsd-sockets)
│   ├── terminal/      # VT100/ANSI emulator, split into data + logic layers
│   │   ├── types.lisp    # cell/screen structs + accessors (data)
│   │   ├── actions.lisp  # cursor/erase/scroll/edit primitives (logic)
│   │   ├── sgr.lisp      # SGR colour/attribute dispatch (defmacro table)
│   │   ├── csi.lisp      # CSI sequence dispatch (defmacro table)
│   │   ├── parser.lisp   # CPS byte-at-a-time state machine
│   │   └── emulator.lisp # screen-process-bytes entry point
│   ├── model.lisp     # Session → Window → Pane model (+ split/relayout)
│   ├── prompt.lisp    # Single-line input-prompt state (interactive rename, …)
│   ├── commands.lisp  # High-level tmux commands (kill/rename/copy-mode)
│   ├── renderer.lisp  # Escape-code renderer (no curses; pure frame composer + writer)
│   ├── input.lisp     # Non-blocking stdin reader
│   ├── runtime.lisp   # Shared state, SIGWINCH handler, per-pane reader thread
│   ├── events.lisp    # process-byte keystroke pipeline + event loop + dispatch
│   ├── server.lisp    # Detach-attach server (owns session, serves clients)
│   ├── client.lisp    # Detach-attach client (thin terminal over a socket)
│   └── main.lisp      # Binary entry point (standalone / server / attach)
└── tests/
    ├── package.lisp        # test package + imports
    ├── helpers.lisp        # shared unit-test helpers
    ├── helpers-b.lisp      # shared integration-test helpers
    ├── suite.lisp          # aggregate runner
    ├── unit/               # feature-focused unit specs
    ├── integration/        # PTY/socket/runtime integration specs
    └── e2e/                # binary-level smoke tests
```

## Architecture

```
stdin ──► main thread ──► prefix dispatch ──► pty-write(active pane fd)
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

The renderer composites all pane screens to the real terminal in a single
buffered write to minimise flicker.  Terminal resizes arrive via `SIGWINCH`,
which flags a one-shot relayout (geometry is never polled per frame, so a
transient bad `ioctl` read can't trigger a resize storm).
