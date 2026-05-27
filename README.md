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
| Double-width (CJK) cell rendering | 🔜 |
| Scrollback buffer | 🔜 |
| Copy mode | 🔜 |
| Config file | 🔜 |
| Client-server detach/attach | 🔜 |

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
| `d` | Detach (exit) |
| `?` | List keys |

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

## Testing

```bash
# Unit + PTY-integration suite (FiveAM). PTY tests self-skip where
# /dev/ptmx is unavailable, so this also works in sandboxed builds:
nix flake check                     # runs the suite as a Nix check
# or, in the dev shell:
sbcl --eval "(asdf:test-system :cl-tmux)" --quit

# End-to-end smoke test: drives the *real* binary inside a PTY,
# types a command, and verifies cl-tmux renders the output.
nix build .
sbcl --no-sysinit --no-userinit --script test/e2e-smoke.lisp result/bin/cl-tmux
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
│   ├── config.lisp    # Prefix key, default shell, key bindings
│   ├── pty.lisp       # PTY + raw-mode (CFFI/sb-posix, no custom C)
│   ├── terminal.lisp  # VT100/ANSI emulator state machine (+ UTF-8, resize)
│   ├── model.lisp     # Session → Window → Pane model (+ split/relayout)
│   ├── renderer.lisp  # Escape-code renderer (no curses)
│   ├── input.lisp     # Non-blocking stdin reader
│   └── main.lisp      # Entry point + event loop (+ SIGWINCH handling)
└── test/
    ├── terminal-tests.lisp  # VT100/ANSI + UTF-8 unit tests
    ├── layout-tests.lisp    # pane geometry invariants
    ├── pty-tests.lisp       # live PTY/shell integration
    ├── suite.lisp           # aggregate runner
    └── e2e-smoke.lisp       # drives the real binary in a PTY
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
