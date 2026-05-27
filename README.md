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
| Status bar with window list & clock | ✅ |
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

## Project structure

```
cl-tmux/
├── flake.nix          # Nix build (pure Lisp, no C compilation step)
├── cl-tmux.asd        # ASDF system definition
└── src/
    ├── package.lisp   # All defpackage declarations
    ├── config.lisp    # Prefix key, default shell, key bindings
    ├── pty.lisp       # PTY + raw-mode (CFFI/sb-posix, no custom C)
    ├── terminal.lisp  # VT100/ANSI emulator state machine
    ├── model.lisp     # Session → Window → Pane data model
    ├── renderer.lisp  # Escape-code renderer (no curses)
    ├── input.lisp     # Non-blocking stdin reader
    └── main.lisp      # Entry point + event loop
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
buffered write to minimise flicker.
