# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Initial public development. Highlights of what the tree contains today:

- tmux-compatible terminal multiplexer written in pure Common Lisp (SBCL +
  sb-posix + CFFI; no custom C).
- Full command surface: every primary command name in tmux's command table
  resolves (canonical names only — short aliases are deliberately rejected),
  with flag-level parity validated against upstream behavior.
- VT100/ANSI emulator: SGR (16/256/true color), alternate screen, scroll
  regions, charsets G0–G3, DECDHL/DECDWL double-size lines, bracketed paste,
  mouse reporting, OSC 52 clipboard, OSC 133 prompt marks, UTF-8 with wide
  (CJK) cells.
- Copy mode with vi-style navigation, selection, search, and 90+
  `-X` commands; paste buffers.
- Format-string engine (`#{...}`) with the full modifier set and 160+
  format variables.
- Options across server/session/window/pane scopes, 28 hook events, key
  tables, and `.tmux.conf`-style configuration including `%if`/`%hidden`,
  variable assignments, and `source-file`.
- Client/server over per-user Unix sockets (`-L`/`-S`, `$TMUX_TMPDIR`),
  session groups sharing one window set, control mode (`-C`).
- Test suite (11,000+ checks) run hermetically via `nix flake check`, now on
  the [`cl-weave`](https://github.com/takeokunn/cl-weave) framework.
- Cold-path reasoning read-models built on the dependency-free
  [`cl-prolog`](https://github.com/takeokunn/cl-prolog) engine, now a **core
  dependency** compiled into the binary (`src/reasoning/`). Two declarative
  domains are projected into Prolog rulebases and queried for relations the
  flat tables cannot express directly:
  - **key bindings** — resolution, reverse lookup, cross-table conflicts,
    root-shadowing, and repeatable-command inference;
  - **command metadata** — the canonical command table with derived
    `accepts-flag`, `scriptable`, and flag-reverse-lookup relations.
  Reasoning is strictly cold-path (introspection/validation); the hot
  per-keystroke dispatch loop stays imperative for speed.
- `cl-weave` regression suite (`cl-tmux/weave`) for the reasoning models, using
  custom matchers, `around-each` fixtures, a property test, and `cl-prolog`'s
  own `deftest-queries` bridge; exposed as the `weave` flake check.
- Five more dependency-light sibling libraries adopted as **core
  dependencies**, each replacing or augmenting a hand-rolled piece of the
  same concern it specializes in:
  - [`cl-cli`](https://github.com/nerima-lisp/cl-cli) — the top-level
    `cl-tmux [flags] [command [flags]]` entry point (`main-startup.lisp` /
    `main-startup-flags.lisp` `*cli-app*`) now parses real tmux(1) global
    flags (`-2`, `-C`/`-CC`, `-D`, `-L`, `-N`, `-S`, `-T`, `-V`, `-c`, `-f`,
    `-h`, `-l`, `-u`, `-v`, verified against `man 1 tmux`) in any order, fixing
    a real bug where a flag before `-C`/`-V` (e.g. `cl-tmux -L sock -C`) used
    to hard-fail with a usage error.
  - [`cl-tty-kit`](https://github.com/nerima-lisp/cl-tty-kit) —
    `rgb-to-256` downsamples true-colour SGR output to the 256-colour palette
    when `-2` is given (`renderer-format.lisp` `*color-downsample-fn*`), the
    first cl-tmux terminal-capability negotiation beyond emitting whatever a
    style asks for unconditionally.
  - [`cl-boundary-kit`](https://github.com/nerima-lisp/cl-boundary-kit) — a
    process boundary (`cl-tmux/config:*process-boundary*`) now sits behind
    every `run-shell` / `if-shell` subprocess call (config-time and
    command-time), swappable for `make-test-process-boundary` /
    `make-recording-process-boundary` in tests without touching a real shell.
  - [`cl-dataflow`](https://github.com/nerima-lisp/cl-dataflow) — a new
    cold-path read-model (`src/dataflow/`, mirroring `src/reasoning/`) models
    the copy-mode lifecycle already documented as a Prolog-style rule table
    atop `commands-copy-mode.lisp` as an inspectable `cl-dataflow` state
    machine, with DOT/Mermaid export; regression-tested by the new
    `cl-tmux/dataflow` cl-weave suite (`dataflow` flake check).
  - [`cl-parser-kit`](https://github.com/nerima-lisp/cl-parser-kit) —
    `commands-tokenizer.lisp`'s shell-style argument splitter is now one
    custom `cl-parser-kit` token rule (the quote/escape-joining scan, which
    has no off-the-shelf equivalent) composed with a whitespace-skip rule and
    run through `tokenize-string`, inheriting span tracking and the library's
    tokenizer resource-limit guards for free.

### Changed

- **Test framework migrated from FiveAM to cl-weave.** The `fiveam` dependency
  is gone from the ASDF test system and the Nix checks. A small compatibility
  shim (`tests/fiveam-compat.lisp`) maps the FiveAM authoring surface
  (`def-suite` / `in-suite` / `test` / `is` / `signals` / …) onto cl-weave's
  registration engine, so the ~296 test files run unchanged while cl-weave
  registers, runs (single-worker sequential), and reports every check. The
  runner (`run-tests`) drives `cl-weave:run-all`; per-test thread cleanup runs
  through a root `after-each` hook.

### Fixed

- `main-startup-flags.lisp`: wrap the `%flag-parser-clause` helper in
  `eval-when` so `define-flag-parser` can expand on a cold compile (the helper
  and its macro-time callers now live in one file after the bootstrap split).
- Restored `load-config-from-stream` / `load-config-from-string` in
  `config-preprocessor.lisp`: a file-split refactor dropped these two exported
  loaders while their callers (`load-config-file`) and exports remained,
  leaving config-file loading undefined at runtime.
- Renamed the startup-only `%flag-value` (flat arg-list scanner, 3 callers) to
  `%startup-flag-value` to end a name collision with dispatch's `%flag-value`
  (alist accessor, 95 callers). Because the startup file loads last, its
  definition had been clobbering dispatch's, breaking every flag-taking
  command (e.g. `switch-client -T`, modifier-arrow resize) at runtime.
- Moved the `with-loop-safe-error` macro to `server-multi-dispatch.lisp` (the
  first file that uses it); it had been defined in `server-multi.lisp`, which
  loads later, so the multi-client command handlers compiled the macro calls as
  undefined-function calls and left `CONDITION` unbound at runtime.
- Fixed an infinite loop in `%consume-global-socket-flags`: its helper
  `%consume-socket-flag` popped its own *local* argv, so the caller never
  advanced and spun forever on the first `-L`/`-S` — hanging every
  `cl-tmux -L <name> <command>` at startup. The helper now returns the
  remaining argv.
- Restored the `-F`-skipping loop in `%list-commands-arguments`; a refactor had
  replaced it with a positional scan that returned the `-F` format value as the
  command name.
