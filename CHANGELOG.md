# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Initial public development. Highlights of what the tree contains today:

- tmux-compatible terminal multiplexer written in pure Common Lisp (SBCL +
  sb-posix + CFFI; no custom C).
- Full command surface: every command name and alias in tmux's command table
  resolves, with flag-level parity validated against upstream behavior.
- VT100/ANSI emulator: SGR (16/256/true color), alternate screen, scroll
  regions, charsets G0–G3, DECDHL/DECDWL double-size lines, bracketed paste,
  mouse reporting, OSC 133 prompt marks, UTF-8 with wide (CJK) cells.
- Copy mode with vi-style navigation, selection, search, and the
  `-X` command set; paste buffers.
- Format-string engine (`#{...}`) with the full modifier set and several
  hundred format variables.
- Options (server/session/window/pane scopes), hooks, key tables, and
  `.tmux.conf`-style configuration including `%if`/`%hidden`, variable
  assignments, `source-file`, and command aliases.
- Client/server over per-user Unix sockets (`-L`/`-S`, `$TMUX_TMPDIR`),
  session groups sharing one window set, control mode (`-C`).
- FiveAM test suite (11,000+ checks) run hermetically via `nix flake check`.
