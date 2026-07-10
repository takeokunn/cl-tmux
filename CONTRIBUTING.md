# Contributing to cl-tmux

Thanks for your interest in improving cl-tmux! This document explains how to
build, test, and submit changes. It also records a few project-specific rules
that are easy to trip over.

## Getting started

The only supported build path is Nix — it pins SBCL and every Lisp dependency,
so there is nothing to install by hand:

```bash
nix develop        # dev shell: SBCL + all deps (incl. FiveAM) on PATH
nix build .        # build the binary → ./result/bin/cl-tmux
nix flake check -L # build + run the full test suite
```

Inside `nix develop` you can load the system interactively:

```bash
sbcl --eval '(require :asdf)' \
     --eval '(push (truename ".") asdf:*central-registry*)' \
     --eval '(asdf:load-system :cl-tmux)'
```

## Running the tests

The suite is FiveAM-based and runs **sequentially by design** — the PTY
integration tests fork real shells via `sb-posix:fork`, and a parallel runner
corrupts forked-child state and leaks reader threads. Do not parallelize it.

```bash
# Preferred: hermetic run through Nix (same as CI)
nix flake check -L

# Or, per-system:
nix build .#checks.<system>.default -L   # e.g. aarch64-darwin, x86_64-linux

# Or, in the dev shell:
sbcl --eval '(require :asdf)' \
     --eval '(push (truename ".") asdf:*central-registry*)' \
     --eval '(asdf:test-system :cl-tmux)' \
     --quit
```

PTY tests self-skip when `/dev/ptmx` is unavailable (e.g. inside the Nix
sandbox on some platforms), so a sandboxed check run is still meaningful.

## Project-specific rules

- **The flake only sees git-tracked files.** `nix build`/`nix flake check`
  copy the *git tree*, not the working directory. If you add a new source
  file — including any file pulled in by a loader `load` form — you must
  `git add` it before the Nix build can see it, or you get a confusing
  "file not found" failure.
- **Tests must not leak global state.** Tests that `bind` keys, set options,
  or install hooks must wrap themselves in the isolation helpers
  (`with-isolated-config`, `with-isolated-hooks`, …) from `tests/helpers-*.lisp`;
  otherwise they clobber the default bindings for every test that runs after
  them.
- **Behavior changes need a tmux reference.** cl-tmux aims for behavioral
  parity with tmux. When changing or adding command/format/escape behavior,
  state in the PR what tmux does (man page section, upstream source, or a
  transcript from a real tmux session) and add a regression test that pins it.
- **Check existing tests before flipping behavior.** Some tests deliberately
  pin *absence* of a feature. If your change makes such a test fail, flip the
  test in the same commit and explain why in the message.
- **Keep the data/logic layering.** `src/` follows a layered layout
  (`domain` / `application` / `infrastructure` / `presentation` / `bootstrap`);
  terminal code further separates data structs (`types`) from logic
  (`actions`, `csi`, `sgr`). New code should land in the matching layer, and
  new public accessors must also be re-exported from the umbrella packages in
  `src/bootstrap/package*.lisp`.

## Commit and PR guidelines

- Work in small, build-verified batches; every commit should pass
  `nix flake check`.
- Write imperative, present-tense commit subjects ("Add X", "Fix Y").
- One logical change per PR. Refactors and behavior changes go in separate
  commits so the behavior diff stays reviewable.
- CI must be green before review.

## Reporting bugs

Please include:

1. what you ran (command line, config file, the byte/escape sequence if it is
   an emulation bug),
2. what tmux does in the same situation, and
3. what cl-tmux does instead.

A failing FiveAM test case is the ideal bug report.
