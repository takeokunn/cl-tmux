# tmux compatibility statement

cl-tmux targets **behavioral parity with tmux**, validated by a large
regression suite (11,000+ FiveAM checks) that pins each verified behavior.
This document states what that means in practice: what is implemented, what
is deliberately different, and where the remaining risk lives.

## Implemented

- **Commands.** Every primary command name in tmux's command table resolves
  in cl-tmux (verified by a deterministic diff against upstream's `cmd_table`,
  both directions — no missing names, no invented ones). Flag-level behavior
  has been closed against upstream semantics across multiple audit sweeps;
  each closed item is pinned by a regression test.
- **Configuration.** `.tmux.conf`-style files: `bind-key`/`unbind-key`
  (including brace blocks and `\;` sequences), `set-option` in all scopes
  with `-a`/`-g`/`-o`/`-w`/`-s` and friends, `if-shell`, `run-shell`,
  `source-file` (with tmux's failure semantics), `set-environment`,
  `set-hook` (including `-w`/`-p` object scoping),
  `%if`/`%elif`/`%else`/`%endif`, `%hidden`, tmux 3.2 variable assignments
  (`NAME=value`), line continuations, and tmux quoting/escape rules.
- **Format strings.** The full documented modifier set
  (`b: d: U: L: n: =N: pN: s/// E: t: m: C: a: q: l:`, comparison/boolean
  operators, and `W:`/`S:`/`P:` iteration) plus 160+ format variables,
  including client, session, window, pane, cursor/mode flags, mouse, and
  search state.
- **Terminal emulation.** VT100/ANSI with 16/256/true color SGR, alternate
  screen, scroll regions, origin mode, G0–G3 charsets with line-drawing
  remap, DECDHL/DECDWL double-size lines (re-emitted to the outer terminal,
  tmux's own strategy), bracketed paste, SGR mouse reporting, OSC 133 prompt
  marks (with copy-mode prompt jumping), UTF-8 with wide (CJK) cells, and
  scrollback with join-aware capture (`capture-pane -J`, `-S`/`-E` line
  ranges).
- **Copy mode.** vi-style navigation, selection (including rectangle),
  search, word/line/paragraph motions, and the `copy-mode -X` command set;
  mouse entry (`-M`); paste buffers with `paste-buffer -p` bracketed paste.
- **Client/server.** Per-user socket directories (`-L`/`-S`, `$TMUX_TMPDIR`,
  mode `0700`, stale-socket recovery), detach/attach, multiple sessions,
  session groups sharing one window set, winlink indexes (`link-window`),
  control mode (`-C` `%output`/`%window-pane-changed`/…), and hooks
  (including alert hooks and scoped hooks).

## Intentionally different

- **Canonical command names only.** tmux short aliases (`neww`, `splitw`,
  `killp`, …) are deliberately rejected by the command parser rather than
  kept as a compatibility layer. Configs written with aliases need to be
  spelled out; genuine typos are rejected with an error instead of being
  silently ignored.
- **`switch-mode` is not implemented.** It was reported as a recent upstream
  command, but its exact semantics could not be verified against upstream
  docs at the time; implementing wrong semantics was judged worse than
  absence. Re-check when upstream documentation is reachable.
- **SBCL-specific process model.** PTYs are spawned via `cl-tty-kit:make-pty`
  (which uses `sb-ext:run-program :pty t`) rather than `forkpty(3)`, so the
  slave path is not exposed (reported as an empty string where tmux would
  report a device path).

## Known remaining risk

- **Flag-level diffs vs. newer tmux releases.** Parity was closed against
  tmux 3.x behavior as documented/observable at audit time; flags added in
  newer releases may be missing until re-audited.
- **Ecosystem fixtures.** Complex status-line configurations (powerline,
  catppuccin, tpm plugins) have not been run as fixtures; the format engine
  covers the documented surface, but untested combinations may expose gaps.
- **Soak behavior.** Long-running-session behaviors (history pressure, many
  clients) are covered by unit/integration tests, not by long soak runs.

Bug reports that include what real tmux does in the same situation are the
fastest to act on — see the issue templates.
