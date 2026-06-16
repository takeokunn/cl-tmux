# cl-tmux tmux-Compatibility Audit

## 2026-06-14 tmux 3.6a Baseline

This repository is not yet proven tmux-compatible.  The current real-tmux
baseline was measured locally with tmux 3.6a against a temporary clean server
started with `tmux -L <clean> -f /dev/null new-session -d`:

| Surface | tmux command | Count |
|---------|--------------|-------|
| Commands | `tmux -L <clean> -f /dev/null list-commands` | 90 |
| Named bindings | `tmux -L <clean> -f /dev/null list-keys -N` | 87 |
| Root bindings | `tmux -L <clean> -f /dev/null list-keys -T root` | 19 |
| Prefix bindings | `tmux -L <clean> -f /dev/null list-keys -T prefix` | 87 |
| Copy-mode bindings | `tmux -L <clean> -f /dev/null list-keys -T copy-mode` | 74 |
| Copy-mode-vi bindings | `tmux -L <clean> -f /dev/null list-keys -T copy-mode-vi` | 87 |
| Global options | `tmux -L <clean> -f /dev/null show-options -g` | 61 |
| Window options | `tmux -L <clean> -f /dev/null show-window-options -g` | 67 |
| Hooks | `tmux -L <clean> -f /dev/null show-hooks -g` | 57 |
| Format variables | `tmux 3.6a` man `FORMATS` plus display-menu popup variable table | 226 |

New machine-readable compatibility state lives in
`test/tmux-compat-matrix.sexp`, and `test/compat/matrix-tests.lisp` now runs
real tmux inventory checks when a local tmux binary is available.  On tmux 3.6a
the test requires the measured inventory counts above to stay stable and
requires every command from `tmux list-commands` and every default key from
`tmux list-keys -T ...` to have a matrix row.  On other tmux versions it records
the version mismatch by skipping the count equality check instead of pretending
the baseline applies.

Known current gaps recorded in the matrix:

- `window-size`: real tmux defaults to `latest`; cl-tmux currently defaults to
  `smallest` for its shared-frame model.
- Option defaults now have row-level tmux 3.6a checks for all 128 clean
  `show-options -g` and `show-window-options -g` rows.  52 default values match
  the cl-tmux registry, 36 are recorded as `:partial` default divergences, and
  40 exact tmux option rows are missing from the registry.  Global options are
  27/13/21; window options are 25/23/19.  These rows prove default values only;
  parser behavior, option scope, validation, formatting, and runtime side
  effects are still not exhaustively proven.
- Real tmux commands are now enumerated as 90 matrix rows.  All 90 names are
  present in cl-tmux's combined bindable-command and argv-command inventories,
  so they are recorded as `:partial` with `:cl-tmux-command t`; cl-tmux also has
  121 command names or aliases that are not tmux 3.6a public command names.
  `list-commands -F '#{command_list_name}'` now uses the tmux public 90-name
  inventory instead of exposing cl-tmux's internal bindable helper commands.
  This is still not complete command compatibility: parser, alias, usage, flag,
  output, server behavior, side-effect, and error behavior remain to be tested
  against real tmux.
- Real tmux default binding tables (`root`, `prefix`, `copy-mode`,
  `copy-mode-vi`) are now enumerated as 267 matrix rows from
  `tmux list-keys -T ...`.  151 rows have a cl-tmux table/key entry and are
  recorded as `:partial`; 116 are absent and recorded as `:missing` (root 0/19,
  prefix 39/48, copy-mode 50/24, copy-mode-vi 62/25).  These rows prove
  inventory and key-label presence only; repeat flags, notes, command strings,
  dispatch semantics, copy-mode behavior, and mouse/menu behavior are not
  differentially proven.  Named binding notes from `list-keys -N` remain
  count-only.
- Real tmux hooks are now enumerated as 57 matrix rows from
  `tmux show-hooks -g`.  19 are present in cl-tmux hook event constants and
  recorded as `:partial`; 38 are absent and recorded as `:missing`.  cl-tmux
  also has 8 hook constants that are not tmux 3.6a global hook names.  These
  rows prove inventory and event-name presence only; `set-hook`, `show-hooks`,
  `run-hook`, firing semantics, and hook format variables are not
  differentially proven.
- Real tmux format variables are now enumerated as 226 matrix rows from the tmux
  3.6a `FORMATS` table plus the display-menu popup variable table.  74 are
  present in `cl-tmux/format:format-context-from-session` and recorded as
  `:partial`; 152 are absent and recorded as `:missing`.  These rows prove
  inventory and context presence only; aliases, modifiers, values, and
  context-specific behavior are not differentially proven.
- Remaining unexpanded surfaces: named binding notes are counted but not
  row-classified.
- Non-TTY query command behavior is now covered for `list-commands
  -F '#{command_list_name}'` by a real differential test when
  `CL_TMUX_COMPAT_BINARY` or `result/bin/cl-tmux` is available.  The `lscm`
  alias is also covered for the same command-name format output.  Usage strings
  and aliases inside command sequences are still unproven.  The `display`
  alias is covered for no-server `-p hello` failure behavior; live-server
  `display-message`, target/client flags, format context, verbose/literal
  flags, hooks, overlays, and in-session behavior remain unproven.  `list-sessions`
  no-server behavior is also covered for stdout, normalized stderr connection
  failure, and exit code.
  `has-session -t no-such-session-xyz`, its `has` alias, and `kill-server`
  no-server behavior are covered for stdout, normalized stderr connection
  failure, and exit code.
  `list-windows` no-server behavior is covered for stdout, normalized stderr
  connection failure, and exit code.  `show-options -g` no-server behavior is
  covered for stdout, normalized stderr connection failure, and exit code.
  `show-window-options -g` no-server behavior is covered for stdout, normalized
  stderr connection failure, and exit code; live-server `show-options` and
  `show-window-options` output,
  scope selection, quiet flags, target-window semantics, and option formatting
  remain unproven.
  Detached `new-session
  -d -s beta -n two`
  against an existing live server is covered by a real differential test that
  compares resulting `list-sessions -F '#{session_name}'` output and the beta
  `list-windows -a -F '#{session_name}:#{window_name}'` row.  Under threaded
  SBCL this cl-tmux path registers a query-visible no-PTY placeholder pane,
  so it does not prove a shell-backed pane that can later be attached or used.
  Live `list-windows` behavior beyond that beta session/window-name
  observation, filters, formatting, flags, target semantics, attached
  `new-session`, grouping, duplicate-name errors, and socket-name semantics
  remain unproven.

The older audit below is retained as historical source-backed analysis, but it
must not be read as a complete proof of current compatibility.

---

This report assesses whether each cl-tmux command behaves like real tmux. Each
divergence was independently verified against the source; verdicts are
`confirmed` (source-backed), `partial` (real but minor / narrow trigger), or
`unverified` (could not be confirmed — none in this run). Explicitly refuted
findings have been excluded.

---

## 1. Executive Summary

Overall, cl-tmux reproduces the *shape* of tmux's prefix-key workflow (C-b, then
a command key) and the default key glyphs mostly match. However, several core
behaviors diverge in ways that affect correctness, navigation, and data safety.

**Divergence counts (final severity):**

| Severity | Count |
|----------|-------|
| High     | 13    |
| Medium   | 21    |
| Low      | 14    |
| **Total**| **48**|

All 16 commands in scope were audited successfully. **No audit agents failed**,
so there are no un-checked commands.

**Top correctness risks (fix these first):**

1. **Unbound prefix keys corrupt the running program's input.** Any
   `C-b <unbound-key>` writes the raw prefix byte (0x02) *plus* the key into the
   active pane's PTY instead of being discarded. This silently corrupts whatever
   is running in the pane. (`send-prefix`, `prefix-key-pipeline`, high)
2. **`C-b C-b` sends two C-b bytes instead of one literal prefix.** There is no
   `send-prefix` command, so the canonical "pass a literal prefix to a nested
   program" gesture is broken. (`send-prefix`, high)
3. **Window selection is by list position, not window index.** `C-b N` uses
   `(nth n windows)`, so the digit is off-by-one vs. the displayed label, and
   killing a middle window re-packs positions so the same digit later targets a
   different window. (`select-window`, high)
4. **Splits re-divide the whole window into N equal panes instead of halving
   the active pane**, and a window can hold only one split orientation — mixed /
   nested layouts are impossible. (`split-horizontal` / `split-vertical`, high)
5. **No directional pane navigation.** `C-b Up/Down/Left/Right` are unbound;
   the only pane focus is cyclic `C-b o`. H/J/K/L are repurposed for resize.
   (`select-pane`, `default-bindings`, high)
6. **Window numbering collides after a kill.** New-window index is
   `(1+ (length windows))`, so killing a middle window then creating one
   produces a duplicate number. (`new-window`, high)
7. **Destructive commands skip tmux's `confirm-before` prompt.** `C-b x`
   (kill-pane) and `C-b &` (kill-window) destroy panes/windows on a single
   keystroke. (`kill-pane`, `kill-window`, high/medium)

Also notable: the headline copy-mode UX is broken — plain `q` does **not** exit
copy mode (only `C-b q`), and scroll keys `[`/`]` are invented, prefix-gated
bindings that real tmux does not use.

---

## 2. Compatibility Matrix

| Command | Key binding matches tmux? | # divergences | Worst severity |
|---------|---------------------------|---------------|----------------|
| new-window (C-b c) | Yes | 4 | High |
| kill-window (C-b &) | **No** (no confirm prompt) | 2 | High |
| kill-pane (C-b x) | Yes | 4 | Medium |
| next-window / previous-window (C-b n / C-b p) | Yes | 0 | — |
| select-window -t N (C-b 0..9) | Yes | 3 | High |
| rename-window (C-b ,) | Yes | 4 | Medium |
| split-window (C-b ") top/bottom | Yes | 4 | High |
| split-window -h (C-b %) left/right | Yes | 4 | Medium |
| select-pane (C-b o cycles) | Yes | 2 | High |
| resize-pane (C-b C-arrow / M-arrow) | **No** (bound to H/J/K/L) | 6 | High |
| copy-mode (C-b [), scroll, q to exit | Yes (entry) | 5 | High |
| list-keys (C-b ?) | Yes | 3 | Medium |
| detach-client (C-b d) | Yes | 0 | — |
| send-prefix (C-b C-b) | **No** (not bound at all) | 3 | High |
| prefix key C-b + keystroke routing | Yes | 3 | High |
| default key-binding table (whole) | **No** (multiple) | 6 | High |

---

## 3. Divergences (grouped by command, high → low severity)

### new-window (C-b c) — binding matches tmux

**[High] Window numbering is count-based, producing duplicate numbers after a kill**
- Expected: tmux assigns the lowest unused index ≥ base-index, filling gaps (kill 1 of 0,1,2 → new window is 1).
- Actual: both id and name are `(1+ (length windows))`; killing a middle window of 1,2,3 then creating one yields `(1+ 2)=3`, duplicating the existing window 3.
- Verdict: confirmed.
- Evidence: `src/model.lisp:145`; `src/events.lisp:28`.
- Fix: compute the new index as the smallest non-negative integer not already used by any window.

**[Medium] New window name is a numeric string, not the running command name**
- Expected: a new window's default name derives from the foreground program (zsh, vim, …) and is live-updated by automatic-rename.
- Actual: name = decimal string of `(1+ (length windows))`; no automatic-rename anywhere.
- Verdict: confirmed.
- Evidence: `src/events.lisp:28`; `src/model.lisp:143-158`; `src/renderer.lisp:117-118`.
- Fix: default the name to the spawned command's basename and add automatic-rename tracking of the active pane's foreground process.

**[Medium] First window is numbered 1, not 0 (base-index default wrong)**
- Expected: default base-index is 0; first window is index 0.
- Actual: initial window is created as "1" and new windows start at 2,3,…; no base-index option.
- Verdict: confirmed.
- Evidence: `src/model.lisp:166`, `src/model.lisp:145`; `src/events.lisp:28`.
- Fix: introduce a `base-index` option defaulting to 0 and seed the initial window from it.

**[Low] New window is appended to the tail; no insertion at lowest free slot**
- Expected: tmux keeps windows ordered by index and inserts at the lowest free index.
- Actual: `session-new-window` unconditionally appends; order is creation order.
- Verdict: confirmed.
- Evidence: `src/model.lisp:155-156`.
- Fix: insert the new window so the list stays sorted by index (corollary of fixing numbering above).

---

### kill-window (C-b &) — binding differs (no confirm prompt)

**[High] `&` kills the window immediately with no confirmation prompt**
- Expected: default binding is `confirm-before -p "kill-window #W? (y/n)" kill-window`; only y/Y destroys it.
- Actual: `&` maps directly to `:kill-window`; the dispatch arm destroys all panes and removes the window with no prompt. No `confirm-before` exists in src/.
- Verdict: confirmed.
- Evidence: `src/config.lisp:33`; `src/events.lisp:126-131`; `src/commands.lisp:26-40`.
- Fix: route `&` through a `confirm-before` prompt (the prompt infra used by rename-window can be reused).

**[Medium] After killing the active window, the lowest-index window is selected, not the adjacent one**
- Expected: tmux selects the next window by index (wrapping), or the last-used window.
- Actual: always re-selects `(first remaining)` regardless of position; no last-window tracking.
- Verdict: confirmed.
- Evidence: `src/commands.lisp:38-39`; `src/model.lisp:132-137`.
- Fix: track a last-window stack and/or select the numeric neighbour of the killed window.

---

### kill-pane (C-b x) — binding matches tmux

**[Medium] No confirmation prompt before killing the pane**
- Expected: `x` → `confirm-before -p "kill-pane #P? (y/n)" kill-pane`.
- Actual: `x` maps directly to `:kill-pane`; pane destroyed on one keystroke.
- Verdict: confirmed.
- Evidence: `src/config.lisp:32`; `src/events.lisp:120-124`; `src/commands.lisp:8-24`.
- Fix: gate `x` behind a `confirm-before` prompt.

**[Medium] Survivor pane selection uses list order, not the active-pane MRU / spatial choice**
- Expected: activate the previously-active pane (last-pane MRU stack), else the spatial neighbour.
- Actual: always selects `(first remaining)`, the oldest pane in list order.
- Verdict: confirmed.
- Evidence: `src/commands.lisp:15,22`.
- Fix: maintain a per-window last-pane stack; fall back to a spatial neighbour.

**[Medium] Empty-window fallthrough selects the first window, not last/next**
- Expected: when killing the last pane destroys the window, switch to the MRU window then the numeric neighbour.
- Actual: falls through to kill-window, which selects `(first remaining)`.
- Verdict: confirmed.
- Evidence: `src/commands.lisp:20`, `src/commands.lisp:38-39`.
- Fix: same last-window stack fix as kill-window.

**[Low] Killing a non-active pane silently moves focus (latent / dead path)**
- Expected: killing a non-active pane leaves the active pane unchanged.
- Actual: `window-select-pane` is called unconditionally on the survivor. (Unreachable via `C-b x` since the only caller passes no pane arg.)
- Verdict: partial.
- Evidence: `src/commands.lisp:22`.
- Fix: only re-select when the killed pane was the active pane.

---

### select-window -t N (C-b 0..9) — binding matches tmux

**[High] Window selected by 0-based list position, not by window index/number**
- Expected: `select-window -t N` selects the window whose *index* equals N (stable, honors base-index, survives gaps/move-window).
- Actual: `(nth n windows)` selects the Nth list element; window `id` is never consulted for selection.
- Verdict: confirmed.
- Evidence: `src/commands.lisp:51-55`; `src/events.lisp:163-165`; `src/model.lisp:36-44,145`.
- Fix: select by matching the window's stored index, not list position.

**[High] Digit-to-window mapping is off-by-one vs. the displayed label**
- Expected: with default base-index 0, the visible label matches the digit pressed.
- Actual: windows are labeled 1,2,3,… but selection is 0-based, so the window labeled "1" is hit by `C-b 0`; the digit is always one less than the label.
- Verdict: confirmed.
- Evidence: `src/model.lisp:166`; `src/events.lisp:28`; `src/renderer.lisp:115-118`; `src/commands.lisp:53`.
- Fix: make labels and selection both use the same per-window index (fixes base-index too).

**[Medium] Killing a middle window re-packs selection positions instead of preserving stable indices**
- Expected: indices stay stable after a kill (0,1,2,3 → 0,2,3; `C-b 3` still hits 3, `C-b 1` is a no-op).
- Actual: `(remove target …)` compacts the list, so the same digit later targets a different window.
- Verdict: confirmed.
- Evidence: `src/commands.lisp:30-31,53`.
- Fix: index-based selection (downstream of the first finding here).

---

### rename-window (C-b ,) — binding matches tmux

**[Medium] Standard command-prompt line-editing keys are unsupported**
- Expected: emacs-style editing — C-a/C-e, C-b/C-f or arrows, C-k, C-u, C-w, mid-string insert/delete.
- Actual: append-only; insertion concatenates to the end, backspace trims the last char, no cursor index; control keys <32 ignored; arrows only handled in copy mode.
- Verdict: confirmed.
- Evidence: `src/events.lisp:77-86`; `src/prompt.lisp:11-15,32-43`.
- Fix: add a cursor index to the prompt struct and implement the emacs editing key-table.

**[Low] Non-ASCII / UTF-8 window names cannot be entered**
- Expected: tmux accepts UTF-8 in the command-prompt.
- Actual: only bytes 32–126 are inserted; every byte ≥127 (all UTF-8 lead/continuation bytes) is dropped.
- Verdict: confirmed.
- Evidence: `src/events.lisp:85` (and docstring at `:74-76`).
- Fix: accumulate and decode UTF-8 byte sequences in the prompt input path.

**[Low] Empty rename input unconditionally sets an empty window name**
- Expected: tmux's rename-window with an empty argument affects automatic-rename state; the common case (Enter on the seeded value) re-applies the name.
- Actual: Enter setfs `window-name` to the raw buffer with no guard; an empty buffer yields a blank name. No automatic-rename concept exists.
- Verdict: partial.
- Evidence: `src/commands.lisp:44-47`; `src/events.lisp:78-82`.
- Fix: define empty-name semantics once automatic-rename exists (low priority).

---

### split-window (C-b ") top/bottom — binding matches tmux

**[High] Each split re-divides the WHOLE window into N equal panes instead of halving the active pane**
- Expected: split halves only the active pane; other panes keep size/position.
- Actual: `window-split` calls `divide-window` with n = total pane count and repositions every pane into n equal slots; the active pane is never scoped.
- Verdict: confirmed.
- Evidence: `src/model.lisp:57-66,100-119`; `test/layout-tests.lisp:95-106`.
- Fix: split only the active pane's rectangle; leave siblings untouched (requires a tree layout).

**[High] A window can hold only one split orientation; a second split in the other direction reorients all panes**
- Expected: arbitrary nested layouts (split a pane vertically, then split a child horizontally).
- Actual: `window-layout` stores a single direction; `window-relayout` re-divides every pane in that one direction, so mixed layouts can't be represented.
- Verdict: confirmed.
- Evidence: `src/model.lisp:77,80-92`.
- Fix: replace the scalar layout with a binary split tree.

**[Medium] No minimum-size guard: split always succeeds and forks a shell even at 1×1 panes**
- Expected: tmux refuses a split when the pane is too small ("create pane failed: pane too small").
- Actual: `divide-window` clamps slots with `(max 1 …)`, so a split always produces ≥1×1 panes and unconditionally forks a shell.
- Verdict: confirmed.
- Evidence: `src/model.lisp:107,111,115,119,67-74`; `test/layout-tests.lisp:83-91`.
- Fix: add a minimum-size precondition that aborts the split with an error message.

**[Low] Internal orientation naming is inverted vs. tmux's -h/-v (cosmetic)**
- Expected: tmux `"` is split -v (top/bottom), `%` is -h (left/right).
- Actual: `"` → `:split-horizontal`, `%` → `:split-vertical`; the user-facing geometry is correct but the labels are swapped vs. tmux's -v/-h naming.
- Verdict: confirmed.
- Evidence: `src/config.lisp:26-27`; `src/model.lisp:113-119`.
- Fix: rename the keywords to match tmux's -v/-h convention.

---

### split-window -h (C-b %) left/right — binding matches tmux

**[Medium] split-window re-divides the entire window instead of splitting only the active pane**
- Expected: `-h` splits only the active pane's region in two; other panes untouched.
- Actual: `window-split` ignores the active pane and redistributes ALL panes into N equal columns, flattening any prior top/bottom layout.
- Verdict: confirmed.
- Evidence: `src/model.lisp:53-78,100-121`; `src/events.lisp:46-50`.
- Fix: scope the split to the active pane (same tree-layout fix as above).

**[Medium] Window only tracks a single split orientation, so mixed/nested splits are impossible**
- Expected: a tree layout supporting e.g. left/right where one side is split top/bottom.
- Actual: single global `:layout` direction; every relayout re-divides uniformly.
- Verdict: confirmed.
- Evidence: `src/model.lisp:77,80-92,100-121`.
- Fix: binary split tree (same as split " finding).

**[Low] No minimum-size guard: split always succeeds even with no room**
- Expected: tmux refuses with "create pane failed: pane too small".
- Actual: `(max 1 …)` clamps slots; split always proceeds, forking a shell and reader thread, possibly creating 1-column panes. (Practical impact limited given the already-flat layout.)
- Verdict: partial.
- Evidence: `src/model.lisp:106-112,53-78`; `src/events.lisp:46-50`.
- Fix: add a minimum-size guard.

**[Low] Internal orientation keyword is inverted vs. tmux flag naming**
- Expected: `-h` (`%`) is HORIZONTAL (left/right).
- Actual: `%` → `:split-vertical` → `:vertical` (left/right); geometry correct but keyword inverted.
- Verdict: confirmed.
- Evidence: `src/config.lisp:27`; `src/events.lisp:117-118`; `src/model.lisp:105-112`.
- Fix: rename keywords to match tmux flags.

---

### select-pane (C-b o cycles) — binding matches tmux (entry)

**[High] Arrow keys (C-b Up/Down/Left/Right) are not bound to directional select-pane; H/J/K/L are bound to resize instead**
- Expected: `C-b Up/Down/Left/Right` → `select-pane -U/-D/-L/-R`; resize is on C-arrow / M-arrow, not H/J/K/L.
- Actual: no directional select-pane exists; only cyclic `:next-pane`. Plain arrows are forwarded to the shell outside copy mode. H/J/K/L are bound to resize.
- Verdict: confirmed.
- Evidence: `src/config.lisp:35-38`; `src/events.lisp:108-112,300-306`.
- Fix: add a directional select-pane command and bind it to `C-b` arrows.

**[Medium] No default binding for last-pane (`C-b ;`)**
- Expected: `C-b o` (next, wrapping) and `C-b ;` (last/MRU pane).
- Actual: the `last-pane` command exists but is never bound by default; `;` is unbound and passes through. Only forward cycling with `o` works.
- Verdict: confirmed.
- Evidence: `src/config.lisp:20-41`; `src/dispatch-core-commands.lisp:283-286`.
- Fix: add a `C-b ;` last-pane binding (needs the last-pane MRU stack).

---

### resize-pane (C-b C-arrow / C-b M-arrow) — binding differs (bound to H/J/K/L)

**[High] Resize is bound to prefix H/J/K/L instead of tmux's C-arrow / M-arrow keys**
- Expected: resize-pane on repeatable C-Left/Right/Up/Down (1 cell) and M-arrows (5 cells).
- Actual: bound to `H/J/K/L`; no binding for Ctrl/Meta arrow sequences.
- Verdict: confirmed.
- Evidence: `src/config.lisp:35-38`.
- Fix: parse C-arrow / M-arrow escape sequences after the prefix and bind them to resize.

**[High] Resize direction gated on the window's single global layout, so the orthogonal axis is always a no-op**
- Expected: resize works in any direction with a neighbour; a window can have both axes.
- Actual: `:left/:right` require layout `:vertical`, `:up/:down` require `:horizontal`; the perpendicular direction can never act.
- Verdict: confirmed.
- Evidence: `src/commands.lisp:66-75`; `src/model.lisp:44,77`.
- Fix: resolve the border to move from the layout tree, independent of a single global orientation.

**[Medium] Resize only repositions the active pane and one neighbour, corrupting layouts of 3+ panes**
- Expected: tmux reflows the whole layout so there are no gaps/overlaps.
- Actual: only PANE and its immediate ADJ neighbour are moved; other panes keep old geometry.
- Verdict: confirmed.
- Evidence: `src/commands.lisp:77-98,100-121`.
- Fix: reflow all affected panes after a border move.

**[Medium] Resizing the last pane toward its trailing edge is a silent no-op**
- Expected: `-R`/`-D` pick a real border to move even on the trailing pane.
- Actual: for `:right`/`:down`, the neighbour is `(nth (min (1+ idx) (1- length)) panes)`, which equals PANE itself for the last pane; the `(not (eq adj pane))` guard then no-ops.
- Verdict: confirmed.
- Evidence: `src/commands.lisp:82-85,104-108`.
- Fix: when at the trailing edge, move the inner (leading) border instead.

**[Medium] Fixed resize amount of 5 with no 1-cell variant and no repeat**
- Expected: C-arrow = 1 cell, M-arrow = 5 cells, both repeatable (-r).
- Actual: dispatch always uses the default 5; no 1-cell variant, no repeat, so each step needs another prefix press.
- Verdict: confirmed.
- Evidence: `src/events.lisp:157-160`; `src/commands.lisp:59`; `src/events.lisp:183-198`.
- Fix: add 1-cell and 5-cell variants and a repeatable key-table.

**[Low] Minimum-size guard requires size > 2 rather than tmux's smaller floor**
- Expected: tmux allows panes down to ~1 line / a couple columns.
- Actual: both grown and shrunk panes must stay strictly > 2 (≥3); stricter than tmux.
- Verdict: confirmed.
- Evidence: `src/commands.lisp:87-88,110-111`.
- Fix: lower the floor to match tmux's PANE_MINIMUM.

---

### copy-mode (C-b [), scroll, q to exit — entry binding matches tmux

**[High] Scroll keys `[` and `]` require the prefix and are not real tmux copy-mode bindings**
- Expected: copy-mode scrolling uses unprefixed Up/Down/k/j, PageUp/PageDown, C-u/C-d; `[`/`]` are not movement keys.
- Actual: `[`/`]` are intercepted as scroll only after the prefix (`C-b [` / `C-b ]`); a plain `[`/`]` is forwarded to the shell.
- Verdict: confirmed.
- Evidence: `src/events.lisp:191-195,305-306`.
- Fix: handle unprefixed navigation keys directly while copy mode is active.

**[Medium] No PageUp/PageDown support**
- Expected: PageUp/PageDown scroll one page in copy mode.
- Actual: only `ESC [ A` / `ESC [ B` (Up/Down) are handled; PageUp/PageDown (`ESC [ 5 ~` / `ESC [ 6 ~`) are 4-byte and never recognized — forwarded to the shell.
- Verdict: confirmed.
- Evidence: `src/events.lisp:213-228,262-268`.
- Fix: extend the escape parser to recognize the `ESC [ N ~` form.

**[Medium] No vi/emacs navigation keys (j/k, C-n/C-p, g/G, C-u/C-d, search)**
- Expected: full mode-keys navigation (line motion, top/bottom, half-page, word motion, search).
- Actual: only viewport Up/Down (and prefixed `[`/`]`); everything else is forwarded to the shell.
- Verdict: confirmed.
- Evidence: `src/events.lisp:151-155,202-235`.
- Fix: implement a copy-mode key-table (vi and emacs variants).

**[Low] Scroll moves the whole viewport with no cursor and a fixed 3-line step**
- Expected: a cursor moves one line per keystroke, scrolling only at the edge.
- Actual: no cursor; each Up/Down scrolls the viewport by a fixed 3 lines.
- Verdict: confirmed.
- Evidence: `src/commands.lisp:135-143`; `src/events.lisp:152,155,218,226`.
- Fix: introduce a copy-mode cursor and selection model.

---

### list-keys (C-b ?) — binding matches tmux

**[Medium] Overlay is dismissed by ANY keypress instead of being a navigable pager exited with q**
- Expected: list-keys output is a scrollable view-mode pager dismissed with q/Escape.
- Actual: any single keystroke clears the overlay and is swallowed; no scrolling/search/paging.
- Verdict: confirmed.
- Evidence: `src/events.lisp:286`; `src/prompt.lisp:64-70`.
- Fix: render list-keys into copy/view mode rather than a one-shot modal overlay.

**[Low] Help output is truncated to the top rows with no scrolling**
- Expected: the pager scrolls through the entire list regardless of terminal height.
- Actual: `render-overlay` draws from row 0 with no scroll offset and hard-truncates each line to COLS. (Only bites on very short terminals; the default table is short.)
- Verdict: confirmed.
- Evidence: `src/renderer.lisp:134-141`.
- Fix: add a scroll offset / paging once the overlay becomes a pager.

**[Low] list-keys content differs from tmux format (no bind-key syntax, custom header)**
- Expected: one `bind-key [-T table] key command` line per binding across all key tables.
- Actual: a custom human header plus `  <key>  <command-keyword>` lines for the prefix table only; copy-mode/root tables omitted.
- Verdict: confirmed.
- Evidence: `src/config.lisp:47-56,88-98`.
- Fix: emit `bind-key` syntax across all key tables.

---

### send-prefix (C-b C-b) — not bound at all

**[High] `C-b C-b` sends TWO C-b bytes instead of one literal prefix**
- Expected: `C-b C-b` → `send-prefix`, sending exactly one 0x02 byte to the pane.
- Actual: the second `C-b` is unbound, so the otherwise arm calls `%passthrough-prefix`, which writes the prefix byte *and* the follow-up byte → 0x02 0x02.
- Verdict: confirmed.
- Evidence: `src/config.lisp:20-41`; `src/events.lisp:167-169,52-62`.
- Fix: add a `send-prefix` command and bind `C-b C-b` to it.

**[High] Any unbound key after the prefix is injected into the pane as prefix+key**
- Expected: a prefix followed by an unbound key is a no-op (key not delivered to the pane; maybe a bell).
- Actual: the otherwise arm injects 0x02 + the key into the active pane's PTY for every unrecognized key (e.g. `C-b z` → ^B z), corrupting the foreground program's input.
- Verdict: confirmed.
- Evidence: `src/events.lisp:167-169,52-62,183-198`.
- Fix: make unbound prefix-table keys a no-op (drop the key; optional bell), never passthrough.

**[Medium] send-prefix is not a bindable command and cannot be rebound**
- Expected: `send-prefix` is a first-class, rebindable command tied to the configured prefix.
- Actual: no `:send-prefix` keyword; behavior exists only as the catch-all passthrough hardcoded to `+prefix-key-code+`.
- Verdict: confirmed.
- Evidence: `src/config.lisp:88-98`; `src/events.lisp:92-169,52-62`.
- Fix: add `:send-prefix` to `*bindable-commands*` and dispatch, sending exactly one prefix byte.

---

### prefix key C-b + keystroke routing — binding matches tmux

**[High] Unbound key after prefix is injected into the pane as C-b + key instead of being discarded**
- Expected: an unbound prefix-table key is consumed/dropped (at most a bell); not written to the PTY.
- Actual: cmd=NIL → otherwise → `%passthrough-prefix` writes 0x02 then the key (e.g. `C-b z` → ^B z).
- Verdict: confirmed.
- Evidence: `src/events.lisp:167-169,52-62,188-198`.
- Fix: same as above — drop unbound prefix keys.

**[Medium] Pressing the prefix twice (C-b C-b) sends two C-b bytes instead of one literal C-b**
- Expected: default `bind-key C-b send-prefix` sends a single literal C-b.
- Actual: second C-b is unbound → passthrough writes 0x02 0x02.
- Verdict: confirmed.
- Evidence: `src/events.lisp:293-298,188-198,167-169,52-62`; `src/config.lisp:20-41`.
- Fix: add the `send-prefix` binding (same root cause as the send-prefix findings).

**[Low] send-prefix is not a bindable command**
- Expected: tmux exposes `send-prefix` as a bindable command (default-bound to C-b).
- Actual: no `:send-prefix` in dispatch, `*bindable-commands*`, or defaults.
- Verdict: confirmed.
- Evidence: `src/events.lisp:90-172`; `src/config.lisp:88-98,20-41`.
- Fix: add the `send-prefix` command.

---

### default key-binding table (whole) — multiple mismatches

**[High] Pane-selection arrow keys (prefix Up/Down/Left/Right) are not bound**
- Expected: `prefix Up/Down/Left/Right` → `select-pane -U/-D/-L/-R`.
- Actual: no arrow entries; arrows are interpreted only in copy mode; outside it they passthrough. No directional select-pane command at all.
- Verdict: confirmed.
- Evidence: `src/config.lisp:20-41,88-93`; `src/events.lisp:202-235,167-169`.
- Fix: add directional select-pane and bind the arrows.

**[Medium] prefix x (kill-pane) skips tmux's confirm-before prompt**
- Expected: `bind x confirm-before -p "kill-pane? (y/n)" kill-pane`.
- Actual: `x` → `:kill-pane` directly; SIGHUP sent on first keystroke.
- Verdict: confirmed.
- Evidence: `src/config.lisp:32`; `src/events.lisp:120-124`; `src/commands.lisp:8-24`.
- Fix: gate behind confirm-before.

**[Medium] prefix & (kill-window) skips tmux's confirm-before prompt**
- Expected: `bind & confirm-before -p "kill-window #W? (y/n)" kill-window`.
- Actual: `&` → `:kill-window` directly; all panes destroyed, no prompt.
- Verdict: confirmed.
- Evidence: `src/config.lisp:33`; `src/events.lisp:126-131`.
- Fix: gate behind confirm-before.

**[Medium] prefix digit selects the Nth window positionally instead of by index**
- Expected: `0-9` → `select-window -t :N`, honoring base-index, stable across renumber/close.
- Actual: `(nth n windows)` — 0-based list position; ignores per-window index/base-index.
- Verdict: confirmed.
- Evidence: `src/commands.lisp:51-55`; `src/events.lisp:163-165,28`.
- Fix: index-based selection.

**[Low] Resize is bound to H/J/K/L instead of tmux's Ctrl/Meta arrow keys**
- Expected: resize on C-arrows (1, repeatable) and M-arrows (5); no H/J/K/L default.
- Actual: capital H/J/K/L → resize (fixed 5, non-repeatable). Non-standard convention but functional.
- Verdict: partial.
- Evidence: `src/config.lisp:35-38`; `src/commands.lisp:59`; `src/events.lisp:157-160`.
- Fix: rebind resize to C-/M-arrows.

**[Low] resize-pane is a no-op when the requested direction doesn't match the split orientation**
- Expected: resize works on any existing border; nested layouts allow both axes.
- Actual: `ecase` on the single `(window-layout window)` — perpendicular directions and layout `nil` return without resizing.
- Verdict: confirmed.
- Evidence: `src/commands.lisp:59-75`; `src/model.lisp:77`.
- Fix: resolve borders from a layout tree (inherent limitation of the flat layout model).

---

## 4. Key-Binding Mismatches

Commands whose default key binding (or default-binding behavior) differs from
real tmux:

- **kill-window (C-b &)** — bound directly to kill; tmux uses `confirm-before` first.
- **kill-pane (C-b x)** — bound directly to kill; tmux uses `confirm-before` first.
- **resize-pane** — bound to prefix **H/J/K/L**; tmux uses **C-arrow** (1 cell) and **M-arrow** (5 cells), both repeatable. No Ctrl/Meta-arrow bindings exist.
- **send-prefix (C-b C-b)** — **not bound at all**; tmux default-binds `C-b send-prefix`.
- **Directional select-pane (C-b Up/Down/Left/Right)** — **missing entirely**; tmux binds these to `select-pane -U/-D/-L/-R`.
- **last-pane (C-b ;)** — command exists but is **unbound** by default; `;` passes through.
- **Unbound prefix keys** — passthrough-injected into the pane instead of being discarded (tmux drops them).

---

## 5. Commands That Appear Faithful

These were checked and found to behave like tmux (no kept divergences):

- **next-window / previous-window (C-b n / C-b p)** — 0 divergences.
- **detach-client (C-b d)** — 0 divergences.

Additionally, the *entry* bindings for the following match tmux even though they
have behavioral divergences elsewhere: `new-window (C-b c)`,
`select-window (C-b 0..9)`, `rename-window (C-b ,)`, `split-window (C-b " / %)`,
`copy-mode entry (C-b [)`, `list-keys (C-b ?)`, `select-pane (C-b o)`.

---

## 6. Audit Gaps

**None.** All 16 commands in scope were audited successfully; no audit agent
failed, so there are no commands requiring a re-run.

One verification caveat: every finding above carries a `confirmed` or `partial`
verdict (source-backed). There were no `unverified` findings in this run.
