# TODO

Current parity audit notes for `cl-tmux`. Most previously tracked gaps are now
either implemented or intentionally unsupported, so this file records only the
remaining compatibility checks and documentation cleanups.

## Confirmed compatibility boundaries

- Keep the canonical-only parser and treat tmux legacy shorthand forms as
  intentionally unsupported.
  - `split-window -p` percent form is intentionally rejected; `-l N%` is the
    supported percent syntax.
  - Config abbreviations / shorthand spellings are intentionally rejected by the
    config parser and covered by tests.

- Config tokenizer fidelity is already implemented.
  - Quote handling is covered by regression tests.
  - Escape handling is covered by regression tests.
  - Any future tmux config differences should be added only when they are backed
    by a concrete failing trace or test.

## Behavioral parity audits

- Audit copy-mode behavior against tmux, especially around navigation and
  selection semantics.
- Audit mouse handling and CSI/escape-key edge cases against tmux-compatible
  expectations.
- Recheck color parsing and terminal control sequence fidelity for uncommon
  OSC/DCS cases.
- Verify line-drawing / ACS remapping behavior on terminals that exercise
  alternate character sets.
- Compare buffer, paste, and status-bar behavior with tmux on real session
  traces.

## Tests and docs

- Keep the compatibility matrix explicit: implemented, intentionally
  unsupported, and not yet covered.
- Add regression tests for every newly confirmed parity item.
- Keep `README.md` aligned with the actual tokenizer and feature surface.
- Keep `TMUX-COVERAGE-ROADMAP.md` as the long-form audit trail, but trim stale
  sections once the TODO above becomes the source of truth.

## Notes

- The old “12-15% complete” framing is stale; the remaining work is mostly
  exact semantics, tokenizer fidelity, and command-flag parity.
- Some current rejections are deliberate product choices, not bugs. When
  implementing each item, confirm whether the goal is strict tmux parity or a
  narrower compatibility surface.
