## What this changes

<!-- One-paragraph summary. -->

## tmux reference

<!-- For behavior changes: what does real tmux do, and how did you confirm it
     (man page, upstream source, live transcript)? Write "n/a" for pure
     refactors/docs. -->

## Checklist

- [ ] `nix flake check` passes locally
- [ ] New/changed behavior is pinned by a regression test
- [ ] New source files are `git add`ed (the flake only sees tracked files)
- [ ] Tests that mutate global state use the isolation helpers
