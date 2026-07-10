# Security Policy

## Supported versions

Only the latest release (and the `main` branch) receive security fixes.

## Threat model notes

cl-tmux is a terminal multiplexer: it executes shells on behalf of the local
user and exposes a Unix-domain socket for client/server attach. Points worth
knowing when assessing an issue:

- The server socket is created in a per-user directory created with mode
  `0700` (under `$TMUX_TMPDIR`, falling back to the system temp dir), mirroring
  tmux's socket model. Anyone who can write to that socket can run commands as
  the owning user — the directory permissions are the security boundary.
- Escape-sequence input from programs running inside panes is untrusted. The
  VT100/ANSI parser is exercised heavily by the test suite, but parser bugs
  that lead to memory unsafety are not expected (SBCL is memory-safe);
  state-confusion or spoofing bugs (e.g. via OSC/DCS) are still valid reports.
- Config files are executed as commands. Loading an untrusted
  `.tmux.conf`-style file is equivalent to running untrusted commands.

## Reporting a vulnerability

Please report suspected vulnerabilities privately via
[GitHub Security Advisories](https://github.com/takeokunn/cl-tmux/security/advisories/new)
rather than opening a public issue. Include reproduction steps and the
platform (OS, SBCL version, terminal). You should receive an acknowledgement
within a week.
