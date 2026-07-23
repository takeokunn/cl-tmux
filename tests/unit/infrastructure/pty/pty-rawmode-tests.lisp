(in-package #:cl-tmux/test)

;;;; Tests for pty-rawmode.lisp — terminal raw mode management.
;;;;
;;;; cl-tmux's enable-raw-mode! / disable-raw-mode! are now thin wrappers that
;;;; delegate to cl-tty-kit:enable-raw-mode / disable-raw-mode.  cl-tty-kit owns
;;;; the saved-termios state (per-fd, depth-counted, thread-safe) and clears a
;;;; superset of cl-tmux's former raw-mode flags.  These tests assert the
;;;; delegation contract rather than cl-tmux-internal termios machinery (the
;;;; old *saved-termios-table* / with-raw-termios-flags internals were removed).

(describe "pty-rawmode-suite"

  ;;; ── Exported wrappers are fbound ─────────────────────────────────────────────

  ;; enable-raw-mode! is an exported function in cl-tmux/pty.
  (it "enable-raw-mode-is-fbound"
    (expect (fboundp 'cl-tmux/pty:enable-raw-mode!)))

  ;; disable-raw-mode! is an exported function in cl-tmux/pty.
  (it "disable-raw-mode-is-fbound"
    (expect (fboundp 'cl-tmux/pty:disable-raw-mode!)))

  ;; The cl-tmux/pty raw-mode internals were deleted in favour of delegation:
  ;; the old termios edit macro and saved-state table must no longer exist.
  (it "old-raw-mode-internals-removed"
    (expect (null (macro-function
                   (or (find-symbol "WITH-RAW-TERMIOS-FLAGS" '#:cl-tmux/pty)
                       (gensym)))))
    (expect (not (boundp (or (find-symbol "*SAVED-TERMIOS-TABLE*" '#:cl-tmux/pty)
                             (gensym))))))

  ;;; ── Delegation to cl-tty-kit ────────────────────────────────────────────────

  ;; enable-raw-mode! forwards to cl-tty-kit:enable-raw-mode, which calls
  ;; tcgetattr on the fd; on a non-TTY fd (a pipe read-end) that fails, so the
  ;; wrapper signals an error — confirming the delegation path is exercised.
  (it "enable-raw-mode-signals-on-non-tty"
    (with-pipe-fds (rfd wfd)
      (declare (ignore wfd))
      (signals error (cl-tmux/pty:enable-raw-mode! rfd))))

  ;; enable-raw-mode! inherits cl-tty-kit's fd validation: a negative fd is
  ;; rejected before any tcgetattr, signalling an error.
  (it "enable-raw-mode-rejects-negative-fd"
    (signals error (cl-tmux/pty:enable-raw-mode! -1)))

  ;; disable-raw-mode! on an fd that was never enabled is a no-op: cl-tty-kit
  ;; finds no saved state and returns without touching the terminal.  fd 99 is a
  ;; non-negative fd with no raw-mode state, so this finishes without signalling.
  (it "disable-raw-mode-noop-when-not-enabled"
    (finishes (cl-tmux/pty:disable-raw-mode! 99)))

  ;; enable-raw-mode! then disable-raw-mode! round-trips on a real TTY and
  ;; restores the terminal — cl-tty-kit remembers and pops the saved state.
  ;; Skipped when stdout is not a TTY (e.g., sandboxed Nix builds).
  (it "enable-then-disable-round-trips-on-tty"
    (let ((is-tty (handler-case (progn (sb-posix:tcgetattr 1) t) (error () nil))))
      (if (not is-tty)
          (skip "stdout is not a TTY (sandboxed environment)")
          (unwind-protect
               (finishes
                 (progn
                   (cl-tmux/pty:enable-raw-mode! 1)
                   (cl-tmux/pty:disable-raw-mode! 1)))
            (ignore-errors (cl-tmux/pty:disable-raw-mode! 1)))))))
