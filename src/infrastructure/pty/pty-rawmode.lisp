(in-package #:cl-tmux/pty)

;;; ── Public: terminal raw mode ──────────────────────────────────────────────
;;;
;;; Raw-mode management is delegated to cl-tty-kit:enable-raw-mode /
;;; disable-raw-mode.  cl-tty-kit clears a strict superset of the iflag/oflag/
;;; cflag/lflag bits cl-tmux formerly cleared here (it adds IGNBRK PARMRK INLCR
;;; IGNCR IXOFF on top of cl-tmux's BRKINT ISTRIP ICRNL IXON etc., and matches
;;; the OPOST clear, CSIZE|PARENB->CS8, ECHO/ICANON/ISIG/IEXTEN off, VMIN=1
;;; VTIME=0), so cl-tmux's prior outer-terminal raw behavior (ISIG off, no echo,
;;; byte-transparent input) is preserved.  cl-tty-kit also owns the saved-state
;;; table (per-fd, depth-counted, thread-safe), so cl-tmux no longer keeps its
;;; own *saved-termios-table* / termios edit macro.
;;;
;;; These thin wrappers keep cl-tmux/pty's exported names enable-raw-mode! /
;;; disable-raw-mode! stable so the call sites (input.lisp, fd 0 = stdin) do not
;;; churn.

(defun enable-raw-mode! (fd)
  "Switch FD to raw (unbuffered, no-echo) mode via cl-tty-kit:enable-raw-mode,
   which remembers the previous settings keyed by FD."
  (cl-tty-kit:enable-raw-mode fd))

(defun disable-raw-mode! (fd)
  "Restore the terminal settings saved by enable-raw-mode! for FD via
   cl-tty-kit:disable-raw-mode."
  (cl-tty-kit:disable-raw-mode fd))
