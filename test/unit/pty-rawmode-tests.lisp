(in-package #:cl-tmux/test)

;;;; Tests for pty-rawmode.lisp — terminal raw mode management.

(def-suite pty-rawmode-suite :description "Terminal raw mode management")
(in-suite pty-rawmode-suite)

(test with-raw-termios-flags-is-defined
  "with-raw-termios-flags is a defined macro."
  (is (macro-function 'cl-tmux/pty::with-raw-termios-flags)
      "with-raw-termios-flags must be a defined macro"))

(test disable-raw-mode-noop-when-not-saved
  "disable-raw-mode! is a no-op when no termios was saved: it must not touch
   the fd and must leave *saved-termios* nil."
  (let ((cl-tmux/pty::*saved-termios* nil))
    (finishes (cl-tmux/pty:disable-raw-mode! -1))
    (is (null cl-tmux/pty::*saved-termios*))))
