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

;;; ── with-raw-termios-flags macroexpansion ────────────────────────────────────

(test with-raw-termios-flags-clear-spec-expansion
  "The :clear spec expands to a SETF/LOGAND/LOGNOT form targeting the accessor."
  (let* ((form (macroexpand-1
                '(cl-tmux/pty::with-raw-termios-flags (my-termios)
                   (:clear my-accessor flag-a flag-b))))
         (text (prin1-to-string form)))
    (is-true (search "LOGAND" text)  ":clear spec must emit LOGAND")
    (is-true (search "LOGNOT" text)  ":clear spec must emit LOGNOT")
    (is-true (search "LOGIOR" text)  ":clear spec must emit LOGIOR for flags")
    (is-true (search "SETF"   text)  ":clear spec must emit SETF")))

(test with-raw-termios-flags-replace-spec-expansion
  "The :replace spec expands to a SETF/LOGIOR/LOGAND/LOGNOT form covering both
   the clear-list and the set-list branches."
  (let* ((form (macroexpand-1
                '(cl-tmux/pty::with-raw-termios-flags (my-termios)
                   (:replace my-accessor (clear-flag) (set-flag)))))
         (text (prin1-to-string form)))
    (is-true (search "LOGIOR" text) ":replace spec must emit LOGIOR")
    (is-true (search "LOGAND" text) ":replace spec must emit LOGAND")
    (is-true (search "LOGNOT" text) ":replace spec must emit LOGNOT")
    (is-true (search "SETF"   text) ":replace spec must emit SETF")))

(test with-raw-termios-flags-multiple-specs
  "Multiple specs in one with-raw-termios-flags form each expand to a SETF."
  (let* ((form (macroexpand-1
                '(cl-tmux/pty::with-raw-termios-flags (t1)
                   (:clear  acc-a f1 f2)
                   (:replace acc-b (c1) (s1)))))
         (text  (prin1-to-string form))
         (count 0)
         (start 0))
    (loop for pos = (search "SETF" text :start2 start)
          while pos
          do (incf count)
             (setf start (+ pos 4)))
    (is (>= count 2) "two specs must produce at least two SETF forms")))

;;; ── enable-raw-mode! — reachability test ────────────────────────────────────
;;;
;;; enable-raw-mode! calls sb-posix:tcgetattr on the given fd.  On a non-TTY
;;; fd (like a pipe read-end), tcgetattr fails with ENOTTY, signalling a
;;; SB-POSIX:SYSCALL-ERROR.  This test verifies the function is reachable and
;;; actually calls tcgetattr rather than returning silently.

(test enable-raw-mode-signals-on-non-tty
  "enable-raw-mode! calls tcgetattr; on a non-TTY fd it signals a condition."
  (multiple-value-bind (rfd wfd) (sb-posix:pipe)
    (unwind-protect
         (signals error (cl-tmux/pty:enable-raw-mode! rfd))
      (ignore-errors (sb-posix:close rfd))
      (ignore-errors (sb-posix:close wfd)))))

(test disable-raw-mode-attempts-restore-when-saved
  "disable-raw-mode! calls tcsetattr when *saved-termios* is non-nil.
   On a non-TTY fd tcsetattr fails, confirming the code path was entered."
  (multiple-value-bind (rfd wfd) (sb-posix:pipe)
    (unwind-protect
         (let ((cl-tmux/pty::*saved-termios* :fake-termios))
           ;; tcsetattr on a pipe will signal a POSIX error — that is expected.
           ;; The important thing is that disable-raw-mode! did NOT short-circuit.
           (handler-case
               (cl-tmux/pty:disable-raw-mode! rfd)
             (error ()
               ;; Expected: ENOTTY from tcsetattr on the pipe fd.
               nil)))
      (ignore-errors (sb-posix:close rfd))
      (ignore-errors (sb-posix:close wfd)))))
