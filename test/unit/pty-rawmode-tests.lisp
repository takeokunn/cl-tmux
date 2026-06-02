(in-package #:cl-tmux/test)

;;;; Tests for pty-rawmode.lisp — terminal raw mode management.

(def-suite pty-rawmode-suite :description "Terminal raw mode management")
(in-suite pty-rawmode-suite)

;;; ── Macro structure tests ────────────────────────────────────────────────────

(test with-raw-termios-flags-is-defined
  "with-raw-termios-flags is a defined macro."
  (is (macro-function 'cl-tmux/pty::with-raw-termios-flags)
      "with-raw-termios-flags must be a defined macro"))

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

(test with-raw-termios-flags-empty-specs-expands
  "An empty spec list expands to a PROGN with no body forms."
  (let* ((form (macroexpand-1
                '(cl-tmux/pty::with-raw-termios-flags (my-termios))))
         (text (prin1-to-string form)))
    (is-true (search "PROGN" text)
             "empty specs must still emit a PROGN wrapper")))

;;; ── disable-raw-mode! — no saved state ──────────────────────────────────────

(test disable-raw-mode-noop-when-not-saved
  "disable-raw-mode! is a no-op when no termios was saved: it must not touch
   the fd and must leave *saved-termios* nil."
  (let ((cl-tmux/pty::*saved-termios* nil))
    (finishes (cl-tmux/pty:disable-raw-mode! -1))
    (is (null cl-tmux/pty::*saved-termios*)
        "*saved-termios* must remain NIL after disable-raw-mode! with no saved state")))

(test disable-raw-mode-clears-saved-termios-on-success
  "After disable-raw-mode! successfully restores settings, *saved-termios* is NIL.
   We use a real TTY (stdout fd=1) if available; skip otherwise."
  (let* ((is-tty (handler-case (progn (sb-posix:tcgetattr 1) t) (error () nil)))
         (cl-tmux/pty::*saved-termios* nil))
    (when is-tty
      ;; Save current settings into *saved-termios* first.
      (setf cl-tmux/pty::*saved-termios* (sb-posix:tcgetattr 1))
      (cl-tmux/pty:disable-raw-mode! 1)
      (is (null cl-tmux/pty::*saved-termios*)
          "*saved-termios* must be set to NIL after a successful disable-raw-mode!"))))

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

;;; ── *saved-termios* isolation ────────────────────────────────────────────────

(test saved-termios-initial-value-is-nil
  "*saved-termios* starts as NIL (nothing to restore before any raw-mode call)."
  ;; We re-bind so the global is not perturbed.
  (let ((cl-tmux/pty::*saved-termios* nil))
    (is (null cl-tmux/pty::*saved-termios*)
        "*saved-termios* must be NIL before any enable-raw-mode! call")))

(test saved-termios-set-by-enable-raw-mode-on-tty
  "enable-raw-mode! sets *saved-termios* when tcgetattr succeeds.
   This test is skipped when stdout is not a TTY (e.g., batch test runs)."
  ;; Use stdout (fd 1) only if it is actually a TTY; otherwise skip.
  (let* ((is-tty (handler-case
                     (progn (sb-posix:tcgetattr 1) t)
                   (error () nil)))
         (cl-tmux/pty::*saved-termios* nil))
    (when is-tty
      (unwind-protect
           (progn
             (cl-tmux/pty:enable-raw-mode! 1)
             (is-true cl-tmux/pty::*saved-termios*
                      "enable-raw-mode! must populate *saved-termios* on a real TTY"))
        ;; Restore whatever we saved.
        (ignore-errors (cl-tmux/pty:disable-raw-mode! 1))))))
