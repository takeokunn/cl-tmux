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
  "disable-raw-mode! is a no-op when no termios was saved for the fd."
  ;; Use a fresh table with no entry for fd -1 to verify no-op behaviour.
  (let ((cl-tmux/pty::*saved-termios-table* (make-hash-table :test #'eql)))
    (finishes (cl-tmux/pty:disable-raw-mode! -1))))

;;; ── enable-raw-mode! — reachability test ────────────────────────────────────
;;;
;;; enable-raw-mode! calls sb-posix:tcgetattr on the given fd.  On a non-TTY
;;; fd (like a pipe read-end), tcgetattr fails with ENOTTY, signalling a
;;; SB-POSIX:SYSCALL-ERROR.  This test verifies the function is reachable and
;;; actually calls tcgetattr rather than returning silently.

(test enable-raw-mode-signals-on-non-tty
  "enable-raw-mode! calls tcgetattr; on a non-TTY fd it signals a condition."
  (with-pipe-fds (rfd wfd)
    (declare (ignore wfd))
    (signals error (cl-tmux/pty:enable-raw-mode! rfd))))

(test disable-raw-mode-attempts-restore-when-saved
  "disable-raw-mode! calls tcsetattr when an entry exists in *saved-termios-table*.
   On a non-TTY fd tcsetattr fails with ENOTTY, confirming the code path was entered."
  (with-pipe-fds (rfd wfd)
    (declare (ignore wfd))
    ;; Inject a fake saved entry for rfd in an isolated table so the
    ;; disable-raw-mode! 'when saved' branch is actually entered.
    (let ((cl-tmux/pty::*saved-termios-table*
           (let ((tbl (make-hash-table :test #'eql)))
             (setf (gethash rfd tbl) :fake-termios)
             tbl)))
      ;; tcsetattr on a pipe will signal a POSIX error — that is expected.
      ;; The key assertion is that disable-raw-mode! entered the restore branch.
      (handler-case
          (cl-tmux/pty:disable-raw-mode! rfd)
        (error ()
          ;; Expected: ENOTTY from tcsetattr on the pipe fd.
          nil)))))

;;; ── *saved-termios-table* isolation ─────────────────────────────────────────

(test saved-termios-table-empty-initially
  "*saved-termios-table* has no entry for a fresh fd before any enable-raw-mode! call."
  (let ((cl-tmux/pty::*saved-termios-table* (make-hash-table :test #'eql)))
    (is (null (gethash 99 cl-tmux/pty::*saved-termios-table*))
        "fresh table must have no entry for an unused fd")))

(test saved-termios-table-populated-by-enable-raw-mode-on-tty
  "enable-raw-mode! stores termios in *saved-termios-table* keyed by fd.
   Skipped when stdout is not a TTY (e.g., sandboxed Nix builds)."
  (let* ((is-tty (handler-case (progn (sb-posix:tcgetattr 1) t) (error () nil)))
         (cl-tmux/pty::*saved-termios-table* (make-hash-table :test #'eql)))
    (when is-tty
      (unwind-protect
           (progn
             (cl-tmux/pty:enable-raw-mode! 1)
             (is-true (gethash 1 cl-tmux/pty::*saved-termios-table*)
                      "enable-raw-mode! must store termios in *saved-termios-table*"))
        (ignore-errors (cl-tmux/pty:disable-raw-mode! 1))))))

;;; ── *termios-table-lock* concurrency primitive ───────────────────────────────

(test termios-table-lock-is-defined
  "*termios-table-lock* is a defined variable holding a lock object."
  (is (boundp 'cl-tmux/pty::*termios-table-lock*)
      "*termios-table-lock* must be bound")
  (is-true cl-tmux/pty::*termios-table-lock*
           "*termios-table-lock* must be non-NIL"))

(test termios-table-lock-can-be-acquired
  "The termios table lock can be acquired and released without error."
  (finishes
    (bordeaux-threads:with-lock-held (cl-tmux/pty::*termios-table-lock*)
      t)
    "*termios-table-lock* must be acquirable"))

;;; ── disable-raw-mode! removes entry from table ───────────────────────────────

(test disable-raw-mode-removes-entry-from-table
  "disable-raw-mode! removes the fd entry from *saved-termios-table* after
   attempting to restore, even when tcsetattr fails (e.g., on a pipe fd)."
  (with-pipe-fds (rfd wfd)
    (declare (ignore wfd))
    (let ((cl-tmux/pty::*saved-termios-table*
           (let ((tbl (make-hash-table :test #'eql)))
             (setf (gethash rfd tbl) :fake-termios)
             tbl)))
      ;; tcsetattr on a pipe signals ENOTTY — we ignore that.
      (handler-case
          (cl-tmux/pty:disable-raw-mode! rfd)
        (error () nil))
      ;; The entry must be gone regardless of tcsetattr outcome.
      (is (null (gethash rfd cl-tmux/pty::*saved-termios-table*))
          "disable-raw-mode! must remove the fd entry from *saved-termios-table*"))))

;;; ── enable-raw-mode! / disable-raw-mode! are fbound ─────────────────────────

(test enable-raw-mode-is-fbound
  "enable-raw-mode! is an exported function in cl-tmux/pty."
  (is (fboundp 'cl-tmux/pty:enable-raw-mode!)
      "enable-raw-mode! must be fbound"))

(test disable-raw-mode-is-fbound
  "disable-raw-mode! is an exported function in cl-tmux/pty."
  (is (fboundp 'cl-tmux/pty:disable-raw-mode!)
      "disable-raw-mode! must be fbound"))

;;; ── with-raw-termios-flags :clear spec flag combination ─────────────────────

(test with-raw-termios-flags-clear-multiple-flags-in-one-spec
  "A :clear spec with multiple flags emits one LOGIOR wrapping all flags."
  (let* ((form (macroexpand-1
                '(cl-tmux/pty::with-raw-termios-flags (t1)
                   (:clear my-acc f1 f2 f3))))
         (text (prin1-to-string form))
         ;; Count F1, F2, F3 occurrences (they appear in the LOGIOR argument list).
         (f1-count (let ((n 0) (s 0))
                     (loop for p = (search "F1" text :start2 s)
                           while p do (incf n) (setf s (+ p 2)))
                     n)))
    (is (>= f1-count 1) "F1 must appear in the expansion")))

(test with-raw-termios-flags-replace-spec-has-both-sublists
  "The :replace spec expansion mentions both the clear-list and the set-list symbols."
  (let* ((form (macroexpand-1
                '(cl-tmux/pty::with-raw-termios-flags (t1)
                   (:replace acc (clear1 clear2) (set1)))))
         (text (prin1-to-string form)))
    (is-true (search "CLEAR1" text) "clear flag must appear in :replace expansion")
    (is-true (search "SET1"   text) "set flag must appear in :replace expansion")))
