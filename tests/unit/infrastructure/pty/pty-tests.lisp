(in-package #:cl-tmux/test)

;;;; Unit tests for pty.lisp: argument-assembly helpers.
;;;; These cover %spawn-directory, %spawn-environment-assignments,
;;;; and %string-non-empty-p without spawning a real PTY process.

(describe "pty-unit-suite"

  ;;; ── %string-non-empty-p ──────────────────────────────────────────────────────

  ;; %string-non-empty-p returns T for a non-empty string.
  (it "string-non-empty-p-true-for-non-empty-string"
    (expect (cl-tmux/pty::%string-non-empty-p "hello") :to-be-truthy))

  ;; %string-non-empty-p returns NIL for an empty string.
  (it "string-non-empty-p-false-for-empty-string"
    (expect (cl-tmux/pty::%string-non-empty-p "") :to-be-falsy))

  ;; %string-non-empty-p returns NIL for NIL.
  (it "string-non-empty-p-false-for-nil"
    (expect (cl-tmux/pty::%string-non-empty-p nil) :to-be-falsy))

  ;; %string-non-empty-p returns NIL for non-string values.
  (it "string-non-empty-p-false-for-non-string"
    (expect (cl-tmux/pty::%string-non-empty-p 42) :to-be-falsy)
    (expect (cl-tmux/pty::%string-non-empty-p '(a b)) :to-be-falsy))

  ;;; ── %spawn-environment-assignments ──────────────────────────────────────────

  ;; %spawn-environment-assignments with only a TERM string produces one NAME=VALUE entry.
  (it "spawn-environment-assignments-with-term-only"
    (let ((result (cl-tmux/pty::%spawn-environment-assignments "xterm-256color" nil)))
      (expect (= 1 (length result)))
      (expect (string= "TERM=xterm-256color" (first result)))))

  ;; %spawn-environment-assignments includes EXTRA-ENV pairs after TERM.
  (it "spawn-environment-assignments-with-extra-env"
    (let ((result (cl-tmux/pty::%spawn-environment-assignments
                   "xterm-256color"
                   (list (cons "MY_VAR" "hello") (cons "OTHER" "42")))))
      (expect (= 3 (length result)))
      (expect (string= "TERM=xterm-256color" (first result)))
      (expect (member "MY_VAR=hello" result :test #'string=))
      (expect (member "OTHER=42" result :test #'string=))))

  ;; %spawn-environment-assignments omits the TERM entry when TERM is empty.
  (it "spawn-environment-assignments-empty-term-skipped"
    (let ((result (cl-tmux/pty::%spawn-environment-assignments
                   "" (list (cons "FOO" "bar")))))
      (expect (= 1 (length result)))
      (expect (string= "FOO=bar" (first result)))))

  ;; %spawn-environment-assignments omits the TERM entry when TERM is NIL.
  (it "spawn-environment-assignments-nil-term-skipped"
    (let ((result (cl-tmux/pty::%spawn-environment-assignments
                   nil (list (cons "X" "y")))))
      (expect (= 1 (length result)))
      (expect (string= "X=y" (first result)))))

  ;; %spawn-environment-assignments silently skips pairs that are not (string . string).
  (it "spawn-environment-assignments-skips-non-string-pair"
    (let ((result (cl-tmux/pty::%spawn-environment-assignments
                   nil (list (cons 42 "val")))))
      (expect (null result))))

  ;; %spawn-environment-assignments returns NIL with no TERM and no extra env.
  (it "spawn-environment-assignments-empty-no-term-no-extra"
    (let ((result (cl-tmux/pty::%spawn-environment-assignments nil nil)))
      (expect (null result))))

  ;;; ── %spawn-directory ─────────────────────────────────────────────────────────

  ;; %spawn-directory returns NIL when START-DIR is NIL.
  (it "spawn-directory-nil-input-returns-nil"
    (expect (null (cl-tmux/pty::%spawn-directory nil))))

  ;; %spawn-directory returns NIL when START-DIR is an empty string.
  (it "spawn-directory-empty-string-returns-nil"
    (expect (null (cl-tmux/pty::%spawn-directory ""))))

  ;; %spawn-directory returns a non-NIL truename for an existing directory.
  (it "spawn-directory-existing-path-returns-truename"
    (let ((result (cl-tmux/pty::%spawn-directory "/tmp")))
      (expect result :to-be-truthy)))

  ;; %spawn-directory returns NIL for a non-existent directory path.
  ;; The simplified implementation uses ignore-errors so failures silently yield NIL.
  (it "spawn-directory-nonexistent-path-returns-nil"
    (let ((result (cl-tmux/pty::%spawn-directory
                   "/nonexistent/path/that/does/not/exist/xyz")))
      (expect (null result))))

  ;;; ── forkpty-with-shell reachability ─────────────────────────────────────────

  ;; forkpty-with-shell is exported from cl-tmux/pty and callable.
  (it "forkpty-with-shell-is-fbound"
    (expect (fboundp 'cl-tmux/pty:forkpty-with-shell)))

  ;; set-pty-size is exported from cl-tmux/pty and callable.
  (it "set-pty-size-is-fbound"
    (expect (fboundp 'cl-tmux/pty:set-pty-size)))

  ;;; ── forkpty-with-shell end-to-end (real PTY) ─────────────────────────────────

  ;; forkpty-with-shell spawns a real child shell and returns a non-negative
  ;; master fd and a positive pid.
  (it "forkpty-with-shell-returns-sane-fd-and-pid"
    (unless (pty-available-p) (skip "no PTY available (sandboxed environment)"))
    (with-pty-shell (fd pid)
      (expect (>= fd 0))
      (expect (plusp pid))))

  ;; forkpty-with-shell returns an empty string for slave-path (SBCL exposes no
  ;; portable slave path), not NIL.
  (it "forkpty-with-shell-slave-path-is-a-string"
    (unless (pty-available-p) (skip "no PTY available (sandboxed environment)"))
    (multiple-value-bind (fd pid slave-path) (forkpty-with-shell 24 80)
      (unwind-protect
           (expect (string= "" slave-path))
        (pty-close fd pid))))

  ;; set-pty-size succeeds (no error) when called on a real spawned PTY master fd.
  (it "set-pty-size-does-not-error-on-real-pty"
    (unless (pty-available-p) (skip "no PTY available (sandboxed environment)"))
    (with-pty-shell (fd pid)
      (finishes (cl-tmux/pty:set-pty-size fd 30 100)
                "set-pty-size must not signal on a live PTY master fd")))

  ;;; ── pty-child-exit-status ────────────────────────────────────────────────────

  ;; pty-child-exit-status returns NIL for an fd with no registered process
  ;; (a foreign fd or a synthetic test pane never went through forkpty-with-shell).
  (it "pty-child-exit-status-unknown-fd-returns-nil"
    (expect (null (cl-tmux/pty:pty-child-exit-status 999999))))

  ;; pty-child-exit-status bounds its wait: a still-running child (never told
  ;; to exit) with a tiny override timeout returns NIL rather than blocking
  ;; forever on sb-ext:process-wait.
  (it "pty-child-exit-status-times-out-on-a-live-child"
    (unless (pty-available-p) (skip "no PTY available (sandboxed environment)"))
    (with-pty-shell (fd pid)
      (expect (null (cl-tmux/pty:pty-child-exit-status fd 0.05)))))

  ;; pty-child-exit-status reports :exited with the real exit code once the
  ;; child has actually terminated.
  (it "pty-child-exit-status-reports-exited-code"
    (unless (pty-available-p) (skip "no PTY available (sandboxed environment)"))
    (multiple-value-bind (fd pid)
        (cl-tmux/pty:forkpty-with-shell 24 80 :default-command "exit 7")
      (unwind-protect
           (progn
             (sleep 0.3)
             (multiple-value-bind (code kind) (cl-tmux/pty:pty-child-exit-status fd)
               (expect (= 7 code))
               (expect (eq :exited kind))))
        (cl-tmux/pty:pty-close fd pid))))

  ;;; ── pty-write / pty-read-blocking (real pipe) ────────────────────────────────

  ;; pty-write encodes a UTF-8 string and writes it to the fd; the bytes are
  ;; readable back via pty-read-blocking.
  (it "pty-write-string-round-trips-through-pipe"
    (with-pipe-fds (rfd wfd)
      (pty-write wfd "hi")
      (let ((result (pty-read-blocking rfd 16)))
        (expect (equalp #(104 105) result)))))

  ;; pty-write accepts a raw octet vector and writes it verbatim.
  (it "pty-write-octet-vector-round-trips-through-pipe"
    (with-pipe-fds (rfd wfd)
      (pty-write wfd (make-array 3 :element-type '(unsigned-byte 8)
                                 :initial-contents '(1 2 3)))
      (let ((result (pty-read-blocking rfd 16)))
        (expect (equalp #(1 2 3) result)))))

  ;; pty-write with a zero-length octet vector performs no write (no bytes land
  ;; on the read end).
  (it "pty-write-empty-octet-vector-is-noop"
    (with-pipe-fds (rfd wfd)
      (pty-write wfd (make-array 0 :element-type '(unsigned-byte 8)))
      (expect (null (cl-tmux/pty:select-fds (list rfd) 10000)))))

  ;; pty-read-blocking returns NIL when the write end of the pipe is closed
  ;; before any data is written (EOF).
  (it "pty-read-blocking-returns-nil-on-eof"
    (with-pipe-fds (rfd wfd)
      (sb-posix:close wfd)
      (expect (null (pty-read-blocking rfd 16)))))

  ;;; ── terminal-size ─────────────────────────────────────────────────────────────

  ;; terminal-size returns (values rows cols), both positive integers — either
  ;; the real ioctl-reported size or the documented fallback.
  (it "terminal-size-returns-two-positive-values"
    (multiple-value-bind (rows cols) (cl-tmux/pty:terminal-size)
      (expect (integerp rows))
      (expect (integerp cols))
      (expect (plusp rows))
      (expect (plusp cols))))

  ;;; ── %target-program-and-args ─────────────────────────────────────────────────

  ;; %target-program-and-args routes a non-empty DEFAULT-COMMAND through /bin/sh -c
  ;; and does not request a PATH search.
  (it "target-program-and-args-with-default-command-uses-sh-c"
    (multiple-value-bind (program args search-p)
        (cl-tmux/pty::%target-program-and-args "echo hi")
      (expect (string= "/bin/sh" program))
      (expect (equal '("-c" "echo hi") args))
      (expect search-p :to-be-falsy)))

  ;; %target-program-and-args with a NIL/empty DEFAULT-COMMAND returns the
  ;; configured default shell directly, with no extra args.
  (it "target-program-and-args-nil-command-uses-default-shell"
    (let ((cl-tmux/config:*default-shell* "/bin/zsh"))
      (multiple-value-bind (program args search-p)
          (cl-tmux/pty::%target-program-and-args nil)
        (expect (string= "/bin/zsh" program))
        (expect (null args))
        (expect search-p :to-be-falsy))))

  ;; %target-program-and-args requests a PATH search (SEARCH-P = T) when the
  ;; configured default shell is not an absolute path.
  (it "target-program-and-args-relative-shell-requests-path-search"
    (let ((cl-tmux/config:*default-shell* "zsh"))
      (multiple-value-bind (program args search-p)
          (cl-tmux/pty::%target-program-and-args "")
        (declare (ignore args))
        (expect (string= "zsh" program))
        (expect search-p :to-be-truthy))))

  ;;; ── install-pty-port ─────────────────────────────────────────────────────────

  ;; install-pty-port sets *spawn-pty*, *write-pty*, *resize-pty*, and *close-pty*
  ;; to the corresponding cl-tmux/pty functions.
  (it "install-pty-port-wires-all-four-ports"
    (let ((cl-tmux/ports:*spawn-pty*  nil)
          (cl-tmux/ports:*write-pty*  nil)
          (cl-tmux/ports:*resize-pty* nil)
          (cl-tmux/ports:*close-pty*  nil))
      (cl-tmux/pty:install-pty-port)
      (expect (eq #'cl-tmux/pty:forkpty-with-shell cl-tmux/ports:*spawn-pty*))
      (expect (eq #'cl-tmux/pty:pty-write cl-tmux/ports:*write-pty*))
      (expect (eq #'cl-tmux/pty:set-pty-size cl-tmux/ports:*resize-pty*))
      (expect (eq #'cl-tmux/pty:pty-close cl-tmux/ports:*close-pty*))))

  ;;; ── terminal-size fallback constants ─────────────────────────────────────────

  ;; +default-term-rows+ and +default-term-cols+ are the shared terminal-size
  ;; fallback constants (24x80), matching *term-rows*/*term-cols* defvar defaults.
  (it "default-term-rows-cols-are-positive-fixnums"
    (expect (= 24 cl-tmux/pty:+default-term-rows+))
    (expect (= 80 cl-tmux/pty:+default-term-cols+))))
