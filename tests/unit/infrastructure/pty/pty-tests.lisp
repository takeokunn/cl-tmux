(in-package #:cl-tmux/test)

;;;; Unit tests for pty.lisp: argument-assembly helpers.
;;;; These cover %spawn-directory, %spawn-environment-assignments,
;;;; and %string-non-empty-p without spawning a real PTY process.

(def-suite pty-unit-suite :description "PTY argument-assembly helpers (unit)")
(in-suite pty-unit-suite)

;;; ── %string-non-empty-p ──────────────────────────────────────────────────────

(test string-non-empty-p-true-for-non-empty-string
  "%string-non-empty-p returns T for a non-empty string."
  (is-true (cl-tmux/pty::%string-non-empty-p "hello")
           "non-empty string must return T"))

(test string-non-empty-p-false-for-empty-string
  "%string-non-empty-p returns NIL for an empty string."
  (is-false (cl-tmux/pty::%string-non-empty-p "")
            "empty string must return NIL"))

(test string-non-empty-p-false-for-nil
  "%string-non-empty-p returns NIL for NIL."
  (is-false (cl-tmux/pty::%string-non-empty-p nil)
            "NIL must return NIL"))

(test string-non-empty-p-false-for-non-string
  "%string-non-empty-p returns NIL for non-string values."
  (is-false (cl-tmux/pty::%string-non-empty-p 42)
            "integer must return NIL")
  (is-false (cl-tmux/pty::%string-non-empty-p '(a b))
            "list must return NIL"))

;;; ── %spawn-environment-assignments ──────────────────────────────────────────

(test spawn-environment-assignments-with-term-only
  "%spawn-environment-assignments with only a TERM string produces one NAME=VALUE entry."
  (let ((result (cl-tmux/pty::%spawn-environment-assignments "xterm-256color" nil)))
    (is (= 1 (length result))
        "one assignment expected when only TERM is provided")
    (is (string= "TERM=xterm-256color" (first result))
        "TERM assignment must be TERM=<value>")))

(test spawn-environment-assignments-with-extra-env
  "%spawn-environment-assignments includes EXTRA-ENV pairs after TERM."
  (let ((result (cl-tmux/pty::%spawn-environment-assignments
                 "xterm-256color"
                 (list (cons "MY_VAR" "hello") (cons "OTHER" "42")))))
    (is (= 3 (length result))
        "three assignments expected: TERM + two extra")
    (is (string= "TERM=xterm-256color" (first result))
        "TERM must come first")
    (is (member "MY_VAR=hello" result :test #'string=)
        "MY_VAR=hello must appear")
    (is (member "OTHER=42" result :test #'string=)
        "OTHER=42 must appear")))

(test spawn-environment-assignments-empty-term-skipped
  "%spawn-environment-assignments omits the TERM entry when TERM is empty."
  (let ((result (cl-tmux/pty::%spawn-environment-assignments
                 "" (list (cons "FOO" "bar")))))
    (is (= 1 (length result))
        "only the extra env entry must be present when TERM is empty")
    (is (string= "FOO=bar" (first result))
        "FOO=bar must be present")))

(test spawn-environment-assignments-nil-term-skipped
  "%spawn-environment-assignments omits the TERM entry when TERM is NIL."
  (let ((result (cl-tmux/pty::%spawn-environment-assignments
                 nil (list (cons "X" "y")))))
    (is (= 1 (length result)) "only the extra entry when TERM is NIL")
    (is (string= "X=y" (first result)) "X=y must be the only entry")))

(test spawn-environment-assignments-skips-non-string-pair
  "%spawn-environment-assignments silently skips pairs that are not (string . string)."
  (let ((result (cl-tmux/pty::%spawn-environment-assignments
                 nil (list (cons 42 "val")))))
    (is (null result)
        "non-string car must be skipped silently")))

(test spawn-environment-assignments-empty-no-term-no-extra
  "%spawn-environment-assignments returns NIL with no TERM and no extra env."
  (let ((result (cl-tmux/pty::%spawn-environment-assignments nil nil)))
    (is (null result) "no assignments produced for nil TERM and nil extra env")))

;;; ── %spawn-directory ─────────────────────────────────────────────────────────

(test spawn-directory-nil-input-returns-nil
  "%spawn-directory returns NIL when START-DIR is NIL."
  (is (null (cl-tmux/pty::%spawn-directory nil))
      "%spawn-directory must return NIL for NIL input"))

(test spawn-directory-empty-string-returns-nil
  "%spawn-directory returns NIL when START-DIR is an empty string."
  (is (null (cl-tmux/pty::%spawn-directory ""))
      "%spawn-directory must return NIL for empty string"))

(test spawn-directory-existing-path-returns-truename
  "%spawn-directory returns a non-NIL truename for an existing directory."
  (let ((result (cl-tmux/pty::%spawn-directory "/tmp")))
    (is-true result "%spawn-directory must return a truthy value for /tmp")))

(test spawn-directory-nonexistent-path-returns-nil
  "%spawn-directory returns NIL for a non-existent directory path.
   The simplified implementation uses ignore-errors so failures silently yield NIL."
  (let ((result (cl-tmux/pty::%spawn-directory
                 "/nonexistent/path/that/does/not/exist/xyz")))
    (is (null result)
        "nonexistent directory must yield NIL (ignored error)")))

;;; ── forkpty-with-shell reachability ─────────────────────────────────────────

(test forkpty-with-shell-is-fbound
  "forkpty-with-shell is exported from cl-tmux/pty and callable."
  (is (fboundp 'cl-tmux/pty:forkpty-with-shell)
      "forkpty-with-shell must be fbound"))

(test set-pty-size-is-fbound
  "set-pty-size is exported from cl-tmux/pty and callable."
  (is (fboundp 'cl-tmux/pty:set-pty-size)
      "set-pty-size must be fbound"))

;;; ── forkpty-with-shell end-to-end (real PTY) ─────────────────────────────────

(test forkpty-with-shell-returns-sane-fd-and-pid
  "forkpty-with-shell spawns a real child shell and returns a non-negative
   master fd and a positive pid."
  (unless (pty-available-p) (skip "no PTY available (sandboxed environment)"))
  (with-pty-shell (fd pid)
    (is (>= fd 0) "master fd must be non-negative")
    (is (plusp pid) "child pid must be positive")))

(test forkpty-with-shell-slave-path-is-a-string
  "forkpty-with-shell returns an empty string for slave-path (SBCL exposes no
   portable slave path), not NIL."
  (unless (pty-available-p) (skip "no PTY available (sandboxed environment)"))
  (multiple-value-bind (fd pid slave-path) (forkpty-with-shell 24 80)
    (unwind-protect
         (is (string= "" slave-path) "slave-path must be the empty string")
      (pty-close fd pid))))

(test set-pty-size-does-not-error-on-real-pty
  "set-pty-size succeeds (no error) when called on a real spawned PTY master fd."
  (unless (pty-available-p) (skip "no PTY available (sandboxed environment)"))
  (with-pty-shell (fd pid)
    (finishes (cl-tmux/pty:set-pty-size fd 30 100)
              "set-pty-size must not signal on a live PTY master fd")))

;;; ── pty-write / pty-read-blocking (real pipe) ────────────────────────────────

(test pty-write-string-round-trips-through-pipe
  "pty-write encodes a UTF-8 string and writes it to the fd; the bytes are
   readable back via pty-read-blocking."
  (with-pipe-fds (rfd wfd)
    (pty-write wfd "hi")
    (let ((result (pty-read-blocking rfd 16)))
      (is (equalp #(104 105) result)
          "pty-read-blocking must return the written bytes ('hi' = 104 105)"))))

(test pty-write-octet-vector-round-trips-through-pipe
  "pty-write accepts a raw octet vector and writes it verbatim."
  (with-pipe-fds (rfd wfd)
    (pty-write wfd (make-array 3 :element-type '(unsigned-byte 8)
                               :initial-contents '(1 2 3)))
    (let ((result (pty-read-blocking rfd 16)))
      (is (equalp #(1 2 3) result)
          "pty-read-blocking must return the written octets unchanged"))))

(test pty-write-empty-octet-vector-is-noop
  "pty-write with a zero-length octet vector performs no write (no bytes land
   on the read end)."
  (with-pipe-fds (rfd wfd)
    (pty-write wfd (make-array 0 :element-type '(unsigned-byte 8)))
    (is (null (cl-tmux/pty:select-fds (list rfd) 10000))
        "an empty write must leave the read end with no data")))

(test pty-read-blocking-returns-nil-on-eof
  "pty-read-blocking returns NIL when the write end of the pipe is closed
   before any data is written (EOF)."
  (with-pipe-fds (rfd wfd)
    (sb-posix:close wfd)
    (is (null (pty-read-blocking rfd 16))
        "pty-read-blocking must return NIL on EOF")))

;;; ── terminal-size ─────────────────────────────────────────────────────────────

(test terminal-size-returns-two-positive-values
  "terminal-size returns (values rows cols), both positive integers — either
   the real ioctl-reported size or the documented fallback."
  (multiple-value-bind (rows cols) (cl-tmux/pty:terminal-size)
    (is (integerp rows) "rows must be an integer")
    (is (integerp cols) "cols must be an integer")
    (is (plusp rows) "rows must be positive")
    (is (plusp cols) "cols must be positive")))

;;; ── %target-program-and-args ─────────────────────────────────────────────────

(test target-program-and-args-with-default-command-uses-sh-c
  "%target-program-and-args routes a non-empty DEFAULT-COMMAND through /bin/sh -c
   and does not request a PATH search."
  (multiple-value-bind (program args search-p)
      (cl-tmux/pty::%target-program-and-args "echo hi")
    (is (string= "/bin/sh" program) "program must be /bin/sh")
    (is (equal '("-c" "echo hi") args) "args must be (-c DEFAULT-COMMAND)")
    (is-false search-p "sh -c invocation must not request a PATH search")))

(test target-program-and-args-nil-command-uses-default-shell
  "%target-program-and-args with a NIL/empty DEFAULT-COMMAND returns the
   configured default shell directly, with no extra args."
  (let ((cl-tmux/config:*default-shell* "/bin/zsh"))
    (multiple-value-bind (program args search-p)
        (cl-tmux/pty::%target-program-and-args nil)
      (is (string= "/bin/zsh" program) "program must be the configured default shell")
      (is (null args) "no extra args when running the shell directly")
      (is-false search-p "an absolute shell path must not request a PATH search"))))

(test target-program-and-args-relative-shell-requests-path-search
  "%target-program-and-args requests a PATH search (SEARCH-P = T) when the
   configured default shell is not an absolute path."
  (let ((cl-tmux/config:*default-shell* "zsh"))
    (multiple-value-bind (program args search-p)
        (cl-tmux/pty::%target-program-and-args "")
      (declare (ignore args))
      (is (string= "zsh" program) "program must be the relative shell name")
      (is-true search-p "a relative shell name must request a PATH search"))))

;;; ── install-pty-port ─────────────────────────────────────────────────────────

(test install-pty-port-wires-all-four-ports
  "install-pty-port sets *spawn-pty*, *write-pty*, *resize-pty*, and *close-pty*
   to the corresponding cl-tmux/pty functions."
  (let ((cl-tmux/ports:*spawn-pty*  nil)
        (cl-tmux/ports:*write-pty*  nil)
        (cl-tmux/ports:*resize-pty* nil)
        (cl-tmux/ports:*close-pty*  nil))
    (cl-tmux/pty:install-pty-port)
    (is (eq #'cl-tmux/pty:forkpty-with-shell cl-tmux/ports:*spawn-pty*)
        "*spawn-pty* must be wired to forkpty-with-shell")
    (is (eq #'cl-tmux/pty:pty-write cl-tmux/ports:*write-pty*)
        "*write-pty* must be wired to pty-write")
    (is (eq #'cl-tmux/pty:set-pty-size cl-tmux/ports:*resize-pty*)
        "*resize-pty* must be wired to set-pty-size")
    (is (eq #'cl-tmux/pty:pty-close cl-tmux/ports:*close-pty*)
        "*close-pty* must be wired to pty-close")))

;;; ── terminal-size fallback constants ─────────────────────────────────────────

(test default-term-rows-cols-are-positive-fixnums
  "+default-term-rows+ and +default-term-cols+ are the shared terminal-size
   fallback constants (24x80), matching *term-rows*/*term-cols* defvar defaults."
  (is (= 24 cl-tmux/pty:+default-term-rows+)
      "+default-term-rows+ must be 24")
  (is (= 80 cl-tmux/pty:+default-term-cols+)
      "+default-term-cols+ must be 80"))
