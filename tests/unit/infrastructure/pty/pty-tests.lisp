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
