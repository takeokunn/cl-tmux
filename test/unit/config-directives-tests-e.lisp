(in-package #:cl-tmux/test)

;;;; Config directives tests — part V: macro registry, env-set-p, key-table edge cases, remaining bind/set directives.

(in-suite config-directives-suite)

(test define-config-directives-macro-is-defined
  "define-config-directives is a defined macro."
  (is (macro-function 'cl-tmux/config::define-config-directives)))

(test define-key-directive-handlers-macro-is-defined
  "define-key-directive-handlers is a defined macro."
  (is (macro-function 'cl-tmux/config::define-key-directive-handlers)))

(test env-set-p-correctly-classifies-strings
  "%env-set-p returns T for non-empty strings and NIL for nil or empty strings."
  (is-true  (cl-tmux/config::%env-set-p "/some/path")  "non-empty string is set")
  (is-true  (cl-tmux/config::%env-set-p "x")           "single-char string is set")
  (is-false (cl-tmux/config::%env-set-p nil)            "nil is not set")
  (is-false (cl-tmux/config::%env-set-p "")             "empty string is not set"))

;;; Tokenizer with quote/escape support

(test config-tokenizer-quoted-double-quotes
  "%config-tokens with a double-quoted string produces a single token preserving spaces."
  (let ((tokens (cl-tmux/config::%config-tokens "bind n \"foo bar\"")))
    (is (equal '("bind" "n" "foo bar") tokens)
        "double-quoted string must yield a single token with spaces: got ~S" tokens)))

(test config-tokenizer-single-quotes
  "%config-tokens with a single-quoted string produces a single token."
  (let ((tokens (cl-tmux/config::%config-tokens "set-shell '/usr/bin/my shell'")))
    (is (= 2 (length tokens))
        "single-quoted path must produce 2 tokens, got ~D: ~S" (length tokens) tokens)
    (is (string= "/usr/bin/my shell" (second tokens))
        "second token must be the single-quoted value, got ~S" (second tokens))))

(test config-tokenizer-backslash-escape
  "%config-tokens: backslash outside quotes escapes the next character."
  ;; The Lisp literal "foo\\ bar" is the 8-char string  foo\ bar  (with a real
  ;; backslash); that is what the tokenizer must collapse to "foo bar".
  (let ((tokens (cl-tmux/config::%config-tokens "foo\\ bar")))
    (is (= 1 (length tokens))
        "backslash-escaped space must yield a single token, got ~S" tokens)
    (is (string= "foo bar" (first tokens))
        "token must be foo bar after backslash-space, got ~S" (first tokens))))

(test config-tokenizer-empty-double-quotes
  "%config-tokens: empty double-quotes produces an empty string token."
  (let ((tokens (cl-tmux/config::%config-tokens "cmd \"\"")))
    (is (= 2 (length tokens))
        "empty double-quotes must yield 2 tokens, got ~S" tokens)
    (is (string= "" (second tokens))
        "second token must be the empty string, got ~S" (second tokens))))

(test config-tokenizer-mixed
  "%config-tokens: mix of plain tokens, quoted tokens, and backslash escapes."
  ;; "a \"b c\" d\\ e" is the literal  a "b c" d\ e  — quoted token preserves the
  ;; inner space; the backslash-space escapes to keep "d e" as one token.
  (let ((tokens (cl-tmux/config::%config-tokens "a \"b c\" d\\ e")))
    (is (= 3 (length tokens)) "must have 3 tokens, got ~S" tokens)
    (dolist (c '((0 "a"   "first token is a")
                 (1 "b c" "second token is b c")
                 (2 "d e" "third token is d e (backslash-space)")))
      (destructuring-bind (idx expected desc) c
        (is (string= expected (nth idx tokens)) "~A" desc))))

;;; bind-key with flags

(test bind-key-no-prefix-n-flag
  "bind -n binds in the root key-table (no prefix required)."
  (with-isolated-key-tables
    (apply-config-directive '("bind" "-n" "C" "new-window"))
    (let ((entry (cl-tmux/config:key-table-lookup "root" #\C)))
      (is (not (null entry))
          "bind -n must add a binding to the root table")
      (is (eq :new-window (cl-tmux/config:key-table-command entry))
          "root binding must be :new-window"))))

(test bind-key-repeatable-r-flag
  "bind -r marks the binding as repeatable."
  (with-isolated-key-tables
    (apply-config-directive '("bind" "-r" "H" "resize-left"))
    (let ((entry (cl-tmux/config:key-table-lookup "prefix" #\H)))
      (is (not (null entry))
          "bind -r must add a binding to the prefix table")
      (is (cl-tmux/config:key-table-repeatable-p entry)
          "binding must be marked repeatable with -r flag"))))

(test bind-key-custom-table-T-flag
  "bind -T table-name binds in the named key-table."
  (with-isolated-key-tables
    (apply-config-directive '("bind" "-T" "copy-mode" "q" "copy-mode-enter"))
    (let ((entry (cl-tmux/config:key-table-lookup "copy-mode" #\q)))
      (is (not (null entry))
          "bind -T copy-mode must add a binding to the copy-mode table")
      (is (eq :copy-mode-enter (cl-tmux/config:key-table-command entry))
          "copy-mode binding must be :copy-mode-enter"))))

(test bind-key-simple-also-updates-key-table
  "Simple bind (no flags) also updates the prefix key-table."
  (with-isolated-key-tables
    (apply-config-directive '("bind" "z" "new-window"))
    (let ((entry (cl-tmux/config:key-table-lookup "prefix" #\z)))
      (is (not (null entry))
          "simple bind must also add to the prefix key-table")
      (is (eq :new-window (cl-tmux/config:key-table-command entry))
          "prefix binding must be :new-window"))))

;;; bind-key directive alias
;;;
;;; NOTE: The six individual set-option alias tests (set-directive-stores-option,
;;; setw-directive-stores-option, etc.) have been removed.  The table-driven test
;;; set-option-directive-aliases-table-driven (below) supersedes them all.

(test bind-key-alias-accepted
  "bind-key is accepted as an alias for bind and creates a prefix binding."
  (with-isolated-key-tables
    (is (eq t (apply-config-directive '("bind-key" "z" "new-window")))
        "bind-key directive should return T")
    (is (eq :new-window (lookup-key-binding #\z))
        "#\\z should be bound to :new-window via bind-key")))

(test bind-key-alias-n-flag
  "bind-key -n binds in the root table (no prefix required)."
  (with-isolated-key-tables
    (apply-config-directive '("bind-key" "-n" "F" "new-window"))
    (let ((entry (cl-tmux/config:key-table-lookup "root" #\F)))
      (is (not (null entry))
          "bind-key -n must add a binding to the root table")
      (is (eq :new-window (cl-tmux/config:key-table-command entry))
          "root table binding must be :new-window"))))

;;; unbind-key directive alias

(test unbind-key-alias-removes-binding
  "unbind-key is accepted as an alias for unbind and removes a binding."
  (with-isolated-config
    (is (eq :new-window (lookup-key-binding #\c))
        "#\\c should be bound to :new-window before unbind-key")
    (is (eq t (apply-config-directive '("unbind-key" "c")))
        "a valid unbind-key directive should return T")
    (is (null (lookup-key-binding #\c))
        "#\\c should be unbound after the unbind-key directive")))

;;; unbind with -n flag

(test unbind-with-n-flag-removes-root-binding
  "unbind -n removes a binding from the root table."
  (with-isolated-key-tables
    (apply-config-directive '("bind" "-n" "X" "new-window"))
    (let ((entry (cl-tmux/config:key-table-lookup "root" #\X)))
      (is (not (null entry)) "binding must exist before unbind -n"))
    (is (eq t (apply-config-directive '("unbind" "-n" "X")))
        "unbind -n must return T")
    (is (null (cl-tmux/config:key-table-lookup "root" #\X))
        "root binding must be removed after unbind -n")))

;;; unbind with -T flag

(test unbind-with-T-flag-removes-named-table-binding
  "unbind -T copy-mode removes a binding from the named table."
  (with-isolated-key-tables
    (apply-config-directive '("bind" "-T" "copy-mode" "q" "copy-mode-enter"))
    (let ((entry (cl-tmux/config:key-table-lookup "copy-mode" #\q)))
      (is (not (null entry)) "binding must exist before unbind -T"))
    (is (eq t (apply-config-directive '("unbind" "-T" "copy-mode" "q")))
        "unbind -T copy-mode must return T")
    (is (null (cl-tmux/config:key-table-lookup "copy-mode" #\q))
        "copy-mode binding must be removed after unbind -T")))

;;; %whitespace-p
;;; NOTE: set-option-directive-stores-option, set-window-option-directive-stores-option,
;;; sets-directive-stores-option, and set-session-option-directive-stores-option have
;;; been removed — all superseded by set-option-directive-aliases-table-driven (below).

(test whitespace-p-recognizes-space-and-tab
  "%whitespace-p returns T for space and tab, NIL for other chars."
  (is-true  (cl-tmux/config::%whitespace-p #\Space) "space is whitespace")
  (is-true  (cl-tmux/config::%whitespace-p #\Tab)   "tab is whitespace")
  (is-false (cl-tmux/config::%whitespace-p #\a)     "letter is not whitespace")
  (is-false (cl-tmux/config::%whitespace-p #\Newline) "newline is not whitespace"))

;;; %parse-bind-key-args edge cases

(test parse-bind-key-args-empty-returns-nil
  "%parse-bind-key-args with empty args list returns NIL."
  (is (null (cl-tmux/config::%parse-bind-key-args '()))
      "empty args must return NIL"))

(test parse-bind-key-args-T-flag-missing-table-returns-nil
  "%parse-bind-key-args with -T and no table name returns NIL."
  (is (null (cl-tmux/config::%parse-bind-key-args '("-T")))
      "-T with no following table name must return NIL"))

(test parse-bind-key-args-unknown-command-returns-nil
  "%parse-bind-key-args with an unknown command returns NIL."
  (is (null (cl-tmux/config::%parse-bind-key-args '("z" "unknown-bogus-command")))
      "unknown command must cause %parse-bind-key-args to return NIL"))

(test parse-bind-key-args-n-and-r-flags-combined
  "%parse-bind-key-args with -n -r binds in root table with repeatable."
  (multiple-value-bind (table key kw repeatable)
      (cl-tmux/config::%parse-bind-key-args '("-n" "-r" "z" "new-window"))
    (is (string= "root" table) "table must be root for -n flag")
    (is (char= #\z key)        "key must be #\\z")
    (is (eq :new-window kw)    "command must be :new-window")
    (is-true repeatable        "repeatable must be T for -r flag")))

;;; %parse-unbind-key-args edge cases

(test parse-unbind-key-args-empty-returns-nil-nil
  "%parse-unbind-key-args with empty args returns (values nil nil)."
  (multiple-value-bind (table key)
      (cl-tmux/config::%parse-unbind-key-args '())
    (is (null table) "table must be NIL for empty args")
    (is (null key)   "key must be NIL for empty args")))

(test parse-unbind-key-args-extra-arg-returns-nil-nil
  "%parse-unbind-key-args with extra trailing arg returns (values nil nil)."
  (multiple-value-bind (table key)
      (cl-tmux/config::%parse-unbind-key-args '("z" "extra"))
    (is (null table) "table must be NIL when extra arg present")
    (is (null key)   "key must be NIL when extra arg present")))

(test parse-unbind-key-args-T-flag-missing-table-returns-nil
  "%parse-unbind-key-args with -T and no table name returns (values nil nil)."
  (multiple-value-bind (table key)
      (cl-tmux/config::%parse-unbind-key-args '("-T"))
    (is (null table) "table must be NIL when -T has no table name")
    (is (null key)   "key must be NIL when -T has no table name")))

;;; Backslash-escape edge case: backslash at end of string

(test config-tokenizer-backslash-at-end-of-string
  "%config-tokens: backslash at the end of string does not signal an error."
  (finishes (cl-tmux/config::%config-tokens "token\\")))

;;; load-config-from-string: all-blank and all-comment input

(test load-from-string-blank-input-returns-zero
  "load-config-from-string with only blanks and comments returns 0."
  (with-isolated-config
    (let ((applied (load-config-from-string
                    (format nil "# comment~%~%   ~%# another~%"))))
      (is (= 0 applied)
          "blank/comment-only input must apply 0 directives, got ~A" applied))))

;;; bind -r -n combined (order-independent flags)

(test bind-key-r-then-n-flag
  "bind -r -n binds in root table and marks repeatable (flag order insensitive)."
  (with-isolated-key-tables
    (apply-config-directive '("bind" "-r" "-n" "G" "new-window"))
    (let ((entry (cl-tmux/config:key-table-lookup "root" #\G)))
      (is (not (null entry))
          "bind -r -n must add a binding to the root table")
      (is (eq :new-window (cl-tmux/config:key-table-command entry))
          "root table binding must be :new-window")
      (is (cl-tmux/config:key-table-repeatable-p entry)
          "binding must be repeatable when -r flag is present"))))

