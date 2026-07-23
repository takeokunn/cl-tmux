(in-package #:cl-tmux/test)

;;;; Config directives tests — part V: macro registry, env-set-p, key-table edge cases, remaining bind/set directives.

(describe "config-directives-suite"

  ;; define-config-directives is a defined macro.
  (it "define-config-directives-macro-is-defined"
    (expect (macro-function 'cl-tmux/config::define-config-directives)))

  ;; define-key-directive-handlers is a defined macro.
  (it "define-key-directive-handlers-macro-is-defined"
    (expect (macro-function 'cl-tmux/config::define-key-directive-handlers)))

  ;; %env-set-p returns T for non-empty strings and NIL for nil or empty strings.
  (it "env-set-p-correctly-classifies-strings"
    (expect (cl-tmux/config::%env-set-p "/some/path") :to-be-truthy)
    (expect (cl-tmux/config::%env-set-p "x") :to-be-truthy)
    (expect (cl-tmux/config::%env-set-p nil) :to-be-falsy)
    (expect (cl-tmux/config::%env-set-p "") :to-be-falsy))

  ;;; Tokenizer with quote/escape support

  ;; %config-tokens with a double-quoted string produces a single token preserving spaces.
  (it "config-tokenizer-quoted-double-quotes"
    (let ((tokens (cl-tmux/config::%config-tokens "bind n \"foo bar\"")))
      (expect (equal '("bind" "n" "foo bar") tokens))))

  ;; %config-tokens with a single-quoted string produces a single token.
  (it "config-tokenizer-single-quotes"
    (let ((tokens (cl-tmux/config::%config-tokens "set-shell '/usr/bin/my shell'")))
      (expect (= 2 (length tokens)))
      (expect (string= "/usr/bin/my shell" (second tokens)))))

  ;; %config-tokens: backslash outside quotes escapes the next character.
  (it "config-tokenizer-backslash-escape"
    ;; The Lisp literal "foo\\ bar" is the 8-char string  foo\ bar  (with a real
    ;; backslash); that is what the tokenizer must collapse to "foo bar".
    (let ((tokens (cl-tmux/config::%config-tokens "foo\\ bar")))
      (expect (= 1 (length tokens)))
      (expect (string= "foo bar" (first tokens)))))

  ;; %config-tokens: empty double-quotes produces an empty string token.
  (it "config-tokenizer-empty-double-quotes"
    (let ((tokens (cl-tmux/config::%config-tokens "cmd \"\"")))
      (expect (= 2 (length tokens)))
      (expect (string= "" (second tokens)))))

  ;; %config-tokens: mix of plain tokens, quoted tokens, and backslash escapes.
  (it "config-tokenizer-mixed"
    ;; "a \"b c\" d\\ e" is the literal  a "b c" d\ e  — quoted token preserves the
    ;; inner space; the backslash-space escapes to keep "d e" as one token.
    (let ((tokens (cl-tmux/config::%config-tokens "a \"b c\" d\\ e")))
      (expect (= 3 (length tokens)))
      (dolist (c '((0 "a"   "first token is a")
                   (1 "b c" "second token is b c")
                   (2 "d e" "third token is d e (backslash-space)")))
        (destructuring-bind (idx expected desc) c
          (declare (ignore desc))
          (expect (string= expected (nth idx tokens)))))))

  ;;; bind/unbind directives with flags

  ;; bind -n binds in the root key-table (no prefix required).
  (it "bind-key-no-prefix-n-flag"
    (with-isolated-key-tables
      (apply-config-directive '("bind" "-n" "C" "new-window"))
      (let ((entry (cl-tmux/config:key-table-lookup "root" #\C)))
        (expect (not (null entry)))
        (expect (eq :new-window (cl-tmux/config:key-table-command entry))))))

  ;; bind -r marks the binding as repeatable.
  (it "bind-key-repeatable-r-flag"
    (with-isolated-key-tables
      (apply-config-directive '("bind" "-r" "H" "resize-left"))
      (let ((entry (cl-tmux/config:key-table-lookup "prefix" #\H)))
        (expect (not (null entry)))
        (expect (cl-tmux/config:key-table-repeatable-p entry)))))

  ;; bind -T table-name binds in the named key-table.
  (it "bind-key-custom-table-T-flag"
    (with-isolated-key-tables
      (apply-config-directive '("bind" "-T" "copy-mode" "q" "copy-mode-enter"))
      (let ((entry (cl-tmux/config:key-table-lookup "copy-mode" #\q)))
        (expect (not (null entry)))
        (expect (eq :copy-mode-enter (cl-tmux/config:key-table-command entry))))))

  ;; Simple bind (no flags) also updates the prefix key-table.
  (it "bind-key-simple-also-updates-key-table"
    (with-isolated-key-tables
      (apply-config-directive '("bind" "z" "new-window"))
      (let ((entry (cl-tmux/config:key-table-lookup "prefix" #\z)))
        (expect (not (null entry)))
        (expect (eq :new-window (cl-tmux/config:key-table-command entry))))))

  ;;; unbind with -n flag

  ;; unbind -n removes a binding from the root table.
  (it "unbind-with-n-flag-removes-root-binding"
    (with-isolated-key-tables
      (apply-config-directive '("bind" "-n" "X" "new-window"))
      (let ((entry (cl-tmux/config:key-table-lookup "root" #\X)))
        (expect (not (null entry))))
      (assert-config-directive-applied '("unbind" "-n" "X")
                                       "unbind -n X")
      (expect (null (cl-tmux/config:key-table-lookup "root" #\X)))))

  ;; Config parsing accepts only canonical bind/unbind directive names.
  (it "key-directive-aliases-are-rejected"
    (with-isolated-key-tables
      (let ((key #\@))
        (assert-config-directive-rejected `("bind-key" ,(string key) "new-window")
                                          "bind-key alias")
        (expect (null (lookup-key-binding key)))
        (assert-config-directive-applied `("bind" ,(string key) "new-window")
                                         "canonical bind")
        (expect (eq :new-window (lookup-key-binding key)))
        (assert-config-directive-rejected `("unbind-key" ,(string key))
                                          "unbind-key alias")
        (expect (eq :new-window (lookup-key-binding key))))))

  ;;; unbind with -T flag

  ;; unbind -T copy-mode removes a binding from the named table.
  (it "unbind-with-T-flag-removes-named-table-binding"
    (with-isolated-key-tables
      (apply-config-directive '("bind" "-T" "copy-mode" "q" "copy-mode-enter"))
      (let ((entry (cl-tmux/config:key-table-lookup "copy-mode" #\q)))
        (expect (not (null entry))))
      (assert-config-directive-applied '("unbind" "-T" "copy-mode" "q")
                                       "unbind -T copy-mode q")
      (expect (null (cl-tmux/config:key-table-lookup "copy-mode" #\q)))))

  ;;; %whitespace-p
  ;;; NOTE: these directive-store cases are now covered by the table-driven helper below.

  ;; %whitespace-p returns T for space and tab, NIL for other chars.
  (it "whitespace-p-recognizes-space-and-tab"
    (expect (cl-tmux/config::%whitespace-p #\Space) :to-be-truthy)
    (expect (cl-tmux/config::%whitespace-p #\Tab) :to-be-truthy)
    (expect (cl-tmux/config::%whitespace-p #\a) :to-be-falsy)
    (expect (cl-tmux/config::%whitespace-p #\Newline) :to-be-falsy))

  ;;; %parse-bind-key-args edge cases

  ;; %parse-bind-key-args with empty args list returns NIL.
  (it "parse-bind-key-args-empty-returns-nil"
    (expect (null (cl-tmux/config::%parse-bind-key-args '()))))

  ;; %parse-bind-key-args with -T and no table name returns NIL.
  (it "parse-bind-key-args-T-flag-missing-table-returns-nil"
    (expect (null (cl-tmux/config::%parse-bind-key-args '("-T")))))

  ;; %parse-bind-key-args with an unknown command returns NIL.
  (it "parse-bind-key-args-unknown-command-returns-nil"
    (expect (null (cl-tmux/config::%parse-bind-key-args '("z" "unknown-bogus-command")))))

  ;; %parse-bind-key-args with -n -r binds in root table with repeatable.
  (it "parse-bind-key-args-n-and-r-flags-combined"
    (multiple-value-bind (table key kw repeatable)
        (cl-tmux/config::%parse-bind-key-args '("-n" "-r" "z" "new-window"))
      (expect (string= "root" table))
      (expect (char= #\z key))
      (expect (eq :new-window kw))
      (expect repeatable :to-be-truthy)))

  ;;; %parse-unbind-key-args edge cases

  ;; %parse-unbind-key-args with empty args returns (values nil nil).
  (it "parse-unbind-key-args-empty-returns-nil-nil"
    (multiple-value-bind (table key)
        (cl-tmux/config::%parse-unbind-key-args '())
      (expect (null table))
      (expect (null key))))

  ;; %parse-unbind-key-args with extra trailing arg returns (values nil nil).
  (it "parse-unbind-key-args-extra-arg-returns-nil-nil"
    (multiple-value-bind (table key)
        (cl-tmux/config::%parse-unbind-key-args '("z" "extra"))
      (expect (null table))
      (expect (null key))))

  ;; %parse-unbind-key-args with -T and no table name returns (values nil nil).
  (it "parse-unbind-key-args-T-flag-missing-table-returns-nil"
    (multiple-value-bind (table key)
        (cl-tmux/config::%parse-unbind-key-args '("-T"))
      (expect (null table))
      (expect (null key))))

  ;;; Backslash-escape edge case: backslash at end of string

  ;; %config-tokens: backslash at the end of string does not signal an error.
  (it "config-tokenizer-backslash-at-end-of-string"
    (finishes (cl-tmux/config::%config-tokens "token\\")))

  ;;; load-config-from-string: all-blank and all-comment input

  ;; load-config-from-string with only blanks and comments returns 0.
  (it "load-from-string-blank-input-returns-zero"
    (with-isolated-config
      (let ((applied (load-config-from-string
                      (format nil "# comment~%~%   ~%# another~%"))))
        (expect (= 0 applied)))))

  ;;; bind -r -n combined (order-independent flags)

  ;; bind -r -n binds in root table and marks repeatable (flag order insensitive).
  (it "bind-key-r-then-n-flag"
    (with-isolated-key-tables
      (apply-config-directive '("bind" "-r" "-n" "G" "new-window"))
      (let ((entry (cl-tmux/config:key-table-lookup "root" #\G)))
        (expect (not (null entry)))
        (expect (eq :new-window (cl-tmux/config:key-table-command entry)))
        (expect (cl-tmux/config:key-table-repeatable-p entry))))))
