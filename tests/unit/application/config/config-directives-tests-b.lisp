(in-package #:cl-tmux/test)

;;;; bind args, tokenizer, set aliases, server flag, terminal-option routing — part II

(in-suite config-directives-suite)

;;; ── %parse-bind-key-args with valid complete args ─────────────────────────

(test parse-bind-key-args-returns-all-values
  "%parse-bind-key-args with valid key+command returns all four values."
  (multiple-value-bind (table key kw repeatable)
      (cl-tmux/config::%parse-bind-key-args '("z" "new-window"))
    (check-table (list (list table "prefix"    "table defaults to prefix")
                       (list key   #\z         "key must be #\\z")
                       (list kw    :new-window "command must be :new-window"))
                 :test #'equal)
    (is (null repeatable) "repeatable must be NIL by default")))

(test parse-bind-key-args-T-flag-specifies-table
  "%parse-bind-key-args with -T uses the given table name."
  (multiple-value-bind (table key kw ignored-rep)
      (cl-tmux/config::%parse-bind-key-args '("-T" "copy-mode" "q" "copy-mode-enter"))
    (declare (ignore ignored-rep))
    (check-table (list (list table "copy-mode"      "table must be copy-mode")
                       (list key   #\q              "key must be #\\q")
                       (list kw    :copy-mode-enter "command must be :copy-mode-enter"))
                 :test #'equal)))

(test parse-bind-key-args-r-flag-sets-repeatable
  "%parse-bind-key-args with -r sets repeatable to T."
  (multiple-value-bind (table ignored-key kw repeatable)
      (cl-tmux/config::%parse-bind-key-args '("-r" "H" "resize-left"))
    (declare (ignore ignored-key))
    (is (string= "prefix" table) "table must be prefix for -r alone")
    (is (eq :resize-left kw)      "command must be :resize-left")
    (is-true repeatable           "repeatable must be T with -r flag")))

;;; ── %tokenize-backslash-escape direct tests ──────────────────────────────


(test tokenize-backslash-escape-at-end-produces-partial-token
  "%tokenize-backslash-escape at the very end of input does not signal."
  (finishes
    (let ((toks (cl-tmux/config::%config-tokens "abc\\")))
      ;; The backslash is at EOL — just one partial token, no error.
      (is (= 1 (length toks)) "must have 1 token even with trailing backslash"))))

;;; ── %tokenize-double-quoted unmatched quote ───────────────────────────────

(test tokenize-double-quoted-unmatched-treats-as-literal
  "%config-tokens: an unmatched double-quote is treated as a literal character."
  ;; Input: a single \" with no closing quote.
  (let ((tokens (cl-tmux/config::%config-tokens "\"")))
    ;; The lone \" starts a token but has no closing quote — the opening \" is
    ;; a literal so we get a token containing the character.
    (is (= 1 (length tokens))
        "unmatched \" must produce 1 token, got ~S" tokens)))

;;; ── %tokenize-single-quoted direct test ──────────────────────────────────


;;; ── Table-driven tokenizer tests ─────────────────────────────────────────
;;;
;;; Parameterises the whitespace-splitting and quoting cases, eliminating the
;;; structural duplication across the 6 separate tokenizer tests above.

(test config-tokens-table-driven
  "%config-tokens produces the correct token list across representative inputs."
  (dolist (entry '(("bind c new-window"            ("bind" "c" "new-window"))
                   ("  set-shell  /bin/bash  "      ("set-shell" "/bin/bash"))
                   (""                              nil)
                   ("   "                           nil)
                   ("cmd \"\""                      ("cmd" ""))
                   ("a \"b c\" d\\ e"               ("a" "b c" "d e"))
                   ("a\\nb"                         ("anb"))
                   ("'hello world'"                 ("hello world"))
                   ("'a\\b'"                        ("a\\b"))))
    (destructuring-bind (input expected) entry
      (let ((result (cl-tmux/config::%config-tokens input)))
        (is (equal expected result)
            "%config-tokens ~S: expected ~S got ~S"
            input expected result)))))

;;; ── apply-config-directive on nil/empty input ─────────────────────────────

(test apply-config-directive-nil-returns-nil
  "apply-config-directive with NIL (empty token list) returns NIL."
  (assert-config-directive-rejected nil "NIL token list"))

;;; ── set option directives: canonical command source ──────────────────────
;;;
;;; The directive table in src/config-directives-set.lisp owns the canonical
;;; command list; this test iterates that source directly so the definition and
;;; verification stay aligned.

(test set-option-directive-commands-table-driven
  "Each canonical set-option directive command stores a value in the global options table."
  (dolist (verb cl-tmux/config::+set-directive-commands+)
    (with-fresh-global-options
      (let ((result (apply-config-directive (list verb "status-interval" "7"))))
        (is (eq t result)
            "~A directive must return T, got ~S" verb result)
        (is (= 7 (cl-tmux/options:get-option "status-interval"))
            "~A must store status-interval = 7 in global options, got ~S"
            verb (cl-tmux/options:get-option "status-interval"))))))

(test set-option-short-aliases-are-rejected
  "Short tmux aliases are not accepted; config directives use canonical commands only."
  (dolist (verb '("set" "setw" "sets"))
    (with-fresh-global-options
      (let ((option-name "@short-alias-probe"))
        (assert-config-directive-rejected (list verb option-name "7") verb)
        (assert-config-directive-rejected (list verb "-g" option-name "7") verb)
        (is (null (cl-tmux/options:get-option option-name nil))
            "~A must not store ~A" verb option-name)))))

;;; ── set-option -s server option routing ──────────────────────────────────────────

(test apply-set-directive-server-flag
  "'set-option -s exit-empty off' routes to server-options; exit-empty is server-only."
  ;; exit-empty is in *server-option-registry* but NOT in *option-registry* /
  ;; *global-options*, so the assertion 'not in global-options' is clean.
  (let ((cl-tmux/options:*server-options* (make-hash-table :test #'equal)))
    (assert-set-directive-option-state '("set-option" "-s" "exit-empty" "off")
                                       "exit-empty" nil
                                       :context "set-option -s exit-empty off"
                                       :server-p t)
    (is (null (cl-tmux/options:get-server-option "exit-empty"))
        "exit-empty must be NIL ('off') in server-options")
    (is (null (cl-tmux/options:get-option "exit-empty" nil))
        "exit-empty must NOT appear in global-options (it is server-only)")))

(test apply-set-directive-accepts-terminal-matching-options
  "terminal-overrides/features are ACCEPTED and stored like real tmux (they
   appear in virtually every real .tmux.conf); cl-tmux applies no
   terminal-matching behavior but must not break config transparency."
  (dolist (form '(("set-option" "-g" "terminal-overrides" "xterm*:RGB")
                  ("set-option" "-g" "terminal-features" "xterm*:RGB")))
    (with-fresh-global-options
      (destructuring-bind (verb flag name value) form
        (declare (ignore verb flag))
        (assert-config-directive-applied form (format nil "~S" form))
        (is (equal value (cl-tmux/options:get-option name nil))
            "~A must be stored like any other option" name)))))
