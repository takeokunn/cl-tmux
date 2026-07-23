(in-package #:cl-tmux/test)

;;;; bind args, tokenizer, canonical set directives, server flag,
;;;; terminal-option routing - part II

(describe "config-directives-suite"

  ;;; ── %parse-bind-key-args with valid complete args ─────────────────────────

  ;; %parse-bind-key-args with valid key+command returns all four values.
  (it "parse-bind-key-args-returns-all-values"
    (multiple-value-bind (table key kw repeatable)
        (cl-tmux/config::%parse-bind-key-args '("z" "new-window"))
      (check-table (list (list table "prefix"    "table defaults to prefix")
                         (list key   #\z         "key must be #\\z")
                         (list kw    :new-window "command must be :new-window"))
                   :test #'equal)
      (expect (null repeatable))))

  ;; %parse-bind-key-args with -T uses the given table name.
  (it "parse-bind-key-args-T-flag-specifies-table"
    (multiple-value-bind (table key kw ignored-rep)
        (cl-tmux/config::%parse-bind-key-args '("-T" "copy-mode" "q" "copy-mode-enter"))
      (declare (ignore ignored-rep))
      (check-table (list (list table "copy-mode"      "table must be copy-mode")
                         (list key   #\q              "key must be #\\q")
                         (list kw    :copy-mode-enter "command must be :copy-mode-enter"))
                   :test #'equal)))

  ;; %parse-bind-key-args with -r sets repeatable to T.
  (it "parse-bind-key-args-r-flag-sets-repeatable"
    (multiple-value-bind (table ignored-key kw repeatable)
        (cl-tmux/config::%parse-bind-key-args '("-r" "H" "resize-left"))
      (declare (ignore ignored-key))
      (expect (string= "prefix" table))
      (expect (eq :resize-left kw))
      (expect repeatable :to-be-truthy)))

  ;;; ── %tokenize-backslash-escape direct tests ──────────────────────────────

  ;; %tokenize-backslash-escape at the very end of input does not signal.
  (it "tokenize-backslash-escape-at-end-produces-partial-token"
    (finishes
      (let ((toks (cl-tmux/config::%config-tokens "abc\\")))
        ;; The backslash is at EOL — just one partial token, no error.
        (expect (= 1 (length toks))))))

  ;;; ── %tokenize-double-quoted unmatched quote ───────────────────────────────

  ;; %config-tokens: an unmatched double-quote is treated as a literal character.
  (it "tokenize-double-quoted-unmatched-treats-as-literal"
    ;; Input: a single \" with no closing quote.
    (let ((tokens (cl-tmux/config::%config-tokens "\"")))
      ;; The lone \" starts a token but has no closing quote — the opening \" is
      ;; a literal so we get a token containing the character.
      (expect (= 1 (length tokens)))))

  ;;; ── %tokenize-single-quoted direct test ──────────────────────────────────


  ;;; ── Table-driven tokenizer tests ─────────────────────────────────────────
  ;;;
  ;;; Parameterises the whitespace-splitting and quoting cases, eliminating the
  ;;; structural duplication across the 6 separate tokenizer tests above.

  ;; %config-tokens produces the correct token list across representative inputs.
  (it "config-tokens-table-driven"
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
          (expect (equal expected result))))))

  ;;; ── apply-config-directive on nil/empty input ─────────────────────────────

  ;; apply-config-directive with NIL (empty token list) returns NIL.
  (it "apply-config-directive-nil-returns-nil"
    (assert-config-directive-rejected nil "NIL token list"))

  ;;; ── set option directives: canonical command source ──────────────────────
  ;;;
  ;;; The directive table in src/config-directives-set.lisp owns the canonical
  ;;; command list; this test iterates that source directly so the definition and
  ;;; verification stay aligned.

  ;; Each canonical set-option directive command stores a value in the global options table.
  (it "set-option-directive-commands-table-driven"
    (dolist (verb cl-tmux/config::+set-directive-commands+)
      (with-fresh-global-options
        (let ((result (apply-config-directive (list verb "status-interval" "7"))))
          (expect (eq t result))
          (expect (= 7 (cl-tmux/options:get-option "status-interval")))))))

  ;; Short tmux aliases are not accepted; config directives use canonical commands only.
  (it "set-option-short-aliases-are-rejected"
    (dolist (verb '("set" "setw" "sets"))
      (with-fresh-global-options
        (let ((option-name "@short-alias-probe"))
          (assert-config-directive-rejected (list verb option-name "7") verb)
          (assert-config-directive-rejected (list verb "-g" option-name "7") verb)
          (expect (null (cl-tmux/options:get-option option-name nil)))))))

  ;;; ── set-option -s server option routing ──────────────────────────────────────────

  ;; 'set-option -s exit-empty off' routes to server-options; exit-empty is server-only.
  (it "apply-set-directive-server-flag"
    ;; exit-empty is in *server-option-registry* but NOT in *option-registry* /
    ;; *global-options*, so the assertion 'not in global-options' is clean.
    (let ((cl-tmux/options:*server-options* (make-hash-table :test #'equal)))
      (assert-set-directive-option-state '("set-option" "-s" "exit-empty" "off")
                                         "exit-empty" nil
                                         :context "set-option -s exit-empty off"
                                         :server-p t)
      (expect (null (cl-tmux/options:get-server-option "exit-empty")))
      (expect (null (cl-tmux/options:get-option "exit-empty" nil)))))

  ;; terminal-overrides/features are ACCEPTED and stored like real tmux (they
  ;; appear in virtually every real .tmux.conf); cl-tmux applies no
  ;; terminal-matching behavior but must not break config transparency.
  (it "apply-set-directive-accepts-terminal-matching-options"
    (dolist (form '(("set-option" "-g" "terminal-overrides" "xterm*:RGB")
                    ("set-option" "-g" "terminal-features" "xterm*:RGB")))
      (with-fresh-global-options
        (destructuring-bind (verb flag name value) form
          (declare (ignore verb flag))
          (assert-config-directive-applied form (format nil "~S" form))
          (expect (equal value (cl-tmux/options:get-option name nil))))))))
