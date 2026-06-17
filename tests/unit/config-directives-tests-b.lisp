(in-package #:cl-tmux/test)

;;;; %parse-bind-key-args, tokenizer, apply-config-directive, set aliases, source-file, run-shell, %expand-tilde, if/elif, unbind-all — part II

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

;;; ── set option directives: shared alias source ───────────────────────────
;;;
;;; The directive table in src/config-directives-set.lisp owns the shared alias
;;; list; this test iterates that source directly so the definition and
;;; verification stay aligned.

(test set-option-directive-aliases-table-driven
  "Each set-option directive alias stores a value in the global options table."
  (dolist (verb cl-tmux/config::+set-directive-aliases+)
    (with-fresh-global-options
      (let ((result (apply-config-directive (list verb "status-interval" "7"))))
        (is (eq t result)
            "~A directive must return T, got ~S" verb result)
        (is (= 7 (cl-tmux/options:get-option "status-interval"))
            "~A must store status-interval = 7 in global options, got ~S"
            verb (cl-tmux/options:get-option "status-interval"))))))

;;; ── set -s server option routing ──────────────────────────────────────────

(test apply-set-directive-server-flag
  "'set -s exit-empty off' routes to server-options; exit-empty is server-only."
  ;; exit-empty is in *server-option-registry* but NOT in *option-registry* /
  ;; *global-options*, so the assertion 'not in global-options' is clean.
  (let ((cl-tmux/options:*server-options* (make-hash-table :test #'equal)))
    (assert-set-directive-option-state '("set" "-s" "exit-empty" "off")
                                       "exit-empty" nil
                                       :context "set -s exit-empty off"
                                       :server-p t)
    (is (null (cl-tmux/options:get-server-option "exit-empty"))
        "exit-empty must be NIL ('off') in server-options")
    (is (null (cl-tmux/options:get-option "exit-empty" nil))
        "exit-empty must NOT appear in global-options (it is server-only)")))

(test apply-set-directive-rejects-unsupported-terminal-options
  "terminal-overrides/features are terminal matching directives cl-tmux does not implement."
  (dolist (form '(("set" "-g" "terminal-overrides" "xterm*:RGB")
                  ("set-option" "-g" "terminal-features" "xterm*:RGB")))
    (with-fresh-global-options
      (destructuring-bind (verb flag name value) form
        (declare (ignore verb flag))
        (assert-config-directive-rejected form (format nil "~S" form))
        (is (not (equal value (cl-tmux/options:get-option name nil)))
            "~A must not be stored when the terminal-matching behavior is unsupported"
            name)))))

;;; ── source-file directive ──────────────────────────────────────────────────

(test source-file-directive-loads-temp-file
  "source-file applies a config file from disk, returning T."
  (with-isolated-config
    (with-temp-config-file (p "bind z next-window")
      (assert-config-directive-applied (list "source-file" (namestring p))
                                       "source-file temp file")
      (is (eq :next-window (lookup-key-binding #\z))
          "#\\z must be bound after source-file"))))

(test source-file-missing-returns-t-silently
  "source-file on a nonexistent file returns T (errors are ignored)."
  (with-isolated-config
    (assert-config-directive-applied '("source-file" "/nonexistent-cl-tmux-config-abc.conf")
                                     "source-file missing path")))

(test source-file-n-parse-only-does-not-execute
  "source-file -n parses the file but executes NOTHING (tmux CMD_PARSE_PARSEONLY).
   Asserts via an OPTION the file would set (a key like z has a DEFAULT binding, so
   'unbound' is not a reliable 'not executed' signal)."
  (with-isolated-config
    (with-temp-config-file (p "set -g status-left PARSEONLY")
      (assert-config-directive-applied (list "source-file" "-n" (namestring p))
                                       "source-file -n parse only")
      (is (not (string= "PARSEONLY" (cl-tmux/options:get-option "status-left")))
          "-n must NOT execute: the option is left unchanged"))))

(test source-file-without-n-executes-control
  "Control: WITHOUT -n the same file DOES set the option — isolating that -n is what
   suppresses execution."
  (with-isolated-config
    (with-temp-config-file (p "set -g status-left EXECUTED")
      (apply-config-directive (list "source-file" (namestring p)))
      (is (string= "EXECUTED" (cl-tmux/options:get-option "status-left"))
          "without -n the option is set"))))

(test source-file-clustered-qn-does-not-execute
  "Clustered -qn is also parse-only (q tolerated, n suppresses execution)."
  (with-isolated-config
    (with-temp-config-file (p "set -g status-left QNFLAG")
      (apply-config-directive (list "source-file" "-qn" (namestring p)))
      (is (not (string= "QNFLAG" (cl-tmux/options:get-option "status-left")))
          "-qn must not execute"))))

(test parse-source-file-flags-clustered
  "%parse-source-file-flags parses clustered -Fnqv and returns the path positionals."
  (multiple-value-bind (n q v f rest)
      (cl-tmux/config::%parse-source-file-flags '("-Fnqv" "/path/to.conf"))
    (is-true  n "parse-only (n)")
    (is-true  q "quiet (q)")
    (is-true  v "verbose (v)")
    (is-true  f "format (F)")
    (is (equal '("/path/to.conf") rest) "positionals = the path")))

(test parse-source-file-flags-target-pane
  "%parse-source-file-flags consumes tmux's -t target-pane without treating it as a path."
  (multiple-value-bind (n q v f rest)
      (cl-tmux/config::%parse-source-file-flags '("-q" "-t" "%1" "/path/to.conf"))
    (declare (ignore n v f))
    (is-true q "quiet (q)")
    (is (equal '("/path/to.conf") rest) "target pane must not remain in positionals")))

(test consume-leading-flag-tokens-stops-at-first-non-flag
  "%consume-leading-flag-tokens walks leading flags and stops at the first positional token."
  (let ((seen '()))
    (is (equal '("cmd" "arg")
               (cl-tmux/config::%consume-leading-flag-tokens
                '("-b" "-F" "cmd" "arg")
                (lambda (tok rest)
                  (push tok seen)
                  (values rest t)))))
    (is (equal '("-F" "-b") seen)
        "callback must see the leading flags in order")))

;;; ── run-shell directive ───────────────────────────────────────────────────

(test run-shell-apply-directive-table
  "run-shell returns T regardless of exit code."
  (dolist (c '(("run-shell" ("true")  "run-shell returns T")
               ("run-shell" ("false") "run-shell error silently returns T")))
    (destructuring-bind (cmd args desc) c
      (assert-config-directive-applied (cons cmd args) desc))))

;;; ── run-shell flag tolerance (-b / -t / -d / -C) ───────────────────────────
;;;
;;; %apply-run-shell-directive strips leading flags so the common
;;; `run-shell -b 'cmd'` form — which the fixed-arity table silently dropped —
;;; is handled.  These tests assert the handler's RETURN VALUE (handled vs not)
;;; rather than shell side-effects; `true` is used so any actual execution is
;;; harmless and fast.

(test run-shell-handler-table
  "%apply-run-shell-directive returns T for handled forms and NIL for non-run commands.
   Each row is (expected cmd args description)."
  (dolist (c '((t   "run-shell" ("-b" "true")                 "run-shell -b true (background flag)")
               (t   "run-shell" ("-t" "0" "-b" "true")      "run-shell -t 0 -b true (target+bg)")
               (nil "bind"      ("x" "next-window")           "bind (non-run command)")
               (t   "run-shell" ("-b")                        "run-shell -b only (flag-only no-op)")
               (t   "run-shell" ("-C" "new-window")           "run-shell -C <cmd> (tmux-cmd no-op)")
               (t   "run-shell" ("-d" "5" "true")             "run-shell -d 5 true (delay flag)")
               (t   "run-shell" ("-x" "true")                 "run-shell -x true (unknown flag skipped)")
               (t   "run-shell" ()                            "run-shell no args (empty no-op)")
               (t   "run-shell" ("-b" "echo" "hello" "world") "run-shell -b echo hello world (multi-word)")))
    (destructuring-bind (expected cmd args desc) c
      (with-isolated-config
        (let ((result (cl-tmux/config::%apply-run-shell-directive cmd args)))
          (if expected
              (is (eq t result) "~A must return T (got ~S)" desc result)
              (is (null result) "~A must return NIL (got ~S)" desc result)))))))

;;; ── %expand-leading-tilde ──────────────────────────────────────────────────

(test expand-leading-tilde-table
  "%expand-leading-tilde expands ~/... to $HOME/...; all other paths pass through."
  (let ((home (or (ignore-errors (sb-ext:posix-getenv "HOME")) "~")))
    (dolist (c (list (list "~/x"                     (concatenate 'string home "/x")                    "~/x → $HOME/x")
                     (list "~/.tmux/plugins/tpm/tpm" (concatenate 'string home "/.tmux/plugins/tpm/tpm") "full tpm path")
                     (list "/abs"   "/abs"   "absolute path unchanged")
                     (list "rel"    "rel"    "relative path unchanged")
                     (list "~"      "~"      "bare ~ unchanged")
                     (list "~/"     "~/"     "exact ~/ unchanged")
                     (list "~user"  "~user"  "~user unchanged")
                     (list "a/~/b"  "a/~/b"  "embedded ~ unchanged")))
      (destructuring-bind (input expected desc) c
        (is (string= expected (cl-tmux/config::%expand-leading-tilde input))
            "~A" desc)))))

;;; ── %if / %else / %endif preprocessor ───────────────────────────────────

(test if-else-endif-truthy-condition
  "%if with a truthy condition applies the then-block and skips the else-block."
  (with-isolated-config
    ;; *config-condition-evaluator* is NIL by default → all conditions truthy.
    (let ((applied (load-config-from-string
                    (format nil "%if 1~%bind z new-window~%%else~%bind z detach~%%endif~%"))))
      (is (= 1 applied)
          "only 1 directive must be applied under a truthy %if, got ~A" applied)
      (is (eq :new-window (lookup-key-binding #\z))
          "#\\z must be :new-window (then-block), not :detach (else-block)"))))

(test if-else-endif-falsy-condition
  "%if with a falsy condition skips the then-block and applies the else-block."
  (with-isolated-config
    ;; Set evaluator to return '0' (falsy) for any condition.
    (let ((cl-tmux/config:*config-condition-evaluator*
            (lambda (s) (declare (ignore s)) "0")))
      (let ((applied (load-config-from-string
                      (format nil "%if 0~%bind z new-window~%%else~%bind z detach~%%endif~%"))))
        (is (= 1 applied)
            "only 1 directive must be applied under a falsy %if, got ~A" applied)
        (is (eq :detach (lookup-key-binding #\z))
            "#\\z must be :detach (else-block) when condition is falsy")))))

(test if-endif-no-else
  "%if without %else applies the block when truthy, applies nothing when falsy."
  (with-isolated-config
    ;; Truthy (default evaluator NIL → all truthy)
    (let ((applied (load-config-from-string
                    (format nil "%if 1~%bind z new-window~%%endif~%"))))
      (is (= 1 applied) "truthy %if without else applies 1 directive"))
    ;; Falsy
    (let ((cl-tmux/config:*config-condition-evaluator*
            (lambda (s) (declare (ignore s)) "0")))
      (let ((applied (load-config-from-string
                      (format nil "%if 0~%bind w detach~%%endif~%"))))
        (is (= 0 applied) "falsy %if without else applies 0 directives")))))

(test if-block-outside-applies-normally
  "Lines outside %if blocks are always applied regardless of evaluator."
  (with-isolated-config
    (let ((applied (load-config-from-string
                    (format nil "bind z new-window~%%if 1~%bind n next-window~%%endif~%bind p prev-window~%"))))
      (is (= 3 applied)
          "3 directives must be applied (2 outside + 1 inside truthy %if)"))))

(test nested-if-blocks
  "Nested %if blocks work: inner block is skipped when outer is falsy."
  (with-isolated-config
    (let ((cl-tmux/config:*config-condition-evaluator*
            (lambda (s) (declare (ignore s)) "0")))
      (let ((applied (load-config-from-string
                      (format nil "%if 0~%%if 1~%bind z new-window~%%endif~%%endif~%"))))
        (is (= 0 applied)
            "no directives inside a falsy outer %if block"))))
  ;; All truthy
  (with-isolated-config
    (let ((applied (load-config-from-string
                    (format nil "%if 1~%%if 1~%bind z new-window~%%endif~%%endif~%"))))
      (is (= 1 applied) "one directive inside nested truthy %if blocks"))))

(test if-condition-evaluated-by-callback
  "%if condition string is passed verbatim to *config-condition-evaluator*."
  (with-isolated-config
    (let ((received nil))
      (let ((cl-tmux/config:*config-condition-evaluator*
              (lambda (s) (setf received s) "1")))
        (load-config-from-string (format nil "%if some-condition~%bind z new-window~%%endif~%"))
        (is (string= "some-condition" received)
            "condition must be passed verbatim to the evaluator, got ~S" received)))))

;;; ── %tmux-conf-paths ─────────────────────────────────────────────────────

(test tmux-conf-paths-returns-list
  "%tmux-conf-paths returns a list of pathname candidates."
  (let ((paths (cl-tmux/config::%tmux-conf-paths #p"/home/user/")))
    (is (listp paths) "must return a list")
    (is (>= (length paths) 1) "must have at least 1 candidate")))

;;; ── config-file-path (environment-variable reading path) ─────────────────
;;;
;;; These tests exercise config-file-path by temporarily overriding
;;; environment variables.  Since posix-getenv reads the real environment
;;; and we cannot setenv from Lisp portably in tests, we test the pure
;;; %config-path-from helper directly (already covered above) and only
;;; verify that config-file-path returns a pathname (not NIL) from the
;;; live environment.

(test config-file-path-returns-pathname
  "config-file-path returns a pathname object (not NIL or a string)."
  (let ((result (config-file-path)))
    (is (pathnamep result)
        "config-file-path must return a pathname, got ~S" result)))

;;; ── set-environment -u (unset) ───────────────────────────────────────────

(test apply-set-environment-u-unsets-variable
  "'set-environment -u VAR' config directive unsets the variable (tmux unset flag)."
  (let ((name "CLTMUX_TEST_ENV_VAR_CFG"))
    (sb-posix:setenv name "x" 1)
    (is (string= "x" (sb-ext:posix-getenv name)) "precondition: var is set")
    (assert-config-directive-applied (list "set-environment" "-u" name)
                                     "set-environment -u")
    (is (null (sb-ext:posix-getenv name))
        "config set-environment -u must unset the variable")))

(test apply-set-environment-t-writes-target-session
  "'set-environment -t target VAR VALUE' config directive writes the target session overlay."
  (let ((name "CLTMUX_TEST_ENV_VAR_CFG_T")
        (target-name "CLTMUX_TEST_ENV_TARGET_CFG_T"))
    (unwind-protect
         (progn
           (ignore-errors (sb-posix:unsetenv name))
           (let ((target (make-fake-session :nwindows 1 :npanes 1)))
             (with-registered-sessions ((target-name target))
               (assert-config-directive-applied (list "set-environment" "-t" target-name name "value")
                                                "set-environment -t")
               (multiple-value-bind (value source)
                   (cl-tmux/model:session-environment-value target name)
                 (is (string= "value" value)
                     "config set-environment -t must write the target session")
                 (is (eq :session source)
                     "config set-environment -t must record a session source"))
               (is (null (sb-ext:posix-getenv name))
                   "config set-environment -t must not touch the process environment"))))
      (ignore-errors (sb-posix:unsetenv name)))))

(test apply-set-environment-g-t-u-is-rejected
  "'set-environment -g -t target -u VAR' config directive is rejected."
  (let ((name "CLTMUX_TEST_ENV_VAR_CFG_GT_U"))
    (unwind-protect
         (progn
           (sb-posix:setenv name "x" 1)
           (assert-config-directive-rejected (list "set-environment" "-g" "-t" "ignored" "-u" name)
                                             "set-environment -g -t target -u")
           (is (string= "x" (sb-ext:posix-getenv name))
               "config set-environment -g -t target -u must not unset VAR"))
      (ignore-errors (sb-posix:unsetenv name)))))

;;; ── %apply-option-side-effects: prefix branch ────────────────────────────
;;;
;;; Tests that "set -g prefix C-a" updates *prefix-key-code* and registers
;;; the new key in the prefix table (the prefix2 branch has no separate
;;; integration path into tests, so we cover the scalar + key-table path here).

(test apply-set-directive-prefix-side-effect
  "'set -g prefix C-a' updates *prefix-key-code* to 1 and binds the new key."
  (with-isolated-key-tables
    (let ((cl-tmux/config:*prefix-key-code* cl-tmux/config:+prefix-key-code+))
      (apply-config-directive '("set" "-g" "prefix" "C-a"))
      (is (= 1 cl-tmux/config:*prefix-key-code*)
          "*prefix-key-code* must be 1 (C-a) after 'set -g prefix C-a'")
      (let ((entry (cl-tmux/config:key-table-lookup "prefix" (code-char 1))))
        (is (not (null entry))
            "C-a (code-char 1) must be bound in the prefix table after prefix change")
        (is (eq :send-prefix (cl-tmux/config:key-table-command entry))
            "the new prefix key must be bound to :send-prefix")))))

;;; ── unbind-all directive ─────────────────────────────────────────────────────

(test apply-config-directive-unbind-all-clears-prefix-table
  "'unbind-all' removes all bindings from the prefix key-table."
  (with-isolated-key-tables
    ;; Verify there's at least one binding first (e.g. C-c = :new-window).
    (let ((before (cl-tmux/config:key-table-lookup "prefix" #\c)))
      (is (not (null before)) "prefix table must have at least one binding before unbind-all"))
    ;; Now clear it.
    (cl-tmux/config:apply-config-directive '("unbind-all"))
    ;; All bindings in prefix table should be gone.
    (is (null (cl-tmux/config:key-table-lookup "prefix" #\c))
        "C-c must be unbound after unbind-all")))

(test apply-config-directive-unbind-all-T-clears-named-table
  "'unbind-all -T root' removes all bindings from the root key-table."
  (with-isolated-key-tables
    ;; Bind something in root.
    (cl-tmux/config:key-table-bind "root" #\x :new-window)
    (cl-tmux/config:apply-config-directive '("unbind-all" "-T" "root"))
    (is (null (cl-tmux/config:key-table-lookup "root" #\x))
        "root binding must be cleared after unbind-all -T root")))
