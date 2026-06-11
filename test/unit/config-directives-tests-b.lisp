(in-package #:cl-tmux/test)

;;;; config-directives tests — part B: %parse-bind-key-args, tokenizer edge cases,
;;;; apply-config-directive, set aliases, set -s, source-file, run-shell,
;;;; %expand-leading-tilde, preprocessor, set-environment, unbind-all,
;;;; side-effects, semicolon bindings, %elif chains, line continuation,
;;;; glob source, inline comments, if-shell, run-shell -C, mid-token #.

(in-suite config-directives-suite)

;;; ── %parse-bind-key-args with valid complete args ─────────────────────────

(test parse-bind-key-args-returns-all-values
  "%parse-bind-key-args with valid key+command returns all four values."
  (multiple-value-bind (table key kw repeatable)
      (cl-tmux/config::%parse-bind-key-args '("z" "new-window"))
    (is (string= "prefix" table) "table defaults to prefix")
    (is (char= #\z key)          "key must be #\\z")
    (is (eq :new-window kw)      "command must be :new-window")
    (is (null repeatable)        "repeatable must be NIL by default")))

(test parse-bind-key-args-T-flag-specifies-table
  "%parse-bind-key-args with -T uses the given table name."
  (multiple-value-bind (table key kw ignored-rep)
      (cl-tmux/config::%parse-bind-key-args '("-T" "copy-mode" "q" "copy-mode-enter"))
    (declare (ignore ignored-rep))
    (is (string= "copy-mode" table) "table must be copy-mode")
    (is (char= #\q key)             "key must be #\\q")
    (is (eq :copy-mode-enter kw)    "command must be :copy-mode-enter")))

(test parse-bind-key-args-r-flag-sets-repeatable
  "%parse-bind-key-args with -r sets repeatable to T."
  (multiple-value-bind (table ignored-key kw repeatable)
      (cl-tmux/config::%parse-bind-key-args '("-r" "H" "resize-left"))
    (declare (ignore ignored-key))
    (is (string= "prefix" table) "table must be prefix for -r alone")
    (is (eq :resize-left kw)      "command must be :resize-left")
    (is-true repeatable           "repeatable must be T with -r flag")))

;;; ── %tokenize-backslash-escape direct tests ──────────────────────────────

(test tokenize-backslash-escape-produces-escaped-char
  "%tokenize-backslash-escape pushes the character following the backslash."
  ;; We verify the behavior indirectly via %config-tokens which calls it.
  (let ((tokens (cl-tmux/config::%config-tokens "a\\nb")))
    (is (= 1 (length tokens))
        "backslash-n must be one token, got ~S" tokens)
    (is (string= "anb" (first tokens))
        "token must be 'anb' (backslash consumed), got ~S" (first tokens))))

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

(test tokenize-single-quoted-preserves-content
  "%config-tokens: single-quoted content is preserved literally."
  (let ((tokens (cl-tmux/config::%config-tokens "'hello world'")))
    (is (= 1 (length tokens))
        "single-quoted string must produce 1 token, got ~S" tokens)
    (is (string= "hello world" (first tokens))
        "token must be 'hello world', got ~S" (first tokens))))

(test tokenize-single-quoted-no-escape-processing
  "%config-tokens: backslash inside single quotes is literal, not an escape."
  (let ((tokens (cl-tmux/config::%config-tokens "'a\\b'")))
    (is (= 1 (length tokens))
        "single-quoted backslash-b must yield 1 token, got ~S" tokens)
    ;; Inside single quotes the backslash is literal, so token = "a\b" (3 chars).
    (is (= 3 (length (first tokens)))
        "token must be 3 chars (backslash is literal), got ~S" (first tokens))))

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
                   ("a \"b c\" d\\ e"               ("a" "b c" "d e"))))
    (destructuring-bind (input expected) entry
      (let ((result (cl-tmux/config::%config-tokens input)))
        (is (equal expected result)
            "%config-tokens ~S: expected ~S got ~S"
            input expected result)))))

;;; ── apply-config-directive on nil/empty input ─────────────────────────────

(test apply-config-directive-nil-returns-nil
  "apply-config-directive with NIL (empty token list) returns NIL."
  (is (null (apply-config-directive nil))
      "NIL token list must return NIL"))

;;; ── set option directives: table-driven aliases ───────────────────────────
;;;
;;; All six set-option aliases (set, set-option, setw, set-window-option,
;;; sets, set-session-option) produce the same result.  This table-driven
;;; test replaces the six near-identical individual tests with a single loop.

(test set-option-directive-aliases-table-driven
  "All six set-option directive aliases store a value in the global options table."
  (dolist (verb '("set" "set-option" "setw" "set-window-option" "sets" "set-session-option"))
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
    (is (eq t (apply-config-directive '("set" "-s" "exit-empty" "off")))
        "set -s must return T")
    (is (null (cl-tmux/options:get-server-option "exit-empty"))
        "exit-empty must be NIL ('off') in server-options")
    (is (null (cl-tmux/options:get-option "exit-empty" nil))
        "exit-empty must NOT appear in global-options (it is server-only)")))

;;; ── source-file / source directive ───────────────────────────────────────

(test source-file-directive-loads-temp-file
  "source-file applies a config file from disk, returning T."
  (with-isolated-config
    (with-temp-config-file (p "bind z next-window")
      (is (eq t (apply-config-directive (list "source-file" (namestring p))))
          "source-file must return T")
      (is (eq :next-window (lookup-key-binding #\z))
          "#\\z must be bound after source-file"))))

(test source-directive-is-alias-for-source-file
  "'source' is accepted as an alias for 'source-file'."
  (with-isolated-config
    (with-temp-config-file (p "bind w last-window")
      (is (eq t (apply-config-directive (list "source" (namestring p))))
          "source alias must return T")
      (is (eq :last-window (lookup-key-binding #\w))
          "#\\w must be bound after source"))))

(test source-file-missing-returns-t-silently
  "source-file on a nonexistent file returns T (errors are ignored)."
  (with-isolated-config
    (is (eq t (apply-config-directive '("source-file" "/nonexistent-cl-tmux-config-abc.conf")))
        "source-file on a missing file must return T (error silently ignored)")))

(test source-file-n-parse-only-does-not-execute
  "source-file -n parses the file but executes NOTHING (tmux CMD_PARSE_PARSEONLY).
   Asserts via an OPTION the file would set (a key like z has a DEFAULT binding, so
   'unbound' is not a reliable 'not executed' signal)."
  (with-isolated-config
    (with-temp-config-file (p "set -g status-left PARSEONLY")
      (is (eq t (apply-config-directive (list "source-file" "-n" (namestring p))))
          "source-file -n returns T")
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

;;; ── run-shell / run directive ─────────────────────────────────────────────

(test run-shell-directive-returns-t
  "run-shell runs a shell command at config parse time and returns T."
  (is (eq t (apply-config-directive '("run-shell" "true")))
      "run-shell must return T"))

(test run-directive-is-alias-for-run-shell
  "'run' is accepted as an alias for 'run-shell'."
  (is (eq t (apply-config-directive '("run" "true")))
      "run alias must return T"))

(test run-shell-errors-ignored
  "run-shell with a failing command returns T (errors silently ignored)."
  (is (eq t (apply-config-directive '("run-shell" "false")))
      "run-shell with exit-code 1 must still return T"))

;;; ── run-shell / run flag tolerance (-b / -t / -d / -C) ────────────────────
;;;
;;; %apply-run-shell-directive strips leading flags so the common
;;; `run-shell -b 'cmd'` / `run -b '~/.tmux/plugins/tpm/tpm'` forms — which the
;;; fixed-arity table silently dropped — are handled.  These tests assert the
;;; handler's RETURN VALUE (handled vs not) rather than shell side-effects;
;;; `true` is used so any actual execution is harmless and fast.

(test run-shell-handler-handles-background-flag
  "%apply-run-shell-directive returns T for 'run-shell -b true' (handled)."
  (with-isolated-config
    (is (eq t (cl-tmux/config::%apply-run-shell-directive "run-shell" '("-b" "true")))
        "run-shell -b true must be handled (T)")))

(test run-shell-handler-handles-bare-command
  "%apply-run-shell-directive returns T for the bare 'run true' (one arg) form."
  (with-isolated-config
    (is (eq t (cl-tmux/config::%apply-run-shell-directive "run" '("true")))
        "run true must be handled (T)")))

(test run-shell-handler-handles-target-then-background-flags
  "%apply-run-shell-directive strips '-t 0 -b' and still handles 'true' (T)."
  (with-isolated-config
    (is (eq t (cl-tmux/config::%apply-run-shell-directive
               "run-shell" '("-t" "0" "-b" "true")))
        "run-shell -t 0 -b true must be handled (T)")))

(test run-shell-handler-ignores-non-run-command
  "%apply-run-shell-directive returns NIL for a non-run command (e.g. bind)."
  (with-isolated-config
    (is (null (cl-tmux/config::%apply-run-shell-directive
               "bind" '("x" "next-window")))
        "a non-run command must not be handled (NIL)")))

(test run-shell-handler-flag-only-is-handled-no-op
  "%apply-run-shell-directive returns T (no error) for a flag-only 'run-shell -b'."
  (with-isolated-config
    (is (eq t (cl-tmux/config::%apply-run-shell-directive "run-shell" '("-b")))
        "a flag-only run-shell -b must be handled as a no-op (T)")))

(test run-shell-handler-C-flag-is-no-op
  "%apply-run-shell-directive returns T for '-C cmd' without shelling out
   (running a tmux command is out of scope; treated as handled/no-op)."
  (with-isolated-config
    (is (eq t (cl-tmux/config::%apply-run-shell-directive
               "run-shell" '("-C" "new-window")))
        "run-shell -C <tmux-cmd> must be handled as a no-op (T)")))

(test run-shell-handler-handles-delay-flag
  "%apply-run-shell-directive strips '-d 5' (delay) and handles 'true' (T)."
  (with-isolated-config
    (is (eq t (cl-tmux/config::%apply-run-shell-directive
               "run-shell" '("-d" "5" "true")))
        "run-shell -d 5 true must be handled (T)")))

(test run-shell-handler-unknown-flag-is-skipped
  "%apply-run-shell-directive skips an unknown bare flag '-x' and handles 'true' (T)."
  (with-isolated-config
    (is (eq t (cl-tmux/config::%apply-run-shell-directive
               "run-shell" '("-x" "true")))
        "run-shell -x true must skip the unknown flag and be handled (T)")))

(test run-shell-handler-run-alias-with-flag
  "%apply-run-shell-directive returns T for the real tpm 'run -b <cmd>' form."
  (with-isolated-config
    (is (eq t (cl-tmux/config::%apply-run-shell-directive "run" '("-b" "true")))
        "run -b true (the tpm form) must be handled (T)")))

(test run-shell-handler-empty-args-is-handled-no-op
  "%apply-run-shell-directive returns T for an empty args list (no-op)."
  (with-isolated-config
    (is (eq t (cl-tmux/config::%apply-run-shell-directive "run-shell" '()))
        "run-shell with no args must be handled as a no-op (T)")))

(test run-shell-handler-multiword-command-joined
  "%apply-run-shell-directive joins multi-word command tokens after the flag (T)."
  (with-isolated-config
    (is (eq t (cl-tmux/config::%apply-run-shell-directive
               "run-shell" '("-b" "echo" "hello" "world")))
        "run-shell -b echo hello world must join and be handled (T)")))

;;; ── %expand-leading-tilde ──────────────────────────────────────────────────

(test expand-leading-tilde-expands-tilde-slash
  "%expand-leading-tilde replaces a leading '~/' with $HOME, and leaves absolute
   and relative paths unchanged."
  (let ((home (or (ignore-errors (sb-ext:posix-getenv "HOME")) "~")))
    (is (string= (concatenate 'string home "/x")
                 (cl-tmux/config::%expand-leading-tilde "~/x"))
        "~/x must expand to $HOME/x")
    (is (string= "/abs" (cl-tmux/config::%expand-leading-tilde "/abs"))
        "an absolute path must pass through unchanged")
    (is (string= "rel" (cl-tmux/config::%expand-leading-tilde "rel"))
        "a relative path must pass through unchanged")))

(test expand-leading-tilde-leaves-non-tilde-slash-unchanged
  "%expand-leading-tilde only expands a leading '~/'; every other form passes
   through unchanged (bare ~, exact ~/, ~user, embedded ~)."
  (is (string= "~" (cl-tmux/config::%expand-leading-tilde "~"))
      "bare ~ (length 1, below the >2 guard) is unchanged")
  (is (string= "~/" (cl-tmux/config::%expand-leading-tilde "~/"))
      "exact ~/ (length 2, below the >2 guard) is unchanged")
  (is (string= "~user" (cl-tmux/config::%expand-leading-tilde "~user"))
      "~user is unchanged (only ~/ is expanded)")
  (is (string= "a/~/b" (cl-tmux/config::%expand-leading-tilde "a/~/b"))
      "an embedded ~ (not leading) is unchanged"))

(test expand-leading-tilde-expands-full-tpm-path
  "%expand-leading-tilde expands the real tpm path '~/.tmux/plugins/tpm/tpm'."
  (let ((home (or (ignore-errors (sb-ext:posix-getenv "HOME")) "~")))
    (is (string= (concatenate 'string home "/.tmux/plugins/tpm/tpm")
                 (cl-tmux/config::%expand-leading-tilde "~/.tmux/plugins/tpm/tpm"))
        "the full tpm path must expand its leading ~/ to $HOME")))

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
    (is (eq t (apply-config-directive (list "set-environment" "-u" name)))
        "set-environment -u must be handled (return T)")
    (is (null (sb-ext:posix-getenv name))
        "config set-environment -u must unset the variable")))

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

;;; ── set -g status off side-effect ────────────────────────────────────────────

(test apply-set-directive-status-off-sets-status-height-zero
  "'set -g status off' sets *status-height* to 0."
  (let ((orig cl-tmux/config:*status-height*))
    (unwind-protect
         (progn
           (cl-tmux/config:apply-config-directive '("set" "-g" "status" "off"))
           (is (= 0 cl-tmux/config:*status-height*)
               "*status-height* must be 0 after 'set -g status off'"))
      (setf cl-tmux/config:*status-height* orig))))

(test apply-set-directive-status-on-sets-status-height-one
  "'set -g status on' sets *status-height* to 1."
  (let ((orig cl-tmux/config:*status-height*))
    (unwind-protect
         (progn
           (setf cl-tmux/config:*status-height* 0)
           (cl-tmux/config:apply-config-directive '("set" "-g" "status" "on"))
           (is (= 1 cl-tmux/config:*status-height*)
               "*status-height* must be 1 after 'set -g status on'"))
      (setf cl-tmux/config:*status-height* orig))))

;;; ── bind -n with argument-bearing command ────────────────────────────────────

(test bind-key-n-split-window-with-c-flag
  "'bind -n C-\\ split-window -c /tmp' binds the control character ^\\ (byte 28)
   and stores the full command token list.  C-<key> tokens now resolve to the
   control CHARACTER the event loop sees (the old string-key form could never
   fire), so the binding is looked up by (code-char 28), not the string \"C-\\\"."
  (with-isolated-key-tables
    (cl-tmux/config:apply-config-directive
     '("bind" "-n" "C-\\" "split-window" "-c" "/tmp"))
    ;; C-\ → (logand (char-code #\\) #x1f) = 28 (FS).  The binding is keyed by
    ;; that control character so it matches the byte a real Ctrl-\ keypress sends.
    (let ((entry (cl-tmux/config:key-table-lookup "root" (code-char 28))))
      (is (not (null entry)) "C-\\ must be bound (as control char 28) in root table")
      (let ((cmd (cl-tmux/config:key-table-command entry)))
        (is (consp cmd) "command for multi-token bind must be a token list")
        (is (string= "split-window" (first cmd)) "first token must be split-window")
        (is (member "-c" cmd :test #'string=) "token list must include -c flag")
        (is (member "/tmp" cmd :test #'string=) "token list must include /tmp")))))

;;; ── Semicolon-separated multi-command bindings ───────────────────────────────

(test bind-key-semicolon-sequence-stored-as-sequence
  "'bind r source-file x \; display y' stores a :sequence command list."
  (with-isolated-key-tables
    (cl-tmux/config:apply-config-directive
     '("bind" "r" "source-file" "/tmp/x" ";" "display-message" "Reloaded"))
    (let ((entry (cl-tmux/config:key-table-lookup "prefix" #\r)))
      (is (not (null entry)) "#\\r must be bound in prefix table")
      (let ((cmd (cl-tmux/config:key-table-command entry)))
        (is (consp cmd) "command must be a cons")
        (is (eq :sequence (car cmd)) "first element must be :sequence")
        (is (= 2 (length (cdr cmd))) ":sequence must have 2 sub-command lists")
        (is (string= "source-file" (first (first (cdr cmd))))
            "first sub-command must start with source-file")
        (is (string= "display-message" (first (second (cdr cmd))))
            "second sub-command must start with display-message")))))

;;; ── Common .tmux.conf patterns ───────────────────────────────────────────────

(test load-config-common-patterns-no-error
  "Common .tmux.conf patterns load without error."
  (with-isolated-config
    (let ((common-config
           "set -g prefix C-a
set -g mouse on
set -g base-index 1
setw -g pane-base-index 1
set -g default-terminal \"screen-256color\"
set -g escape-time 0
set -g history-limit 50000
set -g renumber-windows on
set -g mode-keys vi
bind r source-file /dev/null \; display-message \"Reloaded\"
bind-key -T copy-mode-vi v send-keys -X begin-selection
bind-key -T copy-mode-vi y send-keys -X copy-selection-and-cancel
bind '\"' split-window -c #{pane_current_path}
bind % split-window -h -c #{pane_current_path}
bind c new-window -c #{pane_current_path}
unbind-all
bind-key r source-file /dev/null"))
      (is (zerop (multiple-value-bind (result)
                     (ignore-errors (cl-tmux/config:load-config-from-string common-config))
                   (declare (ignore result))
                   0))
          "common .tmux.conf patterns must load without signaling conditions"))))

(test load-config-bind-T-copy-mode-vi-stores-correctly
  "bind -T copy-mode-vi v send-keys -X begin-selection stores in copy-mode-vi table."
  (with-isolated-config
    (cl-tmux/config:load-config-from-string
     "bind-key -T copy-mode-vi v send-keys -X begin-selection")
    (let ((entry (cl-tmux/config:key-table-lookup "copy-mode-vi" #\v)))
      (is (not (null entry)) "copy-mode-vi must have 'v' binding after load")
      (let ((cmd (cl-tmux/config:key-table-command entry)))
        (is (consp cmd) "command must be a token list")
        (is (string= "send-keys" (first cmd)) "first token must be send-keys")))))

(test load-config-set-g-escape-time-stores-as-server-option
  "'set -s escape-time 0' stores in server options."
  (with-isolated-config
    (cl-tmux/config:load-config-from-string "set -s escape-time 0")
    (is (eql 0 (cl-tmux/options:get-server-option "escape-time"))
        "escape-time must be 0 after 'set -s escape-time 0'")))

;;; ── Bare arg-command abbreviations in bind (single-token path) ───────────────
;;;
;;; `bind X <abbrev> args` (multi-token) already works — it is stored unvalidated
;;; and resolved at dispatch via *arg-command-table*.  A BARE `bind X <abbrev>`
;;; (single token) instead goes through %command-keyword, so each arg-command
;;; abbreviation needs a *command-name-aliases* entry to be accepted.

(test config-bind-accepts-arg-command-abbreviations
  "Bare `bind X <abbrev>` is accepted for each arg-bearing command abbreviation."
  (dolist (abbrev '("capturep" "commandp" "deleteb" "has" "killw"
                    "lastp" "resizew" "selectw" "setb" "swapp"))
    (with-isolated-config
      (is (= 1 (cl-tmux/config:load-config-from-string
                (format nil "bind X ~A" abbrev)))
          "bind X ~A must apply (1 directive); the abbreviation must resolve"
          abbrev))))

(test config-bind-rejects-unknown-single-token-still
  "The abbreviation aliases do not weaken typo rejection: an unknown single-token
   command is still refused."
  (with-isolated-config
    (is (= 0 (cl-tmux/config:load-config-from-string "bind X totally-bogus-cmd"))
        "an unknown bare command must still be rejected (0 applied)")))

;;; ── %elif chains (4-state cond stack) ────────────────────────────────────────
;;;
;;; A plain skip flag mishandles %elif after a matched branch.  These exercise the
;;; :active/:seeking/:taken/:dead state machine.  The identity evaluator makes the
;;; condition "1" truthy and "0" falsy.

(test config-elif-not-taken-after-if-matched
  "%if 1 then %elif 1: only the if-branch applies — the elif must be skipped."
  (with-isolated-config
    (let ((cl-tmux/config:*config-condition-evaluator* (lambda (c) c)))
      (is (= 1 (cl-tmux/config:load-config-from-string
                (format nil "%if 1~%bind a new-window~%%elif 1~%bind b new-window~%%endif~%")))
          "if-true takes only the if-branch, not the following elif"))))

(test config-elif-taken-when-if-false
  "%if 0 / %elif 1: the elif-branch applies."
  (with-isolated-config
    (let ((cl-tmux/config:*config-condition-evaluator* (lambda (c) c)))
      (is (= 1 (cl-tmux/config:load-config-from-string
                (format nil "%if 0~%bind a new-window~%%elif 1~%bind b new-window~%%endif~%")))
          "elif applies when the if condition is false"))))

(test config-else-taken-when-if-and-elif-false
  "%if 0 / %elif 0 / %else: the else-branch applies."
  (with-isolated-config
    (let ((cl-tmux/config:*config-condition-evaluator* (lambda (c) c)))
      (is (= 1 (cl-tmux/config:load-config-from-string
                (format nil "%if 0~%bind a x~%%elif 0~%bind b x~%%else~%bind c new-window~%%endif~%")))
          "else applies when if and all elifs are false"))))

(test config-elif-chain-picks-first-true
  "%if 0 / %elif 0 / %elif 1 / %else: only the first true elif applies."
  (with-isolated-config
    (let ((cl-tmux/config:*config-condition-evaluator* (lambda (c) c)))
      (is (= 1 (cl-tmux/config:load-config-from-string
                (format nil "%if 0~%bind a x~%%elif 0~%bind b x~%%elif 1~%bind c new-window~%%else~%bind d x~%%endif~%")))
          "the first matching elif applies; later elif/else skipped"))))

(test config-if-true-skips-all-elif-and-else
  "%if 1 / %elif 1 / %else: only the if-branch applies."
  (with-isolated-config
    (let ((cl-tmux/config:*config-condition-evaluator* (lambda (c) c)))
      (is (= 1 (cl-tmux/config:load-config-from-string
                (format nil "%if 1~%bind a new-window~%%elif 1~%bind b x~%%else~%bind c x~%%endif~%")))
          "if-true takes only the if-branch; elif and else are skipped"))))

(test config-nested-if-dead-inside-false-branch
  "A nested %if/%elif inside a false outer block applies nothing."
  (with-isolated-config
    (let ((cl-tmux/config:*config-condition-evaluator* (lambda (c) c)))
      (is (= 0 (cl-tmux/config:load-config-from-string
                (format nil "%if 0~%%if 1~%bind a new-window~%%elif 1~%bind b new-window~%%endif~%%endif~%")))
          "nested if/elif in a dead branch stays dead"))))

;;; ── Line continuation (trailing backslash) ───────────────────────────────────

(test line-continues-p-counts-trailing-backslashes
  "%line-continues-p is T for an odd number of trailing backslashes."
  (is-true  (cl-tmux/config::%line-continues-p "foo \\"))
  (is-false (cl-tmux/config::%line-continues-p "foo \\\\"))
  (is-true  (cl-tmux/config::%line-continues-p "foo \\\\\\"))
  (is-false (cl-tmux/config::%line-continues-p "foo"))
  (is-false (cl-tmux/config::%line-continues-p "")))

(test config-line-continuation-joins-directive
  "A line ending in a single backslash continues onto the next line, forming one
   directive."
  (with-isolated-config
    (is (= 1 (cl-tmux/config:load-config-from-string
              (format nil "bind a \\~%new-window")))
        "the continued line is applied as a single bind directive")))

(test config-no-continuation-two-lines-two-directives
  "Without a trailing backslash, two lines remain two separate directives."
  (with-isolated-config
    (is (= 2 (cl-tmux/config:load-config-from-string
              (format nil "bind a new-window~%bind b next-window")))
        "two independent binds apply as two directives")))

;;; ── source-file: -q flags, glob patterns, multiple paths ─────────────────────

(test glob-expand-passthrough-non-glob
  "%glob-expand returns a non-glob path unchanged as a one-element list."
  (is (equal '("/etc/foo.conf") (cl-tmux/config::%glob-expand "/etc/foo.conf"))))

(test glob-expand-empty-for-no-matches
  "%glob-expand returns NIL for a glob that matches nothing."
  (is (null (cl-tmux/config::%glob-expand "/nonexistent-cl-tmux-xyz-dir/*.conf"))))

(test source-files-skips-flags-and-tolerates-missing
  "source-files skips -q/-n/-v flags and ignores missing files (returns T, no error)."
  (is (eq t (cl-tmux/config:source-files '("-q" "/no/such/cl-tmux-file.conf")))))

(test source-files-glob-expands-and-loads-matching-files
  "source-file with a glob loads every matching file; %glob-expand finds them."
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames "cl-tmux-glob-test/" (uiop:temporary-directory)))))
    (ensure-directories-exist dir)
    (unwind-protect
         (progn
           (with-open-file (f (merge-pathnames "a.conf" dir)
                              :direction :output :if-exists :supersede)
             (write-line "# cl-tmux glob test (no global mutation)" f))
           (with-open-file (f (merge-pathnames "b.conf" dir)
                              :direction :output :if-exists :supersede)
             (write-line "# cl-tmux glob test (no global mutation)" f))
           (let ((matches (cl-tmux/config::%glob-expand
                           (namestring (merge-pathnames "*.conf" dir)))))
             (is (= 2 (length matches)) "glob matched both .conf files (got ~A)" matches)
             (is (eq t (cl-tmux/config:source-files
                        (list (namestring (merge-pathnames "*.conf" dir)))))
                 "source-files loads the globbed files without error")))
      (ignore-errors (uiop:delete-directory-tree dir :validate t)))))

;;; ── # comment handling (inline, quote- and format-aware) ─────────────────────

(test strip-config-comment-respects-quotes-and-formats
  "%strip-config-comment removes a comment only outside quotes and not for #{/##."
  (is (string= "set -g foo bar"
               (cl-tmux/config::%strip-config-comment "set -g foo bar # note"))
      "inline comment stripped")
  (is (string= "" (cl-tmux/config::%strip-config-comment "# full line"))
      "full-line comment → empty")
  (is (string= "set x \"#{session_name}\""
               (cl-tmux/config::%strip-config-comment "set x \"#{session_name}\""))
      "# inside double quotes kept")
  (is (string= "set x '# literal'"
               (cl-tmux/config::%strip-config-comment "set x '# literal'"))
      "# inside single quotes kept")
  (is (string= "set x #{session_name}"
               (cl-tmux/config::%strip-config-comment "set x #{session_name}"))
      "unquoted #{ format kept")
  (is (string= "set x ##y"
               (cl-tmux/config::%strip-config-comment "set x ##y"))
      "## escaped-literal not a comment")
  (is (string= "set x bar"
               (cl-tmux/config::%strip-config-comment "set x bar"))
      "no comment → unchanged"))

(test config-inline-comment-not-in-value
  "An inline # comment is stripped before the directive is applied."
  (with-isolated-options ()
    (cl-tmux/config:load-config-from-string "set -g status-left foo # a comment")
    (is (string= "foo" (cl-tmux/options:get-option "status-left"))
        "the comment must not leak into the option value")))

(test config-quoted-hash-preserved-in-value
  "A #{...} inside a quoted value survives (not treated as a comment)."
  (with-isolated-options ()
    (cl-tmux/config:load-config-from-string "set -g status-left \"#{session_name}\"")
    (is (string= "#{session_name}" (cl-tmux/options:get-option "status-left"))
        "the #{...} format must survive as the option value")))

(test config-brace-block-inner-comment-preserved
  "An inline # comment on an inner line of a multi-line brace block does not
   truncate the block — both commands still bind."
  (with-isolated-config
    (load-config-from-string
     (format nil "bind r {~%new-window  # make a window~%next-window~%}"))
    (let* ((entry (cl-tmux/config:key-table-lookup "prefix" #\r))
           (cmd   (cl-tmux/config:key-table-command entry)))
      (is (consp cmd) "command must be a list")
      (is (eq :sequence (first cmd)) "must be a :sequence")
      (is (= 2 (length (rest cmd)))
          "the sequence must hold both commands despite the inner comment (got ~S)"
          cmd))))

;;; ── if-shell config directive: -F format conditions + flag stripping ─────────

(test if-shell-directive-shell-condition-runs-then
  "if-shell with a shell condition that exits 0 applies the THEN command."
  (with-isolated-config
    (cl-tmux/config::%apply-if-shell-directive
     "if-shell" '("true" "set -g status-left THEN" "set -g status-left ELSE"))
    (is (string= "THEN" (cl-tmux/options:get-option "status-left"))
        "exit-0 shell condition must run the THEN command")))

(test if-shell-directive-F-truthy-runs-then
  "if-shell -F with a non-empty, non-zero format runs the THEN command (the -F
   flag must be stripped, not treated as the condition)."
  (with-isolated-config
    (cl-tmux/config::%apply-if-shell-directive
     "if-shell" '("-F" "1" "set -g status-left THEN" "set -g status-left ELSE"))
    (is (string= "THEN" (cl-tmux/options:get-option "status-left"))
        "-F with truthy format must run THEN")))

(test if-shell-directive-F-zero-runs-else
  "if-shell -F with a \"0\" format is false → runs the ELSE command."
  (with-isolated-config
    (cl-tmux/config::%apply-if-shell-directive
     "if-shell" '("-F" "0" "set -g status-left THEN" "set -g status-left ELSE"))
    (is (string= "ELSE" (cl-tmux/options:get-option "status-left"))
        "-F with \"0\" must run ELSE")))

(test if-shell-directive-F-format-expression
  "if-shell -F evaluates a real format expression: #{==:a,a} → 1 → THEN."
  (with-isolated-config
    (cl-tmux/config::%apply-if-shell-directive
     "if-shell" '("-F" "#{==:a,a}" "set -g status-left THEN" "set -g status-left ELSE"))
    (is (string= "THEN" (cl-tmux/options:get-option "status-left"))
        "#{==:a,a} expands to 1 (true) → THEN")))

(test if-shell-directive-strips-background-flag
  "if-shell -b (background) is stripped; the shell condition still drives the
   branch rather than -b being parsed as the condition."
  (with-isolated-config
    (cl-tmux/config::%apply-if-shell-directive
     "if-shell" '("-b" "true" "set -g status-left THEN" "set -g status-left ELSE"))
    (is (string= "THEN" (cl-tmux/options:get-option "status-left"))
        "-b must be stripped, leaving the shell condition to choose THEN")))

(test if-shell-format-true-p-rules
  "%if-shell-format-true-p: empty and \"0\" are false; other text is true."
  (is-true  (cl-tmux/config::%if-shell-format-true-p "1"))
  (is-true  (cl-tmux/config::%if-shell-format-true-p "yes"))
  (is-false (cl-tmux/config::%if-shell-format-true-p "0"))
  (is-false (cl-tmux/config::%if-shell-format-true-p "")))

;;; ── if-shell brace-block then/else bodies (tmux 3.x { ... } syntax) ──────────

(test if-shell-directive-F-brace-then-runs-block
  "if-shell -F with a brace-block THEN runs the block when the condition is truthy."
  (with-isolated-config
    (cl-tmux/config::%apply-if-shell-directive
     "if-shell" '("-F" "1" "{" "set" "-g" "status-left" "THEN" "}"
                  "{" "set" "-g" "status-left" "ELSE" "}"))
    (is (string= "THEN" (cl-tmux/options:get-option "status-left"))
        "truthy -F runs the THEN brace block")))

(test if-shell-directive-F-brace-else-runs-block
  "if-shell -F 0 runs the ELSE brace block."
  (with-isolated-config
    (cl-tmux/config::%apply-if-shell-directive
     "if-shell" '("-F" "0" "{" "set" "-g" "status-left" "THEN" "}"
                  "{" "set" "-g" "status-left" "ELSE" "}"))
    (is (string= "ELSE" (cl-tmux/options:get-option "status-left"))
        "falsy -F runs the ELSE brace block")))

(test if-shell-directive-F-brace-multi-command
  "A brace THEN block with multiple ;-separated commands runs ALL of them."
  (with-isolated-config
    (cl-tmux/config::%apply-if-shell-directive
     "if-shell" '("-F" "1" "{" "set" "-g" "status-left" "A" ";"
                  "set" "-g" "status-right" "B" "}"))
    (is (string= "A" (cl-tmux/options:get-option "status-left")) "first cmd ran")
    (is (string= "B" (cl-tmux/options:get-option "status-right")) "second cmd ran")))

(test if-shell-directive-F-brace-no-else-is-safe
  "if-shell -F 0 with only a THEN brace block (no else) runs nothing — no error."
  (with-isolated-config
    (cl-tmux/config::%apply-if-shell-directive
     "if-shell" '("-F" "0" "{" "set" "-g" "status-left" "THEN" "}"))
    (is (not (string= "THEN" (cl-tmux/options:get-option "status-left")))
        "false condition + no else block → the THEN block does NOT run")))

(test take-brace-or-command-splits-block-and-bare
  "%take-brace-or-command: a { ... ; ... } block → inner command lists + rest; a
   bare token → one re-tokenised command + rest."
  (multiple-value-bind (cmds rest)
      (cl-tmux/config::%take-brace-or-command '("{" "a" "b" ";" "c" "d" "}" "tail"))
    (is (equal '(("a" "b") ("c" "d")) cmds) "two ;-separated commands extracted")
    (is (equal '("tail") rest) "rest is the tokens after the closing brace"))
  (multiple-value-bind (cmds rest)
      (cl-tmux/config::%take-brace-or-command '("set -g x Y" "more"))
    (is (equal '(("set" "-g" "x" "Y")) cmds) "bare token re-tokenised into one command")
    (is (equal '("more") rest) "rest is the remaining tokens")))

;;; ── run-shell -C : run a tmux command, not a shell command ───────────────────

(test run-shell-C-runs-tmux-command
  "run-shell -C 'set -g status-left FOO' runs the tmux command via the config
   dispatcher (was a documented no-op), and reports itself handled."
  (with-isolated-config
    (let ((handled (cl-tmux/config::%apply-run-shell-directive
                    "run-shell" '("-C" "set -g status-left FOO"))))
      (is (eq t handled) "run-shell must report handled")
      (is (string= "FOO" (cl-tmux/options:get-option "status-left"))
          "-C must execute the tmux command (set the option)"))))

(test run-shell-C-alias-run-also-works
  "The 'run' alias with -C executes the tmux command too."
  (with-isolated-config
    (cl-tmux/config::%apply-run-shell-directive
     "run" '("-C" "set -g status-right BAR"))
    (is (string= "BAR" (cl-tmux/options:get-option "status-right"))
        "run -C must execute the tmux command")))

;;; ── Mid-token '#' is a literal, not a comment ────────────────────────────────
;;;
;;; tmux's lexer only begins a comment when '#' is the first character of a token
;;; (line start or just after whitespace).  A '#' in the middle of an unquoted
;;; word — most commonly a hex colour like bg=#0000ff — is a literal character.
;;; Previously %strip-config-comment truncated such values to "bg=".

(test strip-config-comment-keeps-mid-token-hash
  "A '#' in the middle of an unquoted token is literal; only a token-start '#'
   begins a comment."
  (is (string= "set -g status-style bg=#0000ff"
               (cl-tmux/config::%strip-config-comment "set -g status-style bg=#0000ff"))
      "mid-token hex colour kept verbatim")
  (is (string= "set -g @c fg=#ff0000"
               (cl-tmux/config::%strip-config-comment "set -g @c fg=#ff0000"))
      "mid-token hex in a user (@) option kept")
  (is (string= "set -g status-style bg=#0000ff"
               (cl-tmux/config::%strip-config-comment
                "set -g status-style bg=#0000ff # trailing note"))
      "mid-token hex kept AND a real trailing comment still stripped")
  (is (string= "set -g foo"
               (cl-tmux/config::%strip-config-comment "set -g foo #bar"))
      "a '#' at a token start (after whitespace) still begins a comment"))

(test apply-config-line-keeps-hash-colour-end-to-end
  "End-to-end: an unquoted hex colour survives apply-config-line into the option
   value (it used to be truncated by comment stripping)."
  (with-isolated-options ("status-style" "")
    (cl-tmux/config::apply-config-line "set -g status-style bg=#1e1e1e")
    (is (string= "bg=#1e1e1e" (cl-tmux/options:get-option "status-style"))
        "hex colour reaches the option store intact")))

;;; ── set -ag on style options inserts a ',' separator ─────────────────────────
;;;
;;; tmux marks *-style options OPTIONS_TABLE_IS_STYLE; `set -a` appends to them
;;; with a comma so incremental theming (bg first, fg later) composes.  Plain
;;; string options keep separator-less concatenation.

(test apply-set-directive-append-style-comma
  "'set -ag <name>-style v' comma-joins; plain string options still concat."
  ;; Style option with a non-empty current value: comma-joined.
  (with-isolated-options ("status-style" "bg=red")
    (is (eq t (apply-config-directive '("set" "-ag" "status-style" "fg=blue")))
        "set -ag must return T")
    (is (string= "bg=red,fg=blue" (cl-tmux/options:get-option "status-style"))
        "style append must insert a ',' separator"))
  ;; Style option appended onto an empty value: no leading comma.
  (with-isolated-options ("mode-style" "")
    (apply-config-directive '("set" "-ag" "mode-style" "fg=green"))
    (is (string= "fg=green" (cl-tmux/options:get-option "mode-style"))
        "appending onto an empty style value adds no stray comma"))
  ;; Non-style string option: still plain (no comma).
  (with-isolated-options ("status-left" "A")
    (apply-config-directive '("set" "-ag" "status-left" "B"))
    (is (string= "AB" (cl-tmux/options:get-option "status-left"))
        "non-style option keeps separator-less concatenation")))
