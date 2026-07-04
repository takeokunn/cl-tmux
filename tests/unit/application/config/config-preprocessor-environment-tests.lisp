(in-package #:cl-tmux/test)

;;;; config directive tests — preprocessor, environment, and key-table side effects

(in-suite config-directives-suite)

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
    (with-temporary-posix-environment-variable (name "x")
      (is (string= "x" (sb-ext:posix-getenv name)) "precondition: var is set")
      (assert-config-directive-applied (list "set-environment" "-u" name)
                                       "set-environment -u")
      (is (null (sb-ext:posix-getenv name))
          "config set-environment -u must unset the variable"))))

(test apply-set-environment-t-writes-target-session
  "'set-environment -t target VAR VALUE' config directive writes the target session overlay."
  (let ((name "CLTMUX_TEST_ENV_VAR_CFG_T")
        (target-name "CLTMUX_TEST_ENV_TARGET_CFG_T"))
    (with-temporary-posix-environment-variable (name nil)
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
              "config set-environment -t must not touch the process environment"))))))

(test apply-set-environment-g-t-u-is-rejected
  "'set-environment -g -t target -u VAR' config directive is rejected."
  (let ((name "CLTMUX_TEST_ENV_VAR_CFG_GT_U"))
    (with-temporary-posix-environment-variable (name "x")
      (assert-config-directive-rejected (list "set-environment" "-g" "-t" "ignored" "-u" name)
                                        "set-environment -g -t target -u")
      (is (string= "x" (sb-ext:posix-getenv name))
          "config set-environment -g -t target -u must not unset VAR"))))

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

;;; ── %update-config-cond-stack unit tests ─────────────────────────────────
;;;
;;; These test the four-state machine (:active / :seeking / :taken / :dead)
;;; used by load-config-from-stream to process %if/%elif/%else/%endif blocks.
;;; Each test drives the helper directly so the state transitions are clear.

(test update-config-cond-stack-if-pushes-active-when-truthy
  "%if on an empty stack with a truthy condition pushes :active."
  (let ((cl-tmux/config:*config-condition-evaluator* (lambda (s) (declare (ignore s)) "1")))
    (let ((stack (cl-tmux/config::%update-config-cond-stack :if "%if 1" nil)))
      (is (equal '(:active) stack)
          "truthy %if must push :active onto an empty stack"))))

(test update-config-cond-stack-if-pushes-seeking-when-falsy
  "%if on an empty stack with a falsy condition pushes :seeking."
  (let ((cl-tmux/config:*config-condition-evaluator* (lambda (s) (declare (ignore s)) "0")))
    (let ((stack (cl-tmux/config::%update-config-cond-stack :if "%if 0" nil)))
      (is (equal '(:seeking) stack)
          "falsy %if must push :seeking (no branch matched yet)"))))

(test update-config-cond-stack-elif-active-becomes-taken
  "%elif when current state is :active transitions to :taken (already matched)."
  ;; A branch was active → following %elif must skip (state = :taken).
  (let ((cl-tmux/config:*config-condition-evaluator* (lambda (s) (declare (ignore s)) "1")))
    (let ((stack (cl-tmux/config::%update-config-cond-stack :elif "%elif 1" '(:active))))
      (is (equal '(:taken) stack)
          ":active → %elif must transition to :taken"))))

(test update-config-cond-stack-elif-taken-stays-taken
  "%elif when current state is :taken stays :taken (already consumed one branch)."
  (let ((cl-tmux/config:*config-condition-evaluator* (lambda (s) (declare (ignore s)) "1")))
    (let ((stack (cl-tmux/config::%update-config-cond-stack :elif "%elif 1" '(:taken))))
      (is (equal '(:taken) stack)
          ":taken → %elif must remain :taken"))))

(test update-config-cond-stack-elif-dead-stays-dead
  "%elif when current state is :dead stays :dead (outer block is skipped)."
  (let ((cl-tmux/config:*config-condition-evaluator* (lambda (s) (declare (ignore s)) "1")))
    (let ((stack (cl-tmux/config::%update-config-cond-stack :elif "%elif 1" '(:dead))))
      (is (equal '(:dead) stack)
          ":dead → %elif must remain :dead (outer block controls)"))))

(test update-config-cond-stack-else-seeking-becomes-active
  "%else when current state is :seeking transitions to :active."
  (let ((stack (cl-tmux/config::%update-config-cond-stack :else "%else" '(:seeking))))
    (is (equal '(:active) stack)
        ":seeking → %else must transition to :active")))

(test update-config-cond-stack-else-active-becomes-taken
  "%else when current state is :active transitions to :taken."
  (let ((stack (cl-tmux/config::%update-config-cond-stack :else "%else" '(:active))))
    (is (equal '(:taken) stack)
        ":active → %else must transition to :taken")))

(test update-config-cond-stack-else-taken-stays-taken
  "%else when current state is :taken stays :taken."
  (let ((stack (cl-tmux/config::%update-config-cond-stack :else "%else" '(:taken))))
    (is (equal '(:taken) stack)
        ":taken → %else must remain :taken")))

(test update-config-cond-stack-else-dead-stays-dead
  "%else when current state is :dead stays :dead."
  (let ((stack (cl-tmux/config::%update-config-cond-stack :else "%else" '(:dead))))
    (is (equal '(:dead) stack)
        ":dead → %else must remain :dead")))

(test update-config-cond-stack-endif-pops-state
  "%endif pops the innermost state from the stack."
  (let ((stack (cl-tmux/config::%update-config-cond-stack :endif "%endif" '(:active :seeking))))
    (is (equal '(:seeking) stack)
        "%endif must pop the top state, leaving the outer level; got ~S" stack))
  (let ((stack (cl-tmux/config::%update-config-cond-stack :endif "%endif" '(:taken))))
    (is (equal '() stack)
        "%endif on a single-element stack must yield NIL (empty); got ~S" stack)))

(test update-config-cond-stack-nested-if-dead-when-outer-seeking
  "A nested %if when the outer level is :seeking pushes :dead (not evaluated)."
  (let ((cl-tmux/config:*config-condition-evaluator* (lambda (s) (declare (ignore s)) "1")))
    ;; Outer block is :seeking (no branch matched yet) → inner %if must push :dead.
    (let ((stack (cl-tmux/config::%update-config-cond-stack :if "%if 1" '(:seeking))))
      (is (equal '(:dead :seeking) stack)
          "nested %if inside :seeking must push :dead; got ~S" stack))))

;;; ── set -o (only-if-unset) on the config-load path ───────────────────────────

(test config-set-o-only-if-unset-enforced
  "Config-time `set -og` skips an already-present option and writes an absent
   one (tmux cmd-set-option -o semantics on the load path)."
  (with-fresh-global-options
    (is (eq t (apply-config-directive '("set" "-og" "@cfg-o" "first")))
        "first set -og on an unset user option must apply")
    (is (string= "first" (cl-tmux/options:get-option "@cfg-o"))
        "the first value must be stored")
    (is (eq t (apply-config-directive '("set" "-og" "@cfg-o" "second")))
        "second set -og is handled (tmux reports already-set and skips)")
    (is (string= "first" (cl-tmux/options:get-option "@cfg-o"))
        "the existing value must be left untouched")
    (is (eq t (apply-config-directive '("set" "-g" "@cfg-o" "third")))
        "a plain set without -o must still overwrite")
    (is (string= "third" (cl-tmux/options:get-option "@cfg-o"))
        "plain set must overwrite the value")))

;;; ── tmux 3.2 config variable assignment lines ────────────────────────────────

(test config-variable-assignment-lines
  "`NAME=value` config lines set a global environment variable resolvable as
   #{NAME}; `%hidden NAME=value` additionally hides it from child processes."
  (with-isolated-config
    (let ((cl-tmux/model:*global-hidden-environment-names* nil))
      (unwind-protect
           (progn
             ;; Plain assignment: env set + format-resolvable.
             (is (eq t (apply-config-directive '("CLTMUX_CFGVAR=vidal")))
                 "a NAME=value line must be handled")
             (is (string= "vidal" (sb-ext:posix-getenv "CLTMUX_CFGVAR"))
                 "the assignment must reach the global environment")
             (is (string= "prefix-vidal"
                          (cl-tmux/format:expand-format "prefix-#{CLTMUX_CFGVAR}" '()))
                 "#{NAME} must resolve via the format environment fallback")
             ;; %hidden assignment: set + hidden from child envs.
             (is (eq t (apply-config-directive '("%hidden" "CLTMUX_CFGHID=shh")))
                 "a %hidden NAME=value line must be handled")
             (is (member "CLTMUX_CFGHID"
                         cl-tmux/model:*global-hidden-environment-names*
                         :test #'string=)
                 "%hidden must mark the variable hidden")
             ;; Multi-token lines are NOT assignments.
             (is (null (apply-config-directive '("FOO=bar" "extra")))
                 "a multi-token line must not be treated as an assignment"))
        (cl-tmux/model:process-unset-environment "CLTMUX_CFGVAR")
        (cl-tmux/model:process-unset-environment "CLTMUX_CFGHID")))))

(test tmux-standard-short-aliases-stay-unresolved
  "The standard tmux cmd_table short aliases are not accepted as cl-tmux command
   names.  Each row: (alias canonical)."
  (dolist (row '(("confirm" "confirm-before") ("kills" "kill-session")
                 ("next" "next-window")       ("prev" "previous-window")
                 ("nextl" "next-layout")      ("prevl" "previous-layout")
                 ("pipe" "pipe-pane")         ("pipep" "pipe-pane")
                 ("refresh" "refresh-client") ("rename" "rename-session")
                 ("rotatew" "rotate-window")  ("selectl" "select-layout")
                 ("showenv" "show-environment")
                 ("showmsgs" "show-messages") ("unlinkw" "unlink-window")
                 ("newp" "split-window")))
    (destructuring-bind (alias canonical) row
      (declare (ignore canonical))
      (is (string= alias (cl-tmux/config::%canonical-command-name alias))
          "~A must stay unresolved" alias)
      (is (null (cl-tmux/config::%known-command-name-p alias))
          "~A must not be known" alias))))
