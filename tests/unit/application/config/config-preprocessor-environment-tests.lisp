(in-package #:cl-tmux/test)

;;;; config directive tests — preprocessor, environment, and key-table side effects

(describe "config-directives-suite"

  ;;; ── %if / %else / %endif preprocessor ───────────────────────────────────

  ;; %if with a truthy condition applies the then-block and skips the else-block.
  (it "if-else-endif-truthy-condition"
    (with-isolated-config
      ;; *config-condition-evaluator* is NIL by default → all conditions truthy.
      (let ((applied (load-config-from-string
                      (format nil "%if 1~%bind z new-window~%%else~%bind z detach~%%endif~%"))))
        (expect (= 1 applied))
        (expect (eq :new-window (lookup-key-binding #\z))))))

  ;; %if with a falsy condition skips the then-block and applies the else-block.
  (it "if-else-endif-falsy-condition"
    (with-isolated-config
      ;; Set evaluator to return '0' (falsy) for any condition.
      (let ((cl-tmux/config:*config-condition-evaluator*
              (lambda (s) (declare (ignore s)) "0")))
        (let ((applied (load-config-from-string
                        (format nil "%if 0~%bind z new-window~%%else~%bind z detach~%%endif~%"))))
          (expect (= 1 applied))
          (expect (eq :detach (lookup-key-binding #\z)))))))

  ;; %if without %else applies the block when truthy, applies nothing when falsy.
  (it "if-endif-no-else"
    (with-isolated-config
      ;; Truthy (default evaluator NIL → all truthy)
      (let ((applied (load-config-from-string
                      (format nil "%if 1~%bind z new-window~%%endif~%"))))
        (expect (= 1 applied)))
      ;; Falsy
      (let ((cl-tmux/config:*config-condition-evaluator*
              (lambda (s) (declare (ignore s)) "0")))
        (let ((applied (load-config-from-string
                        (format nil "%if 0~%bind w detach~%%endif~%"))))
          (expect (= 0 applied))))))

  ;; Lines outside %if blocks are always applied regardless of evaluator.
  (it "if-block-outside-applies-normally"
    (with-isolated-config
      (let ((applied (load-config-from-string
                      (format nil "bind z new-window~%%if 1~%bind n next-window~%%endif~%bind p prev-window~%"))))
        (expect (= 3 applied)))))

  ;; Nested %if blocks work: inner block is skipped when outer is falsy.
  (it "nested-if-blocks"
    (with-isolated-config
      (let ((cl-tmux/config:*config-condition-evaluator*
              (lambda (s) (declare (ignore s)) "0")))
        (let ((applied (load-config-from-string
                        (format nil "%if 0~%%if 1~%bind z new-window~%%endif~%%endif~%"))))
          (expect (= 0 applied)))))
    ;; All truthy
    (with-isolated-config
      (let ((applied (load-config-from-string
                      (format nil "%if 1~%%if 1~%bind z new-window~%%endif~%%endif~%"))))
        (expect (= 1 applied)))))

  ;; %if condition string is passed verbatim to *config-condition-evaluator*.
  (it "if-condition-evaluated-by-callback"
    (with-isolated-config
      (let ((received nil))
        (let ((cl-tmux/config:*config-condition-evaluator*
                (lambda (s) (setf received s) "1")))
          (load-config-from-string (format nil "%if some-condition~%bind z new-window~%%endif~%"))
          (expect (string= "some-condition" received))))))

  ;;; ── %tmux-conf-paths ─────────────────────────────────────────────────────

  ;; %tmux-conf-paths returns a list of pathname candidates.
  (it "tmux-conf-paths-returns-list"
    (let ((paths (cl-tmux/config::%tmux-conf-paths #p"/home/user/")))
      (expect (listp paths))
      (expect (>= (length paths) 1))))

  ;;; ── config-file-path (environment-variable reading path) ─────────────────
  ;;;
  ;;; These tests exercise config-file-path by temporarily overriding
  ;;; environment variables.  Since posix-getenv reads the real environment
  ;;; and we cannot setenv from Lisp portably in tests, we test the pure
  ;;; %config-path-from helper directly (already covered above) and only
  ;;; verify that config-file-path returns a pathname (not NIL) from the
  ;;; live environment.

  ;; config-file-path returns a pathname object (not NIL or a string).
  (it "config-file-path-returns-pathname"
    (let ((result (config-file-path)))
      (expect (pathnamep result))))

  ;;; ── set-environment -u (unset) ───────────────────────────────────────────

  ;; 'set-environment -u VAR' config directive unsets the variable (tmux unset flag).
  (it "apply-set-environment-u-unsets-variable"
    (let ((name "CLTMUX_TEST_ENV_VAR_CFG"))
      (with-temporary-posix-environment-variable (name "x")
        (expect (string= "x" (sb-ext:posix-getenv name)))
        (assert-config-directive-applied (list "set-environment" "-u" name)
                                         "set-environment -u")
        (expect (null (sb-ext:posix-getenv name))))))

  ;; 'set-environment -t target VAR VALUE' config directive writes the target session overlay.
  (it "apply-set-environment-t-writes-target-session"
    (let ((name "CLTMUX_TEST_ENV_VAR_CFG_T")
          (target-name "CLTMUX_TEST_ENV_TARGET_CFG_T"))
      (with-temporary-posix-environment-variable (name nil)
        (let ((target (make-fake-session :nwindows 1 :npanes 1)))
          (with-registered-sessions ((target-name target))
            (assert-config-directive-applied (list "set-environment" "-t" target-name name "value")
                                             "set-environment -t")
            (multiple-value-bind (value source)
                (cl-tmux/model:session-environment-value target name)
              (expect (string= "value" value))
              (expect (eq :session source)))
            (expect (null (sb-ext:posix-getenv name))))))))

  ;; 'set-environment -g -t target -u VAR' config directive is rejected.
  (it "apply-set-environment-g-t-u-is-rejected"
    (let ((name "CLTMUX_TEST_ENV_VAR_CFG_GT_U"))
      (with-temporary-posix-environment-variable (name "x")
        (assert-config-directive-rejected (list "set-environment" "-g" "-t" "ignored" "-u" name)
                                          "set-environment -g -t target -u")
        (expect (string= "x" (sb-ext:posix-getenv name))))))

  ;;; ── %apply-option-side-effects: prefix branch ────────────────────────────
  ;;;
  ;;; Tests that "set-option -g prefix C-a" updates *prefix-key-code* and registers
  ;;; the new key in the prefix table (the prefix2 branch has no separate
  ;;; integration path into tests, so we cover the scalar + key-table path here).

  ;; 'set-option -g prefix C-a' updates *prefix-key-code* to 1 and binds the new key.
  (it "apply-set-directive-prefix-side-effect"
    (with-isolated-key-tables
      (let ((cl-tmux/config:*prefix-key-code* cl-tmux/config:+prefix-key-code+))
        (apply-config-directive '("set-option" "-g" "prefix" "C-a"))
        (expect (= 1 cl-tmux/config:*prefix-key-code*))
        (let ((entry (cl-tmux/config:key-table-lookup "prefix" (code-char 1))))
          (expect (not (null entry)))
          (expect (eq :send-prefix (cl-tmux/config:key-table-command entry)))))))

  ;;; ── unbind-all directive ─────────────────────────────────────────────────────

  ;; 'unbind-all' removes all bindings from the prefix key-table.
  (it "apply-config-directive-unbind-all-clears-prefix-table"
    (with-isolated-key-tables
      ;; Verify there's at least one binding first (e.g. C-c = :new-window).
      (let ((before (cl-tmux/config:key-table-lookup "prefix" #\c)))
        (expect (not (null before))))
      ;; Now clear it.
      (cl-tmux/config:apply-config-directive '("unbind-all"))
      ;; All bindings in prefix table should be gone.
      (expect (null (cl-tmux/config:key-table-lookup "prefix" #\c)))))

  ;; 'unbind-all -T root' removes all bindings from the root key-table.
  (it "apply-config-directive-unbind-all-T-clears-named-table"
    (with-isolated-key-tables
      ;; Bind something in root.
      (cl-tmux/config:key-table-bind "root" #\x :new-window)
      (cl-tmux/config:apply-config-directive '("unbind-all" "-T" "root"))
      (expect (null (cl-tmux/config:key-table-lookup "root" #\x)))))

  ;;; ── %update-config-cond-stack unit tests ─────────────────────────────────
  ;;;
  ;;; These test the four-state machine (:active / :seeking / :taken / :dead)
  ;;; used by load-config-from-stream to process %if/%elif/%else/%endif blocks.
  ;;; Each test drives the helper directly so the state transitions are clear.

  ;; %if on an empty stack with a truthy condition pushes :active.
  (it "update-config-cond-stack-if-pushes-active-when-truthy"
    (let ((cl-tmux/config:*config-condition-evaluator* (lambda (s) (declare (ignore s)) "1")))
      (let ((stack (cl-tmux/config::%update-config-cond-stack :if "%if 1" nil)))
        (expect (equal '(:active) stack)))))

  ;; %if on an empty stack with a falsy condition pushes :seeking.
  (it "update-config-cond-stack-if-pushes-seeking-when-falsy"
    (let ((cl-tmux/config:*config-condition-evaluator* (lambda (s) (declare (ignore s)) "0")))
      (let ((stack (cl-tmux/config::%update-config-cond-stack :if "%if 0" nil)))
        (expect (equal '(:seeking) stack)))))

  ;; %elif when current state is :active transitions to :taken (already matched).
  (it "update-config-cond-stack-elif-active-becomes-taken"
    ;; A branch was active → following %elif must skip (state = :taken).
    (let ((cl-tmux/config:*config-condition-evaluator* (lambda (s) (declare (ignore s)) "1")))
      (let ((stack (cl-tmux/config::%update-config-cond-stack :elif "%elif 1" '(:active))))
        (expect (equal '(:taken) stack)))))

  ;; %elif when current state is :taken stays :taken (already consumed one branch).
  (it "update-config-cond-stack-elif-taken-stays-taken"
    (let ((cl-tmux/config:*config-condition-evaluator* (lambda (s) (declare (ignore s)) "1")))
      (let ((stack (cl-tmux/config::%update-config-cond-stack :elif "%elif 1" '(:taken))))
        (expect (equal '(:taken) stack)))))

  ;; %elif when current state is :dead stays :dead (outer block is skipped).
  (it "update-config-cond-stack-elif-dead-stays-dead"
    (let ((cl-tmux/config:*config-condition-evaluator* (lambda (s) (declare (ignore s)) "1")))
      (let ((stack (cl-tmux/config::%update-config-cond-stack :elif "%elif 1" '(:dead))))
        (expect (equal '(:dead) stack)))))

  ;; %else when current state is :seeking transitions to :active.
  (it "update-config-cond-stack-else-seeking-becomes-active"
    (let ((stack (cl-tmux/config::%update-config-cond-stack :else "%else" '(:seeking))))
      (expect (equal '(:active) stack))))

  ;; %else when current state is :active transitions to :taken.
  (it "update-config-cond-stack-else-active-becomes-taken"
    (let ((stack (cl-tmux/config::%update-config-cond-stack :else "%else" '(:active))))
      (expect (equal '(:taken) stack))))

  ;; %else when current state is :taken stays :taken.
  (it "update-config-cond-stack-else-taken-stays-taken"
    (let ((stack (cl-tmux/config::%update-config-cond-stack :else "%else" '(:taken))))
      (expect (equal '(:taken) stack))))

  ;; %else when current state is :dead stays :dead.
  (it "update-config-cond-stack-else-dead-stays-dead"
    (let ((stack (cl-tmux/config::%update-config-cond-stack :else "%else" '(:dead))))
      (expect (equal '(:dead) stack))))

  ;; %endif pops the innermost state from the stack.
  (it "update-config-cond-stack-endif-pops-state"
    (let ((stack (cl-tmux/config::%update-config-cond-stack :endif "%endif" '(:active :seeking))))
      (expect (equal '(:seeking) stack)))
    (let ((stack (cl-tmux/config::%update-config-cond-stack :endif "%endif" '(:taken))))
      (expect (equal '() stack))))

  ;; A nested %if when the outer level is :seeking pushes :dead (not evaluated).
  (it "update-config-cond-stack-nested-if-dead-when-outer-seeking"
    (let ((cl-tmux/config:*config-condition-evaluator* (lambda (s) (declare (ignore s)) "1")))
      ;; Outer block is :seeking (no branch matched yet) → inner %if must push :dead.
      (let ((stack (cl-tmux/config::%update-config-cond-stack :if "%if 1" '(:seeking))))
        (expect (equal '(:dead :seeking) stack)))))

  ;;; ── set-option -o (only-if-unset) on the config-load path ───────────────────────────

  ;; Config-time `set-option -og` skips an already-present option and writes an absent
  ;; one (tmux cmd-set-option -o semantics on the load path).
  (it "config-set-o-only-if-unset-enforced"
    (with-fresh-global-options
      (expect (eq t (apply-config-directive '("set-option" "-og" "@cfg-o" "first"))))
      (expect (string= "first" (cl-tmux/options:get-option "@cfg-o")))
      (expect (eq t (apply-config-directive '("set-option" "-og" "@cfg-o" "second"))))
      (expect (string= "first" (cl-tmux/options:get-option "@cfg-o")))
      (expect (eq t (apply-config-directive '("set-option" "-g" "@cfg-o" "third"))))
      (expect (string= "third" (cl-tmux/options:get-option "@cfg-o")))))

  ;;; ── tmux 3.2 config variable assignment lines ────────────────────────────────

  ;; `NAME=value` config lines set a global environment variable resolvable as
  ;; #{NAME}; `%hidden NAME=value` additionally hides it from child processes.
  (it "config-variable-assignment-lines"
    (with-isolated-config
      (let ((cl-tmux/model:*global-hidden-environment-names* nil))
        (unwind-protect
             (progn
               ;; Plain assignment: env set + format-resolvable.
               (expect (eq t (apply-config-directive '("CLTMUX_CFGVAR=vidal"))))
               (expect (string= "vidal" (sb-ext:posix-getenv "CLTMUX_CFGVAR")))
               (expect (string= "prefix-vidal"
                                (cl-tmux/format:expand-format "prefix-#{CLTMUX_CFGVAR}" '())))
               ;; %hidden assignment: set + hidden from child envs.
               (expect (eq t (apply-config-directive '("%hidden" "CLTMUX_CFGHID=shh"))))
               (expect (member "CLTMUX_CFGHID"
                               cl-tmux/model:*global-hidden-environment-names*
                               :test #'string=))
               ;; Multi-token lines are NOT assignments.
               (expect (null (apply-config-directive '("FOO=bar" "extra")))))
          (cl-tmux/model:process-unset-environment "CLTMUX_CFGVAR")
          (cl-tmux/model:process-unset-environment "CLTMUX_CFGHID")))))

  ;; The config loader accepts only the canonical set-environment command name.
  (it "set-environment-short-alias-is-rejected"
    (with-isolated-config
      (unwind-protect
           (progn
             (assert-config-directive-rejected
              '("setenv" "CLTMUX_CFG_ALIAS_PROBE" "forbidden")
              "setenv alias")
             (expect (null (sb-ext:posix-getenv "CLTMUX_CFG_ALIAS_PROBE"))))
        (cl-tmux/model:process-unset-environment "CLTMUX_CFG_ALIAS_PROBE"))))

  ;; The standard tmux cmd_table short aliases are not accepted as cl-tmux command
  ;; names.
  (it "tmux-standard-short-aliases-stay-unresolved"
    (dolist (alias '("confirm" "kills" "next" "prev" "nextl" "prevl"
                     "pipe" "pipep" "refresh" "rename" "rotatew" "selectl"
                     "showenv" "showmsgs" "unlinkw" "newp"))
      (expect (null (cl-tmux/config::%known-command-name-p alias))))))
