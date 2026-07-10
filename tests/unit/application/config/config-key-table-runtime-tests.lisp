(in-package #:cl-tmux/test)

(in-suite config-suite)

;;;; Runtime key-table state, numeric defaults, and default shell tests.

;;; ── Import the config symbols we need ────────────────────────────────────

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(cl-tmux/config:lookup-key-binding
            cl-tmux/config:describe-key-bindings
            cl-tmux/config:+prefix-key-code+
            cl-tmux/config:+max-scrollback-lines+
            cl-tmux/config:+poll-timeout-us+
            cl-tmux/config:+accept-timeout-us+
            cl-tmux/config:+pty-buf-size+
            cl-tmux/config:+pty-poll-timeout-us+)))

;;; ── +max-scrollback-lines+ constant ───────────────────────────────────────

(test max-scrollback-lines-constant
  "+max-scrollback-lines+ equals 1000."
  (is (= 1000 +max-scrollback-lines+)
      "+max-scrollback-lines+ must be 1000, got ~A" +max-scrollback-lines+))

;;; ── Key-table system tests ────────────────────────────────────────────────

(test key-tables-initialized
  "*key-tables* is populated after load."
  (is (hash-table-p cl-tmux/config:*key-tables*)
      "*key-tables* must be a hash-table"))

(test key-tables-required-tables-exist
  "The standard key-tables are created by initialize-default-key-tables."
  (dolist (name '("prefix" "root" "copy-mode" "copy-mode-vi"))
    (is (not (null (gethash name cl-tmux/config:*key-tables*)))
        "\"~A\" table must exist in *key-tables*" name)))

(test key-table-bind-table
  "key-table-bind stores a binding retrievable by key-table-lookup in both 'root' and 'prefix' tables."
  (dolist (row '(("root"   #\a "root table binding")
                 ("prefix" #\c "prefix table binding")))
    (destructuring-bind (table-name key desc) row
      (with-isolated-key-tables
        (cl-tmux/config:key-table-bind table-name key :new-window)
        (let ((entry (cl-tmux/config:key-table-lookup table-name key)))
          (is (not (null entry)) "~A: binding must be found" desc)
          (is (eq :new-window (cl-tmux/config:key-table-command entry))
              "~A: command must be :new-window" desc))))))

(test key-table-repeatable-flag-variants
  "key-table-bind with :repeatable T marks the entry repeatable; without the flag
   it defaults to not repeatable.  Each row: (key cmd rep expected description)."
  (dolist (row '((#\r :resize-left t   t   ":repeatable T must be repeatable")
                 (#\c :new-window  nil nil "no :repeatable flag must not be repeatable")))
    (destructuring-bind (key cmd rep expected desc) row
      (with-isolated-key-tables
        (cl-tmux/config:key-table-bind "prefix" key cmd :repeatable rep)
        (let ((entry (cl-tmux/config:key-table-lookup "prefix" key)))
          (is (not (null entry)) "binding must be found: ~A" desc)
          (is (eql expected (cl-tmux/config:key-table-repeatable-p entry))
              desc))))))

(test key-table-lookup-missing-returns-nil
  "key-table-lookup returns NIL for an absent key and for a non-existent table."
  (let ((cl-tmux/config:*key-tables* (make-hash-table :test #'equal)))
    (is (null (cl-tmux/config:key-table-lookup "prefix"      #\z))
        "absent key must return NIL")
    (is (null (cl-tmux/config:key-table-lookup "nonexistent" #\a))
        "absent table must return NIL")))

;;; ── key-table-repeatable-p nil-safe guard ─────────────────────────────────

(test key-table-repeatable-p-nil-safe
  "key-table-repeatable-p returns NIL when passed NIL (nil-safe guard)."
  (is (null (cl-tmux/config:key-table-repeatable-p nil))
      "key-table-repeatable-p NIL must return NIL without signaling"))

(test key-table-command-nil-safe
  "key-table-command is the car of the entry; key-table-repeatable-p is nil-safe."
  (with-isolated-key-tables
    ;; Absent key returns NIL; nil-safe guard means no error.
    (let ((absent (cl-tmux/config:key-table-lookup "prefix" #\@)))
      (is (null absent) "absent key must return NIL")
      (is (null (cl-tmux/config:key-table-repeatable-p absent))
          "key-table-repeatable-p on NIL must return NIL"))))

(test numeric-constants
  "Timeout and buffer-size constants have the expected values and are all positive."
  (dolist (row `((,+poll-timeout-us+     50000  "+poll-timeout-us+")
                 (,+accept-timeout-us+   100000 "+accept-timeout-us+")
                 (,+pty-buf-size+        4096   "+pty-buf-size+")
                 (,+pty-poll-timeout-us+ 50000  "+pty-poll-timeout-us+")))
    (destructuring-bind (value expected name) row
      (is (= expected value)  "~A must be ~A" name expected)
      (is (plusp value)       "~A must be positive" name))))

;;; ── ensure-key-table side effects ────────────────────────────────────────

(test ensure-key-table-creates-new-table
  "ensure-key-table creates a fresh hash-table for a previously unknown name."
  (with-isolated-key-tables
    (let ((tbl (cl-tmux/config:ensure-key-table "my-table")))
      (is (hash-table-p tbl)
          "ensure-key-table must return a hash-table")
      (is (eq tbl (gethash "my-table" cl-tmux/config:*key-tables*))
          "the returned table must be stored in *key-tables*"))))

(test ensure-key-table-returns-existing-table
  "ensure-key-table returns the same table on repeated calls."
  (with-isolated-key-tables
    (let* ((tbl1 (cl-tmux/config:ensure-key-table "my-table"))
           (tbl2 (cl-tmux/config:ensure-key-table "my-table")))
      (is (eq tbl1 tbl2)
          "ensure-key-table must return the same object on repeated calls"))))

;;; ── lookup-key-binding on digit keys ────────────────────────────────────

(test lookup-digit-keys-bind-select-window
  "The digit characters 0-9 all bind :select-window in the default prefix table."
  (dolist (d '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
    (is (eq :select-window (lookup-key-binding d))
        "digit ~C must be bound to :select-window" d)))

;;; ── send-prefix binding ──────────────────────────────────────────────────

(test send-prefix-binding
  "The prefix key itself (code-char 2 = C-b) is bound to :send-prefix."
  (is (eq :send-prefix (lookup-key-binding (code-char +prefix-key-code+)))
      "C-b (prefix key) must be bound to :send-prefix"))

;;; ── describe-key-bindings header ────────────────────────────────────────

(test describe-key-bindings-has-header
  "describe-key-bindings output uses bind-key -T table format (real tmux list-keys format)."
  (let ((text (describe-key-bindings)))
    (is (search "bind-key" text)
        "output must contain 'bind-key' (real tmux list-keys format)")
    (is (search "-T" text)
        "output must contain '-T' (table specifier)")))

;;; ── initialize-default-key-tables idempotency ─────────────────────────────

(test initialize-default-key-tables-idempotent
  "Calling initialize-default-key-tables twice does not duplicate bindings."
  (with-isolated-key-tables
    (cl-tmux/config::initialize-default-key-tables)
    (let* ((tbl-after-first  (cl-tmux/config:ensure-key-table "prefix"))
           (count-after-first (hash-table-count tbl-after-first)))
      (cl-tmux/config::initialize-default-key-tables)
      (let ((count-after-second (hash-table-count tbl-after-first)))
        (is (= count-after-first count-after-second)
            "prefix table must not grow on second initialize-default-key-tables call")))
    (is (not (null (gethash "root"      cl-tmux/config:*key-tables*)))
        "\"root\" table must exist after double initialization")
    (is (not (null (gethash "copy-mode" cl-tmux/config:*key-tables*)))
        "\"copy-mode\" table must exist after double initialization")
    (is (not (null (gethash "copy-mode-vi" cl-tmux/config:*key-tables*)))
        "\"copy-mode-vi\" table must exist after double initialization")))

;;; ── Key-table name constants ──────────────────────────────────────────────

(test table-name-constants
  "Standard key-table constants have their expected string values."
  (check-table `((,cl-tmux/config:+table-prefix+       "prefix"       "+table-prefix+")
                 (,cl-tmux/config:+table-root+         "root"         "+table-root+")
                 (,cl-tmux/config:+table-copy-mode+    "copy-mode"    "+table-copy-mode+")
                 (,cl-tmux/config:+table-copy-mode-vi+ "copy-mode-vi" "+table-copy-mode-vi+"))
              :test #'string=))

;;; ── *default-shell* and *status-height* initial values ───────────────────

(test default-shell-is-string
  "*default-shell* is a non-empty string (set from $SHELL or /bin/sh)."
  (is (stringp cl-tmux/config:*default-shell*)
      "*default-shell* must be a string")
  (is (plusp (length cl-tmux/config:*default-shell*))
      "*default-shell* must not be empty"))

(test status-height-positive-integer
  "*status-height* is a positive integer (default 1)."
  (is (integerp cl-tmux/config:*status-height*)
      "*status-height* must be an integer")
  (is (plusp cl-tmux/config:*status-height*)
      "*status-height* must be positive"))

;;; ── init-default-shell ────────────────────────────────────────────────────

(test init-default-shell-reads-shell-env-var
  "init-default-shell sets *default-shell* from $SHELL when it is set and
   non-empty."
  (with-isolated-config
    (with-temporary-posix-environment-variable ("SHELL" "/bin/my-test-shell")
      (cl-tmux/config:init-default-shell)
      (is (string= "/bin/my-test-shell" cl-tmux/config:*default-shell*)
          "*default-shell* must be set from $SHELL"))))

(test init-default-shell-ignores-unset-shell-env-var
  "init-default-shell leaves *default-shell* unchanged when $SHELL is unset."
  (with-isolated-config
    (with-temporary-posix-environment-variable ("SHELL" nil)
      (let ((before cl-tmux/config:*default-shell*))
        (cl-tmux/config:init-default-shell)
        (is (string= before cl-tmux/config:*default-shell*)
            "*default-shell* must be unchanged when $SHELL is unset")))))
