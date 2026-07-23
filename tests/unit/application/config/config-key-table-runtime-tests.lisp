(in-package #:cl-tmux/test)

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

(describe "config-suite"

  ;;; ── +max-scrollback-lines+ constant ───────────────────────────────────────

  ;; +max-scrollback-lines+ equals 1000.
  (it "max-scrollback-lines-constant"
    (expect (= 1000 +max-scrollback-lines+)))

  ;;; ── Key-table system tests ────────────────────────────────────────────────

  ;; *key-tables* is populated after load.
  (it "key-tables-initialized"
    (expect (hash-table-p cl-tmux/config:*key-tables*)))

  ;; The standard key-tables are created by initialize-default-key-tables.
  (it "key-tables-required-tables-exist"
    (dolist (name '("prefix" "root" "copy-mode" "copy-mode-vi"))
      (expect (not (null (gethash name cl-tmux/config:*key-tables*))))))

  ;; key-table-bind stores a binding retrievable by key-table-lookup in both 'root' and 'prefix' tables.
  (it "key-table-bind-table"
    (dolist (row '(("root"   #\a "root table binding")
                   ("prefix" #\c "prefix table binding")))
      (destructuring-bind (table-name key desc) row
        (declare (ignore desc))
        (with-isolated-key-tables
          (cl-tmux/config:key-table-bind table-name key :new-window)
          (let ((entry (cl-tmux/config:key-table-lookup table-name key)))
            (expect (not (null entry)))
            (expect (eq :new-window (cl-tmux/config:key-table-command entry))))))))

  ;; key-table-bind with :repeatable T marks the entry repeatable; without the flag
  ;; it defaults to not repeatable.  Each row: (key cmd rep expected description).
  (it "key-table-repeatable-flag-variants"
    (dolist (row '((#\r :resize-left t   t   ":repeatable T must be repeatable")
                   (#\c :new-window  nil nil "no :repeatable flag must not be repeatable")))
      (destructuring-bind (key cmd rep expected desc) row
        (declare (ignore desc))
        (with-isolated-key-tables
          (cl-tmux/config:key-table-bind "prefix" key cmd :repeatable rep)
          (let ((entry (cl-tmux/config:key-table-lookup "prefix" key)))
            (expect (not (null entry)))
            (expect (eql expected (cl-tmux/config:key-table-repeatable-p entry))))))))

  ;; key-table-lookup returns NIL for an absent key and for a non-existent table.
  (it "key-table-lookup-missing-returns-nil"
    (let ((cl-tmux/config:*key-tables* (make-hash-table :test #'equal)))
      (expect (null (cl-tmux/config:key-table-lookup "prefix"      #\z)))
      (expect (null (cl-tmux/config:key-table-lookup "nonexistent" #\a)))))

  ;;; ── key-table-repeatable-p nil-safe guard ─────────────────────────────────

  ;; key-table-repeatable-p returns NIL when passed NIL (nil-safe guard).
  (it "key-table-repeatable-p-nil-safe"
    (expect (null (cl-tmux/config:key-table-repeatable-p nil))))

  ;; key-table-command is the car of the entry; key-table-repeatable-p is nil-safe.
  (it "key-table-command-nil-safe"
    (with-isolated-key-tables
      ;; Absent key returns NIL; nil-safe guard means no error.
      (let ((absent (cl-tmux/config:key-table-lookup "prefix" #\@)))
        (expect (null absent))
        (expect (null (cl-tmux/config:key-table-repeatable-p absent))))))

  ;; Timeout and buffer-size constants have the expected values and are all positive.
  (it "numeric-constants"
    (dolist (row `((,+poll-timeout-us+     50000  "+poll-timeout-us+")
                   (,+accept-timeout-us+   100000 "+accept-timeout-us+")
                   (,+pty-buf-size+        4096   "+pty-buf-size+")
                   (,+pty-poll-timeout-us+ 50000  "+pty-poll-timeout-us+")))
      (destructuring-bind (value expected name) row
        (declare (ignore name))
        (expect (= expected value))
        (expect (plusp value)))))

  ;;; ── ensure-key-table side effects ────────────────────────────────────────

  ;; ensure-key-table creates a fresh hash-table for a previously unknown name.
  (it "ensure-key-table-creates-new-table"
    (with-isolated-key-tables
      (let ((tbl (cl-tmux/config:ensure-key-table "my-table")))
        (expect (hash-table-p tbl))
        (expect (eq tbl (gethash "my-table" cl-tmux/config:*key-tables*))))))

  ;; ensure-key-table returns the same table on repeated calls.
  (it "ensure-key-table-returns-existing-table"
    (with-isolated-key-tables
      (let* ((tbl1 (cl-tmux/config:ensure-key-table "my-table"))
             (tbl2 (cl-tmux/config:ensure-key-table "my-table")))
        (expect (eq tbl1 tbl2)))))

  ;;; ── lookup-key-binding on digit keys ────────────────────────────────────

  ;; The digit characters 0-9 all bind :select-window in the default prefix table.
  (it "lookup-digit-keys-bind-select-window"
    (dolist (d '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
      (expect (eq :select-window (lookup-key-binding d)))))

  ;;; ── send-prefix binding ──────────────────────────────────────────────────

  ;; The prefix key itself (code-char 2 = C-b) is bound to :send-prefix.
  (it "send-prefix-binding"
    (expect (eq :send-prefix (lookup-key-binding (code-char +prefix-key-code+)))))

  ;;; ── describe-key-bindings header ────────────────────────────────────────

  ;; describe-key-bindings output uses bind-key -T table format (real tmux list-keys format).
  (it "describe-key-bindings-has-header"
    (let ((text (describe-key-bindings)))
      (expect (search "bind-key" text))
      (expect (search "-T" text))))

  ;;; ── initialize-default-key-tables idempotency ─────────────────────────────

  ;; Calling initialize-default-key-tables twice does not duplicate bindings.
  (it "initialize-default-key-tables-idempotent"
    (with-isolated-key-tables
      (cl-tmux/config::initialize-default-key-tables)
      (let* ((tbl-after-first  (cl-tmux/config:ensure-key-table "prefix"))
             (count-after-first (hash-table-count tbl-after-first)))
        (cl-tmux/config::initialize-default-key-tables)
        (let ((count-after-second (hash-table-count tbl-after-first)))
          (expect (= count-after-first count-after-second))))
      (expect (not (null (gethash "root"      cl-tmux/config:*key-tables*))))
      (expect (not (null (gethash "copy-mode" cl-tmux/config:*key-tables*))))
      (expect (not (null (gethash "copy-mode-vi" cl-tmux/config:*key-tables*))))))

  ;;; ── Key-table name constants ──────────────────────────────────────────────

  ;; Standard key-table constants have their expected string values.
  (it "table-name-constants"
    (check-table `((,cl-tmux/config:+table-prefix+       "prefix"       "+table-prefix+")
                   (,cl-tmux/config:+table-root+         "root"         "+table-root+")
                   (,cl-tmux/config:+table-copy-mode+    "copy-mode"    "+table-copy-mode+")
                   (,cl-tmux/config:+table-copy-mode-vi+ "copy-mode-vi" "+table-copy-mode-vi+"))
                :test #'string=))

  ;;; ── *default-shell* and *status-height* initial values ───────────────────

  ;; *default-shell* is a non-empty string (set from $SHELL or /bin/sh).
  (it "default-shell-is-string"
    (expect (stringp cl-tmux/config:*default-shell*))
    (expect (plusp (length cl-tmux/config:*default-shell*))))

  ;; *status-height* is a positive integer (default 1).
  (it "status-height-positive-integer"
    (expect (integerp cl-tmux/config:*status-height*))
    (expect (plusp cl-tmux/config:*status-height*)))

  ;;; ── init-default-shell ────────────────────────────────────────────────────

  ;; init-default-shell sets *default-shell* from $SHELL when it is set and
  ;; non-empty.
  (it "init-default-shell-reads-shell-env-var"
    (with-isolated-config
      (with-temporary-posix-environment-variable ("SHELL" "/bin/my-test-shell")
        (cl-tmux/config:init-default-shell)
        (expect (string= "/bin/my-test-shell" cl-tmux/config:*default-shell*)))))

  ;; init-default-shell leaves *default-shell* unchanged when $SHELL is unset.
  (it "init-default-shell-ignores-unset-shell-env-var"
    (with-isolated-config
      (with-temporary-posix-environment-variable ("SHELL" nil)
        (let ((before cl-tmux/config:*default-shell*))
          (cl-tmux/config:init-default-shell)
          (expect (string= before cl-tmux/config:*default-shell*)))))))
