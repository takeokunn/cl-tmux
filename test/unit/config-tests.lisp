(in-package #:cl-tmux/test)

;;;; Configuration and key-binding tests.
;;;;
;;;; These tests are purely functional (no PTY, no threads) and cover:
;;;;   • the compile-time constant +prefix-key-code+,
;;;;   • known bindings in the default prefix key-table,
;;;;   • the lookup-key-binding helper, and
;;;;   • structural invariants of the prefix key-table itself.

(def-suite config-suite :description "Key bindings and configuration")
(in-suite config-suite)

;;; ── Import the config symbols we need ────────────────────────────────────

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(cl-tmux/config:lookup-key-binding
             cl-tmux/config:describe-key-bindings
             cl-tmux/config:+prefix-key-code+
            cl-tmux/config:+max-scrollback-lines+
            cl-tmux/config:+poll-timeout-us+
            cl-tmux/config:+accept-timeout-us+
            cl-tmux/config:+pty-buf-size+
            cl-tmux/config:+pty-poll-timeout-us+
            cl-tmux/config:set-key-binding
            cl-tmux/config:remove-key-binding)))

;;; ── Constant value ─────────────────────────────────────────────────────────

(test prefix-key-code
  "+prefix-key-code+ is 2 (ASCII STX / C-b)."
  (is (= 2 +prefix-key-code+)
      "+prefix-key-code+ should be 2, got ~A" +prefix-key-code+))

;;; ── Known default bindings ────────────────────────────────────────────────

(test lookup-known-bindings-table
  "C-b c creates a new window; C-b d detaches the client."
  (dolist (row '((#\c :new-window "#\\c → :new-window")
                 (#\d :detach     "#\\d → :detach")))
    (destructuring-bind (key expected desc) row
      (is (eq expected (lookup-key-binding key)) "~A" desc))))

(test lookup-unknown-returns-nil
  "An unbound key returns NIL.  #\\z is now bound to :zoom-toggle, so we
   use #\\@ (ASCII 64) which has no default binding."
  (is (null (lookup-key-binding #\@))
      "#\\@ should return NIL (unbound)"))

;;; ── Structural invariants of prefix key-table ──────────────────────────────

(test all-bindings-have-keyword-or-list-values
  "Every value in the prefix key-table is a keyword symbol or a command token list."
  (let* ((tbl (cl-tmux/config:ensure-key-table "prefix"))
         (keys nil))
    (maphash (lambda (k v) (declare (ignore k)) (push v keys)) tbl)
    (dolist (entry keys)
      (let ((cmd (cl-tmux/config:key-table-command entry)))
        (is (or (keywordp cmd) (and (consp cmd) (every #'stringp cmd)))
            "entry ~A should have a keyword or string-list command, got ~A"
            entry cmd)))))

(test all-bindings-have-char-or-string-keys
  "Every key in the prefix key-table is a character or a string."
  (let* ((tbl (cl-tmux/config:ensure-key-table "prefix"))
         (keys nil))
    (maphash (lambda (k v) (declare (ignore v)) (push k keys)) tbl)
    (dolist (k keys)
      (is (or (characterp k)
              (stringp    k))
          "key ~A should be a character or string, got ~A"
          k (type-of k)))))

;;; ── define-initial-key-bindings macro ─────────────────────────────────────
;;;
;;; define-initial-key-bindings expands to side-effecting key-table-bind calls.
;;; It does NOT return an alist.  Tests verify the side effects via key-table-lookup.

(test define-initial-key-bindings-macro-populates-key-table
  "define-initial-key-bindings expands to install-default-prefix-bindings, which
   populates the prefix key-table for char and digit entries when called."
  ;; The macro now expands to (defun install-default-prefix-bindings ...) rather
  ;; than emitting side effects, so we must CALL the generated installer to
  ;; populate the table.  Because the macro redefines the GLOBAL installer with
  ;; this test's custom binding set, save and restore its real definition — else
  ;; later tests that rebuild defaults via initialize-default-key-tables would
  ;; inherit a prefix table missing #\d, #\x, etc. (a cross-test cascade).
  (let ((cl-tmux/config:*key-tables* (make-hash-table :test #'equal))
        (saved-installer
          (fdefinition 'cl-tmux/config::install-default-prefix-bindings)))
    (unwind-protect
         (progn
           (define-initial-key-bindings
             (#\c :new-window)
             (:digits :select-window))
           (cl-tmux/config::install-default-prefix-bindings)
           ;; #\c → :new-window
           (let ((entry (cl-tmux/config:key-table-lookup "prefix" #\c)))
             (is (not (null entry)) "#\\c must have a prefix binding")
             (is (eq :new-window (cl-tmux/config:key-table-command entry))
                 "char entry must bind :new-window"))
           ;; digits 0-9 → :select-window
           (dolist (d '(#\0 #\1 #\5 #\9))
             (let ((entry (cl-tmux/config:key-table-lookup "prefix" d)))
               (is (not (null entry)) "digit ~C must have a prefix binding" d)
               (is (eq :select-window (cl-tmux/config:key-table-command entry))
                   "digit ~C must bind :select-window" d)))
           ;; 11 total entries: 1 char + 10 digits
           (let ((tbl (cl-tmux/config:ensure-key-table "prefix")))
             (is (= 11 (hash-table-count tbl))
                 "prefix table must have exactly 11 entries (1 char + 10 digits)")))
      (setf (fdefinition 'cl-tmux/config::install-default-prefix-bindings)
            saved-installer))))

;;; ── set-key-binding / remove-key-binding ──────────────────────────────────

(test set-key-binding-adds-new
  "set-key-binding adds a brand-new binding that lookup-key-binding finds.
   Uses #\\@ (ASCII 64) which has no default binding."
  (with-isolated-config
    (is (null (lookup-key-binding #\@))
        "#\\@ should start unbound")
    (set-key-binding #\@ :new-window)
    (is (eq :new-window (lookup-key-binding #\@))
        "#\\@ should be bound to :new-window after set-key-binding")))

(test set-key-binding-replaces-existing
  "set-key-binding on an existing key replaces the command without duplicating."
  (with-isolated-config
    (set-key-binding #\z :new-window)
    (is (eq :new-window (lookup-key-binding #\z))
        "#\\z should be bound to :new-window")
    (let* ((tbl (cl-tmux/config:ensure-key-table "prefix"))
           (before (hash-table-count tbl)))
      (set-key-binding #\z :detach)
      (is (eq :detach (lookup-key-binding #\z))
          "#\\z should now be bound to :detach")
      (let ((after (hash-table-count tbl)))
        (is (= before after)
            "prefix table size should not grow (replace, not duplicate)")))))

(test remove-key-binding-removes
  "remove-key-binding removes a binding so lookup returns NIL afterward."
  (with-isolated-config
    (set-key-binding #\z :new-window)
    (is (eq :new-window (lookup-key-binding #\z))
        "#\\z should be bound before removal")
    (remove-key-binding #\z)
    (is (null (lookup-key-binding #\z))
        "#\\z should be unbound after remove-key-binding")))

;;; ── describe-key-bindings (list-keys help text) ─────────────────────────────

(test describe-key-bindings-lists-commands
  "describe-key-bindings produces help text naming the bound commands."
  (let ((text (describe-key-bindings)))
    (dolist (sub '("new-window" "detach" "select-window"))
      (is (search sub text) "should list ~A" sub))))

(test default-prefix-string-bindings-are-listed-and-repeatable
  "Default prefix multi-byte keys are present in the key table and resize arrows are repeatable."
  (with-isolated-config
    (let ((up-entry (cl-tmux/config:key-table-lookup "prefix" "Up"))
          (ctrl-right-entry (cl-tmux/config:key-table-lookup "prefix" "C-Right"))
          (meta-right-entry (cl-tmux/config:key-table-lookup "prefix" "M-Right"))
          (text (cl-tmux/config:describe-key-bindings-for-table "prefix")))
      (is (eq :select-pane-up (cl-tmux/config:key-table-command up-entry))
          "prefix Up must select the pane above")
      (is (equal '("resize-pane" "-R" "1")
                 (cl-tmux/config:key-table-command ctrl-right-entry))
          "prefix C-Right must resize right by 1")
      (is (equal '("resize-pane" "-R" "5")
                 (cl-tmux/config:key-table-command meta-right-entry))
          "prefix M-Right must resize right by 5")
      (is (cl-tmux/config:key-table-repeatable-p ctrl-right-entry)
          "prefix C-Right must be repeatable")
      (is (cl-tmux/config:key-table-repeatable-p meta-right-entry)
          "prefix M-Right must be repeatable")
      (is (search "bind-key -T prefix Up select-pane-up" text)
          "list-keys must show the prefix Up binding")
      (is (search "bind-key -T prefix -r C-Right resize-pane -R 1" text)
          "list-keys must show repeatable C-Right resize binding"))))

(test default-copy-mode-vi-bindings-are-listed
  "Default copy-mode-vi keys are present in the key table and list-keys output."
  (with-isolated-config
    (let ((j-entry (cl-tmux/config:key-table-lookup "copy-mode-vi" #\j))
          (h-entry (cl-tmux/config:key-table-lookup "copy-mode-vi" #\h))
          (page-entry (cl-tmux/config:key-table-lookup "copy-mode-vi" "PageUp"))
          (text (cl-tmux/config:describe-key-bindings-for-table "copy-mode-vi")))
      (is (eq :copy-mode-cursor-down
              (cl-tmux/config:key-table-command j-entry))
          "copy-mode-vi j must move down")
      (is (eq :copy-mode-cursor-left
              (cl-tmux/config:key-table-command h-entry))
          "copy-mode-vi h must move left")
      (is (eq :copy-mode-page-up
              (cl-tmux/config:key-table-command page-entry))
          "copy-mode-vi PageUp must page up")
      (is (search "bind-key -T copy-mode-vi j copy-mode-cursor-down" text)
          "list-keys must show copy-mode-vi j")
      (is (search "bind-key -T copy-mode-vi PageUp copy-mode-page-up" text)
          "list-keys must show copy-mode-vi PageUp"))))

;;; ── +max-scrollback-lines+ constant ───────────────────────────────────────

(test max-scrollback-lines-constant
  "+max-scrollback-lines+ equals 1000."
  (is (= 1000 +max-scrollback-lines+)
      "+max-scrollback-lines+ must be 1000, got ~A" +max-scrollback-lines+))

;;; ── Numeric compile-time constants ─────────────────────────────────────────

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
      (let ((cl-tmux/config:*key-tables* (make-hash-table :test #'equal)))
        (cl-tmux/config:key-table-bind table-name key :new-window)
        (let ((entry (cl-tmux/config:key-table-lookup table-name key)))
          (is (not (null entry)) "~A: binding must be found" desc)
          (is (eq :new-window (cl-tmux/config:key-table-command entry))
              "~A: command must be :new-window" desc))))))

(test key-table-repeatable-flag
  "key-table-bind with :repeatable T marks the entry as repeatable."
  (let ((cl-tmux/config:*key-tables* (make-hash-table :test #'equal)))
    (cl-tmux/config:key-table-bind "prefix" #\r :resize-left :repeatable t)
    (let ((entry (cl-tmux/config:key-table-lookup "prefix" #\r)))
      (is (not (null entry)) "binding must be found")
      (is (cl-tmux/config:key-table-repeatable-p entry)
          "entry must be repeatable"))))

(test key-table-not-repeatable-by-default
  "key-table-bind without :repeatable does not mark the entry as repeatable."
  (let ((cl-tmux/config:*key-tables* (make-hash-table :test #'equal)))
    (cl-tmux/config:key-table-bind "prefix" #\c :new-window)
    (let ((entry (cl-tmux/config:key-table-lookup "prefix" #\c)))
      (is (not (cl-tmux/config:key-table-repeatable-p entry))
          "entry must not be repeatable by default"))))

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
  (let ((cl-tmux/config:*key-tables* (make-hash-table :test #'equal)))
    ;; Absent key returns NIL; nil-safe guard means no error.
    (let ((absent (cl-tmux/config:key-table-lookup "prefix" #\@)))
      (is (null absent) "absent key must return NIL")
      (is (null (cl-tmux/config:key-table-repeatable-p absent))
          "key-table-repeatable-p on NIL must return NIL"))))

(test numeric-constants
  "Timeout and buffer-size constants have the expected values and are all positive."
  (is (= 50000  +poll-timeout-us+)     "+poll-timeout-us+ must be 50000")
  (is (= 100000 +accept-timeout-us+)   "+accept-timeout-us+ must be 100000")
  (is (= 4096   +pty-buf-size+)        "+pty-buf-size+ must be 4096")
  (is (= 50000  +pty-poll-timeout-us+) "+pty-poll-timeout-us+ must be 50000")
  (is (plusp +poll-timeout-us+)     "+poll-timeout-us+ must be positive")
  (is (plusp +accept-timeout-us+)   "+accept-timeout-us+ must be positive")
  (is (plusp +pty-buf-size+)        "+pty-buf-size+ must be positive")
  (is (plusp +pty-poll-timeout-us+) "+pty-poll-timeout-us+ must be positive"))

;;; ── ensure-key-table side effects ────────────────────────────────────────

(test ensure-key-table-creates-new-table
  "ensure-key-table creates a fresh hash-table for a previously unknown name."
  (let ((cl-tmux/config:*key-tables* (make-hash-table :test #'equal)))
    (let ((tbl (cl-tmux/config:ensure-key-table "my-table")))
      (is (hash-table-p tbl)
          "ensure-key-table must return a hash-table")
      (is (eq tbl (gethash "my-table" cl-tmux/config:*key-tables*))
          "the returned table must be stored in *key-tables*"))))

(test ensure-key-table-returns-existing-table
  "ensure-key-table returns the same table on repeated calls."
  (let ((cl-tmux/config:*key-tables* (make-hash-table :test #'equal)))
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
  (let ((cl-tmux/config:*key-tables* (make-hash-table :test #'equal)))
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
  (dolist (c `(("prefix"       ,cl-tmux/config:+table-prefix+)
               ("root"         ,cl-tmux/config:+table-root+)
               ("copy-mode"    ,cl-tmux/config:+table-copy-mode+)
               ("copy-mode-vi" ,cl-tmux/config:+table-copy-mode-vi+)))
    (destructuring-bind (expected actual) c
      (is (string= expected actual) "constant must equal ~S" expected))))

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

;;; ── Table-driven default prefix bindings check ───────────────────────────
;;;
;;; The three near-identical lookup-X-binds-Y tests are consolidated here into
;;; a single table-driven test covering all standard single-char bindings.

(test default-prefix-bindings-table
  "All standard single-char prefix bindings are registered with the correct commands."
  (dolist (pair '((#\c :new-window)
                  (#\n :next-window)
                  (#\p :prev-window)
                  (#\o :next-pane)
                  (#\d :detach)
                  (#\? :list-keys)
                  (#\[ :copy-mode-enter)
                  (#\] :paste-buffer)
                  (#\x :kill-pane-confirm)
                  (#\& :kill-window-confirm)
                  (#\, :rename-window)
                  (#\H :resize-left)
                  (#\J :resize-down)
                  (#\K :resize-up)
                  ;; #\L and #\! are rebound to their tmux-correct commands by
                  ;; events-loop.lisp (loaded after config.lisp): L = last-session
                  ;; (switch-client -l), ! = break-pane.
                  (#\L :last-session)
                  (#\Z :zoom-toggle)
                  (#\$ :rename-session)
                  (#\! :break-pane)))
    (let ((key (first pair))
          (expected (second pair)))
      (is (eq expected (lookup-key-binding key))
          "key ~C must be bound to ~A (got ~A)"
          key expected (lookup-key-binding key))))
  ;; Also verify an unbound key returns NIL
  (is (null (lookup-key-binding #\@))
      "#\\@ (unbound) must return NIL"))

;;; ── key-table-command on a valid entry ───────────────────────────────────

(test key-table-command-extracts-car
  "key-table-command returns the car of a key-table entry (the command keyword)."
  (let ((cl-tmux/config:*key-tables* (make-hash-table :test #'equal)))
    (cl-tmux/config:key-table-bind "prefix" #\c :new-window)
    (let ((entry (cl-tmux/config:key-table-lookup "prefix" #\c)))
      (is (not (null entry)) "entry must exist")
      (is (eq :new-window (cl-tmux/config:key-table-command entry))
          "key-table-command must return :new-window"))))

;;; ── copy-mode table exists after initialize ───────────────────────────────

(test key-tables-copy-mode-table-exists
  "The copy-mode key-tables are created by initialize-default-key-tables."
  (is (not (null (gethash "copy-mode" cl-tmux/config:*key-tables*)))
      "\"copy-mode\" table must exist in *key-tables*")
  (is (not (null (gethash "copy-mode-vi" cl-tmux/config:*key-tables*)))
      "\"copy-mode-vi\" table must exist in *key-tables*"))

;;; ── *prefix-key-code* dynamic variable ──────────────────────────────────────

(test prefix-key-code-dynamic-var-defaults-to-constant
  "*prefix-key-code* defaults to the value of +prefix-key-code+."
  (is (= +prefix-key-code+ cl-tmux/config:*prefix-key-code*)
      "*prefix-key-code* must equal +prefix-key-code+ by default"))

;;; ── %parse-prefix-key ────────────────────────────────────────────────────────

(test parse-prefix-key-table
  "%parse-prefix-key: C-X keys, single chars, and unknown return expected values."
  (dolist (c '(("C-a"        1   "C-a → 1 (logand 97 #x1f)")
               ("C-b"        2   "C-b → 2 (logand 98 #x1f, the default prefix)")
               ("A"          65  "single char 'A' → char-code 65")
               ("UnknownKey" nil "unknown key name → NIL")))
    (destructuring-bind (input expected desc) c
      (is (equal expected (cl-tmux/config::%parse-prefix-key input))
          "~A" desc))))
