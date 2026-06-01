(in-package #:cl-tmux/test)

;;;; Configuration and key-binding tests.
;;;;
;;;; These tests are purely functional (no PTY, no threads) and cover:
;;;;   • the compile-time constant +prefix-key-code+,
;;;;   • known bindings in the default *key-bindings* table,
;;;;   • the lookup-key-binding helper, and
;;;;   • structural invariants of *key-bindings* itself.

(def-suite config-suite :description "Key bindings and configuration")
(in-suite config-suite)

;;; ── Import the config symbols we need ────────────────────────────────────

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(cl-tmux/config:lookup-key-binding
            cl-tmux/config:describe-key-bindings
            cl-tmux/config:*key-bindings*
            cl-tmux/config:+prefix-key-code+
            cl-tmux/config:+max-scrollback-lines+
            cl-tmux/config:+poll-timeout-us+
            cl-tmux/config:set-key-binding
            cl-tmux/config:remove-key-binding)))

;;; ── Constant value ─────────────────────────────────────────────────────────

(test prefix-key-code
  "+prefix-key-code+ is 2 (ASCII STX / C-b)."
  (is (= 2 +prefix-key-code+)
      "+prefix-key-code+ should be 2, got ~A" +prefix-key-code+))

;;; ── Known default bindings ────────────────────────────────────────────────

(test lookup-c-binds-new-window
  "C-b c creates a new window."
  (is (eq :new-window (lookup-key-binding #\c))
      "#\\c should be bound to :new-window"))

(test lookup-d-binds-detach
  "C-b d detaches the client."
  (is (eq :detach (lookup-key-binding #\d))
      "#\\d should be bound to :detach"))

(test lookup-unknown-returns-nil
  "An unbound key returns NIL."
  (is (null (lookup-key-binding #\z))
      "#\\z should return NIL (unbound)"))

;;; ── Structural invariants of *key-bindings* ───────────────────────────────

(test all-bindings-have-keyword-values
  "Every value (cdr) in *key-bindings* is a keyword symbol."
  (dolist (binding *key-bindings*)
    (is (keywordp (cdr binding))
        "binding ~A should have a keyword value, got ~A"
        binding (cdr binding))))

(test all-bindings-have-char-or-string-keys
  "Every key (car) in *key-bindings* is a character or a string."
  (dolist (binding *key-bindings*)
    (is (or (characterp (car binding))
            (stringp    (car binding)))
        "binding ~A should have a character or string key, got ~A"
        binding (car binding))))

;;; ── define-initial-key-bindings macro ─────────────────────────────────────

(test define-initial-key-bindings-macro-produces-alist
  "define-initial-key-bindings produces fresh cons pairs for char and digit entries."
  (let ((bindings (define-initial-key-bindings
                    (#\c :new-window)
                    (:digits :select-window))))
    ;; #\c → :new-window
    (is (eq :new-window (cdr (assoc #\c bindings)))
        "char entry must produce a (char . command) pair")
    ;; digits 0-9 → :select-window
    (dolist (d '(#\0 #\1 #\5 #\9))
      (is (eq :select-window (cdr (assoc d bindings)))
          "digit ~C must map to :select-window" d))
    ;; 11 total entries: 1 char + 10 digits
    (is (= 11 (length bindings)))))

;;; ── set-key-binding / remove-key-binding ──────────────────────────────────

(test set-key-binding-adds-new
  "set-key-binding adds a brand-new binding that lookup-key-binding finds."
  (with-isolated-config
    (is (null (lookup-key-binding #\z))
        "#\\z should start unbound")
    (set-key-binding #\z :new-window)
    (is (eq :new-window (lookup-key-binding #\z))
        "#\\z should be bound to :new-window after set-key-binding")))

(test set-key-binding-replaces-existing
  "set-key-binding on an existing key replaces the command without duplicating."
  (with-isolated-config
    (set-key-binding #\z :new-window)
    (let ((before (count #\z *key-bindings* :key #'car :test #'equal)))
      (is (= 1 before)
          "#\\z should appear exactly once after first bind, got ~A" before))
    (set-key-binding #\z :detach)
    (is (eq :detach (lookup-key-binding #\z))
        "#\\z should now be bound to :detach")
    (let ((after (count #\z *key-bindings* :key #'car :test #'equal)))
      (is (= 1 after)
          "#\\z should still appear exactly once (no duplicate), got ~A" after))))

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
    (is (search "new-window" text)   "should list new-window")
    (is (search "detach" text)       "should list detach")
    (is (search "select-window" text) "should list select-window")))

;;; ── +max-scrollback-lines+ constant ───────────────────────────────────────

(test max-scrollback-lines-constant
  "+max-scrollback-lines+ equals 1000."
  (is (= 1000 +max-scrollback-lines+)
      "+max-scrollback-lines+ must be 1000, got ~A" +max-scrollback-lines+))

;;; ── +poll-timeout-us+ constant ─────────────────────────────────────────────

(test poll-timeout-constant
  "+poll-timeout-us+ equals 50000 µs (50 ms ≈ 20 fps max)."
  (is (= 50000 +poll-timeout-us+)
      "+poll-timeout-us+ should be 50000, got ~A" +poll-timeout-us+)
  (is (plusp +poll-timeout-us+)
      "+poll-timeout-us+ must be positive"))
