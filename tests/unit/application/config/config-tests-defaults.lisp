(in-package #:cl-tmux/test)

(in-suite config-suite)

;;;; Configuration defaults, initialization, and parsing tests split from
;;;; config-tests.lisp so the base file can stay focused on direct binding
;;;; lookup and prefix-table invariants.

;;; ── Import the config symbols we need ────────────────────────────────────

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(cl-tmux/config:lookup-key-binding
             cl-tmux/config:describe-key-bindings
             cl-tmux/config:+prefix-key-code+
            cl-tmux/config:+table-prefix+
            cl-tmux/config:+table-root+
            cl-tmux/config:+table-copy-mode+
            cl-tmux/config:+table-copy-mode-vi+
            cl-tmux/config:*prefix-key-code*)))

;;; ── Default prefix bindings and list-keys output ──────────────────────────

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
                  (#\$ :rename-session)
                  (#\! :break-pane)))
    (let ((key (first pair))
          (expected (second pair)))
      (is (eq expected (lookup-key-binding key))
          "key ~C must be bound to ~A (got ~A)"
          key expected (lookup-key-binding key))))
  (is (null (lookup-key-binding #\@))
      "#\\@ (unbound) must return NIL")
  (is (null (lookup-key-binding #\Z))
      "#\\Z is intentionally unbound; lowercase #\\z is the canonical zoom binding"))

;;; describe-key-bindings-has-header, initialize-default-key-tables-idempotent,
;;; and table-name-constants used to be duplicated here; the canonical copies
;;; (with isolation helpers) live in config-key-table-runtime-tests.lisp.

(test key-table-command-extracts-car
  "key-table-command returns the car of a key-table entry (the command keyword)."
  (let ((cl-tmux/config:*key-tables* (make-hash-table :test #'equal)))
    (cl-tmux/config:key-table-bind "prefix" #\c :new-window)
    (let ((entry (cl-tmux/config:key-table-lookup "prefix" #\c)))
      (is (not (null entry)) "entry must exist")
      (is (eq :new-window (cl-tmux/config:key-table-command entry))
          "key-table-command must return :new-window"))))

(test key-tables-copy-mode-table-exists
  "The copy-mode key-tables are created by initialize-default-key-tables."
  (is (not (null (gethash "copy-mode" cl-tmux/config:*key-tables*)))
      "\"copy-mode\" table must exist in *key-tables*")
  (is (not (null (gethash "copy-mode-vi" cl-tmux/config:*key-tables*)))
      "\"copy-mode-vi\" table must exist in *key-tables*"))

(test mode-keys-default-is-emacs
  "The registry default for mode-keys is emacs, matching tmux (vi is autodetected
   from $VISUAL/$EDITOR at startup, not the static default)."
  (with-isolated-config
    (is (string= "emacs" (cl-tmux/options:get-option "mode-keys"))
        "unset mode-keys must default to emacs")
    (is (string= "emacs" (cl-tmux/options:get-option "status-keys"))
        "unset status-keys must default to emacs")))

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

(test parse-prefix-key-extended-notations
  "%parse-prefix-key also accepts caret control notation, C-Space/C-@ (NUL), and
   control symbols, while None/Any and named keys (M-/F-keys) parse to NIL so the
   byte event loop never tries to match an unmatchable prefix."
  (dolist (c '(("^A"      1   "caret ^A → 1")
               ("^["      27  "caret ^[ → 27 (Escape byte)")
               ("C-Space" 0   "C-Space → 0 (NUL)")
               ("C-@"     0   "C-@ → 0 (NUL)")
               ("None"    nil "None → NIL (disable, not a byte)")
               ("Any"     nil "Any → NIL")
               ("M-a"     nil "M-a → NIL (not single-byte matchable)")
               ("F1"      nil "F1 → NIL (not single-byte matchable)")))
    (destructuring-bind (input expected desc) c
      (is (equal expected (cl-tmux/config::%parse-prefix-key input))
          "~A" desc))))

(test bind-prefix-key-none-disables
  "`set-option -g prefix2 None` disables the secondary prefix (NIL); `set-option -g prefix None`
   resets the primary prefix to the default +prefix-key-code+."
  (let ((cl-tmux/config:*prefix-key-code*  1)
        (cl-tmux/config:*prefix2-key-code* 7))
    (cl-tmux/config::%bind-prefix-key "None" 'cl-tmux/config::*prefix2-key-code*)
    (is (null cl-tmux/config:*prefix2-key-code*)
        "prefix2 None must clear *prefix2-key-code* to NIL")
    (cl-tmux/config::%bind-prefix-key "None" 'cl-tmux/config::*prefix-key-code*)
    (is (= cl-tmux/config:+prefix-key-code+ cl-tmux/config:*prefix-key-code*)
        "prefix None must reset *prefix-key-code* to the default")))
