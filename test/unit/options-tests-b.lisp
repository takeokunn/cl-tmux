(in-package #:cl-tmux/test)

;;;; options tests — part B: define-option-accessor, define-type-coercions,
;;;; table-driven coercion checks, scoped overrides, show-options, list-options,
;;;; per-window option resolution, option default shadowing, server options.

(in-suite options-suite)

;;; ── define-option-accessor macro ─────────────────────────────────────────

(test define-option-accessor-macro-is-defined
  "define-option-accessor is a registered macro."
  (is (macro-function 'cl-tmux/options::define-option-accessor)))

;;; ── define-type-coercions macro ──────────────────────────────────────────

(test define-type-coercions-macro-is-defined
  "define-type-coercions is a registered macro."
  (is (macro-function 'cl-tmux/options::define-type-coercions)))

;;; ── Table-driven coercion checks ─────────────────────────────────────────
;;;
;;; Consolidates the repeated %coerce-value assertions into a single
;;; parameterised block covering all three type branches.

(test coerce-value-table-driven
  "%coerce-value behaves correctly across all registered type branches."
  (dolist (entry '(;; :boolean branch
                   (:boolean "on"    t)
                   (:boolean "true"  t)
                   (:boolean "1"     t)
                   (:boolean "off"   nil)
                   (:boolean "false" nil)
                   (:boolean "0"     nil)
                   (:boolean 42      t)
                   (:boolean nil     nil)
                   ;; :integer branch
                   (:integer "42"       42)
                   (:integer "0"        0)
                   (:integer "not-num"  0)
                   (:integer 3          3)
                   (:integer nil        0)
                   ;; :string branch
                   (:string "hello"  "hello")
                   (:string 42       "42")
                   (:string t        "T")))
    (destructuring-bind (type input expected) entry
      (let ((result (cl-tmux/options::%coerce-value type input)))
        (is (equal expected result)
            "%coerce-value ~S ~S: expected ~S got ~S"
            type input expected result)))))

;;; ── show-option :server scope when absent ────────────────────────────────

(test show-option-server-scope-absent
  "show-option :server for an absent server option says 'not set'."
  (with-fresh-server-options
    (let ((out (cl-tmux/options:show-option "nonexistent-server-opt" :server)))
      (is (search "nonexistent-server-opt" out)
          "show-option :server absent must include option name (got ~S)" out))))

;;; ── set-option returns coerced value ─────────────────────────────────────

(test set-option-returns-coerced-value
  "set-option returns the coerced value, not the input."
  (with-fresh-global-options
    (let ((result (cl-tmux/options:set-option "history-limit" "1234")))
      (is (= 1234 result)
          "set-option must return the coerced integer 1234, got ~S" result))
    (let ((result (cl-tmux/options:set-option "mouse" "on")))
      (is (eq t result)
          "set-option must return T for boolean on, got ~S" result))
    (let ((result (cl-tmux/options:set-option "status-left" "text")))
      (is (string= "text" result)
          "set-option must return the string unchanged, got ~S" result))))

;;; ── integer coercion: non-numeric non-nil value falls back to 0 ──────────

(test integer-coercion-non-numeric-falls-back-to-zero
  "%coerce-value :integer returns 0 for non-numeric non-string non-number input."
  (is (= 0 (cl-tmux/options::%coerce-value :integer t))
      ":integer coercion of T must be 0")
  (is (= 0 (cl-tmux/options::%coerce-value :integer :foo))
      ":integer coercion of a keyword must be 0"))

;;; ── *server-option-registry* is a hash-table ─────────────────────────────

(test server-option-registry-is-hash-table
  "*server-option-registry* is a hash-table populated with at least the three
   standard server options."
  (is (hash-table-p cl-tmux/options:*server-option-registry*)
      "*server-option-registry* must be a hash-table")
  (is (not (null (gethash "escape-time"     cl-tmux/options:*server-option-registry*)))
      "escape-time must be in *server-option-registry*")
  (is (not (null (gethash "exit-empty"      cl-tmux/options:*server-option-registry*)))
      "exit-empty must be in *server-option-registry*")
  (is (not (null (gethash "exit-unattached" cl-tmux/options:*server-option-registry*)))
      "exit-unattached must be in *server-option-registry*"))

;;; ── exit-unattached server option default ────────────────────────────────

(test server-options-exit-unattached-default
  "*server-options* contains exit-unattached = NIL by default."
  (is (null (cl-tmux/options:get-server-option "exit-unattached"))
      "default exit-unattached must be NIL"))

;;; ── Per-window scoped option tests ───────────────────────────────────────

(test set-option-for-window-stores-in-local-hash
  "set-option-for-window stores the value in the window's local-options hash."
  (let ((win (cl-tmux/model:make-window :id 1 :name "test-win")))
    (cl-tmux/options:set-option-for-window "synchronize-panes" t win)
    (is (eq t (gethash "synchronize-panes"
                       (cl-tmux/model:window-local-options win)))
        "local-options hash must contain the stored value")))

(test get-option-for-window-returns-local-override
  "get-option-for-window returns the window-local value when present,
   even when the global option has a different value."
  (let ((win (cl-tmux/model:make-window :id 1 :name "test-win"))
        (cl-tmux/options:*global-options*
         (let ((ht (make-hash-table :test #'equal)))
           (setf (gethash "synchronize-panes" ht) nil)
           ht)))
    (cl-tmux/options:set-option-for-window "synchronize-panes" t win)
    (is (eq t (cl-tmux/options:get-option-for-window "synchronize-panes" win))
        "get-option-for-window must return the local override T")))

(test get-option-for-window-falls-back-to-global
  "get-option-for-window returns the global value when no local override is set."
  (let ((win (cl-tmux/model:make-window :id 1 :name "test-win"))
        (cl-tmux/options:*global-options*
         (let ((ht (make-hash-table :test #'equal)))
           (setf (gethash "history-limit" ht) 9999)
           ht)))
    (is (= 9999 (cl-tmux/options:get-option-for-window "history-limit" win))
        "get-option-for-window must fall back to *global-options*")))

(test get-option-for-window-falls-back-to-spec-default
  "get-option-for-window returns the registered spec default when absent from
   both the local hash and *global-options*."
  (let ((win (cl-tmux/model:make-window :id 1 :name "test-win"))
        (cl-tmux/options:*global-options* (make-hash-table :test #'equal)))
    ;; status defaults to the string "on" in the spec table
    (is (string= "on" (cl-tmux/options:get-option-for-window "status" win))
        "get-option-for-window must return spec default \"on\" for status")))

;;; ── Per-pane scoped option tests ─────────────────────────────────────────

(test set-option-for-pane-stores-in-local-hash
  "set-option-for-pane stores the value in the pane's local-options hash."
  (let ((p (cl-tmux/model:make-pane :id 1)))
    (cl-tmux/options:set-option-for-pane "mouse" t p)
    (is (eq t (gethash "mouse" (cl-tmux/model:pane-local-options p)))
        "pane local-options hash must contain the stored value")))

(test get-option-for-pane-returns-local-override
  "get-option-for-pane returns the pane-local value when present,
   even when the global option has a different value."
  (let ((p (cl-tmux/model:make-pane :id 1))
        (cl-tmux/options:*global-options*
         (let ((ht (make-hash-table :test #'equal)))
           (setf (gethash "mouse" ht) nil)
           ht)))
    (cl-tmux/options:set-option-for-pane "mouse" t p)
    (is (eq t (cl-tmux/options:get-option-for-pane "mouse" p))
        "get-option-for-pane must return the local override T")))

(test get-option-for-pane-falls-back-to-global
  "get-option-for-pane returns the global value when no local override is set."
  (let ((p (cl-tmux/model:make-pane :id 1))
        (cl-tmux/options:*global-options*
         (let ((ht (make-hash-table :test #'equal)))
           (setf (gethash "history-limit" ht) 7777)
           ht)))
    (is (= 7777 (cl-tmux/options:get-option-for-pane "history-limit" p))
        "get-option-for-pane must fall back to *global-options*")))

(test get-option-for-pane-falls-back-to-spec-default
  "get-option-for-pane returns the registered spec default when absent from
   both the local hash and *global-options*."
  (let ((p (cl-tmux/model:make-pane :id 1))
        (cl-tmux/options:*global-options* (make-hash-table :test #'equal)))
    ;; mouse defaults to NIL in the spec table
    (is (null (cl-tmux/options:get-option-for-pane "mouse" p))
        "get-option-for-pane must return spec default NIL for mouse")))

(test window-local-options-isolated-between-windows
  "Two windows have independent local-options hashes."
  (let ((win-a (cl-tmux/model:make-window :id 1 :name "a"))
        (win-b (cl-tmux/model:make-window :id 2 :name "b")))
    (cl-tmux/options:set-option-for-window "mouse" t win-a)
    (is (eq t   (cl-tmux/options:get-option-for-window "mouse" win-a))
        "win-a must have mouse = T")
    (is (null (gethash "mouse" (cl-tmux/model:window-local-options win-b)))
        "win-b local-options must be unaffected by win-a")))

(test pane-local-options-isolated-between-panes
  "Two panes have independent local-options hashes."
  (let ((p1 (cl-tmux/model:make-pane :id 1))
        (p2 (cl-tmux/model:make-pane :id 2)))
    (cl-tmux/options:set-option-for-pane "mouse" t p1)
    (is (eq t   (cl-tmux/options:get-option-for-pane "mouse" p1))
        "p1 must have mouse = T")
    (is (null (gethash "mouse" (cl-tmux/model:pane-local-options p2)))
        "p2 local-options must be unaffected by p1")))

;;; ── Scoped accessors: boolean coercion + fallback chain (newly wired) ────

(test set-option-for-window-coerces-boolean-string
  "set-option-for-window coerces a :boolean string \"on\" to T before storing,
   so get-option-for-window returns T (not the literal string)."
  (with-isolated-config
    (let ((win (make-fake-window 1 "w")))
      (cl-tmux/options:set-option-for-window "synchronize-panes" "on" win)
      (is (eq t (cl-tmux/options:get-option-for-window "synchronize-panes" win))
          "get-option-for-window must return coerced T, not the string \"on\""))))

(test get-option-for-window-falls-back-to-global-value
  "With no window-local override, get-option-for-window returns the GLOBAL value."
  (with-isolated-config
    (cl-tmux/options:set-option "synchronize-panes" t)
    (let ((win (make-fake-window 1 "w")))
      (is (eq t (cl-tmux/options:get-option-for-window "synchronize-panes" win))
          "must fall back to the global synchronize-panes value T"))))

(test set-option-for-window-overrides-global
  "A window-local override wins over the global value for that window, while the
   global option itself remains unchanged."
  (with-isolated-config
    (cl-tmux/options:set-option "synchronize-panes" nil)
    (let ((win (make-fake-window 1 "w")))
      (cl-tmux/options:set-option-for-window "synchronize-panes" "on" win)
      (is (eq t (cl-tmux/options:get-option-for-window "synchronize-panes" win))
          "window-local override must return T")
      (is (null (cl-tmux/options:get-option "synchronize-panes"))
          "global synchronize-panes must remain NIL (not changed by -w set)"))))

(test set-option-for-pane-coerces-boolean-string
  "set-option-for-pane coerces a :boolean string \"on\" to T and stores it per-pane;
   get-option-for-pane returns T."
  (with-isolated-config
    (let* ((win  (make-fake-window 1 "w"))
           (pane (first (cl-tmux/model:window-panes win))))
      (cl-tmux/options:set-option-for-pane "remain-on-exit" "on" pane)
      (is (eq t (cl-tmux/options:get-option-for-pane "remain-on-exit" pane))
          "get-option-for-pane must return coerced T, not the string \"on\""))))

;;; ── Falsey local override beats truthy global (present-p semantics) ──────

(test get-option-for-window-falsey-local-overrides-truthy-global
  "A window-local value explicitly set to a FALSEY value (synchronize-panes
   \"off\" → NIL) must win over a truthy GLOBAL value, instead of falling
   through the or-chain.  The global value itself is unchanged."
  (with-isolated-config
    (cl-tmux/options:set-option "synchronize-panes" t)
    (let ((win (make-fake-window 1 "w")))
      (cl-tmux/options:set-option-for-window "synchronize-panes" "off" win)
      (is (null (cl-tmux/options:get-option-for-window "synchronize-panes" win))
          "window-local off (NIL) must win over global on (T)")
      (is (eq t (cl-tmux/options:get-option "synchronize-panes"))
          "global synchronize-panes must remain T (the -w override is local only)"))))

(test get-option-for-pane-falsey-local-overrides-truthy-global
  "A pane-local value explicitly set to a FALSEY value (remain-on-exit \"off\"
   → NIL) must win over a truthy GLOBAL value, instead of falling through."
  (with-isolated-config
    (cl-tmux/options:set-option "remain-on-exit" t)
    (let* ((win  (make-fake-window 1 "w"))
           (pane (first (cl-tmux/model:window-panes win))))
      (cl-tmux/options:set-option-for-pane "remain-on-exit" "off" pane)
      (is (null (cl-tmux/options:get-option-for-pane "remain-on-exit" pane))
          "pane-local off (NIL) must win over global on (T)")
      (is (eq t (cl-tmux/options:get-option "remain-on-exit"))
          "global remain-on-exit must remain T (the -p override is local only)"))))

(test set-option-for-window-coerces-integer
  "set-option-for-window coerces a non-boolean :integer string (\"5000\") to the
   integer 5000 before storing, so get-option-for-window returns the integer
   (not the literal string)."
  (with-isolated-config
    (let ((win (make-fake-window 1 "w")))
      (cl-tmux/options:set-option-for-window "history-limit" "5000" win)
      (is (eql 5000 (cl-tmux/options:get-option-for-window "history-limit" win))
          "get-option-for-window must return coerced integer 5000, not \"5000\""))))

;;; ── get-option-for-context: full pane→window→global→default precedence ──

(test get-option-for-context-pane-beats-window-beats-global
  "get-option-for-context resolves with precedence pane-local > window-local >
   global, and a present-but-falsey PANE override beats a truthy WINDOW value.
   Uses the registered :boolean option synchronize-panes."
  (with-isolated-config
    (cl-tmux/options:set-option "synchronize-panes" nil)   ; global = NIL
    (let* ((win  (make-fake-window 1 "w" :npanes 1))
           (pane (first (cl-tmux/model:window-panes win))))
      ;; No local overrides → resolves to the global value (NIL).
      (is (null (cl-tmux/options:get-option-for-context
                 "synchronize-panes" :pane pane :window win))
          "with no overrides must return the global value NIL")
      ;; Window-local "on" with no pane override → window value wins over global.
      (cl-tmux/options:set-option-for-window "synchronize-panes" "on" win)
      (is (eq t (cl-tmux/options:get-option-for-context
                 "synchronize-panes" :pane pane :window win))
          "window-local on (T) must beat global NIL when pane has no override")
      ;; Pane-local "off" (NIL) → present-but-falsey pane override beats window "on".
      (cl-tmux/options:set-option-for-pane "synchronize-panes" "off" pane)
      (is (null (cl-tmux/options:get-option-for-context
                 "synchronize-panes" :pane pane :window win))
          "pane-local off (NIL) must beat window-local on (T) — falsey honored"))))

(test get-option-for-context-skips-nil-levels
  "get-option-for-context skips a NIL pane/window level: with both NIL it equals
   get-option; with only one scope it consults that scope's local override."
  (with-isolated-config
    (cl-tmux/options:set-option "synchronize-panes" nil)
    ;; Both pane and window NIL → equivalent to plain get-option.
    (is (eq (cl-tmux/options:get-option "synchronize-panes")
            (cl-tmux/options:get-option-for-context "synchronize-panes"))
        "with no pane/window must equal get-option")
    ;; Only :window supplied, with a window-local override → returns window value.
    (let* ((win  (make-fake-window 1 "w" :npanes 1))
           (pane (first (cl-tmux/model:window-panes win))))
      (cl-tmux/options:set-option-for-window "synchronize-panes" "on" win)
      (is (eq t (cl-tmux/options:get-option-for-context
                 "synchronize-panes" :window win))
          "with only :window must return the window-local value T")
      ;; Only :pane supplied, with a pane-local override → returns pane value.
      (cl-tmux/options:set-option-for-pane "synchronize-panes" "off" pane)
      (is (null (cl-tmux/options:get-option-for-context
                 "synchronize-panes" :pane pane))
          "with only :pane must return the pane-local value NIL"))))

(test get-option-for-context-falls-back-to-registry-default
  "get-option-for-context returns the registered default (via the pre-populated
   global store) when neither pane nor window carries an override.
   history-limit has a non-nil default of 2000."
  (with-isolated-config
    (let* ((win  (make-fake-window 1 "w" :npanes 1))
           (pane (first (cl-tmux/model:window-panes win))))
      (is (eql 2000 (cl-tmux/options:get-option-for-context
                     "history-limit" :pane pane :window win))
          "must return the history-limit default 2000 with no local overrides"))))

(test get-option-for-context-pane-falsey-honored-over-window
  "Explicit: global on, window on, pane off → pane-local falsey override wins
   and get-option-for-context returns NIL (present-p honored at every level)."
  (with-isolated-config
    (cl-tmux/options:set-option "synchronize-panes" t)      ; global = T
    (let* ((win  (make-fake-window 1 "w" :npanes 1))
           (pane (first (cl-tmux/model:window-panes win))))
      (cl-tmux/options:set-option-for-window "synchronize-panes" "on" win)  ; window = T
      (cl-tmux/options:set-option-for-pane   "synchronize-panes" "off" pane) ; pane = NIL
      (is (null (cl-tmux/options:get-option-for-context
                 "synchronize-panes" :pane pane :window win))
          "pane-local off (NIL) must win over both window on and global on")
      ;; The window/global values themselves are unchanged (override is pane-local).
      (is (eq t (cl-tmux/options:get-option-for-window "synchronize-panes" win))
          "window-local value must remain T (pane override is local only)")
      (is (eq t (cl-tmux/options:get-option "synchronize-panes"))
          "global value must remain T (pane override is local only)"))))

(test get-option-for-context-window-falsey-over-global
  "Mirror of the pane-over-window falsey test, one level up: global on, window
   off (NIL) → the WINDOW present-p branch honors the present-but-falsey
   window-local value over the truthy global, so get-option-for-context returns
   NIL when only :window is supplied."
  (with-isolated-config
    (cl-tmux/options:set-option "synchronize-panes" t)            ; global = T
    (let ((win (make-fake-window 1 "w" :npanes 1)))
      (cl-tmux/options:set-option-for-window "synchronize-panes" "off" win)  ; window = NIL
      (is (null (cl-tmux/options:get-option-for-context
                 "synchronize-panes" :window win))
          "window-local off (NIL) must win over global on (T) — window falsey honored")
      ;; The global value itself is unchanged (override is window-local).
      (is (eq t (cl-tmux/options:get-option "synchronize-panes"))
          "global value must remain T (window override is local only)"))))

(test get-option-for-context-global-falsey-over-default
  "Proves the GLOBAL present-p branch returns a present global value rather than
   falling through to the registry default.  history-limit's registry default is
   2000; set the global to a distinguishable sentinel (1) and assert a fresh
   window/pane with no local override resolves to the global value, not 2000."
  (with-isolated-config
    (cl-tmux/options:set-option "history-limit" 1)               ; global differs from default 2000
    (let* ((win  (make-fake-window 1 "w" :npanes 1))
           (pane (first (cl-tmux/model:window-panes win))))
      (is (eql 1 (cl-tmux/options:get-option-for-context
                  "history-limit" :pane pane :window win))
          "must return the present global value 1, not the registry default 2000"))))

;;; ── Command-alias registry ───────────────────────────────────────────────

(test register-and-lookup-command-alias
  "register-command-alias stores an alias retrievable by lookup-command-alias."
  (let ((cl-tmux/options:*command-aliases* (make-hash-table :test #'equal)))
    (cl-tmux/options:register-command-alias "e" "new-window -n")
    (is (string= "new-window -n"
                 (cl-tmux/options:lookup-command-alias "e"))
        "lookup-command-alias must return the registered expansion")))

(test lookup-command-alias-returns-nil-when-absent
  "lookup-command-alias returns NIL for an unregistered alias."
  (let ((cl-tmux/options:*command-aliases* (make-hash-table :test #'equal)))
    (is (null (cl-tmux/options:lookup-command-alias "nonexistent-alias-xyz"))
        "absent alias must return NIL")))

(test list-command-aliases-returns-sorted-alist
  "list-command-aliases returns an alist sorted alphabetically by alias name."
  (let ((cl-tmux/options:*command-aliases* (make-hash-table :test #'equal)))
    (cl-tmux/options:register-command-alias "z" "zoom-toggle")
    (cl-tmux/options:register-command-alias "a" "attach-session")
    (let ((result (cl-tmux/options:list-command-aliases)))
      (is (listp result) "list-command-aliases must return a list")
      (is (= 2 (length result))
          "must have exactly 2 aliases, got ~D" (length result))
      (is (string= "a" (caar result))
          "first alias must be \"a\" (alphabetical), got ~S" (caar result))
      (is (string= "z" (caadr result))
          "second alias must be \"z\" (alphabetical), got ~S" (caadr result)))))

(test list-command-aliases-empty
  "list-command-aliases returns NIL when no aliases are registered."
  (let ((cl-tmux/options:*command-aliases* (make-hash-table :test #'equal)))
    (is (null (cl-tmux/options:list-command-aliases))
        "list-command-aliases must return NIL when registry is empty")))

;;; ── Style-option classification + style-aware append ─────────────────────────

(test style-option-p-classification
  "style-option-p recognises *-style options but excludes clock-mode-style (a
   12/24-hour choice that merely shares the suffix)."
  (is-true  (cl-tmux/options:style-option-p "status-style"))
  (is-true  (cl-tmux/options:style-option-p "window-status-current-style"))
  (is-true  (cl-tmux/options:style-option-p "pane-active-border-style"))
  (is-true  (cl-tmux/options:style-option-p "mode-style"))
  (is-false (cl-tmux/options:style-option-p "clock-mode-style"))
  (is-false (cl-tmux/options:style-option-p "status-left"))
  (is-false (cl-tmux/options:style-option-p "status"))
  (is-false (cl-tmux/options:style-option-p "-style")))

(test append-option-value-style-vs-plain
  "append-option-value comma-joins non-empty style values, plain-concats other
   options, and never emits a stray comma when an operand is empty."
  (dolist (c '(("status-style" "bg=red" "fg=blue" "bg=red,fg=blue" "non-empty style → comma separator")
               ("status-style" ""       "fg=blue" "fg=blue"        "empty old → no leading comma")
               ("status-style" "bg=red" ""        "bg=red"         "empty new → no trailing comma")
               ("status-left"  "A"      "B"       "AB"             "non-style → separator-less concat")))
    (destructuring-bind (name old new expected desc) c
      (is (string= expected (cl-tmux/options:append-option-value name old new)) "~A" desc))))
