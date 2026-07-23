(in-package #:cl-tmux/test)

;;;; options tests — part B: define-option-accessor, define-type-coercions,
;;;; table-driven coercion checks, scoped overrides, show-options, list-options,
;;;; per-window option resolution, option default shadowing, server options.

(describe "options-suite"

  ;;; ── define-option-accessor macro ─────────────────────────────────────────

  ;; define-option-accessor is a registered macro.
  (it "define-option-accessor-macro-is-defined"
    (expect (macro-function 'cl-tmux/options::define-option-accessor)))

  ;;; ── define-type-coercions macro ──────────────────────────────────────────

  ;; define-type-coercions is a registered macro.
  (it "define-type-coercions-macro-is-defined"
    (expect (macro-function 'cl-tmux/options::define-type-coercions)))

  ;;; ── Table-driven coercion checks ─────────────────────────────────────────
  ;;;
  ;;; Consolidates the repeated %coerce-value assertions into a single
  ;;; parameterised block covering all three type branches.

  ;; %coerce-value behaves correctly across all registered type branches.
  (it "coerce-value-table-driven"
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
          (expect (equal expected result))))))

  ;;; ── show-option :server scope when absent ────────────────────────────────

  ;; show-option :server for an absent server option says 'not set'.
  (it "show-option-server-scope-absent"
    (with-fresh-server-options
      (let ((out (cl-tmux/options:show-option "nonexistent-server-opt" :server)))
        (expect (search "nonexistent-server-opt" out)))))

  ;;; ── set-option returns coerced value ─────────────────────────────────────

  ;; set-option returns the coerced value, not the input.
  (it "set-option-returns-coerced-value"
    (with-fresh-global-options
      (let ((result (cl-tmux/options:set-option "history-limit" "1234")))
        (expect (= 1234 result)))
      (let ((result (cl-tmux/options:set-option "mouse" "on")))
        (expect (eq t result)))
      (let ((result (cl-tmux/options:set-option "status-left" "text")))
        (expect (string= "text" result)))))

  ;;; ── integer coercion: non-numeric non-nil value falls back to 0 ──────────

  ;; %coerce-value :integer returns 0 for non-numeric non-string non-number input.
  (it "integer-coercion-non-numeric-falls-back-to-zero"
    (expect (= 0 (cl-tmux/options::%coerce-value :integer t)))
    (expect (= 0 (cl-tmux/options::%coerce-value :integer :foo))))

  ;;; ── *server-option-registry* is a hash-table ─────────────────────────────

  ;; *server-option-registry* is a hash-table populated with at least the three
  ;; standard server options.
  (it "server-option-registry-is-hash-table"
    (expect (hash-table-p cl-tmux/options:*server-option-registry*))
    (dolist (name '("escape-time" "exit-empty" "exit-unattached"))
      (expect (not (null (gethash name cl-tmux/options:*server-option-registry*))))))

  ;;; ── exit-unattached server option default ────────────────────────────────

  ;; *server-options* contains exit-unattached = NIL by default.
  (it "server-options-exit-unattached-default"
    (expect (null (cl-tmux/options:get-server-option "exit-unattached"))))

  ;;; ── Per-window scoped option tests ───────────────────────────────────────

  ;; set-option-for-window stores the value in the window's local-options hash.
  (it "set-option-for-window-stores-in-local-hash"
    (let ((win (cl-tmux/model:make-window :id 1 :name "test-win")))
      (cl-tmux/options:set-option-for-window "synchronize-panes" t win)
      (expect (eq t (gethash "synchronize-panes"
                             (cl-tmux/model:window-local-options win))))))

  ;; get-option-for-window returns the window-local value when present,
  ;; even when the global option has a different value.
  (it "get-option-for-window-returns-local-override"
    (let ((win (cl-tmux/model:make-window :id 1 :name "test-win"))
          (cl-tmux/options:*global-options*
           (let ((ht (make-hash-table :test #'equal)))
             (setf (gethash "synchronize-panes" ht) nil)
             ht)))
      (cl-tmux/options:set-option-for-window "synchronize-panes" t win)
      (expect (eq t (cl-tmux/options:get-option-for-window "synchronize-panes" win)))))

  ;; get-option-for-window returns the global value when no local override is set.
  (it "get-option-for-window-falls-back-to-global"
    (let ((win (cl-tmux/model:make-window :id 1 :name "test-win"))
          (cl-tmux/options:*global-options*
           (let ((ht (make-hash-table :test #'equal)))
             (setf (gethash "history-limit" ht) 9999)
             ht)))
      (expect (= 9999 (cl-tmux/options:get-option-for-window "history-limit" win)))))

  ;; get-option-for-window returns the registered spec default when absent from
  ;; both the local hash and *global-options*.
  (it "get-option-for-window-falls-back-to-spec-default"
    (let ((win (cl-tmux/model:make-window :id 1 :name "test-win"))
          (cl-tmux/options:*global-options* (make-hash-table :test #'equal)))
      ;; status defaults to the string "on" in the spec table
      (expect (string= "on" (cl-tmux/options:get-option-for-window "status" win)))))

  ;;; ── Per-pane scoped option tests ─────────────────────────────────────────

  ;; set-option-for-pane stores the value in the pane's local-options hash.
  (it "set-option-for-pane-stores-in-local-hash"
    (let ((p (cl-tmux/model:make-pane :id 1)))
      (cl-tmux/options:set-option-for-pane "mouse" t p)
      (expect (eq t (gethash "mouse" (cl-tmux/model:pane-local-options p))))))

  ;; get-option-for-pane returns the pane-local value when present,
  ;; even when the global option has a different value.
  (it "get-option-for-pane-returns-local-override"
    (let ((p (cl-tmux/model:make-pane :id 1))
          (cl-tmux/options:*global-options*
           (let ((ht (make-hash-table :test #'equal)))
             (setf (gethash "mouse" ht) nil)
             ht)))
      (cl-tmux/options:set-option-for-pane "mouse" t p)
      (expect (eq t (cl-tmux/options:get-option-for-pane "mouse" p)))))

  ;; get-option-for-pane returns the global value when no local override is set.
  (it "get-option-for-pane-falls-back-to-global"
    (let ((p (cl-tmux/model:make-pane :id 1))
          (cl-tmux/options:*global-options*
           (let ((ht (make-hash-table :test #'equal)))
             (setf (gethash "history-limit" ht) 7777)
             ht)))
      (expect (= 7777 (cl-tmux/options:get-option-for-pane "history-limit" p)))))

  ;; get-option-for-pane returns the registered spec default when absent from
  ;; both the local hash and *global-options*.
  (it "get-option-for-pane-falls-back-to-spec-default"
    (let ((p (cl-tmux/model:make-pane :id 1))
          (cl-tmux/options:*global-options* (make-hash-table :test #'equal)))
      ;; mouse defaults to NIL in the spec table
      (expect (null (cl-tmux/options:get-option-for-pane "mouse" p)))))

  ;; Two windows have independent local-options hashes.
  (it "window-local-options-isolated-between-windows"
    (let ((win-a (cl-tmux/model:make-window :id 1 :name "a"))
          (win-b (cl-tmux/model:make-window :id 2 :name "b")))
      (cl-tmux/options:set-option-for-window "mouse" t win-a)
      (expect (eq t   (cl-tmux/options:get-option-for-window "mouse" win-a)))
      (expect (null (gethash "mouse" (cl-tmux/model:window-local-options win-b))))))

  ;; Two panes have independent local-options hashes.
  (it "pane-local-options-isolated-between-panes"
    (let ((p1 (cl-tmux/model:make-pane :id 1))
          (p2 (cl-tmux/model:make-pane :id 2)))
      (cl-tmux/options:set-option-for-pane "mouse" t p1)
      (expect (eq t   (cl-tmux/options:get-option-for-pane "mouse" p1)))
      (expect (null (gethash "mouse" (cl-tmux/model:pane-local-options p2))))))

  ;;; ── Scoped accessors: boolean coercion + fallback chain (newly wired) ────

  ;; set-option-for-window coerces a :boolean string "on" to T before storing,
  ;; so get-option-for-window returns T (not the literal string).
  (it "set-option-for-window-coerces-boolean-string"
    (with-isolated-config
      (let ((win (make-fake-window 1 "w")))
        (cl-tmux/options:set-option-for-window "synchronize-panes" "on" win)
        (expect (eq t (cl-tmux/options:get-option-for-window "synchronize-panes" win))))))

  ;; With no window-local override, get-option-for-window returns the GLOBAL value.
  (it "get-option-for-window-falls-back-to-global-value"
    (with-isolated-config
      (cl-tmux/options:set-option "synchronize-panes" t)
      (let ((win (make-fake-window 1 "w")))
        (expect (eq t (cl-tmux/options:get-option-for-window "synchronize-panes" win))))))

  ;; A window-local override wins over the global value for that window, while the
  ;; global option itself remains unchanged.
  (it "set-option-for-window-overrides-global"
    (with-isolated-config
      (cl-tmux/options:set-option "synchronize-panes" nil)
      (let ((win (make-fake-window 1 "w")))
        (cl-tmux/options:set-option-for-window "synchronize-panes" "on" win)
        (expect (eq t (cl-tmux/options:get-option-for-window "synchronize-panes" win)))
        (expect (null (cl-tmux/options:get-option "synchronize-panes"))))))

  ;; set-option-for-pane coerces a :boolean string "on" to T and stores it per-pane;
  ;; get-option-for-pane returns T.
  (it "set-option-for-pane-coerces-boolean-string"
    (with-isolated-config
      (let* ((win  (make-fake-window 1 "w"))
             (pane (first (cl-tmux/model:window-panes win))))
        (cl-tmux/options:set-option-for-pane "remain-on-exit" "on" pane)
        (expect (eq t (cl-tmux/options:get-option-for-pane "remain-on-exit" pane))))))

  ;;; ── Falsey local override beats truthy global (present-p semantics) ──────

  ;; A window-local value explicitly set to a FALSEY value (synchronize-panes
  ;; "off" → NIL) must win over a truthy GLOBAL value, instead of falling
  ;; through the or-chain.  The global value itself is unchanged.
  (it "get-option-for-window-falsey-local-overrides-truthy-global"
    (with-isolated-config
      (cl-tmux/options:set-option "synchronize-panes" t)
      (let ((win (make-fake-window 1 "w")))
        (cl-tmux/options:set-option-for-window "synchronize-panes" "off" win)
        (expect (null (cl-tmux/options:get-option-for-window "synchronize-panes" win)))
        (expect (eq t (cl-tmux/options:get-option "synchronize-panes"))))))

  ;; A pane-local value explicitly set to a FALSEY value (remain-on-exit "off"
  ;; → NIL) must win over a truthy GLOBAL value, instead of falling through.
  (it "get-option-for-pane-falsey-local-overrides-truthy-global"
    (with-isolated-config
      (cl-tmux/options:set-option "remain-on-exit" t)
      (let* ((win  (make-fake-window 1 "w"))
             (pane (first (cl-tmux/model:window-panes win))))
        (cl-tmux/options:set-option-for-pane "remain-on-exit" "off" pane)
        (expect (null (cl-tmux/options:get-option-for-pane "remain-on-exit" pane)))
        (expect (eq t (cl-tmux/options:get-option "remain-on-exit"))))))

  ;; set-option-for-window coerces a non-boolean :integer string ("5000") to the
  ;; integer 5000 before storing, so get-option-for-window returns the integer
  ;; (not the literal string).
  (it "set-option-for-window-coerces-integer"
    (with-isolated-config
      (let ((win (make-fake-window 1 "w")))
        (cl-tmux/options:set-option-for-window "history-limit" "5000" win)
        (expect (eql 5000 (cl-tmux/options:get-option-for-window "history-limit" win))))))

  ;;; ── get-option-for-context: full pane→window→global→default precedence ──

  ;; get-option-for-context resolves with precedence pane-local > window-local >
  ;; global, and a present-but-falsey PANE override beats a truthy WINDOW value.
  ;; Uses the registered :boolean option synchronize-panes.
  (it "get-option-for-context-pane-beats-window-beats-global"
    (with-isolated-config
      (cl-tmux/options:set-option "synchronize-panes" nil)   ; global = NIL
      (let* ((win  (make-fake-window 1 "w" :npanes 1))
             (pane (first (cl-tmux/model:window-panes win))))
        ;; No local overrides → resolves to the global value (NIL).
        (expect (null (cl-tmux/options:get-option-for-context
                       "synchronize-panes" :pane pane :window win)))
        ;; Window-local "on" with no pane override → window value wins over global.
        (cl-tmux/options:set-option-for-window "synchronize-panes" "on" win)
        (expect (eq t (cl-tmux/options:get-option-for-context
                       "synchronize-panes" :pane pane :window win)))
        ;; Pane-local "off" (NIL) → present-but-falsey pane override beats window "on".
        (cl-tmux/options:set-option-for-pane "synchronize-panes" "off" pane)
        (expect (null (cl-tmux/options:get-option-for-context
                       "synchronize-panes" :pane pane :window win))))))

  ;; get-option-for-context skips a NIL pane/window level: with both NIL it equals
  ;; get-option; with only one scope it consults that scope's local override.
  (it "get-option-for-context-skips-nil-levels"
    (with-isolated-config
      (cl-tmux/options:set-option "synchronize-panes" nil)
      ;; Both pane and window NIL → equivalent to plain get-option.
      (expect (eq (cl-tmux/options:get-option "synchronize-panes")
                  (cl-tmux/options:get-option-for-context "synchronize-panes")))
      ;; Only :window supplied, with a window-local override → returns window value.
      (let* ((win  (make-fake-window 1 "w" :npanes 1))
             (pane (first (cl-tmux/model:window-panes win))))
        (cl-tmux/options:set-option-for-window "synchronize-panes" "on" win)
        (expect (eq t (cl-tmux/options:get-option-for-context
                       "synchronize-panes" :window win)))
        ;; Only :pane supplied, with a pane-local override → returns pane value.
        (cl-tmux/options:set-option-for-pane "synchronize-panes" "off" pane)
        (expect (null (cl-tmux/options:get-option-for-context
                       "synchronize-panes" :pane pane))))))

  ;; get-option-for-context returns the registered default (via the pre-populated
  ;; global store) when neither pane nor window carries an override.
  ;; history-limit has a non-nil default of 2000.
  (it "get-option-for-context-falls-back-to-registry-default"
    (with-isolated-config
      (let* ((win  (make-fake-window 1 "w" :npanes 1))
             (pane (first (cl-tmux/model:window-panes win))))
        (expect (eql 2000 (cl-tmux/options:get-option-for-context
                           "history-limit" :pane pane :window win))))))

  ;; Explicit: global on, window on, pane off → pane-local falsey override wins
  ;; and get-option-for-context returns NIL (present-p honored at every level).
  (it "get-option-for-context-pane-falsey-honored-over-window"
    (with-isolated-config
      (cl-tmux/options:set-option "synchronize-panes" t)      ; global = T
      (let* ((win  (make-fake-window 1 "w" :npanes 1))
             (pane (first (cl-tmux/model:window-panes win))))
        (cl-tmux/options:set-option-for-window "synchronize-panes" "on" win)  ; window = T
        (cl-tmux/options:set-option-for-pane   "synchronize-panes" "off" pane) ; pane = NIL
        (expect (null (cl-tmux/options:get-option-for-context
                       "synchronize-panes" :pane pane :window win)))
        ;; The window/global values themselves are unchanged (override is pane-local).
        (expect (eq t (cl-tmux/options:get-option-for-window "synchronize-panes" win)))
        (expect (eq t (cl-tmux/options:get-option "synchronize-panes"))))))

  ;; Mirror of the pane-over-window falsey test, one level up: global on, window
  ;; off (NIL) → the WINDOW present-p branch honors the present-but-falsey
  ;; window-local value over the truthy global, so get-option-for-context returns
  ;; NIL when only :window is supplied.
  (it "get-option-for-context-window-falsey-over-global"
    (with-isolated-config
      (cl-tmux/options:set-option "synchronize-panes" t)            ; global = T
      (let ((win (make-fake-window 1 "w" :npanes 1)))
        (cl-tmux/options:set-option-for-window "synchronize-panes" "off" win)  ; window = NIL
        (expect (null (cl-tmux/options:get-option-for-context
                       "synchronize-panes" :window win)))
        ;; The global value itself is unchanged (override is window-local).
        (expect (eq t (cl-tmux/options:get-option "synchronize-panes"))))))

  ;; Proves the GLOBAL present-p branch returns a present global value rather than
  ;; falling through to the registry default.  history-limit's registry default is
  ;; 2000; set the global to a distinguishable sentinel (1) and assert a fresh
  ;; window/pane with no local override resolves to the global value, not 2000.
  (it "get-option-for-context-global-falsey-over-default"
    (with-isolated-config
      (cl-tmux/options:set-option "history-limit" 1)               ; global differs from default 2000
      (let* ((win  (make-fake-window 1 "w" :npanes 1))
             (pane (first (cl-tmux/model:window-panes win))))
        (expect (eql 1 (cl-tmux/options:get-option-for-context
                        "history-limit" :pane pane :window win))))))

  ;;; ── Style-option classification + style-aware append ─────────────────────────

  ;; style-option-p recognises *-style options but excludes clock-mode-style (a
  ;; 12/24-hour choice that merely shares the suffix).
  (it "style-option-p-classification"
    (expect (cl-tmux/options:style-option-p "status-style") :to-be-truthy)
    (expect (cl-tmux/options:style-option-p "window-status-current-style") :to-be-truthy)
    (expect (cl-tmux/options:style-option-p "pane-active-border-style") :to-be-truthy)
    (expect (cl-tmux/options:style-option-p "mode-style") :to-be-truthy)
    (expect (cl-tmux/options:style-option-p "clock-mode-style") :to-be-falsy)
    (expect (cl-tmux/options:style-option-p "status-left") :to-be-falsy)
    (expect (cl-tmux/options:style-option-p "status") :to-be-falsy)
    (expect (cl-tmux/options:style-option-p "-style") :to-be-falsy))

  ;; append-option-value comma-joins non-empty style values, plain-concats other
  ;; options, and never emits a stray comma when an operand is empty.
  (it "append-option-value-style-vs-plain"
    (dolist (c '(("status-style" "bg=red" "fg=blue" "bg=red,fg=blue" "non-empty style -> comma separator")
                 ("status-style" ""       "fg=blue" "fg=blue"        "empty old -> no leading comma")
                 ("status-style" "bg=red" ""        "bg=red"         "empty new -> no trailing comma")
                 ("status-left"  "A"      "B"       "AB"             "non-style -> separator-less concat")))
      (destructuring-bind (name old new expected desc) c
        (declare (ignore desc))
        (expect (string= expected (cl-tmux/options:append-option-value name old new)))))))
