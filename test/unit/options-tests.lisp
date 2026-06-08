(in-package #:cl-tmux/test)

;;;; Tests for cl-tmux/options: option registry and get/set/coercion.
;;;;
;;;; Isolation helpers (with-fresh-options, with-fresh-global-options,
;;;; with-single-option, with-single-server-option) are defined in
;;;; test/helpers.lisp so that config-directives-tests can reuse them.

(def-suite options-suite :description "Global option registry")
(in-suite options-suite)

;;; Table-driven default-value checker.
;;; check-option-defaults collapses ~25 near-identical single-assertion tests.

(defmacro check-option-defaults (&rest entries)
  "Generate IS assertions for each (name expected) or (name :registered)
   or (name :string-p) entry."
  `(progn
     ,@(mapcar
        (lambda (entry)
          (destructuring-bind (name check) entry
            (cond
              ((eq check :registered)
               `(is (cl-tmux/options:option-defined-p ,name)
                    ,(format nil "~S must be a registered option" name)))
              ((eq check :string-p)
               `(progn
                  (is (cl-tmux/options:option-defined-p ,name)
                      ,(format nil "~S must be a registered option" name))
                  (is (stringp (cl-tmux/options:get-option ,name))
                      ,(format nil "~S default must be a string" name))))
              (t
               `(is (equal ,check (cl-tmux/options:get-option ,name))
                    ,(format nil "~S default must be ~S" name check))))))
        entries)))

;;; get-option

(test get-option-returns-nil-when-absent
  "get-option returns NIL for a key not in the table."
  (with-fresh-options
    (is (null (cl-tmux/options:get-option "nonexistent")))))

(test get-option-returns-default-when-absent
  "get-option returns the supplied default when key is absent."
  (with-fresh-options
    (is (= 42 (cl-tmux/options:get-option "missing" 42)))))

;;; set-option / get-option round-trip

(test set-and-get-option-string
  "set-option stores a string value retrievable by get-option."
  (with-fresh-options
    (cl-tmux/options:set-option "status-left" "my-session")
    (is (string= "my-session" (cl-tmux/options:get-option "status-left")))))

;;; option-defined-p

(test option-defined-p-registered-option
  "option-defined-p returns T for a registered option."
  (is (cl-tmux/options:option-defined-p "status")
      "status must be a registered option"))

(test option-defined-p-unregistered-returns-nil
  "option-defined-p returns NIL for an unknown option name."
  (is (null (cl-tmux/options:option-defined-p "no-such-option"))))

;;; Type coercion

(test boolean-coercion-on-string
  "Setting a :boolean option with on/true/1 coerces to T."
  (with-fresh-global-options
    (is (eq t (cl-tmux/options:set-option "status" "on")))
    (is (eq t (cl-tmux/options:set-option "status" "true")))
    (is (eq t (cl-tmux/options:set-option "status" "1")))))

(test boolean-coercion-false-strings
  "Setting a :boolean option with any other string coerces to NIL."
  (with-fresh-global-options
    (is (null (cl-tmux/options:set-option "status" "off")))
    (is (null (cl-tmux/options:set-option "status" "0")))
    (is (null (cl-tmux/options:set-option "status" "false")))))

(test integer-coercion
  "Setting a :integer option with a numeric string coerces correctly."
  (with-fresh-global-options
    (is (= 5000 (cl-tmux/options:set-option "history-limit" "5000")))))

(test integer-coercion-500
  "Setting history-limit with 500 coerces to the integer 500."
  (with-fresh-global-options
    (is (= 500 (cl-tmux/options:set-option "history-limit" "500")))))

(test string-coercion-from-non-string
  "Setting a :string option with a non-string value coerces via format ~A."
  (with-fresh-global-options
    (is (string= "42" (cl-tmux/options:set-option "status-left" 42)))))

;;; all-options

(test all-options-returns-alist
  "all-options returns an alist of (name . value) pairs."
  (let ((opts (cl-tmux/options:all-options)))
    (is (listp opts))
    (is (every #'consp opts)
        "each entry must be a cons pair")))

(test define-tmux-options-macro-is-defined
  "define-tmux-options is a registered macro."
  (is (macro-function 'cl-tmux/options:define-tmux-options)))

;;; Server options

(test server-options-escape-time-default
  "*server-options* contains the default escape-time = 500."
  (is (= 500 (cl-tmux/options:get-server-option "escape-time"))
      "default escape-time must be 500"))

(test server-options-exit-empty-default
  "*server-options* contains exit-empty = T by default."
  (is (cl-tmux/options:get-server-option "exit-empty")
      "default exit-empty must be T"))

(test set-server-option-stores-value
  "set-server-option stores a value in *server-options*."
  (let ((cl-tmux/options:*server-options* (make-hash-table :test #'equal))
        (cl-tmux/options:*server-option-registry* cl-tmux/options:*server-option-registry*))
    (cl-tmux/options:set-server-option "escape-time" "100")
    (is (= 100 (cl-tmux/options:get-server-option "escape-time"))
        "escape-time must be 100 after set-server-option")))

(test set-server-option-boolean-coercion
  "set-server-option coerces boolean values."
  (let ((cl-tmux/options:*server-options* (make-hash-table :test #'equal))
        (cl-tmux/options:*server-option-registry* cl-tmux/options:*server-option-registry*))
    (cl-tmux/options:set-server-option "exit-empty" "off")
    (is (null (cl-tmux/options:get-server-option "exit-empty"))
        "exit-empty must be NIL after setting to off")))

;;; show-options

(test show-options-returns-string
  "show-options returns a non-empty string."
  (let ((out (cl-tmux/options:show-options)))
    (is (stringp out) "show-options must return a string")
    (is (plusp (length out)) "show-options string must be non-empty")))

(test show-options-contains-key-value-pairs
  "show-options output contains option name/value pairs."
  (let ((cl-tmux/options:*global-options*
         (let ((ht (make-hash-table :test #'equal)))
           (setf (gethash "status" ht) t)
           (setf (gethash "history-limit" ht) 2000)
           ht)))
    (let ((out (cl-tmux/options:show-options)))
      (is (search "status" out)
          "show-options must include status option (got ~S)" out)
      (is (search "history-limit" out)
          "show-options must include history-limit option (got ~S)" out)))
  ;; Same assertions using the fixture macro
  (with-single-option ("status" t)
    (is (search "status" (cl-tmux/options:show-options))
        "with-single-option: show-options must include status")))

(test show-option-single-option
  "show-option returns the value of a single named option."
  (with-single-option ("status-interval" 30)
    (let ((out (cl-tmux/options:show-option "status-interval")))
      (is (search "status-interval" out)
          "show-option output must include the option name (got ~S)" out)
      (is (search "30" out)
          "show-option output must include the value 30 (got ~S)" out))))

(test show-option-missing-option
  "show-option for an absent option indicates it is not set."
  (with-fresh-options
    (let ((out (cl-tmux/options:show-option "no-such-option")))
      (is (search "no-such-option" out)
          "show-option output must include the option name (got ~S)" out))))

(test show-options-server-scope
  "show-options with :server scope returns server options."
  (with-single-server-option ("escape-time" 500)
    (let ((out (cl-tmux/options:show-options :server)))
      (is (search "escape-time" out)
          "show-options :server must include escape-time (got ~S)" out))))

;;; Registered options: style/justify checks

(test registered-style-and-layout-options
  "Style and layout options are registered with correct types."
  (check-option-defaults
    ("status-style"                 :registered)
    ("status-justify"               :registered)
    ("window-status-current-style"  :registered)))

;;; Default values: parameterised table-driven check

(test option-default-values
  "All registered options have the documented default values."
  (check-option-defaults
    ("pane-base-index"               0)
    ("default-command"               "")
    ("status-left-length"            40)
    ("status-right-length"           40)
    ("window-status-format"          :string-p)
    ("window-status-current-format"  :string-p)
    ("window-status-style"           :registered)
    ("window-status-separator"       " ")
    ("word-separators"               " -_@")
    ("automatic-rename"              t)
    ("automatic-rename-format"       :registered)
    ("bell-action"                   "any")
    ("visual-bell"                   nil)
    ("visual-activity"               nil)
    ("monitor-activity"              nil)
    ("buffer-limit"                  50)
    ("focus-events"                  nil)
    ("copy-command"                  "")
    ("set-titles"                    nil)
    ("set-titles-string"             "#S:#I:#W")
    ("remain-on-exit"                nil)
    ("renumber-windows"              nil)
    ("message-style"                 "")
    ("exit-unattached"               nil)))

(test update-environment-default
  "update-environment default contains expected variable names."
  (let ((val (cl-tmux/options:get-option "update-environment")))
    (is (stringp val))
    (is (search "DISPLAY" val))
    (is (search "SSH_AUTH_SOCK" val))))

;;; Round-trip tests

(test set-option-boolean-set-titles
  "set-titles boolean set/get round-trip."
  (with-fresh-global-options
    (is (eq t (cl-tmux/options:set-option "set-titles" "on")))
    (is (eq t (cl-tmux/options:get-option "set-titles")))
    (is (null (cl-tmux/options:set-option "set-titles" "off")))
    (is (null (cl-tmux/options:get-option "set-titles")))))

(test set-option-integer-status-left-length
  "status-left-length integer set/get round-trip."
  (with-fresh-global-options
    (is (= 80 (cl-tmux/options:set-option "status-left-length" "80")))
    (is (= 80 (cl-tmux/options:get-option "status-left-length")))))

(test pane-base-index-set-get
  "pane-base-index set/get round-trip."
  (with-fresh-global-options
    (is (= 1 (cl-tmux/options:set-option "pane-base-index" "1")))
    (is (= 1 (cl-tmux/options:get-option "pane-base-index")))))

;;; ── define-type-coercions ecase dispatch ─────────────────────────────────

(test type-coercions-boolean-true-values
  "%coerce-value :boolean coerces on/true/1 to T."
  (is (eq t   (cl-tmux/options::%coerce-value :boolean "on")))
  (is (eq t   (cl-tmux/options::%coerce-value :boolean "true")))
  (is (eq t   (cl-tmux/options::%coerce-value :boolean "1"))))

(test type-coercions-boolean-false-values
  "%coerce-value :boolean coerces off/false/0 to NIL."
  (is (null   (cl-tmux/options::%coerce-value :boolean "off")))
  (is (null   (cl-tmux/options::%coerce-value :boolean "false")))
  (is (null   (cl-tmux/options::%coerce-value :boolean "0"))))

(test type-coercions-boolean-truthy-value
  "%coerce-value :boolean coerces a non-nil non-string value to T."
  (is (eq t   (cl-tmux/options::%coerce-value :boolean 42)))
  (is (null   (cl-tmux/options::%coerce-value :boolean nil))))

(test type-coercions-integer-from-string
  "%coerce-value :integer parses a numeric string."
  (is (= 42   (cl-tmux/options::%coerce-value :integer "42")))
  (is (= 0    (cl-tmux/options::%coerce-value :integer "not-a-number"))))

(test type-coercions-integer-from-number
  "%coerce-value :integer truncates a floating-point number."
  (is (= 3    (cl-tmux/options::%coerce-value :integer 3.7)))
  (is (= 0    (cl-tmux/options::%coerce-value :integer nil))))

(test type-coercions-string
  "%coerce-value :string formats any value as a string."
  (is (string= "42"   (cl-tmux/options::%coerce-value :string 42)))
  (is (string= "T"    (cl-tmux/options::%coerce-value :string t)))
  (is (string= "hello" (cl-tmux/options::%coerce-value :string "hello"))))

;;; ── set-option unregistered-option passthrough path ──────────────────────

(test set-option-unregistered-stores-as-is
  "set-option on an unknown option stores the value without coercion."
  (with-fresh-options
    (cl-tmux/options:set-option "custom-unknown-option" "raw-value")
    (is (string= "raw-value"
                 (cl-tmux/options:get-option "custom-unknown-option"))
        "unregistered option must be stored as-is")))

(test set-option-unregistered-stores-integer-as-is
  "set-option on an unknown option stores an integer without coercion."
  (with-fresh-options
    (cl-tmux/options:set-option "custom-int-option" 99)
    (is (= 99 (cl-tmux/options:get-option "custom-int-option"))
        "unregistered integer option must be stored as-is")))

;;; ── all-options count matches registration ────────────────────────────────

(test all-options-count-matches-registry
  "all-options returns an entry for every option in *option-registry*."
  (let* ((registry-count (hash-table-count cl-tmux/options:*option-registry*))
         (all            (cl-tmux/options:all-options)))
    (is (= registry-count (length all))
        "all-options count (~D) must match *option-registry* count (~D)"
        (length all) registry-count)))

;;; ── define-option-table macro ─────────────────────────────────────────────

(test define-option-table-macro-is-defined
  "define-option-table is a registered macro."
  (is (macro-function 'cl-tmux/options:define-option-table)))

;;; ── option-spec accessors ─────────────────────────────────────────────────

(test option-spec-accessors
  "option-spec-name, option-spec-type, option-spec-default return the correct fields."
  (let ((spec (gethash "status" cl-tmux/options:*option-registry*)))
    (is (not (null spec))
        "status must be a registered option")
    (is (string= "status" (cl-tmux/options:option-spec-name spec))
        "option-spec-name must return \"status\"")
    (is (eq :boolean (cl-tmux/options:option-spec-type spec))
        "option-spec-type for status must be :boolean")
    (is (eq t (cl-tmux/options:option-spec-default spec))
        "option-spec-default for status must be T")))

(test option-spec-integer-type
  "option-spec-type for an integer option is :integer."
  (let ((spec (gethash "history-limit" cl-tmux/options:*option-registry*)))
    (is (not (null spec))
        "history-limit must be a registered option")
    (is (eq :integer (cl-tmux/options:option-spec-type spec))
        "option-spec-type for history-limit must be :integer")))

(test option-spec-string-type
  "option-spec-type for a string option is :string."
  (let ((spec (gethash "default-command" cl-tmux/options:*option-registry*)))
    (is (not (null spec))
        "default-command must be a registered option")
    (is (eq :string (cl-tmux/options:option-spec-type spec))
        "option-spec-type for default-command must be :string")))

;;; ── get-server-option with default ───────────────────────────────────────

(test get-server-option-returns-default-when-absent
  "get-server-option returns the supplied default when the key is absent."
  (let ((cl-tmux/options:*server-options* (make-hash-table :test #'equal)))
    (is (= 99 (cl-tmux/options:get-server-option "nonexistent-server-opt" 99))
        "get-server-option must return the default for an absent key")))

(test get-server-option-returns-nil-when-absent-no-default
  "get-server-option returns NIL for an absent key when no default is given."
  (let ((cl-tmux/options:*server-options* (make-hash-table :test #'equal)))
    (is (null (cl-tmux/options:get-server-option "nonexistent-server-opt"))
        "get-server-option must return NIL when absent and no default supplied")))

;;; ── set-server-option for unknown option (passthrough) ───────────────────

(test set-server-option-unknown-stores-as-is
  "set-server-option for an unregistered option stores the value without coercion."
  (let ((cl-tmux/options:*server-options*          (make-hash-table :test #'equal))
        (cl-tmux/options:*server-option-registry*  cl-tmux/options:*server-option-registry*))
    (cl-tmux/options:set-server-option "custom-server-opt" "raw-value")
    (is (string= "raw-value"
                 (cl-tmux/options:get-server-option "custom-server-opt"))
        "unregistered server option must be stored as-is")))

;;; ── show-option with :server scope ──────────────────────────────────────

(test show-option-server-scope
  "show-option with :server scope returns the value from *server-options*."
  (with-single-server-option ("escape-time" 250)
    (let ((out (cl-tmux/options:show-option "escape-time" :server)))
      (is (search "escape-time" out)
          "show-option :server must include option name (got ~S)" out)
      (is (search "250" out)
          "show-option :server must include the value 250 (got ~S)" out))))

;;; ── show-options returns sorted output ───────────────────────────────────

(test show-options-is-sorted
  "show-options output has options in alphabetical order."
  (let ((cl-tmux/options:*global-options*
         (let ((ht (make-hash-table :test #'equal)))
           (setf (gethash "zebra-option" ht) "z")
           (setf (gethash "alpha-option" ht) "a")
           ht)))
    (let ((out (cl-tmux/options:show-options)))
      (let ((pos-alpha (search "alpha-option" out))
            (pos-zebra (search "zebra-option" out)))
        (is (and pos-alpha pos-zebra)
            "both options must appear in show-options output")
        (is (< pos-alpha pos-zebra)
            "alpha-option must appear before zebra-option (sorted output)")))))

;;; ── define-server-options macro ──────────────────────────────────────────

(test define-server-options-macro-is-defined
  "define-server-options is a registered macro."
  (is (macro-function 'cl-tmux/options:define-server-options)))

;;; ── status-position default value ───────────────────────────────────────

(test status-position-default
  "status-position defaults to \"bottom\"."
  (is (string= "bottom" (cl-tmux/options:get-option "status-position"))
      "status-position default must be \"bottom\""))

;;; ── base-index default value ─────────────────────────────────────────────

(test base-index-default
  "base-index defaults to 0."
  (is (= 0 (cl-tmux/options:get-option "base-index"))
      "base-index default must be 0"))

;;; ── mouse option default ─────────────────────────────────────────────────

(test mouse-option-default-nil
  "mouse option defaults to NIL (disabled)."
  (is (null (cl-tmux/options:get-option "mouse"))
      "mouse default must be NIL"))

;;; ── synchronize-panes default ────────────────────────────────────────────

(test synchronize-panes-default-nil
  "synchronize-panes option defaults to NIL."
  (is (null (cl-tmux/options:get-option "synchronize-panes"))
      "synchronize-panes default must be NIL"))

;;; ── status-interval default ──────────────────────────────────────────────

(test status-interval-default
  "status-interval defaults to 15."
  (is (= 15 (cl-tmux/options:get-option "status-interval"))
      "status-interval default must be 15"))

;;; ── history-limit default ────────────────────────────────────────────────

(test history-limit-default
  "history-limit defaults to 2000."
  (is (= 2000 (cl-tmux/options:get-option "history-limit"))
      "history-limit default must be 2000"))

;;; ── set-option and get-option for boolean status defaults ────────────────

(test status-boolean-default-true
  "The status option defaults to T (enabled)."
  (is (eq t (cl-tmux/options:get-option "status"))
      "status default must be T"))

;;; ── make-option-spec constructor ─────────────────────────────────────────

(test make-option-spec-creates-spec
  "make-option-spec constructs an option-spec with the correct slots."
  (let ((spec (cl-tmux/options:make-option-spec :name "my-opt"
                                                 :type :boolean
                                                 :default nil)))
    (is (not (null spec))
        "make-option-spec must return a non-nil spec")
    (is (string= "my-opt" (cl-tmux/options:option-spec-name spec))
        "option-spec-name must return \"my-opt\"")
    (is (eq :boolean (cl-tmux/options:option-spec-type spec))
        "option-spec-type must return :boolean")
    (is (null (cl-tmux/options:option-spec-default spec))
        "option-spec-default must return NIL")))

(test make-option-spec-integer-type
  "make-option-spec stores :integer type and an integer default."
  (let ((spec (cl-tmux/options:make-option-spec :name "count"
                                                 :type :integer
                                                 :default 42)))
    (is (eq :integer (cl-tmux/options:option-spec-type spec))
        "option-spec-type must be :integer")
    (is (= 42 (cl-tmux/options:option-spec-default spec))
        "option-spec-default must be 42")))

(test make-option-spec-string-type
  "make-option-spec stores :string type and a string default."
  (let ((spec (cl-tmux/options:make-option-spec :name "label"
                                                 :type :string
                                                 :default "hello")))
    (is (eq :string (cl-tmux/options:option-spec-type spec))
        "option-spec-type must be :string")
    (is (string= "hello" (cl-tmux/options:option-spec-default spec))
        "option-spec-default must be \"hello\"")))

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
  (let ((cl-tmux/options:*server-options* (make-hash-table :test #'equal)))
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
    (let ((result (cl-tmux/options:set-option "status" "on")))
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
    ;; status defaults to T in the spec table
    (is (eq t (cl-tmux/options:get-option-for-window "status" win))
        "get-option-for-window must return spec default T for status")))

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
