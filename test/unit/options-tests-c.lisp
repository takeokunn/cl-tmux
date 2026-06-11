(in-package #:cl-tmux/test)

;;;; Options tests — part C: type-coercion dispatch, option-table macro,
;;;; spec accessors, server options, show-option sorting, status defaults.

(in-suite options-suite)

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
    (is (eq :string (cl-tmux/options:option-spec-type spec))
        "option-spec-type for status must be :string (choice off|on|2..5)")
    (is (string= "on" (cl-tmux/options:option-spec-default spec))
        "option-spec-default for status must be \"on\"")))

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
  (with-fresh-server-options
    (is (= 99 (cl-tmux/options:get-server-option "nonexistent-server-opt" 99))
        "get-server-option must return the default for an absent key")))

(test get-server-option-returns-nil-when-absent-no-default
  "get-server-option returns NIL for an absent key when no default is given."
  (with-fresh-server-options
    (is (null (cl-tmux/options:get-server-option "nonexistent-server-opt"))
        "get-server-option must return NIL when absent and no default supplied")))

;;; ── set-server-option for unknown option (passthrough) ───────────────────

(test set-server-option-unknown-stores-as-is
  "set-server-option for an unregistered option stores the value without coercion."
  (with-fresh-server-options
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

(test status-default-is-on-string
  "The status option defaults to the string \"on\" (enabled; status-line-count → 1)."
  (is (string= "on" (cl-tmux/options:get-option "status"))
      "status default must be \"on\""))

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

