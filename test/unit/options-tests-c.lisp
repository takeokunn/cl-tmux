(in-package #:cl-tmux/test)

;;;; Options tests — part C: type-coercion dispatch, option-table macro,
;;;; spec accessors, server options, show-option sorting, status defaults.

(in-suite options-suite)

;;; ── define-type-coercions ecase dispatch ─────────────────────────────────

(test type-coercions-boolean-table
  "%coerce-value :boolean coerces strings, integers, and non-string values correctly."
  (dolist (c '(("on"    t   "on → T")
               ("true"  t   "true → T")
               ("1"     t   "1 → T")
               ("off"   nil "off → NIL")
               ("false" nil "false → NIL")
               ("0"     nil "0 → NIL")
               (42      t   "42 → T")
               (nil     nil "nil → NIL")))
    (destructuring-bind (input expected desc) c
      (is (eq expected (cl-tmux/options::%coerce-value :boolean input)) "~A" desc))))

(test type-coercions-integer-table
  "%coerce-value :integer parses strings and truncates floats; nil/garbage → 0."
  (dolist (c '(("42"           42 "numeric string → 42")
               ("not-a-number" 0  "non-numeric string → 0")
               (3.7            3  "float → truncated to 3")
               (nil            0  "nil → 0")))
    (destructuring-bind (input expected desc) c
      (is (= expected (cl-tmux/options::%coerce-value :integer input)) "~A" desc))))

(test type-coercions-string
  "%coerce-value :string formats any value as a string."
  (dolist (c '((42      "42"    "integer → decimal string")
               (t       "T"     "T → \"T\"")
               ("hello" "hello" "string passes through unchanged")))
    (destructuring-bind (input expected desc) c
      (is (string= expected (cl-tmux/options::%coerce-value :string input)) "~A" desc))))

;;; ── set-option unregistered-option passthrough path ──────────────────────

(test set-option-unregistered-stores-as-is-table
  "set-option stores unregistered values without coercion for any value type."
  (dolist (c '(("custom-unknown-option" "raw-value" "string stored as-is")
               ("custom-int-option"     99          "integer stored as-is")))
    (destructuring-bind (name value desc) c
      (with-fresh-options
        (cl-tmux/options:set-option name value)
        (is (equal value (cl-tmux/options:get-option name)) "~A" desc)))))

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

;;; ── option defaults table ────────────────────────────────────────────────

(test option-defaults-table
  "Key global options return the expected default values from *option-registry*."
  (dolist (c '(("status-position"   "bottom")
               ("base-index"        0)
               ("mouse"             nil)
               ("synchronize-panes" nil)
               ("status-interval"   15)
               ("history-limit"     2000)
               ("status"            "on")))
    (destructuring-bind (name expected) c
      (is (equal expected (cl-tmux/options:get-option name))
          "~A default must be ~S" name expected))))

;;; ── make-option-spec constructor ─────────────────────────────────────────

(test make-option-spec-table
  "make-option-spec stores type, default, and name correctly for each type."
  (dolist (c '((:boolean nil    "my-opt" "boolean with nil default")
               (:integer 42     "count"  "integer with 42 default")
               (:string  "hello" "label" "string with hello default")))
    (destructuring-bind (type default name desc) c
      (let ((spec (cl-tmux/options:make-option-spec :name name :type type :default default)))
        (is (not (null spec))           "~A: non-nil spec" desc)
        (is (string= name (cl-tmux/options:option-spec-name spec))    "~A: name" desc)
        (is (eq type (cl-tmux/options:option-spec-type spec))         "~A: type" desc)
        (is (equal default (cl-tmux/options:option-spec-default spec)) "~A: default" desc)))))

