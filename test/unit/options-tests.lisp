(in-package #:cl-tmux/test)

;;;; Tests for cl-tmux/options: option registry and get/set/coercion.

(def-suite options-suite :description "Global option registry")
(in-suite options-suite)

;;; Isolation helpers.
;;; with-fresh-options:       blank hash tables (no specs, no runtime values).
;;; with-fresh-global-options: copies only *global-options*, preserving *option-registry*.
;;; with-single-option:       a hash table containing exactly one option name/value pair.

(defmacro with-fresh-options (&body body)
  "Run BODY with empty, isolated option hash tables (no registered specs)."
  `(let ((cl-tmux/options:*global-options*   (make-hash-table :test #'equal))
         (cl-tmux/options:*option-registry*  (make-hash-table :test #'equal)))
     ,@body))

(defmacro with-fresh-global-options (&body body)
  "Run BODY with a copy of *global-options* so mutations do not leak.
   *option-registry* is shared so type coercion continues to work."
  `(let ((cl-tmux/options:*global-options*
          (let ((ht (make-hash-table :test #'equal)))
            (maphash (lambda (k v) (setf (gethash k ht) v))
                     cl-tmux/options:*global-options*)
            ht)))
     ,@body))

(defmacro with-single-option ((name value) &body body)
  "Run BODY with *global-options* bound to a hash-table containing only NAME → VALUE."
  `(let ((cl-tmux/options:*global-options*
          (let ((ht (make-hash-table :test #'equal)))
            (setf (gethash ,name ht) ,value)
            ht)))
     ,@body))

(defmacro with-single-server-option ((name value) &body body)
  "Run BODY with *server-options* bound to a hash-table containing only NAME → VALUE."
  `(let ((cl-tmux/options:*server-options*
          (let ((ht (make-hash-table :test #'equal)))
            (setf (gethash ,name ht) ,value)
            ht)))
     ,@body))

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
    ("set-titles-string"             "#W")
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
