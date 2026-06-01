(in-package #:cl-tmux/test)

;;;; Tests for cl-tmux/options: option registry and get/set/coercion.

(def-suite options-suite :description "Global option registry")
(in-suite options-suite)

;;; ── Helper ───────────────────────────────────────────────────────────────────

(defmacro with-fresh-options (&body body)
  "Run BODY with isolated option tables."
  `(let ((cl-tmux/options:*global-options*   (make-hash-table :test #'equal))
         (cl-tmux/options:*option-registry*  (make-hash-table :test #'equal)))
     ,@body))

;;; ── get-option ───────────────────────────────────────────────────────────────

(test get-option-returns-nil-when-absent
  "get-option returns NIL for a key not in the table."
  (with-fresh-options
    (is (null (cl-tmux/options:get-option "nonexistent")))))

(test get-option-returns-default-when-absent
  "get-option returns the supplied default when key is absent."
  (with-fresh-options
    (is (= 42 (cl-tmux/options:get-option "missing" 42)))))

;;; ── set-option / get-option round-trip ──────────────────────────────────────

(test set-and-get-option-string
  "set-option stores a string value retrievable by get-option."
  (with-fresh-options
    (cl-tmux/options:set-option "status-left" "my-session")
    (is (string= "my-session" (cl-tmux/options:get-option "status-left")))))

;;; ── option-defined-p ─────────────────────────────────────────────────────────

(test option-defined-p-registered-option
  "option-defined-p returns T for a registered option."
  (is (cl-tmux/options:option-defined-p "status")
      "\"status\" must be a registered option"))

(test option-defined-p-unregistered-returns-nil
  "option-defined-p returns NIL for an unknown option name."
  (is (null (cl-tmux/options:option-defined-p "no-such-option"))))

;;; ── Type coercion ────────────────────────────────────────────────────────────

(test boolean-coercion-on-string
  "Setting a :boolean option with \"on\"/\"true\"/\"1\" coerces to T."
  (is (eq t (cl-tmux/options:set-option "status" "on")))
  (is (eq t (cl-tmux/options:set-option "status" "true")))
  (is (eq t (cl-tmux/options:set-option "status" "1"))))

(test boolean-coercion-false-strings
  "Setting a :boolean option with any other string coerces to NIL."
  (is (null (cl-tmux/options:set-option "status" "off")))
  (is (null (cl-tmux/options:set-option "status" "0")))
  (is (null (cl-tmux/options:set-option "status" "false"))))

(test integer-coercion
  "Setting a :integer option with a numeric string coerces correctly."
  (is (= 5000 (cl-tmux/options:set-option "history-limit" "5000"))))

(test integer-coercion-500
  "Setting history-limit with \"500\" coerces to the integer 500."
  (is (= 500 (cl-tmux/options:set-option "history-limit" "500"))))

(test string-coercion-from-non-string
  "Setting a :string option with a non-string value coerces via format ~A."
  (is (string= "42" (cl-tmux/options:set-option "status-left" 42))))

;;; ── all-options ──────────────────────────────────────────────────────────────

(test all-options-returns-alist
  "all-options returns an alist of (name . value) pairs."
  (let ((opts (cl-tmux/options:all-options)))
    (is (listp opts))
    (is (every #'consp opts)
        "each entry must be a cons pair")))

(test define-tmux-options-macro-is-defined
  "define-tmux-options is a registered macro."
  (is (macro-function 'cl-tmux/options:define-tmux-options)))
