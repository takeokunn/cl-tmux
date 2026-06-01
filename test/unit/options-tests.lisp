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
;;;
;;; These tests call set-option which modifies the global *global-options*.
;;; They are isolated with with-fresh-global-options to avoid leaking state.

(defmacro with-fresh-global-options (&body body)
  "Run BODY with a copy of *global-options* so mutations don't leak."
  `(let ((cl-tmux/options:*global-options*
          (let ((ht (make-hash-table :test #'equal)))
            (maphash (lambda (k v) (setf (gethash k ht) v))
                     cl-tmux/options:*global-options*)
            ht)))
     ,@body))

(test boolean-coercion-on-string
  "Setting a :boolean option with \"on\"/\"true\"/\"1\" coerces to T."
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
  "Setting history-limit with \"500\" coerces to the integer 500."
  (with-fresh-global-options
    (is (= 500 (cl-tmux/options:set-option "history-limit" "500")))))

(test string-coercion-from-non-string
  "Setting a :string option with a non-string value coerces via format ~A."
  (with-fresh-global-options
    (is (string= "42" (cl-tmux/options:set-option "status-left" 42)))))

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

;;; ── Server options ───────────────────────────────────────────────────────────

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

;;; ── show-options ─────────────────────────────────────────────────────────────

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
          "show-options must include \"status\" option (got ~S)" out)
      (is (search "history-limit" out)
          "show-options must include \"history-limit\" option (got ~S)" out))))

(test show-option-single-option
  "show-option returns the value of a single named option."
  (let ((cl-tmux/options:*global-options*
         (let ((ht (make-hash-table :test #'equal)))
           (setf (gethash "status-interval" ht) 30)
           ht)))
    (let ((out (cl-tmux/options:show-option "status-interval")))
      (is (search "status-interval" out)
          "show-option output must include the option name (got ~S)" out)
      (is (search "30" out)
          "show-option output must include the value 30 (got ~S)" out))))

(test show-option-missing-option
  "show-option for an absent option indicates it is not set."
  (let ((cl-tmux/options:*global-options* (make-hash-table :test #'equal)))
    (let ((out (cl-tmux/options:show-option "no-such-option")))
      (is (search "no-such-option" out)
          "show-option output must include the option name (got ~S)" out))))

(test show-options-server-scope
  "show-options with :server scope returns server options."
  (let ((cl-tmux/options:*server-options*
         (let ((ht (make-hash-table :test #'equal)))
           (setf (gethash "escape-time" ht) 500)
           ht)))
    (let ((out (cl-tmux/options:show-options :server)))
      (is (search "escape-time" out)
          "show-options :server must include escape-time (got ~S)" out))))

;;; ── New options: status-style, status-justify, window-status-current-style ──

(test status-style-option-registered
  "\"status-style\" is a registered option."
  (is (cl-tmux/options:option-defined-p "status-style")
      "\"status-style\" must be a registered option"))

(test status-justify-option-registered
  "\"status-justify\" is a registered option."
  (is (cl-tmux/options:option-defined-p "status-justify")
      "\"status-justify\" must be a registered option"))

(test window-status-current-style-option-registered
  "\"window-status-current-style\" is a registered option."
  (is (cl-tmux/options:option-defined-p "window-status-current-style")
      "\"window-status-current-style\" must be a registered option"))
