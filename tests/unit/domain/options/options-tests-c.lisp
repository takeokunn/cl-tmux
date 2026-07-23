(in-package #:cl-tmux/test)

;;;; Options tests — part C: type-coercion dispatch, option-table macro,
;;;; spec accessors, server options, show-option sorting, status defaults.

(describe "options-suite"

  ;;; ── define-type-coercions ecase dispatch ─────────────────────────────────

  ;; %coerce-value :boolean coerces strings, integers, and non-string values correctly.
  (it "type-coercions-boolean-table"
    (dolist (c '(("on"    t   "on → T")
                 ("true"  t   "true → T")
                 ("1"     t   "1 → T")
                 ("off"   nil "off → NIL")
                 ("false" nil "false → NIL")
                 ("0"     nil "0 → NIL")
                 (42      t   "42 → T")
                 (nil     nil "nil → NIL")))
      (destructuring-bind (input expected desc) c
        (declare (ignore desc))
        (expect (eq expected (cl-tmux/options::%coerce-value :boolean input))))))

  ;; %coerce-value :integer parses strings and truncates floats; nil/garbage → 0.
  (it "type-coercions-integer-table"
    (dolist (c '(("42"           42 "numeric string → 42")
                 ("not-a-number" 0  "non-numeric string → 0")
                 (3.7            3  "float → truncated to 3")
                 (nil            0  "nil → 0")))
      (destructuring-bind (input expected desc) c
        (declare (ignore desc))
        (expect (= expected (cl-tmux/options::%coerce-value :integer input))))))

  ;; %coerce-value :string formats any value as a string.
  (it "type-coercions-string"
    (dolist (c '((42      "42"    "integer -> decimal string")
                 (t       "T"     "T -> \"T\"")
                 ("hello" "hello" "string passes through unchanged")))
      (destructuring-bind (input expected desc) c
        (declare (ignore desc))
        (expect (string= expected (cl-tmux/options::%coerce-value :string input))))))

  ;;; ── set-option unregistered-option passthrough path ──────────────────────

  ;; set-option stores unregistered values without coercion for any value type.
  (it "set-option-unregistered-stores-as-is-table"
    (dolist (c '(("custom-unknown-option" "raw-value" "string stored as-is")
                 ("custom-int-option"     99          "integer stored as-is")))
      (destructuring-bind (name value desc) c
        (declare (ignore desc))
        (with-fresh-options
          (cl-tmux/options:set-option name value)
          (expect (equal value (cl-tmux/options:get-option name)))))))

  ;;; ── all-options count matches registration ────────────────────────────────

  ;; all-options returns an entry for every option in *option-registry*.
  (it "all-options-count-matches-registry"
    (let* ((registry-count (hash-table-count cl-tmux/options:*option-registry*))
           (all            (cl-tmux/options:all-options)))
      (expect (= registry-count (length all)))))

  ;;; ── define-option-table macro ─────────────────────────────────────────────

  ;; define-option-table is a registered macro.
  (it "define-option-table-macro-is-defined"
    (expect (macro-function 'cl-tmux/options:define-option-table)))

  ;;; ── option-spec accessors ─────────────────────────────────────────────────

  ;; option-spec-name, option-spec-type, option-spec-default return the correct fields.
  (it "option-spec-accessors"
    (let ((spec (gethash "status" cl-tmux/options:*option-registry*)))
      (expect (not (null spec)))
      (expect (string= "status" (cl-tmux/options:option-spec-name spec)))
      (expect (eq :string (cl-tmux/options:option-spec-type spec)))
      (expect (string= "on" (cl-tmux/options:option-spec-default spec)))))

  ;; option-spec-type for an integer option is :integer.
  (it "option-spec-integer-type"
    (let ((spec (gethash "history-limit" cl-tmux/options:*option-registry*)))
      (expect (not (null spec)))
      (expect (eq :integer (cl-tmux/options:option-spec-type spec)))))

  ;; option-spec-type for a string option is :string.
  (it "option-spec-string-type"
    (let ((spec (gethash "default-command" cl-tmux/options:*option-registry*)))
      (expect (not (null spec)))
      (expect (eq :string (cl-tmux/options:option-spec-type spec)))))

  ;;; ── get-server-option with default ───────────────────────────────────────

  ;; get-server-option returns the supplied default when the key is absent.
  (it "get-server-option-returns-default-when-absent"
    (with-fresh-server-options
      (expect (= 99 (cl-tmux/options:get-server-option "nonexistent-server-opt" 99)))))

  ;; get-server-option returns NIL for an absent key when no default is given.
  (it "get-server-option-returns-nil-when-absent-no-default"
    (with-fresh-server-options
      (expect (null (cl-tmux/options:get-server-option "nonexistent-server-opt")))))

  ;;; ── set-server-option for unknown option (passthrough) ───────────────────

  ;; set-server-option for an unregistered option stores the value without coercion.
  (it "set-server-option-unknown-stores-as-is"
    (with-fresh-server-options
      (cl-tmux/options:set-server-option "custom-server-opt" "raw-value")
      (expect (string= "raw-value"
                       (cl-tmux/options:get-server-option "custom-server-opt")))))

  ;;; ── show-option with :server scope ──────────────────────────────────────

  ;; show-option with :server scope returns the value from *server-options*.
  (it "show-option-server-scope"
    (with-single-server-option ("escape-time" 250)
      (let ((out (cl-tmux/options:show-option "escape-time" :server)))
        (expect (search "escape-time" out))
        (expect (search "250" out)))))

  ;;; ── show-options returns sorted output ───────────────────────────────────

  ;; show-options output has options in alphabetical order.
  (it "show-options-is-sorted"
    (let ((cl-tmux/options:*global-options*
           (let ((ht (make-hash-table :test #'equal)))
             (setf (gethash "zebra-option" ht) "z")
             (setf (gethash "alpha-option" ht) "a")
             ht)))
      (let ((out (cl-tmux/options:show-options)))
        (let ((pos-alpha (search "alpha-option" out))
              (pos-zebra (search "zebra-option" out)))
          (expect (and pos-alpha pos-zebra))
          (expect (< pos-alpha pos-zebra))))))

  ;;; ── define-server-options macro ──────────────────────────────────────────

  ;; define-server-options is a registered macro.
  (it "define-server-options-macro-is-defined"
    (expect (macro-function 'cl-tmux/options:define-server-options)))

  ;;; ── option defaults table ────────────────────────────────────────────────

  ;; Key global options return the expected default values from *option-registry*.
  (it "option-defaults-table"
    (dolist (c '(("status-position"   "bottom")
                 ("base-index"        0)
                 ("mouse"             nil)
                 ("synchronize-panes" nil)
                 ("status-interval"   15)
                 ("history-limit"     2000)
                 ("status"            "on")))
      (destructuring-bind (name expected) c
        (expect (equal expected (cl-tmux/options:get-option name))))))

  ;;; ── make-option-spec constructor ─────────────────────────────────────────

  ;; make-option-spec stores type, default, and name correctly for each type.
  (it "make-option-spec-table"
    (dolist (c '((:boolean nil    "my-opt" "boolean with nil default")
                 (:integer 42     "count"  "integer with 42 default")
                 (:string  "hello" "label" "string with hello default")))
      (destructuring-bind (type default name desc) c
        (declare (ignore desc))
        (let ((spec (cl-tmux/options:make-option-spec :name name :type type :default default)))
          (expect (not (null spec)))
          (expect (string= name (cl-tmux/options:option-spec-name spec)))
          (expect (eq type (cl-tmux/options:option-spec-type spec)))
          (expect (equal default (cl-tmux/options:option-spec-default spec)))))))

  ;;; ── show-window-options unit tests ──────────────────────────────────────
  ;;;
  ;;; Direct API tests for cl-tmux/options:show-window-option and
  ;;; show-window-options (render layer below the dispatch handlers).
  ;;; These verify the formatting and scope-fallback logic in isolation,
  ;;; without going through the full command dispatch.

  ;; show-window-option falls back to the global value for a registered window-
  ;; scoped option when the window has no local override.
  (it "show-window-option-returns-global-value-for-registered-option"
    (let ((win (cl-tmux/model:make-window :id 1 :name "test-win"))
          (cl-tmux/options:*global-options*
           (let ((ht (make-hash-table :test #'equal)))
             (setf (gethash "mode-keys" ht) "vi")
             ht)))
      (let ((out (cl-tmux/options:show-window-option "mode-keys" win)))
        (expect (not (null out)))
        (expect (search "mode-keys" out))
        (expect (search "vi" out)))))

  ;; show-window-option returns the window-local value when explicitly set,
  ;; overriding any global value.
  (it "show-window-option-returns-window-local-value"
    (let ((win (cl-tmux/model:make-window :id 1 :name "test-win"))
          (cl-tmux/options:*global-options*
           (let ((ht (make-hash-table :test #'equal)))
             (setf (gethash "mode-keys" ht) "emacs")
             ht)))
      (cl-tmux/options:set-option-for-window "mode-keys" "vi" win)
      (let ((out (cl-tmux/options:show-window-option "mode-keys" win)))
        (expect (not (null out)))
        (expect (search "vi" out)))))

  ;; show-window-option with :inherited-p T marks inherited values with '* ' prefix.
  (it "show-window-option-inherited-marks-with-asterisk"
    (let ((win (cl-tmux/model:make-window :id 1 :name "test-win"))
          (cl-tmux/options:*global-options*
           (let ((ht (make-hash-table :test #'equal)))
             (setf (gethash "mode-keys" ht) "vi")
             ht)))
      ;; No window-local override; global value is inherited.
      (let ((out (cl-tmux/options:show-window-option "mode-keys" win :inherited-p t)))
        (expect (not (null out)))
        (expect (search "* mode-keys" out)))))

  ;; show-window-option with :value-only-p T returns just the value string.
  (it "show-window-option-value-only-returns-bare-value"
    (let ((win (cl-tmux/model:make-window :id 1 :name "test-win"))
          (cl-tmux/options:*global-options*
           (let ((ht (make-hash-table :test #'equal)))
             (setf (gethash "mode-keys" ht) "vi")
             ht)))
      (let ((out (cl-tmux/options:show-window-option "mode-keys" win :value-only-p t)))
        (expect (string= "vi" out)))))

  ;; show-window-option returns NIL for an unset user @-option.
  (it "show-window-option-returns-nil-for-unset-user-option"
    (let ((win (cl-tmux/model:make-window :id 1 :name "test-win"))
          (cl-tmux/options:*global-options* (make-hash-table :test #'equal)))
      (expect (null (cl-tmux/options:show-window-option "@nonexistent" win)))))

  ;; show-window-options without flags lists only the window-local options.
  (it "show-window-options-lists-window-local-options"
    (let ((win (cl-tmux/model:make-window :id 1 :name "test-win"))
          (cl-tmux/options:*global-options* (make-hash-table :test #'equal)))
      (cl-tmux/options:set-option-for-window "mode-keys" "vi" win)
      (cl-tmux/options:set-option-for-window "synchronize-panes" t win)
      (let ((out (cl-tmux/options:show-window-options win)))
        (expect (search "mode-keys" out))
        (expect (search "synchronize-panes" out)))))

  ;; show-window-options with :global-p T lists global window-scoped options.
  (it "show-window-options-global-flag-lists-global-options"
    (let ((win (cl-tmux/model:make-window :id 1 :name "test-win"))
          (cl-tmux/options:*global-options*
           (let ((ht (make-hash-table :test #'equal)))
             (setf (gethash "mode-keys" ht) "emacs")
             ht)))
      (let ((out (cl-tmux/options:show-window-options win :global-p t)))
        (expect (search "mode-keys" out))
        (expect (search "emacs" out))))))
