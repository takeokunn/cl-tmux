(in-package #:cl-tmux/test)

;;;; Tests for cl-tmux/options: option registry and get/set/coercion.
;;;;
;;;; Isolation helpers (with-fresh-options, with-fresh-global-options,
;;;; with-single-option, with-single-server-option) are defined in
;;;; tests/helpers-options.lisp so that config-directives-tests can reuse them.

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

(test get-option-unset-reverts-to-registry-default
  "After a registered option is removed (set -u), get-option with NO caller
   default reverts to the registry spec default, mirroring tmux
   options_remove_or_default.  An explicit caller default still wins."
  ;; with-fresh-global-options copies *global-options* but SHARES *option-registry*,
  ;; so default-terminal's spec ("screen") is still registered.
  (with-fresh-global-options
    (remhash "default-terminal" cl-tmux/options:*global-options*)
    (is (string= "screen" (cl-tmux/options:get-option "default-terminal"))
        "unset registered option must read as its registry default \"screen\"")
    ;; A registered option with no caller default also falls back even if never set.
    (remhash "history-limit" cl-tmux/options:*global-options*)
    (is (= 2000 (cl-tmux/options:get-option "history-limit"))
        "unset integer option must read as its registry default 2000")
    ;; An explicitly supplied default (even NIL) is still honored over the registry.
    (is (null (cl-tmux/options:get-option "default-terminal" nil))
        "an explicit NIL caller default must override the registry fallback")
    (is (= 7 (cl-tmux/options:get-option "default-terminal" 7))
        "an explicit caller default must override the registry fallback")
    ;; An UNregistered key still returns the caller default / NIL (no spec to fall back to).
    (is (null (cl-tmux/options:get-option "totally-unknown-opt"))
        "an unregistered absent key has no spec, so it reads as NIL")))

(test get-server-option-unset-reverts-to-registry-default
  "get-server-option falls back to the server registry spec default when a
   registered server option is absent and no caller default is supplied."
  (with-fresh-server-options
    ;; *server-options* is empty here; *server-option-registry* is shared, so
    ;; default-terminal's server spec ("screen") and escape-time (10) apply.
    (is (string= "screen" (cl-tmux/options:get-server-option "default-terminal"))
        "absent registered server option must read as its registry default")
    (is (= 10 (cl-tmux/options:get-server-option "escape-time"))
        "absent registered integer server option must read as its registry default")
    ;; Explicit caller default (incl. NIL) still wins; unregistered key reads as default.
    (is (null (cl-tmux/options:get-server-option "default-terminal" nil))
        "an explicit NIL caller default must override the server registry fallback")
    (is (null (cl-tmux/options:get-server-option "nonexistent-server-opt"))
        "an unregistered absent server key reads as NIL")))

;;; set-option / get-option round-trip

(test set-and-get-option-string
  "set-option stores a string value retrievable by get-option."
  (with-fresh-options
    (cl-tmux/options:set-option "status-left" "my-session")
    (is (string= "my-session" (cl-tmux/options:get-option "status-left")))))

;;; option-defined-p

(test option-defined-p-table
  "option-defined-p returns T for registered options, NIL for unknowns."
  (dolist (row '(("status"         t   "known option → T")
                 ("no-such-option" nil "unknown option → NIL")))
    (destructuring-bind (name expected desc) row
      (is (if expected
              (cl-tmux/options:option-defined-p name)
              (null (cl-tmux/options:option-defined-p name)))
          "~A" desc))))

;;; Type coercion

(test boolean-coercion-table
  "set-option coerces :boolean option strings: on/true/1 → T, off/0/false → NIL.
   Each row: (str expected description)."
  (dolist (row '(("on"    t   "on → T")
                 ("true"  t   "true → T")
                 ("1"     t   "1 → T")
                 ("off"   nil "off → NIL")
                 ("0"     nil "0 → NIL")
                 ("false" nil "false → NIL")))
    (destructuring-bind (str expected desc) row
      (with-fresh-global-options
        (if expected
            (is-true  (cl-tmux/options:set-option "mouse" str) desc)
            (is-false (cl-tmux/options:set-option "mouse" str) desc))))))


(test status-numeric-value-survives-coercion
  "The `status` option is a CHOICE/number (off|on|2..5), NOT a boolean: a line
   count is stored UNCHANGED so the renderer's status-line-count sees it.  The old
   :boolean type coerced \"2\" to NIL, hiding a bar whose rows the layout still
   reserved via *status-height* — the multi-line-status bug."
  (with-fresh-global-options
    (is (string= "2" (cl-tmux/options:set-option "status" "2"))
        "set-option returns the raw \"2\" (no boolean coercion to NIL)")
    (is (string= "2" (cl-tmux/options:get-option "status"))
        "get-option returns \"2\" so status-line-count reserves 2 rows")
    (is (string= "off" (cl-tmux/options:set-option "status" "off"))
        "off is stored verbatim (status-line-count maps it to 0)")
    (is (string= "on" (cl-tmux/options:set-option "status" "on"))
        "on is stored verbatim (status-line-count maps it to 1)")))

(test integer-coercion-table
  "Setting a :integer option with a numeric string coerces to the integer value."
  (dolist (row '(("5000" 5000 "5000 → integer 5000")
                 ("500"   500 "500 → integer 500")))
    (destructuring-bind (str-val expected desc) row
      (with-fresh-global-options
        (is (= expected (cl-tmux/options:set-option "history-limit" str-val))
            "~A" desc)))))

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
  "*server-options* contains the default escape-time = 10."
  (is (= 10 (cl-tmux/options:get-server-option "escape-time"))
      "default escape-time must be 10"))

(test server-options-exit-empty-default
  "*server-options* contains exit-empty = T by default."
  (is (cl-tmux/options:get-server-option "exit-empty")
      "default exit-empty must be T"))

(test set-server-option-stores-value
  "set-server-option stores a value in *server-options*."
  (with-fresh-server-options
    (cl-tmux/options:set-server-option "escape-time" "100")
    (is (= 100 (cl-tmux/options:get-server-option "escape-time"))
        "escape-time must be 100 after set-server-option")))

(test set-server-option-boolean-coercion
  "set-server-option coerces boolean values."
  (with-fresh-server-options
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
    ("visual-bell"                   "off")
    ("visual-activity"               "off")
    ("visual-silence"                "off")
    ("monitor-activity"              nil)
    ("monitor-silence"               0)
    ("monitor-bell"                  t)
    ("activity-action"               "other")
    ("silence-action"                "other")
    ("message-line"                  0)
    ("assume-paste-time"             1)
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

;;; Tests for options-scope.lisp exports

(test option-scope-from-name-window-options
  "option-scope-from-name returns :window for window-scoped option names."
  (dolist (name '("automatic-rename" "mode-keys" "mode-style"
                  "monitor-activity" "synchronize-panes"
                  "window-active-style" "wrap-search"
                  "pane-border-status" "remain-on-exit"))
    (is (eq :window (cl-tmux/options:option-scope-from-name name))
        "~A should be :window scope" name)))

(test option-scope-from-name-session-options
  "option-scope-from-name returns :session for non-window option names."
  (dolist (name '("status" "status-interval" "history-limit"
                  "escape-time" "default-terminal" "prefix"
                  "display-time" "buffer-limit"))
    (is (eq :session (cl-tmux/options:option-scope-from-name name))
        "~A should be :session scope" name)))

;;; ── option-present-for-scope-p (options-scope.lisp) ───────────────────────
