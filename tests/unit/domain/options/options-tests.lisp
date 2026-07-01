(in-package #:cl-tmux/test)

;;;; Tests for cl-tmux/options: option registry and get/set/coercion.
;;;;
;;;; Isolation helpers (with-fresh-options, with-fresh-global-options,
;;;; with-single-option, with-single-server-option) are defined in
;;;; tests/helpers-b.lisp so that config-directives-tests can reuse them.

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
    ("visual-bell"                   nil)
    ("visual-activity"               nil)
    ("visual-silence"                nil)
    ("monitor-activity"              nil)
    ("monitor-silence"               0)
    ("monitor-bell"                  t)
    ("activity-action"               "other")
    ("silence-action"                "other")
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

(test option-present-for-scope-p-table
  "option-present-for-scope-p returns T for @-user options, registered specs,
   present runtime keys, and array entries whose base is registered; NIL for
   an unregistered/absent plain name."
  (with-fresh-global-options
    (dolist (row `(("@my-user-opt"    nil t   "unset @ user option is always present")
                   ("status"          nil t   "registered spec name is present")
                   ("status-format[3]" nil t  "array entry of a registered base is present")
                   ("totally-unknown" nil nil "unregistered unset plain name is absent")))
      (destructuring-bind (name scope expected desc) row
        (is (eq expected (and (cl-tmux/options:option-present-for-scope-p name scope) t))
            "~A" desc)))
    (setf (gethash "runtime-only-opt" cl-tmux/options:*global-options*) "x")
    (is-true (cl-tmux/options:option-present-for-scope-p "runtime-only-opt")
             "a key present in the runtime table (even if unregistered) counts as present")))

(test option-present-for-scope-p-server-scope
  "option-present-for-scope-p consults the server registry/table when SCOPE
   is :server."
  (with-fresh-server-options
    (is-true (cl-tmux/options:option-present-for-scope-p "escape-time" :server)
             "escape-time is a registered server option")
    (is-false (cl-tmux/options:option-present-for-scope-p "no-such-server-opt" :server)
               "an unregistered, unset server option is absent")))

;;; ── option-present-for-display-p (options-scope.lisp) ─────────────────────

(test option-present-for-display-p-user-option-requires-presence
  "option-present-for-display-p requires an @-user option to actually be SET
   in the runtime table (unlike option-present-for-scope-p, which always
   treats @-names as present)."
  (with-fresh-global-options
    (is-false (cl-tmux/options:option-present-for-display-p "@unset-user-opt")
               "an unset @ user option must NOT be displayable")
    (setf (gethash "@set-user-opt" cl-tmux/options:*global-options*) "v")
    (is-true (cl-tmux/options:option-present-for-display-p "@set-user-opt")
             "a set @ user option must be displayable")))

(test option-present-for-display-p-delegates-for-plain-names
  "option-present-for-display-p delegates to option-present-for-scope-p for
   non-@ names."
  (with-fresh-global-options
    (is-true (cl-tmux/options:option-present-for-display-p "status")
             "a registered plain option must be displayable")
    (is-false (cl-tmux/options:option-present-for-display-p "totally-unknown")
               "an unregistered unset plain option must not be displayable")))

;;; ── window-option-present-for-display-p (options-display.lisp) ────────────

(test window-option-present-for-display-p-registered-spec
  "A registered window-scoped option is always displayable, even with no
   local override and GLOBAL-P/INHERITED-P both NIL."
  (let ((win (cl-tmux/model:make-window :id 1 :name "w")))
    (is-true (cl-tmux/options:window-option-present-for-display-p
              "synchronize-panes" win)
             "a registered option name is displayable regardless of local state")))

(test window-option-present-for-display-p-local-override
  "An unregistered @ option becomes displayable once it has a window-local
   override, without GLOBAL-P or INHERITED-P."
  (let ((win (cl-tmux/model:make-window :id 1 :name "w")))
    (is-false (cl-tmux/options:window-option-present-for-display-p "@foo" win)
               "an unregistered, unset user option is not displayable")
    (cl-tmux/options:set-option-for-window "@foo" "bar" win)
    (is-true (cl-tmux/options:window-option-present-for-display-p "@foo" win)
             "a window-local override makes the user option displayable")))

(test window-option-present-for-display-p-global-flag
  "With GLOBAL-P T, an unregistered @ option is displayable only when it
   exists in *global-options*, regardless of any window-local state."
  (let ((win (cl-tmux/model:make-window :id 1 :name "w"))
        (cl-tmux/options:*global-options*
         (let ((ht (make-hash-table :test #'equal)))
           (setf (gethash "@global-only" ht) "v")
           ht)))
    (is-true (cl-tmux/options:window-option-present-for-display-p
              "@global-only" win :global-p t)
             "GLOBAL-P T must see the global-only user option")
    (is-false (cl-tmux/options:window-option-present-for-display-p
               "@not-global" win :global-p t)
               "GLOBAL-P T must not see a name absent from *global-options*")))

(test window-option-present-for-display-p-inherited-flag
  "With INHERITED-P T, an unregistered @ option is displayable when it exists
   globally, even without any window-local override."
  (let ((win (cl-tmux/model:make-window :id 1 :name "w"))
        (cl-tmux/options:*global-options*
         (let ((ht (make-hash-table :test #'equal)))
           (setf (gethash "@inherited-opt" ht) "v")
           ht)))
    (is-true (cl-tmux/options:window-option-present-for-display-p
              "@inherited-opt" win :inherited-p t)
             "INHERITED-P T must see the globally-set user option")))

;;; ── Array-option name parsing (options-scope.lisp) ────────────────────────
;;;
;;; tmux array options use the BASE[N] naming convention (e.g.
;;; "command-alias[0]", "status-format[0]").  These are the single
;;; authoritative parsing/classification helpers for that syntax.

(test array-entry-index-for-base-table
  "%array-entry-index-for-base returns the numeric index for a well-formed
   BASE[N] name, and NIL for anything that does not match BASE exactly
   followed by [N]."
  (dolist (row '(("status-format" "status-format[0]" 0   "index 0")
                 ("status-format" "status-format[12]" 12 "multi-digit index")
                 ("status-format" "status-format"     nil "no brackets at all")
                 ("status-format" "status-format[]"   nil "empty brackets")
                 ("status-format" "status-format[x]"  nil "non-digit index")
                 ("status-format" "other-name[0]"     nil "different base name")
                 ("status-format" "status-format[0"   nil "missing closing bracket")))
    (destructuring-bind (base name expected desc) row
      (is (equal expected (cl-tmux/options::%array-entry-index-for-base base name))
          "~A" desc))))

(test array-entry-base-name-table
  "%array-entry-base-name extracts BASE from a BASE[N] name, or returns NIL
   for a name that is not an array entry."
  (dolist (row '(("status-format[0]"  "status-format" "simple base + index")
                 ("command-alias[12]" "command-alias" "multi-digit index")
                 ("status-format"     nil             "no brackets")
                 ("status-format[]"   nil             "empty brackets are not a valid index")
                 ("status-format[x]"  nil             "non-digit index")))
    (destructuring-bind (name expected desc) row
      (is (equal expected (cl-tmux/options::%array-entry-base-name name)) "~A" desc))))

(test array-option-p-recognises-array-base-once-an-entry-exists
  "%array-option-p is true for a BASE name once at least one BASE[N] entry
   exists in the runtime options table (e.g. after `set status-format[0] ...`);
   false for an indexed entry itself and for a plain unrelated name."
  (with-fresh-global-options
    (is-false (cl-tmux/options::%array-option-p "status-format" nil)
               "status-format has no [N] entries yet -> not recognised as an array base")
    (setf (gethash "status-format[0]" cl-tmux/options:*global-options*) "x")
    (is-true (cl-tmux/options::%array-option-p "status-format" nil)
             "status-format is an array-option base once status-format[0] exists")
    (is-false (cl-tmux/options::%array-option-p "status-format[0]" nil)
               "an indexed entry itself is not the array-option base")
    (is-false (cl-tmux/options::%array-option-p "totally-unknown-base" nil)
               "an unrelated plain name is not an array-option base")))

(test array-option-pairs-collects-runtime-and-registry-entries
  "%array-option-pairs returns sorted (name . value) pairs for BASE[N] entries,
   with a runtime value overriding the registered default at the same index."
  (with-fresh-global-options
    (setf (gethash "status-format[0]" cl-tmux/options:*global-options*) "RUNTIME-0")
    (let ((pairs (cl-tmux/options::%array-option-pairs "status-format" nil)))
      (is (> (length pairs) 0) "at least one status-format[N] entry must be found")
      (let ((entry-0 (assoc "status-format[0]" pairs :test #'string=)))
        (is (not (null entry-0)) "status-format[0] must appear in the pairs")
        (is (string= "RUNTIME-0" (cdr entry-0))
            "the runtime value must override the registered default")))))

(test decimal-digits-p-table
  "%decimal-digits-p is true only for a non-empty run of decimal digit
   characters within the given bounds."
  (dolist (row (list (list "123" 0 3 t   "all-digit substring")
                     (list "12a" 0 3 nil "a non-digit character disqualifies")
                     (list ""    0 0 nil "an empty span is never digits")
                     (list "abc" 0 0 nil "a zero-length span at any position is empty")))
    (destructuring-bind (string start end expected desc) row
      (is (eq expected (and (cl-tmux/options::%decimal-digits-p string start end) t))
          "~A" desc))))

;;; ── show-option/show-options value quoting (options-display.lisp) ─────────
;;;
;;; %quote-option-string / %option-value-string implement tmux's show-options
;;; display quoting: an empty string renders as ''; a value containing a
;;; space/tab/quote/backslash is wrapped in double quotes with \\ and \" escaped;
;;; anything else is printed bare.  Booleans render as on/off.

(test quote-option-string-table
  "%quote-option-string reproduces tmux's show-options quoting rules: empty ->
   ''; a value with a space/tab/quote/backslash is wrapped in double quotes
   with embedded \" and \\ escaped; anything else is printed bare."
  (dolist (row (list (list ""      "''"            "empty string -> ''")
                     (list "plain" "plain"         "no special chars -> bare")
                     (list "a b"   "\"a b\""       "an embedded space is quoted")
                     (list "a\"b"  "\"a\\\"b\""    "an embedded quote is escaped")
                     (list "a\\b"  "\"a\\\\b\"")))
    (destructuring-bind (input expected &optional desc) row
      (is (string= expected (cl-tmux/options::%quote-option-string input))
          "~A" (or desc (format nil "%quote-option-string ~S" input))))))

(test option-value-string-table
  "%option-value-string formats T as \"on\", NIL as \"off\", strings as-is,
   and any other value via princ-to-string."
  (dolist (row (list (list t     "on"  "T -> on")
                     (list nil   "off" "NIL -> off")
                     (list "hi"  "hi"  "string passes through unchanged")
                     (list 42    "42"  "integer -> decimal via princ")))
    (destructuring-bind (input expected desc) row
      (is (string= expected (cl-tmux/options::%option-value-string input)) "~A" desc))))

(test show-options-quotes-values-with-spaces
  "show-options end-to-end: a stored string value containing a space is quoted
   in the rendered output."
  (with-single-option ("status-left" "a b")
    (let ((out (cl-tmux/options:show-options)))
      (is (search "status-left \"a b\"" out)
          "show-options must quote a value containing a space (got ~S)" out))))

(test show-options-empty-string-value-renders-as-quote-pair
  "show-options renders an empty string option value as '' (tmux convention)."
  (with-single-option ("status-left" "")
    (let ((out (cl-tmux/options:show-options)))
      (is (search "status-left ''" out)
          "show-options must render an empty value as '' (got ~S)" out))))

