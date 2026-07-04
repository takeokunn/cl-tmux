(in-package #:cl-tmux/test)

(in-suite options-suite)

;;; option-present-for-scope-p (options-scope.lisp)

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

;;; option-present-for-display-p (options-scope.lisp)

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

;;; window-option-present-for-display-p (options-display.lisp)

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

;;; Array-option name parsing (options-scope.lisp)
;;;
;;; tmux array options use the BASE[N] naming convention (e.g.
;;; "command-alias[0]", "status-format[0]"). These are the single
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

;;; show-option/show-options value quoting (options-display.lisp)
;;;
;;; %quote-option-string / %option-value-string implement tmux's show-options
;;; display quoting: an empty string renders as ''; a value containing a
;;; space/tab/quote/backslash is wrapped in double quotes with \\ and \" escaped;
;;; anything else is printed bare. Booleans render as on/off.

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
