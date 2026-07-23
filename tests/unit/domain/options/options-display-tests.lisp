(in-package #:cl-tmux/test)

(describe "options-suite"

  ;;; option-present-for-scope-p (options-scope.lisp)

  ;; option-present-for-scope-p returns T for @-user options, registered specs,
  ;; present runtime keys, and array entries whose base is registered; NIL for
  ;; an unregistered/absent plain name.
  (it "option-present-for-scope-p-table"
    (with-fresh-global-options
      (dolist (row `(("@my-user-opt"    nil t   "unset @ user option is always present")
                     ("status"          nil t   "registered spec name is present")
                     ("status-format[3]" nil t  "array entry of a registered base is present")
                     ("totally-unknown" nil nil "unregistered unset plain name is absent")))
        (destructuring-bind (name scope expected desc) row
          (declare (ignore desc))
          (expect (eq expected (and (cl-tmux/options:option-present-for-scope-p name scope) t)))))
      (setf (gethash "runtime-only-opt" cl-tmux/options:*global-options*) "x")
      (expect (cl-tmux/options:option-present-for-scope-p "runtime-only-opt") :to-be-truthy)))

  ;; option-present-for-scope-p consults the server registry/table when SCOPE
  ;; is :server.
  (it "option-present-for-scope-p-server-scope"
    (with-fresh-server-options
      (expect (cl-tmux/options:option-present-for-scope-p "escape-time" :server) :to-be-truthy)
      (expect (cl-tmux/options:option-present-for-scope-p "no-such-server-opt" :server) :to-be-falsy)))

  ;;; option-present-for-display-p (options-scope.lisp)

  ;; option-present-for-display-p requires an @-user option to actually be SET
  ;; in the runtime table (unlike option-present-for-scope-p, which always
  ;; treats @-names as present).
  (it "option-present-for-display-p-user-option-requires-presence"
    (with-fresh-global-options
      (expect (cl-tmux/options:option-present-for-display-p "@unset-user-opt") :to-be-falsy)
      (setf (gethash "@set-user-opt" cl-tmux/options:*global-options*) "v")
      (expect (cl-tmux/options:option-present-for-display-p "@set-user-opt") :to-be-truthy)))

  ;; option-present-for-display-p delegates to option-present-for-scope-p for
  ;; non-@ names.
  (it "option-present-for-display-p-delegates-for-plain-names"
    (with-fresh-global-options
      (expect (cl-tmux/options:option-present-for-display-p "status") :to-be-truthy)
      (expect (cl-tmux/options:option-present-for-display-p "totally-unknown") :to-be-falsy)))

  ;;; window-option-present-for-display-p (options-display.lisp)

  ;; A registered window-scoped option is always displayable, even with no
  ;; local override and GLOBAL-P/INHERITED-P both NIL.
  (it "window-option-present-for-display-p-registered-spec"
    (let ((win (cl-tmux/model:make-window :id 1 :name "w")))
      (expect (cl-tmux/options:window-option-present-for-display-p
               "synchronize-panes" win)
              :to-be-truthy)))

  ;; An unregistered @ option becomes displayable once it has a window-local
  ;; override, without GLOBAL-P or INHERITED-P.
  (it "window-option-present-for-display-p-local-override"
    (let ((win (cl-tmux/model:make-window :id 1 :name "w")))
      (expect (cl-tmux/options:window-option-present-for-display-p "@foo" win) :to-be-falsy)
      (cl-tmux/options:set-option-for-window "@foo" "bar" win)
      (expect (cl-tmux/options:window-option-present-for-display-p "@foo" win) :to-be-truthy)))

  ;; With GLOBAL-P T, an unregistered @ option is displayable only when it
  ;; exists in *global-options*, regardless of any window-local state.
  (it "window-option-present-for-display-p-global-flag"
    (let ((win (cl-tmux/model:make-window :id 1 :name "w"))
          (cl-tmux/options:*global-options*
           (let ((ht (make-hash-table :test #'equal)))
             (setf (gethash "@global-only" ht) "v")
             ht)))
      (expect (cl-tmux/options:window-option-present-for-display-p
               "@global-only" win :global-p t)
              :to-be-truthy)
      (expect (cl-tmux/options:window-option-present-for-display-p
               "@not-global" win :global-p t)
              :to-be-falsy)))

  ;; With INHERITED-P T, an unregistered @ option is displayable when it exists
  ;; globally, even without any window-local override.
  (it "window-option-present-for-display-p-inherited-flag"
    (let ((win (cl-tmux/model:make-window :id 1 :name "w"))
          (cl-tmux/options:*global-options*
           (let ((ht (make-hash-table :test #'equal)))
             (setf (gethash "@inherited-opt" ht) "v")
             ht)))
      (expect (cl-tmux/options:window-option-present-for-display-p
               "@inherited-opt" win :inherited-p t)
              :to-be-truthy)))

  ;;; Array-option name parsing (options-scope.lisp)
  ;;;
  ;;; tmux array options use the BASE[N] naming convention (e.g.
  ;;; "command-alias[0]", "status-format[0]"). These are the single
  ;;; authoritative parsing/classification helpers for that syntax.

  ;; %array-entry-index-for-base returns the numeric index for a well-formed
  ;; BASE[N] name, and NIL for anything that does not match BASE exactly
  ;; followed by [N].
  (it "array-entry-index-for-base-table"
    (dolist (row '(("status-format" "status-format[0]" 0   "index 0")
                   ("status-format" "status-format[12]" 12 "multi-digit index")
                   ("status-format" "status-format"     nil "no brackets at all")
                   ("status-format" "status-format[]"   nil "empty brackets")
                   ("status-format" "status-format[x]"  nil "non-digit index")
                   ("status-format" "other-name[0]"     nil "different base name")
                   ("status-format" "status-format[0"   nil "missing closing bracket")))
      (destructuring-bind (base name expected desc) row
        (declare (ignore desc))
        (expect (equal expected (cl-tmux/options::%array-entry-index-for-base base name))))))

  ;; %array-entry-base-name extracts BASE from a BASE[N] name, or returns NIL
  ;; for a name that is not an array entry.
  (it "array-entry-base-name-table"
    (dolist (row '(("status-format[0]"  "status-format" "simple base + index")
                   ("command-alias[12]" "command-alias" "multi-digit index")
                   ("status-format"     nil             "no brackets")
                   ("status-format[]"   nil             "empty brackets are not a valid index")
                   ("status-format[x]"  nil             "non-digit index")))
      (destructuring-bind (name expected desc) row
        (declare (ignore desc))
        (expect (equal expected (cl-tmux/options::%array-entry-base-name name))))))

  ;; %find-spec-by-array-prefix locates a spec via a sibling BASE[N] entry when
  ;; the base name has no direct registry entry — the fallback tier of
  ;; %array-template-spec-for-name that never fires for today's one array
  ;; option (status-format, registered under its bare base key in
  ;; options-registry-data.lisp), but is real, reachable logic for any future
  ;; array option registered only via a BASE[N] entry.
  (it "find-spec-by-array-prefix-locates-sibling-array-entry"
    (let ((table (make-hash-table :test #'equal)))
      (setf (gethash "my-array[0]" table) :spec-for-my-array)
      (expect (eq :spec-for-my-array
                 (cl-tmux/options::%find-spec-by-array-prefix "my-array" table)))))

  ;; %find-spec-by-array-prefix returns NIL when no sibling BASE[N] entry exists.
  (it "find-spec-by-array-prefix-returns-nil-with-no-sibling-entry"
    (let ((table (make-hash-table :test #'equal)))
      (setf (gethash "other-name[0]" table) :unrelated-spec)
      (expect (null (cl-tmux/options::%find-spec-by-array-prefix "my-array" table)))))

  ;; %array-option-p is true for a BASE name once at least one BASE[N] entry
  ;; exists in the runtime options table (e.g. after `set status-format[0] ...`);
  ;; false for an indexed entry itself and for a plain unrelated name.
  (it "array-option-p-recognises-array-base-once-an-entry-exists"
    (with-fresh-global-options
      (expect (cl-tmux/options::%array-option-p "status-format" nil) :to-be-falsy)
      (setf (gethash "status-format[0]" cl-tmux/options:*global-options*) "x")
      (expect (cl-tmux/options::%array-option-p "status-format" nil) :to-be-truthy)
      (expect (cl-tmux/options::%array-option-p "status-format[0]" nil) :to-be-falsy)
      (expect (cl-tmux/options::%array-option-p "totally-unknown-base" nil) :to-be-falsy)))

  ;; %array-option-pairs returns sorted (name . value) pairs for BASE[N] entries,
  ;; with a runtime value overriding the registered default at the same index.
  (it "array-option-pairs-collects-runtime-and-registry-entries"
    (with-fresh-global-options
      (setf (gethash "status-format[0]" cl-tmux/options:*global-options*) "RUNTIME-0")
      (let ((pairs (cl-tmux/options::%array-option-pairs "status-format" nil)))
        (expect (> (length pairs) 0))
        (let ((entry-0 (assoc "status-format[0]" pairs :test #'string=)))
          (expect (not (null entry-0)))
          (expect (string= "RUNTIME-0" (cdr entry-0)))))))

  ;; %decimal-digits-p is true only for a non-empty run of decimal digit
  ;; characters within the given bounds.
  (it "decimal-digits-p-table"
    (dolist (row (list (list "123" 0 3 t   "all-digit substring")
                       (list "12a" 0 3 nil "a non-digit character disqualifies")
                       (list ""    0 0 nil "an empty span is never digits")
                       (list "abc" 0 0 nil "a zero-length span at any position is empty")))
      (destructuring-bind (string start end expected desc) row
        (declare (ignore desc))
        (expect (eq expected (and (cl-tmux/options::%decimal-digits-p string start end) t))))))

  ;;; show-option/show-options value quoting (options-display.lisp)
  ;;;
  ;;; %quote-option-string / %option-value-string implement tmux's show-options
  ;;; display quoting: an empty string renders as ''; a value containing a
  ;;; space/tab/quote/backslash is wrapped in double quotes with \\ and \" escaped;
  ;;; anything else is printed bare. Booleans render as on/off.

  ;; %quote-option-string reproduces tmux's show-options quoting rules: empty ->
  ;; ''; a value with a space/tab/quote/backslash is wrapped in double quotes
  ;; with embedded \" and \\ escaped; anything else is printed bare.
  (it "quote-option-string-table"
    (dolist (row (list (list ""      "''"            "empty string -> ''")
                       (list "plain" "plain"         "no special chars -> bare")
                       (list "a b"   "\"a b\""       "an embedded space is quoted")
                       (list "a\"b"  "\"a\\\"b\""    "an embedded quote is escaped")
                       (list "a\\b"  "\"a\\\\b\"")))
      (destructuring-bind (input expected &optional desc) row
        (declare (ignore desc))
        (expect (string= expected (cl-tmux/options::%quote-option-string input))))))

  ;; %option-value-string formats T as "on", NIL as "off", strings as-is,
  ;; and any other value via princ-to-string.
  (it "option-value-string-table"
    (dolist (row (list (list t     "on"  "T -> on")
                       (list nil   "off" "NIL -> off")
                       (list "hi"  "hi"  "string passes through unchanged")
                       (list 42    "42"  "integer -> decimal via princ")))
      (destructuring-bind (input expected desc) row
        (declare (ignore desc))
        (expect (string= expected (cl-tmux/options::%option-value-string input))))))

  ;; show-options end-to-end: a stored string value containing a space is quoted
  ;; in the rendered output.
  (it "show-options-quotes-values-with-spaces"
    (with-single-option ("status-left" "a b")
      (let ((out (cl-tmux/options:show-options)))
        (expect (search "status-left \"a b\"" out)))))

  ;; show-options renders an empty string option value as '' (tmux convention).
  (it "show-options-empty-string-value-renders-as-quote-pair"
    (with-single-option ("status-left" "")
      (let ((out (cl-tmux/options:show-options)))
        (expect (search "status-left ''" out))))))
