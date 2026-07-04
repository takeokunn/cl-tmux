(in-package #:cl-tmux/test)

;;;; Session tests — environment overlay, process helpers, and child env merge.

(in-suite model-suite)

;;; ── update-environment defaults ────────────────────────────────────────────

(test suppress-update-environment-is-variable
  "*suppress-update-environment* is a special variable that can be rebound."
  (let ((cl-tmux/model:*suppress-update-environment* t))
    (is-true cl-tmux/model:*suppress-update-environment*
             "*suppress-update-environment* must be T when dynamically bound to T"))
  (is (null cl-tmux/model:*suppress-update-environment*)
      "*suppress-update-environment* must revert to NIL after the dynamic binding exits"))

(test default-update-environment-is-list-of-strings
  "+default-update-environment+ is a non-empty list of strings."
  (let ((val cl-tmux/model:+default-update-environment+))
    (is (listp val)
        "+default-update-environment+ must be a list")
    (is (plusp (length val))
        "+default-update-environment+ must be non-empty")
    (dolist (item val)
      (is (stringp item)
          "+default-update-environment+ entries must be strings, got ~S" item))))

(test update-environment-dynamic-variable-rebindable
  "*update-environment* is a special variable that can be dynamically rebound."
  (let ((orig cl-tmux/model:*update-environment*))
    (let ((cl-tmux/model:*update-environment* (list "CUSTOM_VAR")))
      (is (equal (list "CUSTOM_VAR") cl-tmux/model:*update-environment*)
          "*update-environment* must reflect the dynamic binding"))
    (is (equal orig cl-tmux/model:*update-environment*)
        "*update-environment* must revert after the binding exits")))

(test get-update-environment-vars-returns-alist
  "get-update-environment-vars returns an alist of (name . value) pairs."
  (let ((result (get-update-environment-vars)))
    (is (listp result)
        "get-update-environment-vars must return a list")
    (dolist (entry result)
      (is (consp entry)
          "each entry must be a cons pair")
      (is (stringp (car entry))
          "each entry key must be a string")
      (is (stringp (cdr entry))
          "each entry value must be a string"))))

(test get-update-environment-vars-respects-star-update-environment
  "get-update-environment-vars only queries variables listed in *update-environment*."
  (let ((*update-environment* (list "__CL_TMUX_NONEXISTENT_VAR_99999__")))
    (let ((result (get-update-environment-vars)))
      (is (null result)
          "when the env var is absent, result must be NIL"))))

(test get-update-environment-vars-set-variable-included
  "get-update-environment-vars includes variables that ARE set in the environment."
  ;; HOME is reliably set in both POSIX and Nix sandbox environments.
  (let ((*update-environment* (list "HOME")))
    (let ((result (get-update-environment-vars)))
      ;; HOME should be present (if not, the test is vacuously safe to skip)
      (when (sb-ext:posix-getenv "HOME")
        (is (= 1 (length result))
            "exactly one entry when one queried variable is set")
        (is (string= "HOME" (caar result))
            "entry key must be HOME")
        (is (stringp (cdar result))
            "entry value must be a string")))))

;;; ── session environment overlay ────────────────────────────────────────────

(test session-environment-hash-table-by-default
  "session-environment returns a hash-table for a freshly made session."
  (let ((sess (make-session :id 1 :name "s")))
    (is (hash-table-p (session-environment sess))
        "session-environment must return a hash-table")))

(test session-environment-names-returns-list
  "session-environment-names returns a list for a session with no set variables."
  (let ((sess (make-session :id 1 :name "s")))
    (is (listp (session-environment-names sess))
        "session-environment-names must return a list")))

(test session-set-and-get-environment
  "session-set-environment stores a value retrievable by session-environment-value."
  (let ((sess (make-session :id 1 :name "s")))
    (session-set-environment sess "MYVAR" "myval")
    (multiple-value-bind (value source)
        (session-environment-value sess "MYVAR")
      (is (string= "myval" value) "retrieved value must match stored value")
      (is (eq :session source) "source must be :session for a session-local variable"))))

(test session-unset-environment-hides-variable
  "session-unset-environment marks a variable as explicitly unset, hiding the process value."
  (let ((sess (make-session :id 1 :name "s")))
    (session-unset-environment sess "NOSUCHENV_XYZ")
    (multiple-value-bind (value source)
        (session-environment-value sess "NOSUCHENV_XYZ")
      (is (null value) "unset variable must return NIL value")
      (is (eq :unset source) "unset variable must report :unset source"))))

(test session-environment-value-table
  "session-environment-value returns the correct value and source for overlay and
   process fallback scenarios.
   Each row: (env-name action expected-value expected-source description)."
  (dolist (row '(("CLTMUX_TEST_SESSION_ENV_A" :none  "from-process"  :process "absent overlay must inherit process value")
                 ("CLTMUX_TEST_SESSION_ENV_B" :set   "from-overlay"  :session "overlay must shadow process value")
                 ("CLTMUX_TEST_SESSION_ENV_C" :unset nil             :unset   "explicit unset must hide process value")))
    (destructuring-bind (name-str action expected-val expected-src desc) row
      (with-session-and-env-var (sess name name-str "from-process")
        (ecase action
          (:none  nil)
          (:set   (session-set-environment sess name "from-overlay"))
          (:unset (session-unset-environment sess name)))
        (multiple-value-bind (value source) (session-environment-value sess name)
          (is (equal expected-val value) desc)
          (is (eq expected-src source) desc))))))

;;; ── session-child-environment returns a list ───────────────────────────────

(test session-child-environment-returns-list
  "session-child-environment returns a list (possibly empty) of NAME=VALUE strings."
  (let ((sess (make-session :id 1 :name "s")))
    (let ((env (session-child-environment sess)))
      (is (listp env)
          "session-child-environment must return a list")
      (dolist (entry env)
        (is (stringp entry)
            "each entry of child environment must be a string")
        (is (position #\= entry)
            "each entry must contain an '=' separator")))))

;;; ── %environment-entry-name / %environment-entry-value ─────────────────────

(test environment-entry-name-and-value-table
  "%environment-entry-name and %environment-entry-value split a NAME=VALUE
   string on the first '='; both return NIL when no '=' is present."
  (dolist (row '(("FOO=bar"    "FOO" "bar" "simple pair")
                 ("A=B=C"      "A"   "B=C" "value itself may contain '='")
                 ("EMPTY="     "EMPTY" ""   "empty value after '='")
                 ("NOEQUALS"   nil   nil   "no '=' yields NIL for both")))
    (destructuring-bind (entry expected-name expected-value desc) row
      (is (equal expected-name  (cl-tmux/model::%environment-entry-name  entry)) "~A: name"  desc)
      (is (equal expected-value (cl-tmux/model::%environment-entry-value entry)) "~A: value" desc))))

(test environment-strings-to-table-and-back
  "%environment-strings-to-table builds a hash-table from NAME=VALUE strings;
   %environment-table-to-list converts it back to a sorted list of NAME=VALUE."
  (let* ((entries '("B=2" "A=1" "C=3"))
         (table   (cl-tmux/model::%environment-strings-to-table entries)))
    (is (hash-table-p table) "must return a hash-table")
    (is (string= "1" (gethash "A" table)) "A must map to 1")
    (is (string= "2" (gethash "B" table)) "B must map to 2")
    (is (string= "3" (gethash "C" table)) "C must map to 3")
    (is (equal '("A=1" "B=2" "C=3")
               (cl-tmux/model::%environment-table-to-list table))
        "table-to-list must return entries sorted by name")))

(test environment-strings-to-table-skips-entries-without-equals
  "%environment-strings-to-table silently skips entries with no '=' separator."
  (let ((table (cl-tmux/model::%environment-strings-to-table '("GOOD=1" "BADENTRY"))))
    (is (= 1 (hash-table-count table))
        "only the well-formed entry must be stored")
    (is (string= "1" (gethash "GOOD" table)))))

(test assert-environment-variable-name-accepts-valid-names
  "%assert-environment-variable-name does not signal for valid names."
  (dolist (name '("HOME" "PATH" "MY_VAR_1"))
    (finishes (cl-tmux/model::%assert-environment-variable-name name))))

(test assert-environment-variable-name-rejects-invalid-names
  "%assert-environment-variable-name signals an error for NIL, empty, non-string,
   or names containing '='."
  (dolist (bad (list nil "" "HAS=EQUALS" 42))
    (signals error (cl-tmux/model::%assert-environment-variable-name bad))))

;;; ── process-environment helpers ────────────────────────────────────────────

(test process-environment-value-reads-live-process-environment
  "process-environment-value returns the value of a variable set in the real
   process environment, and NIL for one that has never been set."
  (with-process-env-var (name "CLTMUX_TEST_PROC_ENV_VAL" "hello")
    (is (string= "hello" (cl-tmux/model:process-environment-value "CLTMUX_TEST_PROC_ENV_VAL"))
        "must read back the value just set in the process environment"))
  (is (null (cl-tmux/model:process-environment-value "__CL_TMUX_DEFINITELY_UNSET_VAR__"))
      "an unset variable must return NIL"))

(test process-environment-names-includes-known-set-variable
  "process-environment-names returns a sorted list of names that includes a
   variable known to be set in the current process environment."
  (with-process-env-var (name "CLTMUX_TEST_PROC_ENV_NAMES" "x")
    (let ((names (cl-tmux/model:process-environment-names)))
      (is (listp names) "must return a list")
      (is-true (member "CLTMUX_TEST_PROC_ENV_NAMES" names :test #'string=)
               "the freshly-set variable must appear in process-environment-names")
      (is (equal (sort (copy-list names) #'string<) names)
          "process-environment-names must already be sorted"))))

(test process-set-environment-writes-and-returns-value
  "process-set-environment writes NAME=VALUE into the real process environment
   (readable back via process-environment-value) and returns VALUE."
  (with-process-env-var (name "CLTMUX_TEST_PROC_SET_ENV" nil)
    (let ((result (cl-tmux/model:process-set-environment
                   "CLTMUX_TEST_PROC_SET_ENV" "written-value")))
      (is (string= "written-value" result)
          "process-set-environment must return the VALUE argument")
      (is (string= "written-value"
                   (cl-tmux/model:process-environment-value "CLTMUX_TEST_PROC_SET_ENV"))
          "the value must be readable back via process-environment-value"))))

(test process-unset-environment-removes-value-and-returns-name
  "process-unset-environment removes a previously-set variable from the real
   process environment and returns NAME."
  (with-process-env-var (name "CLTMUX_TEST_PROC_UNSET_ENV" "present")
    (let ((result (cl-tmux/model:process-unset-environment "CLTMUX_TEST_PROC_UNSET_ENV")))
      (is (string= "CLTMUX_TEST_PROC_UNSET_ENV" result)
          "process-unset-environment must return NAME")
      (is (null (cl-tmux/model:process-environment-value "CLTMUX_TEST_PROC_UNSET_ENV"))
          "the variable must be gone from the process environment after unset"))))

;;; ── %apply-session-overlay ─────────────────────────────────────────────────

(test apply-session-overlay-merges-set-and-removes-unset
  "%apply-session-overlay merges SESSION's set overlay into TABLE and removes
   names that were explicitly unset."
  (let ((table (make-hash-table :test #'equal))
        (sess  (make-session :id 1 :name "s")))
    (setf (gethash "KEEP" table) "process-value"
          (gethash "REMOVE" table) "process-value")
    (session-set-environment sess "KEEP" "overlay-value")
    (session-unset-environment sess "REMOVE")
    (cl-tmux/model::%apply-session-overlay sess table)
    (is (string= "overlay-value" (gethash "KEEP" table))
        "overlay set value must shadow the pre-existing table entry")
    (is (null (gethash "REMOVE" table))
        "unset name must be removed from TABLE")))

(test apply-session-overlay-nil-session-is-noop
  "%apply-session-overlay does nothing when SESSION is NIL."
  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash "UNTOUCHED" table) "value")
    (finishes (cl-tmux/model::%apply-session-overlay nil table))
    (is (string= "value" (gethash "UNTOUCHED" table))
        "table must be unchanged when session is NIL")))

;;; ── %apply-extra-env ───────────────────────────────────────────────────────

(test apply-extra-env-merges-valid-pairs
  "%apply-extra-env merges (NAME . VALUE) string conses into TABLE."
  (let ((table (make-hash-table :test #'equal)))
    (cl-tmux/model::%apply-extra-env '(("A" . "1") ("B" . "2")) table)
    (is (string= "1" (gethash "A" table)))
    (is (string= "2" (gethash "B" table)))))

(test apply-extra-env-skips-malformed-pairs
  "%apply-extra-env silently skips entries that are not (string . string) conses."
  (let ((table (make-hash-table :test #'equal)))
    (cl-tmux/model::%apply-extra-env (list '("OK" . "yes") 42 '(1 . 2) '("BAD" . 7)) table)
    (is (= 1 (hash-table-count table))
        "only the well-formed (string . string) pair must be merged")
    (is (string= "yes" (gethash "OK" table)))))
