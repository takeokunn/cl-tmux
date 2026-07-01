(in-package #:cl-tmux/test)

;;;; Session tests — part B: start-directory slot, suppress-update-environment,
;;;; +default-update-environment+, all-panes ordering, session-select-window clears flags.

(in-suite model-suite)

;;; ── session-start-directory slot ─────────────────────────────────────────────

(test session-start-directory-defaults-nil
  "session-start-directory defaults to NIL for a freshly created session."
  (let ((sess (make-session :id 1 :name "s")))
    (is (null (cl-tmux/model::session-start-directory sess))
        "session-start-directory must default to NIL")))

(test session-start-directory-settable
  "session-start-directory can be set to a path string and read back."
  (let ((sess (make-session :id 1 :name "s")))
    (setf (cl-tmux/model::session-start-directory sess) "/home/user")
    (is (string= "/home/user" (cl-tmux/model::session-start-directory sess))
        "session-start-directory must return the value written via setf")))

;;; ── *suppress-update-environment* ────────────────────────────────────────────

(test suppress-update-environment-is-variable
  "*suppress-update-environment* is a special variable that can be rebound."
  (let ((cl-tmux/model:*suppress-update-environment* t))
    (is-true cl-tmux/model:*suppress-update-environment*
             "*suppress-update-environment* must be T when dynamically bound to T"))
  (is (null cl-tmux/model:*suppress-update-environment*)
      "*suppress-update-environment* must revert to NIL after the dynamic binding exits"))

;;; ── +default-update-environment+ constant ────────────────────────────────────

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

;;; ── *update-environment* dynamic variable ────────────────────────────────────

(test update-environment-dynamic-variable-rebindable
  "*update-environment* is a special variable that can be dynamically rebound."
  (let ((orig cl-tmux/model:*update-environment*))
    (let ((cl-tmux/model:*update-environment* (list "CUSTOM_VAR")))
      (is (equal (list "CUSTOM_VAR") cl-tmux/model:*update-environment*)
          "*update-environment* must reflect the dynamic binding"))
    (is (equal orig cl-tmux/model:*update-environment*)
        "*update-environment* must revert after the binding exits")))

;;; ── session-select-window clears activity/silence flags ─────────────────────

(test session-select-window-clears-activity-flag
  "session-select-window clears the window-activity-flag when selecting a window."
  (let* ((w0   (make-window :id 0 :name "a" :activity-flag t))
         (sess (make-session :id 1 :name "s" :windows (list w0))))
    (session-select-window sess w0)
    (is-false (cl-tmux/model::window-activity-flag w0)
              "window-activity-flag must be cleared when the window is selected")))

(test session-select-window-clears-silence-flag
  "session-select-window clears the window-silence-flag when selecting a window."
  (let* ((w0   (make-window :id 0 :name "a" :silence-flag t))
         (sess (make-session :id 1 :name "s" :windows (list w0))))
    (session-select-window sess w0)
    (is-false (cl-tmux/model::window-silence-flag w0)
              "window-silence-flag must be cleared when the window is selected")))

;;; ── all-panes ordering ───────────────────────────────────────────────────────

(test all-panes-preserves-window-order
  "all-panes returns panes in window-list order (first window's panes first)."
  (let* ((p0   (make-no-pty-pane 1 0 0 20 5))
         (p1   (make-no-pty-pane 2 0 0 20 5))
         (w0   (make-window :id 0 :name "w0" :panes (list p0)))
         (w1   (make-window :id 1 :name "w1" :panes (list p1)))
         (sess (make-session :id 1 :name "s" :windows (list w0 w1))))
    (let ((panes (all-panes sess)))
      (is (eq p0 (first panes))
          "first pane must come from the first window")
      (is (eq p1 (second panes))
          "second pane must come from the second window"))))

;;; ── session-windows returns the window list ──────────────────────────────────

(test session-windows-returns-complete-list
  "session-windows returns all windows inserted via session-insert-window."
  (let* ((w0   (make-window :id 0 :name "a"))
         (w1   (make-window :id 1 :name "b"))
         (w2   (make-window :id 2 :name "c"))
         (sess (make-session :id 1 :name "s" :windows nil)))
    (session-insert-window sess w0)
    (session-insert-window sess w2)
    (session-insert-window sess w1)
    (is (= 3 (length (session-windows sess)))
        "session-windows must list all inserted windows")
    (is-true (member w0 (session-windows sess)) "w0 must be in the list")
    (is-true (member w1 (session-windows sess)) "w1 must be in the list")
    (is-true (member w2 (session-windows sess)) "w2 must be in the list")))

;;; ── session-environment accessors (structural) ──────────────────────────────

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

;;; ── session-child-environment returns a list ─────────────────────────────────

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

;;; ── %environment-entry-name / %environment-entry-value ──────────────────────

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

;;; ── %environment-strings-to-table / %environment-table-to-list round-trip ───

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

;;; ── %assert-environment-variable-name ────────────────────────────────────────

(test assert-environment-variable-name-accepts-valid-names
  "%assert-environment-variable-name does not signal for valid names."
  (dolist (name '("HOME" "PATH" "MY_VAR_1"))
    (finishes (cl-tmux/model::%assert-environment-variable-name name))))

(test assert-environment-variable-name-rejects-invalid-names
  "%assert-environment-variable-name signals an error for NIL, empty, non-string,
   or names containing '='."
  (dolist (bad (list nil "" "HAS=EQUALS" 42))
    (signals error (cl-tmux/model::%assert-environment-variable-name bad))))

;;; ── process-environment-value / process-environment-names ───────────────────

(test process-environment-value-reads-live-process-environment
  "process-environment-value returns the value of a variable set in the real
   process environment, and NIL for one that has never been set."
  (with-temporary-posix-environment-variable ("CLTMUX_TEST_PROC_ENV_VAL" "hello")
    (is (string= "hello" (cl-tmux/model:process-environment-value "CLTMUX_TEST_PROC_ENV_VAL"))
        "must read back the value just set in the process environment"))
  (is (null (cl-tmux/model:process-environment-value "__CL_TMUX_DEFINITELY_UNSET_VAR__"))
      "an unset variable must return NIL"))

(test process-environment-names-includes-known-set-variable
  "process-environment-names returns a sorted list of names that includes a
   variable known to be set in the current process environment."
  (with-temporary-posix-environment-variable ("CLTMUX_TEST_PROC_ENV_NAMES" "x")
    (let ((names (cl-tmux/model:process-environment-names)))
      (is (listp names) "must return a list")
      (is-true (member "CLTMUX_TEST_PROC_ENV_NAMES" names :test #'string=)
               "the freshly-set variable must appear in process-environment-names")
      (is (equal (sort (copy-list names) #'string<) names)
          "process-environment-names must already be sorted"))))

;;; ── process-set-environment / process-unset-environment ──────────────────────

(test process-set-environment-writes-and-returns-value
  "process-set-environment writes NAME=VALUE into the real process environment
   (readable back via process-environment-value) and returns VALUE."
  (with-temporary-posix-environment-variable ("CLTMUX_TEST_PROC_SET_ENV" nil)
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
  (with-temporary-posix-environment-variable ("CLTMUX_TEST_PROC_UNSET_ENV" "present")
    (let ((result (cl-tmux/model:process-unset-environment "CLTMUX_TEST_PROC_UNSET_ENV")))
      (is (string= "CLTMUX_TEST_PROC_UNSET_ENV" result)
          "process-unset-environment must return NAME")
      (is (null (cl-tmux/model:process-environment-value "CLTMUX_TEST_PROC_UNSET_ENV"))
          "the variable must be gone from the process environment after unset"))))

;;; ── %apply-session-overlay ───────────────────────────────────────────────────

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

;;; ── %apply-extra-env ──────────────────────────────────────────────────────────

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
