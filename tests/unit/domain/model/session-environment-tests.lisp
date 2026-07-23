(in-package #:cl-tmux/test)

;;;; Session tests — environment overlay, process helpers, and child env merge.

(describe "model-suite"

  ;;; ── update-environment defaults ────────────────────────────────────────────

  ;; *suppress-update-environment* is a special variable that can be rebound.
  (it "suppress-update-environment-is-variable"
    (let ((cl-tmux/model:*suppress-update-environment* t))
      (expect cl-tmux/model:*suppress-update-environment* :to-be-truthy))
    (expect (null cl-tmux/model:*suppress-update-environment*)))

  ;; +default-update-environment+ is a non-empty list of strings.
  (it "default-update-environment-is-list-of-strings"
    (let ((val cl-tmux/model:+default-update-environment+))
      (expect (listp val))
      (expect (plusp (length val)))
      (dolist (item val)
        (expect (stringp item)))))

  ;; *update-environment* is a special variable that can be dynamically rebound.
  (it "update-environment-dynamic-variable-rebindable"
    (let ((orig cl-tmux/model:*update-environment*))
      (let ((cl-tmux/model:*update-environment* (list "CUSTOM_VAR")))
        (expect (equal (list "CUSTOM_VAR") cl-tmux/model:*update-environment*)))
      (expect (equal orig cl-tmux/model:*update-environment*))))

  ;; get-update-environment-vars returns an alist of (name . value) pairs.
  (it "get-update-environment-vars-returns-alist"
    (let ((result (get-update-environment-vars)))
      (expect (listp result))
      (dolist (entry result)
        (expect (consp entry))
        (expect (stringp (car entry)))
        (expect (stringp (cdr entry))))))

  ;; get-update-environment-vars only queries variables listed in *update-environment*.
  (it "get-update-environment-vars-respects-star-update-environment"
    (let ((*update-environment* (list "__CL_TMUX_NONEXISTENT_VAR_99999__")))
      (let ((result (get-update-environment-vars)))
        (expect (null result)))))

  ;; get-update-environment-vars includes variables that ARE set in the environment.
  (it "get-update-environment-vars-set-variable-included"
    ;; HOME is reliably set in both POSIX and Nix sandbox environments.
    (let ((*update-environment* (list "HOME")))
      (let ((result (get-update-environment-vars)))
        ;; HOME should be present (if not, the test is vacuously safe to skip)
        (when (sb-ext:posix-getenv "HOME")
          (expect (= 1 (length result)))
          (expect (string= "HOME" (caar result)))
          (expect (stringp (cdar result)))))))

  ;;; ── session environment overlay ────────────────────────────────────────────

  ;; session-environment returns a hash-table for a freshly made session.
  (it "session-environment-hash-table-by-default"
    (let ((sess (make-session :id 1 :name "s")))
      (expect (hash-table-p (session-environment sess)))))

  ;; session-environment-names returns a list for a session with no set variables.
  (it "session-environment-names-returns-list"
    (let ((sess (make-session :id 1 :name "s")))
      (expect (listp (session-environment-names sess)))))

  ;; session-set-environment stores a value retrievable by session-environment-value.
  (it "session-set-and-get-environment"
    (let ((sess (make-session :id 1 :name "s")))
      (session-set-environment sess "MYVAR" "myval")
      (multiple-value-bind (value source)
          (session-environment-value sess "MYVAR")
        (expect (string= "myval" value))
        (expect (eq :session source)))))

  ;; session-unset-environment marks a variable as explicitly unset, hiding the process value.
  (it "session-unset-environment-hides-variable"
    (let ((sess (make-session :id 1 :name "s")))
      (session-unset-environment sess "NOSUCHENV_XYZ")
      (multiple-value-bind (value source)
          (session-environment-value sess "NOSUCHENV_XYZ")
        (expect (null value))
        (expect (eq :unset source)))))

  ;; session-environment-value returns the correct value and source for overlay and
  ;; process fallback scenarios.
  ;; Each row: (env-name action expected-value expected-source description).
  (it "session-environment-value-table"
    (dolist (row '(("CLTMUX_TEST_SESSION_ENV_A" :none  "from-process"  :process "absent overlay must inherit process value")
                   ("CLTMUX_TEST_SESSION_ENV_B" :set   "from-overlay"  :session "overlay must shadow process value")
                   ("CLTMUX_TEST_SESSION_ENV_C" :unset nil             :unset   "explicit unset must hide process value")))
      (destructuring-bind (name-str action expected-val expected-src desc) row
        (declare (ignore desc))
        (with-session-and-env-var (sess name name-str "from-process")
          (ecase action
            (:none  nil)
            (:set   (session-set-environment sess name "from-overlay"))
            (:unset (session-unset-environment sess name)))
          (multiple-value-bind (value source) (session-environment-value sess name)
            (expect (equal expected-val value))
            (expect (eq expected-src source)))))))

  ;;; ── session-child-environment returns a list ───────────────────────────────

  ;; session-child-environment returns a list (possibly empty) of NAME=VALUE strings.
  (it "session-child-environment-returns-list"
    (let ((sess (make-session :id 1 :name "s")))
      (let ((env (session-child-environment sess)))
        (expect (listp env))
        (dolist (entry env)
          (expect (stringp entry))
          (expect (position #\= entry))))))

  ;;; ── %environment-entry-name / %environment-entry-value ─────────────────────

  ;; %environment-entry-name and %environment-entry-value split a NAME=VALUE
  ;; string on the first '='; both return NIL when no '=' is present.
  (it "environment-entry-name-and-value-table"
    (dolist (row '(("FOO=bar"    "FOO" "bar" "simple pair")
                   ("A=B=C"      "A"   "B=C" "value itself may contain '='")
                   ("EMPTY="     "EMPTY" ""   "empty value after '='")
                   ("NOEQUALS"   nil   nil   "no '=' yields NIL for both")))
      (destructuring-bind (entry expected-name expected-value desc) row
        (declare (ignore desc))
        (expect (equal expected-name  (cl-tmux/model::%environment-entry-name  entry)))
        (expect (equal expected-value (cl-tmux/model::%environment-entry-value entry))))))

  ;; %environment-strings-to-table builds a hash-table from NAME=VALUE strings;
  ;; %environment-table-to-list converts it back to a sorted list of NAME=VALUE.
  (it "environment-strings-to-table-and-back"
    (let* ((entries '("B=2" "A=1" "C=3"))
           (table   (cl-tmux/model::%environment-strings-to-table entries)))
      (expect (hash-table-p table))
      (expect (string= "1" (gethash "A" table)))
      (expect (string= "2" (gethash "B" table)))
      (expect (string= "3" (gethash "C" table)))
      (expect (equal '("A=1" "B=2" "C=3")
                 (cl-tmux/model::%environment-table-to-list table)))))

  ;; %environment-strings-to-table silently skips entries with no '=' separator.
  (it "environment-strings-to-table-skips-entries-without-equals"
    (let ((table (cl-tmux/model::%environment-strings-to-table '("GOOD=1" "BADENTRY"))))
      (expect (= 1 (hash-table-count table)))
      (expect (string= "1" (gethash "GOOD" table)))))

  ;; %assert-environment-variable-name does not signal for valid names.
  (it "assert-environment-variable-name-accepts-valid-names"
    (dolist (name '("HOME" "PATH" "MY_VAR_1"))
      (finishes (cl-tmux/model::%assert-environment-variable-name name))))

  ;; %assert-environment-variable-name signals an error for NIL, empty, non-string,
  ;; or names containing '='.
  (it "assert-environment-variable-name-rejects-invalid-names"
    (dolist (bad (list nil "" "HAS=EQUALS" 42))
      (signals error (cl-tmux/model::%assert-environment-variable-name bad))))

  ;;; ── process-environment helpers ────────────────────────────────────────────

  ;; process-environment-value returns the value of a variable set in the real
  ;; process environment, and NIL for one that has never been set.
  (it "process-environment-value-reads-live-process-environment"
    (with-process-env-var (name "CLTMUX_TEST_PROC_ENV_VAL" "hello")
      (expect (string= "hello" (cl-tmux/model:process-environment-value "CLTMUX_TEST_PROC_ENV_VAL"))))
    (expect (null (cl-tmux/model:process-environment-value "__CL_TMUX_DEFINITELY_UNSET_VAR__"))))

  ;; process-environment-names returns a sorted list of names that includes a
  ;; variable known to be set in the current process environment.
  (it "process-environment-names-includes-known-set-variable"
    (with-process-env-var (name "CLTMUX_TEST_PROC_ENV_NAMES" "x")
      (let ((names (cl-tmux/model:process-environment-names)))
        (expect (listp names))
        (expect (member "CLTMUX_TEST_PROC_ENV_NAMES" names :test #'string=) :to-be-truthy)
        (expect (equal (sort (copy-list names) #'string<) names)))))

  ;; process-set-environment writes NAME=VALUE into the real process environment
  ;; (readable back via process-environment-value) and returns VALUE.
  (it "process-set-environment-writes-and-returns-value"
    (with-process-env-var (name "CLTMUX_TEST_PROC_SET_ENV" nil)
      (let ((result (cl-tmux/model:process-set-environment
                     "CLTMUX_TEST_PROC_SET_ENV" "written-value")))
        (expect (string= "written-value" result))
        (expect (string= "written-value"
                     (cl-tmux/model:process-environment-value "CLTMUX_TEST_PROC_SET_ENV"))))))

  ;; process-unset-environment removes a previously-set variable from the real
  ;; process environment and returns NAME.
  (it "process-unset-environment-removes-value-and-returns-name"
    (with-process-env-var (name "CLTMUX_TEST_PROC_UNSET_ENV" "present")
      (let ((result (cl-tmux/model:process-unset-environment "CLTMUX_TEST_PROC_UNSET_ENV")))
        (expect (string= "CLTMUX_TEST_PROC_UNSET_ENV" result))
        (expect (null (cl-tmux/model:process-environment-value "CLTMUX_TEST_PROC_UNSET_ENV"))))))

  ;;; ── %apply-session-overlay ─────────────────────────────────────────────────

  ;; %apply-session-overlay merges SESSION's set overlay into TABLE and removes
  ;; names that were explicitly unset.
  (it "apply-session-overlay-merges-set-and-removes-unset"
    (let ((table (make-hash-table :test #'equal))
          (sess  (make-session :id 1 :name "s")))
      (setf (gethash "KEEP" table) "process-value"
            (gethash "REMOVE" table) "process-value")
      (session-set-environment sess "KEEP" "overlay-value")
      (session-unset-environment sess "REMOVE")
      (cl-tmux/model::%apply-session-overlay sess table)
      (expect (string= "overlay-value" (gethash "KEEP" table)))
      (expect (null (gethash "REMOVE" table)))))

  ;; %apply-session-overlay does nothing when SESSION is NIL.
  (it "apply-session-overlay-nil-session-is-noop"
    (let ((table (make-hash-table :test #'equal)))
      (setf (gethash "UNTOUCHED" table) "value")
      (finishes (cl-tmux/model::%apply-session-overlay nil table))
      (expect (string= "value" (gethash "UNTOUCHED" table)))))

  ;;; ── %apply-extra-env ───────────────────────────────────────────────────────

  ;; %apply-extra-env merges (NAME . VALUE) string conses into TABLE.
  (it "apply-extra-env-merges-valid-pairs"
    (let ((table (make-hash-table :test #'equal)))
      (cl-tmux/model::%apply-extra-env '(("A" . "1") ("B" . "2")) table)
      (expect (string= "1" (gethash "A" table)))
      (expect (string= "2" (gethash "B" table)))))

  ;; %apply-extra-env silently skips entries that are not (string . string) conses.
  (it "apply-extra-env-skips-malformed-pairs"
    (let ((table (make-hash-table :test #'equal)))
      (cl-tmux/model::%apply-extra-env (list '("OK" . "yes") 42 '(1 . 2) '("BAD" . 7)) table)
      (expect (= 1 (hash-table-count table)))
      (expect (string= "yes" (gethash "OK" table))))))
