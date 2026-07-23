;;;; FiveAM-surface compatibility shim implemented on top of cl-weave.
;;;;
;;;; The cl-tmux test suite (296 files, ~3,900 tests, ~11,000 checks) was
;;;; written against FiveAM's flat model: `def-suite' names a suite, `in-suite'
;;;; selects the current one, and top-level `test' forms attach to it.  cl-weave
;;;; instead nests lexically (`describe' / `it').
;;;;
;;;; Rather than rewrite every assertion, this shim reimplements the small
;;;; FiveAM surface the suite actually uses as thin macros over cl-weave's
;;;; registration engine, and the `fiveam' dependency is dropped entirely.
;;;; cl-weave is now THE test framework: it registers, runs, and reports every
;;;; test.  Files keep using `def-suite' / `in-suite' / `test' / `is', but those
;;;; symbols are cl-tmux/test's own, backed by `cl-weave::register-suite' /
;;;; `register-test' and `cl-weave:expect'.
;;;;
;;;; The shim relies on a handful of cl-weave internals (`*current-suite*',
;;;; `make-suite', `add-child', `register-test'); this is deliberate dogfooding
;;;; of a sibling library, and the coupling lives in this one file.

(in-package #:cl-tmux/test)

;;; ── Suite registry ────────────────────────────────────────────────────────
;;;
;;; FiveAM suites are named by symbol; cl-weave suites are objects named by
;;; string.  We map symbol → cl-weave suite here and auto-vivify parents so a
;;; `:in' reference works regardless of definition order.

(defvar *shim-suites* (make-hash-table :test #'eq)
  "Maps a FiveAM-style suite symbol to its backing cl-weave suite object.")

(defun ensure-shim-suite (name &optional parent-name)
  "Return the cl-weave suite for symbol NAME, creating it (and its parent
chain) if needed.  With no PARENT-NAME the suite hangs off the cl-weave root."
  (or (gethash name *shim-suites*)
      (let* ((parent (if parent-name
                         (ensure-shim-suite parent-name)
                         (cl-weave:root-suite)))
             (suite (cl-weave::make-suite
                     :name (string-downcase (symbol-name name))
                     :parent parent
                     :execution-mode :sequential)))
        (cl-weave::add-child parent suite)
        ;; Join background threads at each TOP-LEVEL suite boundary — the same
        ;; granularity FiveAM's runner used.  A cl-weave after-all fires once
        ;; the whole suite (including nested sub-suites) has run, so threads
        ;; persist across the tests WITHIN a suite (integration suites rely on
        ;; a server/reader started by one test surviving into the next) and are
        ;; joined only between top-level suites.  Nested suites inherit this.
        (unless parent-name
          (let ((cl-weave::*current-suite* suite))
            (cl-weave::register-after-all
             (lambda () (ignore-errors (stop-cl-tmux-threads))))))
        (setf (gethash name *shim-suites*) suite))))

;;; ── Suite/test definition macros ──────────────────────────────────────────

(defmacro def-suite (name &key description in)
  "Define a suite NAME (optionally nested under :IN parent).  DESCRIPTION is
accepted for FiveAM source compatibility and ignored."
  (declare (ignore description))
  `(progn (ensure-shim-suite ',name ,(and in `',in)) ',name))

(defmacro in-suite (name)
  "Select NAME as the current suite for subsequent top-level `test' forms.

Sets cl-weave's current-suite globally (not dynamically) so the flat
FiveAM authoring model works: each file loads its `in-suite' then its
`test' forms in order."
  `(progn (setf cl-weave::*current-suite* (ensure-shim-suite ',name)) ',name))

(defmacro test (name &body body)
  "Register a test NAME (a symbol) in the current suite."
  `(cl-weave::register-test ,(string-downcase (symbol-name name))
                            (lambda () ,@body)
                            :execution-mode :sequential))

;;; ── Assertions ────────────────────────────────────────────────────────────
;;;
;;; FiveAM's `is' passes when its form is non-nil and accepts an optional
;;; failure reason (format string + args) that cl-weave supplies itself, so we
;;; ignore it.  `is'/`is-true'/`is-false' map onto cl-weave's smart-assertion
;;; and truthiness matchers; `signals'/`finishes'/`fail'/`skip' forward to the
;;; cl-weave equivalents, whose call shapes already match.

(defmacro is (form &rest reason)
  "Assert FORM is non-nil (cl-weave smart assertion).  REASON is ignored."
  (declare (ignore reason))
  `(cl-weave:expect ,form))

(defmacro is-true (form &rest reason)
  "Assert FORM is truthy."
  (declare (ignore reason))
  `(cl-weave:expect ,form :to-be-truthy))

(defmacro is-false (form &rest reason)
  "Assert FORM is falsy."
  (declare (ignore reason))
  `(cl-weave:expect ,form :to-be-falsy))

(defmacro signals (condition &body body)
  "Assert BODY signals a condition of type CONDITION."
  `(cl-weave:signals ,condition ,@body))

(defmacro finishes (&body body)
  "Assert BODY completes without signalling."
  `(cl-weave:finishes ,@body))

(defmacro pass (&rest ignored)
  "Record a passing assertion unconditionally."
  (declare (ignore ignored))
  `(cl-weave:expect t))

(defmacro fail (&rest arguments)
  "Force a failing assertion with an optional reason."
  `(cl-weave:fail ,@arguments))

(defmacro skip (&rest arguments)
  "Skip the current test with an optional reason."
  `(cl-weave:skip ,@arguments))
