(in-package #:cl-tmux/test)

;;;; Tests for cl-tmux/options: option registry and get/set/coercion.
;;;;
;;;; Isolation helpers (with-fresh-options, with-fresh-global-options,
;;;; with-single-option, with-single-server-option) are defined in
;;;; tests/helpers-options.lisp so that config-directives-tests can reuse them.

;;; Table-driven default-value checker.
;;; check-option-defaults collapses ~25 near-identical single-assertion tests.

(defmacro check-option-defaults (&rest entries)
  "Generate EXPECT assertions for each (name expected) or (name :registered)
   or (name :string-p) entry."
  `(progn
     ,@(mapcar
        (lambda (entry)
          (destructuring-bind (name check) entry
            (cond
              ((eq check :registered)
               `(expect (cl-tmux/options:option-defined-p ,name)))
              ((eq check :string-p)
               `(progn
                  (expect (cl-tmux/options:option-defined-p ,name))
                  (expect (stringp (cl-tmux/options:get-option ,name)))))
              (t
               `(expect (equal ,check (cl-tmux/options:get-option ,name)))))))
        entries)))

(describe "options-suite"

  ;;; get-option

  ;; get-option returns NIL for a key not in the table.
  (it "get-option-returns-nil-when-absent"
    (with-fresh-options
      (expect (null (cl-tmux/options:get-option "nonexistent")))))

  ;; get-option returns the supplied default when key is absent.
  (it "get-option-returns-default-when-absent"
    (with-fresh-options
      (expect (= 42 (cl-tmux/options:get-option "missing" 42)))))

  ;; After a registered option is removed (set -u), get-option with NO caller
  ;; default reverts to the registry spec default, mirroring tmux
  ;; options_remove_or_default.  An explicit caller default still wins.
  (it "get-option-unset-reverts-to-registry-default"
    ;; with-fresh-global-options copies *global-options* but SHARES *option-registry*,
    ;; so default-terminal's spec ("screen") is still registered.
    (with-fresh-global-options
      (remhash "default-terminal" cl-tmux/options:*global-options*)
      (expect (string= "screen" (cl-tmux/options:get-option "default-terminal")))
      ;; A registered option with no caller default also falls back even if never set.
      (remhash "history-limit" cl-tmux/options:*global-options*)
      (expect (= 2000 (cl-tmux/options:get-option "history-limit")))
      ;; An explicitly supplied default (even NIL) is still honored over the registry.
      (expect (null (cl-tmux/options:get-option "default-terminal" nil)))
      (expect (= 7 (cl-tmux/options:get-option "default-terminal" 7)))
      ;; An UNregistered key still returns the caller default / NIL (no spec to fall back to).
      (expect (null (cl-tmux/options:get-option "totally-unknown-opt")))))

  ;; get-server-option falls back to the server registry spec default when a
  ;; registered server option is absent and no caller default is supplied.
  (it "get-server-option-unset-reverts-to-registry-default"
    (with-fresh-server-options
      ;; *server-options* is empty here; *server-option-registry* is shared, so
      ;; default-terminal's server spec ("screen") and escape-time (10) apply.
      (expect (string= "screen" (cl-tmux/options:get-server-option "default-terminal")))
      (expect (= 10 (cl-tmux/options:get-server-option "escape-time")))
      ;; Explicit caller default (incl. NIL) still wins; unregistered key reads as default.
      (expect (null (cl-tmux/options:get-server-option "default-terminal" nil)))
      (expect (null (cl-tmux/options:get-server-option "nonexistent-server-opt")))))

  ;;; set-option / get-option round-trip

  ;; set-option stores a string value retrievable by get-option.
  (it "set-and-get-option-string"
    (with-fresh-options
      (cl-tmux/options:set-option "status-left" "my-session")
      (expect (string= "my-session" (cl-tmux/options:get-option "status-left")))))

  ;;; option-defined-p

  ;; option-defined-p returns T for registered options, NIL for unknowns.
  (it "option-defined-p-table"
    (dolist (row '(("status"         t   "known option → T")
                   ("no-such-option" nil "unknown option → NIL")))
      (destructuring-bind (name expected desc) row
        (declare (ignore desc))
        (expect (if expected
                (cl-tmux/options:option-defined-p name)
                (null (cl-tmux/options:option-defined-p name)))))))

  ;;; Type coercion

  ;; set-option coerces :boolean option strings: on/true/1 → T, off/0/false → NIL.
  ;; Each row: (str expected description).
  (it "boolean-coercion-table"
    (dolist (row '(("on"    t   "on → T")
                   ("true"  t   "true → T")
                   ("1"     t   "1 → T")
                   ("off"   nil "off → NIL")
                   ("0"     nil "0 → NIL")
                   ("false" nil "false → NIL")))
      (destructuring-bind (str expected desc) row
        (declare (ignore desc))
        (with-fresh-global-options
          (if expected
              (expect (cl-tmux/options:set-option "mouse" str) :to-be-truthy)
              (expect (cl-tmux/options:set-option "mouse" str) :to-be-falsy))))))

  ;; The `status` option is a CHOICE/number (off|on|2..5), NOT a boolean: a line
  ;; count is stored UNCHANGED so the renderer's status-line-count sees it.  The old
  ;; :boolean type coerced "2" to NIL, hiding a bar whose rows the layout still
  ;; reserved via *status-height* — the multi-line-status bug.
  (it "status-numeric-value-survives-coercion"
    (with-fresh-global-options
      (expect (string= "2" (cl-tmux/options:set-option "status" "2")))
      (expect (string= "2" (cl-tmux/options:get-option "status")))
      (expect (string= "off" (cl-tmux/options:set-option "status" "off")))
      (expect (string= "on" (cl-tmux/options:set-option "status" "on")))))

  ;; Setting a :integer option with a numeric string coerces to the integer value.
  (it "integer-coercion-table"
    (dolist (row '(("5000" 5000 "5000 → integer 5000")
                   ("500"   500 "500 → integer 500")))
      (destructuring-bind (str-val expected desc) row
        (declare (ignore desc))
        (with-fresh-global-options
          (expect (= expected (cl-tmux/options:set-option "history-limit" str-val)))))))

  ;; Setting a :string option with a non-string value coerces via format ~A.
  (it "string-coercion-from-non-string"
    (with-fresh-global-options
      (expect (string= "42" (cl-tmux/options:set-option "status-left" 42)))))

  ;;; all-options

  ;; all-options returns an alist of (name . value) pairs.
  (it "all-options-returns-alist"
    (let ((opts (cl-tmux/options:all-options)))
      (expect (listp opts))
      (expect (every #'consp opts))))

  ;; define-tmux-options is a registered macro.
  (it "define-tmux-options-macro-is-defined"
    (expect (macro-function 'cl-tmux/options:define-tmux-options)))

  ;;; Server options

  ;; *server-options* contains the default escape-time = 10.
  (it "server-options-escape-time-default"
    (expect (= 10 (cl-tmux/options:get-server-option "escape-time"))))

  ;; *server-options* contains exit-empty = T by default.
  (it "server-options-exit-empty-default"
    (expect (cl-tmux/options:get-server-option "exit-empty")))

  ;; set-server-option stores a value in *server-options*.
  (it "set-server-option-stores-value"
    (with-fresh-server-options
      (cl-tmux/options:set-server-option "escape-time" "100")
      (expect (= 100 (cl-tmux/options:get-server-option "escape-time")))))

  ;; set-server-option coerces boolean values.
  (it "set-server-option-boolean-coercion"
    (with-fresh-server-options
      (cl-tmux/options:set-server-option "exit-empty" "off")
      (expect (null (cl-tmux/options:get-server-option "exit-empty")))))

  ;;; show-options

  ;; show-options returns a non-empty string.
  (it "show-options-returns-string"
    (let ((out (cl-tmux/options:show-options)))
      (expect (stringp out))
      (expect (plusp (length out)))))

  ;; show-options output contains option name/value pairs.
  (it "show-options-contains-key-value-pairs"
    (let ((cl-tmux/options:*global-options*
           (let ((ht (make-hash-table :test #'equal)))
             (setf (gethash "status" ht) t)
             (setf (gethash "history-limit" ht) 2000)
             ht)))
      (let ((out (cl-tmux/options:show-options)))
        (expect (search "status" out))
        (expect (search "history-limit" out))))
    ;; Same assertions using the fixture macro
    (with-single-option ("status" t)
      (expect (search "status" (cl-tmux/options:show-options)))))

  ;; show-option returns the value of a single named option.
  (it "show-option-single-option"
    (with-single-option ("status-interval" 30)
      (let ((out (cl-tmux/options:show-option "status-interval")))
        (expect (search "status-interval" out))
        (expect (search "30" out)))))

  ;; show-option for an absent option indicates it is not set.
  (it "show-option-missing-option"
    (with-fresh-options
      (let ((out (cl-tmux/options:show-option "no-such-option")))
        (expect (search "no-such-option" out)))))

  ;; show-options with :server scope returns server options.
  (it "show-options-server-scope"
    (with-single-server-option ("escape-time" 500)
      (let ((out (cl-tmux/options:show-options :server)))
        (expect (search "escape-time" out)))))

  ;;; Registered options: style/justify checks

  ;; Style and layout options are registered with correct types.
  (it "registered-style-and-layout-options"
    (check-option-defaults
      ("status-style"                 :registered)
      ("status-justify"               :registered)
      ("window-status-current-style"  :registered)))

  ;;; Default values: parameterised table-driven check

  ;; All registered options have the documented default values.
  (it "option-default-values"
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

  ;; update-environment default contains expected variable names.
  (it "update-environment-default"
    (let ((val (cl-tmux/options:get-option "update-environment")))
      (expect (stringp val))
      (expect (search "DISPLAY" val))
      (expect (search "SSH_AUTH_SOCK" val))))

  ;;; Round-trip tests

  ;; set-titles boolean set/get round-trip.
  (it "set-option-boolean-set-titles"
    (with-fresh-global-options
      (expect (eq t (cl-tmux/options:set-option "set-titles" "on")))
      (expect (eq t (cl-tmux/options:get-option "set-titles")))
      (expect (null (cl-tmux/options:set-option "set-titles" "off")))
      (expect (null (cl-tmux/options:get-option "set-titles")))))

  ;; status-left-length integer set/get round-trip.
  (it "set-option-integer-status-left-length"
    (with-fresh-global-options
      (expect (= 80 (cl-tmux/options:set-option "status-left-length" "80")))
      (expect (= 80 (cl-tmux/options:get-option "status-left-length")))))

  ;; pane-base-index set/get round-trip.
  (it "pane-base-index-set-get"
    (with-fresh-global-options
      (expect (= 1 (cl-tmux/options:set-option "pane-base-index" "1")))
      (expect (= 1 (cl-tmux/options:get-option "pane-base-index")))))

  ;;; Tests for options-scope.lisp exports

  ;; option-scope-from-name returns :window for window-scoped option names.
  (it "option-scope-from-name-window-options"
    (dolist (name '("automatic-rename" "mode-keys" "mode-style"
                    "monitor-activity" "synchronize-panes"
                    "window-active-style" "wrap-search"
                    "pane-border-status" "remain-on-exit"))
      (expect (eq :window (cl-tmux/options:option-scope-from-name name)))))

  ;; option-scope-from-name returns :session for non-window option names.
  (it "option-scope-from-name-session-options"
    (dolist (name '("status" "status-interval" "history-limit"
                    "escape-time" "default-terminal" "prefix"
                    "display-time" "buffer-limit"))
      (expect (eq :session (cl-tmux/options:option-scope-from-name name))))))

;;; ── option-present-for-scope-p (options-scope.lisp) ───────────────────────
