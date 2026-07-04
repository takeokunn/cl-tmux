(in-package #:cl-tmux/test)

;;;; config directive tests — source-file, run-shell, and path expansion

(in-suite config-directives-suite)

;;; ── source-file directive ──────────────────────────────────────────────────

(test source-file-directive-loads-temp-file
  "source-file applies a config file from disk, returning T."
  (with-isolated-config
    (with-temp-config-file (p "bind z next-window")
      (assert-config-directive-applied (list "source-file" (namestring p))
                                       "source-file temp file")
      (is (eq :next-window (lookup-key-binding #\z))
          "#\\z must be bound after source-file"))))

(test source-file-short-alias-is-rejected
  "The config loader accepts only the canonical source-file command name."
  (with-isolated-config
    (with-temp-config-file (p "set-option -g status-left SOURCEALIAS")
      (assert-config-directive-rejected (list "source" (namestring p))
                                        "source alias")
      (is (not (string= "SOURCEALIAS" (cl-tmux/options:get-option "status-left")))
          "source alias must not execute the file"))))

(test source-file-missing-returns-nil-and-logs
  "source-file on a nonexistent file returns NIL and logs tmux's diagnostic."
  (with-isolated-config
    (let ((cl-tmux::*message-log* nil))
      (is (null (apply-config-directive
                 '("source-file" "/nonexistent-cl-tmux-config-abc.conf")))
          "source-file missing path must fail")
      (is (= 1 (length cl-tmux::*message-log*))
          "missing path must log exactly one diagnostic")
      (is (search "No such file or directory"
                  (cdr (first cl-tmux::*message-log*)))
          "diagnostic must mention the OS error"))))

(test source-file-n-parse-only-does-not-execute
  "source-file -n parses the file but executes NOTHING (tmux CMD_PARSE_PARSEONLY).
   Asserts via an OPTION the file would set (a key like z has a DEFAULT binding, so
   'unbound' is not a reliable 'not executed' signal)."
  (with-isolated-config
    (with-temp-config-file (p "set-option -g status-left PARSEONLY")
      (assert-config-directive-applied (list "source-file" "-n" (namestring p))
                                       "source-file -n parse only")
      (is (not (string= "PARSEONLY" (cl-tmux/options:get-option "status-left")))
          "-n must NOT execute: the option is left unchanged"))))

(test source-file-without-n-executes-control
  "Control: WITHOUT -n the same file DOES set the option — isolating that -n is what
   suppresses execution."
  (with-isolated-config
    (with-temp-config-file (p "set-option -g status-left EXECUTED")
      (apply-config-directive (list "source-file" (namestring p)))
      (is (string= "EXECUTED" (cl-tmux/options:get-option "status-left"))
          "without -n the option is set"))))

(test source-file-clustered-qn-does-not-execute
  "Clustered -qn is also parse-only (q tolerated, n suppresses execution)."
  (with-isolated-config
    (with-temp-config-file (p "set-option -g status-left QNFLAG")
      (apply-config-directive (list "source-file" "-qn" (namestring p)))
      (is (not (string= "QNFLAG" (cl-tmux/options:get-option "status-left")))
          "-qn must not execute"))))

(test parse-source-file-flags-clustered
  "%parse-source-file-flags parses clustered -Fnqv and returns the path positionals."
  (multiple-value-bind (n q v f rest)
      (cl-tmux/config::%parse-source-file-flags '("-Fnqv" "/path/to.conf"))
    (is-true  n "parse-only (n)")
    (is-true  q "quiet (q)")
    (is-true  v "verbose (v)")
    (is-true  f "format (F)")
    (is (equal '("/path/to.conf") rest) "positionals = the path")))

(test parse-source-file-flags-target-pane
  "%parse-source-file-flags consumes tmux's -t target-pane without treating it as a path."
  (multiple-value-bind (n q v f rest)
      (cl-tmux/config::%parse-source-file-flags '("-q" "-t" "%1" "/path/to.conf"))
    (declare (ignore n v f))
    (is-true q "quiet (q)")
    (is (equal '("/path/to.conf") rest) "target pane must not remain in positionals")))

(test consume-leading-flag-tokens-stops-at-first-non-flag
  "%consume-leading-flag-tokens walks leading flags and stops at the first positional token."
  (let ((seen '()))
    (is (equal '("cmd" "arg")
               (cl-tmux/config::%consume-leading-flag-tokens
                '("-b" "-F" "cmd" "arg")
                (lambda (tok rest)
                  (push tok seen)
                  (values rest t)))))
    (is (equal '("-F" "-b") seen)
        "callback must see the leading flags in order")))

(test source-file-F-expands-format-path
  "source-file -F expands the path as a format string before loading.
   Uses a known literal path (no #{...} variables) to confirm that -F does
   not break plain paths (the expanded form equals the original string)."
  (with-isolated-config
    (with-temp-config-file (p "set-option -g status-left FFORMAT")
      ;; A plain path contains no #{} variables; expand-format returns it unchanged.
      ;; This test confirms the -F code path does not corrupt literal paths.
      (let ((result (cl-tmux/config:apply-config-directive
                     (list "source-file" "-F" (namestring p)))))
        (is-true result "source-file -F with a plain path must succeed")
        (is (string= "FFORMAT" (cl-tmux/options:get-option "status-left"))
            "-F with a plain path must load and execute the file normally")))))

;;; ── run-shell directive ───────────────────────────────────────────────────

(test run-shell-apply-directive-table
  "run-shell returns T regardless of exit code."
  (dolist (c '(("run-shell" ("true")  "run-shell returns T")
               ("run-shell" ("false") "run-shell error silently returns T")))
    (destructuring-bind (cmd args desc) c
      (assert-config-directive-applied (cons cmd args) desc))))

;;; ── run-shell flag handling (-b / -C / -E / -c) ───────────────────────────
;;;
;;; %apply-run-shell-directive strips leading flags so the common
;;; `run-shell -b 'cmd'` form — which the fixed-arity table silently dropped —
;;; is handled.  These tests assert the handler's RETURN VALUE (handled vs not)
;;; rather than shell side-effects; `true` is used so any actual execution is
;;; harmless and fast.

(test run-shell-handler-table
  "%apply-run-shell-directive returns T for handled forms and NIL for non-run commands.
   Each row is (expected cmd args description)."
  (dolist (c '((t   "run-shell" ("-b" "true")                 "run-shell -b true (background flag)")
               (nil "bind"      ("x" "next-window")           "bind (non-run command)")
               (t   "run-shell" ("-b")                        "run-shell -b only (flag-only no-op)")
               (t   "run-shell" ("-C" "new-window")           "run-shell -C <cmd> (tmux-cmd no-op)")
               (t   "run-shell" ("-E" "true")                 "run-shell -E true (combine stderr)")
               (t   "run-shell" ("-c" "/tmp" "true")          "run-shell -c /tmp true (start-directory)")
               (t   "run-shell" ("-bCE" "true")               "run-shell clustered no-arg flags")
               (nil "run-shell" ("-d" "0" "true")             "run-shell -d 0 true (delay flag removed)")
               (nil "run-shell" ("-t" "0" "-b" "true")        "run-shell -t 0 -b true (target flag removed)")
               (nil "run-shell" ("-x" "true")                 "run-shell -x true (unknown flag rejected)")
               (nil "run"       ("true")                      "run alias rejected")
               (t   "run-shell" ()                            "run-shell no args (empty no-op)")
               (t   "run-shell" ("-b" "echo" "hello" "world") "run-shell -b echo hello world (multi-word)")))
    (destructuring-bind (expected cmd args desc) c
      (with-isolated-config
        (let ((result (cl-tmux/config::%apply-run-shell-directive cmd args)))
          (if expected
              (is (eq t result) "~A must return T (got ~S)" desc result)
              (is (null result) "~A must return NIL (got ~S)" desc result)))))))

(test run-shell-c-start-directory-controls-shell-cwd
  "run-shell -c runs the shell command from the requested start-directory."
  (let* ((tmp-dir (merge-pathnames "cl-tmux-run-shell-c/"
                                   (uiop:temporary-directory)))
         (out-file (merge-pathnames "pwd.txt" tmp-dir)))
    (ensure-directories-exist tmp-dir)
    (unwind-protect
         (progn
           (let ((handled (cl-tmux/config::%apply-run-shell-directive
                           "run-shell"
                           (list "-c" (namestring tmp-dir)
                                 (format nil "pwd > ~A" (namestring out-file))))))
             (is (eq t handled) "run-shell -c must be handled"))
           (let ((expected-dir (string-right-trim
                                '(#\/)
                                (namestring (truename tmp-dir))))
                 (actual-dir (string-right-trim
                              '(#\Newline #\Return)
                              (uiop:read-file-string out-file))))
             (is (string= actual-dir expected-dir)
                 "pwd output must match the -c start-directory")))
      (ignore-errors
        (uiop:delete-directory-tree tmp-dir :validate t :if-does-not-exist :ignore)))))

;;; ── %expand-leading-tilde ──────────────────────────────────────────────────

(test expand-leading-tilde-table
  "%expand-leading-tilde expands ~/... to $HOME/...; all other paths pass through."
  (let ((home (or (ignore-errors (sb-ext:posix-getenv "HOME")) "~")))
    (dolist (c (list (list "~/x"                     (concatenate 'string home "/x")                    "~/x → $HOME/x")
                     (list "~/.tmux/plugins/tpm/tpm" (concatenate 'string home "/.tmux/plugins/tpm/tpm") "full tpm path")
                     (list "/abs"   "/abs"   "absolute path unchanged")
                     (list "rel"    "rel"    "relative path unchanged")
                     (list "~"      "~"      "bare ~ unchanged")
                     (list "~/"     "~/"     "exact ~/ unchanged")
                     (list "~user"  "~user"  "~user unchanged")
                     (list "a/~/b"  "a/~/b"  "embedded ~ unchanged")))
      (destructuring-bind (input expected desc) c
        (is (string= expected (cl-tmux/config::%expand-leading-tilde input))
            "~A" desc)))))
