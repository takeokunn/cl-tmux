(in-package #:cl-tmux/test)

;;;; config directive tests — source-file, run-shell, and path expansion

(describe "config-directives-suite"

  ;;; ── source-file directive ──────────────────────────────────────────────────

  ;; source-file applies a config file from disk, returning T.
  (it "source-file-directive-loads-temp-file"
    (with-isolated-config
      (with-temp-config-file (p "bind z next-window")
        (assert-config-directive-applied (list "source-file" (namestring p))
                                         "source-file temp file")
        (expect (eq :next-window (lookup-key-binding #\z))))))

  ;; The config loader accepts only the canonical source-file command name.
  (it "source-file-short-alias-is-rejected"
    (with-isolated-config
      (with-temp-config-file (p "set-option -g status-left SOURCEALIAS")
        (assert-config-directive-rejected (list "source" (namestring p))
                                          "source alias")
        (expect (not (string= "SOURCEALIAS" (cl-tmux/options:get-option "status-left")))))))

  ;; source-file on a nonexistent file returns NIL and logs tmux's diagnostic.
  (it "source-file-missing-returns-nil-and-logs"
    (with-isolated-config
      (let ((cl-tmux::*message-log* nil))
        (expect (null (apply-config-directive
                       '("source-file" "/nonexistent-cl-tmux-config-abc.conf"))))
        (expect (= 1 (length cl-tmux::*message-log*)))
        (expect (search "No such file or directory"
                        (cdr (first cl-tmux::*message-log*)))))))

  ;; source-file -n parses the file but executes NOTHING (tmux CMD_PARSE_PARSEONLY).
  ;; Asserts via an OPTION the file would set (a key like z has a DEFAULT binding, so
  ;; 'unbound' is not a reliable 'not executed' signal).
  (it "source-file-n-parse-only-does-not-execute"
    (with-isolated-config
      (with-temp-config-file (p "set-option -g status-left PARSEONLY")
        (assert-config-directive-applied (list "source-file" "-n" (namestring p))
                                         "source-file -n parse only")
        (expect (not (string= "PARSEONLY" (cl-tmux/options:get-option "status-left")))))))

  ;; Control: WITHOUT -n the same file DOES set the option — isolating that -n is what
  ;; suppresses execution.
  (it "source-file-without-n-executes-control"
    (with-isolated-config
      (with-temp-config-file (p "set-option -g status-left EXECUTED")
        (apply-config-directive (list "source-file" (namestring p)))
        (expect (string= "EXECUTED" (cl-tmux/options:get-option "status-left"))))))

  ;; Clustered -qn is also parse-only (q tolerated, n suppresses execution).
  (it "source-file-clustered-qn-does-not-execute"
    (with-isolated-config
      (with-temp-config-file (p "set-option -g status-left QNFLAG")
        (apply-config-directive (list "source-file" "-qn" (namestring p)))
        (expect (not (string= "QNFLAG" (cl-tmux/options:get-option "status-left")))))))

  ;; %parse-source-file-flags parses clustered -Fnqv and returns the path positionals.
  (it "parse-source-file-flags-clustered"
    (multiple-value-bind (n q v f rest)
        (cl-tmux/config::%parse-source-file-flags '("-Fnqv" "/path/to.conf"))
      (expect n :to-be-truthy)
      (expect q :to-be-truthy)
      (expect v :to-be-truthy)
      (expect f :to-be-truthy)
      (expect (equal '("/path/to.conf") rest))))

  ;; %parse-source-file-flags consumes tmux's -t target-pane without treating it as a path.
  (it "parse-source-file-flags-target-pane"
    (multiple-value-bind (n q v f rest)
        (cl-tmux/config::%parse-source-file-flags '("-q" "-t" "%1" "/path/to.conf"))
      (declare (ignore n v f))
      (expect q :to-be-truthy)
      (expect (equal '("/path/to.conf") rest))))

  ;; %consume-leading-flag-tokens walks leading flags and stops at the first positional token.
  (it "consume-leading-flag-tokens-stops-at-first-non-flag"
    (let ((seen '()))
      (expect (equal '("cmd" "arg")
                     (cl-tmux/config::%consume-leading-flag-tokens
                      '("-b" "-F" "cmd" "arg")
                      (lambda (tok rest)
                        (push tok seen)
                        (values rest t)))))
      (expect (equal '("-F" "-b") seen))))

  ;; source-file -F expands the path as a format string before loading.
  ;; Uses a known literal path (no #{...} variables) to confirm that -F does
  ;; not break plain paths (the expanded form equals the original string).
  (it "source-file-F-expands-format-path"
    (with-isolated-config
      (with-temp-config-file (p "set-option -g status-left FFORMAT")
        ;; A plain path contains no #{} variables; expand-format returns it unchanged.
        ;; This test confirms the -F code path does not corrupt literal paths.
        (let ((result (cl-tmux/config:apply-config-directive
                       (list "source-file" "-F" (namestring p)))))
          (expect result :to-be-truthy)
          (expect (string= "FFORMAT" (cl-tmux/options:get-option "status-left")))))))

  ;;; ── run-shell directive ───────────────────────────────────────────────────

  ;; run-shell returns T regardless of exit code.
  (it "run-shell-apply-directive-table"
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

  ;; %apply-run-shell-directive returns T for handled forms and NIL for non-run commands.
  ;; Each row is (expected cmd args description).
  (it "run-shell-handler-table"
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
        (declare (ignore desc))
        (with-isolated-config
          (let ((result (cl-tmux/config::%apply-run-shell-directive cmd args)))
            (if expected
                (expect (eq t result))
                (expect (null result))))))))

  ;; run-shell -c runs the shell command from the requested start-directory.
  (it "run-shell-c-start-directory-controls-shell-cwd"
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
               (expect (eq t handled)))
             (let ((expected-dir (string-right-trim
                                  '(#\/)
                                  (namestring (truename tmp-dir))))
                   (actual-dir (string-right-trim
                                '(#\Newline #\Return)
                                (uiop:read-file-string out-file))))
               (expect (string= actual-dir expected-dir))))
        (ignore-errors
          (uiop:delete-directory-tree tmp-dir :validate t :if-does-not-exist :ignore)))))

  ;; run-shell -C executes the tmux command and reports handled.
  (it "run-shell-C-table"
    (dolist (c '(("run-shell" "status-left"  "FOO" "run-shell -C")
                 ("run-shell" "status-right" "BAR" "run-shell -C")))
      (destructuring-bind (verb option value desc) c
        (declare (ignore desc))
        (with-isolated-config
          (let ((handled (cl-tmux/config::%apply-run-shell-directive
                          verb (list "-C" (format nil "set-option -g ~A ~A" option value)))))
            (expect (eq t handled))
            (expect (string= value (cl-tmux/options:get-option option))))))))

  ;;; ── %expand-leading-tilde ──────────────────────────────────────────────────

  ;; %expand-leading-tilde expands ~/... to $HOME/...; all other paths pass through.
  (it "expand-leading-tilde-table"
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
          (declare (ignore desc))
          (expect (string= expected (cl-tmux/config::%expand-leading-tilde input))))))))
