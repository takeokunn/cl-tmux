(in-package #:cl-tmux/test)

;;;; Tests for bootstrap command argument parsing helpers.

(describe "main-suite"

  ;;; ── %socket-file-session-name ─────────────────────────────────────────────────

  ;; %socket-file-session-name extracts the session name from a cl-tmux socket path.
  (it "socket-file-session-name-extracts-from-valid-path"
    (dolist (row '(("/tmp/cl-tmux-0.sock"    "0"    "default session name")
                   ("/tmp/cl-tmux-work.sock"  "work"  "named session")
                   ("/tmp/cl-tmux-my-s.sock"  "my-s"  "hyphenated name")))
      (destructuring-bind (path expected desc) row
        (declare (ignore desc))
        (expect (string= expected (cl-tmux::%socket-file-session-name path))))))

  ;; %socket-file-session-name returns NIL for paths without the cl-tmux- prefix.
  (it "socket-file-session-name-returns-nil-for-non-cl-tmux-path"
    (expect (null (cl-tmux::%socket-file-session-name "/tmp/other-program.sock")))
    (expect (null (cl-tmux::%socket-file-session-name nil))))

  ;; %socket-file-session-name returns NIL when the file has no name component.
  (it "socket-file-session-name-returns-nil-for-empty-name"
    ;; A path with no file name part (edge case)
    (expect (null (cl-tmux::%socket-file-session-name ""))))

  ;;; ── %list-commands-arguments ──────────────────────────────────────────────────

  ;; %list-commands-arguments extracts the -F format argument.
  (it "list-commands-arguments-parses-format-flag"
    (multiple-value-bind (fmt name)
        (cl-tmux::%list-commands-arguments '("-F" "#{command_list_name}"))
      (expect (string= "#{command_list_name}" fmt))
      (expect (null name))))

  ;; %list-commands-arguments captures a positional name argument.
  (it "list-commands-arguments-parses-positional-name"
    (multiple-value-bind (fmt name)
        (cl-tmux::%list-commands-arguments '("new-session"))
      (expect (null fmt))
      (expect (string= "new-session" name))))

  ;; %list-commands-arguments parses both -F and a positional name.
  (it "list-commands-arguments-parses-both-flags"
    (multiple-value-bind (fmt name)
        (cl-tmux::%list-commands-arguments '("-F" "#{name}" "kill-server"))
      (expect (string= "#{name}" fmt))
      (expect (string= "kill-server" name))))

  ;; %list-commands-arguments returns (values NIL NIL) for an empty argument list.
  (it "list-commands-arguments-returns-nil-nil-for-empty-list"
    (multiple-value-bind (fmt name)
        (cl-tmux::%list-commands-arguments '())
      (expect (null fmt))
      (expect (null name))))

  ;;; ── define-forwarding-commands macro ─────────────────────────────────────────

  ;; define-forwarding-commands expands to a PROGN containing DEFUN forms.
  (it "define-forwarding-commands-generates-defun"
    (let* ((expansion (macroexpand-1
                       '(cl-tmux::define-forwarding-commands
                          (run-test-cmd "test-cmd" "test docstring"))))
           (text (prin1-to-string expansion)))
      (expect (search "DEFUN" text) :to-be-truthy)
      (expect (search "PROGN" text) :to-be-truthy)))

  ;;; ── run-list-commands fbound check ───────────────────────────────────────────

  ;; run-list-commands is defined as a function.
  (it "run-list-commands-is-fbound"
    (expect (fboundp 'cl-tmux::run-list-commands)))

  ;;; ── parse-new-session-flags ───────────────────────────────────────────────────

  ;; %parse-new-session-flags extracts the -s session name.
  (it "parse-new-session-flags-parses-name-flag"
    (multiple-value-bind (name _win _detach _dir)
        (cl-tmux::%parse-new-session-flags '("-s" "mywork"))
      (declare (ignore _win _detach _dir))
      (expect (string= "mywork" name))))

  ;; %parse-new-session-flags sets detach=T for the -d flag.
  (it "parse-new-session-flags-parses-detach-flag"
    (multiple-value-bind (_name _win detach _dir)
        (cl-tmux::%parse-new-session-flags '("-d"))
      (declare (ignore _name _win _dir))
      (expect detach :to-be-truthy)))

  ;; %parse-new-session-flags extracts -c start-dir.
  (it "parse-new-session-flags-parses-start-dir"
    (multiple-value-bind (_name _win _detach dir)
        (cl-tmux::%parse-new-session-flags '("-c" "/tmp/work"))
      (declare (ignore _name _win _detach))
      (expect (string= "/tmp/work" dir))))

  ;; %parse-new-session-flags returns all-NIL defaults for an empty list.
  (it "parse-new-session-flags-returns-nils-for-empty-args"
    (multiple-value-bind (name win detach dir)
        (cl-tmux::%parse-new-session-flags '())
      (expect (null name))
      (expect (null win))
      (expect (null detach))
      (expect (null dir)))))
