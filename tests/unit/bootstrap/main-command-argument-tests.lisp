(in-package #:cl-tmux/test)

(in-suite main-suite)

;;;; Tests for bootstrap command argument parsing helpers.

;;; ── %socket-file-session-name ─────────────────────────────────────────────────

(test socket-file-session-name-extracts-from-valid-path
  "%socket-file-session-name extracts the session name from a cl-tmux socket path."
  (dolist (row '(("/tmp/cl-tmux-0.sock"    "0"    "default session name")
                 ("/tmp/cl-tmux-work.sock"  "work"  "named session")
                 ("/tmp/cl-tmux-my-s.sock"  "my-s"  "hyphenated name")))
    (destructuring-bind (path expected desc) row
      (is (string= expected (cl-tmux::%socket-file-session-name path))
          "~A: ~S → ~S" desc path expected))))

(test socket-file-session-name-returns-nil-for-non-cl-tmux-path
  "%socket-file-session-name returns NIL for paths without the cl-tmux- prefix."
  (is (null (cl-tmux::%socket-file-session-name "/tmp/other-program.sock"))
      "non-cl-tmux socket must yield NIL")
  (is (null (cl-tmux::%socket-file-session-name nil))
      "NIL input must yield NIL"))

(test socket-file-session-name-returns-nil-for-empty-name
  "%socket-file-session-name returns NIL when the file has no name component."
  ;; A path with no file name part (edge case)
  (is (null (cl-tmux::%socket-file-session-name ""))
      "empty path string must yield NIL"))

;;; ── %list-commands-arguments ──────────────────────────────────────────────────

(test list-commands-arguments-parses-format-flag
  "%list-commands-arguments extracts the -F format argument."
  (multiple-value-bind (fmt name)
      (cl-tmux::%list-commands-arguments '("-F" "#{command_list_name}"))
    (is (string= "#{command_list_name}" fmt)
        "-F argument must be captured as format")
    (is (null name) "no positional name must be NIL")))

(test list-commands-arguments-parses-positional-name
  "%list-commands-arguments captures a positional name argument."
  (multiple-value-bind (fmt name)
      (cl-tmux::%list-commands-arguments '("new-session"))
    (is (null fmt)  "no -F flag must leave format NIL")
    (is (string= "new-session" name)
        "positional arg must be captured as name")))

(test list-commands-arguments-parses-both-flags
  "%list-commands-arguments parses both -F and a positional name."
  (multiple-value-bind (fmt name)
      (cl-tmux::%list-commands-arguments '("-F" "#{name}" "kill-server"))
    (is (string= "#{name}" fmt)   "format must be captured")
    (is (string= "kill-server" name) "positional name must be captured")))

(test list-commands-arguments-returns-nil-nil-for-empty-list
  "%list-commands-arguments returns (values NIL NIL) for an empty argument list."
  (multiple-value-bind (fmt name)
      (cl-tmux::%list-commands-arguments '())
    (is (null fmt)  "empty args: format must be NIL")
    (is (null name) "empty args: name must be NIL")))

;;; ── define-forwarding-commands macro ─────────────────────────────────────────

(test define-forwarding-commands-generates-defun
  "define-forwarding-commands expands to a PROGN containing DEFUN forms."
  (let* ((expansion (macroexpand-1
                     '(cl-tmux::define-forwarding-commands
                        (run-test-cmd "test-cmd" "test docstring"))))
         (text (prin1-to-string expansion)))
    (is-true (search "DEFUN" text)
             "define-forwarding-commands must expand to DEFUN forms")
    (is-true (search "PROGN" text)
             "define-forwarding-commands must wrap in PROGN")))

;;; ── run-list-commands fbound check ───────────────────────────────────────────

(test run-list-commands-is-fbound
  "run-list-commands is defined as a function."
  (is (fboundp 'cl-tmux::run-list-commands)
      "run-list-commands must be fbound"))

;;; ── parse-new-session-flags ───────────────────────────────────────────────────

(test parse-new-session-flags-parses-name-flag
  "%parse-new-session-flags extracts the -s session name."
  (multiple-value-bind (name _win _detach _dir)
      (cl-tmux::%parse-new-session-flags '("-s" "mywork"))
    (declare (ignore _win _detach _dir))
    (is (string= "mywork" name)
        "-s flag must set the session name")))

(test parse-new-session-flags-parses-detach-flag
  "%parse-new-session-flags sets detach=T for the -d flag."
  (multiple-value-bind (_name _win detach _dir)
      (cl-tmux::%parse-new-session-flags '("-d"))
    (declare (ignore _name _win _dir))
    (is-true detach "-d flag must set detach to T")))

(test parse-new-session-flags-parses-start-dir
  "%parse-new-session-flags extracts -c start-dir."
  (multiple-value-bind (_name _win _detach dir)
      (cl-tmux::%parse-new-session-flags '("-c" "/tmp/work"))
    (declare (ignore _name _win _detach))
    (is (string= "/tmp/work" dir)
        "-c flag must set the start directory")))

(test parse-new-session-flags-returns-nils-for-empty-args
  "%parse-new-session-flags returns all-NIL defaults for an empty list."
  (multiple-value-bind (name win detach dir)
      (cl-tmux::%parse-new-session-flags '())
    (is (null name)   "empty args: name must be NIL")
    (is (null win)    "empty args: win-name must be NIL")
    (is (null detach) "empty args: detach must be NIL")
    (is (null dir)    "empty args: start-dir must be NIL")))

;;; ── Global socket flags (-L / -S) ─────────────────────────────────────────────

(test consume-global-socket-flags-parses-L-and-S
  "tmux's global -L/-S flags (separated and attached getopt forms) are consumed
   from the front of argv before the command word and set the socket overrides."
  (let ((cl-tmux::*socket-name-override* nil)
        (cl-tmux::*socket-path-override* nil))
    (is (equal '("new-session" "-s" "x")
               (cl-tmux::%consume-global-socket-flags
                '("-L" "lbl" "-S" "/tmp/p.sock" "new-session" "-s" "x")))
        "both flags must be consumed, leaving the command tail")
    (is (string= "lbl" cl-tmux::*socket-name-override*)
        "-L value must land in *socket-name-override*")
    (is (string= "/tmp/p.sock" cl-tmux::*socket-path-override*)
        "-S value must land in *socket-path-override*"))
  (let ((cl-tmux::*socket-name-override* nil)
        (cl-tmux::*socket-path-override* nil))
    (is (equal '("attach")
               (cl-tmux::%consume-global-socket-flags '("-Lfoo" "attach")))
        "attached -Lfoo form must be consumed")
    (is (string= "foo" cl-tmux::*socket-name-override*)
        "attached -Lfoo must set the socket name"))
  (let ((cl-tmux::*socket-name-override* nil)
        (cl-tmux::*socket-path-override* nil))
    (is (equal '("kill-server")
               (cl-tmux::%consume-global-socket-flags '("kill-server")))
        "argv without global flags must pass through unchanged")
    (is (null cl-tmux::*socket-name-override*)
        "no -L means no socket-name override")))
