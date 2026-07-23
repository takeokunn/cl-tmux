(in-package #:cl-tmux/test)

;;;; Tests for CLI entry point reachability and command forwarding.

;;; ── Coverage: stub handler functions ─────────────────────────────────────────
;;;
;;; sb-ext:exit terminates the process rather than signalling a condition.
;;; We stub it to capture the exit code for testing.

(defmacro with-stubbed-exit (code-var &body body)
  "Stub sb-ext:exit so it captures the :code argument in the existing variable
   CODE-VAR and non-locally exits the body via THROW (matching sb-ext:exit's
   declared return type of NIL — a returning stub triggers SIMPLE-CONTROL-ERROR).
   Assertions should follow the macro form, where CODE-VAR holds the captured
   value.  Uses WITHOUT-PACKAGE-LOCKS because SB-EXT is a locked package."
  (let ((tag     (gensym "EXIT-TAG"))
        (orig    (gensym "ORIG-EXIT")))
    `(sb-ext:without-package-locks
       (let ((,orig (fdefinition 'sb-ext:exit)))
         (setf (fdefinition 'sb-ext:exit)
               (lambda (&rest args &key (code 0) &allow-other-keys)
                 (declare (ignore args))
                 (setf ,code-var code)
                 (throw ',tag nil)))
         (unwind-protect
              (catch ',tag ,@body)
           (setf (fdefinition 'sb-ext:exit) ,orig))))))

(defmacro with-stubbed-server (server-name (forwarded-var exit-code-var) &body body)
  "Stub %running-server-name → SERVER-NAME, run-command-client → captures FORWARDED-VAR.
   FORWARDED-VAR and EXIT-CODE-VAR are fresh bindings visible in BODY."
  `(let ((,forwarded-var nil)
         ,exit-code-var)
     (with-stubbed-fdefinition
         ((cl-tmux::%running-server-name
           (lambda (&optional preferred) (declare (ignore preferred)) ,server-name))
          (cl-tmux::run-command-client
           (lambda (name args) (setf ,forwarded-var (list name args)))))
       ,@body)))

(describe "main-suite"

  ;;; ── run-attach-simple and run-attach-with-flags reachability ─────────────────

  ;; run-attach-simple is defined as a function.
  (it "run-attach-simple-is-fbound"
    (expect (fboundp 'cl-tmux::run-attach-simple)))

  ;; run-attach-with-flags is defined as a function.
  (it "run-attach-with-flags-is-fbound"
    (expect (fboundp 'cl-tmux::run-attach-with-flags)))

  ;; argv (cl-tmux -C) routes to run-control-mode.
  (it "dispatch-control-mode-flag"
    (let ((called nil))
      (with-stubbed-fdefinition
          ((cl-tmux::run-control-mode
            (lambda (&rest a) (declare (ignore a)) (setf called t))))
        (let ((sb-ext:*posix-argv* (list "cl-tmux" "-C")))
          (cl-tmux::main))
        (expect called :to-be-truthy))))

  ;; An unrecognised argv[0] (not a known startup mode, not a -flag) forwards
  ;; to a live default-session server as a command client, rather than
  ;; starting a standalone session — %dispatch-unknown-mode's middle branch,
  ;; previously untested (only the dash-flag-usage-error and no-server
  ;; standalone-fallback shapes are implied by other tests).
  (it "dispatch-unknown-mode-forwards-to-live-server"
    (with-temp-socket-path (path)
      ;; %dispatch-unknown-mode only probes for the socket file's existence
      ;; (the actual connection is delegated to run-command-client, stubbed
      ;; below), so an empty file at PATH is enough to simulate "a server is
      ;; already running".
      (with-open-file (out path :direction :output :if-does-not-exist :create))
      (let (forwarded (standalone-called nil))
        (with-stubbed-fdefinition
            ((cl-tmux::socket-path (lambda (name) (declare (ignore name)) path))
             (cl-tmux::run-command-client
              (lambda (name args) (setf forwarded (list name args))))
             (cl-tmux::run-standalone
              (lambda () (setf standalone-called t))))
          ;; "rename-window" is not itself a *startup-modes* entry (unlike
          ;; list-sessions/kill-server/etc., which main dispatches directly);
          ;; it can only reach a live server as a forwarded command.
          (let ((sb-ext:*posix-argv* (list "cl-tmux" "rename-window" "new-name")))
            (cl-tmux::main))
          (expect (equal (list "0" (list "rename-window" "new-name")) forwarded))
          (expect (null standalone-called))))))

  ;; Without a server, all client commands exit with code 1.
  (it "run-commands-exit-when-no-server"
    (dolist (row '((cl-tmux::run-kill-server         nil)
                    (cl-tmux::run-list-sessions        nil)
                    (cl-tmux::run-list-windows         nil)
                    (cl-tmux::run-display-message      ("-p" "hello"))
                    (cl-tmux::run-show-options         ("-g"))
                    (cl-tmux::run-show-window-options  ("-g"))))
      (destructuring-bind (fn args) row
        (let (exit-code)
          (with-stubbed-exit exit-code
            (funcall fn args))
          (expect (eql 1 exit-code))))))

  ;; Commands forward their name and args to an existing server and exit cleanly.
  (it "run-commands-forward-to-server"
    (dolist (row '(("0"    cl-tmux::run-kill-server         ("-q")
                            ("kill-server" "-q"))
                    ("work" cl-tmux::run-list-sessions        ("-F" "#{session_name}")
                            ("list-sessions" "-F" "#{session_name}"))
                    ("work" cl-tmux::run-list-windows         ("-F" "#{window_name}")
                            ("list-windows" "-F" "#{window_name}"))
                    ("work" cl-tmux::run-display-message      ("-p" "hello")
                            ("display-message" "-p" "hello"))
                    ("work" cl-tmux::run-show-options         ("-g")
                            ("show-options" "-g"))
                    ("work" cl-tmux::run-show-window-options  ("-g")
                            ("show-window-options" "-g"))))
      (destructuring-bind (server fn args expected-fwd) row
        (with-stubbed-server server (forwarded exit-code)
          (with-stubbed-exit exit-code
            (funcall fn args))
          (expect (equal (list server expected-fwd) forwarded))
          (expect (eql 0 exit-code))))))

  ;; run-new-session preserves tmux flags by forwarding the full argv to the server.
  (it "run-new-session-forwards-full-argv-when-server-exists"
    (let ((forwarded nil)
          (attached nil))
      (with-stubbed-fdefinition
          ((cl-tmux::%running-server-name
            (lambda (&optional preferred) (declare (ignore preferred)) "0"))
           (cl-tmux::run-command-client
            (lambda (name args) (setf forwarded (list name args))))
           (cl-tmux::run-client
            (lambda (&rest args) (setf attached args))))
        (cl-tmux::run-new-session '("-s" "work" "-n" "shell" "-c" "/tmp" "-d"))
        (expect (equal '("0" ("new-session" "-s" "work" "-n" "shell" "-c" "/tmp" "-d"))
                       forwarded))
        (expect (null attached)))))

  ;; run-new-session -s NAME creates a session in the existing server, not a new NAME socket.
  (it "run-new-session-discovers-existing-server-before-session-name"
    (let ((forwarded nil)
          (ensured nil)
          (probes '()))
      (with-stubbed-fdefinition
          ((cl-tmux::%running-server-name
            (lambda (&optional preferred)
              (push preferred probes)
              (and (null preferred) "alpha")))
           (cl-tmux::run-command-client
            (lambda (name args) (setf forwarded (list name args))))
           (cl-tmux::%ensure-server-running
            (lambda (name) (setf ensured name))))
        (cl-tmux::run-new-session '("-d" "-s" "beta" "-n" "two"))
        (expect (equal '("alpha" ("new-session" "-d" "-s" "beta" "-n" "two"))
                       forwarded))
        (expect (null ensured))
        (expect (equal '(nil) (reverse probes))))))

  ;; run-source-file with a nonexistent path exits 1 and writes tmux's diagnostic.
  (it "run-source-file-nonexistent-path-exits-1-with-diagnostic"
    (let (exit-code
          (output (make-string-output-stream)))
      (let ((*error-output* output)
            (cl-tmux::*message-log* nil))
        (with-stubbed-exit exit-code
          (cl-tmux::run-source-file (list "/nonexistent/no-such-file.conf"))))
      (expect (eql 1 exit-code))
      (let ((text (get-output-stream-string output)))
        (expect (search "No such file or directory: /nonexistent/no-such-file.conf"
                        text)))))

  ;; run-has-session with a nonexistent socket path exits with code 1.
  (it "run-has-session-no-socket-exits-1"
    (let (exit-code)
      (with-stubbed-exit exit-code
        (cl-tmux::run-has-session (list "-t" "no-such-session-xyz")))
      (expect (eql 1 exit-code))))

  ;; run-has-session with a socket FILE nothing listens on exits 1 — a stale
  ;; socket left by a crashed server is not a live session.
  (it "run-has-session-stale-socket-exits-1"
    (let ((cl-tmux::*socket-path-override*
            (format nil "~A/cl-tmux-has-session-stale-~D.sock"
                    (string-right-trim "/" (or (sb-ext:posix-getenv "TMPDIR") "/tmp"))
                    (random 1000000)))
          exit-code)
      (unwind-protect
           (progn
             (with-open-file (s cl-tmux::*socket-path-override*
                                :direction :output :if-does-not-exist :create)
               (declare (ignore s)))
             (with-stubbed-exit exit-code
               (cl-tmux::run-has-session '("-t" "whatever")))
             (expect (eql 1 exit-code)))
        (ignore-errors (delete-file cl-tmux::*socket-path-override*)))))

  ;; CLI helper handlers are all fbound.
  (it "run-commands-are-fbound"
    (dolist (sym '(cl-tmux::run-kill-server
		 cl-tmux::run-list-sessions
		 cl-tmux::run-list-windows
		 cl-tmux::run-list-commands
		 cl-tmux::run-display-message
		 cl-tmux::run-show-options
		 cl-tmux::run-show-window-options
		 cl-tmux::run-source-file
		 cl-tmux::run-has-session))
      (expect (fboundp sym))))

  ;;; ── -V / --version / -h / --help / bad-flag usage ───────────────────────────

  ;; run-version prints "cl-tmux <version>" to stdout and exits 0.
  (it "run-version-prints-version-and-exits-zero"
    (let (exit-code output)
      (setf output
            (with-output-to-string (*standard-output*)
              (with-stubbed-exit exit-code
                (cl-tmux::run-version nil))))
      (expect (eql 0 exit-code))
      (expect (string= (format nil "cl-tmux ~A~%" (cl-tmux/version:version-string))
                       output))))

  ;; run-usage prints a usage summary to stdout and exits 0.
  (it "run-usage-prints-usage-and-exits-zero"
    (let (exit-code output)
      (setf output
            (with-output-to-string (*standard-output*)
              (with-stubbed-exit exit-code
                (cl-tmux::run-usage nil))))
      (expect (eql 0 exit-code))
      (expect (eql 0 (search "usage: cl-tmux" output)))))

  ;; argv -V/--version routes to run-version; -h/--help routes to run-usage.
  (it "dispatch-version-and-help-flags"
    (dolist (c '(("-V" :version) ("--version" :version)
                 ("-h" :usage)   ("--help" :usage)))
      (destructuring-bind (flag expected) c
        (let ((called nil))
          (with-stubbed-fdefinition
              ((cl-tmux::run-version
                (lambda (&rest a) (declare (ignore a)) (setf called :version)))
               (cl-tmux::run-usage
                (lambda (&rest a) (declare (ignore a)) (setf called :usage))))
            (let ((sb-ext:*posix-argv* (list "cl-tmux" flag)))
              (cl-tmux::main))
            (expect (eq expected called)))))))

  ;; An unknown dash-flag is a usage error (stderr + exit 1), not a silent
  ;; standalone start.
  (it "dispatch-unknown-dash-flag-prints-usage-and-exits-one"
    (let ((standalone-called nil)
          exit-code
          errout)
      (with-stubbed-fdefinition
          ((cl-tmux::run-standalone
            (lambda (&rest a) (declare (ignore a)) (setf standalone-called t))))
        (setf errout
              (with-output-to-string (*error-output*)
                (with-stubbed-exit exit-code
                  (let ((sb-ext:*posix-argv* (list "cl-tmux" "-Z")))
                    (cl-tmux::main))))))
      (expect (eql 1 exit-code))
      ;; cl-cli (main-startup-flags.lisp *cli-app*) now rejects -Z during global
      ;; flag parsing and prints its own diagnostic (e.g. "cl-tmux: Unknown
      ;; option...") before the usage block, so "usage: cl-tmux" no longer
      ;; starts at position 0 -- it just needs to be present.
      (expect (search "usage: cl-tmux" errout) :to-be-truthy)
      (expect standalone-called :to-be-falsy))))
