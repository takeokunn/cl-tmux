(in-package #:cl-tmux/test)

;;;; Tests for CLI entry point reachability and command forwarding.

;;; ── run-attach-simple and run-attach-with-flags reachability ─────────────────

(test run-attach-simple-is-fbound
  "run-attach-simple is defined as a function."
  (is (fboundp 'cl-tmux::run-attach-simple)
      "run-attach-simple must be fbound"))

(test run-attach-with-flags-is-fbound
  "run-attach-with-flags is defined as a function."
  (is (fboundp 'cl-tmux::run-attach-with-flags)
      "run-attach-with-flags must be fbound"))

(test dispatch-control-mode-flag
  "argv (cl-tmux -C) routes to run-control-mode."
  (let ((called nil))
    (with-stubbed-fdefinition
        ((cl-tmux::run-control-mode
          (lambda (&rest a) (declare (ignore a)) (setf called t))))
      (let ((sb-ext:*posix-argv* (list "cl-tmux" "-C")))
        (cl-tmux::main))
      (is-true called "main with -C must call run-control-mode"))))

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

(test run-commands-exit-when-no-server
  "Without a server, all client commands exit with code 1."
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
        (is (eql 1 exit-code)
            (format nil "~A without a server must exit with code 1" fn))))))

(test run-commands-forward-to-server
  "Commands forward their name and args to an existing server and exit cleanly."
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
        (is (equal (list server expected-fwd) forwarded)
            (format nil "~A must forward ~A to server" fn (first expected-fwd)))
        (is (eql 0 exit-code)
            (format nil "~A must exit 0 after forwarding" fn))))))

(test run-new-session-forwards-full-argv-when-server-exists
  "run-new-session preserves tmux flags by forwarding the full argv to the server."
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
      (is (equal '("0" ("new-session" "-s" "work" "-n" "shell" "-c" "/tmp" "-d"))
                 forwarded)
          "new-session must forward all original flags")
      (is (null attached)
          "-d must not attach after forwarding"))))

(test run-new-session-discovers-existing-server-before-session-name
  "run-new-session -s NAME creates a session in the existing server, not a new NAME socket."
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
      (is (equal '("alpha" ("new-session" "-d" "-s" "beta" "-n" "two"))
                 forwarded)
          "new-session must forward to the already-running server")
      (is (null ensured)
          "new-session must not start a second server named after -s")
      (is (equal '(nil) (reverse probes))
          "new-session should discover any running server before treating -s as a socket name"))))

(test run-source-file-nonexistent-path-exits-1-with-diagnostic
  "run-source-file with a nonexistent path exits 1 and writes tmux's diagnostic."
  (let (exit-code
        (output (make-string-output-stream)))
    (let ((*error-output* output)
          (cl-tmux::*message-log* nil))
      (with-stubbed-exit exit-code
        (cl-tmux::run-source-file (list "/nonexistent/no-such-file.conf"))))
    (is (eql 1 exit-code)
        "run-source-file with nonexistent path must fail")
    (let ((text (get-output-stream-string output)))
      (is (search "No such file or directory: /nonexistent/no-such-file.conf"
                  text)
          "run-source-file must write the missing-file diagnostic to stderr"))))

(test run-has-session-no-socket-exits-1
  "run-has-session with a nonexistent socket path exits with code 1."
  (let (exit-code)
    (with-stubbed-exit exit-code
      (cl-tmux::run-has-session (list "-t" "no-such-session-xyz")))
    (is (eql 1 exit-code)
        "run-has-session without socket must exit 1")))

(test run-has-session-stale-socket-exits-1
  "run-has-session with a socket FILE nothing listens on exits 1 — a stale
   socket left by a crashed server is not a live session."
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
           (is (eql 1 exit-code)
               "a stale socket file must not count as a live session"))
      (ignore-errors (delete-file cl-tmux::*socket-path-override*)))))

(test run-commands-are-fbound
  "CLI helper handlers are all fbound."
  (dolist (sym '(cl-tmux::run-kill-server
		 cl-tmux::run-list-sessions
		 cl-tmux::run-list-windows
		 cl-tmux::run-list-commands
		 cl-tmux::run-display-message
		 cl-tmux::run-show-options
		 cl-tmux::run-show-window-options
		 cl-tmux::run-source-file
		 cl-tmux::run-has-session))
    (is (fboundp sym) "~S must be fbound" sym)))
