(in-package #:cl-tmux/test)

;;;; Tests for argv dispatch routing in src/main.lisp (server/attach/standalone).

(def-suite main-suite :description "argv dispatch routing (src/main.lisp)")
(in-suite main-suite)

(defvar *main-calls* nil
  "Records (TAG . ARGS) for each stubbed entry function call.")

(defmacro with-stubbed-entries (&body body)
  "Replace run-server / run-client / run-standalone / %ensure-server-running
   with recorders that push onto *main-calls*, run BODY with a fresh
   *main-calls*, then restore.  %ensure-server-running is stubbed to a no-op so
   tests do not probe or spawn real sockets."
  `(let ((*main-calls* nil))
     (with-stubbed-fdefinition
         ((cl-tmux::run-server
           (lambda (&rest a) (push (cons :server a) *main-calls*)))
          (cl-tmux::run-client
           (lambda (&rest a) (push (cons :client a) *main-calls*)))
          (cl-tmux::run-standalone
           (lambda (&rest a) (push (cons :standalone a) *main-calls*)))
          ;; Stub out the socket-probe / server-spawn so tests stay fast and
          ;; sandboxed.  run-client is still called; attach tests check that.
          (cl-tmux::%ensure-server-running
           (lambda (&rest _) (declare (ignore _)) nil)))
       ,@body)))

(test application-argv-strips-sbcl-wrapper-options
  "%application-argv drops SBCL saved-core wrapper options before dispatch."
  (let ((sb-ext:*posix-argv*
          (list "sbcl" "--noinform" "--core" "/nix/store/core"
                "--no-sysinit" "--no-userinit"
                "list-commands" "-F" "#{command_list_name}")))
    (is (equal '("list-commands" "-F" "#{command_list_name}")
               (cl-tmux::%application-argv)))))

(test dispatch-main-table
  "main routes argv to the correct entry point with the correct session name."
  (dolist (c '((("server" "foo") :server    ("foo") "server with name")
               (("attach" "foo") :client    ("foo") "attach with name")
               (()               :standalone nil    "no args → standalone")
	               (("server")       :server    ("0")   "server default name")
	               (("attach")       :client    ("0")   "attach default name")
	               (("bogus" "foo")  :standalone nil    "unknown mode → standalone")))
    (destructuring-bind (argv-tail expected-key expected-args desc) c
      (with-stubbed-entries
        (let ((sb-ext:*posix-argv* (cons "cl-tmux" argv-tail)))
          (cl-tmux::main))
        (is (= 1 (length *main-calls*)) "~A: exactly one call" desc)
        (is (eq expected-key (car (first *main-calls*))) "~A: entry key" desc)
	(is (equal expected-args (cdr (first *main-calls*))) "~A: args" desc)))))

(test dispatch-main-from-sbcl-wrapper-argv
  "main routes argv correctly when the saved core is launched through SBCL options."
  (let ((*main-calls* nil))
    (with-stubbed-fdefinition
        ((cl-tmux::run-list-commands
          (lambda (&rest a) (push (cons :list-commands a) *main-calls*))))
      (let ((sb-ext:*posix-argv*
              (list "sbcl" "--noinform" "--core" "/nix/store/core"
                    "--no-sysinit" "--no-userinit"
                    "list-commands" "-F" "#{command_list_name}")))
        (cl-tmux::main))
      (is (= 1 (length *main-calls*)))
      (is (eq :list-commands (car (first *main-calls*))))
      (is (equal '(("-F" "#{command_list_name}"))
                 (cdr (first *main-calls*)))))))

;;; ── attach-session dispatch ──────────────────────────────────────────────────

(test dispatch-attach-session-routes-to-run-client
  "argv (cl-tmux attach-session -t myname) routes through *startup-modes* to
   run-attach-with-flags, which parses -t and calls run-client with the session name."
  (with-stubbed-entries
    (let ((sb-ext:*posix-argv* (list "cl-tmux" "attach-session" "-t" "myname")))
      (cl-tmux::main))
    (is (= 1 (length *main-calls*)))
    (is (eq :client (car (first *main-calls*))))
    (is (equal "myname" (first (cdr (first *main-calls*))))
        "run-client must be called with the -t session name")))

(test dispatch-attach-session-default-name
  "argv (cl-tmux attach-session) with no -t defaults to session name \"0\"."
  (with-stubbed-entries
    (let ((sb-ext:*posix-argv* (list "cl-tmux" "attach-session")))
      (cl-tmux::main))
    (is (= 1 (length *main-calls*)))
    (is (eq :client (car (first *main-calls*))))
    (is (equal "0" (first (cdr (first *main-calls*))))
        "default session name must be \"0\"")))

(test dispatch-attach-session-detach-flag
  "argv (cl-tmux attach-session -d) passes :detach-others T to run-client."
  (with-stubbed-entries
    (let ((sb-ext:*posix-argv* (list "cl-tmux" "attach-session" "-d")))
      (cl-tmux::main))
    ;; run-client was called; check that :detach-others T was passed.
    (is (= 1 (length *main-calls*)))
    (is (eq :client (car (first *main-calls*))))
    (let ((call-args (cdr (first *main-calls*))))
      (is (getf (rest call-args) :detach-others)
          ":detach-others T must be passed when -d flag is present"))))

;;; ── *startup-modes* data table ───────────────────────────────────────────────

(test startup-modes-contains-server-attach-and-attach-session
  "*startup-modes* has handler entries for server, attach, and attach-session."
  (is (assoc "server" cl-tmux::*startup-modes* :test #'equal)
      "*startup-modes* must have a 'server' entry")
  (is (assoc "attach" cl-tmux::*startup-modes* :test #'equal)
      "*startup-modes* must have an 'attach' entry")
  (is (assoc "attach-session" cl-tmux::*startup-modes* :test #'equal)
      "*startup-modes* must have an 'attach-session' entry")
  ;; Each entry's cdr is a list starting with the handler symbol.
  (dolist (name '("server" "attach" "attach-session"))
    (let ((entry (alist-value name cl-tmux::*startup-modes* :test #'equal)))
      (is (consp entry)
          "~A entry cdr must be a list" name)
      (is (symbolp (first entry))
          "~A handler (first of cdr) must be a symbol for stub-friendly dispatch" name))))

;;; ── %startup-mode-raw-args-p ────────────────────────────────────────────────

(test startup-mode-raw-args-p-known-raw-modes
  "%startup-mode-raw-args-p returns T for modes that receive the full argv tail."
  (is-true  (cl-tmux::%startup-mode-raw-args-p "attach-session")
	    "attach-session must be a raw-args mode")
  (is-true  (cl-tmux::%startup-mode-raw-args-p "list-commands")
	    "list-commands must be a raw-args mode")
  (is-true  (cl-tmux::%startup-mode-raw-args-p "display-message")
	    "display-message must be a raw-args mode")
  (is-true  (cl-tmux::%startup-mode-raw-args-p "show-options")
	    "show-options must be a raw-args mode")
  (is-true  (cl-tmux::%startup-mode-raw-args-p "show-window-options")
	    "show-window-options must be a raw-args mode")
  (is-true  (cl-tmux::%startup-mode-raw-args-p "list-windows")
	    "list-windows must be a raw-args mode")
  (is-false (cl-tmux::%startup-mode-raw-args-p "server")
	    "server must not be a raw-args mode")
  (is-false (cl-tmux::%startup-mode-raw-args-p "attach")
            "attach must not be a raw-args mode")
  (is-false (cl-tmux::%startup-mode-raw-args-p "bogus")
            "unknown modes must not be raw-args modes"))

;;; ── define-flag-parser macro ────────────────────────────────────────────────

(test define-flag-parser-generates-callable-function
  "A parser generated by define-flag-parser is fbound and callable."
  ;; %parse-attach-flags is generated by define-flag-parser; verify it is fbound.
  (is (fboundp 'cl-tmux::%parse-attach-flags)
      "%parse-attach-flags must be fbound"))


(test parse-attach-flags-order-independent
  "%parse-attach-flags parses -d -t in either order."
  (multiple-value-bind (name1 detach1 read-only-p1)
      (cl-tmux::%parse-attach-flags '("-d" "-t" "sess1"))
    (declare (ignore read-only-p1))
    (multiple-value-bind (name2 detach2 read-only-p2)
        (cl-tmux::%parse-attach-flags '("-t" "sess1" "-d"))
      (declare (ignore read-only-p2))
      (is (string= name1 name2) "name must match regardless of flag order")
      (is (eq detach1 detach2)  "detach must match regardless of flag order"))))

;;; ── define-flag-parser macro expansion tests ─────────────────────────────────

(test define-flag-parser-generates-defun
  "define-flag-parser expands to a DEFUN form."
  (let* ((expansion (macroexpand-1
                     '(cl-tmux::define-flag-parser %test-parser
                          ((myvar nil))
                        (:bool "-x" myvar))))
         (text (prin1-to-string expansion)))
    (is-true (search "DEFUN" text)
             "define-flag-parser must expand to a DEFUN")))

(test define-flag-parser-value-flag-advances-index
  "A :value flag parser consumes two arguments (flag + value)."
  ;; %parse-attach-flags is generated with a :value spec for -t.
  ;; Passing a vector with -t and a value should consume both.
  (multiple-value-bind (name _d _r)
      (cl-tmux::%parse-attach-flags '("-t" "myval"))
    (declare (ignore _d _r))
    (is (string= "myval" name)
        ":value flag must capture the next argument as the variable value")))

(test define-flag-parser-bool-flag-does-not-advance-past-flag
  "A :bool flag parser consumes only the flag token itself, not the next argument."
  ;; After -d, the next element is not a flag → it is an unknown flag, silently consumed.
  (multiple-value-bind (_n detach _r)
      (cl-tmux::%parse-attach-flags '("-d" "extra"))
    (declare (ignore _n _r))
    (is-true detach ":bool flag must set the variable to T")))

(test define-flag-parser-unknown-flag-at-end-does-not-error
  "An unknown flag appearing as the last argument is silently ignored."
  (finishes (cl-tmux::%parse-attach-flags '("-z"))
            "unknown last flag must not error"))

;;; ── *startup-modes* table structure tests ────────────────────────────────────

(test startup-modes-attach-session-has-raw-args-key
  "The 'attach-session' entry in *startup-modes* carries :raw-args-p T."
  (let ((entry (alist-value "attach-session" cl-tmux::*startup-modes* :test #'equal)))
    (is-true (getf (rest entry) :raw-args-p)
             "attach-session entry must have :raw-args-p T")))

(test startup-modes-handler-table
  "Each *startup-modes* entry names the correct handler symbol."
  (dolist (c '(("server" cl-tmux::run-server        "server → run-server")
               ("attach" cl-tmux::run-attach-simple  "attach → run-attach-simple")
               ("attach-session" cl-tmux::run-attach-with-flags
                "attach-session → run-attach-with-flags")
               ("list-commands" cl-tmux::run-list-commands
                "list-commands → run-list-commands")
               ("display-message" cl-tmux::run-display-message
                "display-message → run-display-message")
               ("show-options" cl-tmux::run-show-options
                "show-options → run-show-options")
               ("show-window-options" cl-tmux::run-show-window-options
                "show-window-options → run-show-window-options")
               ("has-session" cl-tmux::run-has-session
                "has-session → run-has-session")))
    (destructuring-bind (mode expected-fn desc) c
      (let ((entry (alist-value mode cl-tmux::*startup-modes* :test #'equal)))
        (is (eq expected-fn (first entry)) "~A" desc)))))

;;; ── server-socket-poll constants ─────────────────────────────────────────────

(test server-socket-poll-constants-are-positive
  "+server-socket-poll-interval-seconds+ and +server-socket-poll-max-iterations+
   are positive numbers used to bound the server-start wait loop."
  (is (plusp cl-tmux::+server-socket-poll-interval-seconds+)
      "+server-socket-poll-interval-seconds+ must be positive")
  (is (plusp cl-tmux::+server-socket-poll-max-iterations+)
      "+server-socket-poll-max-iterations+ must be positive")
  (is (integerp cl-tmux::+server-socket-poll-max-iterations+)
      "+server-socket-poll-max-iterations+ must be an integer"))

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

(test run-source-file-nonexistent-path-exits-cleanly
  "run-source-file with a nonexistent path exits cleanly (code 0)."
  (let (exit-code)
    (with-stubbed-exit exit-code
      (cl-tmux::run-source-file (list "/nonexistent/no-such-file.conf")))
    (is (eql 0 exit-code)
        "run-source-file with nonexistent path must exit cleanly")))

(test run-has-session-no-socket-exits-1
  "run-has-session with a nonexistent socket path exits with code 1."
  (let (exit-code)
    (with-stubbed-exit exit-code
      (cl-tmux::run-has-session (list "-t" "no-such-session-xyz")))
    (is (eql 1 exit-code)
        "run-has-session without socket must exit 1")))

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

;;; ── Coverage: hostname / environment helpers ─────────────────────────────────

(test hostname-short-table
  "%hostname-short strips the domain suffix, passes through when no dot, returns empty for empty."
  (dolist (c '(("myhost.example.com" "myhost" "FQDN → short hostname")
               ("solo"               "solo"   "no dot → full string unchanged")
               (""                   ""       "empty string → empty string")))
    (destructuring-bind (input expected desc) c
      (is (string= expected (cl-tmux::%hostname-short input))
          "~A: ~S → ~S" desc input expected))))

(test safe-getenv-returns-string
  "%safe-getenv returns a string for any variable name."
  (let ((result (cl-tmux::%safe-getenv "PATH")))
    (is (stringp result) "%safe-getenv must return a string"))
  (let ((result (cl-tmux::%safe-getenv "NONEXISTENT_VAR_XYZ_123")))
    (is (stringp result) "%safe-getenv must return a string for missing var")
    (is (string= "" result) "%safe-getenv must return empty string for missing var")))

(test mode-keys-from-editor-string-detects-vi-and-emacs
  "%mode-keys-from-editor-string mirrors tmux's basename/substring vi detection."
  (dolist (c '(("vi"               "vi")
               ("vim"              "vi")
               ("/usr/bin/vi"      "vi")
               ("/usr/local/bin/nvim" "vi")
               ("nano"             "emacs")
               ("/usr/bin/emacs"   "emacs")
               ("emacsclient -c"   "emacs")))
    (destructuring-bind (input expected) c
      (is (string= expected (cl-tmux::%mode-keys-from-editor-string input))
          "~S must map to ~S" input expected)))
  (is (null (cl-tmux::%mode-keys-from-editor-string nil))
      "NIL editor must yield NIL (registry default left untouched)")
  (is (null (cl-tmux::%mode-keys-from-editor-string ""))
      "empty editor must yield NIL (registry default left untouched)"))

(test build-hostname-context-has-expected-keys
  "%build-hostname-context returns a plist with :hostname, :term, :version, etc."
  (let ((ctx (cl-tmux::%build-hostname-context)))
    (is (stringp (getf ctx :hostname))  ":hostname must be a string")
    (is (stringp (getf ctx :version))   ":version must be a string")
    (is (stringp (getf ctx :term))      ":term must be a string")
    (is (string= (cl-tmux/version:version-string) (getf ctx :version))
        ":version must expose the cl-tmux runtime version")))

(test make-format-condition-evaluator
  "%make-format-condition-evaluator returns a callable closure that returns a string."
  (let ((evaluator (cl-tmux::%make-format-condition-evaluator)))
    (is (functionp evaluator)
        "%make-format-condition-evaluator must return a function")
    (is (stringp (funcall evaluator "1"))
        "format condition evaluator must return a string")))

;;; ── %enable-negotiated-terminal-features ──────────────────────────────────────

(test enable-negotiated-terminal-features-is-fbound
  "%enable-negotiated-terminal-features is defined as a function (extracted from
   run-standalone's raw-mode setup)."
  (is (fboundp 'cl-tmux::%enable-negotiated-terminal-features)
      "%enable-negotiated-terminal-features must be fbound"))

(test enable-negotiated-terminal-features-honors-option-gating
  "%enable-negotiated-terminal-features only emits mouse/focus escape sequences
   when the corresponding session option is on; with mouse, extended-keys, and
   focus-events all off, calling it produces no escape-sequence output."
  (with-isolated-options ("mouse" nil "extended-keys" "off" "focus-events" nil)
    (let ((output (with-output-to-string (*standard-output*)
                    (cl-tmux::%enable-negotiated-terminal-features))))
      (is (zerop (length output))
          "no escape sequences must be emitted when all three options are off"))))

(test enable-negotiated-terminal-features-emits-mouse-sequence-when-on
  "%enable-negotiated-terminal-features emits an escape sequence when the mouse
   option is on."
  (with-isolated-options ("mouse" t "extended-keys" "off" "focus-events" nil)
    (let ((output (with-output-to-string (*standard-output*)
                    (cl-tmux::%enable-negotiated-terminal-features))))
      (is (plusp (length output))
          "some escape-sequence output must be emitted when mouse is on")
      (is (find #\Escape output) "output must contain an ESC byte"))))

;;; ── %close-all-pane-ptys ──────────────────────────────────────────────────────

(test close-all-pane-ptys-closes-every-pane-fd
  "%close-all-pane-ptys closes the PTY fd of every pane in the session; a
   subsequent low-level close(2) on any pane's fd then fails with EBADF,
   proving the fd was already released."
  (unless (pty-available-p) (skip "no PTY available (sandboxed environment)"))
  (let ((session (create-initial-session 24 80)))
    (cl-tmux::%close-all-pane-ptys session)
    (dolist (pane (all-panes session))
      (signals sb-posix:syscall-error
        (sb-posix:close (pane-fd pane))))))

(test close-all-pane-ptys-ignores-already-closed-fds
  "%close-all-pane-ptys tolerates being called twice (already-closed fds do not
   signal an error)."
  (unless (pty-available-p) (skip "no PTY available (sandboxed environment)"))
  (let ((session (create-initial-session 24 80)))
    (cl-tmux::%close-all-pane-ptys session)
    (finishes (cl-tmux::%close-all-pane-ptys session)
              "a second call on already-closed fds must not error")))

;;; ── %cleanup-after-session ────────────────────────────────────────────────────

(test cleanup-after-session-closes-ptys-and-clears-status-timer
  "%cleanup-after-session joins the passed reader threads, clears *status-timer*,
   and closes every pane's PTY fd."
  (unless (pty-available-p) (skip "no PTY available (sandboxed environment)"))
  (with-isolated-options ()
    (let ((session (create-initial-session 24 80))
          (cl-tmux::*status-timer* nil)
          (cl-tmux::*running* t))
      (cl-tmux::%cleanup-after-session session nil)
      (is (null cl-tmux::*status-timer*)
          "*status-timer* must be NIL after cleanup")
      (finishes (cl-tmux::%close-all-pane-ptys session)
                "panes must already have closed fds (idempotent close)"))))

;;; ── %initialize-session-environment ───────────────────────────────────────────

(test initialize-session-environment-wires-condition-evaluator-and-loads-config
  "%initialize-session-environment wires *config-condition-evaluator*, applies
   editor-derived mode-keys, wires the terminal option callbacks, and calls
   load-config-file."
  (with-isolated-options ()
    (let ((load-called nil)
          (cl-tmux/config:*config-condition-evaluator*
            cl-tmux/config:*config-condition-evaluator*)
          (cl-tmux/terminal:*history-limit-function*
            cl-tmux/terminal:*history-limit-function*)
          (cl-tmux/terminal:*alternate-screen-enabled-function*
            cl-tmux/terminal:*alternate-screen-enabled-function*)
          (cl-tmux/terminal:*scroll-on-clear-function*
            cl-tmux/terminal:*scroll-on-clear-function*))
      (with-stubbed-fdefinition
          ((cl-tmux::load-config-file
            (lambda (&rest args) (declare (ignore args)) (setf load-called t) 0)))
        (cl-tmux::%initialize-session-environment)
        (is-true load-called
                 "%initialize-session-environment must call load-config-file")
        (is (functionp cl-tmux/config:*config-condition-evaluator*)
            "*config-condition-evaluator* must be wired to a function")
        (is (functionp cl-tmux/terminal:*history-limit-function*)
            "*history-limit-function* must be wired to a function")))))

;;; ── %apply-editor-mode-keys ────────────────────────────────────────────────────

(test apply-editor-mode-keys-sets-vi-from-visual-env
  "%apply-editor-mode-keys sets status-keys/mode-keys to \"vi\" when $VISUAL
   names a vi-like editor."
  (with-isolated-options ()
    (with-stubbed-fdefinition
        ((cl-tmux::%safe-getenv
          (lambda (name) (if (string= name "VISUAL") "/usr/bin/vim" ""))))
      (cl-tmux::%apply-editor-mode-keys)
      (is (string= "vi" (cl-tmux/options:get-option "status-keys"))
          "status-keys must be set to \"vi\"")
      (is (string= "vi" (cl-tmux/options:get-option "mode-keys"))
          "mode-keys must be set to \"vi\""))))

(test apply-editor-mode-keys-no-op-when-no-editor-env
  "%apply-editor-mode-keys leaves the registry defaults untouched when neither
   $VISUAL nor $EDITOR is set."
  (with-isolated-options ()
    (let ((before (cl-tmux/options:get-option "mode-keys")))
      (with-stubbed-fdefinition
          ((cl-tmux::%safe-getenv (lambda (name) (declare (ignore name)) "")))
        (cl-tmux::%apply-editor-mode-keys)
        (is (equal before (cl-tmux/options:get-option "mode-keys"))
            "mode-keys must be unchanged when no editor env var is set")))))

;;; ── %die-with-message ─────────────────────────────────────────────────────────

(test die-with-message-formats-and-exits-with-code-1
  "%die-with-message writes the formatted message to *error-output* and exits
   with code 1."
  (let (exit-code (output (make-string-output-stream)))
    (let ((*error-output* output))
      (with-stubbed-exit exit-code
        (cl-tmux::%die-with-message "boom: ~A~%" "reason")))
    (is (eql 1 exit-code) "%die-with-message must exit with code 1")
    (is (search "boom: reason" (get-output-stream-string output))
        "%die-with-message must format its message to *error-output*")))

;;; ── %wire-option-callbacks ────────────────────────────────────────────────────

(test wire-option-callbacks-sets-history-limit-function
  "%wire-option-callbacks wires cl-tmux/terminal:*history-limit-function*."
  (let ((old cl-tmux/terminal:*history-limit-function*))
    (unwind-protect
         (progn
           (cl-tmux::%wire-option-callbacks)
           (is (functionp cl-tmux/terminal:*history-limit-function*)
               "*history-limit-function* must be a function after %wire-option-callbacks"))
      (setf cl-tmux/terminal:*history-limit-function* old))))

(test wire-option-callbacks-sets-alternate-screen-function
  "%wire-option-callbacks wires cl-tmux/terminal:*alternate-screen-enabled-function*."
  (let ((old cl-tmux/terminal:*alternate-screen-enabled-function*))
    (unwind-protect
         (progn
           (cl-tmux::%wire-option-callbacks)
           (is (functionp cl-tmux/terminal:*alternate-screen-enabled-function*)
               "*alternate-screen-enabled-function* must be a function after %wire-option-callbacks"))
      (setf cl-tmux/terminal:*alternate-screen-enabled-function* old))))

(test wire-option-callbacks-sets-scroll-on-clear-function
  "%wire-option-callbacks wires cl-tmux/terminal:*scroll-on-clear-function*."
  (let ((old cl-tmux/terminal:*scroll-on-clear-function*))
    (unwind-protect
         (progn
           (cl-tmux::%wire-option-callbacks)
           (is (functionp cl-tmux/terminal:*scroll-on-clear-function*)
               "*scroll-on-clear-function* must be a function after %wire-option-callbacks"))
      (setf cl-tmux/terminal:*scroll-on-clear-function* old))))

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
