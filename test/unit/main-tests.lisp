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
  `(let ((orig-server     (fdefinition 'cl-tmux::run-server))
         (orig-client     (fdefinition 'cl-tmux::run-client))
         (orig-standalone (fdefinition 'cl-tmux::run-standalone))
         (orig-ensure     (fdefinition 'cl-tmux::%ensure-server-running))
         (*main-calls* nil))
     (unwind-protect
          (progn
            (setf (fdefinition 'cl-tmux::run-server)
                  (lambda (&rest a) (push (cons :server a) *main-calls*)))
            (setf (fdefinition 'cl-tmux::run-client)
                  (lambda (&rest a) (push (cons :client a) *main-calls*)))
            (setf (fdefinition 'cl-tmux::run-standalone)
                  (lambda (&rest a) (push (cons :standalone a) *main-calls*)))
            ;; Stub out the socket-probe / server-spawn so tests stay fast and
            ;; sandboxed.  run-client is still called; attach tests check that.
            (setf (fdefinition 'cl-tmux::%ensure-server-running)
                  (lambda (&rest _) (declare (ignore _)) nil))
            ,@body)
       (setf (fdefinition 'cl-tmux::run-server)              orig-server)
       (setf (fdefinition 'cl-tmux::run-client)              orig-client)
       (setf (fdefinition 'cl-tmux::run-standalone)          orig-standalone)
       (setf (fdefinition 'cl-tmux::%ensure-server-running)  orig-ensure))))

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

;;; ── %parse-attach-flags unit tests ──────────────────────────────────────────

(test parse-attach-flags-table
  "%parse-attach-flags: -t sets name, -d sets detach, -r sets read-only; defaults are \"0\"/nil/nil."
  (dolist (c (list '(("-t" "mysession" "-d" "-r") "mysession" t   t   "all flags")
                   '(()                            "0"         nil nil "no flags → defaults")
                   '(("-x" "-t" "abc")             "abc"       nil nil "unknown flags ignored")
                   '(("-t" "only-name")            "only-name" nil nil "-t alone")
                   '(("-d")                        "0"         t   nil "-d alone")
                   '(("-r")                        "0"         nil t   "-r alone")))
    (destructuring-bind (flags expected-name expected-detach expected-ro desc) c
      (multiple-value-bind (name detach read-only-p)
          (cl-tmux::%parse-attach-flags flags)
        (is (string= expected-name name)    "~A: name" desc)
        (is (eq expected-detach detach)     "~A: detach" desc)
        (is (eq expected-ro    read-only-p) "~A: read-only-p" desc)))))

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
    (let ((entry (cdr (assoc name cl-tmux::*startup-modes* :test #'equal))))
      (is (consp entry)
          "~A entry cdr must be a list" name)
      (is (symbolp (first entry))
          "~A handler (first of cdr) must be a symbol for stub-friendly dispatch" name))))

;;; ── %startup-mode-raw-args-p ────────────────────────────────────────────────

(test startup-mode-raw-args-p-attach-session-is-true
  "%startup-mode-raw-args-p returns T only for 'attach-session'."
  (is-true  (cl-tmux::%startup-mode-raw-args-p "attach-session")
            "attach-session must be a raw-args mode")
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
    (is (string= "myval" name)
        ":value flag must capture the next argument as the variable value")))

(test define-flag-parser-bool-flag-does-not-advance-past-flag
  "A :bool flag parser consumes only the flag token itself, not the next argument."
  ;; After -d, the next element is not a flag → it is an unknown flag, silently consumed.
  (multiple-value-bind (_n detach _r)
      (cl-tmux::%parse-attach-flags '("-d" "extra"))
    (is-true detach ":bool flag must set the variable to T")))

(test define-flag-parser-unknown-flag-at-end-does-not-error
  "An unknown flag appearing as the last argument is silently ignored."
  (finishes (cl-tmux::%parse-attach-flags '("-z"))
            "unknown last flag must not error"))

;;; ── *startup-modes* table structure tests ────────────────────────────────────

(test startup-modes-attach-session-has-raw-args-key
  "The 'attach-session' entry in *startup-modes* carries :raw-args-p T."
  (let ((entry (cdr (assoc "attach-session" cl-tmux::*startup-modes* :test #'equal))))
    (is-true (getf (rest entry) :raw-args-p)
             "attach-session entry must have :raw-args-p T")))

(test startup-modes-handler-table
  "Each *startup-modes* entry names the correct handler symbol."
  (dolist (c '(("server" cl-tmux::run-server        "server → run-server")
               ("attach" cl-tmux::run-attach-simple  "attach → run-attach-simple")))
    (destructuring-bind (mode expected-fn desc) c
      (let ((entry (cdr (assoc mode cl-tmux::*startup-modes* :test #'equal))))
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
  (let ((orig   (fdefinition 'cl-tmux::run-control-mode))
        (called nil))
    (unwind-protect
         (progn
           (setf (fdefinition 'cl-tmux::run-control-mode)
                 (lambda (&rest a) (declare (ignore a)) (setf called t)))
           (let ((sb-ext:*posix-argv* (list "cl-tmux" "-C")))
             (cl-tmux::main))
           (is-true called "main with -C must call run-control-mode"))
      (setf (fdefinition 'cl-tmux::run-control-mode) orig))))

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

(test run-kill-server-exits
  "run-kill-server captures exit code 0."
  (let (exit-code)
    (with-stubbed-exit exit-code
      (cl-tmux::run-kill-server nil))
    (is (eql 0 exit-code)
        "run-kill-server must exit with code 0")))

(test run-list-sessions-exits
  "run-list-sessions captures exit code 0."
  (let (exit-code)
    (with-stubbed-exit exit-code
      (cl-tmux::run-list-sessions nil))
    (is (eql 0 exit-code)
        "run-list-sessions must exit with code 0")))

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
  "run-kill-server, run-list-sessions, run-source-file, and run-has-session are all fbound."
  (dolist (sym '(cl-tmux::run-kill-server
                 cl-tmux::run-list-sessions
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

(test build-hostname-context-has-expected-keys
  "%build-hostname-context returns a plist with :hostname, :term, :version, etc."
  (let ((ctx (cl-tmux::%build-hostname-context)))
    (is (stringp (getf ctx :hostname))  ":hostname must be a string")
    (is (stringp (getf ctx :version))   ":version must be a string")
    (is (stringp (getf ctx :term))      ":term must be a string")
    (is (string= "3.5" (getf ctx :version))
        ":version must be \"3.5\" for compatibility")))

(test make-format-condition-evaluator
  "%make-format-condition-evaluator returns a callable closure that returns a string."
  (let ((evaluator (cl-tmux::%make-format-condition-evaluator)))
    (is (functionp evaluator)
        "%make-format-condition-evaluator must return a function")
    (is (stringp (funcall evaluator "1"))
        "format condition evaluator must return a string")))

;;; ── Coverage: server-launch timeout constant ─────────────────────────────────

(test server-launch-timeout-constant-is-positive
  "+server-launch-timeout-seconds+ is a positive integer."
  (is (plusp cl-tmux::+server-launch-timeout-seconds+)
      "+server-launch-timeout-seconds+ must be positive")
  (is (integerp cl-tmux::+server-launch-timeout-seconds+)
      "+server-launch-timeout-seconds+ must be an integer"))
