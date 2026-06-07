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

(test dispatch-server-with-name
  "argv (cl-tmux server foo) routes to run-server with name \"foo\"."
  (with-stubbed-entries
    (let ((sb-ext:*posix-argv* (list "cl-tmux" "server" "foo")))
      (cl-tmux::main))
    (is (= 1 (length *main-calls*)))
    (is (eq :server (car (first *main-calls*))))
    (is (equal (list "foo") (cdr (first *main-calls*))))))

(test dispatch-attach-with-name
  "argv (cl-tmux attach foo) routes to run-client with name \"foo\"."
  (with-stubbed-entries
    (let ((sb-ext:*posix-argv* (list "cl-tmux" "attach" "foo")))
      (cl-tmux::main))
    (is (= 1 (length *main-calls*)))
    (is (eq :client (car (first *main-calls*))))
    (is (equal (list "foo") (cdr (first *main-calls*))))))

(test dispatch-standalone-default
  "argv (cl-tmux) with no mode routes to run-standalone."
  (with-stubbed-entries
    (let ((sb-ext:*posix-argv* (list "cl-tmux")))
      (cl-tmux::main))
    (is (= 1 (length *main-calls*)))
    (is (eq :standalone (car (first *main-calls*))))
    (is (null (cdr (first *main-calls*))))))

(test dispatch-server-default-name
  "argv (cl-tmux server) with no name falls back to default name \"0\"."
  (with-stubbed-entries
    (let ((sb-ext:*posix-argv* (list "cl-tmux" "server")))
      (cl-tmux::main))
    (is (eq :server (car (first *main-calls*))))
    (is (equal (list "0") (cdr (first *main-calls*))))))

(test dispatch-attach-default-name
  "argv (cl-tmux attach) with no name falls back to default name \"0\"."
  (with-stubbed-entries
    (let ((sb-ext:*posix-argv* (list "cl-tmux" "attach")))
      (cl-tmux::main))
    (is (eq :client (car (first *main-calls*))))
    (is (equal (list "0") (cdr (first *main-calls*))))))

(test dispatch-unknown-mode-standalone
  "An unrecognized mode falls through to run-standalone."
  (with-stubbed-entries
    (let ((sb-ext:*posix-argv* (list "cl-tmux" "bogus" "foo")))
      (cl-tmux::main))
    (is (eq :standalone (car (first *main-calls*))))))

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

(test parse-attach-flags-all-flags
  "%parse-attach-flags correctly parses -d, -r, and -t <name> independently."
  (multiple-value-bind (name detach read-only-p)
      (cl-tmux::%parse-attach-flags '("-t" "mysession" "-d" "-r"))
    (is (string= "mysession" name))
    (is-true detach)
    (is-true read-only-p)))

(test parse-attach-flags-defaults
  "%parse-attach-flags returns default name \"0\" when no flags are given."
  (multiple-value-bind (name detach read-only-p)
      (cl-tmux::%parse-attach-flags '())
    (is (string= "0" name))
    (is-false detach)
    (is-false read-only-p)))

(test parse-attach-flags-unknown-flags-ignored
  "%parse-attach-flags silently ignores unrecognized flags."
  (multiple-value-bind (name detach read-only-p)
      (cl-tmux::%parse-attach-flags '("-x" "-t" "abc"))
    (is (string= "abc" name))
    (is-false detach)
    (is-false read-only-p)))

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

(test parse-attach-flags-value-flag-t-alone
  "%parse-attach-flags parses -t alone and returns default booleans."
  (multiple-value-bind (name detach read-only-p)
      (cl-tmux::%parse-attach-flags '("-t" "only-name"))
    (is (string= "only-name" name) "name must be 'only-name'")
    (is-false detach     "detach must default to NIL when -d absent")
    (is-false read-only-p "read-only-p must default to NIL when -r absent")))

(test parse-attach-flags-bool-d-alone
  "%parse-attach-flags parses -d alone, leaving name at default."
  (multiple-value-bind (name detach read-only-p)
      (cl-tmux::%parse-attach-flags '("-d"))
    (is (string= "0" name) "name must be default '0' when -t absent")
    (is-true  detach     "detach must be T when -d is present")
    (is-false read-only-p "read-only-p must remain NIL when -r absent")))

(test parse-attach-flags-bool-r-alone
  "%parse-attach-flags parses -r alone, leaving name and detach at defaults."
  (multiple-value-bind (name detach read-only-p)
      (cl-tmux::%parse-attach-flags '("-r"))
    (is (string= "0" name) "name must be default '0' when -t absent")
    (is-false detach "detach must remain NIL when -d absent")
    (is-true  read-only-p "read-only-p must be T when -r is present")))

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

(test startup-modes-server-handler-is-run-server
  "The 'server' entry in *startup-modes* names run-server as its handler."
  (let ((entry (cdr (assoc "server" cl-tmux::*startup-modes* :test #'equal))))
    (is (eq 'cl-tmux::run-server (first entry))
        "server entry handler must be run-server")))

(test startup-modes-attach-handler-is-run-attach-simple
  "The 'attach' entry in *startup-modes* names run-attach-simple as its handler."
  (let ((entry (cdr (assoc "attach" cl-tmux::*startup-modes* :test #'equal))))
    (is (eq 'cl-tmux::run-attach-simple (first entry))
        "attach entry handler must be run-attach-simple")))

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
