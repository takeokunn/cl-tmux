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
  (multiple-value-bind (name detach ro)
      (cl-tmux::%parse-attach-flags '("-t" "mysession" "-d" "-r"))
    (is (string= "mysession" name))
    (is-true detach)
    (is-true ro)))

(test parse-attach-flags-defaults
  "%parse-attach-flags returns default name \"0\" when no flags are given."
  (multiple-value-bind (name detach ro)
      (cl-tmux::%parse-attach-flags '())
    (is (string= "0" name))
    (is-false detach)
    (is-false ro)))

(test parse-attach-flags-unknown-flags-ignored
  "%parse-attach-flags silently ignores unrecognized flags."
  (multiple-value-bind (name detach ro)
      (cl-tmux::%parse-attach-flags '("-x" "-t" "abc"))
    (is (string= "abc" name))
    (is-false detach)
    (is-false ro)))

;;; ── *startup-modes* data table ───────────────────────────────────────────────

(test startup-modes-contains-server-attach-and-attach-session
  "*startup-modes* has symbol handler entries for server, attach, and attach-session."
  (is (assoc "server" cl-tmux::*startup-modes* :test #'equal)
      "*startup-modes* must have a 'server' entry")
  (is (assoc "attach" cl-tmux::*startup-modes* :test #'equal)
      "*startup-modes* must have an 'attach' entry")
  (is (assoc "attach-session" cl-tmux::*startup-modes* :test #'equal)
      "*startup-modes* must have an 'attach-session' entry")
  (dolist (name '("server" "attach" "attach-session"))
    (is (symbolp (cdr (assoc name cl-tmux::*startup-modes* :test #'equal)))
        "~A handler must be a symbol (for stub-friendly dispatch)" name)))
