(in-package #:cl-tmux/test)

;;;; Tests for argv dispatch routing in src/main.lisp (server/attach/standalone).

(def-suite main-suite :description "argv dispatch routing (src/main.lisp)")
(in-suite main-suite)

(defvar *main-calls* nil
  "Records (TAG . ARGS) for each stubbed entry function call.")

(defmacro with-stubbed-entries (&body body)
  "Replace run-server / run-client / run-standalone with recorders that push
   onto *main-calls*, run BODY with a fresh *main-calls*, then restore."
  `(let ((orig-server (fdefinition 'cl-tmux::run-server))
         (orig-client (fdefinition 'cl-tmux::run-client))
         (orig-standalone (fdefinition 'cl-tmux::run-standalone))
         (*main-calls* nil))
     (unwind-protect
          (progn
            (setf (fdefinition 'cl-tmux::run-server)
                  (lambda (&rest a) (push (cons :server a) *main-calls*)))
            (setf (fdefinition 'cl-tmux::run-client)
                  (lambda (&rest a) (push (cons :client a) *main-calls*)))
            (setf (fdefinition 'cl-tmux::run-standalone)
                  (lambda (&rest a) (push (cons :standalone a) *main-calls*)))
            ,@body)
       (setf (fdefinition 'cl-tmux::run-server) orig-server)
       (setf (fdefinition 'cl-tmux::run-client) orig-client)
       (setf (fdefinition 'cl-tmux::run-standalone) orig-standalone))))

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

;;; ── *startup-modes* data table ───────────────────────────────────────────────

(test startup-modes-contains-server-and-attach
  "*startup-modes* is an alist with symbol handler entries for \"server\" and \"attach\".
   Storing symbols (not function objects) allows test stubs to override the handlers."
  (is (assoc "server" cl-tmux::*startup-modes* :test #'equal)
      "*startup-modes* must have a 'server' entry")
  (is (assoc "attach" cl-tmux::*startup-modes* :test #'equal)
      "*startup-modes* must have an 'attach' entry")
  (is (symbolp (cdr (assoc "server" cl-tmux::*startup-modes* :test #'equal)))
      "server handler must be a symbol (for stub-friendly dispatch)")
  (is (symbolp (cdr (assoc "attach" cl-tmux::*startup-modes* :test #'equal)))
      "attach handler must be a symbol (for stub-friendly dispatch)"))
