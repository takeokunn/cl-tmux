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
  "main routes argv to the correct entry point with the correct session name
   (the first positional entry-function argument).  attach-session forwards
   extra keyword args (e.g. :detach-others) after the name, so only the
   leading session-name argument is compared here; the detach-flag case is
   covered separately below."
  (dolist (c '((("server" "foo")                :server     "foo" "server with name")
               (("attach" "foo")                :client     "foo" "attach with name")
               (()                              :standalone nil   "no args → standalone")
               (("server")                      :server     "0"   "server default name")
               (("attach")                      :client     "0"   "attach default name")
               (("bogus" "foo")                 :standalone nil   "unknown mode → standalone")
               (("attach-session" "-t" "myname") :client     "myname" "attach-session with -t name")
               (("attach-session")              :client     "0"   "attach-session default name")))
    (destructuring-bind (argv-tail expected-key expected-name desc) c
      (with-stubbed-entries
        (let ((sb-ext:*posix-argv* (cons "cl-tmux" argv-tail)))
          (cl-tmux::main))
        (is (= 1 (length *main-calls*)) "~A: exactly one call" desc)
        (is (eq expected-key (car (first *main-calls*))) "~A: entry key" desc)
        (is (equal expected-name (first (cdr (first *main-calls*)))) "~A: name arg" desc)))))

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
  (multiple-value-bind (_n detach _r)
      (cl-tmux::%parse-attach-flags '("-d"))
    (declare (ignore _n _r))
    (is-true detach ":bool flag must set the variable to T")))

(defmacro define-flag-parser-error-cases (test-name parser-name &body cases)
  `(test ,test-name
     "Generated flag parsers reject unknown tokens instead of accepting fallback paths."
     (dolist (case ',cases)
       (destructuring-bind (args description) case
         (signals error
           (,parser-name args)
           "~A: unknown token must signal an error" description)))))

(define-flag-parser-error-cases define-flag-parser-rejects-unknown-tokens
    cl-tmux::%parse-attach-flags
  (("-z") "unknown flag at end")
  (("-d" "extra") "extra token after bool flag"))

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
