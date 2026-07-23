(in-package #:cl-tmux/test)

;;;; Tests for argv dispatch routing in src/main.lisp (server/attach/standalone).

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

(defmacro define-flag-parser-error-cases (test-name parser-name &body cases)
  `(it ,(string-downcase (symbol-name test-name))
     (dolist (case ',cases)
       (destructuring-bind (args description) case
         (signals error
           (,parser-name args)
           "~A: unknown token must signal an error" description)))))

(describe "main-suite"

  ;; %application-argv drops SBCL saved-core wrapper options before dispatch.
  (it "application-argv-strips-sbcl-wrapper-options"
    (let ((sb-ext:*posix-argv*
            (list "sbcl" "--noinform" "--core" "/nix/store/core"
                  "--no-sysinit" "--no-userinit"
                  "list-commands" "-F" "#{command_list_name}")))
      (expect (equal '("list-commands" "-F" "#{command_list_name}")
                     (cl-tmux::%application-argv)))))

  ;; main routes argv to the correct entry point with the correct session name
  ;; (the first positional entry-function argument).  attach-session forwards
  ;; extra keyword args (e.g. :detach-others) after the name, so only the
  ;; leading session-name argument is compared here; the detach-flag case is
  ;; covered separately below.
  (it "dispatch-main-table"
    (dolist (c '((("server" "foo")                :server     "foo" "server with name")
                 (("attach" "foo")                :client     "foo" "attach with name")
                 (()                              :standalone nil   "no args → standalone")
                 (("server")                      :server     "0"   "server default name")
                 (("attach")                      :client     "0"   "attach default name")
                 (("bogus" "foo")                 :standalone nil   "unknown mode → standalone")
                 (("attach-session" "-t" "myname") :client     "myname" "attach-session with -t name")
                 (("attach-session")              :client     "0"   "attach-session default name")))
      (destructuring-bind (argv-tail expected-key expected-name desc) c
        (declare (ignore desc))
        (with-stubbed-entries
          (let ((sb-ext:*posix-argv* (cons "cl-tmux" argv-tail)))
            (cl-tmux::main))
          (expect (= 1 (length *main-calls*)))
          (expect (eq expected-key (car (first *main-calls*))))
          (expect (equal expected-name (first (cdr (first *main-calls*)))))))))

  ;; argv (cl-tmux attach-session -d) passes :detach-others T to run-client.
  (it "dispatch-attach-session-detach-flag"
    (with-stubbed-entries
      (let ((sb-ext:*posix-argv* (list "cl-tmux" "attach-session" "-d")))
        (cl-tmux::main))
      ;; run-client was called; check that :detach-others T was passed.
      (expect (= 1 (length *main-calls*)))
      (expect (eq :client (car (first *main-calls*))))
      (let ((call-args (cdr (first *main-calls*))))
        (expect (getf (rest call-args) :detach-others)))))

  ;; main routes argv correctly when the saved core is launched through SBCL options.
  (it "dispatch-main-from-sbcl-wrapper-argv"
    (let ((*main-calls* nil))
      (with-stubbed-fdefinition
          ((cl-tmux::run-list-commands
            (lambda (&rest a) (push (cons :list-commands a) *main-calls*))))
        (let ((sb-ext:*posix-argv*
                (list "sbcl" "--noinform" "--core" "/nix/store/core"
                      "--no-sysinit" "--no-userinit"
                      "list-commands" "-F" "#{command_list_name}")))
          (cl-tmux::main))
        (expect (= 1 (length *main-calls*)))
        (expect (eq :list-commands (car (first *main-calls*))))
        (expect (equal '(("-F" "#{command_list_name}"))
                       (cdr (first *main-calls*)))))))

  ;;; ── *startup-modes* data table ───────────────────────────────────────────────

  ;; *startup-modes* has handler entries for server, attach, and attach-session.
  (it "startup-modes-contains-server-attach-and-attach-session"
    (expect (assoc "server" cl-tmux::*startup-modes* :test #'equal))
    (expect (assoc "attach" cl-tmux::*startup-modes* :test #'equal))
    (expect (assoc "attach-session" cl-tmux::*startup-modes* :test #'equal))
    ;; Each entry's cdr is a list starting with the handler symbol.
    (dolist (name '("server" "attach" "attach-session"))
      (let ((entry (alist-value name cl-tmux::*startup-modes* :test #'equal)))
        (expect (consp entry))
        (expect (symbolp (first entry))))))

  ;;; ── %startup-mode-raw-args-p ────────────────────────────────────────────────

  ;; %startup-mode-raw-args-p returns T for modes that receive the full argv tail.
  (it "startup-mode-raw-args-p-known-raw-modes"
    (expect (cl-tmux::%startup-mode-raw-args-p "attach-session") :to-be-truthy)
    (expect (cl-tmux::%startup-mode-raw-args-p "list-commands") :to-be-truthy)
    (expect (cl-tmux::%startup-mode-raw-args-p "display-message") :to-be-truthy)
    (expect (cl-tmux::%startup-mode-raw-args-p "show-options") :to-be-truthy)
    (expect (cl-tmux::%startup-mode-raw-args-p "show-window-options") :to-be-truthy)
    (expect (cl-tmux::%startup-mode-raw-args-p "list-windows") :to-be-truthy)
    (expect (cl-tmux::%startup-mode-raw-args-p "server") :to-be-falsy)
    (expect (cl-tmux::%startup-mode-raw-args-p "attach") :to-be-falsy)
    (expect (cl-tmux::%startup-mode-raw-args-p "bogus") :to-be-falsy))

  ;;; ── define-flag-parser macro ────────────────────────────────────────────────

  ;; A parser generated by define-flag-parser is fbound and callable.
  (it "define-flag-parser-generates-callable-function"
    ;; %parse-attach-flags is generated by define-flag-parser; verify it is fbound.
    (expect (fboundp 'cl-tmux::%parse-attach-flags)))

  ;; %parse-attach-flags parses -d -t in either order.
  (it "parse-attach-flags-order-independent"
    (multiple-value-bind (name1 detach1 read-only-p1)
        (cl-tmux::%parse-attach-flags '("-d" "-t" "sess1"))
      (declare (ignore read-only-p1))
      (multiple-value-bind (name2 detach2 read-only-p2)
          (cl-tmux::%parse-attach-flags '("-t" "sess1" "-d"))
        (declare (ignore read-only-p2))
        (expect (string= name1 name2))
        (expect (eq detach1 detach2)))))

  ;;; ── define-flag-parser macro expansion tests ─────────────────────────────────

  ;; define-flag-parser expands to a DEFUN form.
  (it "define-flag-parser-generates-defun"
    (let* ((expansion (macroexpand-1
                       '(cl-tmux::define-flag-parser %test-parser
                            ((myvar nil))
                          (:bool "-x" myvar))))
           (text (prin1-to-string expansion)))
      (expect (search "DEFUN" text))))

  ;; A :value flag parser consumes two arguments (flag + value).
  (it "define-flag-parser-value-flag-advances-index"
    ;; %parse-attach-flags is generated with a :value spec for -t.
    ;; Passing a vector with -t and a value should consume both.
    (multiple-value-bind (name _d _r)
        (cl-tmux::%parse-attach-flags '("-t" "myval"))
      (declare (ignore _d _r))
      (expect (string= "myval" name))))

  ;; A :bool flag parser consumes only the flag token itself, not the next argument.
  (it "define-flag-parser-bool-flag-does-not-advance-past-flag"
    (multiple-value-bind (_n detach _r)
        (cl-tmux::%parse-attach-flags '("-d"))
      (declare (ignore _n _r))
      (expect detach :to-be-truthy)))

  (define-flag-parser-error-cases define-flag-parser-rejects-unknown-tokens
      cl-tmux::%parse-attach-flags
    (("-z") "unknown flag at end")
    (("-d" "extra") "extra token after bool flag"))

  ;;; ── *startup-modes* table structure tests ────────────────────────────────────

  ;; The 'attach-session' entry in *startup-modes* carries :raw-args-p T.
  (it "startup-modes-attach-session-has-raw-args-key"
    (let ((entry (alist-value "attach-session" cl-tmux::*startup-modes* :test #'equal)))
      (expect (getf (rest entry) :raw-args-p) :to-be-truthy)))

  ;; Each *startup-modes* entry names the correct handler symbol.
  (it "startup-modes-handler-table"
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
        (declare (ignore desc))
        (let ((entry (alist-value mode cl-tmux::*startup-modes* :test #'equal)))
          (expect (eq expected-fn (first entry)))))))

  ;;; ── server-socket-poll constants ─────────────────────────────────────────────

  ;; +server-socket-poll-interval-seconds+ and +server-socket-poll-max-iterations+
  ;; are positive numbers used to bound the server-start wait loop.
  (it "server-socket-poll-constants-are-positive"
    (expect (plusp cl-tmux::+server-socket-poll-interval-seconds+))
    (expect (plusp cl-tmux::+server-socket-poll-max-iterations+))
    (expect (integerp cl-tmux::+server-socket-poll-max-iterations+))))
