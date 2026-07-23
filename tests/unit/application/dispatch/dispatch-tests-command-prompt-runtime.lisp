(in-package #:cl-tmux/test)

;;;; Dispatch command-prompt and command-line runtime tests.

(describe "dispatch-suite"

  ;;; ── command-prompt %N template substitution ─────────────────────────────────

  ;; %substitute-percent replaces tmux-style %1/%2 (single percent) with the args —
  ;; the classic `command-prompt -p name: "new-window -n '%1'"` idiom.
  (it "substitute-percent-replaces-single-percent-args"
    (expect (string= "new-window -n 'shell'"
                     (cl-tmux::%substitute-percent "new-window -n '%1'" '("shell"))))
    (expect (string= "swap a b"
                     (cl-tmux::%substitute-percent "swap %1 %2" '("a" "b")))))

  ;; %% is a literal percent; a missing arg expands to empty; %1 does not match
  ;; inside %10; a non-arg %x is left verbatim.
  (it "substitute-percent-handles-literal-and-edge-cases"
    (dolist (c '(("100%% done" ()          "100% done" "%% → literal %")
                 ("x%2"        ("only-one") "x"         "reference past arg list → empty")
                 ("%10"        ("v")        "v0"        "%1 must not match inside %10")
                 ("%z"         ("a")        "%z"        "non-digit %x is left verbatim")))
      (destructuring-bind (template args expected desc) c
        (declare (ignore desc))
        (expect (string= expected (cl-tmux::%substitute-percent template args))))))

  ;;; ── :command-prompt dispatch ─────────────────────────────────────────────────

  ;; :command-prompt opens a prompt with label ": ".
  (it "dispatch-command-prompt-opens-prompt"
    (with-fake-session (s)
      (let ((*prompt* nil))
        (cl-tmux::dispatch-command s :command-prompt nil)
        (expect (prompt-active-p))
        (expect (string= ": " (prompt-label *prompt*))))))

  ;; :command-prompt with empty input does not crash.
  (it "dispatch-command-prompt-empty-input-is-noop"
    (with-fake-session (s)
      (let ((*prompt* nil))
        (cl-tmux::dispatch-command s :command-prompt nil)
        (expect (prompt-active-p))
        (finishes (funcall (prompt-on-submit *prompt*) "")))))

  ;; :command-prompt with an unknown command name shows an error overlay.
  (it "dispatch-command-prompt-unknown-command-shows-overlay"
    (with-fake-session (s)
      (let ((*prompt* nil) (*overlay* nil))
        (cl-tmux::dispatch-command s :command-prompt nil)
        (funcall (prompt-on-submit *prompt*) "no-such-command-xyz")
        (assert-overlay-contains "unknown command" *overlay*
                                 "unknown command"))))

  ;; :command-prompt with 'list-windows' executes that command (opens overlay).
  (it "dispatch-command-prompt-known-command-executes"
    (with-fake-session (s :nwindows 1)
      (let ((*prompt* nil) (*overlay* nil))
        (cl-tmux::dispatch-command s :command-prompt nil)
        (funcall (prompt-on-submit *prompt*) "list-windows")
        (assert-overlay-active "list-windows via command-prompt must open an overlay"))))

  ;;; ── %run-command-line / display-message with arguments ───────────────────────

  ;; :command-prompt 'display-message #{session_name}' expands the format and shows
  ;; the result (not the literal #{...}).
  (it "command-prompt-display-message-expands-format"
    (with-fake-session (s)
      (let ((*prompt* nil) (*overlay* nil))
        (cl-tmux::dispatch-command s :command-prompt nil)
        (funcall (prompt-on-submit *prompt*) "display-message #{session_name}")
        (assert-overlay-contains "0" *overlay*
                                 "command-prompt display-message")
        (assert-overlay-not-contains "#{" *overlay*
                                     "command-prompt display-message"))))

  ;; %run-command-line with a bare command name dispatches it by name (no args).
  (it "run-command-line-no-arg-command-falls-through"
    (with-fake-session (s :nwindows 2)
      (let ((*overlay* nil))
        (cl-tmux::%run-command-line s "next-window")
        (expect (eq (second (session-windows s)) (session-active-window s))))))

  ;; display-message with multiple unquoted args joins them with spaces.
  (it "run-command-line-display-message-joins-args"
    (with-fake-session (s)
      (let ((*overlay* nil))
        (cl-tmux::%run-command-line s "display-message hello world")
        (assert-overlay-contains "hello world" *overlay*
                                 "display-message hello world"))))

  ;; display-message -l shows ARGS verbatim, WITHOUT expanding #{...} formats —
  ;; the inverse of the default expansion path.
  (it "display-message-l-flag-shows-literal-format"
    (with-fake-session (s)
      (let ((*overlay* nil))
        (cl-tmux::%run-command-line s "display-message -l #{session_name}")
        (assert-overlay-contains "#{session_name}" *overlay*
                                 "display-message -l")
        (assert-overlay-not-contains "0" *overlay*
                                     "display-message -l"))))

  ;; display-message rejects unsupported client/stdout/verbose flags.
  (it "display-message-rejects-unsupported-client-output-flags"
    (with-fake-session (s)
      (dolist (command '("display-message -c someclient #{session_name}"
                         "display-message -a #{session_name}"
                         "display-message -C #{session_name}"
                         "display-message -I #{session_name}"
                         "display-message -N #{session_name}"
                         "display-message -p #{session_name}"
                         "display-message -v #{session_name}"))
        (let ((*overlay* nil)
              (cl-tmux::*message-log* nil))
          (cl-tmux::%run-command-line s command)
          (assert-overlay-contains "display-message: unsupported argument"
                                   *overlay*
                                   command)
          (expect (null cl-tmux::*message-log*))))))

  ;; display-message -F fmt uses FMT as the template instead of the positional args.
  (it "display-message-F-uses-format-flag"
    (with-fake-session (s)
      (let ((*overlay* nil))
        (cl-tmux::%run-command-line s "display-message -F #{session_name}")
        (expect (and *overlay* (search (cl-tmux::session-name s) *overlay*))))))

  ;; %run-command-line with blank input does not signal an error.
  (it "run-command-line-empty-is-noop"
    (with-fake-session (s)
      (finishes (cl-tmux::%run-command-line s "   ")))))
