(in-package #:cl-tmux/test)

;;;; Dispatch command-prompt and command-line runtime tests.

(in-suite dispatch-suite)

;;; ── command-prompt %N template substitution ─────────────────────────────────

(test substitute-percent-replaces-single-percent-args
  "%substitute-percent replaces tmux-style %1/%2 (single percent) with the args —
   the classic `command-prompt -p name: \"new-window -n '%1'\"` idiom."
  (is (string= "new-window -n 'shell'"
               (cl-tmux::%substitute-percent "new-window -n '%1'" '("shell")))
      "%1 must be replaced by the first arg")
  (is (string= "swap a b"
               (cl-tmux::%substitute-percent "swap %1 %2" '("a" "b")))
      "%1 and %2 must be replaced positionally"))

(test substitute-percent-handles-literal-and-edge-cases
  "%% is a literal percent; a missing arg expands to empty; %1 does not match
   inside %10; a non-arg %x is left verbatim."
  (dolist (c '(("100%% done" ()          "100% done" "%% → literal %")
               ("x%2"        ("only-one") "x"         "reference past arg list → empty")
               ("%10"        ("v")        "v0"        "%1 must not match inside %10")
               ("%z"         ("a")        "%z"        "non-digit %x is left verbatim")))
    (destructuring-bind (template args expected desc) c
      (is (string= expected (cl-tmux::%substitute-percent template args)) "~A" desc))))

;;; ── :command-prompt dispatch ─────────────────────────────────────────────────

(test dispatch-command-prompt-opens-prompt
  ":command-prompt opens a prompt with label \": \"."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :command-prompt nil)
      (is (prompt-active-p) ":command-prompt must open a prompt")
      (is (string= ": " (prompt-label *prompt*))
          ":command-prompt prompt label must be \": \""))))

(test dispatch-command-prompt-empty-input-is-noop
  ":command-prompt with empty input does not crash."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :command-prompt nil)
      (is (prompt-active-p) "prompt must be open")
      (finishes (funcall (prompt-on-submit *prompt*) "")
                "empty input must not signal an error"))))

(test dispatch-command-prompt-unknown-command-shows-overlay
  ":command-prompt with an unknown command name shows an error overlay."
  (with-fake-session (s)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :command-prompt nil)
      (funcall (prompt-on-submit *prompt*) "no-such-command-xyz")
      (assert-overlay-contains "unknown command" *overlay*
                               "unknown command"))))

(test dispatch-command-prompt-known-command-executes
  ":command-prompt with 'list-windows' executes that command (opens overlay)."
  (with-fake-session (s :nwindows 1)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :command-prompt nil)
      (funcall (prompt-on-submit *prompt*) "list-windows")
      (assert-overlay-active "list-windows via command-prompt must open an overlay"))))

;;; ── %run-command-line / display-message with arguments ───────────────────────

(test command-prompt-display-message-expands-format
  ":command-prompt 'display-message #{session_name}' expands the format and shows
   the result (not the literal #{...})."
  (with-fake-session (s)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :command-prompt nil)
      (funcall (prompt-on-submit *prompt*) "display-message #{session_name}")
      (assert-overlay-contains "0" *overlay*
                               "command-prompt display-message")
      (assert-overlay-not-contains "#{" *overlay*
                                   "command-prompt display-message"))))

(test run-command-line-no-arg-command-falls-through
  "%run-command-line with a bare command name dispatches it by name (no args)."
  (with-fake-session (s :nwindows 2)
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line s "next-window")
      (is (eq (second (session-windows s)) (session-active-window s))
          "next-window via %run-command-line must switch to the second window"))))

(test run-command-line-display-message-joins-args
  "display-message with multiple unquoted args joins them with spaces."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line s "display-message hello world")
      (assert-overlay-contains "hello world" *overlay*
                               "display-message hello world"))))

(test display-message-l-flag-shows-literal-format
  "display-message -l shows ARGS verbatim, WITHOUT expanding #{...} formats —
   the inverse of the default expansion path."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line s "display-message -l #{session_name}")
      (assert-overlay-contains "#{session_name}" *overlay*
                               "display-message -l")
      (assert-overlay-not-contains "0" *overlay*
                                   "display-message -l"))))

(test display-message-rejects-compatibility-flags
  "display-message rejects the old client/stdout/verbose compatibility flags."
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
        (is (null cl-tmux::*message-log*)
            "~A must not add a message to the log" command)))))

(test display-message-F-uses-format-flag
  "display-message -F fmt uses FMT as the template instead of the positional args."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line s "display-message -F #{session_name}")
      (is (and *overlay* (search (cl-tmux::session-name s) *overlay*))
          "display-message -F must expand the -F format template"))))

(test run-command-line-empty-is-noop
  "%run-command-line with blank input does not signal an error."
  (with-fake-session (s)
    (finishes (cl-tmux::%run-command-line s "   ")
              "blank command line must be a safe no-op")))
