(in-package #:cl-tmux/test)

;;;; Dispatch set-environment command tests.

(in-suite dispatch-suite)

(test dispatch-set-environment-opens-prompt
  ":set-environment opens a prompt for NAME VALUE."
  (with-dispatch-prompt (s :set-environment :label "set-env NAME VALUE"
                                            :context ":set-environment must open a prompt")))

(test dispatch-set-environment-empty-input-is-noop
  ":set-environment with empty input does not crash."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :set-environment nil)
      (is (prompt-active-p) "prompt must be open")
      (finishes (funcall (prompt-on-submit *prompt*) "")
                ":set-environment empty input must not error"))))

(test cmd-set-environment-u-unsets-variable
  "set-environment -u VAR unsets the variable (tmux's unset flag, previously only
   -r was recognised)."
  (with-fake-session (s)
    (let ((name "CLTMUX_TEST_ENV_VAR_U"))
      (session-set-environment s name "hello")
      (cl-tmux::%cmd-set-environment-prompt s (list "-u" name))
      (multiple-value-bind (value source) (session-environment-value s name)
        (is (null value) "set-environment -u must clear the stored value")
        (is (eq :unset source) "set-environment -u must record an explicit unset")))))

(test cmd-set-environment-g-sets-process-variable
  "set-environment -g NAME VALUE stores the value in the process environment."
  (with-fake-session (s)
    (let ((name "CLTMUX_TEST_ENV_VAR_G")
          (value "global value"))
      (with-temporary-posix-environment-variable (name nil)
        (cl-tmux::%cmd-set-environment-prompt s (list "-g" name value))
        (is (string= value (sb-ext:posix-getenv name))
            "set-environment -g must update the process environment")
        (is (null (gethash name (session-environment s)))
            "set-environment -g must not write the session overlay")))))

(test cmd-set-environment-f-expands-value-as-format
  "set-environment -F NAME VALUE expands VALUE as a format string before storing
   it (tmux set-environment -F)."
  (with-fake-session (s)
    (let ((name "CLTMUX_TEST_ENV_VAR_F"))
      (cl-tmux::%cmd-set-environment-prompt s (list "-F" name "#{session_name}"))
      (multiple-value-bind (value source) (session-environment-value s name)
        (declare (ignore source))
        (is (string= (cl-tmux::session-name s) value)
            "set-environment -F must expand #{session_name} to the session name")
        (is (not (search "#{" value))
            "set-environment -F must not store the unexpanded template")))))

(test cmd-set-environment-t-target-session-writes-value
  "set-environment -t target NAME VALUE stores the value in the target session."
  (with-fake-session (s)
    (let ((name "CLTMUX_TEST_ENV_VAR_T"))
      (with-session-and-window-names (s "alpha")
        (let ((target (make-fake-session :nwindows 1 :npanes 1)))
          (with-session-and-window-names (target "beta")
            (with-registered-sessions (("alpha" s) ("beta" target))
              (cl-tmux::%cmd-set-environment-prompt s (list "-t" "beta" name "value"))
              (multiple-value-bind (value source)
                  (session-environment-value target name)
                (is (string= "value" value)
                    "set-environment -t must update the target session")
                (is (eq :session source)
                    "set-environment -t must record the session source"))
              (multiple-value-bind (value source)
                  (session-environment-value s name)
                (is (null value)
                    "set-environment -t must not touch the source session")
                (is (null source)
                    "set-environment -t must leave the source session unchanged")))))))))

(test cmd-set-environment-g-and-t-are-mutually-exclusive
  "set-environment -g -t target NAME VALUE is rejected when both scopes are given."
  (with-fake-session (s)
    (let ((name "CLTMUX_TEST_ENV_VAR_GT"))
      (let ((cl-tmux::*overlay* nil))
        (cl-tmux::%cmd-set-environment-prompt s (list "-g" "-t" "ignored" name "value"))
        (is (search "mutually exclusive" cl-tmux::*overlay*)
            "set-environment must reject -g together with -t")))))

(test cmd-set-environment-unknown-flag-is-rejected-before-mutating
  "set-environment rejects unsupported flags before touching the process environment."
  (with-fake-session (s)
    (let ((name "CLTMUX_TEST_ENV_VAR_UNKNOWN_FLAG"))
      ;; NAME starts absent (unique per test), so a faithful reject leaves it
      ;; untouched — neither value nor source — matching the non-mutation checks
      ;; used by the other set-environment tests in this file.
      (let ((*overlay* nil))
        (cl-tmux::%cmd-set-environment-prompt s (list "-Z" name "value"))
        (is (search "unsupported argument" *overlay*)
            "set-environment -Z must show an unsupported-argument error"))
      (multiple-value-bind (value source) (session-environment-value s name)
        (is (null value) "set-environment -Z must not set NAME")
        (is (null source) "set-environment -Z must not create NAME")))))
