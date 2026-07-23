(in-package #:cl-tmux/test)

;;;; Dispatch set-environment command tests.

(describe "dispatch-suite"

  ;; :set-environment opens a prompt for NAME VALUE.
  (it "dispatch-set-environment-opens-prompt"
    (with-dispatch-prompt (s :set-environment :label "set-env NAME VALUE"
                                              :context ":set-environment must open a prompt")))

  ;; :set-environment with empty input does not crash.
  (it "dispatch-set-environment-empty-input-is-noop"
    (with-fake-session (s)
      (let ((*prompt* nil))
        (cl-tmux::dispatch-command s :set-environment nil)
        (expect (prompt-active-p))
        (finishes (funcall (prompt-on-submit *prompt*) "")
                  ":set-environment empty input must not error"))))

  ;; set-environment -u VAR unsets the variable (tmux's unset flag, previously only
  ;; -r was recognised).
  (it "cmd-set-environment-u-unsets-variable"
    (with-fake-session (s)
      (let ((name "CLTMUX_TEST_ENV_VAR_U"))
        (session-set-environment s name "hello")
        (cl-tmux::%cmd-set-environment-prompt s (list "-u" name))
        (multiple-value-bind (value source) (session-environment-value s name)
          (expect (null value))
          (expect (eq :unset source))))))

  ;; set-environment -g NAME VALUE stores the value in the process environment.
  (it "cmd-set-environment-g-sets-process-variable"
    (with-fake-session (s)
      (let ((name "CLTMUX_TEST_ENV_VAR_G")
            (value "global value"))
        (with-temporary-posix-environment-variable (name nil)
          (cl-tmux::%cmd-set-environment-prompt s (list "-g" name value))
          (expect (string= value (sb-ext:posix-getenv name)))
          (expect (null (gethash name (session-environment s))))))))

  ;; set-environment -F NAME VALUE expands VALUE as a format string before storing
  ;; it (tmux set-environment -F).
  (it "cmd-set-environment-f-expands-value-as-format"
    (with-fake-session (s)
      (let ((name "CLTMUX_TEST_ENV_VAR_F"))
        (cl-tmux::%cmd-set-environment-prompt s (list "-F" name "#{session_name}"))
        (multiple-value-bind (value source) (session-environment-value s name)
          (declare (ignore source))
          (expect (string= (cl-tmux::session-name s) value))
          (expect (not (search "#{" value)))))))

  ;; set-environment -t target NAME VALUE stores the value in the target session.
  (it "cmd-set-environment-t-target-session-writes-value"
    (with-fake-session (s)
      (let ((name "CLTMUX_TEST_ENV_VAR_T"))
        (with-session-and-window-names (s "alpha")
          (let ((target (make-fake-session :nwindows 1 :npanes 1)))
            (with-session-and-window-names (target "beta")
              (with-registered-sessions (("alpha" s) ("beta" target))
                (cl-tmux::%cmd-set-environment-prompt s (list "-t" "beta" name "value"))
                (multiple-value-bind (value source)
                    (session-environment-value target name)
                  (expect (string= "value" value))
                  (expect (eq :session source)))
                (multiple-value-bind (value source)
                    (session-environment-value s name)
                  (expect (null value))
                  (expect (null source))))))))))

  ;; set-environment -g -t target NAME VALUE is rejected when both scopes are given.
  (it "cmd-set-environment-g-and-t-are-mutually-exclusive"
    (with-fake-session (s)
      (let ((name "CLTMUX_TEST_ENV_VAR_GT"))
        (let ((cl-tmux::*overlay* nil))
          (cl-tmux::%cmd-set-environment-prompt s (list "-g" "-t" "ignored" name "value"))
          (expect (search "mutually exclusive" cl-tmux::*overlay*))))))

  ;; set-environment rejects unsupported flags before touching the process environment.
  (it "cmd-set-environment-unknown-flag-is-rejected-before-mutating"
    (with-fake-session (s)
      (let ((name "CLTMUX_TEST_ENV_VAR_UNKNOWN_FLAG"))
        ;; NAME starts absent (unique per test), so a faithful reject leaves it
        ;; untouched — neither value nor source — matching the non-mutation checks
        ;; used by the other set-environment tests in this file.
        (let ((*overlay* nil))
          (cl-tmux::%cmd-set-environment-prompt s (list "-Z" name "value"))
          (expect (search "unsupported argument" *overlay*)))
        (multiple-value-bind (value source) (session-environment-value s name)
          (expect (null value))
          (expect (null source)))))))
