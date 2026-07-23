(in-package #:cl-tmux/test)

;;;; Dispatch show-environment and overlay tests.

(describe "dispatch-suite"

  ;; show-environment with a single NAME argument displays or marks the value correctly.
  ;; Each row: (flags set-value expected-fmt message).
  (it "cmd-show-environment-single-name-forms"
    (dolist (row '((""   "visible"  "~A=visible"               "show-environment NAME")
                   ("-s" "visible"  "~A='visible'; export ~A"  "show-environment -s")
                   (""   nil        "-~A"                       "show-environment missing")))
      (destructuring-bind (flags set-value expected-fmt msg) row
        (with-fake-session (s)
          (let ((name "CLTMUX_TEST_SHOWENV_UNIFIED"))
            (if set-value
                (session-set-environment s name set-value)
                (session-unset-environment s name))
            (with-run-command-line-overlay
                (s (if (string= flags "")
                       (format nil "show-environment ~A" name)
                       (format nil "show-environment ~A ~A" flags name)))
              (assert-overlay-contains (format nil expected-fmt name name)
                                       *overlay*
                                       msg)))))))

  ;; show-environment without NAME displays the environment list.
  (it "cmd-show-environment-listing-shows-header-and-entries"
    (with-fake-session (s)
      (let ((name-a "CLTMUX_TEST_SHOWENV_LIST_A")
            (name-b "CLTMUX_TEST_SHOWENV_LIST_B"))
        (session-set-environment s name-a "one")
        (session-set-environment s name-b "two")
        (with-run-command-line-overlay (s "show-environment")
          (assert-overlay-contains-all
              (list "environment"
                    (format nil "  ~A=one" name-a)
                    (format nil "  ~A=two" name-b))
              *overlay*
              "show-environment")))))

  ;; show-environment -g NAME displays the process environment value.
  (it "cmd-show-environment-g-shows-process-value"
    (with-fake-session (s)
      (let ((name "CLTMUX_TEST_SHOWENV_G")
            (value "visible"))
        (with-temporary-posix-environment-variable (name value)
          (with-run-command-line-overlay (s (format nil "show-environment -g ~A" name))
            (assert-overlay-contains (format nil "~A=~A" name value)
                                     *overlay*
                                     "show-environment -g"))))))

  ;; show-environment -t target NAME displays the target session value.
  (it "cmd-show-environment-t-target-session-displays-value"
    (with-fake-session (s)
      (let ((name "CLTMUX_TEST_SHOWENV_T"))
        (let ((target (make-fake-session :nwindows 1 :npanes 1)))
          (with-session-and-window-names (s "alpha")
            (with-session-and-window-names (target "beta")
              (with-registered-sessions (("alpha" s) ("beta" target))
                (session-set-environment target name "visible")
                (with-run-command-line-overlay (s (format nil "show-environment -t beta ~A" name))
                  (assert-overlay-contains (format nil "~A=visible" name)
                                           *overlay*
                                           "show-environment -t")))))))))

  ;; show-environment -g -t target NAME is rejected when both scopes are given.
  (it "cmd-show-environment-g-and-t-are-mutually-exclusive"
    (with-fake-session (s)
      (let ((name "CLTMUX_TEST_SHOWENV_GT"))
        (let ((*overlay* nil))
          (cl-tmux::%run-command-line s (format nil "show-environment -g -t ignored ~A" name))
          (assert-overlay-contains "mutually exclusive"
                                   *overlay*
                                   "show-environment -g -t")))))

  ;; show-environment rejects unknown flags and extra NAME arguments before showing values.
  (it "cmd-show-environment-unsupported-arguments-are-rejected-before-reading"
    (with-fake-session (s)
      (let ((name "CLTMUX_TEST_SHOWENV_UNSUPPORTED"))
        (session-set-environment s name "hidden")
        (dolist (args (list (list "-Z" name)
                            (list name "extra")))
          (let ((*overlay* nil))
            (cl-tmux::%cmd-show-environment-arg s args)
            (assert-overlay-contains "unsupported argument"
                                     *overlay*
                                     "show-environment unsupported")
            (expect (null (search "hidden" *overlay*))))))))

  ;; :show-hooks opens an overlay describing registered command hooks.
  (it "dispatch-show-hooks-shows-overlay"
    (with-fake-session (s)
      (let ((*overlay* nil))
        (cl-tmux::dispatch-command s :show-hooks nil)
        (assert-overlay-active ":show-hooks must open an overlay"))))

  ;; :show-prompt-history with empty history opens an overlay saying '(no prompt history)'.
  (it "dispatch-show-prompt-history-empty-shows-overlay"
    (with-fake-session (s)
      (let ((*overlay* nil)
            (cl-tmux::*prompt-history* nil))
        (cl-tmux::dispatch-command s :show-prompt-history nil)
        (assert-overlay-active ":show-prompt-history must open an overlay")
        (assert-overlay-contains "no prompt history" *overlay*
                                 "overlay must say 'no prompt history' when empty"))))

  ;; :show-prompt-history with entries lists them.
  (it "dispatch-show-prompt-history-populated-shows-entries"
    (with-fake-session (s)
      (let ((*overlay* nil)
            (cl-tmux::*prompt-history* (list "list-windows" "next-window")))
        (cl-tmux::dispatch-command s :show-prompt-history nil)
        (assert-overlay-active ":show-prompt-history must open an overlay")
        (assert-overlay-contains "list-windows" *overlay*
                                 "overlay must contain 'list-windows'")
        (assert-overlay-contains "next-window" *overlay*
                                 "overlay must contain 'next-window'"))))

  ;; :show-server-options opens an overlay with server options.
  (it "dispatch-show-server-options-shows-overlay"
    (with-fake-session (s)
      (let ((*overlay* nil))
        (cl-tmux::dispatch-command s :show-server-options nil)
        (expect (and *overlay* (plusp (length *overlay*))))
        (expect (search "server options" *overlay*)))))

  ;; :suspend-client dispatches without signalling an error (sends SIGTSTP).
  (it "dispatch-suspend-client-does-not-error"
    (with-fake-session (s)
      ;; SIGTSTP is sent to the current process; we cannot easily test it was
      ;; actually delivered, but we verify dispatch does not signal a CL error.
      (finishes (cl-tmux::dispatch-command s :suspend-client nil)
                ":suspend-client must not signal a Lisp error"))))
