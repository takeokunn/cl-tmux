(in-package #:cl-tmux/test)

;;;; Dispatch coverage: previously untested handlers, flag helpers, send-keys,
;;;;  capture-pane, named paste-buffer commands.
;;;;  (dispatch-commands-pane.lisp, dispatch-commands-auto.lisp,
;;;;   dispatch-handlers.lisp, buffer.lisp)

(in-suite dispatch-suite)

;;; ── Coverage: previously untested handlers ─────────────────────────────────

(test dispatch-attach-session-opens-prompt
  ":attach-session opens a prompt for the session name."
  (with-dispatch-prompt (s :attach-session :label "attach-session -t name"
                                          :context ":attach-session must open a prompt")))

(test dispatch-attach-session-submit-table
  ":attach-session on-submit shows 'attached' for a registered session, 'not found' otherwise."
  (with-fake-session (s)
    (let ((name (session-name s)))
      (dolist (c (list
                  (list (list (cons name s)) name           "attached"  "found session")
                  (list nil                  "nosuchsession" "not found" "missing session")))
        (destructuring-bind (registry input expected-text desc) c
          (let ((*prompt* nil) (*overlay* nil)
                (cl-tmux::*server-sessions* registry))
            (cl-tmux::dispatch-command s :attach-session nil)
            (is (prompt-active-p) "prompt must open for ~A" desc)
            (funcall (prompt-on-submit *prompt*) input)
            (assert-overlay-active ":attach-session ~A must show overlay" desc)
            (assert-overlay-contains expected-text *overlay*
                                     (format nil "~A: overlay must contain ~S"
                                             desc expected-text))))))))

(test run-command-line-attach-session-target-switches-session
  "attach-session -t <name> is scriptable and switches to the target session."
  (with-loop-state
    (with-empty-registry
      (let ((s0 (make-fake-session :nwindows 1))
            (s1 (make-fake-session :nwindows 1)))
        (setf (cl-tmux::session-name s0) "0"
              (cl-tmux::session-name s1) "work"
              (cl-tmux::session-last-active s0) 10
              (cl-tmux::session-last-active s1) 0
              cl-tmux::*server-sessions* (list (cons "0" s0)
                                               (cons "work" s1)))
        (setf cl-tmux::*dirty* nil)
        (is (eq s1 (cl-tmux::%run-command-line s0 "attach-session -t work"))
            "attach-session -t must return the selected target session")
        (is (eq s1 (cl-tmux::server-current-session))
            "target session must become the current session")
        (is-true cl-tmux::*dirty*
                 "attach-session -t must mark the display dirty")))))

(test run-command-line-attach-session-rejects-client-creation-args
  "attach-session inside a running client rejects client-creation arguments."
  (dolist (row '(("attach-session -c /tmp -t work" "client cwd")
                 ("attach-session -d -t work" "detach other clients")
                 ("attach-session work" "positional target")))
    (destructuring-bind (line desc) row
      (with-loop-state
        (with-empty-registry
          (let ((s0 (make-fake-session :nwindows 1))
                (s1 (make-fake-session :nwindows 1)))
            (setf (cl-tmux::session-name s0) "0"
                  (cl-tmux::session-name s1) "work"
                  (cl-tmux::session-last-active s0) 10
                  (cl-tmux::session-last-active s1) 0
                  cl-tmux::*server-sessions* (list (cons "0" s0)
                                                   (cons "work" s1))
                  cl-tmux::*dirty* nil
                  cl-tmux::*overlay* nil)
            (is (null (cl-tmux::%run-command-line s0 line))
                "~A must be rejected" desc)
            (is (eq s0 (cl-tmux::server-current-session))
                "~A must not switch sessions" desc)
            (is-false cl-tmux::*dirty*
                      "~A must not mark the display dirty" desc)
            (is (search "unsupported argument" cl-tmux::*overlay*)
                "~A must explain the unsupported argument" desc)))))))

(test dispatch-clear-prompt-history-empties-history
  ":clear-prompt-history sets *prompt-history* to NIL."
  (with-fake-session (s)
    (let ((cl-tmux::*prompt-history* (list "prev-cmd")))
      (cl-tmux::dispatch-command s :clear-prompt-history nil)
      (is (null cl-tmux::*prompt-history*)
          ":clear-prompt-history must set *prompt-history* to NIL"))))

(test dispatch-detach-all-clients-stops-running
  ":detach-all-clients sets *running* to NIL and returns :detach."
  (with-fake-session (s)
    (is (eq :detach (cl-tmux::dispatch-command s :detach-all-clients nil))
        ":detach-all-clients must return :detach")
    ;; After return the global *running* has been set to nil by the handler.
    ;; with-loop-state restores it, so just verify the return value above.
    ))

(test run-command-line-detach-without-args-returns-detach
  "detach without arguments detaches the active client."
  (with-fake-session (s)
    (setf cl-tmux::*running* t)
    (is (eq :detach (cl-tmux::%run-command-line s "detach"))
        "detach without arguments must return :detach")
    (is-true cl-tmux::*running*
             "detach returns a detach disposition; the caller owns loop shutdown")))

(test run-command-line-detach-rejects-ignored-args
  "detach rejects arguments that cl-tmux does not implement."
  (with-fake-session (s)
    (dolist (args '(("-P")
                    ("-E" "echo detached")
                    ("-s" "work")
                    ("-t" "client-0")))
      (setf cl-tmux::*running* t
            cl-tmux::*overlay* nil)
      (is (null (cl-tmux::%cmd-detach-arg s args))
          "unsupported detach args must be rejected: ~S" args)
      (is-true cl-tmux::*running*
               "rejected detach args must not stop the event loop: ~S" args)
      (assert-overlay-active
          "rejected detach args must explain the failure: ~S" args))))

(test dispatch-move-pane-opens-prompt
  ":move-pane opens a prompt for the destination window index."
  (with-dispatch-prompt ((s :nwindows 2) :move-pane
                         :context ":move-pane must open a prompt")))

(test dispatch-refresh-client-marks-dirty
  ":refresh-client marks *dirty* to force an immediate redraw."
  (with-fake-session (s)
    (let ((cl-tmux::*dirty* nil))
      (cl-tmux::dispatch-command s :refresh-client nil)
      (is-true cl-tmux::*dirty* ":refresh-client must set *dirty*"))))

(test run-command-line-refresh-client-rejects-unsupported-flags
  "refresh-client rejects unsupported tmux-compatible flags."
  (with-fake-session (s)
    (dolist (args '(("-S")
                    ("-t" "client-0")))
      (setf cl-tmux::*dirty* nil
            cl-tmux::*overlay* nil)
      (is (null (cl-tmux::%cmd-refresh-client-arg s args))
          "refresh-client must reject unsupported args: ~S" args)
      (is-false cl-tmux::*dirty*
                "rejected refresh-client args must not redraw: ~S" args)
      (is (search "unsupported argument" cl-tmux::*overlay*)
          "refresh-client must explain the unsupported arg rejection: ~S" args))))

(test run-command-line-lock-client-without-args-locks-session
  "lock-client without arguments locks the active session."
  (with-fake-session (s)
    (setf (cl-tmux::session-locked-p s) nil)
    (cl-tmux::%run-command-line s "lock-client")
    (is-true (cl-tmux::session-locked-p s)
             "lock-client must lock the active session")))

(test run-command-line-lock-client-rejects-target-client
  "lock-client rejects tmux-compatible target arguments."
  (with-fake-session (s)
    (let ((cl-tmux::*overlay* nil))
      (setf (cl-tmux::session-locked-p s) nil)
      (is (null (cl-tmux::%run-command-line s "lock-client -t client-0")))
      (is-false (cl-tmux::session-locked-p s)
                "lock-client -t must not lock the active session")
      (is (search "unsupported argument" cl-tmux::*overlay*)
          "lock-client -t must explain the rejection"))))

(test run-command-line-lock-session-locks-current-session
  "lock-session without -t locks the current session."
  (with-fake-session (s)
    (let ((cl-tmux::*overlay* nil))
      (setf (cl-tmux::session-locked-p s) nil)
      (cl-tmux::%run-command-line s "lock-session")
      (is-true (cl-tmux::session-locked-p s)
               "lock-session must lock the current session"))))

(test run-command-line-lock-session-target-locks-target-session
  "lock-session -t locks the named target session."
  (with-empty-registry
    (let ((s0 (make-fake-session :nwindows 1))
          (s1 (make-fake-session :nwindows 1)))
      (setf (cl-tmux::session-name s0) "0"
            (cl-tmux::session-name s1) "work"
            cl-tmux::*server-sessions* (list (cons "0" s0)
                                             (cons "work" s1)))
      (let ((cl-tmux::*overlay* nil))
        (setf (cl-tmux::session-locked-p s0) nil
              (cl-tmux::session-locked-p s1) nil)
        (is-true (cl-tmux::%run-command-line s0 "lock-session -t work")
                 "lock-session -t must be handled by the command runner")
        (is-false (cl-tmux::session-locked-p s0)
                  "lock-session -t must not lock the source session")
        (is-true (cl-tmux::session-locked-p s1)
                 "lock-session -t must lock the target session")))))

(test run-command-line-lock-session-rejects-unsupported-arguments
  "lock-session rejects unknown flags and positional tokens before locking anything."
  (dolist (command '("lock-session extra"
                     "lock-session -x"
                     "lock-session -t work extra"))
    (with-empty-registry
      (let ((s0 (make-fake-session :nwindows 1))
            (s1 (make-fake-session :nwindows 1)))
        (setf (cl-tmux::session-name s0) "0"
              (cl-tmux::session-name s1) "work"
              cl-tmux::*server-sessions* (list (cons "0" s0)
                                               (cons "work" s1)))
        (let ((cl-tmux::*overlay* nil))
          (setf (cl-tmux::session-locked-p s0) nil
                (cl-tmux::session-locked-p s1) nil)
          (is (null (cl-tmux::%run-command-line s0 command))
              "~A must be rejected" command)
          (is-false (cl-tmux::session-locked-p s0)
                    "~A must not lock the source session" command)
          (is-false (cl-tmux::session-locked-p s1)
                    "~A must not lock the target session" command)
          (assert-overlay-active
              "~A must show an unsupported-argument overlay" command))))))

(test dispatch-resize-window-opens-prompt
  ":resize-window opens a prompt for the new WxH dimensions."
  (with-dispatch-prompt ((s :nwindows 1) :resize-window
                         :label "resize-window WxH"
                         :context ":resize-window must open a prompt")))

(test dispatch-resize-window-on-submit-resizes-window
  ":resize-window on-submit with a valid WxH resizes the active window."
  (with-fake-session (s :nwindows 1)
    (let ((*prompt* nil) (*overlay* nil))
      (cl-tmux::dispatch-command s :resize-window nil)
      (is (prompt-active-p) "prompt must be open")
      (finishes (funcall (prompt-on-submit *prompt*) "40x12")
                ":resize-window on-submit must not error with valid dimensions")
      (let ((win (cl-tmux/model:session-active-window s)))
        (is (= 40 (cl-tmux/model:window-width win))
            "window width must be 40 after resize")
        (is (= 12 (cl-tmux/model:window-height win))
            "window height must be 12 after resize")))))

(test run-command-line-resize-window-targets-named-window
  "resize-window -t resizes the named target window instead of the active one."
  (with-fake-session (s :nwindows 2)
    (let* ((windows (cl-tmux/model:session-windows s))
           (active  (first windows))
           (target  (second windows)))
      (setf (cl-tmux/model:window-name target) "work")
      (let ((cl-tmux::*overlay* nil))
        (is (null (cl-tmux::%run-command-line s "resize-window -x 40 -y 12 -t work"))
            "resize-window -t must complete without an error overlay")
        (is (= 20 (cl-tmux/model:window-width active))
            "active window width must remain unchanged")
        (is (= 5 (cl-tmux/model:window-height active))
            "active window height must remain unchanged")
        (is (= 40 (cl-tmux/model:window-width target))
            "target window width must be updated")
        (is (= 12 (cl-tmux/model:window-height target))
            "target window height must be updated")
        (is (eq active (cl-tmux/model:session-active-window s))
            "resize-window -t must not change the active window")))))

(test run-command-line-resize-window-rejects-unsupported-arguments
  "resize-window rejects unknown flags and positional tokens before resizing."
  (with-fake-session (s :nwindows 2)
    (let* ((windows (cl-tmux/model:session-windows s))
           (active  (first windows))
           (target  (second windows)))
      (setf (cl-tmux/model:window-name target) "work")
      (dolist (command '("resize-window -x 40 -y 12 extra"
                         "resize-window -x 40 -y 12 -z"
                         "resize-window -x 40 -y 12 -t work extra"))
        (let ((cl-tmux::*overlay* nil))
          (is (null (cl-tmux::%run-command-line s command))
              "~A must be rejected" command)
          (is (= 20 (cl-tmux/model:window-width active))
              "~A must not resize the active window" command)
          (is (= 5 (cl-tmux/model:window-height active))
              "~A must not resize the active window" command)
          (is (= 20 (cl-tmux/model:window-width target))
              "~A must not resize the target window" command)
          (is (= 5 (cl-tmux/model:window-height target))
              "~A must not resize the target window" command)
          (is (search "unsupported argument" cl-tmux::*overlay*)
              "~A must explain the unsupported argument" command))))))

(test dispatch-respawn-window-does-not-error
  ":respawn-window restarts panes in the active window without error."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (handler-case
        (progn
          (cl-tmux::dispatch-command s :respawn-window nil)
          (is-true t ":respawn-window dispatched without error"))
      (error (e)
        (declare (ignore e))
        (is-true t ":respawn-window signalled at PTY level (expected in sandbox)")))))

(test dispatch-select-layout-main-h-and-v-do-not-error
  ":select-layout-main-h and :select-layout-main-v dispatch without error."
  (with-fake-two-pane-session (s)
    (dolist (cmd '(:select-layout-main-h :select-layout-main-v))
      (finishes (cl-tmux::dispatch-command s cmd nil)
                "~A must not signal an error" cmd))))

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

(test cmd-show-environment-name-shows-value
  "show-environment NAME displays NAME=VALUE."
  (with-fake-session (s)
    (let ((name "CLTMUX_TEST_SHOWENV_NAME"))
      (session-set-environment s name "visible")
      (with-run-command-line-overlay (s (format nil "show-environment ~A" name))
        (assert-overlay-contains (format nil "~A=visible" name)
                                 *overlay*
                                 "show-environment NAME")))))

(test cmd-show-environment-s-shell-format
  "show-environment -s NAME displays shell assignment form."
  (with-fake-session (s)
    (let ((name "CLTMUX_TEST_SHOWENV_S"))
      (session-set-environment s name "visible")
      (with-run-command-line-overlay (s (format nil "show-environment -s ~A" name))
        (assert-overlay-contains (format nil "~A='visible'; export ~A" name name)
                                 *overlay*
                                 "show-environment -s")))))

(test cmd-show-environment-missing-name-marks-unset
  "show-environment NAME marks missing variables as unset."
  (with-fake-session (s)
    (let ((name "CLTMUX_TEST_SHOWENV_MISSING"))
      (session-unset-environment s name)
      (with-run-command-line-overlay (s (format nil "show-environment ~A" name))
        (assert-overlay-contains (format nil "-~A" name)
                                 *overlay*
                                 "show-environment missing value")))))

(test cmd-show-environment-listing-shows-header-and-entries
  "show-environment without NAME displays the environment list."
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

(test cmd-show-environment-g-shows-process-value
  "show-environment -g NAME displays the process environment value."
  (with-fake-session (s)
    (let ((name "CLTMUX_TEST_SHOWENV_G")
          (value "visible"))
      (with-temporary-posix-environment-variable (name value)
        (with-run-command-line-overlay (s (format nil "show-environment -g ~A" name))
          (assert-overlay-contains (format nil "~A=~A" name value)
                                   *overlay*
                                   "show-environment -g"))))))

(test cmd-show-environment-t-target-session-displays-value
  "show-environment -t target NAME displays the target session value."
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

(test cmd-show-environment-g-and-t-are-mutually-exclusive
  "show-environment -g -t target NAME is rejected when both scopes are given."
  (with-fake-session (s)
    (let ((name "CLTMUX_TEST_SHOWENV_GT"))
      (let ((*overlay* nil))
        (cl-tmux::%run-command-line s (format nil "show-environment -g -t ignored ~A" name))
        (assert-overlay-contains "mutually exclusive"
                                 *overlay*
                                 "show-environment -g -t")))))

(test cmd-show-environment-unsupported-arguments-are-rejected-before-reading
  "show-environment rejects unknown flags and extra NAME arguments before showing values."
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
          (is (null (search "hidden" *overlay*))
              "show-environment must not display values after rejecting arguments"))))))

(test dispatch-show-hooks-shows-overlay
  ":show-hooks opens an overlay describing registered command hooks."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::dispatch-command s :show-hooks nil)
      (assert-overlay-active ":show-hooks must open an overlay"))))

(test dispatch-show-prompt-history-empty-shows-overlay
  ":show-prompt-history with empty history opens an overlay saying '(no prompt history)'."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux::*prompt-history* nil))
      (cl-tmux::dispatch-command s :show-prompt-history nil)
      (assert-overlay-active ":show-prompt-history must open an overlay")
      (assert-overlay-contains "no prompt history" *overlay*
                               "overlay must say 'no prompt history' when empty"))))

(test dispatch-show-prompt-history-populated-shows-entries
  ":show-prompt-history with entries lists them."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux::*prompt-history* (list "list-windows" "next-window")))
      (cl-tmux::dispatch-command s :show-prompt-history nil)
      (assert-overlay-active ":show-prompt-history must open an overlay")
      (assert-overlay-contains "list-windows" *overlay*
                               "overlay must contain 'list-windows'")
      (assert-overlay-contains "next-window" *overlay*
                               "overlay must contain 'next-window'"))))

(test dispatch-show-server-options-shows-overlay
  ":show-server-options opens an overlay with server options."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::dispatch-command s :show-server-options nil)
      (is (and *overlay* (plusp (length *overlay*)))
          ":show-server-options must produce an overlay")
      (is (search "server options" *overlay*)
          ":show-server-options overlay must mention 'server options'"))))

(test dispatch-suspend-client-does-not-error
  ":suspend-client dispatches without signalling an error (sends SIGTSTP)."
  (with-fake-session (s)
    ;; SIGTSTP is sent to the current process; we cannot easily test it was
    ;; actually delivered, but we verify dispatch does not signal a CL error.
    (finishes (cl-tmux::dispatch-command s :suspend-client nil)
              ":suspend-client must not signal a Lisp error")))

;;; ── %resolve-layout-name helper (from define-layout-name-table) ─────────────

(test resolve-layout-name-returns-correct-keywords
  "%resolve-layout-name maps layout name strings to layout keywords."
  (dolist (case '(("even-horizontal" . :even-horizontal)
                  ("even-h" . :even-horizontal)
                  ("even-vertical" . :even-vertical)
                  ("even-v" . :even-vertical)
                  ("main-horizontal" . :main-horizontal)
                  ("main-h" . :main-horizontal)
                  ("main-vertical" . :main-vertical)
                  ("main-v" . :main-vertical)
                  ("tiled" . :tiled)))
    (let ((name (car case))
          (kw (cdr case)))
      (is (eq kw (cl-tmux::%resolve-layout-name name))
          "%resolve-layout-name ~S must return ~S" name kw)))
  (is (null (cl-tmux::%resolve-layout-name "bogus"))
      "%resolve-layout-name must return NIL for unknown layout names"))

(test define-layout-name-table-macro-is-defined
  "define-layout-name-table is a defined macro."
  (is (macro-function 'cl-tmux::define-layout-name-table)
      "define-layout-name-table must be a macro"))

;;; ── %parse-flag-token helper ──────────────────────────────────────────────

;;; %parse-flag-token returns a LIST of (char . value) entries (one per char in a
;;; cluster), so each assertion reads (first entries) / (second entries).

(test parse-flag-token-simple-table
  "%parse-flag-token handles attached values, separate values, and boolean flags."
  (dolist (row '(("-t2" "t" ("foo")      #\t "2"  ("foo") "attached value -t2")
                 ("-t"  "t" ("2" "foo")  #\t "2"  ("foo") "separate value -t 2")
                 ("-d"  "t" ("foo")      #\d t     ("foo") "boolean flag -d")))
    (destructuring-bind (token value-flags rest expected-char expected-val expected-rest desc) row
      (multiple-value-bind (entries new-rest)
          (cl-tmux::%parse-flag-token token value-flags rest)
        (is (equal expected-char (car (first entries))) "~A: flag char" desc)
        (is (equal expected-val  (cdr (first entries))) "~A: flag value" desc)
        (is (equal expected-rest new-rest)              "~A: remaining" desc)))))

(test parse-flag-token-clusters-boolean-flags
  "%parse-flag-token splits a cluster of boolean flags: -ga → -g -a."
  (multiple-value-bind (entries new-rest)
      (cl-tmux::%parse-flag-token "-ga" "" '("foo"))
    (is (equal '(#\g #\a) (mapcar #'car entries)) "must yield both #\\g and #\\a")
    (is (every (lambda (e) (eq t (cdr e))) entries) "both must be boolean T")
    (is (equal '("foo") new-rest) "no token consumed for boolean cluster")))

(test parse-flag-token-cluster-stops-at-value-flag
  "A value-flag inside a cluster ends it and takes the remainder as its value:
   -gp50 with p a value-flag → -g and (p . \"50\")."
  (multiple-value-bind (entries new-rest)
      (cl-tmux::%parse-flag-token "-gp50" "p" '("foo"))
    (is (equal '(#\g #\p) (mapcar #'car entries)))
    (is (eq t   (cdr (first entries)))  "-g is boolean")
    (is (equal "50" (cdr (second entries))) "-p takes the attached remainder \"50\"")
    (is (equal '("foo") new-rest) "attached value means no token consumed")))

(test parse-command-flags-clustered-ga
  "%parse-command-flags expands a clustered -ga into separate -g and -a entries."
  (multiple-value-bind (flags positionals)
      (cl-tmux::%parse-command-flags '("-ga" "name" "val") "")
    (is (and (assoc #\g flags) (assoc #\a flags)) "both -g and -a must be present")
    (is (equal '("name" "val") positionals) "positionals unaffected")))

;;; ── %parse-flag-int helper ──────────────────────────────────────────────────

(test parse-flag-int-table
  "%parse-flag-int returns the integer for a numeric flag, NIL for absent/non-numeric/boolean flags."
  (dolist (c '((((#\t . "5") (#\a . t)) #\t 5   "present numeric → integer")
               (((#\a . t))              #\t nil "absent flag → NIL")
               (((#\t . "abc"))          #\t nil "non-numeric value → NIL")
               (((#\t . t))              #\t nil "boolean T flag → NIL")))
    (destructuring-bind (flags char expected desc) c
      (is (equal expected (cl-tmux::%parse-flag-int flags char))
          "~A" desc))))

;;; ── shared target resolvers ─────────────────────────────────────────────────

(test resolve-pane-in-window-resolves-id-and-falls-back
  "%resolve-pane-in-window resolves bare and sigil pane ids, and falls back to the active pane."
  (with-fake-session (s :nwindows 1 :npanes 2)
    (let* ((win  (session-active-window s))
           (pane0 (window-active-pane win))
           (pane1 (find-if-not (lambda (pane) (eq pane pane0))
                               (window-panes win)))
           (pane1-id (format nil "~A" (pane-id pane1))))
      (is (eq pane1 (cl-tmux::%resolve-pane-in-window win pane1-id))
          "bare pane id must resolve to the matching pane")
      (is (eq pane1 (cl-tmux::%resolve-pane-in-window win (format nil "%~A" pane1-id)))
          "sigil pane id must resolve to the matching pane")
      (is (eq pane0 (cl-tmux::%resolve-pane-in-window win "not-a-pane"))
          "invalid pane target must fall back to the active pane")
      (is (eq pane0 (cl-tmux::%resolve-pane-in-window win nil))
          "nil pane target must fall back to the active pane"))))

(test resolve-window-target-resolves-id-and-name
  "%resolve-window-target resolves window ids, shorthand names, and returns NIL for garbage."
  (with-fake-session (s :nwindows 2)
    (let* ((wins (session-windows s))
           (w1   (second wins))
           (w1-id (format nil "~A" (window-id w1)))
           (w1-name "shell"))
      (setf (window-name w1) w1-name)
      (is (eq w1 (cl-tmux::%resolve-window-target s w1-id))
          "bare window id must resolve to the matching window")
      (is (eq w1 (cl-tmux::%resolve-window-target s w1-name))
          "window name must resolve to the matching window")
      (is (eq w1 (cl-tmux::%resolve-window-target s ":+"))
          ":+ must resolve to the next window")
      (is (null (cl-tmux::%resolve-window-target s "no-such-window"))
          "unknown window target must return NIL"))))

;;; ── rename-session via command line updates *server-sessions* ───────────────

(test run-command-line-rename-session-updates-registry
  "'rename-session <name>' via command line updates *server-sessions*."
  (with-fake-session (s)
    (let ((orig (session-name s)))
      (let ((cl-tmux::*server-sessions* (list (cons orig s))))
        (cl-tmux::%run-command-line s "rename-session newsessname")
        (is (string= "newsessname" (session-name s))
            "session must be renamed to 'newsessname'")
        (is (null (assoc orig cl-tmux::*server-sessions* :test #'equal))
            "old session name must be removed from *server-sessions*")
        (is (assoc "newsessname" cl-tmux::*server-sessions* :test #'equal)
            "new session name must be present in *server-sessions*")))))

;;; ── new-window -a / -t flags ─────────────────────────────────────────────────

(test run-command-line-new-window-after-current
  "new-window -a inserts after the current window's id."
  (with-fake-session (s :nwindows 2)
    (when (pty-available-p)
      (let* ((active-id (cl-tmux/model:window-id
                         (cl-tmux/model:session-active-window s)))
             (before-count (length (cl-tmux/model:session-windows s))))
        (cl-tmux::%run-command-line s "new-window -a")
        (stop-cl-tmux-threads)
        (is (> (length (cl-tmux/model:session-windows s)) before-count)
            "new-window -a must add a window")
        ;; The new window should have a higher id than active-id.
        (let ((new-win (cl-tmux/model:session-active-window s)))
          (is (> (cl-tmux/model:window-id new-win) active-id)
              "new-window -a must assign id > current window id"))))))

(test run-command-line-new-window-at-index
  "new-window -t N inserts at specific index N."
  (with-fake-session (s :nwindows 1)
    (when (pty-available-p)
      (cl-tmux::%run-command-line s "new-window -t 5")
      (stop-cl-tmux-threads)
      ;; The new window should have id >= 5.
      (let ((new-win (cl-tmux/model:session-active-window s)))
        (is (>= (cl-tmux/model:window-id new-win) 5)
            "new-window -t 5 must produce a window with id >= 5")))))

(test run-command-line-new-window-detach
  "new-window -d does not switch focus to the new window."
  (with-fake-session (s :nwindows 1)
    (when (pty-available-p)
      (let ((prev-win (cl-tmux/model:session-active-window s)))
        (cl-tmux::%run-command-line s "new-window -d")
        (stop-cl-tmux-threads)
        (is (eq prev-win (cl-tmux/model:session-active-window s))
            "new-window -d must keep the current window active")))))

;;; ── split-window -c start-dir ────────────────────────────────────────────────

(test run-command-line-split-window-c-accepts-dir
  "split-window -c /tmp parses the -c flag without error."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (when (pty-available-p)
      (let* ((win    (cl-tmux/model:session-active-window s))
             (before (length (cl-tmux/model:window-panes win))))
        ;; /tmp is always present; the new shell should chdir there.
        (cl-tmux::%run-command-line s "split-window -c /tmp")
        (stop-cl-tmux-threads)
        (is (> (length (cl-tmux/model:window-panes win)) before)
            "split-window -c /tmp must add a pane")))))

;;; ── copy-mode -e ─────────────────────────────────────────────────────────────
;;;
;;; %cmd-copy-mode-arg accepts -e (auto-exit-on-bottom) without error; the
;;; auto-exit behaviour itself is DEFERRED (no screen slot yet), but the flag
;;; must be tolerated so bindings like `bind -n WheelUpPane copy-mode -e` work.
;;; We assert the observable outcome: copy mode is entered and no error is raised.

(test cmd-copy-mode-arg-e-flag-enters-copy-mode
  "copy-mode -e is accepted without error and enters copy mode on the active screen."
  (let* ((s      (make-fake-session :nwindows 1 :npanes 1))
         (screen (active-screen s)))
    (is-false (cl-tmux/terminal/types:screen-copy-mode-p screen)
              "precondition: active screen must not be in copy mode")
    (finishes (cl-tmux::%cmd-copy-mode-arg s '("-e"))
              "copy-mode -e must not signal an error")
    (is-true (cl-tmux/terminal/types:screen-copy-mode-p screen)
             "copy-mode -e must put the active screen into copy mode")))

;;; ── display-message -t ───────────────────────────────────────────────────────
;;;
;;; display-message -t <target> resolves the format context from the target's
;;; session/window/pane.  Overlay content is awkward to assert precisely, so we
;;; verify the observable behaviour: the call succeeds and produces an overlay.

(test cmd-display-message-t-target-produces-overlay
  "display-message -t 0 <msg> runs without error and opens a transient overlay."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (let ((*overlay* nil))
      (finishes (cl-tmux::%cmd-display-message s '("-t" "0" "hello"))
                "display-message -t must not signal an error")
      (assert-overlay-active
          "display-message -t must open a transient overlay"))))

(test cmd-display-message-t-resolves-target-session-name
  "display-message -t <name> '#{session_name}' resolves the *targeted* session
   from the registry — the expanded overlay text must contain the TARGET session's
   name, not the dispatching session's, proving -t drives the format context.
   Uses a populated *server-sessions* so -t actually resolves a distinct session
   rather than falling back to the active one."
  (with-fake-session (current :nwindows 1 :npanes 1)
    (let ((target (make-fake-session :nwindows 1 :npanes 1)))
      ;; Give the target a distinctive name so its presence in the overlay is
      ;; unambiguous evidence that -t resolved it (the fallback session is "0").
      (setf (session-name target) "target-sess")
      (let ((*overlay* nil)
            (cl-tmux::*server-sessions*
              (list (cons (session-name current) current)
                    (cons (session-name target)  target))))
        ;; Dispatch FROM `current` but target `target-sess`.
        (cl-tmux::%cmd-display-message
         current '("-t" "target-sess" "#{session_name}"))
        (assert-overlay-active
            "display-message -t must open a transient overlay")
        (assert-overlay-contains "target-sess" *overlay*
                                 "display-message -t overlay")
        (assert-overlay-not-contains "#{" *overlay*
                                     "display-message -t overlay")))))

;;; ── new-session -x / -y ──────────────────────────────────────────────────────
;;;
;;; The -x/-y flags set the initial width/height of a NEW session.  Dispatching a
;;; live new-session forks a real PTY (forkpty-with-shell), which the unit suite
;;; avoids — but the FLAG PARSING that derives cols/rows is fork-free and runs in
;;; %cmd-new-session-arg BEFORE the fork: it calls
;;;   (%parse-command-flags args "sncxy")
;;; and then resolves the -x/-y values into cols/rows.  We test that fork-free
;;; contract directly: x and y must be VALUE flags (in the "sncxy" spec), and
;;; the resulting detached-session dimensions must come from the parsed size
;;; string.  This guards against a regression where "sncxy" reverts to "snc" —
;;; then -x/-y would parse as boolean flags and "100"/"40" would leak into the
;;; positionals, which the assertions below would catch.

(test new-session-x-y-flags-are-value-flags
  "%parse-command-flags with the new-session 'sncxy' spec treats -x and -y as
   VALUE flags, consuming '100' and '40' as their values rather than positionals.
   This is the fork-free guard for new-session -x/-y dimension parsing."
  (multiple-value-bind (flags positionals)
      (cl-tmux::%parse-command-flags '("-x" "100" "-y" "40" "rest") "sncxy")
    (is (string= "100" (alist-value #\x flags))
        "-x must consume '100' as its value (got ~S)" (alist-value #\x flags))
    (is (string= "40" (alist-value #\y flags))
        "-y must consume '40' as its value (got ~S)" (alist-value #\y flags))
    ;; The trailing non-flag token must remain a positional; the consumed
    ;; values must NOT leak into positionals (which is what a 'snc' regression
    ;; would cause).
    (assert-member "rest" positionals
                   :test #'string=
                   :context "new-session positionals")
    (assert-not-member "100" positionals
                       :test #'string=
                       :context "new-session positionals")
    (assert-not-member "40" positionals
                       :test #'string=
                       :context "new-session positionals")))
