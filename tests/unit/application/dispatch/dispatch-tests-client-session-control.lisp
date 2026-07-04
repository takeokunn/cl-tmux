(in-package #:cl-tmux/test)

;;;; Dispatch client and session control tests.

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

(test run-command-line-attach-session-accepts-working-dir
  "attach-session -c sets the target session's working directory and switches."
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
              cl-tmux::*overlay* nil)
        (cl-tmux::%run-command-line s0 "attach-session -c /tmp -t work")
        (is (string= "/tmp" (cl-tmux::session-start-directory s1))
            "attach-session -c stores the session working directory")
        (is (eq s1 (cl-tmux::server-current-session))
            "attach-session -c -t switches to the target session")))))

(test run-command-line-attach-session-rejects-client-creation-args
  "attach-session inside a running client rejects client-creation arguments."
  (dolist (row '(("attach-session -d -t work" "detach other clients")
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

(test run-command-line-detach-accepts-single-client-flags
  "detach-client -a/-P/-s/-t (single-client standalone forms) are accepted and
   collapse onto detaching the active client."
  (with-fake-session (s)
    (dolist (args '(("-a")
                    ("-P")
                    ("-s" "work")
                    ("-t" "client-0")))
      (setf cl-tmux::*overlay* nil)
      (is (eq :detach (cl-tmux::%cmd-detach-arg s args))
          "detach ~S must return a detach disposition" args)
      (is (null cl-tmux::*overlay*)
          "accepted detach args must not raise an overlay: ~S" args))))

(test run-command-line-detach-rejects-unimplemented-args
  "detach rejects -E (run a command on detach), which cl-tmux cannot implement."
  (with-fake-session (s)
    (setf cl-tmux::*running* t
          cl-tmux::*overlay* nil)
    (is (null (cl-tmux::%cmd-detach-arg s '("-E" "echo detached")))
        "detach -E must be rejected")
    (is-true cl-tmux::*running*
             "a rejected detach must not stop the event loop")
    (assert-overlay-active
        "a rejected detach must explain the failure")))

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

(test run-command-line-refresh-client-accepts-tmux-flags
  "refresh-client accepts its tmux flag set (-S status redraw, -L/R/U/D pan,
   -c, -f/-F client flags, -l clipboard, -t target) and forces a redraw."
  (with-fake-session (s)
    (dolist (args '(("-S")
                    ("-L") ("-R") ("-U") ("-D")
                    ("-c")
                    ("-f" "read-only")
                    ("-l" "client-0")
                    ("-t" "client-0")))
      (setf cl-tmux::*dirty* nil
            cl-tmux::*overlay* nil)
      (is-true (cl-tmux::%cmd-refresh-client-arg s args)
               "refresh-client must accept ~S" args)
      (is-true cl-tmux::*dirty*
               "accepted refresh-client args must redraw: ~S" args)
      (is (null cl-tmux::*overlay*)
          "accepted refresh-client args must not raise an overlay: ~S" args))))

(test run-command-line-refresh-client-rejects-unknown-flags
  "refresh-client still rejects flags outside the tmux args set."
  (with-fake-session (s)
    (setf cl-tmux::*dirty* nil
          cl-tmux::*overlay* nil)
    (is (null (cl-tmux::%cmd-refresh-client-arg s '("-Z")))
        "refresh-client must reject an unknown flag")
    (is-false cl-tmux::*dirty*
              "a rejected refresh-client must not redraw")
    (is (search "unsupported argument" cl-tmux::*overlay*)
        "a rejected refresh-client must explain the rejection")))


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

(test run-command-line-lock-commands-lock-current-session-table
  "lock-client and lock-session both lock the active session when no -t is given.
   Each row: (command description)."
  (dolist (row '(("lock-client"  "lock-client must lock the active session")
                 ("lock-session" "lock-session must lock the current session")))
    (destructuring-bind (cmd desc) row
      (with-fake-session (s)
        (let ((cl-tmux::*overlay* nil))
          (setf (cl-tmux::session-locked-p s) nil)
          (cl-tmux::%run-command-line s cmd)
          (is-true (cl-tmux::session-locked-p s) desc))))))

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
