(in-package #:cl-tmux/test)

;;;; Dispatch runtime command tests: bind/unbind, rename, and respawn.

(in-suite dispatch-suite)

(test run-command-line-bind-with-args-binds-key
  "Runtime 'bind <key> <command>' (with args) binds via the config directive path,
   not the interactive prompt — so command-prompt / control-mode bind works."
  (with-option-session (s)
      (with-loop-state
        (cl-tmux::%run-command-line s "bind y new-window")
        (is (eq :new-window
                (cl-tmux/config:key-table-command
                 (cl-tmux/config:key-table-lookup "prefix" #\y)))
            "runtime 'bind y new-window' must bind #\\y → :new-window in prefix"))))

(test run-command-line-unbind-a-clears-prefix-table
  "Runtime 'unbind -a' clears the prefix table (arg-bearing unbind routes through
   the config directive logic, including the -a whole-table form)."
  (with-option-session (s)
      (with-loop-state
        (is (not (null (cl-tmux/config:key-table-lookup "prefix" #\c)))
            "prefix has bindings before unbind -a")
        (cl-tmux::%run-command-line s "unbind -a")
        (is (null (cl-tmux/config:key-table-lookup "prefix" #\c))
            "runtime 'unbind -a' must clear the prefix table"))))

(test cmd-set-option-clustered-ga-appends
  "'set-option -ga name val' (clustered -g -a) APPENDS — regression: the cluster was
   parsed as -g only, silently dropping -a and overwriting instead of appending."
  (with-option-session (s)
      (cl-tmux/options:set-option "@opt" "A")
      (cl-tmux::%run-command-line s "set-option -ga @opt B")
      (is (string= "AB" (cl-tmux/options:get-option "@opt"))
          "set-option -ga must append B to A, yielding AB")))

(test cmd-set-option-F-expands-format-value
  "'set-option -gF name #{...}' expands the format value once at set time."
  (with-option-session (s)                      ; session name is "0"
    (with-loop-state
      (cl-tmux::%run-command-line s "set-option -gF @opt #{session_name}")
      (is (string= "0" (cl-tmux/options:get-option "@opt"))
          "-F must store the expanded session name, not the literal #{...}"))))

(test run-command-line-rename-window
  "'rename-window <name>' renames the active window."
  (with-fake-session (s :nwindows 1)
    (cl-tmux::%run-command-line s "rename-window mywin")
    (is (string= "mywin" (window-name (session-active-window s)))
        "active window must be renamed to 'mywin'")))

(test run-command-line-rename-window-t-targets-window
  "'rename-window -t 1 newname' renames window-id 1, NOT the active window, and
   does not fold the -t flag tokens into the new name."
  (with-fake-session (s :nwindows 2)
    (let ((w0 (first  (session-windows s)))    ; id 0, active
          (w1 (second (session-windows s))))   ; id 1
      (cl-tmux::%run-command-line s "rename-window -t 1 newname")
      (is (string= "newname" (window-name w1))
          "window-id 1 must be renamed to 'newname'")
      (is (not (string= "newname" (window-name w0)))
          "the active window (id 0) must be unchanged"))))

(test run-command-line-rename-window-rejects-unsupported-flags
  "rename-window rejects unknown flags before changing the window name."
  (with-fake-session (s :nwindows 1)
    (let* ((win (session-active-window s))
           (before (window-name win))
           (*overlay* nil))
      (is (null (cl-tmux::%run-command-line s "rename-window -x renamed"))
          "rename-window -x must be rejected")
      (is (string= before (window-name win))
          "rename-window -x must not rename the window")
      (assert-overlay-contains "unsupported argument" *overlay*
                                "rename-window -x"))))

(test run-command-line-rename-session
  "'rename-session <name>' renames the session."
  (with-fake-session (s)
    (cl-tmux::%run-command-line s "rename-session mysess")
    (is (string= "mysess" (session-name s))
        "session must be renamed to 'mysess'")))

(test run-command-line-rename-session-t-targets-session
  "'rename-session -t other newname' renames the -t target session, not the
   current one, and does not fold the flag tokens into the name."
  (with-fake-session (cur)
    (let ((other (make-fake-session)))
      (setf (cl-tmux::session-name cur)   "cur"
            (cl-tmux::session-name other) "other")
      (let ((cl-tmux::*server-sessions* (list (cons "cur" cur) (cons "other" other))))
        (cl-tmux::%run-command-line cur "rename-session -t other newname")
        (is (string= "newname" (session-name other))
            "the -t target session must be renamed to 'newname'")
        (is (string= "cur" (session-name cur))
            "the current session must be unchanged")))))

(test run-command-line-rename-session-missing-target-falls-back-to-current
  "'rename-session -t missing newname' falls back to the current session when
   the target cannot be resolved."
  (with-fake-session (cur)
    (setf (cl-tmux::session-name cur) "cur")
    (let ((cl-tmux::*server-sessions* (list (cons "cur" cur))))
      (cl-tmux::%run-command-line cur "rename-session -t missing newname")
      (is (string= "newname" (session-name cur))
          "missing -t should still rename the current session")
      (is (assoc "newname" cl-tmux::*server-sessions* :test #'equal)
          "the registry must be updated to the fallback session's new name")
      (is (null (assoc "cur" cl-tmux::*server-sessions* :test #'equal))
          "the old registry key must be removed"))))

(test run-command-line-rename-session-no-arg-opens-prompt
  "'rename-session' with no argument falls through to the prompt."
  (with-fake-session (s)
    (let ((cl-tmux::*prompt* nil))
      (cl-tmux::%run-command-line s "rename-session")
      (is (prompt-active-p)
          "no-arg rename-session must open the rename prompt"))))

(test run-command-line-rename-session-rejects-unsupported-flags
  "rename-session rejects unknown flags before changing the session registry."
  (with-fake-session (s)
    (setf (cl-tmux::session-name s) "old")
    (let ((cl-tmux::*server-sessions* (list (cons "old" s)))
          (*overlay* nil))
      (is (null (cl-tmux::%run-command-line s "rename-session -x new"))
          "rename-session -x must be rejected")
      (is (string= "old" (session-name s))
          "rename-session -x must not rename the session")
      (is (assoc "old" cl-tmux::*server-sessions* :test #'equal)
          "rename-session -x must keep the old registry entry")
      (is (null (assoc "new" cl-tmux::*server-sessions* :test #'equal))
          "rename-session -x must not add a new registry entry")
      (assert-overlay-contains "unsupported argument" *overlay*
                                "rename-session -x"))))

(test cmd-set-window-option-t-targets-window
  "set-window-option -t 1 @wopt myval sets the WINDOW-LOCAL option on window-id 1, not the
   active window — and -t no longer leaks into the option name."
  (with-fake-session (s :nwindows 2)
    (let ((w0 (first  (session-windows s)))    ; id 0, active
          (w1 (second (session-windows s))))   ; id 1
      (cl-tmux::%run-command-line s "set-window-option -t 1 @wopt myval")
      (is (string= "myval" (cl-tmux/options:get-option-for-window "@wopt" w1))
          "window-id 1 must have the window-local @wopt = myval")
      (is (null (cl-tmux/options:get-option-for-window "@wopt" w0))
          "the active window (id 0) must NOT have @wopt set"))))

(test cmd-respawn-pane-without-k-errors-on-live-pane
  "respawn-pane without -k on a still-running pane (fd > 0) is an error and does
   NOT respawn — matching tmux (the model would otherwise fork unconditionally)."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (let ((pane (window-active-pane (session-active-window s))))
      (setf (cl-tmux/model:pane-fd pane) 5)       ; simulate a live PTY
      (let ((*overlay* nil))
        (cl-tmux::%cmd-respawn-pane-arg s '())
        (assert-overlay-active
            "respawn-pane without -k on a live pane must show an error overlay")
        (is (= 5 (cl-tmux/model:pane-fd pane))
            "the live pane must NOT be respawned (fd unchanged → no fork)")))))

(test cmd-respawn-window-without-k-errors-on-live-pane
  "respawn-window without -k errors when ANY pane in the window is still running,
   and does NOT respawn — matching tmux."
  (with-fake-two-pane-session (s)
    (let* ((win (session-active-window s))
           (p1  (first (window-panes win))))
      (setf (cl-tmux/model:pane-fd p1) 5)         ; one pane is live
      (let ((*overlay* nil))
        (cl-tmux::%cmd-respawn-window-arg s '())
        (assert-overlay-active
            "respawn-window without -k with a live pane must show an error overlay")
        (is (= 5 (cl-tmux/model:pane-fd p1))
            "the window must NOT be respawned (live pane fd unchanged → no fork)")))))

(test cmd-respawn-pane-forwards-overrides
  "respawn-pane forwards -c, repeated -e, and positional command overrides."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (let* ((pane (window-active-pane (session-active-window s)))
           (cl-tmux::*dirty* nil)
           (*overlay* nil))
      (with-mocked-respawn-pane (calls reader-calls)
        (is (eql t (cl-tmux::%cmd-respawn-pane-arg
                    s '("-k" "-c" "/tmp" "-e" "NAME=value" "-e" "EMPTY" "printf" "hello world")))
            "respawn-pane must accept overrides")
        (is-false *overlay* "accepted overrides must not show an overlay")
        (is (eql t cl-tmux::*dirty*) "accepted respawn-pane calls must mark the model dirty")
        (is (= 1 (length calls)) "respawn-pane must be invoked once")
        (destructuring-bind (session called-pane start-dir default-command extra-env)
            (first calls)
          (is (eq s session) "the original session must be forwarded")
          (is (eq pane called-pane) "the active pane must be forwarded")
          (is (string= "/tmp" start-dir) "-c must become :start-dir")
          (is (string= "printf hello world" default-command)
              "positional words must become the command override")
          (is (equal '(("NAME" . "value") ("EMPTY" . "")) extra-env)
              "repeated -e flags must become extra-env pairs"))
        (is (= 1 (length reader-calls)) "start-reader-thread must run once")
        (is (eq pane (first reader-calls))
            "the respawned pane must be handed to the reader thread")))))

(test cmd-respawn-window-forwards-overrides
  "respawn-window forwards -c, repeated -e, and positional command overrides to every pane."
  (with-fake-two-pane-session (s)
    (let* ((win (session-active-window s))
           (panes (window-panes win))
           (cl-tmux::*dirty* nil)
           (*overlay* nil))
      (with-mocked-respawn-pane (calls reader-calls)
        (is (eql t (cl-tmux::%cmd-respawn-window-arg
                    s '("-k" "-c" "/tmp" "-e" "A=1" "echo" "ok")))
            "respawn-window must accept overrides")
        (is-false *overlay* "accepted overrides must not show an overlay")
        (is (eql t cl-tmux::*dirty*) "accepted respawn-window calls must mark the model dirty")
        (is (= 2 (length calls)) "respawn-window must respawn every pane")
        (dolist (call (nreverse calls))
          (destructuring-bind (session called-pane start-dir default-command extra-env) call
            (is (eq s session) "the original session must be forwarded")
            (is (member called-pane panes :test #'eq) "each pane must be forwarded")
            (is (string= "/tmp" start-dir) "-c must become :start-dir")
            (is (string= "echo ok" default-command)
                "positional words must become the shared command override")
            (is (equal '(("A" . "1")) extra-env)
                "repeated -e flags must become shared extra-env pairs")))
        (is (= 2 (length reader-calls)) "each respawned pane must start a reader thread")
        (dolist (pane panes)
          (is (member pane reader-calls :test #'eq)
              "every pane must be handed to a reader thread"))))))

