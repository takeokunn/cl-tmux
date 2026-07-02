(in-package #:cl-tmux/test)

;;;; dispatch tests — part D: %cmd-set-option scope routing, side-effects,
;;;; run-command-line bind/unbind/rename, set-hook, cmd-list-commands.

(in-suite dispatch-suite)

;;; ── %cmd-set-option scope routing: -w / -p / global ──────────────────────

(test cmd-set-option-w-routes-to-window-local
  "'set -w' routes to the active window's local options and leaves the global
   value unchanged."
  (with-option-session (s)
    (let* ((win (cl-tmux/model:session-active-window s)))
      (cl-tmux/options:set-option "synchronize-panes" nil)
      (cl-tmux::%cmd-set-option s '("-w" "synchronize-panes" "on"))
      (is (eq t (cl-tmux/options:get-option-for-window "synchronize-panes" win))
          "active window's synchronize-panes must be T after 'set -w'")
      (is (null (cl-tmux/options:get-option "synchronize-panes"))
          "global synchronize-panes must remain NIL — 'set -w' must not touch it"))))

(test cmd-set-option-o-w-checks-window-local-not-global
  "'set -o -w name value' consults the WINDOW-LOCAL store, not the global table:
   a global value must NOT block a window-local -o set (audit #4 — the
   only-if-unset check used to always read *global-options*)."
  (with-option-session (s)
    (let ((win (cl-tmux/model:session-active-window s)))
      (remhash "@plugin-opt" (cl-tmux/model:window-local-options win))
      (cl-tmux/options:set-option "@plugin-opt" "global-value")
      (cl-tmux::%cmd-set-option s '("-o" "-w" "@plugin-opt" "win-value"))
      (is (string= "win-value"
                   (nth-value 0 (gethash "@plugin-opt"
                                         (cl-tmux/model:window-local-options win))))
          "-o -w must set the window-local option when unset, even with a global value")
      (is (string= "global-value" (cl-tmux/options:get-option "@plugin-opt"))
          "-o -w must not touch the global value"))))

(test cmd-set-option-o-w-skips-when-window-local-already-set
  "'set -o -w name value' is a no-op when the WINDOW already has a local override."
  (with-option-session (s)
    (let ((win (cl-tmux/model:session-active-window s)))
      (cl-tmux/options:set-option-for-window "@plugin-opt" "win-user-value" win)
      (cl-tmux::%cmd-set-option s '("-o" "-w" "@plugin-opt" "default-value"))
      (is (string= "win-user-value"
                   (nth-value 0 (gethash "@plugin-opt"
                                         (cl-tmux/model:window-local-options win))))
          "-o -w must leave the existing window-local value untouched"))))

(test cmd-set-window-option-defaults-to-window-scope
  "set-window-option (no -g) sets a WINDOW-local option, not global —
   tmux's set-window-option is `set -w`."
  (with-option-session (s)
    (let* ((win (cl-tmux/model:session-active-window s)))
      (cl-tmux/options:set-option "synchronize-panes" nil)
      (cl-tmux::%cmd-set-window-option s '("synchronize-panes" "on"))
      (is (eq t (cl-tmux/options:get-option-for-window "synchronize-panes" win))
          "bare set-window-option must set the window-local option to T")
      (is (null (cl-tmux/options:get-option "synchronize-panes"))
          "bare set-window-option must NOT touch the global option"))))

(test cmd-set-window-option-g-overrides-to-global
  "set-window-option -g sets the GLOBAL option (explicit -g wins over the injected -w)."
  (with-option-session (s)
    (let* ((win (cl-tmux/model:session-active-window s)))
      (cl-tmux/options:set-option "synchronize-panes" nil)
      (cl-tmux::%cmd-set-window-option s '("-g" "synchronize-panes" "on"))
      (is (eq t (cl-tmux/options:get-option "synchronize-panes"))
          "set-window-option -g must set the global option")
      (is (null (nth-value 1 (gethash "synchronize-panes"
                                      (cl-tmux/model:window-local-options win))))
          "set-window-option -g must NOT create a window-local override"))))

(test cmd-set-option-p-routes-to-pane-local
  "'set -p' routes to the active pane's local options and leaves the global
   value unchanged."
  (with-option-session (s)
    (let* ((pane (cl-tmux/model:session-active-pane s)))
      (cl-tmux/options:set-option "remain-on-exit" nil)
      (cl-tmux::%cmd-set-option s '("-p" "remain-on-exit" "on"))
      (is (eq t (cl-tmux/options:get-option-for-pane "remain-on-exit" pane))
          "active pane's remain-on-exit must be T after 'set -p'")
      (is (null (cl-tmux/options:get-option "remain-on-exit"))
          "global remain-on-exit must remain NIL — 'set -p' must not touch it"))))

(test cmd-set-option-p-unset-clears-pane-local-not-global
  "'set -p -u' removes the pane-local override and leaves the global value intact."
  (with-option-session (s)
    (let* ((pane (cl-tmux/model:session-active-pane s)))
      (cl-tmux/options:set-option "remain-on-exit" t)
      (cl-tmux/options:set-option-for-pane "remain-on-exit" "on" pane)
      (cl-tmux::%cmd-set-option s '("-p" "-u" "remain-on-exit"))
      (is (not (nth-value 1 (gethash "remain-on-exit"
                                     (cl-tmux/model:pane-local-options pane))))
          "active pane's remain-on-exit must be removed after 'set -p -u'")
      (is (eq t (cl-tmux/options:get-option "remain-on-exit"))
          "global remain-on-exit must remain intact"))))

(test cmd-set-option-status-applies-side-effect-at-runtime
  "Runtime `set -g status off/on` runs the option side-effect, updating
   *status-height* — not only the .tmux.conf path (e.g. `bind b set -g status`)."
  (with-option-session (s)
      (cl-tmux::%cmd-set-option s '("-g" "status" "off"))
      (is (= 0 cl-tmux/config:*status-height*)
          "set -g status off must hide the status bar at runtime")
      (cl-tmux::%cmd-set-option s '("-g" "status" "on"))
      (is (= 1 cl-tmux/config:*status-height*)
          "set -g status on must restore the status bar at runtime")))

(test cmd-set-option-default-shell-applies-side-effect-at-runtime
  "Runtime `set -g default-shell` updates *default-shell* via the side-effect."
  (with-option-session (s)
      (cl-tmux::%cmd-set-option s '("-g" "default-shell" "/bin/zsh"))
      (is (string= "/bin/zsh" cl-tmux/config:*default-shell*)
          "runtime set -g default-shell must update *default-shell*")))

(test cmd-set-option-prefix-applies-side-effect-at-runtime
  "Runtime `set -g prefix C-a` rebinds the prefix key code via the side-effect."
  (with-option-session (s)
      (cl-tmux::%cmd-set-option s '("-g" "prefix" "C-a"))
      (is (= 1 cl-tmux/config:*prefix-key-code*)
          "set -g prefix C-a must set *prefix-key-code* to ^A (1) at runtime")))

(test cmd-set-option-escape-time-syncs-to-server-store
  "escape-time is read from the server store by the ESC-flush, but is commonly
   set via `set -g escape-time 0` (global).  The side-effect syncs it across, so
   the common form takes effect."
  (with-option-session (s)
      ;; -g writes the global store; the side-effect mirrors it into the server store.
      (cl-tmux::%cmd-set-option s '("-g" "escape-time" "0"))
      (is (= 0 (cl-tmux/options:get-server-option "escape-time"))
          "set -g escape-time 0 must reach the server store the flush reads")
      ;; bare set (no scope) also syncs.
      (cl-tmux::%cmd-set-option s '("escape-time" "10"))
      (is (= 10 (cl-tmux/options:get-server-option "escape-time"))
          "bare set escape-time must also reach the server store")))

(test cmd-set-option-unset-resets-runtime-side-effects
  "`set-option -u` must reset special-option runtime state back to defaults,
   not just remove the stored option value."
  (with-option-session (s)
    (let ((cl-tmux/config:*status-height* 4)
          (cl-tmux/config:*default-shell* "/bin/zsh")
          (cl-tmux/config:*prefix-key-code* 1)
          (cl-tmux/config:*prefix2-key-code* 42)
          (cl-tmux/model:*update-environment* '("CUSTOM" "PATH")))
      (cl-tmux/options:set-server-option "escape-time" 0)
      (cl-tmux::%cmd-set-option s '("-g" "status" "off"))
      (cl-tmux::%cmd-set-option s '("-g" "default-shell" "/bin/zsh"))
      (cl-tmux::%cmd-set-option s '("-g" "prefix" "C-a"))
      (cl-tmux::%cmd-set-option s '("-g" "prefix2" "C-g"))
      (cl-tmux::%cmd-set-option s '("-g" "update-environment" "HOME PATH"))
      (cl-tmux::%cmd-set-option s '("-g" "escape-time" "0"))
      (cl-tmux::%cmd-set-option s '("-u" "status"))
      (cl-tmux::%cmd-set-option s '("-u" "default-shell"))
      (cl-tmux::%cmd-set-option s '("-u" "prefix"))
      (cl-tmux::%cmd-set-option s '("-u" "prefix2"))
      (cl-tmux::%cmd-set-option s '("-u" "update-environment"))
      (cl-tmux::%cmd-set-option s '("-u" "escape-time"))
      (is (= 1 cl-tmux/config:*status-height*)
          "unset status must restore the default height of 1")
      (is (string= "/bin/sh" cl-tmux/config:*default-shell*)
          "unset default-shell must restore /bin/sh")
      (is (= cl-tmux/config:+prefix-key-code+ cl-tmux/config:*prefix-key-code*)
          "unset prefix must restore +prefix-key-code+")
      (is (null cl-tmux/config:*prefix2-key-code*)
          "unset prefix2 must restore NIL")
      (is (equal cl-tmux/model:+default-update-environment+
                 cl-tmux/model:*update-environment*)
          "unset update-environment must restore the session default list")
      (is (= 10 (cl-tmux/options:get-server-option "escape-time"))
          "unset escape-time must restore the server default of 10"))))

(test cmd-set-option-plain-routes-to-global
  "A plain 'set-option' (no scope flag) of a SESSION-scoped option sets the global
   store.  WINDOW-scoped names route to the active window instead (audit #7)."
  (with-option-session (s)
      (cl-tmux::%cmd-set-option s '("history-limit" "5000"))
      (is (= 5000 (cl-tmux/options:get-option "history-limit"))
          "plain 'set-option' of a session option must set the global value")))

(test cmd-set-option-plain-window-option-routes-to-window
  "A plain 'set-option' of a WINDOW-scoped option name (no flag) routes to the
   active window's local store, mirroring tmux options_scope_from_name (audit #7)."
  (with-option-session (s)
    (let ((win (cl-tmux/model:session-active-window s)))
      (cl-tmux::%cmd-set-option s '("synchronize-panes" "on"))
      (is (eq t (nth-value 0 (gethash "synchronize-panes"
                                      (cl-tmux/model:window-local-options win))))
          "plain set of a window option must write the active window's local store")
      (is (null (cl-tmux/options:get-option "synchronize-panes"))
          "plain set of a window option must NOT change the global value (stays default nil)"))))

(test cmd-set-option-gw-stays-global
  "'set -g -w' must set the GLOBAL option (the explicit -g overrides -w) and
   leave the active window WITHOUT a local override.  Guards the (not globalp)
   gate in %cmd-set-option."
  (with-option-session (s)
    (let* ((win (cl-tmux/model:session-active-window s)))
      (cl-tmux/options:set-option "synchronize-panes" nil)
      (cl-tmux::%cmd-set-option s '("-g" "-w" "synchronize-panes" "on"))
      (is (eq t (cl-tmux/options:get-option "synchronize-panes"))
          "-g must override -w: global synchronize-panes must be T")
      (is (null (nth-value 1 (gethash "synchronize-panes"
                                      (cl-tmux/model:window-local-options win))))
          "active window must have NO local override (local hash lacks the key)")
      (is (eq t (cl-tmux/options:get-option-for-window "synchronize-panes" win))
          "get-option-for-window must fall back to the global value T"))))

(test cmd-set-option-rejects-unsupported-terminal-options
  "Runtime set-option rejects terminal matching directives instead of storing no-op state."
  (dolist (args '(("-g" "terminal-overrides" "xterm*:RGB")
                  ("-g" "terminal-features" "xterm*:RGB")))
    (with-option-session (s)
      (let ((*overlay* nil))
        (destructuring-bind (_ name value) args
          (declare (ignore _))
          (cl-tmux::%cmd-set-option s args)
          (is (search "unsupported option" *overlay*)
              "~A must produce an unsupported-option overlay" name)
          (is (not (equal value (cl-tmux/options:get-option name nil)))
              "~A must not be stored when terminal matching is unsupported" name))))))

(test cmd-set-option-o-flag-variants
  "set -o only sets the option when no value exists: skips if already set,
   writes default if absent.
   Each row: (pre-value expected description)."
  (dolist (row '(("user-value" "user-value"    "-o must leave existing value untouched")
                 (nil          "default-value" "-o must set the option when currently unset")))
    (destructuring-bind (pre-value expected desc) row
      (with-option-session (s)
        (if pre-value
            (cl-tmux/options:set-option "@plugin-opt" pre-value)
            (remhash "@plugin-opt" cl-tmux/options:*global-options*))
        (cl-tmux::%cmd-set-option s '("-o" "@plugin-opt" "default-value"))
        (is (string= expected (cl-tmux/options:get-option "@plugin-opt"))
            desc)))))

(test cmd-set-option-o-already-set-reports-tmux-error
  "set -o on an already-set option reports tmux's 'already set: NAME' error;
   -q suppresses it.  Each row: (args expect-error-p description)."
  (dolist (row '((("-o" "@plugin-opt" "v2")      t   "-o must report already set")
                 (("-o" "-q" "@plugin-opt" "v2") nil "-o -q must stay silent")))
    (destructuring-bind (args expect-error-p desc) row
      (with-option-session (s)
        (let ((*overlay* nil))
          (cl-tmux/options:set-option "@plugin-opt" "v1")
          (cl-tmux::%cmd-set-option s args)
          (is (string= "v1" (cl-tmux/options:get-option "@plugin-opt"))
              "~A: existing value must stay untouched" desc)
          (if expect-error-p
              (is (search "already set: @plugin-opt" *overlay*) desc)
              (is (null (and *overlay*
                             (search "already set" *overlay*)))
                  desc)))))))

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

(test set-hook-after-select-window-fires-config-command
  "set-hook -g after-select-window <cmd> (the .tmux.conf path) fires the command
   when select-window runs — i.e. the hook reaches run-command-hooks, not just the
   programmatic add-hook registry."
  (with-fake-session (s :nwindows 2)
    (cl-tmux/hooks:clear-command-hooks "after-select-window")
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line
       s "set-hook -g after-select-window \"display-message hooked\"")
      (cl-tmux::%run-command-line s "select-window -n")
      (assert-overlay-contains "hooked" *overlay*
                               "the set-hook after-select-window command")
      (cl-tmux/hooks:clear-command-hooks "after-select-window"))))

(test set-hook-after-select-pane-fires-config-command
  "set-hook -g after-select-pane <cmd> fires when select-pane runs (config path)."
  (with-fake-two-pane-session (s)
    (cl-tmux/hooks:clear-command-hooks "after-select-pane")
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line
       s "set-hook -g after-select-pane \"display-message picked\"")
      (cl-tmux::%run-command-line s "select-pane -t 2")
      (assert-overlay-contains "picked" *overlay*
                               "the set-hook after-select-pane command")
      (cl-tmux/hooks:clear-command-hooks "after-select-pane"))))

(test set-hook-select-pane-session-lookup-table
  "window-pane-changed and pane-focus-in both fire on select-pane -t 2 via session lookup."
  (dolist (row '(("window-pane-changed" "swapped" "must fire via session lookup")
                 ("pane-focus-in"       "focused" "must fire via session lookup")))
    (destructuring-bind (hook-name word desc) row
      (with-fake-two-pane-session (s)
        (cl-tmux/hooks:clear-command-hooks hook-name)
        (let ((cl-tmux::*server-sessions* (list (cons "0" s)))
              (*overlay* nil))
          (cl-tmux::%run-command-line
           s (format nil "set-hook -g ~A \"display-message ~A\"" hook-name word))
          (cl-tmux::%run-command-line s "select-pane -t 2")
          (assert-overlay-contains word *overlay*
                                   (format nil "~A ~A" hook-name desc))
          (cl-tmux/hooks:clear-command-hooks hook-name))))))

(test set-hook-after-rename-window-fires-config-via-unified-run-hooks
  "set-hook -g after-rename-window <cmd> fires on rename — proving the unified
   run-hooks now drives .tmux.conf set-hook for hooks whose firing point only
   called run-hooks (after-rename-window was previously config-broken)."
  (with-fake-session (s :nwindows 1)
    (cl-tmux/hooks:clear-command-hooks "after-rename-window")
    (let ((cl-tmux::*server-sessions* (list (cons "0" s)))
          (*overlay* nil))
      (cl-tmux::%run-command-line
       s "set-hook -g after-rename-window \"display-message renamed-hook\"")
      (cl-tmux::%run-command-line s "rename-window newname")
      (assert-overlay-contains "renamed-hook" *overlay*
                               "after-rename-window set-hook")
      (cl-tmux/hooks:clear-command-hooks "after-rename-window"))))

(test cmd-list-commands-resolves-prefixes-and-reports-ambiguity
  "tmux 3.6a resolves unique prefixes for list-commands and reports the full
   ambiguous candidate list."
  (with-fake-session (s)
    (dolist (case '(("list-commands new-w"
                     "new-window (neww) [-abdkPS] [-c start-directory] [-e environment] [-F format] [-n window-name] [-t target-window] [shell-command [argument ...]]")
                    ("list-commands list-s"
                     "list-sessions (ls) [-F format] [-f filter]")
                    ("list-commands lscm"
                     "list-commands (lscm) [-F format] [command]")
                    ("list-commands list"
                     "ambiguous command: list, could be: list-buffers, list-clients, list-commands, list-keys, list-panes, list-sessions, list-windows")))
      (destructuring-bind (line expected) case
        (let ((*overlay* nil))
          (cl-tmux::%run-command-line s line)
          (assert-overlay-contains expected *overlay* line))))))

(test cmd-list-commands-format-command-list-name
  "tmux 3.6a expands command_list_* fields for list-commands format output."
  (with-fake-session (s)
    (dolist (case '(("list-commands -F '#{command_list_name}|#{command_list_alias}|#{command_list_usage}' list-sessions"
                     "list-sessions|ls|[-F format] [-f filter]")
                    ("list-commands -F '#{command_list_name}|#{command_list_alias}|#{command_list_usage}' list-commands"
                     "list-commands|lscm|[-F format] [command]")))
      (destructuring-bind (line expected) case
        (let ((*overlay* nil))
          (cl-tmux::%run-command-line s line)
          (assert-overlay-contains expected *overlay* line)
          (assert-overlay-not-contains "#{command_list_name}" *overlay* line)
          (assert-overlay-not-contains "#{command_list_alias}" *overlay* line)
          (assert-overlay-not-contains "#{command_list_usage}" *overlay* line))))))

(test cmd-list-commands-unsupported-arguments-are-rejected-before-output
  "tmux 3.6a rejects invalid list-commands flags and excess arguments before
   output."
  (with-fake-session (s)
    (dolist (case '(("list-commands -Z" "command list-commands: unknown flag -Z")
                    ("list-commands -F" "command list-commands: -F expects an argument")
                    ("list-commands new-window kill-pane"
                     "command list-commands: too many arguments (need at most 1)")))
      (destructuring-bind (line expected) case
        (let ((*overlay* nil))
          (cl-tmux::%run-command-line s line)
          (assert-overlay-contains expected *overlay* line)
          (assert-overlay-not-contains "new-window" *overlay* line))))))

(test run-command-line-rename-window-no-arg-opens-prompt
  "'rename-window' with no argument falls through to the prompt (name table)."
  (with-fake-session (s :nwindows 1)
    (let ((*prompt* nil))
      (cl-tmux::%run-command-line s "rename-window")
      (is (prompt-active-p)
          "no-arg rename-window must open the rename prompt"))))
