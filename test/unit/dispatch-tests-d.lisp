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

(test cmd-set-window-option-defaults-to-window-scope
  "setw / set-window-option (no -g) sets a WINDOW-local option, not global —
   tmux's setw is `set -w`."
  (with-option-session (s)
    (let* ((win (cl-tmux/model:session-active-window s)))
      (cl-tmux/options:set-option "synchronize-panes" nil)
      (cl-tmux::%cmd-set-window-option s '("synchronize-panes" "on"))
      (is (eq t (cl-tmux/options:get-option-for-window "synchronize-panes" win))
          "bare setw must set the window-local option to T")
      (is (null (cl-tmux/options:get-option "synchronize-panes"))
          "bare setw must NOT touch the global option"))))

(test cmd-set-window-option-g-overrides-to-global
  "setw -g sets the GLOBAL option (explicit -g wins over the injected -w)."
  (with-option-session (s)
    (let* ((win (cl-tmux/model:session-active-window s)))
      (cl-tmux/options:set-option "synchronize-panes" nil)
      (cl-tmux::%cmd-set-window-option s '("-g" "synchronize-panes" "on"))
      (is (eq t (cl-tmux/options:get-option "synchronize-panes"))
          "setw -g must set the global option")
      (is (null (nth-value 1 (gethash "synchronize-panes"
                                      (cl-tmux/model:window-local-options win))))
          "setw -g must NOT create a window-local override"))))

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

(test cmd-set-option-plain-routes-to-global
  "A plain 'set' (no -w/-p) still sets the global option."
  (with-option-session (s)
      (cl-tmux/options:set-option "synchronize-panes" nil)
      (cl-tmux::%cmd-set-option s '("synchronize-panes" "on"))
      (is (eq t (cl-tmux/options:get-option "synchronize-panes"))
          "plain 'set' must set the global synchronize-panes to T")))

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

(test cmd-set-option-o-skips-when-already-set
  "'set -o name value' is a no-op when the option already has a value — the
   only-if-unset plugin idiom must NOT clobber a user override."
  (with-option-session (s)
      (cl-tmux/options:set-option "@plugin-opt" "user-value")
      (cl-tmux::%cmd-set-option s '("-o" "@plugin-opt" "default-value"))
      (is (string= "user-value" (cl-tmux/options:get-option "@plugin-opt"))
          "-o must leave the existing value untouched")))

(test cmd-set-option-o-sets-when-unset
  "'set -o name value' DOES set the option when it has no value yet (seeds a
   default that a later plain set can still override)."
  (with-option-session (s)
      ;; Ensure no prior override exists.
      (remhash "@plugin-opt" cl-tmux/options:*global-options*)
      (cl-tmux::%cmd-set-option s '("-o" "@plugin-opt" "default-value"))
      (is (string= "default-value" (cl-tmux/options:get-option "@plugin-opt"))
          "-o must set the option when it is currently unset")))

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
  "'set -ga name val' (clustered -g -a) APPENDS — regression: the cluster was
   parsed as -g only, silently dropping -a and overwriting instead of appending."
  (with-option-session (s)
      (cl-tmux/options:set-option "@opt" "A")
      (cl-tmux::%run-command-line s "set -ga @opt B")
      (is (string= "AB" (cl-tmux/options:get-option "@opt"))
          "set -ga must append B to A, yielding AB")))

(test cmd-set-option-F-expands-format-value
  "'set -gF name #{...}' expands the format value once at set time."
  (with-option-session (s)                      ; session name is "0"
    (with-loop-state
      (cl-tmux::%run-command-line s "set -gF @opt #{session_name}")
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
  "setw -t 1 @wopt myval sets the WINDOW-LOCAL option on window-id 1, not the
   active window — and -t no longer leaks into the option name."
  (with-fake-session (s :nwindows 2)
    (let ((w0 (first  (session-windows s)))    ; id 0, active
          (w1 (second (session-windows s))))   ; id 1
      (cl-tmux::%run-command-line s "setw -t 1 @wopt myval")
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
        (is (overlay-active-p)
            "respawn-pane without -k on a live pane must show an error overlay")
        (is (= 5 (cl-tmux/model:pane-fd pane))
            "the live pane must NOT be respawned (fd unchanged → no fork)")))))

(test cmd-respawn-window-without-k-errors-on-live-pane
  "respawn-window without -k errors when ANY pane in the window is still running,
   and does NOT respawn — matching tmux."
  (with-fake-session (s :nwindows 1 :npanes 2)
    (let* ((win (session-active-window s))
           (p1  (first (window-panes win))))
      (setf (cl-tmux/model:pane-fd p1) 5)         ; one pane is live
      (let ((*overlay* nil))
        (cl-tmux::%cmd-respawn-window-arg s '())
        (is (overlay-active-p)
            "respawn-window without -k with a live pane must show an error overlay")
        (is (= 5 (cl-tmux/model:pane-fd p1))
            "the window must NOT be respawned (live pane fd unchanged → no fork)")))))

(test cmd-respawn-pane-rejects-unimplemented-overrides
  "respawn-pane rejects start-dir/env/command overrides that cl-tmux does not implement."
  (dolist (args '(("-c" "/tmp")
                  ("-e" "NAME=value")
                  ("echo" "ignored")))
    (with-fake-session (s :nwindows 1 :npanes 1)
      (let* ((pane (window-active-pane (session-active-window s)))
             (fd (cl-tmux/model:pane-fd pane))
             (pid (cl-tmux/model:pane-pid pane))
             (cl-tmux::*dirty* nil)
             (*overlay* nil))
        (is (null (cl-tmux::%cmd-respawn-pane-arg s args))
            "~S must be rejected instead of accepted as a no-op override" args)
        (is (search "unsupported argument" *overlay*)
            "~S must explain that the argument is unsupported" args)
        (is (eql fd (cl-tmux/model:pane-fd pane))
            "~S must leave the pane fd unchanged" args)
        (is (eql pid (cl-tmux/model:pane-pid pane))
            "~S must leave the pane pid unchanged" args)
        (is-false cl-tmux::*dirty*
                  "~S must not mark the model dirty after rejection" args)))))

(test cmd-respawn-window-rejects-unimplemented-overrides
  "respawn-window rejects start-dir/env/command overrides that cl-tmux does not implement."
  (dolist (args '(("-c" "/tmp")
                  ("-e" "NAME=value")
                  ("echo" "ignored")))
    (with-fake-session (s :nwindows 1 :npanes 2)
      (let* ((win (session-active-window s))
             (pane-states (mapcar (lambda (pane)
                                    (list pane
                                          (cl-tmux/model:pane-fd pane)
                                          (cl-tmux/model:pane-pid pane)))
                                  (window-panes win)))
             (cl-tmux::*dirty* nil)
             (*overlay* nil))
        (is (null (cl-tmux::%cmd-respawn-window-arg s args))
            "~S must be rejected instead of accepted as a no-op override" args)
        (is (search "unsupported argument" *overlay*)
            "~S must explain that the argument is unsupported" args)
        (dolist (state pane-states)
          (destructuring-bind (pane fd pid) state
            (is (eql fd (cl-tmux/model:pane-fd pane))
                "~S must leave pane fd unchanged" args)
            (is (eql pid (cl-tmux/model:pane-pid pane))
                "~S must leave pane pid unchanged" args)))
        (is-false cl-tmux::*dirty*
                  "~S must not mark the model dirty after rejection" args)))))

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
  (with-fake-session (s :nwindows 1 :npanes 2)
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
      (with-fake-session (s :nwindows 1 :npanes 2)
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

(test cmd-list-commands-filters-by-name
  "list-commands <name> shows only that command (tmux's filter); bare
   list-commands shows the full list."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line s "list-commands new-window")
      (assert-overlay-contains "new-window" *overlay*
                               "list-commands new-window")
      (assert-overlay-not-contains "kill-pane" *overlay*
                                   "list-commands new-window"))))

(test cmd-list-commands-uses-public-tmux-command-names
  "list-commands lists tmux public commands, not cl-tmux's internal bindable
   helper command names."
  (let ((names (cl-tmux::%list-command-public-names)))
    (is (equal "attach-session" (first names)))
    (is (member "list-commands" names :test #'string=))
    (is (member "set-buffer" names :test #'string=))
    (is (member "set-option" names :test #'string=))
    (is (member "set-window-option" names :test #'string=))
    (is (member "show-options" names :test #'string=))
    (is (member "wait-for" names :test #'string=))
    (is (null (member "set" names :test #'string=))
        "alias-only set spelling must not be exposed as a public command")
    (is (null (member "setw" names :test #'string=))
        "alias-only setw spelling must not be exposed as a public command")
    (is (null (member "copy-mode-enter" names :test #'string=))
        "copy-mode helper commands must not be exposed as public commands")
    (is (null (member "split-horizontal" names :test #'string=))
        "binding aliases must not be exposed as public command names")))

(test cmd-list-commands-format-command-list-name
  "list-commands -F expands #{command_list_name} for each listed command."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line
       s "list-commands -F '#{command_list_name}' list-sessions")
      (assert-overlay-contains "list-sessions" *overlay*
                               "formatted list-commands")
      (assert-overlay-not-contains "#{command_list_name}" *overlay*
                                   "formatted list-commands"))))

(test cmd-list-commands-unsupported-arguments-are-rejected-before-output
  "list-commands rejects unknown flags and extra filters instead of ignoring them."
  (with-fake-session (s)
    (dolist (line '("list-commands -Z new-window"
                    "list-commands new-window kill-pane"))
      (let ((*overlay* nil))
        (cl-tmux::%run-command-line s line)
        (assert-overlay-contains "unsupported argument" *overlay*
                                 "list-commands")
        (assert-overlay-not-contains "new-window" *overlay*
                                     "list-commands")))))

(test run-command-line-rename-window-no-arg-opens-prompt
  "'rename-window' with no argument falls through to the prompt (name table)."
  (with-fake-session (s :nwindows 1)
    (let ((*prompt* nil))
      (cl-tmux::%run-command-line s "rename-window")
      (is (prompt-active-p)
          "no-arg rename-window must open the rename prompt"))))
