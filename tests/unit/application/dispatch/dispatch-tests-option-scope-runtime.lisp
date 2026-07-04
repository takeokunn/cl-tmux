(in-package #:cl-tmux/test)

;;;; Dispatch option command tests: scope routing and runtime side effects.

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

(test cmd-set-option-accepts-terminal-matching-options
  "Runtime set-option ACCEPTS terminal-overrides/terminal-features like real
   tmux (they appear in virtually every real .tmux.conf); cl-tmux stores them
   even though it applies no terminal-matching behavior."
  (dolist (args '(("-g" "terminal-overrides" "xterm*:RGB")
                  ("-g" "terminal-features" "xterm*:RGB")))
    (with-option-session (s)
      (let ((*overlay* nil))
        (destructuring-bind (_ name value) args
          (declare (ignore _))
          (cl-tmux::%cmd-set-option s args)
          (is (null (and *overlay* (search "unsupported option" *overlay*)))
              "~A must not be rejected" name)
          (is (equal value (cl-tmux/options:get-option name nil))
              "~A must be stored like any other option" name))))))

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

