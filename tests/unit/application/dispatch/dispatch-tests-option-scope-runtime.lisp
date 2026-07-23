(in-package #:cl-tmux/test)

;;;; Dispatch option command tests: scope routing and runtime side effects.

(describe "dispatch-suite"

  ;;; ── %cmd-set-option scope routing: -w / -p / global ──────────────────────

  ;; 'set -w' routes to the active window's local options and leaves the global
  ;; value unchanged.
  (it "cmd-set-option-w-routes-to-window-local"
    (with-option-session (s)
      (let* ((win (cl-tmux/model:session-active-window s)))
        (cl-tmux/options:set-option "synchronize-panes" nil)
        (cl-tmux::%cmd-set-option s '("-w" "synchronize-panes" "on"))
        (expect (eq t (cl-tmux/options:get-option-for-window "synchronize-panes" win)))
        (expect (null (cl-tmux/options:get-option "synchronize-panes"))))))

  ;; 'set -o -w name value' consults the WINDOW-LOCAL store, not the global table:
  ;; a global value must NOT block a window-local -o set (audit #4 — the
  ;; only-if-unset check used to always read *global-options*).
  (it "cmd-set-option-o-w-checks-window-local-not-global"
    (with-option-session (s)
      (let ((win (cl-tmux/model:session-active-window s)))
        (remhash "@plugin-opt" (cl-tmux/model:window-local-options win))
        (cl-tmux/options:set-option "@plugin-opt" "global-value")
        (cl-tmux::%cmd-set-option s '("-o" "-w" "@plugin-opt" "win-value"))
        (expect (string= "win-value"
                     (nth-value 0 (gethash "@plugin-opt"
                                           (cl-tmux/model:window-local-options win)))))
        (expect (string= "global-value" (cl-tmux/options:get-option "@plugin-opt"))))))

  ;; 'set -o -w name value' is a no-op when the WINDOW already has a local override.
  (it "cmd-set-option-o-w-skips-when-window-local-already-set"
    (with-option-session (s)
      (let ((win (cl-tmux/model:session-active-window s)))
        (cl-tmux/options:set-option-for-window "@plugin-opt" "win-user-value" win)
        (cl-tmux::%cmd-set-option s '("-o" "-w" "@plugin-opt" "default-value"))
        (expect (string= "win-user-value"
                     (nth-value 0 (gethash "@plugin-opt"
                                           (cl-tmux/model:window-local-options win))))))))

  ;; set-window-option (no -g) sets a WINDOW-local option, not global —
  ;; tmux's set-window-option is `set -w`.
  (it "cmd-set-window-option-defaults-to-window-scope"
    (with-option-session (s)
      (let* ((win (cl-tmux/model:session-active-window s)))
        (cl-tmux/options:set-option "synchronize-panes" nil)
        (cl-tmux::%cmd-set-window-option s '("synchronize-panes" "on"))
        (expect (eq t (cl-tmux/options:get-option-for-window "synchronize-panes" win)))
        (expect (null (cl-tmux/options:get-option "synchronize-panes"))))))

  ;; set-window-option -g sets the GLOBAL option (explicit -g wins over the injected -w).
  (it "cmd-set-window-option-g-overrides-to-global"
    (with-option-session (s)
      (let* ((win (cl-tmux/model:session-active-window s)))
        (cl-tmux/options:set-option "synchronize-panes" nil)
        (cl-tmux::%cmd-set-window-option s '("-g" "synchronize-panes" "on"))
        (expect (eq t (cl-tmux/options:get-option "synchronize-panes")))
        (expect (null (nth-value 1 (gethash "synchronize-panes"
                                        (cl-tmux/model:window-local-options win))))))))

  ;; 'set -p' routes to the active pane's local options and leaves the global
  ;; value unchanged.
  (it "cmd-set-option-p-routes-to-pane-local"
    (with-option-session (s)
      (let* ((pane (cl-tmux/model:session-active-pane s)))
        (cl-tmux/options:set-option "remain-on-exit" nil)
        (cl-tmux::%cmd-set-option s '("-p" "remain-on-exit" "on"))
        (expect (eq t (cl-tmux/options:get-option-for-pane "remain-on-exit" pane)))
        (expect (null (cl-tmux/options:get-option "remain-on-exit"))))))

  ;; 'set -p -u' removes the pane-local override and leaves the global value intact.
  (it "cmd-set-option-p-unset-clears-pane-local-not-global"
    (with-option-session (s)
      (let* ((pane (cl-tmux/model:session-active-pane s)))
        (cl-tmux/options:set-option "remain-on-exit" t)
        (cl-tmux/options:set-option-for-pane "remain-on-exit" "on" pane)
        (cl-tmux::%cmd-set-option s '("-p" "-u" "remain-on-exit"))
        (expect (not (nth-value 1 (gethash "remain-on-exit"
                                       (cl-tmux/model:pane-local-options pane)))))
        (expect (eq t (cl-tmux/options:get-option "remain-on-exit"))))))

  ;; Runtime `set-option -g status off/on` runs the option side-effect, updating
  ;; *status-height* — not only the .tmux.conf path.
  (it "cmd-set-option-status-applies-side-effect-at-runtime"
    (with-option-session (s)
        (cl-tmux::%cmd-set-option s '("-g" "status" "off"))
        (expect (= 0 cl-tmux/config:*status-height*))
        (cl-tmux::%cmd-set-option s '("-g" "status" "on"))
        (expect (= 1 cl-tmux/config:*status-height*))))

  ;; Runtime `set-option -g default-shell` updates *default-shell* via the side-effect.
  (it "cmd-set-option-default-shell-applies-side-effect-at-runtime"
    (with-option-session (s)
        (cl-tmux::%cmd-set-option s '("-g" "default-shell" "/bin/zsh"))
        (expect (string= "/bin/zsh" cl-tmux/config:*default-shell*))))

  ;; Runtime `set-option -g prefix C-a` rebinds the prefix key code via the side-effect.
  (it "cmd-set-option-prefix-applies-side-effect-at-runtime"
    (with-option-session (s)
        (cl-tmux::%cmd-set-option s '("-g" "prefix" "C-a"))
        (expect (= 1 cl-tmux/config:*prefix-key-code*))))

  ;; escape-time is read from the server store by the ESC-flush, but is commonly
  ;; set via `set-option -g escape-time 0` (global).  The side-effect syncs it across, so
  ;; the common form takes effect.
  (it "cmd-set-option-escape-time-syncs-to-server-store"
    (with-option-session (s)
        ;; -g writes the global store; the side-effect mirrors it into the server store.
        (cl-tmux::%cmd-set-option s '("-g" "escape-time" "0"))
        (expect (= 0 (cl-tmux/options:get-server-option "escape-time")))
        ;; bare set (no scope) also syncs.
        (cl-tmux::%cmd-set-option s '("escape-time" "10"))
        (expect (= 10 (cl-tmux/options:get-server-option "escape-time")))))

  ;; `set-option -u` must reset special-option runtime state back to defaults,
  ;; not just remove the stored option value.
  (it "cmd-set-option-unset-resets-runtime-side-effects"
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
        (expect (= 1 cl-tmux/config:*status-height*))
        (expect (string= "/bin/sh" cl-tmux/config:*default-shell*))
        (expect (= cl-tmux/config:+prefix-key-code+ cl-tmux/config:*prefix-key-code*))
        (expect (null cl-tmux/config:*prefix2-key-code*))
        (expect (equal cl-tmux/model:+default-update-environment+
                   cl-tmux/model:*update-environment*))
        (expect (= 10 (cl-tmux/options:get-server-option "escape-time"))))))

  ;; A plain 'set-option' (no scope flag) of a SESSION-scoped option sets the global
  ;; store.  WINDOW-scoped names route to the active window instead (audit #7).
  (it "cmd-set-option-plain-routes-to-global"
    (with-option-session (s)
        (cl-tmux::%cmd-set-option s '("history-limit" "5000"))
        (expect (= 5000 (cl-tmux/options:get-option "history-limit")))))

  ;; A plain 'set-option' of a WINDOW-scoped option name (no flag) routes to the
  ;; active window's local store, mirroring tmux options_scope_from_name (audit #7).
  (it "cmd-set-option-plain-window-option-routes-to-window"
    (with-option-session (s)
      (let ((win (cl-tmux/model:session-active-window s)))
        (cl-tmux::%cmd-set-option s '("synchronize-panes" "on"))
        (expect (eq t (nth-value 0 (gethash "synchronize-panes"
                                        (cl-tmux/model:window-local-options win)))))
        (expect (null (cl-tmux/options:get-option "synchronize-panes"))))))

  ;; 'set -g -w' must set the GLOBAL option (the explicit -g overrides -w) and
  ;; leave the active window WITHOUT a local override.  Guards the (not globalp)
  ;; gate in %cmd-set-option.
  (it "cmd-set-option-gw-stays-global"
    (with-option-session (s)
      (let* ((win (cl-tmux/model:session-active-window s)))
        (cl-tmux/options:set-option "synchronize-panes" nil)
        (cl-tmux::%cmd-set-option s '("-g" "-w" "synchronize-panes" "on"))
        (expect (eq t (cl-tmux/options:get-option "synchronize-panes")))
        (expect (null (nth-value 1 (gethash "synchronize-panes"
                                        (cl-tmux/model:window-local-options win)))))
        (expect (eq t (cl-tmux/options:get-option-for-window "synchronize-panes" win))))))

  ;; Runtime set-option ACCEPTS terminal-overrides/terminal-features like real
  ;; tmux (they appear in virtually every real .tmux.conf); cl-tmux stores them
  ;; even though it applies no terminal-matching behavior.
  (it "cmd-set-option-accepts-terminal-matching-options"
    (dolist (args '(("-g" "terminal-overrides" "xterm*:RGB")
                    ("-g" "terminal-features" "xterm*:RGB")))
      (with-option-session (s)
        (let ((*overlay* nil))
          (destructuring-bind (_ name value) args
            (declare (ignore _))
            (cl-tmux::%cmd-set-option s args)
            (expect (null (and *overlay* (search "unsupported option" *overlay*))))
            (expect (equal value (cl-tmux/options:get-option name nil))))))))

  ;; set -o only sets the option when no value exists: skips if already set,
  ;; writes default if absent.
  ;; Each row: (pre-value expected description).
  (it "cmd-set-option-o-flag-variants"
    (dolist (row '(("user-value" "user-value"    "-o must leave existing value untouched")
                   (nil          "default-value" "-o must set the option when currently unset")))
      (destructuring-bind (pre-value expected desc) row
        (declare (ignore desc))
        (with-option-session (s)
          (if pre-value
              (cl-tmux/options:set-option "@plugin-opt" pre-value)
              (remhash "@plugin-opt" cl-tmux/options:*global-options*))
          (cl-tmux::%cmd-set-option s '("-o" "@plugin-opt" "default-value"))
          (expect (string= expected (cl-tmux/options:get-option "@plugin-opt")))))))

  ;; set -o on an already-set option reports tmux's 'already set: NAME' error;
  ;; -q suppresses it.  Each row: (args expect-error-p description).
  (it "cmd-set-option-o-already-set-reports-tmux-error"
    (dolist (row '((("-o" "@plugin-opt" "v2")      t   "-o must report already set")
                   (("-o" "-q" "@plugin-opt" "v2") nil "-o -q must stay silent")))
      (destructuring-bind (args expect-error-p desc) row
        (declare (ignore desc))
        (with-option-session (s)
          (let ((*overlay* nil))
            (cl-tmux/options:set-option "@plugin-opt" "v1")
            (cl-tmux::%cmd-set-option s args)
            (expect (string= "v1" (cl-tmux/options:get-option "@plugin-opt")))
            (if expect-error-p
                (expect (search "already set: @plugin-opt" *overlay*))
                (expect (null (and *overlay*
                               (search "already set" *overlay*)))))))))))
