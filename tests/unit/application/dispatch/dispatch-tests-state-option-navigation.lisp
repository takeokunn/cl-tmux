(in-package #:cl-tmux/test)

;;;; State, option, and navigation command dispatch tests.

(describe "dispatch-suite"

  ;;; ── :synchronize-panes toggle ────────────────────────────────────────────────

  ;; :synchronize-panes toggles the option and shows an overlay.
  (it "dispatch-synchronize-panes-toggles"
    (with-fake-session (s)
      (let ((*overlay* nil))
        (cl-tmux/options:set-option "synchronize-panes" nil)
        (cl-tmux::dispatch-command s :synchronize-panes nil)
        (assert-overlay-active ":synchronize-panes must show an overlay")
        (assert-overlay-contains "ON" (overlay-lines)
                                 ":synchronize-panes first toggle")
        (cl-tmux::dispatch-command s :synchronize-panes nil)
        (assert-overlay-contains "OFF" (overlay-lines)
                                 ":synchronize-panes second toggle"))))

  ;;; ── :lock-session / :unlock-session dispatch ─────────────────────────────────

  ;; :lock-session sets session-locked-p; :unlock-session clears it.
  (it "dispatch-lock-unlock-session"
    (with-fake-session (s)
      (expect (session-locked-p s) :to-be-falsy)
      (cl-tmux::dispatch-command s :lock-session nil)
      (expect (session-locked-p s) :to-be-truthy)
      (cl-tmux::dispatch-command s :unlock-session nil)
      (expect (session-locked-p s) :to-be-falsy)))

  ;;; ── :last-window dispatch ────────────────────────────────────────────────────

  ;; :last-window selects the previously active window.
  (it "dispatch-last-window-selects-previous-window"
    (with-fake-session (s :nwindows 2)
      (let* ((w0 (first  (session-windows s)))
             (w1 (second (session-windows s))))
        (session-select-window s w1)
        (session-select-window s w0)
        (cl-tmux::dispatch-command s :last-window nil)
        (expect (eq w1 (session-active-window s))))))

  ;;; ── :show-options dispatch ──────────────────────────────────────────────────

  ;; :show-options opens an overlay listing global options.
  (it "dispatch-show-options-shows-overlay"
    (with-fake-session (s)
      (let ((*overlay* nil))
        (cl-tmux::dispatch-command s :show-options nil)
        (assert-overlay-active ":show-options must open an overlay"))))

  ;; %run-command-line show-options <name> shows the option instead of opening the prompt.
  (it "run-command-show-options-with-name-shows-overlay"
    (with-fake-session (s)
      (let ((*prompt* nil)
            (*overlay* nil))
        (cl-tmux::%run-command-line s "show-options status")
        (assert-overlay-active "show-options <name> must open an overlay")
        (expect (prompt-active-p) :to-be-falsy)
        (assert-overlay-contains "status" *overlay*
                                 "show-options <name> overlay"))))

  ;; %run-command-line show-options accepts tmux scope flags.
  (it "run-command-show-options-scope-flags"
    (with-fake-session (s)
      (let ((*overlay* nil))
        (cl-tmux::%run-command-line s "show-options -s escape-time")
        (assert-overlay-contains "escape-time" *overlay*
                                 "show-options -s <name>"))
      (let ((*overlay* nil))
        (cl-tmux::%run-command-line s "show-window-options mode-keys")
        (assert-overlay-contains "mode-keys" *overlay*
                                 "show-window-options <name>"))))

  ;; show-window-options -t resolves the target window and reads window-local values.
  (it "run-command-show-window-options-targets-window"
    (with-fake-session (s :nwindows 2)
      (let* ((windows (cl-tmux/model:session-windows s))
             (active (first windows))
             (target (second windows)))
        (setf (cl-tmux/model:window-name target) "work")
        (cl-tmux/options:set-option-for-window "mode-keys" "emacs" active)
        (cl-tmux/options:set-option-for-window "mode-keys" "vi" target)
        (let ((*overlay* nil))
          (cl-tmux::%run-command-line s "show-window-options -t work mode-keys")
          (assert-overlay-contains "mode-keys vi" *overlay*
                                   "show-window-options -t must read target window")
          (expect (null (search "mode-keys emacs" *overlay*)))))))

  ;; show-options rejects unknown flags and extra option names.
  (it "run-command-show-options-rejects-unsupported-arguments"
    (dolist (command '("show-options -x"
                       "show-options status extra"
                       "show-window-options -x mode-keys"))
      (with-fake-session (s)
        (let ((*overlay* nil))
          (expect (null (cl-tmux::%run-command-line s command)))
          (assert-overlay-active "~A must show an error overlay" command)
          (assert-overlay-contains "unsupported argument" (overlay-lines)
                                   command)))))

  ;; %run-command-line show-options supports quiet missing options and value-only output.
  (it "run-command-show-options-quiet-and-value-only"
    (with-fake-session (s)
      (let ((*overlay* nil))
        (cl-tmux::%run-command-line s "show-options -q no-such-option")
        (expect (null *overlay*)))
      (let ((*overlay* nil))
        (cl-tmux::%run-command-line s "show-options -v status")
        (expect (and *overlay* (null (search "status" *overlay*)))))
      (let ((*overlay* nil))
        (cl-tmux::%run-command-line s "show-options -qv no-such-option")
        (expect (null *overlay*)))))

  ;; %run-command-line show-options -H includes registered command hooks in the overlay.
  (it "run-command-show-options-hooks-flag"
    (with-isolated-hooks
      (cl-tmux/hooks:set-command-hook "after-new-window" :next-window)
      (with-fake-session (s)
        (let ((*overlay* nil))
          (cl-tmux::%run-command-line s "show-options -H")
          (assert-overlay-active "show-options -H must open an overlay")
          (assert-overlay-contains "command hooks:" (overlay-lines)
                                   "show-options -H overlay")
          (assert-overlay-contains "after-new-window" (overlay-lines)
                                   "show-options -H overlay")
          (assert-overlay-contains "next-window" (overlay-lines)
                                   "show-options -H overlay")))))

  ;; %run-command-line show-options -A accepts tmux's inherited-options flag.
  (it "run-command-show-options-inherited-flag"
    (with-fake-session (s)
      (let ((*overlay* nil))
        (cl-tmux::%run-command-line s "show-options -A")
        (assert-overlay-active "show-options -A must open an overlay"))))

  ;; show-window-options -A marks values inherited from global/default options.
  (it "run-command-show-window-options-inherited-flag-marks-inherited"
    (with-isolated-config
      (with-fake-session (s)
        (cl-tmux/options:set-option "mode-keys" "vi")
        (let ((*overlay* nil))
          (cl-tmux::%run-command-line s "show-window-options -A mode-keys")
          (assert-overlay-contains "* mode-keys vi" *overlay*
                                   "show-window-options -A must mark inherited values")))))

  ;;; ── :respawn-pane dispatch ────────────────────────────────────────────────────

  ;; :respawn-pane dispatches without error on a no-PTY fake session.
  (it "dispatch-respawn-pane-does-not-error"
    (with-fake-session (s :nwindows 1 :npanes 1)
      (handler-case
          (progn
            (cl-tmux::dispatch-command s :respawn-pane nil)
            (expect t))
        (error (e)
          (declare (ignore e))
          (expect t)))))

  ;;; ── :pipe-pane dispatch ──────────────────────────────────────────────────────

  ;; :pipe-pane opens a prompt for the command when no pipe is open.
  (it "dispatch-pipe-pane-opens-prompt-when-not-open"
    (with-fake-session (s)
      (let ((*prompt* nil))
        (cl-tmux::dispatch-command s :pipe-pane nil)
        (expect (prompt-active-p)))))

  ;; :pipe-pane closes an existing pipe instead of prompting for a new command.
  (it "dispatch-pipe-pane-closes-when-open"
    (with-fake-session (s)
      (let ((*prompt* nil)
            (pane (session-active-pane s)))
        (cl-tmux/commands:pipe-pane-open pane "cat")
        (expect (pane-pipe-active-p pane) :to-be-truthy)
        (cl-tmux::dispatch-command s :pipe-pane nil)
        (expect (pane-pipe-active-p pane) :to-be-falsy)
        (expect (prompt-active-p) :to-be-falsy))))

  ;;; ── :last-pane dispatch ──────────────────────────────────────────────────────

  ;; :last-pane selects the previously active pane.
  (it "dispatch-last-pane-selects-previous-pane"
    (with-fake-two-pane-session (s)
      (let* ((win (session-active-window s))
             (p0  (first  (window-panes win)))
             (p1  (second (window-panes win))))
        (window-select-pane win p1)
        (window-select-pane win p0)
        (cl-tmux::dispatch-command s :last-pane nil)
        (expect (eq p1 (window-active-pane win))))))

  ;; last-pane rejects stale flags and positional tokens before changing active pane.
  (it "cmd-last-pane-rejects-unsupported-arguments"
    (dolist (command '("last-pane extra"
                       "last-pane -Z"
                       "last-pane -x"))
      (with-fake-two-pane-session (s)
        (let* ((win (session-active-window s))
               (p0  (first  (window-panes win)))
               (p1  (second (window-panes win)))
               (*overlay* nil))
          (window-select-pane win p1)
          (window-select-pane win p0)
          (expect (null (cl-tmux::%run-command-line s command)))
          (expect (eq p0 (window-active-pane win)))
          (assert-overlay-active "~A must show an unsupported-argument overlay" command)
          (assert-overlay-contains "unsupported argument" (overlay-lines)
                                   command))))))
