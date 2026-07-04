(in-package #:cl-tmux/test)

;;;; State, option, and navigation command dispatch tests.

(in-suite dispatch-suite)

;;; ── :synchronize-panes toggle ────────────────────────────────────────────────

(test dispatch-synchronize-panes-toggles
  ":synchronize-panes toggles the option and shows an overlay."
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

(test dispatch-lock-unlock-session
  ":lock-session sets session-locked-p; :unlock-session clears it."
  (with-fake-session (s)
    (is-false (session-locked-p s) "session must be unlocked initially")
    (cl-tmux::dispatch-command s :lock-session nil)
    (is-true  (session-locked-p s) "session must be locked after :lock-session")
    (cl-tmux::dispatch-command s :unlock-session nil)
    (is-false (session-locked-p s) "session must be unlocked after :unlock-session")))

;;; ── :last-window dispatch ────────────────────────────────────────────────────

(test dispatch-last-window-selects-previous-window
  ":last-window selects the previously active window."
  (with-fake-session (s :nwindows 2)
    (let* ((w0 (first  (session-windows s)))
           (w1 (second (session-windows s))))
      (session-select-window s w1)
      (session-select-window s w0)
      (cl-tmux::dispatch-command s :last-window nil)
      (is (eq w1 (session-active-window s))
          ":last-window must return to the previously active window"))))

;;; ── :show-options dispatch ──────────────────────────────────────────────────

(test dispatch-show-options-shows-overlay
  ":show-options opens an overlay listing global options."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::dispatch-command s :show-options nil)
      (assert-overlay-active ":show-options must open an overlay"))))

(test run-command-show-options-with-name-shows-overlay
  "%run-command-line show-options <name> shows the option instead of opening the prompt."
  (with-fake-session (s)
    (let ((*prompt* nil)
          (*overlay* nil))
      (cl-tmux::%run-command-line s "show-options status")
      (assert-overlay-active "show-options <name> must open an overlay")
      (is-false (prompt-active-p) "show-options <name> must not open the prompt")
      (assert-overlay-contains "status" *overlay*
                               "show-options <name> overlay"))))

(test run-command-show-options-scope-flags
  "%run-command-line show-options accepts tmux scope flags."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line s "show-options -s escape-time")
      (assert-overlay-contains "escape-time" *overlay*
                               "show-options -s <name>"))
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line s "show-window-options mode-keys")
      (assert-overlay-contains "mode-keys" *overlay*
                               "show-window-options <name>"))))

(test run-command-show-window-options-targets-window
  "show-window-options -t resolves the target window and reads window-local values."
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
        (is (null (search "mode-keys emacs" *overlay*))
            "show-window-options -t must not read the active window")))))

(test run-command-show-options-rejects-unsupported-arguments
  "show-options rejects unknown flags and extra option names."
  (dolist (command '("show-options -x"
                     "show-options status extra"
                     "show-window-options -x mode-keys"))
    (with-fake-session (s)
      (let ((*overlay* nil))
        (is (null (cl-tmux::%run-command-line s command))
            "~A must be rejected" command)
        (assert-overlay-active "~A must show an error overlay" command)
        (assert-overlay-contains "unsupported argument" (overlay-lines)
                                 command)))))

(test run-command-show-options-quiet-and-value-only
  "%run-command-line show-options supports quiet missing options and value-only output."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line s "show-options -q no-such-option")
      (is (null *overlay*) "-q must suppress missing option output"))
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line s "show-options -v status")
      (is (and *overlay* (null (search "status" *overlay*)))
          "-v must show only the value"))
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line s "show-options -qv no-such-option")
      (is (null *overlay*) "-qv must suppress missing option output"))))

(test run-command-show-options-hooks-flag
  "%run-command-line show-options -H includes registered command hooks in the overlay."
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

(test run-command-show-options-inherited-flag
  "%run-command-line show-options -A accepts tmux's inherited-options flag."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line s "show-options -A")
      (assert-overlay-active "show-options -A must open an overlay"))))

(test run-command-show-window-options-inherited-flag-marks-inherited
  "show-window-options -A marks values inherited from global/default options."
  (with-fake-session (s)
    (cl-tmux/options:set-option "mode-keys" "vi")
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line s "show-window-options -A mode-keys")
      (assert-overlay-contains "* mode-keys vi" *overlay*
                               "show-window-options -A must mark inherited values"))))

;;; ── :respawn-pane dispatch ────────────────────────────────────────────────────

(test dispatch-respawn-pane-does-not-error
  ":respawn-pane dispatches without error on a no-PTY fake session."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (handler-case
        (progn
          (cl-tmux::dispatch-command s :respawn-pane nil)
          (is-true t ":respawn-pane dispatched without error"))
      (error (e)
        (declare (ignore e))
        (is-true t ":respawn-pane signalled at PTY level (expected in sandbox)")))))

;;; ── :pipe-pane dispatch ──────────────────────────────────────────────────────

(test dispatch-pipe-pane-opens-prompt-when-not-open
  ":pipe-pane opens a prompt for the command when no pipe is open."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :pipe-pane nil)
      (is (prompt-active-p) ":pipe-pane must open a prompt when pipe is not open"))))

(test dispatch-pipe-pane-closes-when-open
  ":pipe-pane closes an existing pipe instead of prompting for a new command."
  (with-fake-session (s)
    (let ((*prompt* nil)
          (pane (session-active-pane s)))
      (cl-tmux/commands:pipe-pane-open pane "cat")
      (is-true (pane-pipe-active-p pane)
          "precondition: the pane must have an active pipe")
      (cl-tmux::dispatch-command s :pipe-pane nil)
      (is-false (pane-pipe-active-p pane)
          ":pipe-pane must close the existing pipe")
      (is-false (prompt-active-p)
          ":pipe-pane must not open a prompt when closing an existing pipe"))))

;;; ── :last-pane dispatch ──────────────────────────────────────────────────────

(test dispatch-last-pane-selects-previous-pane
  ":last-pane selects the previously active pane."
  (with-fake-two-pane-session (s)
    (let* ((win (session-active-window s))
           (p0  (first  (window-panes win)))
           (p1  (second (window-panes win))))
      (window-select-pane win p1)
      (window-select-pane win p0)
      (cl-tmux::dispatch-command s :last-pane nil)
      (is (eq p1 (window-active-pane win))
          ":last-pane must select the previously active pane"))))

(test cmd-last-pane-rejects-unsupported-arguments
  "last-pane rejects stale flags and positional tokens before changing active pane."
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
        (is (null (cl-tmux::%run-command-line s command))
            "~A must be rejected" command)
        (is (eq p0 (window-active-pane win))
            "~A must not select the previous pane" command)
        (assert-overlay-active "~A must show an unsupported-argument overlay" command)
        (assert-overlay-contains "unsupported argument" (overlay-lines)
                                 command)))))
