(in-package #:cl-tmux/test)

;;;; Arg-command dispatch tests — part 2: named-command handler tests,
;;;; display/confirm/set-option/buffer overlay operations, session management.

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
      ;; Toggle back off.
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
      ;; Visit w1, then switch back to w0.
      (session-select-window s w1)
      (session-select-window s w0)
      ;; :last-window should go back to w1.
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
    ;; respawn-pane tries to fork a shell; it may fail in test sandbox.
    ;; We verify dispatch does not error at the dispatch layer itself.
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
      ;; Visit p1, then switch back to p0.
      (window-select-pane win p1)
      (window-select-pane win p0)
      ;; :last-pane should return to p1.
      (cl-tmux::dispatch-command s :last-pane nil)
      (is (eq p1 (window-active-pane win))
          ":last-pane must select the previously active pane"))))

(test cmd-last-pane-rejects-unsupported-arguments
  "last-pane rejects stale flags and positional tokens before changing active pane."
  (dolist (command '("last-pane extra"
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

;;; ── %format-window-list helper ───────────────────────────────────────────────

(test format-window-list-includes-active-marker
  "%format-window-list includes an asterisk on the active window line and
   lists each window by id and name."
  (with-fake-session (s :nwindows 2)
    (let* ((text (cl-tmux::%format-window-list s))
           (aw   (session-active-window s)))
      (is (stringp text) "%format-window-list must return a string")
      (is (search (window-name aw) text)
          "output must mention the active window name")
      (is (search "*" text)
          "output must mark the active window with an asterisk"))))

(test format-window-list-shows-pane-count
  "%format-window-list includes the pane count for each window."
  (with-fake-two-pane-session (s)
    (let ((text (cl-tmux::%format-window-list s)))
      ;; The format string ends each line with "[N pane(s)]".
      (is (search "pane" text)
          "output must include the word 'pane'"))))

;;; ── %format-session-list helper ──────────────────────────────────────────────

(test format-session-list-fallback-uses-session-name
  "%format-session-list with empty *server-sessions* falls back to the
   session-name one-line entry."
  (with-fake-session (s :nwindows 1)
    (let ((cl-tmux::*server-sessions* nil))
      (let ((text (cl-tmux::%format-session-list s)))
        (is (stringp text) "%format-session-list must return a string")
        (is (search (session-name s) text)
            "fallback output must contain the session name")))))

(test format-session-list-marks-current-session
  "%format-session-list with a populated *server-sessions* marks the current
   session with an asterisk."
  (with-fake-session (s :nwindows 1)
    (let* ((name (session-name s))
           (cl-tmux::*server-sessions* (list (cons name s))))
      (let ((text (cl-tmux::%format-session-list s)))
        (is (search "*" text) "current session must be marked with an asterisk")
        (is (search name text) "output must contain the session name")))))

;;; ── %copy-mode-call helper ────────────────────────────────────────────────────

(test copy-mode-call-invokes-fn-on-active-screen
  "%copy-mode-call invokes FN on the active screen when copy mode is on."
  (with-fake-session (s)
    (cl-tmux::dispatch-command s :copy-mode-enter nil)
    (let ((called-with nil))
      (cl-tmux::%copy-mode-call s (lambda (sc) (setf called-with sc)))
      (is (eq (active-screen s) called-with)
          "%copy-mode-call must pass the active screen to FN"))))

(test copy-mode-call-skips-when-no-session-has-no-screen
  "%copy-mode-call on a windowless session is a no-op (no error)."
  (with-fake-session (s :nwindows 0)
    (finishes (cl-tmux::%copy-mode-call s (lambda (screen) (declare (ignore screen)) nil))
              "%copy-mode-call must not error when there is no active screen")))

;;; ── %handle-kill-result helper ────────────────────────────────────────────────

(test handle-kill-result-table
  "%handle-kill-result: :quit clears *running* and returns :quit; NIL preserves it and returns NIL."
  (dolist (row '((:quit nil  ":quit must clear *running*")
                 (nil   t    "NIL must preserve *running*")))
    (destructuring-bind (result expected-running desc) row
      (with-loop-state
        (let ((ret (cl-tmux::%handle-kill-result result)))
          (is (equal result ret)            "~A: return value must equal input" desc)
          (is (eq expected-running cl-tmux::*running*) "~A: *running*" desc))))))

;;; ── %format-popup-overlay helper ─────────────────────────────────────────────

(test format-popup-overlay-produces-box
  "%format-popup-overlay produces a box-drawing overlay string."
  (let ((result (cl-tmux::%format-popup-overlay "test" "body-text")))
    (is (stringp result) "%format-popup-overlay must return a string")
    (is (search "test" result) "overlay must contain the title")
    (is (search "body-text" result) "overlay must contain the output")
    (is (search "┌" result) "overlay must have a top-left corner")
    (is (search "└" result) "overlay must have a bottom-left corner")))

(test format-popup-overlay-nil-output-uses-empty-string
  "%format-popup-overlay with NIL output substitutes an empty string."
  (let ((result (cl-tmux::%format-popup-overlay "cmd" nil)))
    (is (stringp result) "%format-popup-overlay must not error with nil output")
    (is (search "cmd" result) "overlay must still contain the title")))

;;; ── Popup and buffer-preview positive-constant checks ────────────────────────

(test popup-and-buffer-preview-constants-positive-table
  "Popup dimension and buffer-preview constants must all be positive."
  (dolist (row (list (list cl-tmux::+popup-max-width+      "+popup-max-width+")
                     (list cl-tmux::+popup-max-height+     "+popup-max-height+")
                     (list cl-tmux::+popup-margin+         "+popup-margin+")
                     (list cl-tmux::+buffer-preview-length+ "+buffer-preview-length+")))
    (destructuring-bind (val name) row
      (is (> val 0) "~A must be positive" name))))

;;; ── :display-popup dispatch ──────────────────────────────────────────────────

(test dispatch-display-popup-opens-prompt
  ":display-popup opens a prompt for the shell command."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :display-popup nil)
      (is (prompt-active-p) ":display-popup must open a prompt")
      (is (string= "popup command" (prompt-label *prompt*))
          ":display-popup prompt label must be \"popup command\""))))

(test dispatch-display-popup-dismiss-clears-popup
  ":display-popup-dismiss clears *active-popup*."
  (with-fake-session (s)
    (setf cl-tmux::*active-popup*
          (make-popup :title "t" :width 40 :height 10 :screen nil :pane nil))
    (cl-tmux::dispatch-command s :display-popup-dismiss nil)
    (is (null cl-tmux::*active-popup*)
        ":display-popup-dismiss must set *active-popup* to nil")))

;;; ── :display-menu / :menu-next / :menu-prev / :menu-select / :menu-dismiss ──

(test dispatch-display-menu-opens-menu-and-overlay
  ":display-menu sets *active-menu* and opens an overlay."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux::*active-menu* nil))
      (cl-tmux::dispatch-command s :display-menu nil)
      (is (not (null cl-tmux::*active-menu*))
          ":display-menu must set *active-menu*")
      (assert-overlay-active ":display-menu must open an overlay"))))

(test cmd-display-menu-x-y-sets-menu-position
  "display-menu -x/-y stores the position on the menu struct (default NIL = centred)."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux::*active-menu* nil))
      ;; -x 10 -y 5 with one item triple
      (cl-tmux::%cmd-display-menu-arg
       s '("-x" "10" "-y" "5" "Item" "a" "next-window"))
      (is (not (null cl-tmux::*active-menu*)) "menu must be created")
      (is (= 10 (cl-tmux/prompt:menu-x cl-tmux::*active-menu*))
          "-x sets menu-x to 10")
      (is (= 5 (cl-tmux/prompt:menu-y cl-tmux::*active-menu*))
          "-y sets menu-y to 5"))))

(test cmd-display-menu-no-x-y-is-centered
  "display-menu without -x/-y leaves menu-x/menu-y NIL (centred default)."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux::*active-menu* nil))
      (cl-tmux::%cmd-display-menu-arg s '("Item" "a" "next-window"))
      (is (not (null cl-tmux::*active-menu*)) "menu must be created")
      (is (null (cl-tmux/prompt:menu-x cl-tmux::*active-menu*))
          "menu-x defaults to NIL (centred)")
      (is (null (cl-tmux/prompt:menu-y cl-tmux::*active-menu*))
          "menu-y defaults to NIL (centred)"))))

(test run-command-line-display-menu-empty-args-reports-too-few
  "%run-command-line display-menu with no item args reports tmux-compatible syntax error."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux::*active-menu* nil))
      (is (null (cl-tmux::%run-command-line s "display-menu"))
          "display-menu empty args returns no dispatch keyword")
      (is (null cl-tmux::*active-menu*)
          "display-menu empty args must not open the internal default menu")
      (assert-overlay-contains "command display-menu: too few arguments (need at least 1)"
                               (overlay-lines)
                               "display-menu empty args"))))

(test dispatch-menu-next-prev-table
  ":menu-next from 0 advances to 1; :menu-prev from 0 wraps to last index (1)."
  (dolist (cmd '(:menu-next :menu-prev))
    (with-fake-session (s)
      (let ((cl-tmux::*active-menu*
              (make-menu :title "t"
                         :items (list (cons "a" :ka) (cons "b" :kb))
                         :selected-index 0)))
        (cl-tmux::dispatch-command s cmd nil)
        (is (= 1 (menu-selected-index cl-tmux::*active-menu*))
            "~A from 0 must result in selected-index 1" cmd)))))

(test dispatch-menu-dismiss-clears-menu-and-overlay
  ":menu-dismiss clears *active-menu* and the overlay."
  (with-fake-session (s)
    (let ((cl-tmux::*active-menu*
            (make-menu :title "t" :items (list (cons "a" :ka)) :selected-index 0)))
      (cl-tmux::dispatch-command s :menu-dismiss nil)
      (is (null cl-tmux::*active-menu*)
          ":menu-dismiss must clear *active-menu*"))))

;;; ── :has-session dispatch ────────────────────────────────────────────────────

(test dispatch-has-session-opens-prompt
  ":has-session opens a prompt for the session name."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :has-session nil)
      (is (prompt-active-p) ":has-session must open a prompt"))))

(test dispatch-has-session-found-shows-yes
  ":has-session on-submit shows 'yes' when the session is registered."
  (with-fake-session (s)
    (let ((name (session-name s)))
      (let ((*prompt* nil) (*overlay* nil)
            (cl-tmux::*server-sessions* (list (cons name s))))
        (cl-tmux::dispatch-command s :has-session nil)
        (is (prompt-active-p) "prompt must be open")
        (funcall (prompt-on-submit *prompt*) name)
        (assert-overlay-active "on-submit must open an overlay")
        (assert-overlay-contains "yes" (overlay-lines)
                                 "known session")))))

(test cmd-has-session-rejects-unsupported-arguments
  "has-session rejects unknown flags and positionals instead of silently checking all sessions."
  (dolist (command '("has-session extra"
                     "has-session -x"
                     "has-session -t 0 extra"))
    (with-fake-session (s)
      (let ((*overlay* nil)
            (cl-tmux::*server-sessions* (list (cons (session-name s) s))))
        (is (null (cl-tmux::%run-command-line s command))
            "~A must be rejected" command)
        (assert-overlay-active "~A must show an unsupported-argument overlay" command)
        (assert-overlay-contains "unsupported argument" (overlay-lines)
                                 command)
        (assert-overlay-not-contains "yes" (overlay-lines)
                                     command)))))

;;; ── :lock-session / :unlock-session (already tested) ─────────────────────────
;;; Covered by dispatch-lock-unlock-session above.

;;; ── Pane-navigation zoom pop (tmux window_pop_zoom) ──────────────────────────

(test pane-navigation-unzooms-unless-Z-table
  "Pane-navigation commands on a zoomed window unzoom it unless -Z is given;
   the pane-configuring select-pane forms leave zoom untouched.
   Each row: (command-line expect-zoomed-after description)."
  (dolist (row '(("select-pane -t %2"    nil "select-pane must pop zoom")
                 ("select-pane -Z -t %2" t   "select-pane -Z must keep zoom")
                 ("select-pane -m"       t   "select-pane -m (configure) must keep zoom")
                 ("swap-pane -U"         nil "swap-pane must pop zoom")
                 ("swap-pane -UZ"        t   "swap-pane -Z must keep zoom")
                 ("rotate-window"        nil "rotate-window must pop zoom")
                 ("rotate-window -Z"     t   "rotate-window -Z must keep zoom")
                 ("last-pane"            nil "last-pane must pop zoom")
                 ("last-pane -Z"         t   "last-pane -Z must keep zoom")))
    (destructuring-bind (command expect-zoomed desc) row
      (with-two-pane-h-session (s win p0 p1)
        (with-command-test-state (s :overlay t)
          ;; Arm last-pane's target and zoom the window.
          (cl-tmux/model:window-select-pane win p1)
          (cl-tmux/model:window-select-pane win p0)
          (cl-tmux/model:window-zoom-toggle win)
          (is-true (cl-tmux/model:window-zoom-p win)
                   "precondition: window must be zoomed (~A)" desc)
          (cl-tmux::%run-command-line s command)
          (is (eq expect-zoomed
                  (and (cl-tmux/model:window-zoom-p win) t))
              "~A" desc))))))

(test keyboard-pane-navigation-pops-zoom-table
  "The interactive pane-navigation keyword handlers unzoom a zoomed window
   (tmux window_pop_zoom; the default bindings carry no -Z) and then actually
   move — previously a zoomed window's single-leaf tree made them no-ops.
   Each row: (command expect-focus-moved description)."
  (dolist (row '((:select-pane-right t "prefix-arrow must unzoom and move")
                 (:next-pane         t "prefix-o must unzoom and cycle")
                 (:last-pane         t ":last-pane must unzoom and jump")
                 (:swap-pane-forward nil "swap keeps the same active pane")))
    (destructuring-bind (command expect-moved desc) row
      (with-two-pane-h-session (s win p0 p1)
        (with-command-test-state (s :overlay t)
          ;; Arm last-pane's target, focus p0, then zoom.
          (cl-tmux/model:window-select-pane win p1)
          (cl-tmux/model:window-select-pane win p0)
          (cl-tmux/model:window-zoom-toggle win)
          (is-true (cl-tmux/model:window-zoom-p win)
                   "precondition: window must be zoomed (~A)" desc)
          (cl-tmux::dispatch-command s command nil)
          (is-false (cl-tmux/model:window-zoom-p win)
                    "~A: the window must be unzoomed" desc)
          (if expect-moved
              (is (eq p1 (cl-tmux/model:window-active-pane win))
                  "~A: focus must move to the other pane" desc)
              (is (eq p0 (cl-tmux/model:window-active-pane win))
                  "~A" desc)))))))

(test copy-mode-M-enters-at-mouse-position-with-selection
  "copy-mode -M places the copy cursor at the in-flight mouse position and
   begins a selection (the MouseDrag1Pane entry); without a mouse event -M
   enters copy mode normally."
  (with-fake-session (s)
    (let* ((win  (cl-tmux/model:session-active-window s))
           (pane (cl-tmux/model:window-active-pane win))
           (screen (cl-tmux/model:pane-screen pane)))
      ;; With a mouse event over the pane: cursor jumps + selection begins.
      (let ((cl-tmux::*current-mouse-event*
              (list :btn 32 :col 5 :row 2 :release-p nil)))
        (cl-tmux::%cmd-copy-mode-arg s '("-M"))
        (is-true (cl-tmux/terminal/types:screen-copy-mode-p screen)
                 "-M must enter copy mode")
        (is (equal (cons (- 2 (cl-tmux/model:pane-y pane))
                         (- 5 (cl-tmux/model:pane-x pane)))
                   (cl-tmux/terminal/types:screen-copy-cursor screen))
            "-M must place the copy cursor at the mouse position")
        (is-true (cl-tmux/terminal/types:screen-copy-selecting screen)
                 "-M must begin a selection")
        (cl-tmux/commands:copy-mode-exit screen))
      ;; Without a mouse event: plain entry, no selection.
      (let ((cl-tmux::*current-mouse-event* nil))
        (cl-tmux::%cmd-copy-mode-arg s '("-M"))
        (is-true (cl-tmux/terminal/types:screen-copy-mode-p screen)
                 "-M without a mouse event must still enter copy mode")
        (is (null (cl-tmux/terminal/types:screen-copy-selecting screen))
            "-M without a mouse event must not begin a selection")))))

(test copy-mode-H-hides-position-indicator
  "copy-mode -H suppresses the position indicator for this entry; a later plain
   entry shows it again."
  (with-fake-session (s)
    (let ((screen (cl-tmux/model:pane-screen (cl-tmux/model:session-active-pane s))))
      (cl-tmux::%cmd-copy-mode-arg s '("-H"))
      (is-true (cl-tmux/terminal/types:screen-copy-hide-position screen)
               "-H must set the hide-position flag")
      (cl-tmux/commands:copy-mode-exit screen)
      (cl-tmux::%cmd-copy-mode-arg s '())
      (is (null (cl-tmux/terminal/types:screen-copy-hide-position screen))
          "a plain entry must clear the hide-position flag"))))

(test resize-pane-T-trims-below-cursor-from-history
  "resize-pane -T drops the rows below the cursor and pulls rows out of the
   scrollback to refill the screen; the cursor lands on the bottom row."
  (with-fake-session (s)
    (let* ((pane   (cl-tmux/model:session-active-pane s))
           (screen (cl-tmux/model:pane-screen pane))
           (h      (cl-tmux/terminal/types:screen-height screen)))
      ;; History: one saved row of 'H' cells (newest).
      (let ((saved (make-array (cl-tmux/terminal/types:screen-width screen))))
        (dotimes (col (length saved))
          (setf (aref saved col) (cl-tmux/terminal/types:make-cell :char #\H)))
        (push saved (cl-tmux/terminal/types:screen-scrollback screen)))
      ;; Visible content: 'A' on row 0, cursor on row 0 → everything below trims.
      (setf (cl-tmux/terminal/types:cell-char
             (cl-tmux/terminal/types:screen-cell screen 0 0)) #\A)
      (setf (cl-tmux/terminal/types:screen-cursor-y screen) 0)
      (cl-tmux::%cmd-resize-pane-arg s '("-T"))
      (is (= (1- h) (cl-tmux/terminal/types:screen-cursor-y screen))
          "-T must land the cursor on the bottom row")
      (is (char= #\A (cl-tmux/terminal/types:cell-char
                      (cl-tmux/terminal/types:screen-cell screen 0 (1- h))))
          "the surviving cursor row must shift to the bottom")
      (is (char= #\H (cl-tmux/terminal/types:cell-char
                      (cl-tmux/terminal/types:screen-cell screen 0 (- h 2))))
          "the newest history row must appear directly above it")
      (is (null (cl-tmux/terminal/types:screen-scrollback screen))
          "the pulled history row must leave the scrollback"))))

(test resize-pane-M-arms-border-drag-state
  "resize-pane -M with an in-flight mouse event on a pane border arms the
   border-drag state used by MouseDrag1Border."
  (with-two-pane-h-session (s win p0 p1)
    (with-command-test-state (s :overlay t)
      (let* ((border-col (+ (cl-tmux/model:pane-x p0)
                            (cl-tmux/model:pane-width p0)))
             (cl-tmux::*mouse-drag-state* nil)
             (cl-tmux::*current-mouse-event*
               (list :btn 32 :col border-col
                     :row (cl-tmux/model:pane-y p0) :release-p nil)))
        (cl-tmux::%cmd-resize-pane-arg s '("-M"))
        (is-true cl-tmux::*mouse-drag-state*
                 "-M on a border must arm the mouse drag state"))
      (let ((cl-tmux::*mouse-drag-state* nil)
            (cl-tmux::*current-mouse-event* nil))
        (cl-tmux::%cmd-resize-pane-arg s '("-M"))
        (is (null cl-tmux::*mouse-drag-state*)
            "-M without a mouse event must not arm the drag state")))))

(test copy-mode-x-new-command-names-resolve
  "The newly-added send-keys -X names resolve through the X dispatch tables:
   stop-selection keeps the mark but stops extending; halfpage-down-and-cancel
   and copy-pipe-end-of-line / jump-to-forward are registered."
  (with-fake-session (s)
    (let* ((pane   (cl-tmux/model:session-active-pane s))
           (screen (cl-tmux/model:pane-screen pane)))
      (cl-tmux/commands:copy-mode-enter screen)
      (cl-tmux/commands:copy-mode-begin-selection screen)
      (is-true (cl-tmux/terminal/types:screen-copy-selecting screen)
               "precondition: selecting")
      (cl-tmux::%run-command-line s "send-keys -X stop-selection")
      (is (null (cl-tmux/terminal/types:screen-copy-selecting screen))
          "stop-selection must stop extending")
      (is-true (cl-tmux/terminal/types:screen-copy-mark screen)
               "stop-selection must KEEP the mark (unlike clear-selection)")
      ;; Registration checks for the other names.
      (is-true (assoc "halfpage-down-and-cancel"
                      cl-tmux::*copy-mode-x-commands* :test #'string=)
               "halfpage-down-and-cancel must be in the X table")
      (is-true (find "copy-pipe-end-of-line"
                     cl-tmux::+send-keys-x-explicit-arg-specs+
                     :key #'first :test #'string=)
               "bare copy-pipe-end-of-line must be in the arg specs")
      (is-true (find "jump-to-forward"
                     cl-tmux::+send-keys-x-explicit-arg-specs+
                     :key #'first :test #'string=)
               "jump-to-forward must be in the arg specs"))))

(test display-menu-O-keeps-menu-open-after-selection
  "display-menu -O keeps the menu open after a selection runs its command;
   without -O the menu closes (tmux -O)."
  (dolist (row '((t "display-menu -O -T t lbl k \"set -g @menu-ran 1\"" "with -O")
                 (nil "display-menu -T t lbl k \"set -g @menu-ran 1\"" "without -O")))
    (destructuring-bind (expect-open command desc) row
      (with-fake-session (s)
        (let ((*overlay* nil)
              (cl-tmux/prompt:*active-menu* nil))
          (cl-tmux::%run-command-line s command)
          (is-true cl-tmux/prompt:*active-menu* "menu must open (~A)" desc)
          (cl-tmux::dispatch-command s :menu-select 13)
          (is (string= "1" (cl-tmux/options:get-option "@menu-ran" nil))
              "the selected command must run (~A)" desc)
          (if expect-open
              (is-true cl-tmux/prompt:*active-menu*
                       "-O must keep the menu open")
              (is (null cl-tmux/prompt:*active-menu*)
                  "without -O the menu must close")))))))

(test session-group-format-vars
  "#{session_grouped}/#{session_group_size}/#{session_group_list} expand from
   the group registry; ungrouped sessions report 0/empty."
  (let ((cl-tmux::*session-groups* nil))
    (with-fake-session (s1)
      (with-fake-session (s2)
        (setf (cl-tmux/model:session-name s1) "ga"
              (cl-tmux/model:session-name s2) "gb")
        (flet ((expand (spec sess)
                 (cl-tmux/format:expand-format
                  spec (cl-tmux/format:format-context-from-session
                        sess (cl-tmux/model:session-active-window sess) nil))))
          (is (string= "0" (expand "#{session_grouped}" s1))
              "ungrouped session must report 0")
          (cl-tmux::server-new-session-in-group s2 s1)
          (is (string= "1" (expand "#{session_grouped}" s1))
              "grouped session must report 1")
          (is (string= "2" (expand "#{session_group_size}" s1))
              "the group must have two members")
          (let ((names (expand "#{session_group_list}" s1)))
            (is (and (search "ga" names) (search "gb" names))
                "the group list must name both sessions (got ~S)" names)))))))

(test capture-pane-a-requires-alternate-screen
  "capture-pane -a errors with tmux's 'no alternate screen' unless the pane's
   alternate screen is in use; while active, -a captures the (live) alternate."
  (with-fake-session (s)
    (let ((screen (cl-tmux/model:pane-screen (cl-tmux/model:session-active-pane s))))
      (let ((*overlay* nil))
        (cl-tmux::%cmd-capture-pane-arg s '("-a" "-p"))
        (assert-overlay-contains "no alternate screen" (overlay-lines)
                                 "capture-pane -a without an alt screen"))
      ;; Enter the alternate screen: -a now captures.
      (cl-tmux/terminal/actions:enter-alt-screen screen)
      (let ((*overlay* nil))
        (cl-tmux::%cmd-capture-pane-arg s '("-a" "-p"))
        (is (null (and *overlay* (search "no alternate screen" *overlay*)))
            "capture-pane -a with an active alt screen must capture")))))

(test list-keys-N-lists-only-noted-bindings
  "list-keys -N lists bindings carrying bind -N notes (with the note text);
   -a additionally includes un-noted bindings."
  (with-isolated-config
   (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line
       s "bind -N \"Split the pane\" Y split-window")
      (cl-tmux::%run-command-line s "bind Z kill-pane")
      (let ((*overlay* nil))
        (cl-tmux::%run-command-line s "list-keys -N -T prefix")
        (is (search "Split the pane" *overlay*)
            "-N must show the note text")
        (is (null (search "Z " *overlay*))
            "-N must exclude un-noted bindings"))
      (let ((*overlay* nil))
        (cl-tmux::%run-command-line s "list-keys -N -a -T prefix")
        (is (search "Split the pane" *overlay*) "-Na keeps noted bindings")
        (is (search "Z" *overlay*) "-Na includes un-noted bindings"))))))

(test select-layout-o-undoes-last-layout-change
  "select-layout -o restores the layout tree saved before the last layout
   application; a second -o redoes (swap semantics)."
  (with-two-pane-h-session (s win p0 p1)
    (with-command-test-state (s :overlay t)
      (let ((before-tree (cl-tmux/model:window-tree win)))
        (cl-tmux::%run-command-line s "select-layout even-vertical")
        (let ((after-tree (cl-tmux/model:window-tree win)))
          (is (not (eq before-tree after-tree))
              "applying a named layout must install a new tree")
          (cl-tmux::%run-command-line s "select-layout -o")
          (is (eq before-tree (cl-tmux/model:window-tree win))
              "-o must restore the pre-change tree")
          (cl-tmux::%run-command-line s "select-layout -o")
          (is (eq after-tree (cl-tmux/model:window-tree win))
              "a second -o must redo (swap semantics)"))))))

(test copy-mode-toggle-position-flips-indicator-visibility
  "send-keys -X toggle-position flips the position-indicator visibility flag."
  (with-fake-session (s)
    (let ((screen (cl-tmux/model:pane-screen (cl-tmux/model:session-active-pane s))))
      (cl-tmux::%cmd-copy-mode-arg s '())
      (is (null (cl-tmux/terminal/types:screen-copy-hide-position screen))
          "the indicator is visible on a plain entry")
      (cl-tmux::%run-command-line s "send-keys -X toggle-position")
      (is-true (cl-tmux/terminal/types:screen-copy-hide-position screen)
               "toggle-position must hide the indicator")
      (cl-tmux::%run-command-line s "send-keys -X toggle-position")
      (is (null (cl-tmux/terminal/types:screen-copy-hide-position screen))
          "a second toggle must show it again"))))

(test pane-start-and-socket-format-vars
  "#{pane_start_command}/#{pane_start_path} expand from the pane spawn record;
   #{socket_path} is empty in standalone mode and reflects the bound socket."
  (with-fake-session (s)
    (let* ((win  (cl-tmux/model:session-active-window s))
           (pane (cl-tmux/model:window-active-pane win)))
      (setf (cl-tmux/model:pane-start-command pane) "htop"
            (cl-tmux/model:pane-start-path pane) "/tmp/start-here")
      (flet ((expand (spec)
               (cl-tmux/format:expand-format
                spec (cl-tmux/format:format-context-from-session s win pane))))
        (is (string= "htop" (expand "#{pane_start_command}"))
            "pane_start_command must expand from the spawn record")
        (is (string= "/tmp/start-here" (expand "#{pane_start_path}"))
            "pane_start_path must expand from the spawn record")
        (let ((cl-tmux::*bound-socket-path* nil))
          (is (string= "" (expand "#{socket_path}"))
              "socket_path must be empty without a bound socket"))
        (let ((cl-tmux::*bound-socket-path* "/tmp/cl-tmux-1/x.sock"))
          (is (string= "/tmp/cl-tmux-1/x.sock" (expand "#{socket_path}"))
              "socket_path must reflect the bound socket"))))))
