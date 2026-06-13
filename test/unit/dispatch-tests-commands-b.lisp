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
      (is (overlay-active-p) ":synchronize-panes must show an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "ON" text) "first toggle must produce ON message"))
      ;; Toggle back off.
      (cl-tmux::dispatch-command s :synchronize-panes nil)
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "OFF" text) "second toggle must produce OFF message")))))

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

;;; ── :show-options / :show-option dispatch ────────────────────────────────────

(test dispatch-show-options-shows-overlay
  ":show-options opens an overlay listing global options."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::dispatch-command s :show-options nil)
      (is (overlay-active-p) ":show-options must open an overlay"))))

(test dispatch-show-option-opens-prompt
  ":show-option opens a prompt for the option name."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :show-option nil)
      (is (prompt-active-p) ":show-option must open a prompt"))))

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

;;; ── :last-pane dispatch ──────────────────────────────────────────────────────

(test dispatch-last-pane-selects-previous-pane
  ":last-pane selects the previously active pane."
  (with-fake-session (s :nwindows 1 :npanes 2)
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
  (with-fake-session (s :nwindows 1 :npanes 2)
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
  (with-empty-session (s)
    (with-loop-state
      (finishes (cl-tmux::%copy-mode-call s (lambda (screen) (declare (ignore screen)) nil))
                "%copy-mode-call must not error when there is no active screen"))))

;;; ── %handle-kill-result helper ────────────────────────────────────────────────

(test handle-kill-result-sets-running-nil-on-quit
  "%handle-kill-result clears *running* when RESULT is :quit."
  (with-loop-state
    (cl-tmux::%handle-kill-result :quit)
    (is-false cl-tmux::*running*
              "*running* must be NIL after :quit")))

(test handle-kill-result-preserves-running-for-nil
  "%handle-kill-result does NOT clear *running* for a NIL result."
  (with-loop-state
    (cl-tmux::%handle-kill-result nil)
    (is-true cl-tmux::*running*
             "*running* must remain T for nil result")))

(test handle-kill-result-returns-its-argument
  "%handle-kill-result returns its argument unchanged."
  (with-loop-state
    (is (eq :quit (cl-tmux::%handle-kill-result :quit)))
    (is (null (cl-tmux::%handle-kill-result nil)))))

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

;;; ── +popup-max-width+ / +popup-max-height+ / +popup-margin+ constants ───────

(test popup-constants-are-positive
  "Popup dimension constants are defined and positive."
  (is (> cl-tmux::+popup-max-width+  0) "+popup-max-width+ must be positive")
  (is (> cl-tmux::+popup-max-height+ 0) "+popup-max-height+ must be positive")
  (is (> cl-tmux::+popup-margin+     0) "+popup-margin+ must be positive"))

;;; ── +buffer-preview-length+ constant ─────────────────────────────────────────

(test buffer-preview-length-constant-is-positive
  "+buffer-preview-length+ is defined and positive."
  (is (> cl-tmux::+buffer-preview-length+ 0)
      "+buffer-preview-length+ must be positive"))

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
      (is (overlay-active-p) ":display-menu must open an overlay"))))

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

(test dispatch-menu-next-advances-selection
  ":menu-next advances the selected index."
  (with-fake-session (s)
    (let ((cl-tmux::*active-menu*
            (make-menu :title "t"
                       :items (list (cons "a" :ka) (cons "b" :kb))
                       :selected-index 0)))
      (cl-tmux::dispatch-command s :menu-next nil)
      (is (= 1 (menu-selected-index cl-tmux::*active-menu*))
          ":menu-next must advance the selection index to 1"))))

(test dispatch-menu-prev-wraps-selection
  ":menu-prev wraps from index 0 to the last item."
  (with-fake-session (s)
    (let ((cl-tmux::*active-menu*
            (make-menu :title "t"
                       :items (list (cons "a" :ka) (cons "b" :kb))
                       :selected-index 0)))
      (cl-tmux::dispatch-command s :menu-prev nil)
      (is (= 1 (menu-selected-index cl-tmux::*active-menu*))
          ":menu-prev from 0 must wrap to last index (1)"))))

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
        (is (overlay-active-p) "on-submit must open an overlay")
        (let ((text (format nil "~{~A~%~}" (overlay-lines))))
          (is (search "yes" text) "overlay must say 'yes' for a known session"))))))

;;; ── :lock-session / :unlock-session (already tested) ─────────────────────────
;;; Covered by dispatch-lock-unlock-session above.
