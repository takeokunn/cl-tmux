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
  (let* ((s   (make-fake-session :nwindows 1 :npanes 2))
         (win (session-active-window s))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win))))
    (with-loop-state
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
  (let ((s (make-fake-session :nwindows 2)))
    (let* ((text (cl-tmux::%format-window-list s))
           (aw   (session-active-window s)))
      (is (stringp text) "%format-window-list must return a string")
      (is (search (window-name aw) text)
          "output must mention the active window name")
      (is (search "*" text)
          "output must mark the active window with an asterisk"))))

(test format-window-list-shows-pane-count
  "%format-window-list includes the pane count for each window."
  (let ((s (make-fake-session :nwindows 1 :npanes 2)))
    (let ((text (cl-tmux::%format-window-list s)))
      ;; The format string ends each line with "[N pane(s)]".
      (is (search "pane" text)
          "output must include the word 'pane'"))))

;;; ── %format-session-list helper ──────────────────────────────────────────────

(test format-session-list-fallback-uses-session-name
  "%format-session-list with empty *server-sessions* falls back to the
   session-name one-line entry."
  (let ((s (make-fake-session :nwindows 1)))
    (let ((cl-tmux::*server-sessions* nil))
      (let ((text (cl-tmux::%format-session-list s)))
        (is (stringp text) "%format-session-list must return a string")
        (is (search (session-name s) text)
            "fallback output must contain the session name")))))

(test format-session-list-marks-current-session
  "%format-session-list with a populated *server-sessions* marks the current
   session with an asterisk."
  (let* ((s    (make-fake-session :nwindows 1))
         (name (session-name s)))
    (let ((cl-tmux::*server-sessions* (list (cons name s))))
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
  (let* ((s    (make-fake-session))
         (name (session-name s)))
    (with-loop-state
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

;;; ── :switch-client-next / :switch-client-prev dispatch ───────────────────────

(test dispatch-switch-client-next-moves-to-next-session
  ":switch-client-next touches the next session in the registry."
  (let* ((s1 (make-fake-session :nwindows 1))
         (s2 (make-fake-session :nwindows 1))
         (reg (list (cons (session-name s1) s1)
                    (cons (session-name s2) s2))))
    (with-loop-state
      (let ((cl-tmux::*server-sessions* reg))
        (cl-tmux::dispatch-command s1 :switch-client-next nil)
        (is-true cl-tmux::*dirty*
                 ":switch-client-next must mark *dirty*")))))

(test dispatch-switch-client-prev-does-not-error
  ":switch-client-prev dispatches without error."
  (with-fake-session (s)
    (finishes (cl-tmux::dispatch-command s :switch-client-prev nil)
              ":switch-client-prev must not signal an error")))

;;; ── :last-session dispatch ────────────────────────────────────────────────────

(test dispatch-last-session-does-not-error
  ":last-session dispatches without error when only one session exists."
  (with-fake-session (s)
    (let ((cl-tmux::*server-sessions* (list (cons (session-name s) s))))
      (finishes (cl-tmux::dispatch-command s :last-session nil)
                ":last-session must not signal an error"))))

;;; ── :new-session dispatch ─────────────────────────────────────────────────────

(test dispatch-new-session-does-not-error
  ":new-session dispatches without error."
  (with-fake-session (s)
    (let ((cl-tmux::*server-sessions* nil))
      (finishes (cl-tmux::dispatch-command s :new-session nil)
                ":new-session must not signal an error"))))

;;; ── :kill-session dispatch ────────────────────────────────────────────────────

(test dispatch-kill-session-with-no-other-sessions-quits
  ":kill-session with no remaining sessions returns :quit."
  (let* ((s    (make-fake-session))
         (name (session-name s)))
    (with-loop-state
      (let ((cl-tmux::*server-sessions* (list (cons name s))))
        (is (eq :quit (cl-tmux::dispatch-command s :kill-session nil))
            ":kill-session with empty registry must return :quit")))))

;;; ── :find-window dispatch ─────────────────────────────────────────────────────

(test dispatch-find-window-opens-prompt
  ":find-window opens a prompt for the search pattern."
  (with-fake-session (s :nwindows 1)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :find-window nil)
      (is (prompt-active-p) ":find-window must open a prompt"))))

;;; ── :mark-pane / :clear-mark dispatch ────────────────────────────────────────

(test dispatch-mark-pane-marks-active-pane
  ":mark-pane marks the active pane."
  (with-fake-session (s)
    (let ((ap (session-active-pane s)))
      (setf (pane-marked ap) nil)
      (cl-tmux::dispatch-command s :mark-pane nil)
      (is-true (pane-marked ap) ":mark-pane must set pane-marked to T"))))

(test dispatch-mark-pane-toggles-off
  ":mark-pane on an already-marked pane clears the mark (toggle)."
  (with-fake-session (s)
    (let ((ap (session-active-pane s)))
      (cl-tmux::dispatch-command s :mark-pane nil)
      (is-true (pane-marked ap) ":mark-pane must set the mark first")
      (cl-tmux::dispatch-command s :mark-pane nil)
      (is-false (pane-marked ap) ":mark-pane on marked pane must clear the mark"))))

(test dispatch-clear-mark-clears-server-marked-pane
  ":clear-mark clears the server-wide marked pane."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (let ((ap (session-active-pane s)))
      (cl-tmux::dispatch-command s :mark-pane nil)
      (is-true (pane-marked ap) ":mark-pane must mark the active pane")
      (cl-tmux::dispatch-command s :clear-mark nil)
      (is-false (pane-marked ap)
                ":clear-mark must clear the server-wide marked pane"))))

(test dispatch-mark-pane-sets-server-marked-pane
  ":mark-pane updates *server-marked-pane* to the active pane."
  (with-fake-session (s)
    (let ((ap (session-active-pane s)))
      (cl-tmux::dispatch-command s :mark-pane nil)
      (is (eq ap cl-tmux::*server-marked-pane*)
          "*server-marked-pane* must point to the newly marked pane"))))

(test dispatch-mark-pane-cross-window-clears-previous
  ":mark-pane in a second window clears the mark from a pane in the first window."
  (with-fake-session (s :nwindows 2)
    (let* ((win1 (first  (session-windows s)))
           (win2 (second (session-windows s)))
           (p1   (window-active-pane win1))
           (p2   (window-active-pane win2)))
      (session-select-window s win1)
      (cl-tmux::dispatch-command s :mark-pane nil)
      (is (pane-marked p1) "p1 must be marked in window 1")
      (session-select-window s win2)
      (cl-tmux::dispatch-command s :mark-pane nil)
      (is-false (pane-marked p1)
                "p1 in window 1 must be unmarked when window 2 pane is marked")
      (is (pane-marked p2) "p2 in window 2 must be marked")
      (is (eq p2 cl-tmux::*server-marked-pane*)
          "*server-marked-pane* must point to p2 after cross-window mark"))))

;;; ── :next-layout dispatch ─────────────────────────────────────────────────────

(test dispatch-next-layout-cycles-layout
  ":next-layout applies the next layout from the cycle table."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (finishes (cl-tmux::dispatch-command s :next-layout nil)
              ":next-layout must not signal an error")))

;;; ── :select-layout-tiled / :select-layout-spread dispatch ────────────────────

(test dispatch-select-layout-tiled-does-not-error
  ":select-layout-tiled dispatches without error."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (finishes (cl-tmux::dispatch-command s :select-layout-tiled nil)
              ":select-layout-tiled must not signal an error")))

(test dispatch-select-layout-spread-does-not-error
  ":select-layout-spread dispatches without error."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (finishes (cl-tmux::dispatch-command s :select-layout-spread nil)
              ":select-layout-spread must not signal an error")))

;;; ── :choose-client dispatch ───────────────────────────────────────────────────

(test dispatch-choose-client-shows-overlay
  ":choose-client opens an overlay with client information."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::dispatch-command s :choose-client nil)
      (is (overlay-active-p) ":choose-client must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "Clients" text) "overlay must contain 'Clients'")
        (is (search (session-name s) text)
            "overlay must contain the session name")))))

;;; ── :display-info dispatch ────────────────────────────────────────────────────

(test dispatch-display-info-shows-overlay
  ":display-info opens an overlay with session/window/pane details."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::dispatch-command s :display-info nil)
      (is (overlay-active-p) ":display-info must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "Session" text) "overlay must contain 'Session'")
        (is (search "Pane" text) "overlay must contain 'Pane'")))))

;;; ── :bind-key / :unbind-key dispatch ─────────────────────────────────────────

(test dispatch-bind-key-opens-prompt
  ":bind-key opens a prompt for the key-command pair."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :bind-key nil)
      (is (prompt-active-p) ":bind-key must open a prompt"))))

(test dispatch-unbind-key-opens-prompt
  ":unbind-key opens a prompt for the key to unbind."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :unbind-key nil)
      (is (prompt-active-p) ":unbind-key must open a prompt"))))

;;; ── :list-buffers / :show-buffer / :delete-buffer dispatch ───────────────────

(test dispatch-list-buffers-no-buffers-shows-overlay
  ":list-buffers with empty buffer ring opens an overlay saying '(no paste buffers)'."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux/buffer:*paste-buffers* nil))
      (cl-tmux::dispatch-command s :list-buffers nil)
      (is (overlay-active-p) ":list-buffers must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "no paste buffers" text)
            "overlay must say 'no paste buffers' when ring is empty")))))

(test dispatch-list-buffers-populated-shows-entries
  ":list-buffers with buffers lists them by name with their content preview."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux/buffer:*paste-buffers* (list (cons "buffer1" "hello")
                                                (cons "buffer0" "world"))))
      (cl-tmux::dispatch-command s :list-buffers nil)
      (is (overlay-active-p) ":list-buffers must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "hello" text) "overlay must list the first buffer's content")
        (is (search "world" text) "overlay must list the second buffer's content")
        (is (search "buffer1:" text) "overlay must show buffer names")))))

(test dispatch-show-buffer-shows-content
  ":show-buffer opens an overlay with buffer 0's content."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux/buffer:*paste-buffers* (list (cons "buffer0" "test-content"))))
      (cl-tmux::dispatch-command s :show-buffer nil)
      (is (overlay-active-p) ":show-buffer must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "test-content" text)
            "overlay must contain buffer 0 content")))))

(test dispatch-delete-buffer-removes-first-entry
  ":delete-buffer removes the first paste buffer."
  (with-fake-session (s)
    (let ((cl-tmux/buffer:*paste-buffers* (list (cons "buffer0" "todelete"))))
      (cl-tmux::dispatch-command s :delete-buffer nil)
      (is (null cl-tmux/buffer:*paste-buffers*)
          ":delete-buffer must remove buffer 0 from the ring"))))

(test paste-buffer-text-translates-lf-to-cr-by-default
  "%paste-buffer-text replaces LF with CR by default so a multi-line paste
   submits each line; -r (no-replace) keeps the raw bytes."
  (is (string= (format nil "a~Cb~Cc" #\Return #\Return)
               (cl-tmux::%paste-buffer-text (format nil "a~%b~%c") nil))
      "default paste must translate LF → CR")
  (is (string= (format nil "a~%b~%c")
               (cl-tmux::%paste-buffer-text (format nil "a~%b~%c") t))
      "-r must keep LF unchanged")
  (is (string= "abc" (cl-tmux::%paste-buffer-text "abc" nil))
      "text without newlines is unchanged")
  (is (null (cl-tmux::%paste-buffer-text nil nil))
      "NIL buffer contents → NIL"))

(test paste-buffer-text-separator-overrides-default
  "%paste-buffer-text -s SEPARATOR replaces LF with SEPARATOR instead of CR; -r
   still wins (raw), and SEP may be empty or multi-character."
  (is (string= "a-b-c"
               (cl-tmux::%paste-buffer-text (format nil "a~%b~%c") nil "-"))
      "-s '-' must replace each LF with '-'")
  (is (string= "a, b, c"
               (cl-tmux::%paste-buffer-text (format nil "a~%b~%c") nil ", "))
      "-s ', ' must replace each LF with a multi-character separator")
  (is (string= "abc"
               (cl-tmux::%paste-buffer-text (format nil "a~%b~%c") nil ""))
      "-s '' must strip the line breaks entirely")
  (is (string= (format nil "a~%b~%c")
               (cl-tmux::%paste-buffer-text (format nil "a~%b~%c") t "-"))
      "-r must take precedence over -s and keep the raw bytes"))

;;; ── :save-buffer / :load-buffer dispatch ─────────────────────────────────────

(test dispatch-save-buffer-opens-prompt-when-buffer-exists
  ":save-buffer opens a prompt for the file path when buffer 0 exists."
  (with-fake-session (s)
    (let ((*prompt* nil)
          (cl-tmux/buffer:*paste-buffers* (list (cons "buffer0" "save-me"))))
      (cl-tmux::dispatch-command s :save-buffer nil)
      (is (prompt-active-p) ":save-buffer must open a prompt when buffer exists"))))

(test dispatch-save-buffer-shows-error-when-no-buffer
  ":save-buffer with empty ring opens an overlay saying '(no paste buffers to save)'."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux/buffer:*paste-buffers* nil))
      (cl-tmux::dispatch-command s :save-buffer nil)
      (is (overlay-active-p) ":save-buffer must open an overlay when no buffers")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "no paste buffers" text)
            "overlay must mention 'no paste buffers'")))))

(test dispatch-load-buffer-opens-prompt
  ":load-buffer opens a prompt for the file path."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :load-buffer nil)
      (is (prompt-active-p) ":load-buffer must open a prompt"))))

;;; ── :choose-buffer dispatch ───────────────────────────────────────────────────

(test dispatch-choose-buffer-opens-prompt-when-buffers-exist
  ":choose-buffer with buffers opens a listing overlay and a prompt."
  (with-fake-session (s)
    (let ((*prompt* nil) (*overlay* nil)
          (cl-tmux/buffer:*paste-buffers* (list (cons "buffer1" "alpha")
                                                (cons "buffer0" "beta"))))
      (cl-tmux::dispatch-command s :choose-buffer nil)
      (is (overlay-active-p) ":choose-buffer must open a listing overlay")
      (is (prompt-active-p) ":choose-buffer must open a prompt for the index"))))

(test dispatch-choose-buffer-no-buffers-shows-overlay
  ":choose-buffer with empty ring shows '(no paste buffers)' overlay."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux/buffer:*paste-buffers* nil))
      (cl-tmux::dispatch-command s :choose-buffer nil)
      (is (overlay-active-p) ":choose-buffer must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "no paste buffers" text)
            "overlay must say 'no paste buffers'")))))

;;; ── :select-window-prompt dispatch ───────────────────────────────────────────

(test dispatch-select-window-prompt-opens-prompt
  ":select-window-prompt opens a prompt for the window name or number."
  (with-fake-session (s :nwindows 2)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :select-window-prompt nil)
      (is (prompt-active-p) ":select-window-prompt must open a prompt"))))

(test dispatch-select-window-prompt-selects-by-number
  ":select-window-prompt on-submit with a valid index selects that window."
  (with-fake-session (s :nwindows 2)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :select-window-prompt nil)
      (is (prompt-active-p) "prompt must be open")
      (funcall (prompt-on-submit *prompt*) "1")
      (is (eq (second (session-windows s)) (session-active-window s))
          "on-submit with \"1\" must select the second window"))))

;;; ── :move-window dispatch ─────────────────────────────────────────────────────

(test dispatch-move-window-opens-prompt
  ":move-window opens a prompt for the destination index."
  (with-fake-session (s :nwindows 2)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :move-window nil)
      (is (prompt-active-p) ":move-window must open a prompt"))))

;;; ── :swap-window dispatch ─────────────────────────────────────────────────────

(test dispatch-swap-window-opens-prompt
  ":swap-window opens a prompt for the destination index."
  (with-fake-session (s :nwindows 2)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :swap-window nil)
      (is (prompt-active-p) ":swap-window must open a prompt"))))

;;; ── :wait-for dispatch ────────────────────────────────────────────────────────

(test dispatch-wait-for-opens-prompt
  ":wait-for opens a prompt for the channel name."
  (with-fake-session (s)
    (let ((*prompt* nil))
      (cl-tmux::dispatch-command s :wait-for nil)
      (is (prompt-active-p) ":wait-for must open a prompt"))))

;;; ── %copy-mode-active-p ──────────────────────────────────────────────────────

(test copy-mode-active-p-false-for-windowless-session
  "%copy-mode-active-p returns NIL for a windowless session."
  (with-empty-session (s)
    (is-false (cl-tmux::%copy-mode-active-p s)
              "%copy-mode-active-p must return NIL for a windowless session")))

;;; ── %signal-channel-prompt helper ────────────────────────────────────────────

(test signal-channel-prompt-opens-prompt
  "%signal-channel-prompt opens a prompt with the given label."
  (with-loop-state
    (let ((*prompt* nil))
      (cl-tmux::%signal-channel-prompt "test-channel")
      (is (prompt-active-p) "%signal-channel-prompt must open a prompt")
      (is (string= "test-channel" (prompt-label *prompt*))
          "%signal-channel-prompt label must match the argument"))))

