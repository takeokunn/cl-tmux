(in-package #:cl-tmux/test)

;;;; Copy-mode paging dispatch tests.
;;;;  Continued in dispatch-tests-client-session-control.lisp and related responsibility files (coverage: previously untested
;;;;  handlers, send-keys, capture-pane, paste-buffer) and
;;;;  dispatch-tests-session-c.lisp (options, session management, control mode).
;;;;  (dispatch-core.lisp, dispatch-commands-pane.lisp, commands-copy-mode.lisp)

;;; ── copy-mode paging / scrolling / movement / selection / copy commands ─────
;;;
;;; All commands below share one contract: dispatch without error when copy mode
;;; is active.  One table-driven test covers the whole set; the command keyword
;;; in the finishes message keeps per-command failure messages specific.

(defmacro with-copy-mode-active ((session-var) &body body)
  "Enter copy mode on a fresh fake session bound to SESSION-VAR, run BODY.
   Used to test copy-mode dispatch commands in isolation."
  `(with-fake-session (,session-var)
     (cl-tmux::dispatch-command ,session-var :copy-mode-enter nil)
     (expect (cl-tmux::%copy-mode-active-p ,session-var))
     ,@body))

(describe "dispatch-suite"

  ;; All copy-mode navigation, selection, and copy commands dispatch without error
  ;; when copy mode is active.
  (it "copy-mode-commands-dispatch-without-error"
    (with-copy-mode-active (s)
      (dolist (cmd '(:copy-mode-page-up          :copy-mode-page-down
                     :copy-mode-half-page-up     :copy-mode-half-page-down
                     :copy-mode-scroll-up-line   :copy-mode-scroll-down-line
                     :copy-mode-word-forward     :copy-mode-word-backward
                     :copy-mode-word-end
                     :copy-mode-line-start       :copy-mode-line-end
                     :copy-mode-top              :copy-mode-bottom
                     :copy-mode-high             :copy-mode-middle        :copy-mode-low
                     :copy-mode-begin-line-selection
                     :copy-mode-copy-end-of-line :copy-mode-copy-line
                     :copy-mode-search-next      :copy-mode-search-prev))
        (finishes (cl-tmux::dispatch-command s cmd nil)
                  "~A must not signal an error in copy mode" cmd))))

  ;; :copy-mode-choose-buffer opens an overlay saying 'no paste buffers' when ring is empty.
  (it "dispatch-copy-mode-choose-buffer-no-buffers-shows-overlay"
    (with-fake-session (s)
      (cl-tmux::dispatch-command s :copy-mode-enter nil)
      (expect (cl-tmux::%copy-mode-active-p s))
      (let ((*overlay* nil)
            (cl-tmux/buffer:*paste-buffers* nil))
        (cl-tmux::dispatch-command s :copy-mode-choose-buffer nil)
        (assert-overlay-active ":copy-mode-choose-buffer must open an overlay")
        (assert-overlay-contains "no paste buffers" (overlay-lines)
                                 ":copy-mode-choose-buffer"))))

  ;; :copy-mode-choose-buffer with buffers lists them by index.
  (it "dispatch-copy-mode-choose-buffer-with-entries"
    (with-fake-session (s)
      (cl-tmux::dispatch-command s :copy-mode-enter nil)
      (expect (cl-tmux::%copy-mode-active-p s))
      (let ((*overlay* nil)
            (cl-tmux/buffer:*paste-buffers* (list (cons "buffer1" "alpha")
                                                  (cons "buffer0" "beta"))))
        (cl-tmux::dispatch-command s :copy-mode-choose-buffer nil)
        (assert-overlay-active ":copy-mode-choose-buffer must open an overlay")
        (assert-overlay-contains "0:" (overlay-lines)
                                 ":copy-mode-choose-buffer")
        (assert-overlay-contains "1:" (overlay-lines)
                                 ":copy-mode-choose-buffer"))))

  ;;; ── copy-mode commands are no-ops when copy mode is off ─────────────────────

  ;; Copy-mode dispatch commands do not error when copy mode is not active.
  (it "dispatch-copy-mode-commands-noop-outside-copy-mode"
    (with-fake-session (s)
      (expect (cl-tmux::%copy-mode-active-p s) :to-be-falsy)
      (dolist (cmd '(:copy-mode-page-up :copy-mode-page-down
                     :copy-mode-word-forward :copy-mode-word-backward
                     :copy-mode-line-start :copy-mode-line-end
                     :copy-mode-top :copy-mode-bottom))
        (finishes (cl-tmux::dispatch-command s cmd nil)
                  "~A must not error when copy mode is off" cmd))))

  ;;; ── with-active-pane body execution ─────────────────────────────────────────

  ;; with-active-pane evaluates BODY and binds PANE-VAR when a pane is active.
  (it "with-active-pane-evaluates-body-when-pane-exists"
    (let* ((s  (make-fake-session))
           (ap (session-active-pane s))
           (result nil))
      (cl-tmux::with-active-pane (p s)
        (setf result p))
      (expect (eq ap result))))

  ;; with-active-pane does not evaluate BODY when no active pane is present.
  (it "with-active-pane-skips-body-for-windowless-session"
    (with-empty-session (s)
      (let ((called nil))
        (cl-tmux::with-active-pane (_p s)
          (setf called (not (null _p))))
        (expect called :to-be-falsy))))

  ;;; ── %format-menu selected-index=0 ───────────────────────────────────────────

  ;; %format-menu with selected-index=0 places the arrow marker next to the first item.
  (it "format-menu-selected-index-zero-marks-first-item"
    (let* ((menu   (make-menu :title "T"
                               :items (list (cons "First" :k1) (cons "Second" :k2))
                               :selected-index 0))
           (output (cl-tmux::%format-menu menu)))
      (expect (stringp output))
      (expect (search "▶" output))
      (expect (search "First" output))))

  ;;; ── %apply-named-layout-to-session :tiled ────────────────────────────────────

  ;; %apply-named-layout-to-session :tiled dispatches without error on a 2-pane window.
  (it "apply-named-layout-tiled-does-not-error"
    (with-two-pane-layout-session (sess win p0 p1)
      (expect (and win p0 p1))
      (finishes (cl-tmux::%apply-named-layout-to-session sess :tiled)
                "%apply-named-layout-to-session :tiled must not signal an error")))

  ;;; ── %handle-kill-result :detach does not clear *running* ─────────────────────

  ;; %handle-kill-result does NOT clear *running* for a :detach result.
  (it "handle-kill-result-preserves-running-for-detach"
    (with-loop-state
      (cl-tmux::%handle-kill-result :detach)
      (expect cl-tmux::*running* :to-be-truthy)))

  ;;; ── %format-window-list empty session ───────────────────────────────────────

  ;; %format-window-list with no windows returns an empty string.
  (it "format-window-list-empty-session-returns-empty-string"
    (with-empty-session (s)
      (let ((text (cl-tmux::%format-window-list s)))
        (expect (stringp text))
        (expect (string= "" text)))))

  ;;; ── %format-session-list window count ────────────────────────────────────────

  ;; %format-session-list includes the window count for each session.
  (it "format-session-list-shows-window-count"
    (let* ((s    (make-fake-session :nwindows 2))
           (name (session-name s)))
      (let ((cl-tmux::*server-sessions* (list (cons name s))))
        (let ((text (cl-tmux::%format-session-list s)))
          (expect (search "2 window" text))))))

  ;;; ── dispatch-command outcome propagation ─────────────────────────────────────

  ;; dispatch-command propagates :quit when kill-window eliminates the last window.
  (it "dispatch-command-returns-quit-from-killing-last-window"
    (with-fake-session (s :nwindows 1)
      (expect (eq :quit (cl-tmux::dispatch-command s :kill-window nil)))))

  ;; dispatch-command returns NIL and marks *dirty* for :next-window.
  (it "dispatch-command-returns-nil-and-marks-dirty-for-next-window"
    (with-fake-session (s :nwindows 2)
      (let ((result (cl-tmux::dispatch-command s :next-window nil)))
        (expect (null result))
        (expect cl-tmux::*dirty* :to-be-truthy))))

  ;;; ── %copy-mode-cmd exhaustive override-table coverage ───────────────────────

  ;; %copy-mode-cmd returns the correct keyword for every character in the override table.
  (it "copy-mode-cmd-correct-overrides-for-all-table-chars"
    (dolist (case '((#\q . :copy-mode-exit)
                    (#\i . :copy-mode-exit)
                    (#\Space . :copy-mode-begin-selection)
                    (#\v . :copy-mode-begin-selection)
                    (#\V . :copy-mode-begin-line-selection)
                    (#\y . :copy-mode-yank)
                    (#\w . :copy-mode-word-forward)
                    (#\b . :copy-mode-word-backward)
                    (#\e . :copy-mode-word-end)
                    (#\0 . :copy-mode-line-start)
                    (#\$ . :copy-mode-line-end)
                    (#\g . :copy-mode-top)
                    (#\G . :copy-mode-bottom)
                    (#\H . :copy-mode-high)
                    (#\M . :copy-mode-middle)
                    (#\L . :copy-mode-low)
                    (#\D . :copy-mode-copy-pipe-end-of-line-and-cancel)
                    (#\Y . :copy-mode-copy-line)
                    (#\n . :copy-mode-search-next)
                    (#\N . :copy-mode-search-prev)
                    (#\/ . :copy-mode-search-forward-prompt)
                    (#\? . :copy-mode-search-backward-prompt)
                    (#\= . :copy-mode-choose-buffer)))
      (let ((ch (car case))
            (kw (cdr case)))
        (expect (eq kw (cl-tmux::%copy-mode-cmd ch))))))

  ;;; ── %format-tree-entry with multiple windows ─────────────────────────────────

  ;; %format-tree-entry marks the active window's id with an asterisk.
  (it "format-tree-entry-marks-active-window-with-asterisk"
    (let* ((screen0 (make-screen 20 5))
           (pane0   (make-pane :id 1 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                               :screen screen0))
           (win0    (make-window :id 0 :name "alpha" :width 20 :height 5
                                 :panes (list pane0)
                                 :tree  (make-layout-leaf pane0)))
           (screen1 (make-screen 20 5))
           (pane1   (make-pane :id 2 :fd -1 :pid -1 :x 0 :y 0 :width 20 :height 5
                               :screen screen1))
           (win1    (make-window :id 1 :name "beta" :width 20 :height 5
                                 :panes (list pane1)
                                 :tree  (make-layout-leaf pane1))))
      (window-select-pane win0 pane0)
      (window-select-pane win1 pane1)
      (let ((output
              (with-output-to-string (s)
                (cl-tmux::%format-tree-entry s "sess" "sess"
                                            (list win0 win1) win0))))
        (expect (search "*0" output))
        (expect (search "alpha" output))
        (expect (search "beta"  output)))))

  ;;; ── clear-history (clear a pane's scrollback) ─────────────────────────────────

  ;; clear-scrollback empties a screen's scrollback, leaving the visible grid intact.
  (it "clear-scrollback-empties-history"
    (let ((s (make-screen 20 5)))
      (seed-scrollback s 3)
      (expect (= 3 (length (cl-tmux/terminal/types::screen-scrollback s))))
      (cl-tmux/terminal/actions:clear-scrollback s)
      (expect (null (cl-tmux/terminal/types::screen-scrollback s)))))

  ;; :clear-history clears the active pane's scrollback history.
  (it "dispatch-clear-history-empties-active-pane-scrollback"
    (with-fake-session (s)
      (let ((screen (pane-screen (window-active-pane (session-active-window s)))))
        (seed-scrollback screen 3)
        (cl-tmux::dispatch-command s :clear-history nil)
        (expect (null (cl-tmux/terminal/types::screen-scrollback screen))))))

  ;;; ── named-command table (C-b : prompt resolution) ─────────────────────────────
  )
