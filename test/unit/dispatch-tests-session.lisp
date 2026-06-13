(in-package #:cl-tmux/test)

;;;; Copy-mode paging dispatch tests.
;;;;  Continued in dispatch-tests-session-b.lisp (coverage: previously untested
;;;;  handlers, send-keys, capture-pane, paste-buffer) and
;;;;  dispatch-tests-session-c.lisp (options, session management, control mode).
;;;;  (dispatch-core.lisp, dispatch-commands-pane.lisp, commands-copy-mode.lisp)

(in-suite dispatch-suite)

;;; ── copy-mode paging / scrolling / movement command dispatch ─────────────────
;;;
;;; These commands delegate to copy-mode helpers via %copy-mode-call.
;;; We verify each dispatches without error when copy mode is active.

(defmacro with-copy-mode-active ((session-var) &body body)
  "Enter copy mode on a fresh fake session bound to SESSION-VAR, run BODY.
   Used to test copy-mode dispatch commands in isolation."
  `(with-fake-session (,session-var)
     (cl-tmux::dispatch-command ,session-var :copy-mode-enter nil)
     (is (cl-tmux::%copy-mode-active-p ,session-var)
         "copy mode must be on before testing copy-mode commands")
     ,@body))

(test copy-mode-page-up-scrolls-viewport-back
  ":copy-mode-page-up scrolls the copy-mode viewport toward the beginning of history."
  (with-copy-mode-active (s)
    (finishes (cl-tmux::dispatch-command s :copy-mode-page-up nil)
              ":copy-mode-page-up must not signal an error")))

(test copy-mode-page-down-scrolls-viewport-forward
  ":copy-mode-page-down scrolls the copy-mode viewport toward the end of the buffer."
  (with-copy-mode-active (s)
    (finishes (cl-tmux::dispatch-command s :copy-mode-page-down nil)
              ":copy-mode-page-down must not signal an error")))

(test copy-mode-half-page-up-scrolls-half-viewport-back
  ":copy-mode-half-page-up scrolls the viewport back by half a screen."
  (with-copy-mode-active (s)
    (finishes (cl-tmux::dispatch-command s :copy-mode-half-page-up nil)
              ":copy-mode-half-page-up must not signal an error")))

(test copy-mode-half-page-down-scrolls-half-viewport-forward
  ":copy-mode-half-page-down scrolls the viewport forward by half a screen."
  (with-copy-mode-active (s)
    (finishes (cl-tmux::dispatch-command s :copy-mode-half-page-down nil)
              ":copy-mode-half-page-down must not signal an error")))

(test copy-mode-scroll-up-line-scrolls-one-line-back
  ":copy-mode-scroll-up-line scrolls the viewport back by one line."
  (with-copy-mode-active (s)
    (finishes (cl-tmux::dispatch-command s :copy-mode-scroll-up-line nil)
              ":copy-mode-scroll-up-line must not signal an error")))

(test copy-mode-scroll-down-line-scrolls-one-line-forward
  ":copy-mode-scroll-down-line scrolls the viewport forward by one line."
  (with-copy-mode-active (s)
    (finishes (cl-tmux::dispatch-command s :copy-mode-scroll-down-line nil)
              ":copy-mode-scroll-down-line must not signal an error")))

(test copy-mode-word-forward-advances-cursor-by-one-word
  ":copy-mode-word-forward advances the copy-mode cursor to the start of the next word."
  (with-copy-mode-active (s)
    (finishes (cl-tmux::dispatch-command s :copy-mode-word-forward nil)
              ":copy-mode-word-forward must not signal an error")))

(test copy-mode-word-backward-retreats-cursor-by-one-word
  ":copy-mode-word-backward moves the copy-mode cursor to the start of the previous word."
  (with-copy-mode-active (s)
    (finishes (cl-tmux::dispatch-command s :copy-mode-word-backward nil)
              ":copy-mode-word-backward must not signal an error")))

(test copy-mode-word-end-advances-cursor-to-end-of-word
  ":copy-mode-word-end advances the copy-mode cursor to the last character of the current word."
  (with-copy-mode-active (s)
    (finishes (cl-tmux::dispatch-command s :copy-mode-word-end nil)
              ":copy-mode-word-end must not signal an error")))

(test copy-mode-line-start-moves-cursor-to-column-zero
  ":copy-mode-line-start moves the copy-mode cursor to column 0 of the current line."
  (with-copy-mode-active (s)
    (finishes (cl-tmux::dispatch-command s :copy-mode-line-start nil)
              ":copy-mode-line-start must not signal an error")))

(test copy-mode-line-end-moves-cursor-to-last-column
  ":copy-mode-line-end moves the copy-mode cursor to the last column of the current line."
  (with-copy-mode-active (s)
    (finishes (cl-tmux::dispatch-command s :copy-mode-line-end nil)
              ":copy-mode-line-end must not signal an error")))

(test copy-mode-top-moves-cursor-to-first-visible-line
  ":copy-mode-top moves the copy-mode cursor to the top of the visible viewport."
  (with-copy-mode-active (s)
    (finishes (cl-tmux::dispatch-command s :copy-mode-top nil)
              ":copy-mode-top must not signal an error")))

(test copy-mode-bottom-moves-cursor-to-last-visible-line
  ":copy-mode-bottom moves the copy-mode cursor to the bottom of the visible viewport."
  (with-copy-mode-active (s)
    (finishes (cl-tmux::dispatch-command s :copy-mode-bottom nil)
              ":copy-mode-bottom must not signal an error")))

(test copy-mode-high-moves-cursor-to-top-of-screen
  ":copy-mode-high positions the copy-mode cursor on the topmost screen row (H key)."
  (with-copy-mode-active (s)
    (finishes (cl-tmux::dispatch-command s :copy-mode-high nil)
              ":copy-mode-high must not signal an error")))

(test copy-mode-middle-moves-cursor-to-middle-of-screen
  ":copy-mode-middle positions the copy-mode cursor on the middle screen row (M key)."
  (with-copy-mode-active (s)
    (finishes (cl-tmux::dispatch-command s :copy-mode-middle nil)
              ":copy-mode-middle must not signal an error")))

(test copy-mode-low-moves-cursor-to-bottom-of-screen
  ":copy-mode-low positions the copy-mode cursor on the lowest screen row (L key)."
  (with-copy-mode-active (s)
    (finishes (cl-tmux::dispatch-command s :copy-mode-low nil)
              ":copy-mode-low must not signal an error")))

(test copy-mode-begin-line-selection-starts-line-wise-selection
  ":copy-mode-begin-line-selection starts a line-wise selection at the cursor position."
  (with-copy-mode-active (s)
    (finishes (cl-tmux::dispatch-command s :copy-mode-begin-line-selection nil)
              ":copy-mode-begin-line-selection must not signal an error")))

(test copy-mode-copy-end-of-line-yanks-to-line-end
  ":copy-mode-copy-end-of-line copies text from the cursor to the end of the line."
  (with-copy-mode-active (s)
    (finishes (cl-tmux::dispatch-command s :copy-mode-copy-end-of-line nil)
              ":copy-mode-copy-end-of-line must not signal an error")))

(test copy-mode-copy-line-yanks-entire-current-line
  ":copy-mode-copy-line copies the entire current line into the paste buffer."
  (with-copy-mode-active (s)
    (finishes (cl-tmux::dispatch-command s :copy-mode-copy-line nil)
              ":copy-mode-copy-line must not signal an error")))

(test copy-mode-search-next-advances-to-next-match
  ":copy-mode-search-next moves the cursor to the next search match (n key)."
  (with-copy-mode-active (s)
    (finishes (cl-tmux::dispatch-command s :copy-mode-search-next nil)
              ":copy-mode-search-next must not signal an error")))

(test copy-mode-search-prev-retreats-to-previous-match
  ":copy-mode-search-prev moves the cursor to the previous search match (N key)."
  (with-copy-mode-active (s)
    (finishes (cl-tmux::dispatch-command s :copy-mode-search-prev nil)
              ":copy-mode-search-prev must not signal an error")))

(test dispatch-copy-mode-choose-buffer-no-buffers-shows-overlay
  ":copy-mode-choose-buffer opens an overlay saying 'no paste buffers' when ring is empty."
  (with-fake-session (s)
    (cl-tmux::dispatch-command s :copy-mode-enter nil)
    (is (cl-tmux::%copy-mode-active-p s) "copy mode must be on")
    (let ((*overlay* nil)
          (cl-tmux/buffer:*paste-buffers* nil))
      (cl-tmux::dispatch-command s :copy-mode-choose-buffer nil)
      (is (overlay-active-p)
          ":copy-mode-choose-buffer must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "no paste buffers" text)
            "overlay must say 'no paste buffers' when ring is empty")))))

(test dispatch-copy-mode-choose-buffer-with-entries
  ":copy-mode-choose-buffer with buffers lists them by index."
  (with-fake-session (s)
    (cl-tmux::dispatch-command s :copy-mode-enter nil)
    (is (cl-tmux::%copy-mode-active-p s) "copy mode must be on")
    (let ((*overlay* nil)
          (cl-tmux/buffer:*paste-buffers* (list (cons "buffer1" "alpha")
                                                (cons "buffer0" "beta"))))
      (cl-tmux::dispatch-command s :copy-mode-choose-buffer nil)
      (is (overlay-active-p)
          ":copy-mode-choose-buffer must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "0:" text) "overlay must list buffer 0")
        (is (search "1:" text) "overlay must list buffer 1")))))

;;; ── copy-mode commands are no-ops when copy mode is off ─────────────────────

(test dispatch-copy-mode-commands-noop-outside-copy-mode
  "Copy-mode dispatch commands do not error when copy mode is not active."
  (with-fake-session (s)
    (is-false (cl-tmux::%copy-mode-active-p s) "copy mode must be off")
    (dolist (cmd '(:copy-mode-page-up :copy-mode-page-down
                   :copy-mode-word-forward :copy-mode-word-backward
                   :copy-mode-line-start :copy-mode-line-end
                   :copy-mode-top :copy-mode-bottom))
      (finishes (cl-tmux::dispatch-command s cmd nil)
                "~A must not error when copy mode is off" cmd))))

;;; ── with-active-pane body execution ─────────────────────────────────────────

(test with-active-pane-evaluates-body-when-pane-exists
  "with-active-pane evaluates BODY and binds PANE-VAR when a pane is active."
  (let* ((s  (make-fake-session))
         (ap (session-active-pane s))
         (result nil))
    (cl-tmux::with-active-pane (p s)
      (setf result p))
    (is (eq ap result)
        "with-active-pane must bind PANE-VAR to the active pane")))

(test with-active-pane-skips-body-for-windowless-session
  "with-active-pane does not evaluate BODY when no active pane is present."
  (with-empty-session (s)
    (let ((called nil))
      (cl-tmux::with-active-pane (_p s)
        (setf called (not (null _p))))
      (is-false called
                "with-active-pane body must not execute when no active pane"))))

;;; ── %format-menu selected-index=0 ───────────────────────────────────────────

(test format-menu-selected-index-zero-marks-first-item
  "%format-menu with selected-index=0 places the arrow marker next to the first item."
  (let* ((menu   (make-menu :title "T"
                             :items (list (cons "First" :k1) (cons "Second" :k2))
                             :selected-index 0))
         (output (cl-tmux::%format-menu menu)))
    (is (stringp output) "%format-menu must return a string")
    (is (search "▶" output) "output must have the ▶ marker")
    (is (search "First" output) "output must contain the first item label")))

;;; ── %apply-named-layout-to-session :tiled ────────────────────────────────────

(test apply-named-layout-tiled-does-not-error
  "%apply-named-layout-to-session :tiled dispatches without error on a 2-pane window."
  (with-two-pane-layout-session (sess win p0 p1)
    (is (and win p0 p1) "two-pane layout fixture created")
    (finishes (cl-tmux::%apply-named-layout-to-session sess :tiled)
              "%apply-named-layout-to-session :tiled must not signal an error")))

;;; ── %handle-kill-result :detach does not clear *running* ─────────────────────

(test handle-kill-result-preserves-running-for-detach
  "%handle-kill-result does NOT clear *running* for a :detach result."
  (with-loop-state
    (cl-tmux::%handle-kill-result :detach)
    (is-true cl-tmux::*running*
             "*running* must remain T after :detach")))

;;; ── %format-window-list empty session ───────────────────────────────────────

(test format-window-list-empty-session-returns-empty-string
  "%format-window-list with no windows returns an empty string."
  (with-empty-session (s)
    (let ((text (cl-tmux::%format-window-list s)))
      (is (stringp text) "%format-window-list must return a string")
      (is (string= "" text)
          "%format-window-list must return empty string for windowless session"))))

;;; ── %format-session-list window count ────────────────────────────────────────

(test format-session-list-shows-window-count
  "%format-session-list includes the window count for each session."
  (let* ((s    (make-fake-session :nwindows 2))
         (name (session-name s)))
    (let ((cl-tmux::*server-sessions* (list (cons name s))))
      (let ((text (cl-tmux::%format-session-list s)))
        (is (search "2 window" text)
            "output must include the window count '2 window'")))))

;;; ── dispatch-command outcome propagation ─────────────────────────────────────

(test dispatch-command-returns-quit-from-killing-last-window
  "dispatch-command propagates :quit when kill-window eliminates the last window."
  (with-fake-session (s :nwindows 1)
    (is (eq :quit (cl-tmux::dispatch-command s :kill-window nil))
        "dispatch-command must return :quit when the last window is killed")))

(test dispatch-command-returns-nil-and-marks-dirty-for-next-window
  "dispatch-command returns NIL and marks *dirty* for :next-window."
  (with-fake-session (s :nwindows 2)
    (let ((result (cl-tmux::dispatch-command s :next-window nil)))
      (is (null result)
          "dispatch-command must return NIL for :next-window")
      (is-true cl-tmux::*dirty*
               "dispatch-command must mark *dirty* for :next-window"))))

;;; ── %copy-mode-cmd exhaustive override-table coverage ───────────────────────

(test copy-mode-cmd-correct-overrides-for-all-table-chars
  "%copy-mode-cmd returns the correct keyword for every character in the override table."
  (flet ((check (ch kw)
           (is (eq kw (cl-tmux::%copy-mode-cmd ch))
               "%copy-mode-cmd ~C must return ~S" ch kw)))
    (check #\q :copy-mode-exit)
    (check #\i :copy-mode-exit)
    (check #\Space :copy-mode-begin-selection)
    (check #\v :copy-mode-begin-selection)
    (check #\V :copy-mode-begin-line-selection)
    (check #\y :copy-mode-yank)
    (check #\w :copy-mode-word-forward)
    (check #\b :copy-mode-word-backward)
    (check #\e :copy-mode-word-end)
    (check #\0 :copy-mode-line-start)
    (check #\$ :copy-mode-line-end)
    (check #\g :copy-mode-top)
    (check #\G :copy-mode-bottom)
    (check #\H :copy-mode-high)
    (check #\M :copy-mode-middle)
    (check #\L :copy-mode-low)
    (check #\D :copy-mode-copy-end-of-line)
    (check #\Y :copy-mode-copy-line)
    (check #\n :copy-mode-search-next)
    (check #\N :copy-mode-search-prev)
    (check #\/ :copy-mode-search-forward-prompt)
    (check #\? :copy-mode-search-backward-prompt)
    (check #\= :copy-mode-choose-buffer)))

;;; ── %format-tree-entry with multiple windows ─────────────────────────────────

(test format-tree-entry-marks-active-window-with-asterisk
  "%format-tree-entry marks the active window's id with an asterisk."
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
      (is (search "*0" output)
          "active window id must appear with an asterisk prefix")
      (is (search "alpha" output) "first window name must appear in output")
      (is (search "beta"  output) "second window name must appear in output"))))

;;; ── clear-history (clear a pane's scrollback) ─────────────────────────────────

(test clear-scrollback-empties-history
  "clear-scrollback empties a screen's scrollback, leaving the visible grid intact."
  (let ((s (make-screen 20 5)))
    (seed-scrollback s 3)
    (is (= 3 (length (cl-tmux/terminal/types::screen-scrollback s)))
        "scrollback must be seeded with 3 rows")
    (cl-tmux/terminal/actions:clear-scrollback s)
    (is (null (cl-tmux/terminal/types::screen-scrollback s))
        "scrollback must be empty after clear-scrollback")))

(test dispatch-clear-history-empties-active-pane-scrollback
  ":clear-history clears the active pane's scrollback history."
  (with-fake-session (s)
    (let ((screen (pane-screen (window-active-pane (session-active-window s)))))
      (seed-scrollback screen 3)
      (cl-tmux::dispatch-command s :clear-history nil)
      (is (null (cl-tmux/terminal/types::screen-scrollback screen))
          ":clear-history must empty the active pane's scrollback"))))

;;; ── named-command table (C-b : prompt resolution) ─────────────────────────────
