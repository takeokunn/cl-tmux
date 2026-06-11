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
  `(let ((,session-var (make-fake-session)))
     (with-loop-state
       (cl-tmux::dispatch-command ,session-var :copy-mode-enter nil)
       (is (cl-tmux::%copy-mode-active-p ,session-var)
           "copy mode must be on before testing copy-mode commands")
       ,@body)))

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
    (with-loop-state
      (finishes (cl-tmux::%apply-named-layout-to-session sess :tiled)
                "%apply-named-layout-to-session :tiled must not signal an error"))))

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

(test named-command-break-pane-is-recognized
  "%dispatch-named-command recognizes 'break-pane' and breaks the pane into a window."
  (with-fake-session (s :nwindows 1 :npanes 2)
    (let ((*overlay* nil))
      (cl-tmux::%dispatch-named-command s "break-pane")
      (is (not (and *overlay* (search "unknown command" *overlay*)))
          "break-pane must be a recognized command name")
      (is (= 2 (length (session-windows s)))
          "break-pane must move the pane into a second window"))))

(test named-command-unknown-shows-error-overlay
  "%dispatch-named-command shows an unknown-command overlay for an unrecognized name."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%dispatch-named-command s "no-such-command-xyz")
      (is (and *overlay* (search "unknown command" *overlay*))
          "an unknown command name must show the unknown-command overlay"))))

;;; ── select-layout arg command ────────────────────────────────────────────────

(test run-command-line-select-layout-even-horizontal
  "%run-command-line select-layout even-horizontal applies even-horizontal layout."
  (with-fake-session (s :nwindows 1 :npanes 2)
    (cl-tmux::%run-command-line s "select-layout even-horizontal")
    ;; Layout must be applied without error — just check the window still has 2 panes.
    (is (= 2 (length (cl-tmux/model:window-panes (cl-tmux/model:session-active-window s))))
        "select-layout even-horizontal must leave pane count unchanged")))

(test run-command-line-select-layout-main-horizontal
  "%run-command-line select-layout main-horizontal applies main-horizontal layout."
  (with-fake-session (s :nwindows 1 :npanes 3)
    (cl-tmux::%run-command-line s "select-layout main-horizontal")
    (is (= 3 (length (cl-tmux/model:window-panes (cl-tmux/model:session-active-window s))))
        "select-layout main-horizontal must leave pane count unchanged")))

(test run-command-line-select-layout-unknown-is-noop
  "%run-command-line select-layout with an unknown name is a no-op (no error)."
  (with-fake-session (s :nwindows 1 :npanes 2)
    (is (null (cl-tmux::%run-command-line s "select-layout bogus-layout"))
        "unknown layout name must not raise an error")))

;;; ── set-option -u (unset) ────────────────────────────────────────────────────

(test run-command-line-set-option-unset
  "%run-command-line 'set -u <name>' removes the option from *global-options*."
  (let ((cl-tmux/options:*global-options*
         (let ((h (make-hash-table :test #'equal)))
           (setf (gethash "status-left" h) "my-value")
           h)))
    (let ((s (make-fake-session)))
      (with-loop-state
        (cl-tmux::%run-command-line s "set -u status-left")
        (is (not (gethash "status-left" cl-tmux/options:*global-options*))
            "set -u status-left must remove the key from *global-options*")))))

(test set-option-w-unset-clears-window-local-not-global
  "setw -u <opt> (= set -w -u) removes the WINDOW-local override, leaving the
   global value intact (scope-aware -u, was always unsetting global)."
  (let ((cl-tmux/options:*global-options* (make-hash-table :test #'equal))
        (s (make-fake-session :nwindows 1)))
    (with-loop-state
      (let ((win (cl-tmux/model:session-active-window s)))
        (cl-tmux/options:set-option "mode-keys" "emacs")             ; global
        (cl-tmux/options:set-option-for-window "mode-keys" "vi" win) ; window-local
        (cl-tmux::%run-command-line s "setw -u mode-keys")
        (is (not (nth-value 1 (gethash "mode-keys"
                                       (cl-tmux/model:window-local-options win))))
            "setw -u must remove the window-local override")
        (is (equal "emacs" (cl-tmux/options:get-option "mode-keys"))
            "the global value must remain untouched")))))

(test set-option-a-w-appends-to-window-local-value
  "set -aw <opt> X appends to the WINDOW-local value (scope-aware -a, was always
   appending to the global store)."
  (let ((cl-tmux/options:*global-options* (make-hash-table :test #'equal))
        (s (make-fake-session :nwindows 1)))
    (with-loop-state
      (let ((win (cl-tmux/model:session-active-window s)))
        (cl-tmux/options:set-option-for-window "@x" "ab" win)
        (cl-tmux::%run-command-line s "set -aw @x cd")
        (is (equal "abcd" (cl-tmux/options:get-option-for-window "@x" win))
            "set -aw must append to the window-local value")
        (is (not (nth-value 1 (gethash "@x" cl-tmux/options:*global-options*)))
            "the global store must not gain the option")))))

;;; ── list-panes arg command ───────────────────────────────────────────────────

(test run-command-line-list-panes-shows-overlay
  "%run-command-line list-panes shows an overlay listing panes."
  (with-fake-session (s :nwindows 1 :npanes 2)
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line s "list-panes")
      (is (and *overlay* (plusp (length *overlay*)))
          "list-panes must produce a non-empty overlay"))))

;;; ── split-window arg command ─────────────────────────────────────────────────

(test parse-split-size-absolute-vs-percentage
  "%parse-split-size: a plain integer is absolute cells; an N% value is a real
   fraction (modern tmux's `-l 30%`, equivalent to the deprecated `-p 30`)."
  (is (eql 30 (cl-tmux::%parse-split-size "30"))
      "\"30\" → 30 absolute cells (integer)")
  (is (= 0.30 (cl-tmux::%parse-split-size "30%"))
      "\"30%\" → 0.30 fraction")
  (is (= 0.5 (cl-tmux::%parse-split-size "50%"))
      "\"50%\" → 0.5 fraction")
  (is (= 1.0 (cl-tmux::%parse-split-size "100%"))
      "\"100%\" → 1.0 fraction")
  (is (null (cl-tmux::%parse-split-size nil))
      "NIL value → NIL")
  (is (floatp (cl-tmux::%parse-split-size "30%"))
      "a percentage must be a real (fraction), not an integer cell count")
  (is (integerp (cl-tmux::%parse-split-size "30"))
      "an absolute value must stay an integer (cells)"))

(test run-command-line-split-window-default-vertical-stack
  "%run-command-line split-window (no flags) adds a new pane below."
  (with-fake-session (s :nwindows 1 :npanes 1)
    ;; split-window forks a PTY; skip if not available
    (when (pty-available-p)
      (let* ((win   (cl-tmux/model:session-active-window s))
             (before (length (cl-tmux/model:window-panes win))))
        (cl-tmux::%run-command-line s "split-window")
        (stop-cl-tmux-threads)
        (is (> (length (cl-tmux/model:window-panes win)) before)
            "split-window must add a pane to the active window")))))

(test run-command-line-split-window-h-flag
  "%run-command-line split-window -h adds a pane to the right."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (when (pty-available-p)
      (let* ((win    (cl-tmux/model:session-active-window s))
             (before (length (cl-tmux/model:window-panes win))))
        (cl-tmux::%run-command-line s "split-window -h")
        (stop-cl-tmux-threads)
        (is (> (length (cl-tmux/model:window-panes win)) before)
            "split-window -h must add a pane to the active window")))))

(test split-window-P-F-uses-custom-format
  "split-window -d -P -F '...' prints the CUSTOM format for the new pane instead of
   the default session:window.pane [WxH] summary."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (when (pty-available-p)
      (let ((*overlay* nil))
        (cl-tmux::%run-command-line s "split-window -d -P -F MARK#{pane_id}")
        (stop-cl-tmux-threads)
        (let ((text (format nil "~{~A~%~}" (overlay-lines))))
          (is (search "MARK" text)
              "-F custom format must appear in the overlay (got ~S)" text)
          (is (null (search "[" text))
              "default [WxH] summary must NOT be used when -F is given (got ~S)" text))))))

(test split-window-f-full-spans-window-width
  "split-window -f -v adds a pane spanning the FULL window width (a full-window
   split at the layout root), not just the active pane's width."
  (let* ((win (%vsplit-window 20))   ; p0|p1 side by side; window width 41
         (s   (make-session :id 1 :name "0" :windows (list win))))
    (session-select-window s win)
    (with-loop-state
      (when (pty-available-p)
        (cl-tmux::%run-command-line s "split-window -f -v")
        (stop-cl-tmux-threads)
        (is (= 3 (length (window-panes win))) "a third pane was added")
        (let ((newest (car (last (window-panes win)))))
          (is (= (window-width win) (pane-width newest))
              "the -f pane must span the full window width (~D), got ~D"
              (window-width win) (pane-width newest)))))))

;;; ── new-window -n name ───────────────────────────────────────────────────────

(test run-command-line-new-window-with-name
  "%run-command-line new-window -n myname creates a window named myname."
  (with-fake-session (s :nwindows 1)
    (when (pty-available-p)
      (cl-tmux::%run-command-line s "new-window -n myname")
      (stop-cl-tmux-threads)
      (let ((win (cl-tmux/model:session-active-window s)))
        (is (string= "myname" (cl-tmux/model:window-name win))
            "new-window -n must set the window name")))))

(test new-window-P-F-uses-custom-format
  "new-window -d -P -F '...' prints the CUSTOM format to the overlay instead of the
   default session:window.pane [WxH] summary."
  (with-fake-session (s :nwindows 1)
    (when (pty-available-p)
      (let ((*overlay* nil))
        (cl-tmux::%run-command-line s "new-window -d -P -F MARK#{window_index}")
        (stop-cl-tmux-threads)
        (let ((text (format nil "~{~A~%~}" (overlay-lines))))
          (is (search "MARK" text)
              "-F custom format must appear in the overlay (got ~S)" text)
          (is (null (search "[" text))
              "default [WxH] summary must NOT be used when -F is given (got ~S)" text))))))

;;; ── show-window-options / show-session-options ───────────────────────────────

(test dispatch-show-window-options-shows-overlay
  ":show-window-options shows an overlay with window options."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::dispatch-command s :show-window-options nil)
      (is (and *overlay* (plusp (length *overlay*)))
          ":show-window-options must produce an overlay"))))

(test dispatch-show-session-options-shows-overlay
  ":show-session-options shows an overlay with session options."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::dispatch-command s :show-session-options nil)
      (is (and *overlay* (plusp (length *overlay*)))
          ":show-session-options must produce an overlay"))))

;;; ── server management commands ───────────────────────────────────────────────

(test dispatch-server-info-shows-overlay
  ":server-info shows an overlay with server information."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::dispatch-command s :server-info nil)
      (is (and *overlay* (plusp (length *overlay*)))
          ":server-info must produce an overlay")
      (is (search "server" *overlay*)
          ":server-info overlay must mention 'server'"))))

(test dispatch-list-clients-shows-overlay
  ":list-clients shows an overlay listing connected clients."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::dispatch-command s :list-clients nil)
      (is (and *overlay* (plusp (length *overlay*)))
          ":list-clients must produce an overlay"))))

(test dispatch-lock-server-locks-all-sessions
  ":lock-server sets locked-p on all sessions."
  (let ((s1 (make-fake-session))
        (s2 (make-fake-session)))
    (let ((cl-tmux::*server-sessions*
           (list (cons "a" s1) (cons "b" s2))))
      (with-loop-state
        (cl-tmux::dispatch-command s1 :lock-server nil)
        (is (cl-tmux/model:session-locked-p s1)
            ":lock-server must lock s1")
        (is (cl-tmux/model:session-locked-p s2)
            ":lock-server must lock all sessions including s2")))))

(test dispatch-show-environment-shows-overlay
  ":show-environment shows an overlay with environment variables."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::dispatch-command s :show-environment nil)
      (is (and *overlay* (plusp (length *overlay*)))
          ":show-environment must produce an overlay"))))

;;; ── dynamic prefix key ───────────────────────────────────────────────────────

(test dynamic-prefix-key-default-is-ctrl-b
  "*prefix-key-code* defaults to +prefix-key-code+ (2 = C-b)."
  (is (= cl-tmux/config:+prefix-key-code+ cl-tmux/config:*prefix-key-code*)
      "*prefix-key-code* must equal +prefix-key-code+ initially"))

(test apply-config-directive-set-prefix-updates-runtime-var
  "'set -g prefix C-a' updates *prefix-key-code* to 1 (C-a)."
  (let ((cl-tmux/config:*prefix-key-code* cl-tmux/config:+prefix-key-code+)
        (cl-tmux/config:*key-tables* (make-hash-table :test #'equal)))
    (cl-tmux/config::initialize-default-key-tables)
    (cl-tmux/config:apply-config-directive '("set" "-g" "prefix" "C-a"))
    (is (= 1 cl-tmux/config:*prefix-key-code*)
        "'set -g prefix C-a' must set *prefix-key-code* to 1")))

;;; ── command alias dispatch ───────────────────────────────────────────────────

(test command-alias-dispatch-expands-and-runs
  "A registered command alias is expanded and dispatched."
  (let ((s (make-fake-session)))
    (let ((cl-tmux/options:*command-aliases* (make-hash-table :test #'equal)))
      (cl-tmux/options:register-command-alias "nw" "new-window")
      (with-loop-state
        (when (pty-available-p)
          (let* ((win    (cl-tmux/model:session-active-window s))
                 (before (length (cl-tmux/model:session-windows s))))
            (cl-tmux::%run-command-line s "nw")
            (stop-cl-tmux-threads)
            (is (> (length (cl-tmux/model:session-windows s)) before)
                "alias 'nw' → new-window must create a new window")))))))

;;; ── new-window -d (detached) ─────────────────────────────────────────────────

(test run-command-line-new-window-d-does-not-switch
  "new-window -d creates a window without switching focus."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (when (pty-available-p)
      (let* ((orig-win (cl-tmux/model:session-active-window s)))
        (cl-tmux::%run-command-line s "new-window -d")
        (stop-cl-tmux-threads)
        (is (> (length (cl-tmux/model:session-windows s)) 1)
            "new-window -d must create a window")
        (is (eq orig-win (cl-tmux/model:session-active-window s))
            "new-window -d must not change the active window")))))

;;; ── split-window -d (detached) ───────────────────────────────────────────────

(test run-command-line-split-window-d-does-not-switch
  "split-window -d creates a pane without switching focus."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (when (pty-available-p)
      (let* ((win      (cl-tmux/model:session-active-window s))
             (orig-pane (cl-tmux/model:window-active-pane win)))
        (cl-tmux::%run-command-line s "split-window -d")
        (stop-cl-tmux-threads)
        (is (> (length (cl-tmux/model:window-panes win)) 1)
            "split-window -d must add a pane")
        (is (eq orig-pane (cl-tmux/model:window-active-pane win))
            "split-window -d must not change the active pane")))))

