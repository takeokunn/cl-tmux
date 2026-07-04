(in-package #:cl-tmux/test)

;;;; Dispatch session window lifecycle command tests

(in-suite dispatch-suite)

;;; ── split-window arg command ─────────────────────────────────────────────────

(test parse-split-size-absolute-vs-percentage
  "%parse-split-size: a plain integer is absolute cells; an N% value is a real
   fraction of the parent pane."
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

(test run-command-line-split-window-variants-add-pane
  "split-window with no flags, -h, or -l N% each adds one pane to the active window.
   Each row: (command message)."
  (dolist (row '(("split-window"        "split-window must add a pane to the active window")
                 ("split-window -h"     "split-window -h must add a pane to the active window")
                 ("split-window -l 30%" "split-window -l 30% must add a pane to the active window")))
    (destructuring-bind (cmd msg) row
      (with-pty-command-increasing-count
          (s cmd
             :count-form (length (cl-tmux/model:window-panes
                                  (cl-tmux/model:session-active-window s)))
             :count-context msg)))))

(test run-command-line-split-window-rejects-percent-shorthand
  "split-window rejects the removed -p percentage shorthand before adding panes."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (let* ((win (cl-tmux/model:session-active-window s))
           (before-panes (copy-list (cl-tmux/model:window-panes win))))
      (with-command-rejection-state (s
                                     (cl-tmux::%run-command-line s "split-window -p 30")
                                     "unsupported argument"
                                     "split-window -p 30")
        (is (equal before-panes (cl-tmux/model:window-panes win))
            "split-window -p 30 must not add or reorder panes after rejection")))))

(test run-command-line-split-window-I-feeds-stdin-without-pty
  "split-window -I creates a no-PTY pane and writes stdin into its screen."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (let* ((win (session-active-window s))
           (before (length (window-panes win)))
           (new-pane (with-input-from-string (*standard-input* "from stdin")
                       (cl-tmux::%run-command-line s "split-window -I"))))
      (is (= (1+ before) (length (window-panes win)))
          "split-window -I must create one pane")
      (is (eq new-pane (car (last (window-panes win))))
          "split-window -I must return the newly-created pane")
      (is (= -1 (pane-fd new-pane))
          "split-window -I pane must not have a PTY fd")
      (is (= -1 (pane-pid new-pane))
          "split-window -I pane must not have a child process")
      (is (search "from stdin" (row-string (pane-screen new-pane) 0))
          "stdin bytes must be rendered into the new pane's screen"))))

(test split-window-P-F-uses-custom-format
  "split-window -d -P -F '...' prints the CUSTOM format for the new pane instead of
   the default session:window.pane [WxH] summary."
  (with-pty-run-command-line-overlay (s "split-window -d -P -F MARK#{pane_id}")
    (assert-overlay-uses-custom-format
     '("MARK")
     *overlay*
     "-F custom format must appear in the overlay")))

(test split-window-f-full-spans-window-width
  "split-window -f -v adds a pane spanning the FULL window width (a full-window
   split at the layout root), not just the active pane's width."
  (let* ((win (%vsplit-window 20))   ; p0|p1 side by side; window width 41
         (s   (make-session :id 1 :name "0" :windows (list win))))
    (session-select-window s win)
    (with-loop-state
      (cl-tmux::%run-command-line s "split-window -f -v")
      (is (= 3 (length (window-panes win))) "a third pane was added")
      (let ((newest (car (last (window-panes win)))))
        (is (= (window-width win) (pane-width newest))
            "the -f pane must span the full window width (~D), got ~D"
            (window-width win) (pane-width newest))))))

(test run-command-line-split-window-targets-named-window
  "split-window -t :NAME splits the named window instead of the active one."
  (with-pty-session (s :nwindows 2 :npanes 1)
    (let* ((wins        (session-windows s))
           (home        (first wins))
           (work        (second wins))
           (home-before (length (window-panes home)))
           (work-before (length (window-panes work))))
      (setf (window-name home) "home"
            (window-name work) "work")
      (cl-tmux::%run-command-line s "split-window -t :work")
      (is (eq work (session-active-window s))
          "split-window -t must switch focus to the named target window")
      (is (= home-before (length (window-panes home)))
          "split-window -t must not mutate the non-target window")
      (is (> (length (window-panes work)) work-before)
          "split-window -t must add a pane to the target window"))))

;;; ── new-window -n name ───────────────────────────────────────────────────────

(test run-command-line-new-window-with-name
  "%run-command-line new-window -n myname creates a window named myname."
  (with-pty-command-increasing-count
      (s "new-window -n myname"
         :count-form (length (cl-tmux/model:session-windows s))
         :count-context "new-window -n must create a window")
    (let ((win (cl-tmux/model:session-active-window s)))
      (is (string= "myname" (cl-tmux/model:window-name win))
          "new-window -n must set the window name"))))

(test new-window-P-F-uses-custom-format
  "new-window -d -P -F '...' prints the CUSTOM format to the overlay instead of the
   default session:window.pane [WxH] summary."
  (with-pty-run-command-line-overlay (s "new-window -d -P -F MARK#{window_index}")
    (assert-overlay-uses-custom-format
     '("MARK")
     *overlay*
     "-F custom format must appear in the overlay")))

;;; ── show-window-options / show-session-options ───────────────────────────────

(test dispatch-show-options-overlay-table
  ":show-window-options, :show-session-options, :list-clients, and :show-environment each produce a non-empty overlay."
  (dolist (cmd '(:show-window-options :show-session-options :list-clients :show-environment))
    (with-fake-session (s)
      (with-dispatch-overlay (s cmd)
        (is (and *overlay* (plusp (length *overlay*)))
            "~A must produce a non-empty overlay" cmd)))))

;;; ── server management commands ───────────────────────────────────────────────

(test dispatch-server-info-shows-overlay
  ":server-info shows an overlay with server information."
  (with-fake-session (s)
    (with-dispatch-overlay (s :server-info)
      (is (and *overlay* (plusp (length *overlay*)))
          ":server-info must produce an overlay")
      (assert-overlay-contains "server" *overlay*
                               ":server-info overlay must mention 'server'"))))

(test dispatch-lock-server-locks-all-sessions
  ":lock-server sets locked-p on all sessions."
  (with-fake-session (s1)
    (let ((s2 (make-fake-session)))
      (with-registered-sessions (("a" s1) ("b" s2))
        (cl-tmux::dispatch-command s1 :lock-server nil)
        (is (cl-tmux/model:session-locked-p s1)
            ":lock-server must lock s1")
        (is (cl-tmux/model:session-locked-p s2)
            ":lock-server must lock all sessions including s2")))))


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

;;; ── new-window -d (detached) ─────────────────────────────────────────────────

(test run-command-line-new-window-d-does-not-switch
  "new-window -d creates a window without switching focus."
  (with-pty-command-preserving-focus
      (s "new-window -d"
         :count-form (length (cl-tmux/model:session-windows s))
         :active-form (cl-tmux/model:session-active-window s)
         :count-context "new-window -d must create a window"
         :focus-context "new-window -d must not change the active window")))

;;; ── split-window -d (detached) ───────────────────────────────────────────────

(test run-command-line-split-window-d-does-not-switch
  "split-window -d creates a pane without switching focus."
  (with-pty-command-preserving-focus
      (s "split-window -d"
         :count-form (length (cl-tmux/model:window-panes
                              (cl-tmux/model:session-active-window s)))
         :active-form (cl-tmux/model:window-active-pane
                       (cl-tmux/model:session-active-window s))
         :count-context "split-window -d must add a pane"
         :focus-context "split-window -d must not change the active pane")))
