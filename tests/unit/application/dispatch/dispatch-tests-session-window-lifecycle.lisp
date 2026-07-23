(in-package #:cl-tmux/test)

;;;; Dispatch session window lifecycle command tests

(describe "dispatch-suite"

  ;;; ── split-window arg command ─────────────────────────────────────────────────

  ;; %parse-split-size: a plain integer is absolute cells; an N% value is a real
  ;; fraction of the parent pane.
  (it "parse-split-size-absolute-vs-percentage"
    (expect (eql 30 (cl-tmux::%parse-split-size "30")))
    (expect (= 0.30 (cl-tmux::%parse-split-size "30%")))
    (expect (= 0.5 (cl-tmux::%parse-split-size "50%")))
    (expect (= 1.0 (cl-tmux::%parse-split-size "100%")))
    (expect (null (cl-tmux::%parse-split-size nil)))
    (expect (floatp (cl-tmux::%parse-split-size "30%")))
    (expect (integerp (cl-tmux::%parse-split-size "30"))))

  ;; split-window with no flags, -h, or -l N% each adds one pane to the active window.
  ;; Each row: (command message).
  (it "run-command-line-split-window-variants-add-pane"
    (dolist (row '(("split-window"        "split-window must add a pane to the active window")
                   ("split-window -h"     "split-window -h must add a pane to the active window")
                   ("split-window -l 30%" "split-window -l 30% must add a pane to the active window")))
      (destructuring-bind (cmd msg) row
        (with-pty-command-increasing-count
            (s cmd
               :count-form (length (cl-tmux/model:window-panes
                                    (cl-tmux/model:session-active-window s)))
               :count-context msg)))))

  ;; split-window rejects the removed -p percentage shorthand before adding panes.
  (it "run-command-line-split-window-rejects-percent-shorthand"
    (with-fake-session (s :nwindows 1 :npanes 1)
      (let* ((win (cl-tmux/model:session-active-window s))
             (before-panes (copy-list (cl-tmux/model:window-panes win))))
        (with-command-rejection-state (s
                                       (cl-tmux::%run-command-line s "split-window -p 30")
                                       "unsupported argument"
                                       "split-window -p 30")
          (expect (equal before-panes (cl-tmux/model:window-panes win)))))))

  ;; split-window -I creates a no-PTY pane and writes stdin into its screen.
  (it "run-command-line-split-window-I-feeds-stdin-without-pty"
    (with-fake-session (s :nwindows 1 :npanes 1)
      (let* ((win (session-active-window s))
             (before (length (window-panes win)))
             (new-pane (with-input-from-string (*standard-input* "from stdin")
                         (cl-tmux::%run-command-line s "split-window -I"))))
        (expect (= (1+ before) (length (window-panes win))))
        (expect (eq new-pane (car (last (window-panes win)))))
        (expect (= -1 (pane-fd new-pane)))
        (expect (= -1 (pane-pid new-pane)))
        (expect (search "from stdin" (row-string (pane-screen new-pane) 0))))))

  ;; split-window -d -P -F '...' prints the CUSTOM format for the new pane instead of
  ;; the default session:window.pane [WxH] summary.
  (it "split-window-P-F-uses-custom-format"
    (with-pty-run-command-line-overlay (s "split-window -d -P -F MARK#{pane_id}")
      (assert-overlay-uses-custom-format
       '("MARK")
       *overlay*
       "-F custom format must appear in the overlay")))

  ;; split-window -f -v adds a pane spanning the FULL window width (a full-window
  ;; split at the layout root), not just the active pane's width.
  (it "split-window-f-full-spans-window-width"
    (let* ((win (%vsplit-window 20))   ; p0|p1 side by side; window width 41
           (s   (make-session :id 1 :name "0" :windows (list win))))
      (session-select-window s win)
      (with-loop-state
        (cl-tmux::%run-command-line s "split-window -f -v")
        (expect (= 3 (length (window-panes win))))
        (let ((newest (car (last (window-panes win)))))
          (expect (= (window-width win) (pane-width newest)))))))

  ;; split-window -t :NAME splits the named window instead of the active one.
  (it "run-command-line-split-window-targets-named-window"
    (with-pty-session (s :nwindows 2 :npanes 1)
      (let* ((wins        (session-windows s))
             (home        (first wins))
             (work        (second wins))
             (home-before (length (window-panes home)))
             (work-before (length (window-panes work))))
        (setf (window-name home) "home"
              (window-name work) "work")
        (cl-tmux::%run-command-line s "split-window -t :work")
        (expect (eq work (session-active-window s)))
        (expect (= home-before (length (window-panes home))))
        (expect (> (length (window-panes work)) work-before)))))

  ;;; ── new-window -n name ───────────────────────────────────────────────────────

  ;; %run-command-line new-window -n myname creates a window named myname.
  (it "run-command-line-new-window-with-name"
    (with-pty-command-increasing-count
        (s "new-window -n myname"
           :count-form (length (cl-tmux/model:session-windows s))
           :count-context "new-window -n must create a window")
      (let ((win (cl-tmux/model:session-active-window s)))
        (expect (string= "myname" (cl-tmux/model:window-name win))))))

  ;; new-window -d -P -F '...' prints the CUSTOM format to the overlay instead of the
  ;; default session:window.pane [WxH] summary.
  (it "new-window-P-F-uses-custom-format"
    (with-pty-run-command-line-overlay (s "new-window -d -P -F MARK#{window_index}")
      (assert-overlay-uses-custom-format
       '("MARK")
       *overlay*
       "-F custom format must appear in the overlay")))

  ;;; ── show-window-options / show-session-options ───────────────────────────────

  ;; :show-window-options, :show-session-options, :list-clients, and :show-environment each produce a non-empty overlay.
  (it "dispatch-show-options-overlay-table"
    (dolist (cmd '(:show-window-options :show-session-options :list-clients :show-environment))
      (with-fake-session (s)
        (with-dispatch-overlay (s cmd)
          (expect (and *overlay* (plusp (length *overlay*))))))))

  ;;; ── server management commands ───────────────────────────────────────────────

  ;; :server-info shows an overlay with server information.
  (it "dispatch-server-info-shows-overlay"
    (with-fake-session (s)
      (with-dispatch-overlay (s :server-info)
        (expect (and *overlay* (plusp (length *overlay*))))
        (assert-overlay-contains "server" *overlay*
                                 ":server-info overlay must mention 'server'"))))

  ;; :lock-server sets locked-p on all sessions.
  (it "dispatch-lock-server-locks-all-sessions"
    (with-fake-session (s1)
      (let ((s2 (make-fake-session)))
        (with-registered-sessions (("a" s1) ("b" s2))
          (cl-tmux::dispatch-command s1 :lock-server nil)
          (expect (cl-tmux/model:session-locked-p s1))
          (expect (cl-tmux/model:session-locked-p s2))))))

  ;;; ── dynamic prefix key ───────────────────────────────────────────────────────

  ;; *prefix-key-code* defaults to +prefix-key-code+ (2 = C-b).
  (it "dynamic-prefix-key-default-is-ctrl-b"
    (expect (= cl-tmux/config:+prefix-key-code+ cl-tmux/config:*prefix-key-code*)))

  ;; 'set-option -g prefix C-a' updates *prefix-key-code* to 1 (C-a).
  (it "apply-config-directive-set-prefix-updates-runtime-var"
    (let ((cl-tmux/config:*prefix-key-code* cl-tmux/config:+prefix-key-code+)
          (cl-tmux/config:*key-tables* (make-hash-table :test #'equal)))
      (cl-tmux/config::initialize-default-key-tables)
      (cl-tmux/config:apply-config-directive '("set-option" "-g" "prefix" "C-a"))
      (expect (= 1 cl-tmux/config:*prefix-key-code*))))

  ;;; ── new-window -d (detached) ─────────────────────────────────────────────────

  ;; new-window -d creates a window without switching focus.
  (it "run-command-line-new-window-d-does-not-switch"
    (with-pty-command-preserving-focus
        (s "new-window -d"
           :count-form (length (cl-tmux/model:session-windows s))
           :active-form (cl-tmux/model:session-active-window s)
           :count-context "new-window -d must create a window"
           :focus-context "new-window -d must not change the active window")))

  ;;; ── split-window -d (detached) ───────────────────────────────────────────────

  ;; split-window -d creates a pane without switching focus.
  (it "run-command-line-split-window-d-does-not-switch"
    (with-pty-command-preserving-focus
        (s "split-window -d"
           :count-form (length (cl-tmux/model:window-panes
                                (cl-tmux/model:session-active-window s)))
           :active-form (cl-tmux/model:window-active-pane
                         (cl-tmux/model:session-active-window s))
           :count-context "split-window -d must add a pane"
           :focus-context "split-window -d must not change the active pane"))))
