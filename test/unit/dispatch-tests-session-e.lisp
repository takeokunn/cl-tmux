(in-package #:cl-tmux/test)

;;;; Dispatch session tests — part E: named-command table, select-layout arg,
;;;; set-option -u, list-panes, split-window, new-window, show-window/session-options,
;;;; server management, dynamic prefix key, command alias, new-window/split-window -d.

(in-suite dispatch-suite)

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
    (with-fake-session (s)
      (cl-tmux::%run-command-line s "set -u status-left")
      (is (not (gethash "status-left" cl-tmux/options:*global-options*))
          "set -u status-left must remove the key from *global-options*"))))

(test set-option-w-unset-clears-window-local-not-global
  "setw -u <opt> (= set -w -u) removes the WINDOW-local override, leaving the
   global value intact (scope-aware -u, was always unsetting global)."
  (let ((cl-tmux/options:*global-options* (make-hash-table :test #'equal)))
    (with-fake-session (s :nwindows 1)
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
  (let ((cl-tmux/options:*global-options* (make-hash-table :test #'equal)))
    (with-fake-session (s :nwindows 1)
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

(test run-command-line-list-panes-format-uses-arg-handler
  "%run-command-line list-panes -F expands pane formats through the arg handler."
  (with-fake-session (s :nwindows 1 :npanes 2)
    (setf (session-name s) "alpha")
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line
       s "list-panes -F '#{session_name}:#{window_index}.#{pane_id}'")
      (is (search "alpha:0.1" *overlay*)
          "list-panes -F must include the first formatted pane")
      (is (search "alpha:0.2" *overlay*)
          "list-panes -F must include the second formatted pane")
      (is (null (search "[" *overlay*))
          "custom format output should replace the default pane listing"))))

(test run-command-line-list-panes-targets-window
  "%run-command-line list-panes -t lists panes from the target window."
  (with-fake-session (s :nwindows 2 :npanes 1)
    (let* ((wins   (session-windows s))
           (home   (first wins))
           (target (second wins)))
      (setf (window-name home) "home"
            (window-name target) "work")
      (let ((*overlay* nil))
        (cl-tmux::%run-command-line
         s "list-panes -t :work -F '#{window_name}:#{pane_id}'")
        (is (search "work:1" *overlay*)
            "list-panes -t must include panes from the target window")
        (is (null (search "home:1" *overlay*))
            "list-panes -t must not list the active window when another is targeted")))))

(test run-command-line-list-panes-all-sessions
  "%run-command-line list-panes -a lists panes across registered sessions."
  (with-fake-session (s1 :nwindows 1 :npanes 1)
    (let ((s2 (make-fake-session :nwindows 1 :npanes 1)))
      (setf (session-name s1) "alpha"
            (session-name s2) "beta")
      (let ((cl-tmux::*server-sessions*
              (list (cons (session-name s1) s1)
                    (cons (session-name s2) s2)))
            (*overlay* nil))
        (cl-tmux::%run-command-line
         s1 "list-panes -a -F '#{session_name}:#{pane_id}'")
        (is (search "alpha:1" *overlay*)
            "list-panes -a must include panes from the current session")
        (is (search "beta:1" *overlay*)
            "list-panes -a must include panes from other registered sessions")))))

(test run-command-line-list-panes-session-scope
  "%run-command-line list-panes -s lists panes across the target/current session."
  (with-fake-session (s :nwindows 2 :npanes 1)
    (let ((wins (session-windows s)))
      (setf (window-name (first wins)) "zero"
            (window-name (second wins)) "one")
      (let ((*overlay* nil))
        (cl-tmux::%run-command-line
         s "list-panes -s -F '#{window_name}:#{pane_id}'")
        (is (search "zero:1" *overlay*)
            "list-panes -s must include panes from the first window")
        (is (search "one:1" *overlay*)
            "list-panes -s must include panes from later windows")))))

;;; ── list-windows arg command ─────────────────────────────────────────────────

(test run-command-line-list-windows-format-uses-arg-handler
  "%run-command-line list-windows -F expands window formats through the arg handler."
  (with-fake-session (s :nwindows 2 :npanes 1)
    (setf (session-name s) "alpha")
    (let ((wins (session-windows s)))
      (setf (window-name (first wins)) "home"
            (window-name (second wins)) "work")
      (let ((*overlay* nil))
        (cl-tmux::%run-command-line
         s "list-windows -F '#{session_name}:#{window_name}'")
        (is (search "alpha:home" *overlay*)
            "list-windows -F must include the first formatted window")
        (is (search "alpha:work" *overlay*)
            "list-windows -F must include the second formatted window")))))

(test run-command-line-list-windows-targets-session
  "%run-command-line list-windows -t lists windows from the target session."
  (with-fake-session (s1 :nwindows 1 :npanes 1)
    (let ((s2 (make-fake-session :nwindows 1 :npanes 1)))
      (setf (session-name s1) "alpha"
            (session-name s2) "beta"
            (window-name (first (session-windows s1))) "home"
            (window-name (first (session-windows s2))) "work")
      (let ((cl-tmux::*server-sessions*
              (list (cons (session-name s1) s1)
                    (cons (session-name s2) s2)))
            (*overlay* nil))
        (cl-tmux::%run-command-line
         s1 "list-windows -t beta -F '#{session_name}:#{window_name}'")
        (is (search "beta:work" *overlay*)
            "list-windows -t must include windows from the target session")
        (is (null (search "alpha:home" *overlay*))
            "list-windows -t must not list the current session when another is targeted")))))

(test run-command-line-list-windows-all-sessions
  "%run-command-line list-windows -a lists windows across registered sessions."
  (with-fake-session (s1 :nwindows 1 :npanes 1)
    (let ((s2 (make-fake-session :nwindows 1 :npanes 1)))
      (setf (session-name s1) "alpha"
            (session-name s2) "beta"
            (window-name (first (session-windows s1))) "home"
            (window-name (first (session-windows s2))) "work")
      (let ((cl-tmux::*server-sessions*
              (list (cons (session-name s1) s1)
                    (cons (session-name s2) s2)))
            (*overlay* nil))
        (cl-tmux::%run-command-line
         s1 "list-windows -a -F '#{session_name}:#{window_name}'")
        (is (search "alpha:home" *overlay*)
            "list-windows -a must include windows from the current session")
        (is (search "beta:work" *overlay*)
            "list-windows -a must include windows from other registered sessions")))))

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

(test dispatch-show-options-overlay-table
  ":show-window-options, :show-session-options, :list-clients, and :show-environment each produce a non-empty overlay."
  (dolist (cmd '(:show-window-options :show-session-options :list-clients :show-environment))
    (with-fake-session (s)
      (let ((*overlay* nil))
        (cl-tmux::dispatch-command s cmd nil)
        (is (and *overlay* (plusp (length *overlay*)))
            "~A must produce a non-empty overlay" cmd)))))

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

(test dispatch-lock-server-locks-all-sessions
  ":lock-server sets locked-p on all sessions."
  (with-fake-session (s1)
    (let ((s2 (make-fake-session)))
      (let ((cl-tmux::*server-sessions*
             (list (cons "a" s1) (cons "b" s2))))
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

;;; ── command alias dispatch ───────────────────────────────────────────────────

(test command-alias-dispatch-expands-and-runs
  "A registered command alias is expanded and dispatched."
  (with-fake-session (s)
    (let ((cl-tmux/options:*command-aliases* (make-hash-table :test #'equal)))
      (cl-tmux/options:register-command-alias "nw" "new-window")
      (when (pty-available-p)
        (let* ((win    (cl-tmux/model:session-active-window s))
               (before (length (cl-tmux/model:session-windows s))))
          (cl-tmux::%run-command-line s "nw")
          (stop-cl-tmux-threads)
          (is (> (length (cl-tmux/model:session-windows s)) before)
              "alias 'nw' → new-window must create a new window"))))))

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
