(in-package #:cl-tmux/test)

;;;; Dispatch session tests — part E: named-command table, select-layout arg,
;;;; set-option -u, list-panes, split-window, new-window, show-window/session-options,
;;;; server management, dynamic prefix key, command alias, new-window/split-window -d.

(in-suite dispatch-suite)

(test named-command-break-pane-is-recognized
  "%dispatch-named-command recognizes 'break-pane' and breaks the pane into a window."
  (with-fake-two-pane-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%dispatch-named-command s "break-pane")
      (is (null *overlay*)
          "break-pane must be a recognized command name")
      (is (= 2 (length (session-windows s)))
          "break-pane must move the pane into a second window"))))

(test named-command-unknown-shows-error-overlay
  "%dispatch-named-command shows an unknown-command overlay for an unrecognized name."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%dispatch-named-command s "no-such-command-xyz")
      (assert-overlay-contains "unknown command" *overlay*
                               "an unknown command name must show the unknown-command overlay"))))

;;; ── select-layout arg command ────────────────────────────────────────────────

(test run-command-line-select-layout-known-layouts-do-not-error
  "select-layout applies named layouts without error and leaves the pane count unchanged.
   Each row: (npanes layout-name description)."
  (dolist (row '((2 "even-horizontal" "even-horizontal must not change pane count")
                 (3 "main-horizontal" "main-horizontal must not change pane count")))
    (destructuring-bind (npanes layout desc) row
      (with-fake-session (s :nwindows 1 :npanes npanes)
        (cl-tmux::%run-command-line s (format nil "select-layout ~A" layout))
        (is (= npanes
               (length (cl-tmux/model:window-panes
                        (cl-tmux/model:session-active-window s))))
            desc)))))

(test run-command-line-select-layout-unknown-is-noop
  "%run-command-line select-layout with an unknown name is a no-op (no error)."
  (with-fake-two-pane-session (s)
    (is (null (cl-tmux::%run-command-line s "select-layout bogus-layout"))
        "unknown layout name must not raise an error")))

;;; ── set-option -u (unset) ────────────────────────────────────────────────────

(test run-command-line-set-option-unset
  "%run-command-line 'set-option -u <name>' removes the option from *global-options*."
  (let ((cl-tmux/options:*global-options*
         (let ((h (make-hash-table :test #'equal)))
           (setf (gethash "status-left" h) "my-value")
           h)))
    (with-fake-session (s)
      (cl-tmux::%run-command-line s "set-option -u status-left")
      (is (not (gethash "status-left" cl-tmux/options:*global-options*))
          "set-option -u status-left must remove the key from *global-options*"))))

(test set-option-w-unset-clears-window-local-not-global
  "set-window-option -u <opt> (= set-window-option -u) removes the WINDOW-local override, leaving the
   global value intact (scope-aware -u, was always unsetting global)."
  (let ((cl-tmux/options:*global-options* (make-hash-table :test #'equal)))
    (with-fake-session (s :nwindows 1)
      (let ((win (cl-tmux/model:session-active-window s)))
        (cl-tmux/options:set-option "mode-keys" "emacs")             ; global
        (cl-tmux/options:set-option-for-window "mode-keys" "vi" win) ; window-local
        (cl-tmux::%run-command-line s "set-window-option -u mode-keys")
        (is (not (nth-value 1 (gethash "mode-keys"
                                       (cl-tmux/model:window-local-options win))))
            "set-window-option -u must remove the window-local override")
        (is (equal "emacs" (cl-tmux/options:get-option "mode-keys"))
            "the global value must remain untouched")))))

(test set-option-a-w-appends-to-window-local-value
  "set-option -aw <opt> X appends to the WINDOW-local value (scope-aware -a, was always
   appending to the global store)."
  (let ((cl-tmux/options:*global-options* (make-hash-table :test #'equal)))
    (with-fake-session (s :nwindows 1)
      (let ((win (cl-tmux/model:session-active-window s)))
        (cl-tmux/options:set-option-for-window "@x" "ab" win)
        (cl-tmux::%run-command-line s "set-option -aw @x cd")
        (is (equal "abcd" (cl-tmux/options:get-option-for-window "@x" win))
            "set-option -aw must append to the window-local value")
        (is (not (nth-value 1 (gethash "@x" cl-tmux/options:*global-options*)))
            "the global store must not gain the option")))))

;;; ── list-clients arg command ─────────────────────────────────────────────────

(test run-command-line-list-clients-format-uses-client-records
  "%run-command-line list-clients -F expands client formats through the arg handler."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (with-session-and-window-names (s "alpha")
      (let ((cl-tmux::*clients*
              (list (cl-tmux::%make-client-conn :rows 50 :cols 200)
                    (cl-tmux::%make-client-conn :rows 24 :cols 80))))
        (with-run-command-line-overlay
            (s "list-clients -F '#{client_name}:#{client_width}x#{client_height}:#{client_session}'")
          (assert-overlay-contains-all
           '("client-0:200x50:alpha" "client-1:80x24:alpha")
           *overlay*
           "list-clients -F must include both attached clients"))))))

(test run-command-line-list-clients-default-format-uses-session-name
  "%run-command-line list-clients without -F uses the current session name and client geometry."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (with-session-and-window-names (s "alpha")
      (let ((cl-tmux::*clients*
              (list (cl-tmux::%make-client-conn :rows 50 :cols 200))))
        (with-run-command-line-overlay
            (s "list-clients")
          (assert-overlay-contains "client-0: alpha [200x50]" *overlay*
                                   "list-clients default output must use the session name and width x height"))))))

(test run-command-line-list-clients-local-fallback
  "%run-command-line list-clients lists a local pseudo-client without attached sockets."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (with-session-and-window-names (s "alpha")
      (let ((cl-tmux::*clients* nil)
            (cl-tmux::*term-cols* 132)
            (cl-tmux::*term-rows* 43))
        (with-run-command-line-overlay
            (s "list-clients -F '#{client_name}:#{client_width}x#{client_height}:#{client_session}'")
          (assert-overlay-contains "local:132x43:alpha" *overlay*
                                   "list-clients must expose a deterministic local fallback client"))))))

(test run-command-line-list-clients-target-session-context
  "%run-command-line list-clients -t uses the target session in client formats."
  (with-fake-session (s1 :nwindows 1 :npanes 1)
    (let ((s2 (make-fake-session :nwindows 1 :npanes 1)))
      (with-session-and-window-names (s1 "alpha")
        (with-session-and-window-names (s2 "beta")
          (let ((cl-tmux::*clients* nil))
            (with-registered-sessions (("alpha" s1) ("beta" s2))
              (with-run-command-line-overlay
                  (s1 "list-clients -t beta -F '#{client_session}:#{client_name}'")
                (assert-overlay-contains "beta:local" *overlay*
                                         "list-clients -t must use the target session for format expansion")))))))))

(test run-command-line-list-clients-filter-matches-expanded-rows
  "list-clients -f keeps rows whose expanded format contains the filter string."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (with-session-and-window-names (s "alpha")
      (let ((cl-tmux::*clients*
              (list (cl-tmux::%make-client-conn :rows 50 :cols 200)
                    (cl-tmux::%make-client-conn :rows 24 :cols 80))))
        (with-run-command-line-overlay
            (s "list-clients -f client-1 -F '#{client_name}:#{client_width}'")
          (assert-overlay-not-contains "client-0:200" *overlay*
                                       "list-clients -f must remove non-matching expanded rows")
          (assert-overlay-contains "client-1:80" *overlay*
                                   "list-clients -f must keep matching expanded rows"))))))

;;; ── list-sessions arg command ────────────────────────────────────────────────

(test run-command-line-list-sessions-filter-matches-expanded-rows
  "list-sessions -f keeps rows whose expanded format contains the filter string."
  (with-fake-session (s1 :nwindows 1 :npanes 1)
    (let ((s2 (make-fake-session :nwindows 1 :npanes 1)))
      (with-session-and-window-names (s1 "alpha")
        (with-session-and-window-names (s2 "beta")
          (with-registered-sessions (("alpha" s1) ("beta" s2))
            (with-run-command-line-overlay
                (s1 "list-sessions -f beta -F '#{session_name}'")
              (assert-overlay-contains-all
               '("beta")
               *overlay*
               "list-sessions -f must keep matching expanded rows")
              (assert-overlay-not-contains "alpha" *overlay*
                                           "list-sessions -f must remove non-matching expanded rows"))))))))

(test non-empty-overlay-lines-drop-blank-rows
  "%non-empty-overlay-lines splits text and removes empty rows."
  (is (equal '("alpha" "beta")
             (cl-tmux::%non-empty-overlay-lines (format nil "alpha~%~%beta~%")))
      "blank overlay rows must be removed before filtering"))

;;; ── list-panes arg command ───────────────────────────────────────────────────

(test run-command-line-list-panes-shows-overlay
  "%run-command-line list-panes shows an overlay listing panes."
  (with-fake-two-pane-session (s)
    (with-run-command-line-overlay (s "list-panes")
      (is (and *overlay* (plusp (length *overlay*)))
          "list-panes must produce a non-empty overlay"))))

(test run-command-line-list-panes-format-uses-arg-handler
  "%run-command-line list-panes -F expands pane formats through the arg handler."
  (with-fake-two-pane-session (s)
    (with-session-name (s "alpha")
      (with-run-command-line-overlay
          (s "list-panes -F '#{session_name}:#{window_index}.#{pane_id}'")
        (assert-overlay-uses-custom-format
         '("alpha:0.1" "alpha:0.2")
         *overlay*
         "list-panes -F must include both formatted panes")))))

(test run-command-line-list-panes-targets-window
  "%run-command-line list-panes -t lists panes from the target window."
  (with-fake-session (s :nwindows 2 :npanes 1)
    (let* ((wins   (session-windows s))
           (home   (first wins))
           (target (second wins)))
      (with-window-names (s "home" "work")
        (with-run-command-line-overlay
            (s "list-panes -t :work -F '#{window_name}:#{pane_id}'")
          (assert-overlay-contains "work:1" *overlay*
                                   "list-panes -t must include panes from the target window")
          (assert-overlay-not-contains "home:1" *overlay*
                                       "list-panes -t must not list the active window when another is targeted"))))))

(test run-command-line-list-panes-all-sessions
  "%run-command-line list-panes -a lists panes across registered sessions."
  (with-fake-session (s1 :nwindows 1 :npanes 1)
    (let ((s2 (make-fake-session :nwindows 1 :npanes 1)))
      (with-session-name (s1 "alpha")
        (with-session-name (s2 "beta")
          (with-registered-sessions (("alpha" s1) ("beta" s2))
            (with-run-command-line-overlay
                (s1 "list-panes -a -F '#{session_name}:#{pane_id}'")
               (assert-overlay-contains-all
                '("alpha:1" "beta:1")
                *overlay*
                "list-panes -a must include panes from every registered session"))))))))

(test run-command-line-list-panes-session-scope
  "%run-command-line list-panes -s lists panes across the target/current session."
  (with-fake-session (s :nwindows 2 :npanes 1)
    (let ((wins (session-windows s)))
      (with-window-names (s "zero" "one")
        (with-run-command-line-overlay
            (s "list-panes -s -F '#{window_name}:#{pane_id}'")
          (assert-overlay-contains-all
           '("zero:1" "one:1")
           *overlay*
           "list-panes -s must include panes from every window"))))))

(test run-command-line-list-panes-filter-matches-expanded-rows
  "list-panes -f keeps rows whose expanded format contains the filter string."
  (with-fake-two-pane-session (s)
    (with-run-command-line-overlay
        (s "list-panes -f 2 -F '#{pane_id}'")
      (assert-overlay-not-contains "1" *overlay*
                                   "list-panes -f must remove non-matching expanded rows")
      (assert-overlay-contains "2" *overlay*
                               "list-panes -f must keep matching expanded rows"))))

;;; ── list-windows arg command ─────────────────────────────────────────────────

(test run-command-line-list-windows-format-uses-arg-handler
  "%run-command-line list-windows -F expands window formats through the arg handler."
  (with-fake-session (s :nwindows 2 :npanes 1)
    (with-session-and-window-names (s "alpha" "home" "work")
      (with-run-command-line-overlay
          (s "list-windows -F '#{session_name}:#{window_name}'")
        (assert-overlay-contains-all
         '("alpha:home" "alpha:work")
         *overlay*
         "list-windows -F must include both formatted windows")))))

(test run-command-line-list-windows-targets-session
  "%run-command-line list-windows -t lists windows from the target session."
  (with-fake-session (s1 :nwindows 1 :npanes 1)
    (let ((s2 (make-fake-session :nwindows 1 :npanes 1)))
      (with-session-and-window-names (s1 "alpha" "home")
        (with-session-and-window-names (s2 "beta" "work")
          (with-registered-sessions (("alpha" s1) ("beta" s2))
            (with-run-command-line-overlay
                (s1 "list-windows -t beta -F '#{session_name}:#{window_name}'")
              (assert-overlay-contains "beta:work" *overlay*
                                       "list-windows -t must include windows from the target session")
              (assert-overlay-not-contains "alpha:home" *overlay*
                                           "list-windows -t must not list the current session when another is targeted"))))))))

(test run-command-line-list-windows-all-sessions
  "%run-command-line list-windows -a lists windows across registered sessions."
  (with-fake-session (s1 :nwindows 1 :npanes 1)
    (let ((s2 (make-fake-session :nwindows 1 :npanes 1)))
      (with-session-and-window-names (s1 "alpha" "home")
        (with-session-and-window-names (s2 "beta" "work")
          (with-registered-sessions (("alpha" s1) ("beta" s2))
            (with-run-command-line-overlay
                (s1 "list-windows -a -F '#{session_name}:#{window_name}'")
               (assert-overlay-contains-all
                '("alpha:home" "beta:work")
                *overlay*
                "list-windows -a must include windows from every registered session"))))))))

(test run-command-line-list-windows-all-sessions-falls-back-to-current
  "%run-command-line list-windows -a still shows the current session when no sessions are registered."
  (with-fake-session (s :nwindows 1 :npanes 1)
    (with-session-and-window-names (s "alpha" "home")
      (with-empty-registry
        (with-run-command-line-overlay
            (s "list-windows -a -F '#{session_name}:#{window_name}'")
          (assert-overlay-contains "alpha:home" *overlay*
                                    "list-windows -a must keep the current session when no sessions are registered"))))))

(test run-command-line-list-windows-filter-matches-expanded-rows
  "list-windows -f keeps rows whose expanded format contains the filter string."
  (with-fake-session (s :nwindows 2 :npanes 1)
    (with-window-names (s "home" "work")
      (with-run-command-line-overlay
          (s "list-windows -f work -F '#{window_name}'")
        (assert-overlay-not-contains "home" *overlay*
                                     "list-windows -f must remove non-matching expanded rows")
        (assert-overlay-contains "work" *overlay*
                                 "list-windows -f must keep matching expanded rows")))))

(test run-command-line-list-commands-reject-unsupported-arguments
  "list-* arg commands reject unknown flags and extra positionals before
   producing rows."
  (with-fake-session (s :nwindows 2 :npanes 2)
    (with-session-and-window-names (s "alpha" "home" "work")
      (let ((cl-tmux::*clients* nil)
            (cases '(("list-sessions extra" "list-sessions: unsupported argument" " windows")
                     ("list-sessions -Z" "list-sessions: unsupported argument" " windows")
                     ("list-clients extra" "list-clients: unsupported argument" "local")
                     ("list-clients -Z" "list-clients: unsupported argument" "local")
                     ("list-windows extra" "list-windows: unsupported argument" "home")
                     ("list-windows -Z" "list-windows: unsupported argument" "home")
                     ("list-panes extra" "list-panes: unsupported argument" " (active)")
                     ("list-panes -Z" "list-panes: unsupported argument" " (active)"))))
        (with-command-line-rejection-cases (line message row-token cases)
          (cl-tmux::%run-command-line s line)
          (assert-overlay-rejects-before-row *overlay* message row-token line))))))

(test filtered-overlay-lines-string-keeps-matching-rows-in-order
  "%filtered-overlay-lines-string returns only matching rows, in order."
  (is (string= (format nil "work~%workbench")
               (cl-tmux::%filtered-overlay-lines-string
                '("home" "work" "workbench")
                "work"))
      "filtered overlay rows must preserve ordering and exclude non-matches"))

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
  "split-window with no flags, -h, or -p each adds one pane to the active window.
   Each row: (command message)."
  (dolist (row '(("split-window"       "split-window must add a pane to the active window")
                 ("split-window -h"    "split-window -h must add a pane to the active window")
                 ("split-window -p 30" "split-window -p 30 must add a pane to the active window")))
    (destructuring-bind (cmd msg) row
      (with-pty-command-increasing-count
          (s cmd
             :count-form (length (cl-tmux/model:window-panes
                                  (cl-tmux/model:session-active-window s)))
             :count-context msg)))))

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
