(in-package #:cl-tmux/test)

;;;; Arg-taking command dispatch tests
;;;;  (src/dispatch-commands.lisp, dispatch-commands-lifecycle.lisp,
;;;;   dispatch-commands-pane.lisp, dispatch-commands-shell.lisp,
;;;;   dispatch-commands-auto.lisp, dispatch-commands-runner.lisp).

(in-suite dispatch-suite)

;;; ── %parse-command-flags + -t target commands ───────────────────────────────

(test parse-command-flags-value-and-boolean
  "%parse-command-flags separates -t<value> flags, boolean flags, and positionals."
  (flet ((flags (toks vf)
           (multiple-value-bind (f p) (cl-tmux::%parse-command-flags toks vf)
             (declare (ignore p)) f))
         (pos (toks vf)
           (multiple-value-bind (f p) (cl-tmux::%parse-command-flags toks vf)
             (declare (ignore f)) p)))
    (is (equal "2" (cdr (assoc #\t (flags '("-t" "2") "t"))))
        "-t 2 (separate) → value \"2\"")
    (is (equal "2" (cdr (assoc #\t (flags '("-t2") "t"))))
        "-t2 (attached) → value \"2\"")
    (is (eq t (cdr (assoc #\d (flags '("-d") "t"))))
        "-d (not a value flag) → boolean T")
    (is (equal '("foo" "bar") (pos '("-d" "foo" "-t" "2" "bar") "t"))
        "non-flag tokens are positionals in order")))

(test run-command-line-select-window-by-number
  "'select-window -t N' selects the window whose window-id is N."
  (with-fake-session (s :nwindows 3)
    (cl-tmux::%run-command-line s "select-window -t 2")
    (is (= 2 (window-id (session-active-window s)))
        "select-window -t 2 must activate window-id 2")))

(test run-command-line-select-window-by-name
  "'select-window -t <name>' selects the window with that (non-numeric) name."
  (with-fake-session (s :nwindows 2)
    (setf (window-name (second (session-windows s))) "alpha")
    (cl-tmux::%run-command-line s "select-window -t alpha")
    (is (string= "alpha" (window-name (session-active-window s)))
        "select-window -t alpha must activate the window named 'alpha'")))

(test run-command-line-select-window-T-toggles-to-last
  "'select-window -T -t N' toggles to the last window when already on window N,
   but selects N normally when not currently on it."
  (with-fake-session (s :nwindows 2)
    (let* ((w0 (first  (cl-tmux/model:session-windows s)))
           (w1 (second (cl-tmux/model:session-windows s))))
      ;; session-last-window is recency-based (window-last-active-time, 1s
      ;; granularity); seed distinct OLD stamps so the two same-second selects
      ;; below produce an unambiguous last-window order (w0 older than w1's NOW).
      (setf (cl-tmux/model:window-last-active-time w0) 100
            (cl-tmux/model:window-last-active-time w1) 50)
      ;; From w0, -T -t 1 is NOT on the target → select w1 normally.
      (cl-tmux::%run-command-line s "select-window -T -t 1")
      (is (eq w1 (session-active-window s))
          "-T when not on the target must select the target (w1)")
      ;; Now on w1 (w0 is last); -T -t 1 IS on the target → toggle to last (w0).
      (cl-tmux::%run-command-line s "select-window -T -t 1")
      (is (eq w0 (session-active-window s))
          "-T when already on the target must toggle to the last window (w0)"))))

(test run-command-line-select-pane-by-id-table
  "'select-pane -t 2' and 'select-pane -t %2' both activate pane-id 2."
  (dolist (cmd '("select-pane -t 2" "select-pane -t %2"))
    (with-fake-session (s :nwindows 1 :npanes 2)
      (let ((win (session-active-window s)))
        (is (= 1 (pane-id (window-active-pane win))) "~A: pane 1 is initially active" cmd)
        (cl-tmux::%run-command-line s cmd)
        (is (= 2 (pane-id (window-active-pane win))) "~A must activate pane-id 2" cmd)))))

(test run-command-line-select-pane-l-selects-last
  "'select-pane -l' returns to the previously active pane."
  (with-fake-session (s :nwindows 1 :npanes 2)
    (let* ((win (session-active-window s)))
      ;; Start on pane 1, move to pane 2 (pane 1 becomes last-active).
      (cl-tmux::%run-command-line s "select-pane -t 2")
      (is (= 2 (pane-id (window-active-pane win))) "now on pane 2")
      (cl-tmux::%run-command-line s "select-pane -l")
      (is (= 1 (pane-id (window-active-pane win)))
          "select-pane -l must return to the previously active pane (1)"))))

(test run-command-line-select-pane-t-T-titles-target-pane
  "'select-pane -t N -T title' sets pane N's title, NOT the active pane's."
  (with-fake-session (s :nwindows 1 :npanes 2)
    (let* ((win (session-active-window s))
           (p1  (window-active-pane win))
           (p2  (find 2 (window-panes win) :key #'pane-id)))
      (cl-tmux::%run-command-line s "select-pane -t 2 -T mytitle")
      (is (string= "mytitle" (cl-tmux/model:pane-title p2))
          "select-pane -t 2 -T must set pane 2's title")
      (is (not (string= "mytitle" (cl-tmux/model:pane-title p1)))
          "the active pane (1) title must be unchanged"))))

(test run-command-line-select-pane-t-d-disables-target-pane-input
  "'select-pane -t N -d' disables input on pane N, NOT the active pane."
  (with-fake-session (s :nwindows 1 :npanes 2)
    (let* ((win (session-active-window s))
           (p1  (window-active-pane win))
           (p2  (find 2 (window-panes win) :key #'pane-id)))
      (cl-tmux::%run-command-line s "select-pane -t 2 -d")
      (is-true  (cl-tmux/model:pane-input-disabled p2) "pane 2 input disabled")
      (is-false (cl-tmux/model:pane-input-disabled p1) "active pane 1 unaffected"))))

(test run-command-line-select-pane-M-clears-mark
  "'select-pane -M' on a marked pane clears the server-wide mark (toggle)."
  (with-fake-session (s :nwindows 1 :npanes 2)
    (let* ((win (session-active-window s)))
      (let ((ap (window-active-pane win)))
        (cl-tmux::dispatch-command s :mark-pane nil)
        (is (pane-marked ap) "pane must be marked before select-pane -M")
        (cl-tmux::%run-command-line s "select-pane -M")
        (is (null (pane-marked ap))
            "select-pane -M must clear the pane mark")))))

;;; ── kill-window / kill-pane with -t target ───────────────────────────────────

(test run-command-line-kill-window-by-target
  "'kill-window -t N' kills the window whose window-id is N."
  (with-fake-session (s :nwindows 3)
    (cl-tmux::%run-command-line s "kill-window -t 1")
    (is (= 2 (length (session-windows s))) "one window must be removed")
    (is (null (find 1 (session-windows s) :key #'window-id))
        "window-id 1 must be gone")))

(test run-command-line-kill-pane-by-target
  "'kill-pane -t N' kills the pane with pane-id N in the active window."
  (with-fake-session (s :nwindows 1 :npanes 2)
    (let* ((win (session-active-window s)))   ; pane-ids 1,2
      (cl-tmux::%run-command-line s "kill-pane -t 2")
      (is (= 1 (length (window-panes win))) "one pane must be removed")
      (is (null (find 2 (window-panes win) :key #'pane-id))
          "pane-id 2 must be gone"))))

(test run-command-line-kill-pane-invalid-target-is-noop
  "'kill-pane -t <nonexistent>' must NOT kill the active pane by accident."
  (with-fake-session (s :nwindows 1 :npanes 2)
    (let* ((win (session-active-window s)))
      (cl-tmux::%run-command-line s "kill-pane -t 99")
      (is (= 2 (length (window-panes win)))
          "no pane may be removed for a -t target that matches nothing"))))

(test run-command-line-kill-window-no-arg-kills-active
  "'kill-window' with no -t kills the active window (name-table fallthrough)."
  (with-fake-session (s :nwindows 2)
    (let* ((active (session-active-window s)))
      (cl-tmux::%run-command-line s "kill-window")
      (is (= 1 (length (session-windows s))) "the active window must be removed")
      (is (null (find active (session-windows s)))
          "the previously active window must be gone"))))

;;; ── swap-window -s -t (two value flags) ──────────────────────────────────────

(test run-command-line-swap-window-exchanges-indices
  "'swap-window -s X -t Y' exchanges the two windows' INDEX NUMBERS (ids): the
   content at X and Y trade indices, the list stays sorted by id."
  (with-fake-session (s :nwindows 3)
    (let* ((w0 (find 0 (session-windows s) :key #'window-id))
           (w1 (find 1 (session-windows s) :key #'window-id))
           (w2 (find 2 (session-windows s) :key #'window-id)))
      (cl-tmux::%run-command-line s "swap-window -s 0 -t 2")
      (is (= 2 (window-id w0)) "window formerly #0 now has index 2")
      (is (= 0 (window-id w2)) "window formerly #2 now has index 0")
      (is (= 1 (window-id w1)) "the middle window keeps index 1")
      (is (equal '(0 1 2) (mapcar #'window-id (session-windows s)))
          "the window list stays sorted by index"))))

(test run-command-line-swap-window-default-source-is-active
  "'swap-window -t Y' uses the active window as the source."
  (with-fake-session (s :nwindows 2)
    (let* ((active (session-active-window s))
           (w1     (find 1 (session-windows s) :key #'window-id)))
      (cl-tmux::%run-command-line s "swap-window -t 1")
      (is (= 1 (window-id active)) "the active window's index becomes 1")
      (is (= 0 (window-id w1)) "the other window's index becomes 0"))))

(test run-command-line-swap-window-unknown-target-is-noop
  "'swap-window -s 0 -t 99' (no such dst) leaves the window indices unchanged."
  (with-fake-session (s :nwindows 3)
    (let* ((ids-before  (mapcar #'window-id (session-windows s))))
      (cl-tmux::%run-command-line s "swap-window -s 0 -t 99")
      (is (equal ids-before (mapcar #'window-id (session-windows s)))
          "a -t target that matches nothing must not change indices"))))

;;; ── arg-taking key bindings + source-file ────────────────────────────────────

(test dispatch-prefix-bound-command-line-runs
  "A key bound to a command line runs it: bind X display-message hi, then prefix+X
   shows 'hi' in an overlay (verifies dispatch-prefix-command's token-list path)."
  (with-isolated-config
    (with-fake-session (s)
      (let ((*overlay* nil))
        (cl-tmux/config:apply-config-directive '("bind" "X" "display-message" "hi"))
        (cl-tmux::dispatch-prefix-command s (char-code #\X))
        (is (overlay-active-p) "the bound display-message must open an overlay")
        (let ((text (format nil "~{~A~%~}" (overlay-lines))))
          (is (search "hi" text)
              "overlay must contain the bound command's output 'hi' (got ~S)" text))))))

(test cmd-source-file-loads-config-file
  "source-file <path> loads the file and applies its directives (end-to-end with
   the set -g fix)."
  (with-isolated-options ()
    (let ((path (format nil "/tmp/cl-tmux-srcfile-~D.conf" (get-universal-time))))
      (unwind-protect
           (progn
             (with-open-file (out path :direction :output :if-exists :supersede
                                       :if-does-not-exist :create)
               (write-line "set -g status off" out))
             (cl-tmux::%run-command-line (make-fake-session)
                                         (format nil "source-file ~A" path))
             (is (string= "off" (cl-tmux/options:get-option "status"))
                 "source-file must apply 'set -g status off' from the file"))
        (ignore-errors (delete-file path))))))

(test cmd-source-file-missing-path-no-crash
  "source-file on a non-existent path is a safe no-op (no error signalled)."
  (finishes (cl-tmux::%run-command-line (make-fake-session)
                                        "source-file /no/such/cl-tmux-file.conf")
            "source-file on a missing file must not signal"))

;;; ── move-window -t <n> (renumber the active window) ──────────────────────────

(test run-command-line-move-window-to-free-number
  "'move-window -t N' renumbers the active window to window-id N when N is free."
  (with-fake-session (s :nwindows 2)
    (let* ((win (session-active-window s)))
      (cl-tmux::%run-command-line s "move-window -t 5")
      (is (= 5 (window-id win))
          "active window must be renumbered to window-id 5"))))

(test run-command-line-move-window-to-taken-number-shifts-up
  "'move-window -t N' onto a taken index moves the window there and shifts the
   occupant up (tmux winlink_shuffle_up) — not a silent no-op."
  (with-fake-session (s :nwindows 2)
    (let* ((win (session-active-window s))
           (w1  (find 1 (session-windows s) :key #'window-id)))
      (cl-tmux::%run-command-line s "move-window -t 1")   ; 1 is taken
      (is (= 1 (window-id win))
          "the active window takes the requested index 1")
      (is (= 2 (window-id w1))
          "the window formerly at 1 shifts up to 2"))))

;;; ── if-shell -F <cond> <then> [<else>] (format-conditional) ──────────────────

(test run-command-line-if-shell-F-true-runs-then
  "if-shell -F with a truthy condition runs the THEN command line."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line s "if-shell -F 1 \"display-message yes\"")
      (is (overlay-active-p) "truthy if-shell -F must run THEN (overlay opens)")
      (is (search "yes" (format nil "~{~A~%~}" (overlay-lines)))
          "overlay must show the THEN command's output"))))

(test run-command-line-if-shell-F-false-runs-else
  "if-shell -F with a falsey condition (\"0\") runs the ELSE command line."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line
       s "if-shell -F 0 \"display-message yes\" \"display-message no\"")
      (is (search "no" (format nil "~{~A~%~}" (overlay-lines)))
          "falsey if-shell -F must run ELSE, not THEN"))))

(test run-command-line-if-shell-F-format-condition
  "if-shell -F evaluates a #{...} format as its condition.  A non-zero value is
   truthy.  We use #{window_count}=1 (truthy) rather than #{window_index} (which
   equals the window id; with base-index=0 the first window has id 0 = falsey)."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line
       s "if-shell -F \"#{window_count}\" \"display-message named\"")
      (is (search "named" (format nil "~{~A~%~}" (overlay-lines)))
          "a non-zero #{window_count} (1) must be truthy → THEN"))))

(test run-command-line-if-shell-F-empty-condition-no-then
  "if-shell -F with an empty condition and no else runs nothing."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line s "if-shell -F \"\" \"display-message x\"")
      (is (not (overlay-active-p))
          "an empty (falsey) condition with no else must not run THEN"))))

;;; ── %dispatch-named-command helper ──────────────────────────────────────────

(test dispatch-named-command-new-window
  "%dispatch-named-command \"next-window\" selects the next window."
  ;; Use next-window (no fork/no thread) to avoid leaking reader threads
  ;; that would prevent later PTY tests from forking.
  (with-fake-session (s :nwindows 2)
    (let ((*overlay* nil))
      (cl-tmux::%dispatch-named-command s "next-window")
      (is-true cl-tmux::*dirty*
               "%dispatch-named-command 'next-window' must mark *dirty*")
      (is (eq (second (session-windows s)) (session-active-window s))
          "next-window must switch to the second window"))))

(test dispatch-named-command-unknown-shows-overlay
  "%dispatch-named-command with an unrecognized name shows an overlay."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%dispatch-named-command s "totally-unknown-xyz")
      (is (overlay-active-p) "unknown command must open an error overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "unknown command" text) "overlay must mention 'unknown command'")
        (is (search "totally-unknown-xyz" text)
            "overlay must include the bad command name")))))

;;; ── :show-messages dispatch ──────────────────────────────────────────────────

(test dispatch-show-messages-empty-log-shows-overlay
  ":show-messages with empty *message-log* opens an overlay saying '(no messages)'."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux::*message-log* nil))
      (cl-tmux::dispatch-command s :show-messages nil)
      (is (overlay-active-p) ":show-messages must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "no messages" text)
            "overlay must say '(no messages)' when log is empty")))))

(test dispatch-show-messages-populated-log-shows-entries
  ":show-messages with entries in *message-log* lists them."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux::*message-log* (list (cons 0 "hello") (cons 1 "world"))))
      (cl-tmux::dispatch-command s :show-messages nil)
      (is (overlay-active-p) ":show-messages must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "hello" text) "overlay must contain 'hello'")
        (is (search "world" text) "overlay must contain 'world'")))))

(test run-command-line-show-messages-accepts-flags
  "show-messages [-JT] [-t target-client] is reachable from the command line."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux::*message-log* (list (cons 0 "alpha") (cons 1 "beta"))))
      (cl-tmux::%run-command-line s "show-messages -J -t client0")
      (is (overlay-active-p)
          "show-messages with -J/-t must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "alpha" text) "overlay must contain 'alpha'")
        (is (search "beta" text) "overlay must contain 'beta'")))))

(test run-command-line-showmsgs-alias-accepts-terminal-flag
  "showmsgs -T accepts tmux's terminal debug flag and still shows messages."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux::*message-log* (list (cons 0 "terminal-ish"))))
      (cl-tmux::%run-command-line s "showmsgs -T")
      (is (overlay-active-p)
          "showmsgs alias with -T must open an overlay")
      (let ((text (format nil "~{~A~%~}" (overlay-lines))))
        (is (search "terminal-ish" text)
            "overlay must contain the logged message")))))
