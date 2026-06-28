(in-package #:cl-tmux/test)

;;;; Arg-taking command dispatch tests
;;;;  (src/dispatch-commands.lisp, dispatch-commands-lifecycle.lisp,
;;;;   dispatch-commands-pane.lisp, dispatch-commands-shell.lisp,
;;;;   dispatch-commands-auto.lisp, dispatch-commands-runner.lisp).

(in-suite dispatch-suite)

;;; ── %parse-command-flags + -t target commands ───────────────────────────────

(test parse-command-flags-value-and-boolean
  "%parse-command-flags separates -t<value> flags, boolean flags, and positionals."
  (multiple-value-bind (flags positionals)
      (cl-tmux::%parse-command-flags '("-t" "2") "t")
    (declare (ignore positionals))
    (is (equal "2" (alist-value #\t flags))
        "-t 2 (separate) → value \"2\""))
  (multiple-value-bind (flags positionals)
      (cl-tmux::%parse-command-flags '("-t2") "t")
    (declare (ignore positionals))
    (is (equal "2" (alist-value #\t flags))
        "-t2 (attached) → value \"2\""))
  (multiple-value-bind (flags positionals)
      (cl-tmux::%parse-command-flags '("-d") "t")
    (declare (ignore positionals))
    (is (eq t (alist-value #\d flags))
        "-d (not a value flag) → boolean T"))
  (multiple-value-bind (flags positionals)
      (cl-tmux::%parse-command-flags '("-d" "foo" "-t" "2" "bar") "t")
    (declare (ignore flags))
    (is (equal '("foo" "bar") positionals)
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

(test run-command-line-select-window-rejects-unsupported-arguments
  "select-window rejects unknown flags and positional tokens before changing windows."
  (dolist (command '("select-window -n extra"
                     "select-window -x"
                     "select-window -t 2 extra"))
    (with-fake-session (s :nwindows 2)
      (let ((initial (session-active-window s))
            (*overlay* nil))
        (is (null (cl-tmux::%run-command-line s command))
            "~A must be rejected" command)
        (is (eq initial (session-active-window s))
            "~A must not change the active window" command)
        (assert-overlay-contains "unsupported argument"
                                  (overlay-lines) command)))))

(test run-command-line-select-pane-by-id-table
  "'select-pane -t 2' and 'select-pane -t %2' both activate pane-id 2."
  (dolist (cmd '("select-pane -t 2" "select-pane -t %2"))
    (with-fake-two-pane-session (s)
      (let ((win (session-active-window s)))
        (is (= 1 (pane-id (window-active-pane win))) "~A: pane 1 is initially active" cmd)
        (cl-tmux::%run-command-line s cmd)
        (is (= 2 (pane-id (window-active-pane win))) "~A must activate pane-id 2" cmd)))))

(test run-command-line-select-pane-l-selects-last
  "'select-pane -l' returns to the previously active pane."
  (with-fake-two-pane-session (s)
    (let* ((win (session-active-window s)))
      ;; Start on pane 1, move to pane 2 (pane 1 becomes last-active).
      (cl-tmux::%run-command-line s "select-pane -t 2")
      (is (= 2 (pane-id (window-active-pane win))) "now on pane 2")
      (cl-tmux::%run-command-line s "select-pane -l")
      (is (= 1 (pane-id (window-active-pane win)))
          "select-pane -l must return to the previously active pane (1)"))))

(test run-command-line-select-pane-t-T-titles-target-pane
  "'select-pane -t N -T title' sets pane N's title, NOT the active pane's."
  (with-fake-two-pane-session (s)
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
  (with-fake-two-pane-session (s)
    (let* ((win (session-active-window s))
           (p1  (window-active-pane win))
           (p2  (find 2 (window-panes win) :key #'pane-id)))
      (cl-tmux::%run-command-line s "select-pane -t 2 -d")
      (is-true  (cl-tmux/model:pane-input-disabled p2) "pane 2 input disabled")
      (is-false (cl-tmux/model:pane-input-disabled p1) "active pane 1 unaffected"))))

(test run-command-line-select-pane-M-clears-mark
  "'select-pane -M' on a marked pane clears the server-wide mark (toggle)."
  (with-fake-two-pane-session (s)
    (let* ((win (session-active-window s)))
      (let ((ap (window-active-pane win)))
        (cl-tmux::dispatch-command s :mark-pane nil)
        (is (pane-marked ap) "pane must be marked before select-pane -M")
        (cl-tmux::%run-command-line s "select-pane -M")
        (is (null (pane-marked ap))
            "select-pane -M must clear the pane mark")
        (is (null cl-tmux::*server-marked-pane*)
            "select-pane -M must also clear the server-wide marked pane")))))

(test run-command-line-select-pane-m-sets-server-marked-pane
  "'select-pane -m' marks the target pane server-wide (sets *server-marked-pane*)
   so join-pane/swap-pane can use it as the default source; re-marking the same
   pane toggles the mark off, matching tmux's server_set/clear_marked."
  (with-fake-two-pane-session (s)
    (let* ((win (session-active-window s))
           (ap (window-active-pane win))
           (cl-tmux::*server-marked-pane* nil))
      (cl-tmux::%run-command-line s "select-pane -m")
      (is (pane-marked ap) "select-pane -m must mark the pane")
      (is (eq ap cl-tmux::*server-marked-pane*)
          "select-pane -m must set the server-wide marked pane")
      (cl-tmux::%run-command-line s "select-pane -m")
      (is (null (pane-marked ap))
          "re-marking the same pane toggles the per-pane mark off")
      (is (null cl-tmux::*server-marked-pane*)
          "toggling off must clear the server-wide marked pane"))))

(test run-command-line-select-pane-rejects-unsupported-arguments
  "select-pane rejects unknown flags and positional tokens before mutating panes."
  (dolist (command '("select-pane -t 2 extra"
                     "select-pane -x"
                     "select-pane -d extra"
                     "select-pane -t 2 -T title extra"))
    (with-fake-two-pane-session (s)
      (let* ((win (session-active-window s))
             (initial (window-active-pane win))
             (target (find 2 (window-panes win) :key #'pane-id))
             (*overlay* nil))
        (is (null (cl-tmux::%run-command-line s command))
            "~A must be rejected" command)
        (is (eq initial (window-active-pane win))
            "~A must not change the active pane" command)
        (is-false (cl-tmux/model:pane-input-disabled target)
                  "~A must not disable target pane input" command)
        (is (string= "" (cl-tmux/model:pane-title target))
            "~A must not set the target pane title" command)
        (assert-overlay-contains "unsupported argument"
                                  (overlay-lines) command)))))

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
  (with-fake-two-pane-session (s)
    (let* ((win (session-active-window s)))   ; pane-ids 1,2
      (cl-tmux::%run-command-line s "kill-pane -t 2")
      (is (= 1 (length (window-panes win))) "one pane must be removed")
      (is (null (find 2 (window-panes win) :key #'pane-id))
          "pane-id 2 must be gone"))))

(test run-command-line-kill-pane-invalid-target-is-noop
  "'kill-pane -t <nonexistent>' must NOT kill the active pane by accident."
  (with-fake-two-pane-session (s)
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

(test run-command-line-kill-commands-reject-unsupported-arguments
  "kill-window and kill-pane reject unknown flags and extra positionals."
  (dolist (case '(("kill-window extra" "kill-window: unsupported argument" :windows)
                  ("kill-window -Z" "kill-window: unsupported argument" :windows)
                  ("kill-pane extra" "kill-pane: unsupported argument" :panes)
                  ("kill-pane -Z" "kill-pane: unsupported argument" :panes)))
    (destructuring-bind (cmd message kind) case
      (ecase kind
        (:windows
         (with-fake-session (s :nwindows 2)
           (let ((before (copy-list (session-windows s)))
                 (*overlay* nil))
             (cl-tmux::%run-command-line s cmd)
             (assert-overlay-contains message *overlay* cmd)
             (is (equal before (session-windows s))
                 "~A must not mutate the window list" cmd))))
        (:panes
         (with-fake-two-pane-session (s)
           (let* ((win (session-active-window s))
                  (before (copy-list (window-panes win)))
                  (*overlay* nil))
             (cl-tmux::%run-command-line s cmd)
             (assert-overlay-contains message *overlay* cmd)
             (is (equal before (window-panes win))
                 "~A must not mutate the pane list" cmd))))))))

(test run-command-line-link-commands-reject-unsupported-arguments
  "link-window and unlink-window reject unknown flags and extra positionals."
  (dolist (cmd '("link-window -t dst extra" "link-window -Z -t dst"))
    (with-fake-session (src :nwindows 1)
      (with-fake-session (dst :nwindows 1)
        (setf (session-name src) "src"
              (session-name dst) "dst")
        (let ((src-before (copy-list (session-windows src)))
              (dst-before (copy-list (session-windows dst)))
              (cl-tmux::*server-sessions* (list (cons "src" src) (cons "dst" dst)))
              (*overlay* nil))
          (cl-tmux::%run-command-line src cmd)
          (assert-overlay-contains "link-window: unsupported argument"
                                    *overlay* cmd)
          (is (equal src-before (session-windows src))
              "~A must not mutate the source window list" cmd)
          (is (equal dst-before (session-windows dst))
              "~A must not mutate the destination window list" cmd)))))

(test run-command-line-link-window-selects-linked-window-by-default
  "'link-window -s 0 -t dst -k' (no -d) makes the linked window current in dst."
  (with-fake-session (src :nwindows 1)
    (with-fake-session (dst :nwindows 1)
      (setf (session-name src) "src"
            (session-name dst) "dst")
      (let* ((src-win (session-active-window src))
             (cl-tmux::*server-sessions* (list (cons "src" src) (cons "dst" dst)))
             (*overlay* nil))
        (cl-tmux::%run-command-line src "link-window -s 0 -t dst -k")
        (is (eq src-win (session-active-window dst))
            "without -d the linked window becomes current in the destination")))))

(test run-command-line-link-window-d-keeps-dst-active-window
  "'link-window -d -s 0 -t dst -k' leaves the destination's active window unchanged."
  (with-fake-session (src :nwindows 1)
    (with-fake-session (dst :nwindows 2)
      (setf (session-name src) "src"
            (session-name dst) "dst")
      (let* ((dst-active (session-active-window dst))
             (cl-tmux::*server-sessions* (list (cons "src" src) (cons "dst" dst)))
             (*overlay* nil))
        (cl-tmux::%run-command-line src "link-window -d -s 0 -t dst -k")
        (is (eq dst-active (session-active-window dst))
            "-d must not change the destination's active window")))))
  (dolist (cmd '("unlink-window extra" "unlink-window -Z"))
    (with-fake-session (s :nwindows 2)
      (let ((before (copy-list (session-windows s)))
            (*overlay* nil))
        (cl-tmux::%run-command-line s cmd)
        (assert-overlay-contains "unlink-window: unsupported argument"
                                  *overlay* cmd)
        (is (equal before (session-windows s))
            "~A must not mutate the window list" cmd)))))

;;; ── swap-window -s -t (two value flags) ──────────────────────────────────────

(test run-command-line-swap-window-exchanges-indices
  "'swap-window -s X -t Y' exchanges the two windows' INDEX NUMBERS (ids): the
   content at X and Y trade indices, the list stays sorted by id."
  (with-fake-session (s :nwindows 3)
    (let* ((w0 (find 0 (session-windows s) :key #'window-id))
           (w1 (find 1 (session-windows s) :key #'window-id))
           (w2 (find 2 (session-windows s) :key #'window-id)))
      (cl-tmux::%run-command-line s "swap-window -s 0 -t 2")
      (dolist (row (list (list w0 2 "window formerly #0 now has index 2")
                         (list w2 0 "window formerly #2 now has index 0")
                         (list w1 1 "the middle window keeps index 1")))
        (destructuring-bind (win expected desc) row
          (is (= expected (window-id win)) "~A" desc)))
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

(test run-command-line-swap-window-selects-source-or-keeps-active
  "swap-window without -d selects the swapped source window; with -d the
   pre-selected window stays active.
   Each row: (pre-select-id command description)."
  (dolist (row '((0 "swap-window -s 0 -t 2"    "no -d: swapped source becomes active")
                 (1 "swap-window -d -s 0 -t 2" "-d: pre-selected window stays active")))
    (destructuring-bind (pre-id cmd desc) row
      (with-fake-session (s :nwindows 3)
        (let ((pre-win (find pre-id (session-windows s) :key #'window-id)))
          (session-select-window s pre-win)
          (cl-tmux::%run-command-line s cmd)
          (is (eq pre-win (session-active-window s)) desc))))))

(test run-command-line-swap-window-unknown-target-is-noop
  "'swap-window -s 0 -t 99' (no such dst) leaves the window indices unchanged."
  (with-fake-session (s :nwindows 3)
    (let* ((ids-before  (mapcar #'window-id (session-windows s))))
      (cl-tmux::%run-command-line s "swap-window -s 0 -t 99")
      (is (equal ids-before (mapcar #'window-id (session-windows s)))
          "a -t target that matches nothing must not change indices"))))

(test run-command-line-swap-window-rejects-unsupported-arguments
  "swap-window rejects unknown flags and extra positionals before swapping."
  (dolist (command '("swap-window extra"
                     "swap-window -Z"
                     "swap-window -s 0 -t 2 extra"))
    (with-fake-session (s :nwindows 3)
      (let ((before (mapcar #'window-id (session-windows s)))
            (*overlay* nil))
        (is (null (cl-tmux::%run-command-line s command))
            "~A must be rejected" command)
        (is (equal before (mapcar #'window-id (session-windows s)))
            "~A must not change any window indices" command)
        (assert-overlay-contains "swap-window: unsupported argument"
                                  (overlay-lines) command)))))

;;; ── arg-taking key bindings + source-file ────────────────────────────────────

(test dispatch-prefix-bound-command-line-runs
  "A key bound to a command line runs it: bind X display-message hi, then prefix+X
   shows 'hi' in an overlay (verifies dispatch-prefix-command's token-list path)."
  (with-isolated-config
    (with-fake-session (s)
      (let ((*overlay* nil))
        (cl-tmux/config:apply-config-directive '("bind" "X" "display-message" "hi"))
        (cl-tmux::dispatch-prefix-command s (char-code #\X))
        (assert-overlay-contains "hi" (overlay-lines)
                                  "the bound display-message")))))

(test cmd-source-file-loads-config-file
  "source-file <path> loads the file and applies its directives (end-to-end with
   the set -g fix)."
  (with-isolated-config
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

(test run-command-line-move-window-selects-active-window-variants
  "move-window without -d selects the moved window as active; with -d the
   original active window is preserved.
   Each row: (command detach-p description)."
  (dolist (row '(("move-window -s 0 -t 5"    nil "no -d: moved window becomes active")
                 ("move-window -d -s 0 -t 5" t   "-d: original window stays active")))
    (destructuring-bind (cmd detach-p desc) row
      (with-fake-session (s :nwindows 2)
        (let* ((win   (session-active-window s))
               (other (find 1 (session-windows s) :key #'window-id)))
          (session-select-window s other)
          (cl-tmux::%run-command-line s cmd)
          (is (eq (if detach-p other win) (session-active-window s))
              desc))))))

(test run-command-line-move-window-rejects-unsupported-arguments
  "move-window rejects unknown flags and extra positionals before moving."
  (dolist (command '("move-window extra"
                     "move-window -Z"
                     "move-window -s 0 -t 2 extra"))
    (with-fake-session (s :nwindows 3)
      (let ((before (mapcar #'window-id (session-windows s)))
            (active-before (session-active-window s))
            (*overlay* nil))
        (is (null (cl-tmux::%run-command-line s command))
            "~A must be rejected" command)
        (is (equal before (mapcar #'window-id (session-windows s)))
            "~A must not change any window indices" command)
        (is (eq active-before (session-active-window s))
            "~A must not change the active window" command)
        (assert-overlay-contains "move-window: unsupported argument"
                                  (overlay-lines) command)))))

;;; ── if-shell -F <cond> <then> [<else>] (format-conditional) ──────────────────

(test run-command-line-if-shell-F-overlay-table
  "if-shell -F with truthy/falsey/format conditions and accepted flags each produce
   the expected overlay fragment.  Each row: (command expected-fragment message)."
  (dolist (row '(("if-shell -F 1 \"display-message yes\""
                  "yes" "truthy if-shell -F")
                 ("if-shell -F 0 \"display-message yes\" \"display-message no\""
                  "no" "falsey if-shell -F")
                 ("if-shell -F \"#{window_count}\" \"display-message named\""
                  "named" "a non-zero #{window_count} (1)")
                 ("if-shell -b -t %1 -F 1 \"display-message if-target-ok\""
                  "if-target-ok" "if-shell -b/-t")))
    (destructuring-bind (cmd expected msg) row
      (with-fake-session (s)
        (let ((*overlay* nil))
          (cl-tmux::%run-command-line s cmd)
          (assert-overlay-contains expected (overlay-lines) msg))))))

(test run-command-line-if-shell-F-uses-target-context
  "if-shell -t <pane> -F evaluates the condition in the target pane's context,
   not the active pane's context."
  (with-fake-two-pane-session (s)
    (let* ((win (session-active-window s))
           (target (find 2 (window-panes win) :key #'pane-id))
           (*overlay* nil))
      (setf (cl-tmux/model:pane-title target) "target-title")
      (cl-tmux::%run-command-line
       s "if-shell -t %2 -F \"#{pane_title}\" \"display-message target\" \"display-message active\"")
      (assert-overlay-contains "target" (overlay-lines)
                                "if-shell -F must evaluate against the target pane")
      (is (null (search "active" (format nil "~{~A~%~}" (overlay-lines))))
          "if-shell -F must not fall back to the active pane context"))))

(test run-command-line-if-shell-F-empty-condition-no-then
  "if-shell -F with an empty condition and no else runs nothing."
  (with-fake-session (s)
    (let ((*overlay* nil))
      (cl-tmux::%run-command-line s "if-shell -F \"\" \"display-message x\"")
      (assert-overlay-inactive
       "an empty (falsey) condition with no else must not run THEN"))))


(test run-command-line-if-shell-rejects-unsupported-arguments
  "if-shell rejects unknown flags and extra positionals before running anything."
  (dolist (command '("if-shell -Z -F 1 \"display-message x\""
                     "if-shell -F 1 \"display-message x\" \"display-message y\" extra"))
    (with-fake-session (s)
      (let ((*overlay* nil))
        (is (null (cl-tmux::%run-command-line s command))
            "~A must be rejected" command)
        (assert-overlay-contains "if-shell: unsupported argument"
                                  (overlay-lines) command)))))

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
      (assert-overlay-contains "unknown command" (overlay-lines)
                                "unknown command")
      (assert-overlay-contains "totally-unknown-xyz" (overlay-lines)
                                "unknown command"))))

;;; ── :show-messages dispatch ──────────────────────────────────────────────────

(test dispatch-show-messages-empty-log-shows-overlay
  ":show-messages with empty *message-log* opens an overlay saying '(no messages)'."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux::*message-log* nil))
      (cl-tmux::dispatch-command s :show-messages nil)
      (assert-overlay-contains "no messages" (overlay-lines)
                                ":show-messages"))))

(test dispatch-show-messages-populated-log-shows-entries
  ":show-messages with entries in *message-log* lists them."
  (with-fake-session (s)
    (let ((*overlay* nil)
          (cl-tmux::*message-log* (list (cons 0 "hello") (cons 1 "world"))))
      (cl-tmux::dispatch-command s :show-messages nil)
      (is (equal '("hello" "world") (overlay-lines))
          ":show-messages must render one message per line")
      (assert-overlay-contains "hello" (overlay-lines)
                                ":show-messages")
      (assert-overlay-contains "world" (overlay-lines)
                                ":show-messages"))))

(test dispatch-show-messages-defaults-to-current-client-log
  ":show-messages with a current client context uses that client's log."
  (with-fake-session (s)
    (let* ((*overlay* nil)
           (cl-tmux::*message-log* (list (cons 0 "global")))
           (cl-tmux::*current-client-conn*
             (cl-tmux::%make-client-conn
              :state (cl-tmux::make-input-state)
              :message-log (list (cons 1 "client")))))
      (cl-tmux::dispatch-command s :show-messages nil)
      (is (equal '("client") (overlay-lines))
          ":show-messages must prefer the current client's log")
      (assert-overlay-not-contains "global" (overlay-lines)
                                   ":show-messages"))))

(test run-command-line-show-messages-targets-client-log
  "show-messages -t client-1 shows the targeted client's message log."
  (with-fake-session (s)
    (let* ((*overlay* nil)
           (a (cl-tmux::%make-client-conn
               :state (cl-tmux::make-input-state)
               :message-log (list (cons 0 "alpha"))))
           (b (cl-tmux::%make-client-conn
               :state (cl-tmux::make-input-state)
               :message-log (list (cons 0 "beta"))))
           (cl-tmux::*clients* (list a b))
           (cl-tmux::*current-client-conn* a)
           (cl-tmux::*message-log* (list (cons 0 "global"))))
      (cl-tmux::%run-command-line s "show-messages -t client-1")
      (is (equal '("beta") (overlay-lines))
          "show-messages -t client-1 must show the second client's log")
      (assert-overlay-not-contains "alpha" (overlay-lines)
                                   "show-messages -t client-1")
      (assert-overlay-not-contains "global" (overlay-lines)
                                   "show-messages -t client-1"))))

(test run-command-line-show-messages-rejects-stale-flags
  "show-messages rejects stale tmux parity flags."
  (with-fake-session (s)
    (let* ((client (cl-tmux::%make-client-conn
                    :state (cl-tmux::make-input-state)
                    :message-log (list (cons 0 "alpha") (cons 1 "beta"))))
           (cl-tmux::*clients* (list client))
           (cl-tmux::*message-log* (list (cons 0 "alpha") (cons 1 "beta"))))
      (dolist (line '("show-messages -J"
                      "show-messages -T"))
        (let ((*overlay* nil))
          (cl-tmux::%run-command-line s line)
          (assert-overlay-active line)
          (assert-overlay-contains "show-messages: unsupported argument"
                                   (overlay-lines) line)
          (assert-overlay-not-contains "alpha" (overlay-lines) line)
          (assert-overlay-not-contains "beta" (overlay-lines) line))))))
