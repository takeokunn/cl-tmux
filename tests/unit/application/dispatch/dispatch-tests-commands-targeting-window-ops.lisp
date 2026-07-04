(in-package #:cl-tmux/test)

;;;; Arg-taking target and window command dispatch tests

(in-suite dispatch-suite)

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

(test run-command-line-unlink-commands-reject-unsupported-arguments
  "unlink-window rejects unknown flags and extra positionals before mutating the window list."
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
