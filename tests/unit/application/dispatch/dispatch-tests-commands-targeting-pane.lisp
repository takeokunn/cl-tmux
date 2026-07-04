(in-package #:cl-tmux/test)

;;;; Dispatch tests - select-pane target resolution.

(in-suite dispatch-suite)

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
