(in-package #:cl-tmux/test)

;;;; Dispatch tests - select-pane target resolution.

(describe "dispatch-suite"

  ;; 'select-pane -t 2' and 'select-pane -t %2' both activate pane-id 2.
  (it "run-command-line-select-pane-by-id-table"
    (dolist (cmd '("select-pane -t 2" "select-pane -t %2"))
      (with-fake-two-pane-session (s)
        (let ((win (session-active-window s)))
          (expect (= 1 (pane-id (window-active-pane win))))
          (cl-tmux::%run-command-line s cmd)
          (expect (= 2 (pane-id (window-active-pane win))))))))

  ;; 'select-pane -l' returns to the previously active pane.
  (it "run-command-line-select-pane-l-selects-last"
    (with-fake-two-pane-session (s)
      (let* ((win (session-active-window s)))
        ;; Start on pane 1, move to pane 2 (pane 1 becomes last-active).
        (cl-tmux::%run-command-line s "select-pane -t 2")
        (expect (= 2 (pane-id (window-active-pane win))))
        (cl-tmux::%run-command-line s "select-pane -l")
        (expect (= 1 (pane-id (window-active-pane win)))))))

  ;; 'select-pane -t N -T title' sets pane N's title, NOT the active pane's.
  (it "run-command-line-select-pane-t-T-titles-target-pane"
    (with-fake-two-pane-session (s)
      (let* ((win (session-active-window s))
             (p1  (window-active-pane win))
             (p2  (find 2 (window-panes win) :key #'pane-id)))
        (cl-tmux::%run-command-line s "select-pane -t 2 -T mytitle")
        (expect (string= "mytitle" (cl-tmux/model:pane-title p2)))
        (expect (not (string= "mytitle" (cl-tmux/model:pane-title p1)))))))

  ;; 'select-pane -t N -d' disables input on pane N, NOT the active pane.
  (it "run-command-line-select-pane-t-d-disables-target-pane-input"
    (with-fake-two-pane-session (s)
      (let* ((win (session-active-window s))
             (p1  (window-active-pane win))
             (p2  (find 2 (window-panes win) :key #'pane-id)))
        (cl-tmux::%run-command-line s "select-pane -t 2 -d")
        (expect (cl-tmux/model:pane-input-disabled p2) :to-be-truthy)
        (expect (cl-tmux/model:pane-input-disabled p1) :to-be-falsy))))

  ;; 'select-pane -M' on a marked pane clears the server-wide mark (toggle).
  (it "run-command-line-select-pane-M-clears-mark"
    (with-fake-two-pane-session (s)
      (let* ((win (session-active-window s)))
        (let ((ap (window-active-pane win)))
          (cl-tmux::dispatch-command s :mark-pane nil)
          (expect (pane-marked ap))
          (cl-tmux::%run-command-line s "select-pane -M")
          (expect (null (pane-marked ap)))
          (expect (null cl-tmux::*server-marked-pane*))))))

  ;; 'select-pane -m' marks the target pane server-wide (sets *server-marked-pane*)
  ;; so join-pane/swap-pane can use it as the default source; re-marking the same
  ;; pane toggles the mark off, matching tmux's server_set/clear_marked.
  (it "run-command-line-select-pane-m-sets-server-marked-pane"
    (with-fake-two-pane-session (s)
      (let* ((win (session-active-window s))
             (ap (window-active-pane win))
             (cl-tmux::*server-marked-pane* nil))
        (cl-tmux::%run-command-line s "select-pane -m")
        (expect (pane-marked ap))
        (expect (eq ap cl-tmux::*server-marked-pane*))
        (cl-tmux::%run-command-line s "select-pane -m")
        (expect (null (pane-marked ap)))
        (expect (null cl-tmux::*server-marked-pane*)))))

  ;; select-pane rejects unknown flags and positional tokens before mutating panes.
  (it "run-command-line-select-pane-rejects-unsupported-arguments"
    (dolist (command '("select-pane -t 2 extra"
                       "select-pane -x"
                       "select-pane -d extra"
                       "select-pane -t 2 -T title extra"))
      (with-fake-two-pane-session (s)
        (let* ((win (session-active-window s))
               (initial (window-active-pane win))
               (target (find 2 (window-panes win) :key #'pane-id))
               (*overlay* nil))
          (expect (null (cl-tmux::%run-command-line s command)))
          (expect (eq initial (window-active-pane win)))
          (expect (cl-tmux/model:pane-input-disabled target) :to-be-falsy)
          (expect (string= "" (cl-tmux/model:pane-title target)))
          (assert-overlay-contains "unsupported argument"
                                    (overlay-lines) command))))))
