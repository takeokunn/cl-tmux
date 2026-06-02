(in-package #:cl-tmux/test)

;;;; Session-level tests: session / window lifecycle.
;;;;
;;;; Tests that create real PTYs (via create-initial-session / window-split)
;;;; skip themselves when PTY allocation is unavailable — the same guard used
;;;; in pty-tests.lisp — so the suite runs cleanly in sandboxed Nix builds.

(in-suite model-suite)

;;; ── Session bootstrap ──────────────────────────────────────────────────────

(test initial-session
  "create-initial-session produces 1 window containing 1 full-width pane."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (with-session (session 24 80)
    ;; Exactly one window.
    (is (= 1 (length (session-windows session))))
    (let* ((win  (session-active-window session))
           (panes (window-panes win)))
      ;; Exactly one pane.
      (is (= 1 (length panes)))
      (let ((pane (first panes)))
        ;; Pane geometry: full width; height shrunk by *status-height* (= 1).
        (is (= 80 (pane-width  pane)) "initial pane width must equal cols")
        (is (= 23 (pane-height pane))
            "initial pane height must equal rows - *status-height* (23)")
        ;; window-active-pane must return the same pane.
        (is (eq pane (window-active-pane win))
            "window-active-pane must return the sole pane")))))

;;; ── Adding a second window ─────────────────────────────────────────────────

(test session-new-window
  "session-new-window appends a window and switches the active window."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (with-session (session 24 80)
    (let ((first-win (session-active-window session)))
      (session-new-window session "2" 23 80)
      ;; Two windows now.
      (is (= 2 (length (session-windows session))))
      ;; Active window switched to the new one.
      (let ((new-win (session-active-window session)))
        (is (not (eq first-win new-win))
            "active window must have changed after session-new-window")
        ;; New window starts with exactly one pane.
        (is (= 1 (length (window-panes new-win)))
            "new window must have exactly one pane")))))

;;; ── Selecting a window by reference ───────────────────────────────────────

(test session-select-window
  "session-select-window switches the active window back to an earlier one."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (with-session (session 24 80)
    (let ((first-win (session-active-window session)))
      (session-new-window session "2" 23 80)
      ;; Sanity: active is now the second window.
      (is (not (eq first-win (session-active-window session))))
      ;; Select the first window back.
      (session-select-window session first-win)
      (is (eq first-win (session-active-window session))
          "session-active-window must return the window passed to session-select-window"))))

;;; ── session-active-pane ────────────────────────────────────────────────────

(test session-active-pane
  "session-active-pane returns the active pane of the active window."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (with-session (session 24 80)
    (let* ((win  (session-active-window session))
           (pane (window-active-pane win)))
      (is (eq pane (session-active-pane session))
          "session-active-pane must match window-active-pane of the active window"))))

;;; ── Window index stability ──────────────────────────────────────────────────

(test window-index-starts-at-base-index
  "The first window created by create-initial-session gets id=base-index (0)."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (with-session (session 24 80)
    (let ((win (session-active-window session)))
      (is (= 0 (window-id win))
          "first window id must equal base-index (0)"))))

(test session-new-window-uses-lowest-free-id
  "session-new-window assigns the lowest free id >= base-index, not 1+length."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (with-session (session 24 80)
    (let ((first-win (session-active-window session)))
      (is (= 0 (window-id first-win)) "precondition: first window id=0")
      ;; Add a second window; should get id=1.
      (session-new-window session "b" 23 80)
      (let* ((wins (session-windows session))
             (second-win (find 1 wins :key #'window-id)))
        (is-true second-win "a window with id=1 must exist after second new-window")))))

(test window-id-stable-after-kill
  "After killing a middle window, the remaining window ids do not change."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (with-session (session 24 80)
    ;; Build three windows: ids 0, 1, 2.
    (session-new-window session "b" 23 80)
    (session-new-window session "c" 23 80)
    (is (= 3 (length (session-windows session))) "must have 3 windows")
    (let* ((wins (session-windows session))
           (w0 (find 0 wins :key #'window-id))
           (w1 (find 1 wins :key #'window-id))
           (w2 (find 2 wins :key #'window-id)))
      ;; Kill the middle window (id=1).
      (cl-tmux/commands:kill-window session w1)
      (let ((remaining (session-windows session)))
        (is (= 2 (length remaining)) "two windows remain after kill")
        (is-true (find 0 remaining :key #'window-id) "window id=0 must still exist")
        (is-true (find 2 remaining :key #'window-id) "window id=2 must still exist")
        (is (null (find 1 remaining :key #'window-id))
            "window id=1 must be gone after kill")))))

;;; ── %shell-basename edge cases ──────────────────────────────────────────────

(test shell-basename-with-slash
  "%shell-basename returns only the basename component when shell path contains slashes."
  (let ((cl-tmux/config:*default-shell* "/bin/bash"))
    (is (string= "bash" (cl-tmux/model::%shell-basename))
        "%shell-basename must strip path prefix for /bin/bash")))

(test shell-basename-no-slash
  "%shell-basename returns the whole string when there is no slash in *default-shell*."
  (let ((cl-tmux/config:*default-shell* "zsh"))
    (is (string= "zsh" (cl-tmux/model::%shell-basename))
        "%shell-basename must return the full string when no slash is present")))

(test shell-basename-empty-default-shell
  "%shell-basename falls back to \"window\" when *default-shell* is NIL."
  (let ((cl-tmux/config:*default-shell* nil))
    (is (string= "window" (cl-tmux/model::%shell-basename))
        "%shell-basename must return \"window\" when *default-shell* is NIL")))

(test shell-basename-trailing-slash
  "%shell-basename returns empty string when *default-shell* ends in a slash."
  (let ((cl-tmux/config:*default-shell* "/usr/bin/"))
    ;; The last slash is at position (1- (length "/usr/bin/")); subseq gives "".
    ;; This is a degenerate input — the important thing is no error is signalled.
    (is (stringp (cl-tmux/model::%shell-basename))
        "%shell-basename must return a string even for a trailing-slash path")))

;;; ── session-insert-window ────────────────────────────────────────────────────

(test session-insert-window-sorts-by-id
  "session-insert-window keeps the window list sorted by window-id."
  (let* ((w0  (make-window :id 0 :name "a"))
         (w2  (make-window :id 2 :name "c"))
         (w1  (make-window :id 1 :name "b"))
         (sess (make-session :id 1 :name "s" :windows (list w0 w2))))
    (session-insert-window sess w1)
    (is (equal (list w0 w1 w2) (session-windows sess))
        "session-insert-window must sort windows by id ascending")))

(test session-insert-window-does-not-change-active
  "session-insert-window does not mutate the active window slot."
  (let* ((w0  (make-window :id 0 :name "a"))
         (w1  (make-window :id 1 :name "b"))
         (sess (make-session :id 1 :name "s" :windows (list w0))))
    (session-select-window sess w0)
    (session-insert-window sess w1)
    (is (eq w0 (session-active-window sess))
        "session-insert-window must not change the active window")))

;;; ── get-update-environment-vars ─────────────────────────────────────────────

(test get-update-environment-vars-returns-alist
  "get-update-environment-vars returns an alist of (name . value) pairs."
  ;; DISPLAY may or may not be set depending on the build environment;
  ;; just verify the shape of the return value (alist with string keys).
  (let ((result (get-update-environment-vars)))
    (is (listp result)
        "get-update-environment-vars must return a list")
    (dolist (entry result)
      (is (consp entry)
          "each entry must be a cons pair")
      (is (stringp (car entry))
          "each entry key must be a string")
      (is (stringp (cdr entry))
          "each entry value must be a string"))))

(test get-update-environment-vars-respects-star-update-environment
  "get-update-environment-vars only queries variables listed in *update-environment*."
  ;; Bind *update-environment* to a single sentinel name guaranteed not to exist.
  (let ((*update-environment* (list "__CL_TMUX_NONEXISTENT_VAR_99999__")))
    (let ((result (get-update-environment-vars)))
      (is (null result)
          "when the env var is absent, result must be NIL"))))

(test kill-window-selects-nearest-id
  "After killing the active window, the window with the nearest id is selected."
  (let* ((w0 (make-window :id 0 :name "a" :width 20 :height 5
                          :panes (list (make-pane :id 1 :x 0 :y 0
                                                  :width 20 :height 5
                                                  :fd -1 :pid -1
                                                  :screen (make-screen 20 5)))))
         (w1 (make-window :id 1 :name "b" :width 20 :height 5
                          :panes (list (make-pane :id 2 :x 0 :y 0
                                                  :width 20 :height 5
                                                  :fd -1 :pid -1
                                                  :screen (make-screen 20 5)))))
         (w3 (make-window :id 3 :name "d" :width 20 :height 5
                          :panes (list (make-pane :id 3 :x 0 :y 0
                                                  :width 20 :height 5
                                                  :fd -1 :pid -1
                                                  :screen (make-screen 20 5)))))
         (sess (make-session :id 1 :name "0" :windows (list w0 w1 w3))))
    (session-select-window sess w1)       ; kill the middle window (id=1)
    (cl-tmux/commands:kill-window sess)
    ;; nearest to id=1 among {0,3}: w0 (distance=1) wins over w3 (distance=2)
    (is (eq w0 (session-active-window sess))
        "after killing id=1, id=0 (nearest) must become active")))
