(in-package #:cl-tmux/test)

;;;; Pane tests - pane/window operations.

(in-suite model-suite)

;;; ── swap-pane exchanges rects ────────────────────────────────────────────────

(test swap-pane-exchanges-rects
  "swap-pane exchanges the x/y/width/height between two panes."
  (let* ((p0  (make-no-pty-pane 1  0 0 20 5))
         (p1  (make-no-pty-pane 2 21 0 20 5))
         (win (make-window :id 1 :name "w" :width 41 :height 5
                           :tree (make-layout-split :h
                                    (make-layout-leaf p0) (make-layout-leaf p1)
                                    1/2)
                           :panes (list p0 p1))))
    (window-select-pane win p0)
    (let ((x0-before (pane-x p0))
          (x1-before (pane-x p1)))
      (swap-pane win :right)
      (is (= x1-before (pane-x p0)) "p0 must have p1's former x after swap")
      (is (= x0-before (pane-x p1)) "p1 must have p0's former x after swap"))))

;;; ── capture-pane returns text ────────────────────────────────────────────────

(test capture-pane-returns-text
  "capture-pane returns a string containing the content fed to the pane's screen."
  (let* ((screen (make-screen 20 5))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                            :fd -1 :pid -1 :screen screen)))
    (feed screen "HELLO")
    (let ((result (capture-pane pane)))
      (is (stringp result) "capture-pane must return a string")
      (is (not (null (search "HELLO" result)))
          "capture-pane output must contain the fed text"))))

;;; ── last-pane cycles ─────────────────────────────────────────────────────────

(test last-pane-cycles
  "window-select-pane updates window-last-active; switching back via :last-pane
   returns to the previous pane."
  (let* ((p0  (make-no-pty-pane 1  0 0 20 5))
         (p1  (make-no-pty-pane 2 21 0 20 5))
         (win (make-window :id 1 :name "w" :width 41 :height 5
                           :tree (make-layout-split :h
                                    (make-layout-leaf p0) (make-layout-leaf p1)
                                    1/2)
                           :panes (list p0 p1))))
    ;; Start on p0
    (window-select-pane win p0)
    (is (eq p0 (window-active-pane win)) "precondition: p0 is active")
    ;; Switch to p1 — this should record p0 as last-active
    (window-select-pane win p1)
    (is (eq p1 (window-active-pane win)) "p1 must be active after select")
    (is (eq p0 (window-last-active win)) "p0 must be the last-active pane")
    ;; Simulate :last-pane by selecting window-last-active
    (let ((last (window-last-active win)))
      (when last (window-select-pane win last)))
    (is (eq p0 (window-active-pane win)) "last-pane must return to p0")))

;;; ── display-panes overlay active ─────────────────────────────────────────────

(test display-panes-overlay-active
  ":display-panes activates the overlay (overlay-active-p returns T)."
  (with-fake-session (sess :nwindows 1 :npanes 2)
    (let ((*overlay* nil))
      (cl-tmux::dispatch-command sess :display-panes nil)
      (assert-overlay-active ":display-panes must activate the overlay"))))

;;; ── respawn-pane resets fd/pid ───────────────────────────────────────────────

(test respawn-pane-updates-fd-and-pid
  "respawn-pane closes the old PTY and assigns a fresh fd/pid to the pane.
   Uses pty-available-p to skip when PTY spawning is not available."
  (unless (pty-available-p)
    (skip "PTY not available"))
  (with-session (session 20 20)
    (let* ((pane (session-active-pane session))
           (old-pid (pane-pid pane)))
      (respawn-pane session pane)
      ;; The new pid must differ (a new child process was spawned).
      ;; The fd may or may not be the same number (OS fd recycling), but
      ;; it must be non-negative (a valid open fd).
      (is (not (= old-pid (pane-pid pane)))
          "pid must change after respawn (a new process was spawned)")
      (is (>= (pane-fd pane) 0)
          "pane-fd must be a non-negative open fd after respawn"))))
