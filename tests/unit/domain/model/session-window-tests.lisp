(in-package #:cl-tmux/test)

;;;; Session tests — start-directory slot, all-panes ordering, and window flag clearing.

(in-suite model-suite)

;;; ── session-start-directory slot ─────────────────────────────────────────────

(test session-start-directory-defaults-nil
  "session-start-directory defaults to NIL for a freshly created session."
  (let ((sess (make-session :id 1 :name "s")))
    (is (null (cl-tmux/model::session-start-directory sess))
        "session-start-directory must default to NIL")))

(test session-start-directory-settable
  "session-start-directory can be set to a path string and read back."
  (let ((sess (make-session :id 1 :name "s")))
    (setf (cl-tmux/model::session-start-directory sess) "/home/user")
    (is (string= "/home/user" (cl-tmux/model::session-start-directory sess))
        "session-start-directory must return the value written via setf")))

;;; ── session-select-window clears activity/silence flags ─────────────────────

(test session-select-window-clears-activity-flag
  "session-select-window clears the window-activity-flag when selecting a window."
  (let* ((w0   (make-window :id 0 :name "a" :activity-flag t))
         (sess (make-session :id 1 :name "s" :windows (list w0))))
    (session-select-window sess w0)
    (is-false (cl-tmux/model::window-activity-flag w0)
              "window-activity-flag must be cleared when the window is selected")))

(test session-select-window-clears-silence-flag
  "session-select-window clears the window-silence-flag when selecting a window."
  (let* ((w0   (make-window :id 0 :name "a" :silence-flag t))
         (sess (make-session :id 1 :name "s" :windows (list w0))))
    (session-select-window sess w0)
    (is-false (cl-tmux/model::window-silence-flag w0)
              "window-silence-flag must be cleared when the window is selected")))

;;; ── all-panes ordering ───────────────────────────────────────────────────────

(test all-panes-preserves-window-order
  "all-panes returns panes in window-list order (first window's panes first)."
  (let* ((p0   (make-no-pty-pane 1 0 0 20 5))
         (p1   (make-no-pty-pane 2 0 0 20 5))
         (w0   (make-window :id 0 :name "w0" :panes (list p0)))
         (w1   (make-window :id 1 :name "w1" :panes (list p1)))
         (sess (make-session :id 1 :name "s" :windows (list w0 w1))))
    (let ((panes (all-panes sess)))
      (is (eq p0 (first panes))
          "first pane must come from the first window")
      (is (eq p1 (second panes))
          "second pane must come from the second window"))))

;;; ── session-windows returns the window list ──────────────────────────────────

(test session-windows-returns-complete-list
  "session-windows returns all windows inserted via session-insert-window."
  (let* ((w0   (make-window :id 0 :name "a"))
         (w1   (make-window :id 1 :name "b"))
         (w2   (make-window :id 2 :name "c"))
         (sess (make-session :id 1 :name "s" :windows nil)))
    (session-insert-window sess w0)
    (session-insert-window sess w2)
    (session-insert-window sess w1)
    (is (= 3 (length (session-windows sess)))
        "session-windows must list all inserted windows")
    (is-true (member w0 (session-windows sess)) "w0 must be in the list")
    (is-true (member w1 (session-windows sess)) "w1 must be in the list")
    (is-true (member w2 (session-windows sess)) "w2 must be in the list")))
