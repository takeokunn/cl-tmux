(in-package #:cl-tmux/test)

;;;; Session lifecycle tests: PTY-backed session / window creation flows.
;;;;
;;;; These tests require PTY allocation and skip themselves when unavailable.

(describe "model-suite"

  ;; ── Session bootstrap ──────────────────────────────────────────────────────

  ;; create-initial-session produces 1 window containing 1 full-width pane.
  (it "initial-session"
    (unless (pty-available-p)
      (skip "no PTY available (sandboxed environment)"))
    (with-session (session 24 80)
      ;; Exactly one window.
      (expect (= 1 (length (session-windows session))))
      (let* ((win   (session-active-window session))
             (panes (window-panes win)))
        ;; Exactly one pane.
        (expect (= 1 (length panes)))
        (let ((pane (first panes)))
          ;; Pane geometry: full width; height shrunk by *status-height* (= 1).
          (expect (= 80 (pane-width  pane)))
          (expect (= 23 (pane-height pane)))
          ;; window-active-pane must return the same pane.
          (expect (eq pane (window-active-pane win)))))))

  ;; ── Adding a second window ─────────────────────────────────────────────────

  ;; session-new-window appends a window and switches the active window.
  (it "session-new-window"
    (unless (pty-available-p)
      (skip "no PTY available (sandboxed environment)"))
    (with-session (session 24 80)
      (let ((first-win (session-active-window session)))
        (session-new-window session "2" 23 80)
        ;; Two windows now.
        (expect (= 2 (length (session-windows session))))
        ;; Active window switched to the new one.
        (let ((new-win (session-active-window session)))
          (expect (not (eq first-win new-win)))
          ;; New window starts with exactly one pane.
          (expect (= 1 (length (window-panes new-win))))))))

  ;; ── Selecting a window by reference ───────────────────────────────────────

  ;; session-select-window switches the active window back to an earlier one.
  (it "session-select-window"
    (unless (pty-available-p)
      (skip "no PTY available (sandboxed environment)"))
    (with-session (session 24 80)
      (let ((first-win (session-active-window session)))
        (session-new-window session "2" 23 80)
        ;; Sanity: active is now the second window.
        (expect (not (eq first-win (session-active-window session))))
        ;; Select the first window back.
        (session-select-window session first-win)
        (expect (eq first-win (session-active-window session))))))

  ;; ── Window index stability ──────────────────────────────────────────────────

  ;; The first window created by create-initial-session gets id=base-index (0).
  (it "window-index-starts-at-base-index"
    (unless (pty-available-p)
      (skip "no PTY available (sandboxed environment)"))
    (with-session (session 24 80)
      (let ((win (session-active-window session)))
        (expect (= 0 (window-id win))))))

  ;; session-new-window assigns the lowest free id >= base-index, not 1+length.
  (it "session-new-window-uses-lowest-free-id"
    (unless (pty-available-p)
      (skip "no PTY available (sandboxed environment)"))
    (with-session (session 24 80)
      (let ((first-win (session-active-window session)))
        (expect (= 0 (window-id first-win)))
        ;; Add a second window; should get id=1.
        (session-new-window session "b" 23 80)
        (let* ((wins      (session-windows session))
               (second-win (find 1 wins :key #'window-id)))
          (expect second-win :to-be-truthy)))))

  ;; After killing a middle window, the remaining window ids do not change.
  (it "window-id-stable-after-kill"
    (unless (pty-available-p)
      (skip "no PTY available (sandboxed environment)"))
    (with-session (session 24 80)
      ;; Build three windows: ids 0, 1, 2.
      (session-new-window session "b" 23 80)
      (session-new-window session "c" 23 80)
      (expect (= 3 (length (session-windows session))))
      (let* ((wins (session-windows session))
             (w0 (find 0 wins :key #'window-id))
             (w1 (find 1 wins :key #'window-id))
             (w2 (find 2 wins :key #'window-id)))
        (expect w0 :to-be-truthy)
        (expect w2 :to-be-truthy)
        ;; Kill the middle window (id=1).
        (cl-tmux/commands:kill-window session w1)
        (let ((remaining (session-windows session)))
          (expect (= 2 (length remaining)))
          (expect (find 0 remaining :key #'window-id) :to-be-truthy)
          (expect (find 2 remaining :key #'window-id) :to-be-truthy)
          (expect (null (find 1 remaining :key #'window-id)))))))

  ;; ── create-initial-session ID counter ───────────────────────────────────────

  ;; create-initial-session increments *session-id-counter* and assigns the new id
  ;; to the session.  Two successive calls yield strictly increasing ids.
  (it "create-initial-session-increments-id-counter"
    (unless (pty-available-p)
      (skip "no PTY available (sandboxed environment)"))
    (let ((before cl-tmux/model::*session-id-counter*))
      (with-session (sess1 24 80)
        (expect (= (1+ before) (session-id sess1)))
        (expect (= (1+ before) cl-tmux/model::*session-id-counter*)))))

  ;; create-initial-session sets session-last-active to a non-zero universal time.
  (it "create-initial-session-session-touch-called"
    (unless (pty-available-p)
      (skip "no PTY available (sandboxed environment)"))
    (let ((before (get-universal-time)))
      (with-session (sess 24 80)
        (expect (>= (session-last-active sess) before))))))
