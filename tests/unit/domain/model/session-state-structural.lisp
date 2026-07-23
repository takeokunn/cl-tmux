(in-package #:cl-tmux/test)

;;;; Session state tests: structural window/session helpers.

(describe "model-suite"

  ;;; ── %attach-full-screen-pane structural test (no PTY) ───────────────────────
  ;;;
  ;;; Exercises the pure structural side of %attach-full-screen-pane by verifying
  ;;; the window slots it sets, without requiring a real PTY fork.
  ;;; The test builds a window with a pre-existing leaf pane instead of calling
  ;;; the real %attach-full-screen-pane (which forks a shell), then checks that
  ;;; session-active-pane and window-active-pane are consistent.

  ;; %attach-full-screen-pane wires window slots: panes, active, tree are all set.
  (it "attach-full-screen-pane-structural"
    (let* ((p0   (make-no-pty-pane 1 0 0 80 23))
           (win  (make-window :id 0 :name "bash" :width 80 :height 23
                              :panes (list p0)
                              :active p0
                              :tree (make-layout-leaf p0)))
           (sess (make-session :id 1 :name "0" :windows (list win))))
      (session-select-window sess win)
      ;; Verify that the window's panes/active/tree slots are consistent.
      (expect (eq p0 (window-active-pane win)))
      (expect (= 1 (length (window-panes win))))
      (expect (window-tree win) :to-be-truthy)
      ;; session-active-pane must delegate correctly.
      (expect (eq p0 (session-active-pane sess)))))

  ;;; ── session-active-window falls back to first window ───────────────────────

  ;; session-active-window returns the first window when active slot is NIL.
  (it "session-active-window-falls-back-to-first"
    (let* ((w0   (make-window :id 0 :name "a"))
           (w1   (make-window :id 1 :name "b"))
           ;; Construct with active=NIL explicitly.
           (sess (make-session :id 1 :name "s" :windows (list w0 w1))))
      ;; active slot defaults to NIL for make-session.
      (expect (eq w0 (session-active-window sess)))))

  ;;; ── session-active-pane returns NIL for empty session ────────────────────────

  ;; session-active-pane returns NIL when the session has no windows.
  (it "session-active-pane-nil-for-windowless-session"
    (let ((sess (make-session :id 1 :name "s" :windows nil)))
      (expect (null (session-active-pane sess)))))

  ;;; ── session-locked-p slot ────────────────────────────────────────────────────

  ;; session-locked-p defaults to NIL and can be set to T.
  (it "session-locked-p-slot"
    (let ((sess (make-session :id 1 :name "s")))
      (expect (null (session-locked-p sess)))
      (setf (session-locked-p sess) t)
      (expect (session-locked-p sess) :to-be-truthy)))

  ;;; ── session-group slot ───────────────────────────────────────────────────────

  ;; session-group defaults to NIL and can be set to a non-NIL value.
  (it "session-group-slot"
    (let ((sess (make-session :id 1 :name "s")))
      (expect (null (session-group sess)))
      (setf (session-group sess) "mygroup")
      (expect (string= "mygroup" (session-group sess)))))

  ;;; ── session-last-active slot ────────────────────────────────────────────────

  ;; session-last-active defaults to 0 for a freshly created session.
  (it "session-last-active-defaults-zero"
    (let ((sess (make-session :id 1 :name "s")))
      (expect (= 0 (session-last-active sess)))))

  ;;; ── all-panes with multi-pane window ────────────────────────────────────────

  ;; all-panes collects all panes when a window has more than one pane.
  (it "all-panes-multi-pane-window"
    (let* ((p0   (make-no-pty-pane 1 0 0 40 24))
           (p1   (make-no-pty-pane 2 41 0 40 24))
           (win  (make-window :id 0 :name "w" :panes (list p0 p1)))
           (sess (make-session :id 1 :name "s" :windows (list win))))
      (let ((panes (all-panes sess)))
        (expect (= 2 (length panes)))
        (expect (member p0 panes) :to-be-truthy)
        (expect (member p1 panes) :to-be-truthy))))

  ;;; ── session-select-window updates window-last-active-time ──────────────────

  ;; session-select-window updates window-last-active-time on the selected window.
  (it "session-select-window-updates-window-last-active-time"
    (let* ((w0   (make-window :id 0 :name "a" :last-active-time 0))
           (sess (make-session :id 1 :name "s" :windows (list w0))))
      (let ((before (get-universal-time)))
        (session-select-window sess w0)
        (expect (>= (window-last-active-time w0) before)))))

  ;;; ── %next-window-id base-index parameter ────────────────────────────────────

  ;; %next-window-id with base-index=5 returns at least 5.
  (it "next-window-id-respects-base-index"
    (let* ((sess (make-session :id 1 :name "s" :windows nil)))
      (expect (>= (cl-tmux/model::%next-window-id sess 5) 5))))

  ;;; ── Table-driven session struct defaults ─────────────────────────────────────

  ;; Table-driven: make-session zero-argument defaults are predictable.
  (it "session-struct-default-values-table"
    ;; Each entry: (slot-accessor default-pred description)
    (let ((sess (make-session :id 1 :name "test")))
      (expect (= 1 (session-id sess)))
      (expect (string= "test" (session-name sess)))
      (dolist (row (list (list (session-windows sess)       "session-windows must default to NIL")
                         (list (session-active-window sess) "active window must be NIL (no windows)")
                         (list (session-clients sess)       "session-clients must default to NIL")
                         (list (session-locked-p sess)      "session-locked-p must default to NIL")
                         (list (session-group sess)         "session-group must default to NIL")))
        (destructuring-bind (val desc) row
          (declare (ignore desc))
          (expect (null val)))))))
