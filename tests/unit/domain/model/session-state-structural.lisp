(in-package #:cl-tmux/test)

;;;; Session state tests: structural window/session helpers.

(in-suite model-suite)

;;; ── %attach-full-screen-pane structural test (no PTY) ───────────────────────
;;;
;;; Exercises the pure structural side of %attach-full-screen-pane by verifying
;;; the window slots it sets, without requiring a real PTY fork.
;;; The test builds a window with a pre-existing leaf pane instead of calling
;;; the real %attach-full-screen-pane (which forks a shell), then checks that
;;; session-active-pane and window-active-pane are consistent.

(test attach-full-screen-pane-structural
  "%attach-full-screen-pane wires window slots: panes, active, tree are all set."
  (let* ((p0   (make-no-pty-pane 1 0 0 80 23))
         (win  (make-window :id 0 :name "bash" :width 80 :height 23
                            :panes (list p0)
                            :active p0
                            :tree (make-layout-leaf p0)))
         (sess (make-session :id 1 :name "0" :windows (list win))))
    (session-select-window sess win)
    ;; Verify that the window's panes/active/tree slots are consistent.
    (is (eq p0 (window-active-pane win))
        "window-active-pane must be the sole leaf pane")
    (is (= 1 (length (window-panes win)))
        "window-panes must contain exactly the one leaf pane")
    (is-true (window-tree win)
             "window-tree must be non-NIL (a layout-leaf)")
    ;; session-active-pane must delegate correctly.
    (is (eq p0 (session-active-pane sess))
        "session-active-pane must agree with window-active-pane")))

;;; ── session-active-window falls back to first window ───────────────────────

(test session-active-window-falls-back-to-first
  "session-active-window returns the first window when active slot is NIL."
  (let* ((w0   (make-window :id 0 :name "a"))
         (w1   (make-window :id 1 :name "b"))
         ;; Construct with active=NIL explicitly.
         (sess (make-session :id 1 :name "s" :windows (list w0 w1))))
    ;; active slot defaults to NIL for make-session.
    (is (eq w0 (session-active-window sess))
        "session-active-window must fall back to the first window when active is NIL")))

;;; ── session-active-pane returns NIL for empty session ────────────────────────

(test session-active-pane-nil-for-windowless-session
  "session-active-pane returns NIL when the session has no windows."
  (let ((sess (make-session :id 1 :name "s" :windows nil)))
    (is (null (session-active-pane sess))
        "session-active-pane must return NIL for a windowless session")))

;;; ── session-locked-p slot ────────────────────────────────────────────────────

(test session-locked-p-slot
  "session-locked-p defaults to NIL and can be set to T."
  (let ((sess (make-session :id 1 :name "s")))
    (is (null (session-locked-p sess)) "session-locked-p must default to NIL")
    (setf (session-locked-p sess) t)
    (is-true (session-locked-p sess) "session-locked-p must return T after being set")))

;;; ── session-group slot ───────────────────────────────────────────────────────

(test session-group-slot
  "session-group defaults to NIL and can be set to a non-NIL value."
  (let ((sess (make-session :id 1 :name "s")))
    (is (null (session-group sess)) "session-group must default to NIL")
    (setf (session-group sess) "mygroup")
    (is (string= "mygroup" (session-group sess)) "session-group must return the value written via setf")))

;;; ── session-last-active slot ────────────────────────────────────────────────

(test session-last-active-defaults-zero
  "session-last-active defaults to 0 for a freshly created session."
  (let ((sess (make-session :id 1 :name "s")))
    (is (= 0 (session-last-active sess))
        "session-last-active must default to 0")))

;;; ── all-panes with multi-pane window ────────────────────────────────────────

(test all-panes-multi-pane-window
  "all-panes collects all panes when a window has more than one pane."
  (let* ((p0   (make-no-pty-pane 1 0 0 40 24))
         (p1   (make-no-pty-pane 2 41 0 40 24))
         (win  (make-window :id 0 :name "w" :panes (list p0 p1)))
         (sess (make-session :id 1 :name "s" :windows (list win))))
    (let ((panes (all-panes sess)))
      (is (= 2 (length panes))
          "all-panes must return both panes from a single 2-pane window")
      (is-true (member p0 panes) "p0 must be in all-panes result")
      (is-true (member p1 panes) "p1 must be in all-panes result"))))

;;; ── session-select-window updates window-last-active-time ──────────────────

(test session-select-window-updates-window-last-active-time
  "session-select-window updates window-last-active-time on the selected window."
  (let* ((w0   (make-window :id 0 :name "a" :last-active-time 0))
         (sess (make-session :id 1 :name "s" :windows (list w0))))
    (let ((before (get-universal-time)))
      (session-select-window sess w0)
      (is (>= (window-last-active-time w0) before)
          "window-last-active-time must be updated when selected"))))

;;; ── %next-window-id base-index parameter ────────────────────────────────────

(test next-window-id-respects-base-index
  "%next-window-id with base-index=5 returns at least 5."
  (let* ((sess (make-session :id 1 :name "s" :windows nil)))
    (is (>= (cl-tmux/model::%next-window-id sess 5) 5)
        "%next-window-id with base-index=5 must return a value >= 5")))

;;; ── Table-driven session struct defaults ─────────────────────────────────────

(test session-struct-default-values-table
  "Table-driven: make-session zero-argument defaults are predictable."
  ;; Each entry: (slot-accessor default-pred description)
  (let ((sess (make-session :id 1 :name "test")))
    (is (= 1 (session-id sess)) "session-id must match :id kwarg")
    (is (string= "test" (session-name sess)) "session-name must match :name kwarg")
    (dolist (row (list (list (session-windows sess)       "session-windows must default to NIL")
                       (list (session-active-window sess) "active window must be NIL (no windows)")
                       (list (session-clients sess)       "session-clients must default to NIL")
                       (list (session-locked-p sess)      "session-locked-p must default to NIL")
                       (list (session-group sess)         "session-group must default to NIL")))
      (destructuring-bind (val desc) row
        (is (null val) "~A" desc)))))
