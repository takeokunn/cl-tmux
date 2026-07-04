(in-package #:cl-tmux/test)

;;;; pane lifecycle command tests: kill-pane, join-pane insertion, swap-two-panes

(in-suite commands-suite)

(defparameter *swap-two-panes-noop-cases*
  '((:same-pane
     "same pane -> no-op, returns NIL"
     "panes list must be unchanged after same-pane no-op")
    (:nil-first
     "NIL first argument -> no-op, returns NIL"
     "panes list must be unchanged after NIL first argument")
    (:nil-second
     "NIL second argument -> no-op, returns NIL"
     "panes list must be unchanged after NIL second argument")
    (:missing-pane
     "pane not in window -> no-op, returns NIL"
     "panes list must be unchanged after missing-pane no-op")))

(defun %swap-two-panes-noop-args (case p0 outsider)
  (ecase case
    (:same-pane (values p0 p0))
    (:nil-first (values nil p0))
    (:nil-second (values p0 nil))
    (:missing-pane (values p0 outsider))))

(defun %swap-two-panes-noop-checks (win p0 pane-a pane-b return-message unchanged-message)
  (list (list (null (swap-two-panes win pane-a pane-b)) t return-message)
        (list (eq p0 (first (window-panes win))) t unchanged-message)))

(defun %pane-geometry (pane)
  (list (pane-x pane)
        (pane-y pane)
        (pane-width pane)
        (pane-height pane)))

(defun %swap-two-panes-geometry-checks (p0 p1 p0-before p1-before)
  (destructuring-bind (x0 y0 w0 h0) p0-before
    (destructuring-bind (x1 y1 w1 h1) p1-before
      (list (list (pane-x p0) x1 "p0 x must be p1's old x after swap")
            (list (pane-y p0) y1 "p0 y must be p1's old y after swap")
            (list (pane-width p0) w1 "p0 width must be p1's old width after swap")
            (list (pane-height p0) h1 "p0 height must be p1's old height after swap")
            (list (pane-x p1) x0 "p1 x must be p0's old x after swap")
            (list (pane-y p1) y0 "p1 y must be p0's old y after swap")
            (list (pane-width p1) w0 "p1 width must be p0's old width after swap")
            (list (pane-height p1) h0 "p1 height must be p0's old height after swap")))))

(defun %join-pane-insert-checks (win src)
  (list (list (not (null (member src (window-panes win)))) t
              "inserted pane must appear in window-panes after insertion")
        (list (eq win (pane-window src)) t
              "pane-window must be updated to dst-window after insertion")))

(defmacro with-swap-two-panes-noop-case ((case return-message unchanged-message) &body body)
  `(dolist (row *swap-two-panes-noop-cases*)
     (destructuring-bind (,case ,return-message ,unchanged-message) row
       ,@body)))

(defmacro with-joined-pane-in-window ((win src) &body body)
  `(let* ((,win (%vsplit-window 20))
          (dst-pane (first (window-panes ,win)))
          (,src (make-pane :id 99 :x 0 :y 0 :width 10 :height 5
                           :fd -1 :pid -1 :screen (make-screen 10 5))))
     (window-select-pane ,win dst-pane)
     (cl-tmux/commands::%join-pane-insert-into-dst ,src ,win :h)
     ,@body))

;;; ── kill-pane ─────────────────────────────────────────────────────────────────

(test kill-pane-multi-pane-survivor-relayouts-and-reselects
  "Killing one pane of a multi-pane window keeps the window alive: the survivor
   is removed-from? no — the killed pane is removed, the survivor is reselected
   and the window is relaid out.  Returns NIL (session not quit)."
  (let* ((win (%vsplit-window 20))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win)))
         (sess (%make-session-with-window win)))
    (window-select-pane win p0)
    (is (null (kill-pane sess)) "surviving pane => not a quit")
    (is (equal (list p1) (window-panes win)) "killed pane removed; survivor kept")
    (is (eq p1 (window-active-pane win)) "survivor becomes the active pane")
    (is (= 1 (length (session-windows sess))) "window still present")))

(test kill-pane-emptied-window-falls-through-to-kill-window
  "Killing the sole pane of the sole window empties the window, which then
   falls through to kill-window; with no windows left the session quits."
  (let* ((p0  (%make-test-pane))
         (win (make-window :id 1 :name "w" :width 20 :height 5
                           :tree (make-layout-leaf p0)
                           :panes (list p0)))
         (sess (make-session :id 1 :name "0" :windows (list win))))
    (window-select-pane win p0)
    (session-select-window sess win)
    (is (eq :quit (kill-pane sess)) "emptying the last window quits the session")
    (is (null (session-windows sess)) "no windows remain")))

(test kill-pane-emptied-window-of-two-survives
  "Emptying one window of two falls through to kill-window, which keeps the
   session alive on the remaining window."
  (let* ((p0  (%make-test-pane))
         (w1  (make-window :id 1 :name "a" :width 20 :height 5
                           :tree (make-layout-leaf p0)
                           :panes (list p0)))
         (w2  (make-window :id 2 :name "b" :width 20 :height 5
                           :panes (list (%make-test-pane))))
         (sess (make-session :id 1 :name "0" :windows (list w1 w2))))
    (window-select-pane w1 p0)
    (session-select-window sess w1)
    (is (null (kill-pane sess)) "session survives on the other window")
    (is (equal (list w2) (session-windows sess)) "emptied window removed")
    (is (eq w2 (session-active-window sess)) "active window switches to survivor")))

(test kill-pane-nonactive-does-not-reselect
  "Killing a non-active pane does not change the active pane."
  (let* ((win (%vsplit-window 20))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win)))
         (sess (%make-session-with-window win)))
    (window-select-pane win p0)
    (kill-pane sess p1)
    (is (eq p0 (window-active-pane win))
        "active pane must remain p0 when a non-active pane is killed")))

(test kill-pane-active-selects-mru-pane
  "When the active pane is killed, the last-active pane is selected if present."
  (let* ((win (%vsplit-window 20))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win)))
         (sess (%make-session-with-window win)))
    (window-select-pane win p1)
    (window-select-pane win p0)
    (is (eq p1 (window-last-active win)) "precondition: last-active is p1")
    (kill-pane sess p0)
    (is (eq p1 (window-active-pane win))
        "last-active pane (p1) must be selected after active pane (p0) is killed")))

;;; ── %join-pane-insert-into-dst direct unit tests ─────────────────────────────

(test join-pane-insert-into-dst-state-table
  "%join-pane-insert-into-dst appends src-pane and wires pane-window."
  (with-joined-pane-in-window (win src)
    (check-table (%join-pane-insert-checks win src) :test #'eq)))

;;; ── swap-two-panes direct unit tests ─────────────────────────────────────────
;;;
;;; swap-two-panes has zero dedicated tests; its geometry-swap path is only
;;; exercised indirectly through swap-pane direction dispatch.  The tests below
;;; cover the no-op paths (same pane, missing pane) and the index-exchange path.

(test swap-two-panes-noop-table
  "swap-two-panes returns NIL and preserves pane order for invalid inputs."
  (with-swap-two-panes-noop-case (case return-message unchanged-message)
    (let* ((win (%vsplit-window 20))
           (p0  (first (window-panes win)))
           (outsider (make-pane :id 99 :x 0 :y 0 :width 5 :height 5
                                :fd -1 :pid -1 :screen (make-screen 5 5))))
      (multiple-value-bind (pane-a pane-b)
          (%swap-two-panes-noop-args case p0 outsider)
        (check-table
         (%swap-two-panes-noop-checks win p0 pane-a pane-b return-message unchanged-message)
         :test #'eq)))))

(test swap-two-panes-exchanges-list-order-and-geometry
  "swap-two-panes exchanges both the pane list positions and screen geometry."
  (let* ((win (%vsplit-window 20))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win)))
         (p0-before (%pane-geometry p0))
         (p1-before (%pane-geometry p1)))
    (is (eq p0 (swap-two-panes win p0 p1))
        "swap-two-panes returns the first argument (pane-a) on success")
    (is (eq p1 (first  (window-panes win))) "p1 must now be first in panes list")
    (is (eq p0 (second (window-panes win))) "p0 must now be second in panes list")
    (check-table (%swap-two-panes-geometry-checks p0 p1 p0-before p1-before)
                 :test #'=)))
