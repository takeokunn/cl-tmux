(in-package #:cl-tmux/test)

;;;; Pane tests - predicates and hit-testing.

(in-suite model-suite)

;;; ── pane-at-position hit test ────────────────────────────────────────────────

(test pane-at-position-table
  "pane-at-position returns the pane containing (x,y), or NIL for the separator gap."
  (let* ((p0  (make-no-pty-pane 1  0 0 40 24))
         (p1  (make-no-pty-pane 2 41 0 40 24))
         (win (make-window :id 1 :name "w" :width 81 :height 24
                           :panes (list p0 p1)
                           :tree (make-layout-split :h
                                    (make-layout-leaf p0)
                                    (make-layout-leaf p1)
                                    1/2))))
    (is (eq  p0  (pane-at-position win 10 5)) "col 10 hits p0 [0,40)")
    (is (eq  p1  (pane-at-position win 50 5)) "col 50 hits p1 [41,81)")
    (is (null    (pane-at-position win 40 5)) "col 40 is separator → NIL")))

(test pane-at-position-returns-nil-for-empty-window
  "pane-at-position returns NIL when the window has no panes."
  (let ((win (make-window :id 1 :name "w" :panes nil)))
    (is (null (pane-at-position win 0 0))
        "pane-at-position on empty window must return NIL")))

;;; ── pane-live-p direct unit tests ────────────────────────────────────────────

(test pane-live-p-table
  "pane-live-p returns T only when fd > 0; fd <= 0 and NIL are all not-live.
   :nil sentinel means pass NIL directly instead of creating a pane.
   Each row: (fd expected description)."
  (dolist (row '((5    t   "pane with fd > 0 must be live")
                 (-1   nil "pane with fd = -1 must not be live")
                 (0    nil "pane with fd = 0 must not be reported as live")
                 (:nil nil "pane-live-p NIL must return NIL")))
    (destructuring-bind (fd expected desc) row
      (let ((pane (if (eq fd :nil)
                      nil
                      (make-pane :id 1 :x 0 :y 0 :width 80 :height 24
                                 :fd fd :pid -1 :screen (make-screen 80 24)))))
        (if expected
            (is-true  (pane-live-p pane) desc)
            (is-false (pane-live-p pane) desc))))))

;;; ── pane-pipe-active-p direct unit tests ─────────────────────────────────────

(test pane-pipe-active-p-table
  "pane-pipe-active-p returns truthy when any pipe slot is non-NIL, NIL otherwise.
   :nil sentinel means pass NIL directly. :none means no slot is set.
   Each row: (setup expected description)."
  (dolist (row '((:none      nil "pane with no pipe resources must not be active")
                 (:pipe-fd   t   "pipe-fd set => pipe must be active")
                 (:pipe-out  t   "pipe-output-stream set => pipe must be active")
                 (:pipe-proc t   "pipe-process set => pipe must be active")
                 (:nil       nil "pane-pipe-active-p NIL must return NIL")))
    (destructuring-bind (setup expected desc) row
      (let ((pane (unless (eq setup :nil) (make-no-pty-pane 1 0 0 80 24))))
        (ecase setup
          (:none      nil)
          (:pipe-fd   (setf (pane-pipe-fd             pane) :fake-fd))
          (:pipe-out  (setf (pane-pipe-output-stream  pane) :fake-stream))
          (:pipe-proc (setf (pane-pipe-process        pane) :fake-process))
          (:nil       nil))
        (if expected
            (is-true  (pane-pipe-active-p pane) desc)
            (is-false (pane-pipe-active-p pane) desc))))))

;;; ── %pane-border-status-reservation direct tests ─────────────────────────────
;;;
;;; The path where height = 1 means the pane is "too short" — even when
;;; status is "top" or "bottom", no row can be reserved without leaving
;;; the content height at 0.  The function must fall back to offset=0, height=1.

(test pane-border-status-reservation-too-short-does-not-reserve
  "%pane-border-status-reservation returns (0, height) for any non-off status
   when height is 1 (too short to reserve a row for the title bar)."
  (dolist (status '("top" "bottom"))
    (multiple-value-bind (y-offset content-height)
        (cl-tmux/model::%pane-border-status-reservation status 1)
      (is (= 0 y-offset)
          "~A with height=1: y-offset must be 0 (no row to reserve)" status)
      (is (= 1 content-height)
          "~A with height=1: content-height must be 1 (no reservation)" status))))

(test pane-border-status-reservation-top-normal
  "%pane-border-status-reservation with status=top and height > 1 reserves the
   first row: y-offset 1, content-height (height - 1)."
  (multiple-value-bind (y-offset content-height)
      (cl-tmux/model::%pane-border-status-reservation "top" 10)
    (is (= 1 y-offset) "top: y-offset must be 1")
    (is (= 9 content-height) "top: content-height must be height-1")))

(test pane-border-status-reservation-off-returns-full-height
  "%pane-border-status-reservation with status=off returns (0, height) regardless
   of height — the off path must never reserve a row."
  (multiple-value-bind (y-offset content-height)
      (cl-tmux/model::%pane-border-status-reservation "off" 24)
    (is (= 0 y-offset) "off: y-offset must be 0")
    (is (= 24 content-height) "off: content-height must be full height")))
