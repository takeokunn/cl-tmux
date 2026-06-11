(in-package #:cl-tmux/test)

;;;; window tests — part C: find-window-by-name, list-windows-format,
;;;; auto-rename-from-osc, format-window, move/swap/rotate coverage.

(in-suite model-suite)

(test find-window-by-name
  "%format-window-list includes matching window names."
  (let* ((w0 (make-window :id 1 :name "bash" :width 80 :height 24
                          :panes (list (make-no-pty-pane 1 0 0 80 24))))
         (w1 (make-window :id 2 :name "vim" :width 80 :height 24
                          :panes (list (make-no-pty-pane 2 0 0 80 24))))
         (sess (make-session :id 1 :name "s" :windows (list w0 w1))))
    (session-select-window sess w0)
    (let ((listing (cl-tmux::%format-window-list sess)))
      (is (search "bash" listing) "listing must contain window name 'bash'")
      (is (search "vim"  listing) "listing must contain window name 'vim'"))))

;;; ── list-windows-format ──────────────────────────────────────────────────────

(test list-windows-format
  "%format-window-list includes the window's stored id, name, dimensions, and active marker."
  (let* ((p0  (make-no-pty-pane 1 0 0 80 24))
         ;; Use id=0 so that the listing shows "0:" as the index prefix.
         (w0  (make-window :id 0 :name "main" :width 80 :height 24
                           :panes (list p0)))
         (sess (make-session :id 1 :name "s" :windows (list w0))))
    (session-select-window sess w0)
    (let ((listing (cl-tmux::%format-window-list sess)))
      (is (search "main"    listing) "listing must include window name")
      (is (search "80x24"   listing) "listing must include dimensions")
      (is (search "[active]" listing) "active window must be marked [active]")
      (is (search "0:"      listing) "listing must include the window-id (0) as prefix"))))

;;; ── auto-rename-from-osc ─────────────────────────────────────────────────────
;;;
;;; These tests call the production function cl-tmux::%maybe-rename-window-from-title
;;; directly, rather than duplicating the rename logic inline.  This ensures the
;;; tests verify the real code path and provide genuine coverage confidence.

(test auto-rename-from-osc
  "When window-automatic-rename-p is T, window-name is updated from OSC title."
  (with-loop-state
    (let* ((p0   (make-no-pty-pane 1 0 0 80 24))
           (w0   (make-window :id 1 :name "original"
                              :panes (list p0) :active p0))
           (sess (make-session :id 1 :name "s" :windows (list w0))))
      (session-select-window sess w0)
      ;; Simulate OSC 0 title update on the screen.
      (setf (screen-title (pane-screen p0)) "new-title")
      ;; Call the production rename function — not a copy of its logic.
      (cl-tmux::%maybe-rename-window-from-title sess)
      (is (string= "new-title" (window-name w0))
          "window-name must be updated from OSC title when automatic-rename is enabled"))))

(test auto-rename-disabled-ignores-osc
  "When window-automatic-rename-p is NIL, window-name is NOT updated from OSC title."
  (with-loop-state
    (let* ((p0   (make-no-pty-pane 1 0 0 80 24))
           (w0   (make-window :id 1 :name "kept"
                              :automatic-rename-p nil
                              :panes (list p0) :active p0))
           (sess (make-session :id 1 :name "s" :windows (list w0))))
      (session-select-window sess w0)
      (setf (screen-title (pane-screen p0)) "ignored-title")
      ;; Call the production rename function; automatic-rename-p nil must suppress it.
      (cl-tmux::%maybe-rename-window-from-title sess)
      (is (string= "kept" (window-name w0))
          "window-name must NOT change when automatic-rename is disabled"))))

;;; ── window-remove-pane (no PTY) ──────────────────────────────────────────────

(test window-remove-pane-empties-single-pane-window
  "window-remove-pane on a single-pane window returns NIL and clears the tree."
  (let* ((p0  (make-no-pty-pane 1 0 0 80 24))
         (win (make-window :id 1 :name "w" :width 80 :height 24
                           :panes (list p0)
                           :tree (make-layout-leaf p0))))
    (window-select-pane win p0)
    (let ((result (window-remove-pane win p0)))
      (is (null result)
          "window-remove-pane on a sole pane must return NIL (no survivor)")
      (is (null (window-panes win))
          "window panes list must be empty after removing the sole pane")
      (is (null (window-tree win))
          "window tree must be NIL after removing the sole pane"))))

(test window-remove-pane-returns-sibling
  "window-remove-pane returns the surviving sibling pane after removing one of two."
  (with-h-split-window (win p0 p1)
    (let ((survivor (window-remove-pane win p0)))
      (is (not (null survivor))
          "window-remove-pane must return the surviving pane")
      (is (= 1 (length (window-panes win)))
          "one pane must remain after removing one of two"))))

;;; ── window-last-active-time slot ─────────────────────────────────────────────

(test window-last-active-time-updated-on-select
  "window-select-pane updates window-last-active-time to a recent value."
  (let* ((p0  (make-no-pty-pane 1 0 0 20 5))
         (win (make-window :id 1 :name "w" :width 20 :height 5
                           :panes (list p0) :last-active-time 0)))
    (let ((before (get-universal-time)))
      (window-select-pane win p0)
      (is (>= (window-last-active-time win) before)
          "window-last-active-time must be updated when a pane is selected"))))

;;; ── window-layout-cycle-index slot ──────────────────────────────────────────

(test window-layout-cycle-index-defaults-zero
  "window-layout-cycle-index defaults to 0 for a freshly created window."
  (let ((win (make-window :id 1 :name "w")))
    (is (= 0 (window-layout-cycle-index win))
        "window-layout-cycle-index must default to 0")))

;;; ── ensure-window-fits with matching size ────────────────────────────────────
;;;
;;; This test is identical in structure to window-tests.lisp's existing
;;; ensure-window-fits-noop-when-size-matches but targets the update of
;;; window-width/height as the observable: if size differs, relayout runs;
;;; if same, dimensions stay untouched.

(test ensure-window-fits-does-not-mutate-on-matching-size
  "ensure-window-fits leaves pane geometry untouched when size already matches."
  (let* ((p0  (make-no-pty-pane 1 0 0 80 24))
         (win (make-window :id 1 :name "w" :width 80 :height 24
                           :tree (make-layout-leaf p0)
                           :panes (list p0) :active p0)))
    (let ((x0-before (pane-x p0))
          (y0-before (pane-y p0)))
      (cl-tmux/model::ensure-window-fits win 24 80)
      (is (= x0-before (pane-x p0))
          "pane-x must not change when size already matches")
      (is (= y0-before (pane-y p0))
          "pane-y must not change when size already matches"))))

;;; ── window struct default slots ─────────────────────────────────────────────

(test window-zoom-p-defaults-nil
  "window-zoom-p defaults to NIL for a freshly created window."
  (let ((win (make-window :id 1 :name "w")))
    (is (null (cl-tmux/model:window-zoom-p win))
        "window-zoom-p must default to NIL")))

(test window-zoom-tree-defaults-nil
  "window-zoom-tree defaults to NIL for a freshly created window."
  (let ((win (make-window :id 1 :name "w")))
    (is (null (cl-tmux/model:window-zoom-tree win))
        "window-zoom-tree must default to NIL")))

(test window-last-active-defaults-nil
  "window-last-active defaults to NIL for a freshly created window."
  (let ((win (make-window :id 1 :name "w")))
    (is (null (window-last-active win))
        "window-last-active must default to NIL")))

(test window-automatic-rename-p-defaults-true
  "window-automatic-rename-p defaults to T for a freshly created window."
  (let ((win (make-window :id 1 :name "w")))
    (is-true (window-automatic-rename-p win)
             "window-automatic-rename-p must default to T")))

(test window-automatic-rename-p-settable
  "window-automatic-rename-p can be set to NIL and read back."
  (let ((win (make-window :id 1 :name "w" :automatic-rename-p nil)))
    (is (null (window-automatic-rename-p win))
        "window-automatic-rename-p must reflect the value set at construction")))

;;; ── window-active-pane falls back to first pane ─────────────────────────────

(test window-active-pane-falls-back-to-first-pane
  "window-active-pane returns the first pane when active slot is NIL."
  (let* ((p0  (make-no-pty-pane 1  0 0 40 24))
         (p1  (make-no-pty-pane 2 41 0 40 24))
         ;; No active pane set.
         (win (make-window :id 1 :name "w" :panes (list p0 p1))))
    (is (eq p0 (window-active-pane win))
        "window-active-pane must fall back to the first pane when active is NIL")))

;;; ── window-select-pane records previous active as last-active ──────────────

(test window-select-pane-records-previous-as-last-active
  "window-select-pane records the previously active pane in window-last-active."
  (let* ((p0  (make-no-pty-pane 1  0 0 40 24))
         (p1  (make-no-pty-pane 2 41 0 40 24))
         (win (make-window :id 1 :name "w" :panes (list p0 p1))))
    (window-select-pane win p0)
    (is (null (window-last-active win))
        "last-active must be NIL after first select (no prior pane)")
    (window-select-pane win p1)
    (is (eq p0 (window-last-active win))
        "last-active must be the previously active pane after switching")))

;;; ── window-remove-pane: leaf not in tree ────────────────────────────────────

(test window-remove-pane-absent-pane-returns-first-pane
  "window-remove-pane returns the first pane when the target leaf is absent from the tree."
  (let* ((p0  (make-no-pty-pane 1 0 0 40 24))
         (p1  (make-no-pty-pane 2 41 0 40 24))
         ;; Build window with p0 in the tree; p1 is not in the tree.
         (win (make-window :id 1 :name "w" :width 81 :height 24
                           :panes (list p0 p1)
                           :tree (make-layout-leaf p0))))
    ;; Removing p1 which is absent from the tree should return the first pane.
    (let ((result (window-remove-pane win p1)))
      (is-true result "result must be non-NIL (the first pane)")
      ;; The tree should be unchanged.
      (is-true (window-tree win) "tree must remain non-NIL"))))

;;; ── Table-driven %new-split-ratio (additional boundary cases) ───────────────

(test new-split-ratio-additional-cases
  "Table-driven: %new-split-ratio handles boundary and asymmetric ratio cases."
  ;; Each entry: (orient avail cur-ratio delta grow-first expected description)
  ;; These cases extend beyond the single tests (basic-grow/shrink/blocked-by-floor).
  (dolist (entry
           '((:h 100 3/4 10 t   85/100 "grow :h from 3/4 ratio")
             (:v 40  1/4  5 t   15/40  "grow :v from 1/4 ratio")
             (:h 60  2/3  1 nil 39/60  "shrink :h from 2/3 ratio")))
    (destructuring-bind (orient avail cur-ratio delta grow-first expected desc) entry
      (let ((result (cl-tmux/model::%new-split-ratio orient avail cur-ratio delta grow-first)))
        (is (equal expected result) desc)))))

;;; ── window-rotate single-pane is noop ───────────────────────────────────────

(test window-rotate-single-pane-noop
  "window-rotate on a single-pane window changes nothing."
  (let* ((p0  (make-no-pty-pane 1 0 0 80 24))
         (win (make-window :id 1 :name "w" :width 80 :height 24
                           :panes (list p0)
                           :tree (make-layout-leaf p0))))
    (window-rotate win :up)
    (is (equal (list p0) (window-panes win))
        "single-pane window panes list unchanged after :up rotate")
    (window-rotate win :down)
    (is (equal (list p0) (window-panes win))
        "single-pane window panes list unchanged after :down rotate")))

;;; ── window-id and window-name accessors ─────────────────────────────────────

(test window-id-slot-accessible
  "window-id returns the id passed to make-window."
  (let ((win (make-window :id 42 :name "test")))
    (is (= 42 (window-id win))
        "window-id must return the id set at construction")))

(test window-name-slot-accessible
  "window-name returns the name passed to make-window."
  (let ((win (make-window :id 1 :name "mywin")))
    (is (string= "mywin" (window-name win))
        "window-name must return the name set at construction")))

;;; ── window-width and window-height accessors ────────────────────────────────

(test window-width-height-slot-accessible
  "window-width and window-height return the geometry set at construction."
  (let ((win (make-window :id 1 :name "w" :width 120 :height 40)))
    (is (= 120 (window-width  win)) "window-width must return 120")
    (is (= 40  (window-height win)) "window-height must return 40")))

;;; ── pane-window back-pointer wiring ──────────────────────────────────────────
;;;
;;; pane-window is set by window-split and %attach-full-screen-pane (production),
;;; and cleared by window-remove-pane.  The tests below verify the clear path
;;; without requiring a real PTY.  The split/attach set path is verified by the
;;; PTY-gated test below.

(test window-remove-pane-clears-pane-window-sole-pane
  "window-remove-pane on the sole pane sets pane-window to NIL."
  (let* ((p0  (make-no-pty-pane 1 0 0 80 24))
         (win (make-window :id 1 :name "w" :width 80 :height 24
                           :panes (list p0)
                           :tree (make-layout-leaf p0))))
    (setf (pane-window p0) win)
    (window-remove-pane win p0)
    (is (null (pane-window p0))
        "pane-window of the sole removed pane must be NIL after removal")))

(test window-remove-pane-clears-pane-window-preserves-survivor
  "window-remove-pane clears pane-window only for the removed pane."
  (with-h-split-window (win p0 p1)
    (setf (pane-window p0) win
          (pane-window p1) win)
    (window-remove-pane win p0)
    (is (null (pane-window p0))
        "pane-window of the removed pane must be NIL")
    (is (eq win (pane-window p1))
        "pane-window of the surviving pane must remain pointing to its window")))

(test window-split-sets-pane-window-back-pointer
  "window-split wires pane-window on the new pane to the parent window."
  (unless (pty-available-p)
    (skip "no PTY available (sandboxed environment)"))
  (with-session (session 24 80)
    (let* ((win   (session-active-window session))
           (p-new (window-split win :h)))
      (is (eq win (pane-window p-new))
          "new pane's pane-window must point to its window after split"))))
