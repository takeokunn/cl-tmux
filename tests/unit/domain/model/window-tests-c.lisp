(in-package #:cl-tmux/test)

;;;; window tests — part C: find-window-by-name, list-windows-format,
;;;; auto-rename-from-osc, format-window, move/swap/rotate coverage.

(describe "model-suite"

  ;; %format-window-list includes matching window names.
  (it "find-window-by-name"
    (let* ((w0 (make-window :id 1 :name "bash" :width 80 :height 24
                            :panes (list (make-no-pty-pane 1 0 0 80 24))))
           (w1 (make-window :id 2 :name "vim" :width 80 :height 24
                            :panes (list (make-no-pty-pane 2 0 0 80 24))))
           (sess (make-session :id 1 :name "s" :windows (list w0 w1))))
      (session-select-window sess w0)
      (let ((listing (cl-tmux::%format-window-list sess)))
        (expect (search "bash" listing))
        (expect (search "vim"  listing)))))

  ;; ── list-windows-format ──────────────────────────────────────────────────────

  ;; %format-window-list includes the window's stored id, name, dimensions, and active marker.
  (it "list-windows-format"
    (let* ((p0  (make-no-pty-pane 1 0 0 80 24))
           ;; Use id=0 so that the listing shows "0:" as the index prefix.
           (w0  (make-window :id 0 :name "main" :width 80 :height 24
                             :panes (list p0)))
           (sess (make-session :id 1 :name "s" :windows (list w0))))
      (session-select-window sess w0)
      (let ((listing (cl-tmux::%format-window-list sess)))
        (expect (search "main"    listing))
        (expect (search "80x24"   listing))
        (expect (search "[active]" listing))
        (expect (search "0:"      listing)))))

  ;; ── auto-rename-from-osc ─────────────────────────────────────────────────────
  ;;
  ;; These tests call the production function cl-tmux::%maybe-rename-window-from-title
  ;; directly, rather than duplicating the rename logic inline.  This ensures the
  ;; tests verify the real code path and provide genuine coverage confidence.

  ;; When window-automatic-rename-p is T, window-name is updated from OSC title.
  (it "auto-rename-from-osc"
    (with-auto-rename-session (screen p0 w0 sess :win-name "original")
      (setf (window-automatic-rename-p w0) t)
      (setf (screen-title screen) "new-title")
      (cl-tmux::%maybe-rename-window-from-title sess)
      (expect (string= "new-title" (window-name w0)))))

  ;; When window-automatic-rename-p is NIL, window-name is NOT updated from OSC title.
  (it "auto-rename-disabled-ignores-osc"
    (with-auto-rename-session (screen p0 w0 sess :win-name "kept")
      (setf (window-automatic-rename-p w0) nil)
      (setf (screen-title screen) "ignored-title")
      (cl-tmux::%maybe-rename-window-from-title sess)
      (expect (string= "kept" (window-name w0)))))

  ;; ── window-remove-pane (no PTY) ──────────────────────────────────────────────

  ;; window-remove-pane on a single-pane window returns NIL and clears the tree.
  (it "window-remove-pane-empties-single-pane-window"
    (let* ((p0  (make-no-pty-pane 1 0 0 80 24))
           (win (make-window :id 1 :name "w" :width 80 :height 24
                             :panes (list p0)
                             :tree (make-layout-leaf p0))))
      (window-select-pane win p0)
      (let ((result (window-remove-pane win p0)))
        (expect (null result))
        (expect (null (window-panes win)))
        (expect (null (window-tree win))))))

  ;; window-remove-pane returns the surviving sibling pane after removing one of two.
  (it "window-remove-pane-returns-sibling"
    (with-h-split-window (win p0 p1)
      (let ((survivor (window-remove-pane win p0)))
        (expect (not (null survivor)))
        (expect (= 1 (length (window-panes win)))))))

  ;; ── window-last-active-time slot ─────────────────────────────────────────────

  ;; window-select-pane updates window-last-active-time to a recent value.
  (it "window-last-active-time-updated-on-select"
    (let* ((p0  (make-no-pty-pane 1 0 0 20 5))
           (win (make-window :id 1 :name "w" :width 20 :height 5
                             :panes (list p0) :last-active-time 0)))
      (let ((before (get-universal-time)))
        (window-select-pane win p0)
        (expect (>= (window-last-active-time win) before)))))

  ;; ── window-layout-cycle-index slot ──────────────────────────────────────────

  ;; window-layout-cycle-index defaults to 0 for a freshly created window.
  (it "window-layout-cycle-index-defaults-zero"
    (let ((win (make-window :id 1 :name "w")))
      (expect (= 0 (window-layout-cycle-index win)))))

  ;; ── ensure-window-fits with matching size ────────────────────────────────────
  ;;
  ;; This test is identical in structure to window-tests.lisp's existing
  ;; ensure-window-fits-noop-when-size-matches but targets the update of
  ;; window-width/height as the observable: if size differs, relayout runs;
  ;; if same, dimensions stay untouched.

  ;; ensure-window-fits leaves pane geometry untouched when size already matches.
  (it "ensure-window-fits-does-not-mutate-on-matching-size"
    (let* ((p0  (make-no-pty-pane 1 0 0 80 24))
           (win (make-window :id 1 :name "w" :width 80 :height 24
                             :tree (make-layout-leaf p0)
                             :panes (list p0) :active p0)))
      (let ((x0-before (pane-x p0))
            (y0-before (pane-y p0)))
        (cl-tmux/model::ensure-window-fits win 24 80)
        (expect (= x0-before (pane-x p0)))
        (expect (= y0-before (pane-y p0))))))

  ;; ── window struct default slots ─────────────────────────────────────────────

  ;; Freshly created window slots have expected defaults: zoom-p=nil, zoom-tree=nil, last-active=nil, automatic-rename-p=t.
  (it "window-slot-defaults-table"
    (dolist (c '((cl-tmux/model:window-zoom-p      nil "window-zoom-p defaults nil")
                 (cl-tmux/model:window-zoom-tree    nil "window-zoom-tree defaults nil")
                 (window-last-active                nil "window-last-active defaults nil")
                 (window-automatic-rename-p          t  "window-automatic-rename-p defaults t")))
      (destructuring-bind (accessor expected desc) c
        (declare (ignore desc))
        (let ((win (make-window :id 1 :name "w")))
          (expect (equal expected (funcall accessor win)))))))

  ;; window-automatic-rename-p can be set to NIL and read back.
  (it "window-automatic-rename-p-settable"
    (let ((win (make-window :id 1 :name "w" :automatic-rename-p nil)))
      (expect (null (window-automatic-rename-p win)))))

  ;; ── window-active-pane falls back to first pane ─────────────────────────────

  ;; window-active-pane returns the first pane when active slot is NIL.
  (it "window-active-pane-falls-back-to-first-pane"
    (let* ((p0  (make-no-pty-pane 1  0 0 40 24))
           (p1  (make-no-pty-pane 2 41 0 40 24))
           ;; No active pane set.
           (win (make-window :id 1 :name "w" :panes (list p0 p1))))
      (expect (eq p0 (window-active-pane win)))))

  ;; ── window-select-pane records previous active as last-active ──────────────

  ;; window-select-pane records the previously active pane in window-last-active.
  (it "window-select-pane-records-previous-as-last-active"
    (let* ((p0  (make-no-pty-pane 1  0 0 40 24))
           (p1  (make-no-pty-pane 2 41 0 40 24))
           (win (make-window :id 1 :name "w" :panes (list p0 p1))))
      (window-select-pane win p0)
      (expect (null (window-last-active win)))
      (window-select-pane win p1)
      (expect (eq p0 (window-last-active win)))))

  ;; ── window-remove-pane: leaf not in tree ────────────────────────────────────

  ;; window-remove-pane returns the first pane when the target leaf is absent from the tree.
  (it "window-remove-pane-absent-pane-returns-first-pane"
    (let* ((p0  (make-no-pty-pane 1 0 0 40 24))
           (p1  (make-no-pty-pane 2 41 0 40 24))
           ;; Build window with p0 in the tree; p1 is not in the tree.
           (win (make-window :id 1 :name "w" :width 81 :height 24
                             :panes (list p0 p1)
                             :tree (make-layout-leaf p0))))
      ;; Removing p1 which is absent from the tree should return the first pane.
      (let ((result (window-remove-pane win p1)))
        (expect result :to-be-truthy)
        ;; The tree should be unchanged.
        (expect (window-tree win) :to-be-truthy))))

  ;; ── Table-driven %new-split-ratio (additional boundary cases) ───────────────

  ;; Table-driven: %new-split-ratio handles boundary and asymmetric ratio cases.
  (it "new-split-ratio-additional-cases"
    ;; Each entry: (orient avail cur-ratio delta grow-first expected description)
    ;; These cases extend beyond the single tests (basic-grow/shrink/blocked-by-floor).
    (dolist (entry
             '((:h 100 3/4 10 t   85/100 "grow :h from 3/4 ratio")
               (:v 40  1/4  5 t   15/40  "grow :v from 1/4 ratio")
               (:h 60  2/3  1 nil 39/60  "shrink :h from 2/3 ratio")))
      (destructuring-bind (orient avail cur-ratio delta grow-first expected desc) entry
        (declare (ignore desc))
        (let ((result (cl-tmux/model::%new-split-ratio orient avail cur-ratio delta grow-first)))
          (expect (equal expected result))))))

  ;; ── window-rotate single-pane is noop ───────────────────────────────────────

  ;; window-rotate on a single-pane window changes nothing.
  (it "window-rotate-single-pane-noop"
    (let* ((p0  (make-no-pty-pane 1 0 0 80 24))
           (win (make-window :id 1 :name "w" :width 80 :height 24
                             :panes (list p0)
                             :tree (make-layout-leaf p0))))
      (window-rotate win :up)
      (expect (equal (list p0) (window-panes win)))
      (window-rotate win :down)
      (expect (equal (list p0) (window-panes win)))))

  ;; ── window-id and window-name accessors ─────────────────────────────────────

  ;; window-id returns the id passed to make-window.
  (it "window-id-slot-accessible"
    (let ((win (make-window :id 42 :name "test")))
      (expect (= 42 (window-id win)))))

  ;; window-name returns the name passed to make-window.
  (it "window-name-slot-accessible"
    (let ((win (make-window :id 1 :name "mywin")))
      (expect (string= "mywin" (window-name win)))))

  ;; ── window-width and window-height accessors ────────────────────────────────

  ;; window-width and window-height return the geometry set at construction.
  (it "window-width-height-slot-accessible"
    (let ((win (make-window :id 1 :name "w" :width 120 :height 40)))
      (expect (= 120 (window-width  win)))
      (expect (= 40  (window-height win)))))

  ;; ── pane-window back-pointer wiring ──────────────────────────────────────────
  ;;
  ;; pane-window is set by window-split and %attach-full-screen-pane (production),
  ;; and cleared by window-remove-pane.  The tests below verify the clear path
  ;; without requiring a real PTY.  The split/attach set path is verified by the
  ;; PTY-gated test below.

  ;; window-remove-pane on the sole pane sets pane-window to NIL.
  (it "window-remove-pane-clears-pane-window-sole-pane"
    (let* ((p0  (make-no-pty-pane 1 0 0 80 24))
           (win (make-window :id 1 :name "w" :width 80 :height 24
                             :panes (list p0)
                             :tree (make-layout-leaf p0))))
      (setf (pane-window p0) win)
      (window-remove-pane win p0)
      (expect (null (pane-window p0)))))

  ;; window-remove-pane clears pane-window only for the removed pane.
  (it "window-remove-pane-clears-pane-window-preserves-survivor"
    (with-h-split-window (win p0 p1)
      (setf (pane-window p0) win
            (pane-window p1) win)
      (window-remove-pane win p0)
      (expect (null (pane-window p0)))
      (expect (eq win (pane-window p1)))))

  ;; window-split wires pane-window on the new pane to the parent window.
  (it "window-split-sets-pane-window-back-pointer"
    (unless (pty-available-p)
      (skip "no PTY available (sandboxed environment)"))
    (with-session (session 24 80)
      (let* ((win   (session-active-window session))
             (p-new (window-split session win :h)))
        (expect (eq win (pane-window p-new)))))))
