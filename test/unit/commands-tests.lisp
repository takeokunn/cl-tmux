(in-package #:cl-tmux/test)

;;;; resize-pane, copy-mode-scroll, kill-pane, select-window, rename-session, copy-mode begin/yank/other-end — part I

(def-suite commands-suite :description "Pane resize / copy-mode-scroll / kill-pane command logic (src/commands.lisp)")
(in-suite commands-suite)

;;; ── Local fixtures (no PTY: fd -1, pid -1) ──────────────────────────────────
;;;
;;; Tree-based split windows: assembled with make-layout-leaf / make-layout-split
;;; to avoid legacy flat-layout paths.  tl-leaf / tl-window helpers are defined
;;; in layout-tree-tests.lisp and share the same cl-tmux/test package.

(defun %vsplit-window (&optional (each 20))
  "Vertical split: two EACH-wide panes, one separator column between them."
  (let* ((w   (+ each 1 each))
         (p0  (make-pane :id 1 :x 0          :y 0 :width each :height 5
                         :fd -1 :pid -1 :screen (make-screen each 5)))
         (p1  (make-pane :id 2 :x (+ each 1) :y 0 :width each :height 5
                         :fd -1 :pid -1 :screen (make-screen each 5)))
         (win (make-window :id 1 :name "w" :width w :height 5
                           :tree (make-layout-split :h
                                    (make-layout-leaf p0) (make-layout-leaf p1)
                                    (/ each (- w 1)))
                           :panes (list p0 p1))))
    (window-select-pane win p0)
    win))

(defun %hsplit-window (&optional (each 10))
  "Horizontal split: two EACH-tall panes, one separator row between them."
  (let* ((h   (+ each 1 each))
         (p0  (make-pane :id 1 :x 0 :y 0          :width 20 :height each
                         :fd -1 :pid -1 :screen (make-screen 20 each)))
         (p1  (make-pane :id 2 :x 0 :y (+ each 1) :width 20 :height each
                         :fd -1 :pid -1 :screen (make-screen 20 each)))
         (win (make-window :id 1 :name "w" :width 20 :height h
                           :tree (make-layout-split :v
                                    (make-layout-leaf p0) (make-layout-leaf p1)
                                    (/ each (- h 1)))
                           :panes (list p0 p1))))
    (window-select-pane win p0)
    win))

(defun %make-session-with-window (win)
  "Create a minimal session containing WIN as the active window."
  (let ((sess (make-session :id 1 :name "0" :windows (list win))))
    (session-select-window sess win)
    sess))

;;; ── resize-pane: vertical split ─────────────────────────────────────────────

(test resize-vertical-right-grows-active-shrinks-neighbour
  "On a vertical split, :right grows the active (left) pane and shrinks its
   right neighbour; resize-pane delegates to window-resize-active."
  (let* ((win (%vsplit-window 20))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win))))
    (is (eq p0 (resize-pane win :right 5)) "returns the active pane on resize")
    (is (= 25 (pane-width p0)) "active pane grows by amount")
    (is (= 15 (pane-width p1)) "neighbour shrinks by amount")
    (is (= 26 (pane-x p1))
        "neighbour x = active.x + active.width + 1 (separator column)")))

(test resize-vertical-left-picks-previous-neighbour
  "When the active pane is the right one, :left adjusts against the previous
   (left) neighbour rather than a non-existent right neighbour."
  (let* ((win (%vsplit-window 20))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win))))
    (window-select-pane win p1)            ; make the right pane active
    (is (eq p1 (resize-pane win :left 5)))
    (is (= 15 (pane-width p1)) "active shrinks toward the left")
    (is (= 25 (pane-width p0)) "previous neighbour grows")))

(test resize-pane-no-tree-returns-nil
  "resize-pane with no tree (NIL) returns NIL immediately."
  (let* ((p0  (%make-test-pane))
         (win (make-window :id 1 :name "w" :width 20 :height 5
                           :panes (list p0) :tree nil)))
    (window-select-pane win p0)
    (is (null (resize-pane win :right 5)) "no tree => NIL")))

;;; ── resize-pane: horizontal split ───────────────────────────────────────────

(test resize-horizontal-down-grows-active-shrinks-lower
  "On a horizontal split, :down grows the active (upper) pane and shrinks the
   lower neighbour; the neighbour's y slides one row past the grown pane."
  (let* ((win (%hsplit-window 10))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win))))
    (is (eq p0 (resize-pane win :down 3)))
    (is (= 13 (pane-height p0)) "upper pane grows by amount")
    (is (= 7  (pane-height p1)) "lower pane shrinks by amount")))

;;; ── resize-pane -x / -y: absolute size (command arg form) ────────────────────

(test resize-pane-x-absolute-grows-active
  "resize-pane -x N sets the active pane to an absolute width of N (grow case)."
  (let* ((win (%vsplit-window 20))
         (p0  (first (window-panes win)))
         (s   (%make-session-with-window win)))
    (cl-tmux::%cmd-resize-pane-arg s '("-x" "25"))
    (is (= 25 (pane-width p0)) "resize-pane -x 25 must make the active pane 25 wide")))

(test resize-pane-x-absolute-shrinks-active
  "resize-pane -x N shrinks the active pane when N < current width — verifies the
   signed-delta border move shrinks as well as grows."
  (let* ((win (%vsplit-window 20))
         (p0  (first (window-panes win)))
         (s   (%make-session-with-window win)))
    (cl-tmux::%cmd-resize-pane-arg s '("-x" "15"))
    (is (= 15 (pane-width p0)) "resize-pane -x 15 must shrink the active pane to 15")))

(test resize-pane-y-absolute-sets-height
  "resize-pane -y N sets the active pane to an absolute height of N."
  (let* ((win (%hsplit-window 10))
         (p0  (first (window-panes win)))
         (s   (%make-session-with-window win)))
    (cl-tmux::%cmd-resize-pane-arg s '("-y" "13"))
    (is (= 13 (pane-height p0)) "resize-pane -y 13 must make the active pane 13 tall")))

;;; ── copy-mode-scroll ─────────────────────────────────────────────────────────

(defun %make-test-pane (&key (id 1) (x 0) (y 0) (w 20) (h 5))
  "Return a no-PTY pane with a fresh screen of W x H."
  (make-pane :id id :x x :y y :width w :height h
             :fd -1 :pid -1 :screen (make-screen w h)))

(defun %screen-with-scrollback (n)
  "A copy-mode screen carrying N scrollback rows (contents irrelevant)."
  (let ((s (make-screen 20 5)))
    (setf (screen-scrollback s)
          (loop repeat n collect (make-array 0)))
    (cl-tmux/commands::copy-mode-enter s)
    s))

(test copy-mode-scroll-back-clamps-to-scrollback-length
  "Scrolling back (positive delta) past the oldest line clamps the copy-offset
   to the scrollback length."
  (let ((s (%screen-with-scrollback 3)))
    (cl-tmux/commands::copy-mode-scroll s 100)
    (is (= 3 (screen-copy-offset s)) "offset clamped to scrollback length")
    (is-true (cl-tmux/terminal/types:screen-dirty-p s)
             "scrolling marks the screen dirty")))

(test copy-mode-scroll-forward-clamps-at-zero
  "Scrolling forward (negative delta) past the live view clamps the offset at 0."
  (let ((s (%screen-with-scrollback 3)))
    (cl-tmux/commands::copy-mode-scroll s 100)   ; first jump to the oldest line
    (is (= 3 (screen-copy-offset s)))
    (cl-tmux/commands::copy-mode-scroll s -100)  ; then race back to live
    (is (= 0 (screen-copy-offset s)) "offset clamped at 0")))

(test copy-mode-selection-honours-scroll-offset
  "A full-row selection made while scrolled back into the scrollback yanks the
   text the user SEES at that viewport row (via screen-display-cell), not the
   live-grid row at the same index.  Regression guard for the screen-cell ->
   screen-display-cell fix in %extract-row-chars."
  (let ((s (make-screen 8 3)))
    (feed-lines s "AAA" "BBB" "CCC" "DDD" "EEE")
    (cl-tmux/commands::copy-mode-enter s)
    (cl-tmux/commands::copy-mode-scroll s 1000)   ; scroll fully back
    (is (plusp (screen-copy-offset s))
        "precondition: 5 lines on a height-3 screen must create scrollback")
    (let ((w      (screen-width s))
          (offset (screen-copy-offset s)))
      (let ((expected (string-right-trim " " (display-row-string s 0))))
        ;; Mark and cursor at viewport row 0; supply the offset so %selection-bounds
        ;; can correctly compute virtual rows even though we're setting mark manually.
        (setf (screen-copy-mark        s) (cons 0 0)
              (screen-copy-mark-offset s) offset
              (screen-copy-cursor      s) (cons 0 w)
              (screen-copy-selecting   s) t)
        (is (string= expected
                     (string-right-trim " " (or (cl-tmux/commands::%selection-text s) "")))
            "scrolled-back selection yanks the displayed (scrollback) text, not the live row")))))

(test copy-mode-enter-e-sets-exit-on-bottom
  "copy-mode-enter with :exit-on-bottom t sets the screen slot."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s :exit-on-bottom t)
    (is-true (cl-tmux/terminal/types:screen-copy-exit-on-bottom s)
             "exit-on-bottom slot must be set by -e")))

(test copy-mode-e-auto-exits-on-scroll-to-bottom
  "With exit-on-bottom (copy-mode -e), scrolling back down to the live bottom
   (offset 0) auto-exits copy mode."
  (let ((s (%screen-with-scrollback 3)))
    ;; Re-enter with -e semantics and scroll up into the scrollback.
    (cl-tmux/commands::copy-mode-enter s :exit-on-bottom t)
    (cl-tmux/commands::copy-mode-scroll s 2)        ; scroll back 2 lines (offset 2)
    (is (= 2 (screen-copy-offset s)) "scrolled back into scrollback")
    (is-true (screen-copy-mode-p s) "still in copy mode while scrolled up")
    (cl-tmux/commands::copy-mode-scroll s -100)     ; race back to the live bottom
    (is-false (screen-copy-mode-p s)
              "copy-mode -e must auto-exit when scrolled to offset 0")))

(test copy-mode-e-no-exit-while-scrolling-up
  "copy-mode -e does NOT exit while scrolling upward (positive delta)."
  (let ((s (%screen-with-scrollback 3)))
    (cl-tmux/commands::copy-mode-enter s :exit-on-bottom t)
    (cl-tmux/commands::copy-mode-scroll s 100)      ; scroll up to oldest
    (is-true (screen-copy-mode-p s)
             "scrolling up must not trigger exit-on-bottom")))

(test copy-mode-scroll-noop-when-not-in-copy-mode
  "Outside copy mode, copy-mode-scroll does nothing: offset stays put and the
   call returns NIL."
  (let ((s (make-screen 20 5)))
    (setf (screen-scrollback s) (list (make-array 0) (make-array 0)))
    (is-false (screen-copy-mode-p s) "precondition: not in copy mode")
    (is (null (cl-tmux/commands::copy-mode-scroll s 100)))
    (is (= 0 (screen-copy-offset s)) "offset untouched when not in copy mode")))

;;; ── kill-pane ─────────────────────────────────────────────────────────────────

(test kill-pane-multi-pane-survivor-relayouts-and-reselects
  "Killing one pane of a multi-pane window keeps the window alive: the survivor
   is removed-from? no — the killed pane is removed, the survivor is reselected
   and the window is relaid out.  Returns NIL (session not quit)."
  (let* ((win (%vsplit-window 20))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win)))
         (sess (%make-session-with-window win)))
    (window-select-pane win p0)            ; kill the active (first) pane
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

;;; ── select-window-by-number ─────────────────────────────────────────────────

(test select-window-by-number-selects-nth-window
  "select-window-by-number activates the Nth (0-based) window."
  (let* ((s  (make-fake-session :nwindows 3))
         (w0 (first  (session-windows s)))
         (w2 (third  (session-windows s))))
    (with-loop-state
      (is (eq w0 (session-active-window s)) "starts on window 0")
      (select-window-by-number s 2)
      (is (eq w2 (session-active-window s)) "index 2 selects the third window")
      (select-window-by-number s 0)
      (is (eq w0 (session-active-window s)) "index 0 selects the first window"))))

(test select-window-by-number-out-of-range-is-noop
  "select-window-by-number with an out-of-range index leaves the active window unchanged."
  (let* ((s      (make-fake-session :nwindows 2))
         (before (session-active-window s)))
    (select-window-by-number s 99)
    (is (eq before (session-active-window s))
        "out-of-range index must not change the active window")))

(test select-window-by-id-stable-after-kill
  "After killing a middle window, select-window-by-number still finds the
   window by its stored id, not by list position."
  (let* ((w0 (make-window :id 0 :name "a" :width 20 :height 5
                          :panes (list (%make-test-pane :id 1))))
         (w1 (make-window :id 1 :name "b" :width 20 :height 5
                          :panes (list (%make-test-pane :id 2))))
         (w2 (make-window :id 2 :name "c" :width 20 :height 5
                          :panes (list (%make-test-pane :id 3))))
         (sess (make-session :id 1 :name "0" :windows (list w0 w1 w2))))
    (session-select-window sess w0)
    ;; Kill the middle window (id=1).
    (kill-window sess w1)
    ;; List is now [w0, w2].  select-window-by-number 2 must still find w2
    ;; (id=2 is at list-position 1 after the kill).
    (select-window-by-number sess 2)
    (is (eq w2 (session-active-window sess))
        "select-window-by-number must find w2 by id=2 even after w1 was killed")))

(test kill-pane-nonactive-does-not-reselect
  "Killing a non-active pane does not change the active pane."
  (let* ((win (%vsplit-window 20))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win)))
         (sess (%make-session-with-window win)))
    ;; Make p0 active, then kill p1 (non-active).
    (window-select-pane win p0)
    (kill-pane sess p1)
    ;; p0 must remain active.
    (is (eq p0 (window-active-pane win))
        "active pane must remain p0 when a non-active pane is killed")))

(test kill-pane-active-selects-mru-pane
  "When the active pane is killed, the last-active pane is selected if present."
  (let* ((win (%vsplit-window 20))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win)))
         (sess (%make-session-with-window win)))
    ;; Visit p1 first, then switch back to p0 so p1 is last-active.
    (window-select-pane win p1)
    (window-select-pane win p0)
    ;; Now last-active should be p1.
    (is (eq p1 (window-last-active win)) "precondition: last-active is p1")
    ;; Kill p0 (active) — should reselect p1 (last-active).
    (kill-pane sess p0)
    (is (eq p1 (window-active-pane win))
        "last-active pane (p1) must be selected after active pane (p0) is killed")))

;;; ── rename-session ──────────────────────────────────────────────────────────

(test rename-session-changes-session-name
  "rename-session sets the session's name to the supplied string."
  (let ((sess (make-session :id 1 :name "old" :windows nil)))
    (cl-tmux/commands:rename-session sess "new")
    (is (string= "new" (session-name sess)) "session name must be updated to \"new\"")))

(test rename-session-ignores-empty-string
  "rename-session with an empty name is a no-op: the session name is unchanged."
  (let ((sess (make-session :id 1 :name "original" :windows nil)))
    (cl-tmux/commands:rename-session sess "")
    (is (string= "original" (session-name sess))
        "empty rename must not change the session name")))

(test rename-session-ignores-nil
  "rename-session with a NIL name is a no-op."
  (let ((sess (make-session :id 1 :name "keep" :windows nil)))
    (cl-tmux/commands:rename-session sess nil)
    (is (string= "keep" (session-name sess))
        "nil rename must not change the session name")))

;;; ── %copy-mode-clamp-cursor direct unit tests ────────────────────────────────

(test copy-mode-clamp-cursor-row-above-viewport
  "%copy-mode-clamp-cursor clamps a cursor row above 0 to row 0."
  (let ((s (make-screen 20 5)))
    (setf (screen-copy-cursor s) (cons -3 2))
    (cl-tmux/commands::%copy-mode-clamp-cursor s)
    (let ((cursor (screen-copy-cursor s)))
      (is (= 0 (car cursor)) "row clamped from -3 to 0")
      (is (= 2 (cdr cursor)) "col unchanged"))))

(test copy-mode-clamp-cursor-row-below-viewport
  "%copy-mode-clamp-cursor clamps a cursor row >= height to height-1."
  (let ((s (make-screen 20 5)))
    (setf (screen-copy-cursor s) (cons 10 3))
    (cl-tmux/commands::%copy-mode-clamp-cursor s)
    (let ((cursor (screen-copy-cursor s)))
      (is (= 4 (car cursor)) "row clamped from 10 to height-1 (4)")
      (is (= 3 (cdr cursor)) "col unchanged"))))

(test copy-mode-clamp-cursor-col-out-of-range
  "%copy-mode-clamp-cursor clamps col to [0, width-1] independently of row."
  (let ((s (make-screen 10 5)))
    (setf (screen-copy-cursor s) (cons 2 25))
    (cl-tmux/commands::%copy-mode-clamp-cursor s)
    (let ((cursor (screen-copy-cursor s)))
      (is (= 2 (car cursor)) "row unchanged")
      (is (= 9 (cdr cursor)) "col clamped from 25 to width-1 (9)"))))

(test copy-mode-clamp-cursor-noop-when-nil
  "%copy-mode-clamp-cursor is a no-op when the cursor is NIL."
  (let ((s (make-screen 20 5)))
    (setf (screen-copy-cursor s) nil)
    (cl-tmux/commands::%copy-mode-clamp-cursor s)
    (is (null (screen-copy-cursor s)) "nil cursor must remain nil")))

(test copy-mode-clamp-cursor-in-bounds-unchanged
  "%copy-mode-clamp-cursor leaves an already-in-bounds cursor unchanged."
  (let ((s (make-screen 20 5)))
    (setf (screen-copy-cursor s) (cons 2 10))
    (cl-tmux/commands::%copy-mode-clamp-cursor s)
    (let ((cursor (screen-copy-cursor s)))
      (is (= 2 (car cursor)) "in-bounds row must not change")
      (is (= 10 (cdr cursor)) "in-bounds col must not change"))))

;;; ── %join-pane-insert-into-dst direct unit tests ─────────────────────────────

(test join-pane-insert-into-dst-returns-src-pane-on-success
  "%join-pane-insert-into-dst returns SRC-PANE when insertion succeeds."
  (let* ((win (%vsplit-window 20))
         (p0  (first  (window-panes win)))
         (src (make-pane :id 99 :x 0 :y 0 :width 10 :height 5
                         :fd -1 :pid -1 :screen (make-screen 10 5))))
    (window-select-pane win p0)
    (is (eq src (cl-tmux/commands::%join-pane-insert-into-dst src win :h))
        "%join-pane-insert-into-dst must return src-pane on success")))

(test join-pane-insert-into-dst-adds-pane-to-window
  "%join-pane-insert-into-dst appends src-pane to the destination window's pane list."
  (let* ((win (%vsplit-window 20))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win)))
         (src (make-pane :id 99 :x 0 :y 0 :width 10 :height 5
                         :fd -1 :pid -1 :screen (make-screen 10 5))))
    (window-select-pane win p0)
    (cl-tmux/commands::%join-pane-insert-into-dst src win :h)
    (is (member src (window-panes win))
        "inserted pane must appear in window-panes after %join-pane-insert-into-dst")))

(test join-pane-insert-into-dst-sets-pane-window
  "%join-pane-insert-into-dst wires pane-window to the destination window."
  (let* ((win (%vsplit-window 20))
         (p0  (first  (window-panes win)))
         (src (make-pane :id 99 :x 0 :y 0 :width 10 :height 5
                         :fd -1 :pid -1 :screen (make-screen 10 5))))
    (window-select-pane win p0)
    (cl-tmux/commands::%join-pane-insert-into-dst src win :h)
    (is (eq win (pane-window src))
        "pane-window must be updated to dst-window after insertion")))

(test join-pane-insert-into-dst-returns-nil-when-no-active-pane
  "%join-pane-insert-into-dst returns NIL when the destination has no active pane."
  (let* ((win (make-window :id 1 :name "empty" :width 20 :height 5
                           :panes nil :tree nil))
         (src (make-pane :id 99 :x 0 :y 0 :width 10 :height 5
                         :fd -1 :pid -1 :screen (make-screen 10 5))))
    (is (null (cl-tmux/commands::%join-pane-insert-into-dst src win :h))
        "%join-pane-insert-into-dst must return NIL when destination has no active leaf")))
