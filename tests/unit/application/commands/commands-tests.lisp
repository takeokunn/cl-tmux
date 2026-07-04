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

(test resize-pane-x-absolute-table
  "resize-pane -x N sets the active pane's width to N (both grow and shrink paths)."
  (dolist (row '(("25" 25 "resize-pane -x 25 grows pane from 20 to 25")
                 ("15" 15 "resize-pane -x 15 shrinks pane from 20 to 15")))
    (destructuring-bind (n-str expected desc) row
      (let* ((win (%vsplit-window 20))
             (p0  (first (window-panes win)))
             (s   (%make-session-with-window win)))
        (cl-tmux::%cmd-resize-pane-arg s (list "-x" n-str))
        (is (= expected (pane-width p0)) "~A" desc)))))

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
  (with-fake-session (s :nwindows 3)
    (let ((w0 (first  (session-windows s)))
          (w2 (third  (session-windows s))))
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

(test rename-session-ignores-invalid-names-table
  "rename-session with \"\" or NIL is a no-op: the session name remains unchanged."
  (dolist (row '((""  "empty string is a no-op")
                 (nil "nil is a no-op")))
    (destructuring-bind (new-name desc) row
      (let ((sess (make-session :id 1 :name "keep" :windows nil)))
        (cl-tmux/commands:rename-session sess new-name)
        (is (string= "keep" (session-name sess)) "~A" desc)))))

;;; ── %join-pane-insert-into-dst direct unit tests ─────────────────────────────

(test join-pane-insert-into-dst-state-table
  "%join-pane-insert-into-dst appends src-pane and wires pane-window."
  (with-joined-pane-in-window (win src)
    (check-table
     (list (list (not (null (member src (window-panes win)))) t
                 "inserted pane must appear in window-panes after insertion")
           (list (eq win (pane-window src)) t
                 "pane-window must be updated to dst-window after insertion"))
     :test #'eq)))

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
         ;; Capture original geometry.
         (x0  (pane-x p0)) (y0 (pane-y p0)) (w0 (pane-width p0)) (h0 (pane-height p0))
         (x1  (pane-x p1)) (y1 (pane-y p1)) (w1 (pane-width p1)) (h1 (pane-height p1)))
    (is (eq p0 (swap-two-panes win p0 p1))
        "swap-two-panes returns the first argument (pane-a) on success")
    ;; List order exchanged.
    (is (eq p1 (first  (window-panes win))) "p1 must now be first in panes list")
    (is (eq p0 (second (window-panes win))) "p0 must now be second in panes list")
    ;; Geometry exchanged.
    (is (= x1 (pane-x p0))     "p0 x must be p1's old x after swap")
    (is (= y1 (pane-y p0))     "p0 y must be p1's old y after swap")
    (is (= w1 (pane-width  p0)) "p0 width must be p1's old width after swap")
    (is (= h1 (pane-height p0)) "p0 height must be p1's old height after swap")
    (is (= x0 (pane-x p1))     "p1 x must be p0's old x after swap")
    (is (= y0 (pane-y p1))     "p1 y must be p0's old y after swap")
    (is (= w0 (pane-width  p1)) "p1 width must be p0's old width after swap")
    (is (= h0 (pane-height p1)) "p1 height must be p0's old height after swap")))
