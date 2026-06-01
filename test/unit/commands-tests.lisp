(in-package #:cl-tmux/test)

;;;; Pane resize / copy-mode-scroll / kill-pane command logic (src/commands.lisp).

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
  (let* ((p0  (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                         :fd -1 :pid -1 :screen (make-screen 20 5)))
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

;;; ── copy-mode-scroll ─────────────────────────────────────────────────────────

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
         (sess (make-session :id 1 :name "0" :windows (list win))))
    (session-select-window sess win)
    (window-select-pane win p0)            ; kill the active (first) pane
    (is (null (kill-pane sess)) "surviving pane => not a quit")
    (is (equal (list p1) (window-panes win)) "killed pane removed; survivor kept")
    (is (eq p1 (window-active-pane win)) "survivor becomes the active pane")
    (is (= 1 (length (session-windows sess))) "window still present")))

(test kill-pane-emptied-window-falls-through-to-kill-window
  "Killing the sole pane of the sole window empties the window, which then
   falls through to kill-window; with no windows left the session quits."
  (let* ((p0  (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                         :fd -1 :pid -1 :screen (make-screen 20 5)))
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
  (let* ((p0  (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                         :fd -1 :pid -1 :screen (make-screen 20 5)))
         (w1  (make-window :id 1 :name "a" :width 20 :height 5
                           :tree (make-layout-leaf p0)
                           :panes (list p0)))
         (w2  (make-window :id 2 :name "b" :width 20 :height 5
                           :panes (list (make-pane :id 1 :fd -1 :pid -1
                                                   :screen (make-screen 20 5)))))
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

;;; ── copy-mode-begin-selection and copy-mode-yank ────────────────────────────

(defun %copy-mode-screen (&key (w 20) (h 5) (content ""))
  "Return a copy-mode screen pre-filled with CONTENT (no PTY required)."
  (let ((s (make-screen w h)))
    (unless (string= content "")
      (feed s content))
    (cl-tmux/commands::copy-mode-enter s)
    s))

(test copy-mode-begin-selection-sets-selecting-flag
  "copy-mode-begin-selection sets screen-copy-selecting to T and places mark at cursor."
  (let ((s (%copy-mode-screen)))
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 5))
    (cl-tmux/commands::copy-mode-begin-selection s)
    (is-true  (cl-tmux/terminal/types:screen-copy-selecting s)
              "copy-selecting must be T after begin-selection")
    (is (equal (cons 2 5) (cl-tmux/terminal/types:screen-copy-mark s))
        "mark must be placed at the cursor position on begin-selection")))

(test copy-mode-begin-selection-noop-outside-copy-mode
  "copy-mode-begin-selection is a no-op when copy mode is not active."
  (let ((s (make-screen 20 5)))
    ;; Do NOT enter copy mode — screen-copy-mode-p is NIL.
    (cl-tmux/commands::copy-mode-begin-selection s)
    (is-false (cl-tmux/terminal/types:screen-copy-selecting s)
              "selecting must remain NIL when not in copy mode")))

(test copy-mode-yank-pushes-text-to-paste-buffers
  "copy-mode-yank copies the selected region to *paste-buffers* and exits copy mode."
  ;; Use a small screen so we can predict cell content precisely.
  ;; Feed "hello" to row 0; mark at col 0 row 0, cursor at col 4 row 0.
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "hello")
      (cl-tmux/commands::copy-mode-enter s)
      ;; mark at col 0, cursor at col 5 (exclusive end), both on row 0
      ;; → the copy loop runs col from 0 below 5 → "hello"
      (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
            (cl-tmux/terminal/types:screen-copy-mark   s) (cons 0 0)
            (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 5))
      (cl-tmux/commands::copy-mode-yank s)
      ;; Copy mode must be deactivated after yank.
      (is-false (screen-copy-mode-p s) "copy mode must exit after yank")
      ;; Selection must be cleared.
      (is-false (cl-tmux/terminal/types:screen-copy-selecting s)
                "copy-selecting must be NIL after yank")
      ;; Text must have landed in *paste-buffers*.
      (is (= 1 (length cl-tmux/buffer:*paste-buffers*))
          "exactly one paste buffer entry must be present after yank")
      (let ((yanked (cl-tmux/buffer:get-paste-buffer 0)))
        (is (string= "hello" yanked)
            "yanked text must equal the selected content \"hello\" (got ~S)" yanked)))))

(test copy-mode-yank-noop-when-no-selection
  "copy-mode-yank with no active selection does not push to *paste-buffers*."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (%copy-mode-screen :content "data")))
      ;; Ensure no selection is active.
      (setf (cl-tmux/terminal/types:screen-copy-selecting s) nil)
      (cl-tmux/commands::copy-mode-yank s)
      (is (null cl-tmux/buffer:*paste-buffers*)
          "paste buffers must remain empty when no selection was active"))))

(test copy-mode-cancel-selection-clears-all-state
  "copy-mode-cancel-selection resets mark, cursor, and selecting flag."
  (let ((s (%copy-mode-screen)))
    (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
          (cl-tmux/terminal/types:screen-copy-mark      s) (cons 1 2)
          (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 1 5))
    (cl-tmux/commands::copy-mode-cancel-selection s)
    (is (null (cl-tmux/terminal/types:screen-copy-mark s))
        "copy-mark must be NIL after cancel")
    (is (null (cl-tmux/terminal/types:screen-copy-cursor s))
        "copy-cursor must be NIL after cancel")
    (is-false (cl-tmux/terminal/types:screen-copy-selecting s)
              "copy-selecting must be NIL after cancel")))

;;; ── copy-mode-move-cursor ────────────────────────────────────────────────────

(defun %copy-mode-screen-20x5 ()
  "10×5 copy-mode screen with cursor pre-placed at (2 . 5)."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 5))
    s))

(test copy-mode-move-cursor-left-decrements-col
  "Moving :left decrements the column by 1."
  (let ((s (%copy-mode-screen-20x5)))
    (cl-tmux/commands::copy-mode-move-cursor s :left)
    (is (equal (cons 2 4) (cl-tmux/terminal/types:screen-copy-cursor s))
        "column must decrease by 1 on :left")
    (is-true (cl-tmux/terminal/types:screen-dirty-p s)
             "screen must be dirty after cursor move")))

(test copy-mode-move-cursor-right-increments-col
  "Moving :right increments the column by 1."
  (let ((s (%copy-mode-screen-20x5)))
    (cl-tmux/commands::copy-mode-move-cursor s :right)
    (is (equal (cons 2 6) (cl-tmux/terminal/types:screen-copy-cursor s))
        "column must increase by 1 on :right")))

(test copy-mode-move-cursor-up-decrements-row
  "Moving :up decrements the row by 1."
  (let ((s (%copy-mode-screen-20x5)))
    (cl-tmux/commands::copy-mode-move-cursor s :up)
    (is (equal (cons 1 5) (cl-tmux/terminal/types:screen-copy-cursor s))
        "row must decrease by 1 on :up")))

(test copy-mode-move-cursor-down-increments-row
  "Moving :down increments the row by 1."
  (let ((s (%copy-mode-screen-20x5)))
    (cl-tmux/commands::copy-mode-move-cursor s :down)
    (is (equal (cons 3 5) (cl-tmux/terminal/types:screen-copy-cursor s))
        "row must increase by 1 on :down")))

(test copy-mode-move-cursor-clamps-left-at-zero
  "Moving :left when already at column 0 stays at column 0."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 0))
    (cl-tmux/commands::copy-mode-move-cursor s :left)
    (is (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "column must not go below 0")))

(test copy-mode-move-cursor-clamps-up-at-zero
  "Moving :up when already at row 0 stays at row 0."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 5))
    (cl-tmux/commands::copy-mode-move-cursor s :up)
    (is (= 0 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "row must not go below 0")))

(test copy-mode-move-cursor-clamps-right-at-screen-edge
  "Moving :right when at the last column stays at (width - 1)."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 19))
    (cl-tmux/commands::copy-mode-move-cursor s :right)
    (is (= 19 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "column must clamp at width-1")))

(test copy-mode-move-cursor-clamps-down-at-screen-edge
  "Moving :down when at the last row stays at (height - 1)."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 4 5))
    (cl-tmux/commands::copy-mode-move-cursor s :down)
    (is (= 4 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "row must clamp at height-1")))

(test copy-mode-move-cursor-initialises-nil-cursor
  "When copy-cursor is NIL, the cursor is initialised to (0 . 0) before moving."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    ;; cursor starts as NIL — verify the initialisation path
    (is (null (cl-tmux/terminal/types:screen-copy-cursor s)) "precondition: cursor is nil")
    (cl-tmux/commands::copy-mode-move-cursor s :right)
    (is (equal (cons 0 1) (cl-tmux/terminal/types:screen-copy-cursor s))
        "nil cursor must be treated as (0 . 0) before the move")))

(test copy-mode-move-cursor-sets-mark-anchor-when-selecting-and-mark-nil
  "When copy-selecting is T and mark is NIL, the first move sets the mark anchor."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 1 3)
          (cl-tmux/terminal/types:screen-copy-selecting s) t
          (cl-tmux/terminal/types:screen-copy-mark      s) nil)
    (cl-tmux/commands::copy-mode-move-cursor s :right)
    (is (not (null (cl-tmux/terminal/types:screen-copy-mark s)))
        "mark must be placed when copy-selecting is T and mark was nil")))

(test copy-mode-move-cursor-noop-outside-copy-mode
  "copy-mode-move-cursor does nothing when copy mode is not active."
  (let ((s (make-screen 20 5)))
    ;; do NOT enter copy mode
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 5))
    (cl-tmux/commands::copy-mode-move-cursor s :left)
    (is (equal (cons 2 5) (cl-tmux/terminal/types:screen-copy-cursor s))
        "cursor must be unchanged outside copy mode")))

;;; ── rename-window ────────────────────────────────────────────────────────────

(test rename-window-sets-name
  "rename-window sets the window name to the supplied string."
  (let ((win (make-window :id 1 :name "old" :width 20 :height 5 :panes nil)))
    (cl-tmux/commands:rename-window win "new")
    (is (string= "new" (window-name win))
        "window name must be updated to \"new\"")))

(test rename-window-nil-window-is-noop
  "rename-window with NIL window does not signal an error."
  (finishes (cl-tmux/commands:rename-window nil "irrelevant")))

;;; ── kill-window (direct path) ────────────────────────────────────────────────

(test kill-window-explicit-window-arg-removes-that-window
  "kill-window with an explicit WINDOW removes that specific window even when it
   is not the active one."
  (let* ((p0  (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                         :fd -1 :pid -1 :screen (make-screen 20 5)))
         (p1  (make-pane :id 2 :x 0 :y 0 :width 20 :height 5
                         :fd -1 :pid -1 :screen (make-screen 20 5)))
         (w1  (make-window :id 1 :name "a" :width 20 :height 5
                           :tree (make-layout-leaf p0) :panes (list p0)))
         (w2  (make-window :id 2 :name "b" :width 20 :height 5
                           :tree (make-layout-leaf p1) :panes (list p1)))
         (sess (make-session :id 1 :name "0" :windows (list w1 w2))))
    (session-select-window sess w1)          ; active = w1
    ;; Kill the non-active window w2 explicitly.
    (is (null (kill-window sess w2))
        "killing a non-active window must return NIL (session survives)")
    (is (equal (list w1) (session-windows sess))
        "only w2 must be removed from the session")
    (is (eq w1 (session-active-window sess))
        "active window must remain w1 when the killed window was not active")))

(test kill-window-last-window-returns-quit
  "Destroying the sole window of a session returns :quit."
  (let* ((p0  (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                         :fd -1 :pid -1 :screen (make-screen 20 5)))
         (w1  (make-window :id 1 :name "w" :width 20 :height 5
                           :tree (make-layout-leaf p0) :panes (list p0)))
         (sess (make-session :id 1 :name "0" :windows (list w1))))
    (session-select-window sess w1)
    (is (eq :quit (kill-window sess))
        "killing the sole window must return :quit")
    (is (null (session-windows sess)) "session must have no windows")))

(test kill-window-active-switches-to-remaining
  "Killing the active window of two switches the active pointer to the survivor."
  (let* ((p0  (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                         :fd -1 :pid -1 :screen (make-screen 20 5)))
         (p1  (make-pane :id 2 :x 0 :y 0 :width 20 :height 5
                         :fd -1 :pid -1 :screen (make-screen 20 5)))
         (w1  (make-window :id 1 :name "a" :width 20 :height 5
                           :tree (make-layout-leaf p0) :panes (list p0)))
         (w2  (make-window :id 2 :name "b" :width 20 :height 5
                           :tree (make-layout-leaf p1) :panes (list p1)))
         (sess (make-session :id 1 :name "0" :windows (list w1 w2))))
    (session-select-window sess w1)
    (is (null (kill-window sess))
        "session with a remaining window must not quit")
    (is (eq w2 (session-active-window sess))
        "active window must switch to the survivor after killing the active one")))

;;; ── run-shell ────────────────────────────────────────────────────────────────
;;;
;;; Tests use /bin/true (always exits 0) and /bin/echo (prints output) which are
;;; universally available on POSIX systems.  Background mode is verified via the
;;; T return value without inspecting the process object.

(test run-shell-foreground-captures-stdout
  "run-shell (background nil) returns a string containing the command's output."
  (let ((out (cl-tmux/commands:run-shell "echo hello")))
    (is (stringp out) "return value must be a string")
    (is (search "hello" out) "output must contain the echoed word")))

(test run-shell-background-returns-t
  "run-shell :background T returns T immediately without waiting."
  (let ((result (cl-tmux/commands:run-shell "true" :background t)))
    (is (eq t result) "background run must return T")))

(test run-shell-foreground-empty-command-returns-string
  "run-shell with a no-op command returns an empty or whitespace-only string."
  (let ((out (cl-tmux/commands:run-shell "true")))
    (is (stringp out) "return value must be a string even for a no-op command")))

;;; ── if-shell ─────────────────────────────────────────────────────────────────

(test if-shell-zero-exit-calls-then-fn
  "if-shell calls THEN-FN when the command exits with code 0."
  (let ((called nil))
    (cl-tmux/commands:if-shell "true" (lambda () (setf called t)))
    (is-true called "then-fn must be invoked for a zero-exit command")))

(test if-shell-nonzero-exit-calls-else-fn
  "if-shell calls ELSE-FN when the command exits non-zero."
  (let ((else-called nil))
    (cl-tmux/commands:if-shell "false"
                               (lambda () nil)
                               (lambda () (setf else-called t)))
    (is-true else-called "else-fn must be invoked for a non-zero-exit command")))

(test if-shell-nonzero-exit-no-else-fn-is-noop
  "if-shell with a non-zero exit and no ELSE-FN does not signal an error."
  (finishes (cl-tmux/commands:if-shell "false" (lambda () nil))))

(test if-shell-zero-exit-no-then-fn-is-noop
  "if-shell with a zero exit and NIL THEN-FN does not signal an error."
  (finishes (cl-tmux/commands:if-shell "true" nil)))

;;; ── %selection-text ──────────────────────────────────────────────────────────
;;;
;;; %selection-text is a private helper in cl-tmux/commands that extracts the
;;; selected text from a copy-mode screen.  It returns NIL when no selection is
;;; active, a string for a single-row selection, and a newline-joined string for
;;; a multi-row selection.

(defun %make-selecting-screen (content mark cursor &key (w 20) (h 5))
  "Return a copy-mode screen pre-filled with CONTENT, with mark and cursor set
   to MARK and CURSOR respectively, and copy-selecting T."
  (let ((s (make-screen w h)))
    (unless (string= content "")
      (feed s content))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
          (cl-tmux/terminal/types:screen-copy-mark      s) mark
          (cl-tmux/terminal/types:screen-copy-cursor    s) cursor)
    s))

(test selection-text-returns-nil-when-no-selection
  "%selection-text returns NIL when copy-selecting is NIL (no active selection)."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-selecting s) nil)
    (is (null (cl-tmux/commands::%selection-text s))
        "%selection-text must return NIL when no selection is active")))

(test selection-text-returns-nil-when-mark-nil
  "%selection-text returns NIL when copy-selecting is T but mark is NIL."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
          (cl-tmux/terminal/types:screen-copy-mark      s) nil
          (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 5))
    (is (null (cl-tmux/commands::%selection-text s))
        "%selection-text must return NIL when mark is NIL")))

(test selection-text-single-row-returns-correct-text
  "%selection-text returns the correct string for a single-row selection."
  (let ((s (%make-selecting-screen "hello world"
                                   (cons 0 0)    ; mark: row 0, col 0
                                   (cons 0 5)))) ; cursor: row 0, col 5
    (let ((text (cl-tmux/commands::%selection-text s)))
      (is (stringp text) "%selection-text must return a string for a valid selection")
      (is (string= "hello" text)
          "%selection-text must return \"hello\" for cols 0-4 of row 0 (got ~S)" text))))

(test selection-text-multi-row-returns-newline-joined-text
  "%selection-text returns newline-joined text for a multi-row selection."
  ;; Feed two rows: row 0 = "abc", then CR+LF, row 1 = "def".
  (let ((s (make-screen 20 5)))
    (feed s "abc")
    (feed s (format nil "~C~C" #\Return #\Linefeed))
    (feed s "def")
    (cl-tmux/commands::copy-mode-enter s)
    ;; Select from row 0 col 0 to row 1 col 3.
    (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
          (cl-tmux/terminal/types:screen-copy-mark      s) (cons 0 0)
          (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 1 3))
    (let ((text (cl-tmux/commands::%selection-text s)))
      (is (stringp text) "result must be a string")
      (is (find #\Newline text) "multi-row result must contain a newline")
      ;; Row 0 contributes cols 0..2 = "abc"; row 1 contributes cols 0..2 = "def".
      (is (string= (format nil "abc~%def") text)
          "%selection-text must be \"abc\\ndef\" for rows 0-1 (got ~S)" text))))

(test selection-text-reversed-mark-cursor-order
  "%selection-text normalises selection when cursor is before mark."
  ;; mark at col 5, cursor at col 0: result should still be cols 0-4.
  (let ((s (%make-selecting-screen "hello world"
                                   (cons 0 5)    ; mark: row 0, col 5
                                   (cons 0 0)))) ; cursor: row 0, col 0
    (let ((text (cl-tmux/commands::%selection-text s)))
      (is (stringp text) "%selection-text must return a string even when mark > cursor")
      (is (string= "hello" text)
          "%selection-text must normalise reversed mark/cursor (got ~S)" text))))

;;; ── swap-pane ────────────────────────────────────────────────────────────────

(test swap-pane-right-cycles-panes-forward
  "swap-pane :right on a two-pane window moves the active pane to index 1 and
   swaps the positions (pane-x) of the two panes."
  (let* ((win (%vsplit-window 20))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win)))
         (x0-before (pane-x p0))
         (x1-before (pane-x p1)))
    ;; p0 is active (index 0); swap :right -> p0 moves to index 1
    (let ((result (swap-pane win :right)))
      (is (eq p0 result)
          "swap-pane must return the active pane")
      (is (eq p1 (first  (window-panes win)))
          "after :right swap, the former neighbour occupies index 0")
      (is (eq p0 (second (window-panes win)))
          "after :right swap, the active pane occupies index 1")
      ;; Geometry must be exchanged
      (is (= x1-before (pane-x p0))
          "active pane x must equal former neighbour's x after swap")
      (is (= x0-before (pane-x p1))
          "former neighbour x must equal active pane's former x after swap"))))

(test swap-pane-left-cycles-panes-backward
  "swap-pane :left wraps the active pane modularly backward in the panes list."
  (let* ((win (%vsplit-window 20))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win))))
    ;; p0 is active at index 0; :left -> mod(-1, 2) = 1 -> swaps with p1
    (swap-pane win :left)
    (is (eq p1 (first  (window-panes win)))
        "after :left wrap from index 0, neighbour at (mod -1 n) occupies index 0")
    (is (eq p0 (second (window-panes win)))
        "active pane wraps to index 1 on :left from index 0")))

(test swap-pane-right-from-last-wraps-to-first
  "swap-pane :right from the last-index pane wraps modularly to index 0."
  (let* ((win (%vsplit-window 20))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win))))
    ;; Make p1 (index 1, last) the active pane, then swap :right
    (window-select-pane win p1)
    (swap-pane win :right)
    ;; mod(1+1, 2) = 0 => p1 and p0 swap
    (is (eq p0 (second (window-panes win)))
        "p0 moves to index 1 after :right wrap from index 1")
    (is (eq p1 (first  (window-panes win)))
        "p1 wraps to index 0 after :right from the last slot")))

(test swap-pane-single-pane-returns-nil
  "swap-pane on a window with exactly one pane returns NIL (no neighbour to swap)."
  (let* ((p0  (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                         :fd -1 :pid -1 :screen (make-screen 20 5)))
         (win (make-window :id 1 :name "w" :width 20 :height 5
                           :tree (make-layout-leaf p0)
                           :panes (list p0))))
    (window-select-pane win p0)
    (is (null (swap-pane win :right))
        "swap-pane on a single-pane window must return NIL")))

(test swap-pane-geometry-exchanged
  "The x/y/width/height of both panes are exchanged by swap-pane."
  (let* ((win (%vsplit-window 20))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win)))
         (p0-x (pane-x p0)) (p0-y (pane-y p0))
         (p0-w (pane-width p0)) (p0-h (pane-height p0))
         (p1-x (pane-x p1)) (p1-y (pane-y p1))
         (p1-w (pane-width p1)) (p1-h (pane-height p1)))
    (swap-pane win :right)
    (is (= p1-x (pane-x p0)) "active pane x must be former neighbour x")
    (is (= p1-y (pane-y p0)) "active pane y must be former neighbour y")
    (is (= p1-w (pane-width  p0)) "active pane width must be former neighbour width")
    (is (= p1-h (pane-height p0)) "active pane height must be former neighbour height")
    (is (= p0-x (pane-x p1)) "former neighbour x must be original active pane x")
    (is (= p0-y (pane-y p1)) "former neighbour y must be original active pane y")
    (is (= p0-w (pane-width  p1)) "former neighbour width must be original active width")
    (is (= p0-h (pane-height p1)) "former neighbour height must be original active height")))

;;; ── capture-pane ─────────────────────────────────────────────────────────────

(defun %make-pane-with-content (content &key (w 20) (h 5))
  "Build a no-PTY pane whose screen has been fed CONTENT."
  (let* ((screen (make-screen w h))
         (pane   (make-pane :id 1 :x 0 :y 0 :width w :height h
                            :fd -1 :pid -1 :screen screen)))
    (unless (string= content "")
      (feed screen content))
    pane))

(test capture-pane-returns-string
  "capture-pane always returns a string (even on an empty pane)."
  (let* ((pane (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                          :fd -1 :pid -1 :screen (make-screen 20 5)))
         (result (capture-pane pane)))
    (is (stringp result) "capture-pane must return a string")))

(test capture-pane-visible-content-contains-fed-text
  "capture-pane returns the visible screen content including text fed to the pane."
  (let* ((pane   (%make-pane-with-content "ABC"))
         (result (capture-pane pane)))
    (is (stringp result) "capture-pane result must be a string")
    (is (not (null (search "ABC" result)))
        "capture-pane output must contain the fed text \"ABC\" (got ~S)" result)))

(test capture-pane-visible-only-excludes-scrollback
  "capture-pane without :include-scrollback only dumps visible rows, not scrollback."
  (let* ((screen (make-screen 20 5))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                            :fd -1 :pid -1 :screen screen)))
    ;; Manually inject a distinguishable scrollback row
    (let ((sb-row (make-array 20 :initial-element
                              (cl-tmux/terminal/types:make-cell
                               :char #\X :fg 7 :bg 0 :attrs 0 :width 1))))
      (setf (cl-tmux/terminal/types:screen-scrollback screen) (list sb-row)))
    (feed screen "visible")
    (let ((result (capture-pane pane)))
      (is (not (null (search "visible" result)))
          "visible content must appear in capture-pane output")
      (is (null (search "XXXXXXXXXXXXXXXXX" result))
          "scrollback content must NOT appear when include-scrollback is nil"))))

(test capture-pane-with-scrollback-prepends-history
  "capture-pane with :include-scrollback T prepends scrollback rows before visible rows."
  (let* ((screen (make-screen 20 5))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                            :fd -1 :pid -1 :screen screen)))
    ;; Manually inject a scrollback row containing 'Q' characters
    (let ((sb-row (make-array 20 :initial-element
                              (cl-tmux/terminal/types:make-cell
                               :char #\Q :fg 7 :bg 0 :attrs 0 :width 1))))
      (setf (cl-tmux/terminal/types:screen-scrollback screen) (list sb-row)))
    (feed screen "visible")
    (let ((result (capture-pane pane :include-scrollback t)))
      (is (not (null (search "QQ" result)))
          "scrollback content must appear when include-scrollback is T")
      (is (not (null (search "visible" result)))
          "visible content must also appear when include-scrollback is T")
      ;; Scrollback should come before visible content in the output
      (let ((q-pos       (search "QQ"      result))
            (visible-pos (search "visible" result)))
        (is (< q-pos visible-pos)
            "scrollback rows must precede visible rows in the output")))))

(test capture-pane-height-rows-newlines
  "capture-pane emits exactly (screen-height) newline-terminated rows."
  (let* ((pane   (%make-pane-with-content "" :w 10 :h 3))
         (result (capture-pane pane))
         (lines  (count #\Newline result)))
    (is (= 3 lines)
        "capture-pane must emit exactly height (~D) newline characters (got ~D)"
        3 lines)))
