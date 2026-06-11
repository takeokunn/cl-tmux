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
         (s   (make-session :id 1 :name "0" :windows (list win))))
    (session-select-window s win)
    (cl-tmux::%cmd-resize-pane-arg s '("-x" "25"))
    (is (= 25 (pane-width p0)) "resize-pane -x 25 must make the active pane 25 wide")))

(test resize-pane-x-absolute-shrinks-active
  "resize-pane -x N shrinks the active pane when N < current width — verifies the
   signed-delta border move shrinks as well as grows."
  (let* ((win (%vsplit-window 20))
         (p0  (first (window-panes win)))
         (s   (make-session :id 1 :name "0" :windows (list win))))
    (session-select-window s win)
    (cl-tmux::%cmd-resize-pane-arg s '("-x" "15"))
    (is (= 15 (pane-width p0)) "resize-pane -x 15 must shrink the active pane to 15")))

(test resize-pane-y-absolute-sets-height
  "resize-pane -y N sets the active pane to an absolute height of N."
  (let* ((win (%hsplit-window 10))
         (p0  (first (window-panes win)))
         (s   (make-session :id 1 :name "0" :windows (list win))))
    (session-select-window s win)
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
         (sess (make-session :id 1 :name "0" :windows (list win))))
    (session-select-window sess win)
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
         (sess (make-session :id 1 :name "0" :windows (list win))))
    (session-select-window sess win)
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

(test copy-mode-yank-enqueues-osc52-when-set-clipboard-on
  "With set-clipboard on (tmux default), copy-mode-yank enqueues an OSC 52
   sequence on the screen's clipboard-queue so the renderer copies the selection
   to the host system clipboard."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (with-isolated-options ("set-clipboard" "on")
      (let ((s (make-screen 20 5)))
        (feed s "hello")
        (cl-tmux/commands::copy-mode-enter s)
        (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
              (cl-tmux/terminal/types:screen-copy-mark   s) (cons 0 0)
              (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 5))
        (cl-tmux/commands::copy-mode-yank s)
        (let ((q (cl-tmux/terminal/types:screen-clipboard-queue s)))
          (is (= 1 (length q)) "exactly one OSC 52 sequence enqueued")
          (is (search "]52;c;" (first q)) "the sequence is an OSC 52 clipboard set")
          (is (search "aGVsbG8=" (first q)) "encodes the yanked text (base64 of hello)"))))))

(test copy-mode-yank-no-osc52-when-set-clipboard-off
  "With set-clipboard off, copy-mode-yank does NOT enqueue an OSC 52 sequence."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (with-isolated-options ("set-clipboard" "off")
      (let ((s (make-screen 20 5)))
        (feed s "hello")
        (cl-tmux/commands::copy-mode-enter s)
        (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
              (cl-tmux/terminal/types:screen-copy-mark   s) (cons 0 0)
              (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 5))
        (cl-tmux/commands::copy-mode-yank s)
        (is (null (cl-tmux/terminal/types:screen-clipboard-queue s))
            "no OSC 52 enqueued when set-clipboard is off")))))

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

;;; ── copy-mode-other-end ──────────────────────────────────────────────────────

(test copy-mode-other-end-swaps-cursor-and-mark
  "copy-mode-other-end exchanges the cursor and mark ends of the selection."
  (let ((s (%copy-mode-screen)))
    (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
          (cl-tmux/terminal/types:screen-copy-mark      s) (cons 0 2)
          (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 5))
    (cl-tmux/commands::copy-mode-other-end s)
    (is (equal (cons 0 2) (cl-tmux/terminal/types:screen-copy-cursor s))
        "cursor must take the former mark end")
    (is (equal (cons 0 5) (cl-tmux/terminal/types:screen-copy-mark s))
        "mark must take the former cursor end")
    (is-true (cl-tmux/terminal/types:screen-dirty-p s)
             "screen must be dirty after other-end")))

(test copy-mode-other-end-no-op-when-not-selecting
  "copy-mode-other-end is a harmless no-op when no selection is active."
  (let ((s (%copy-mode-screen)))
    ;; No selection: selecting NIL, mark/cursor stay as set by copy-mode-enter.
    (setf (cl-tmux/terminal/types:screen-copy-selecting s) nil
          (cl-tmux/terminal/types:screen-copy-mark      s) nil
          (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 3))
    (finishes (cl-tmux/commands::copy-mode-other-end s))
    (is (null (cl-tmux/terminal/types:screen-copy-mark s))
        "mark must remain NIL when not selecting")
    (is (equal (cons 0 3) (cl-tmux/terminal/types:screen-copy-cursor s))
        "cursor must be unchanged when not selecting")))

(test copy-mode-other-end-no-op-when-mark-nil
  "copy-mode-other-end does not swap (and stays clean) when mark is NIL even
   though selecting is T — guards against a half-initialised selection."
  (let ((s (%copy-mode-screen)))
    (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
          (cl-tmux/terminal/types:screen-copy-mark      s) nil
          (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 4)
          (cl-tmux/terminal/types:screen-dirty-p        s) nil)
    (finishes (cl-tmux/commands::copy-mode-other-end s))
    (is (equal (cons 0 4) (cl-tmux/terminal/types:screen-copy-cursor s))
        "cursor must be unchanged when mark is NIL")
    (is (null (cl-tmux/terminal/types:screen-copy-mark s))
        "mark must remain NIL")
    (is-false (cl-tmux/terminal/types:screen-dirty-p s)
              "screen must not be marked dirty when no swap occurs")))

;;; ── copy-mode-clear-selection (send -X clear-selection) ──────────────────────

(test copy-mode-clear-selection-drops-selection-keeps-cursor
  "copy-mode-clear-selection clears the mark + selection flags but keeps the
   cursor and stays in copy mode (tmux clear-selection / default vi Escape)."
  (let ((s (%copy-mode-screen)))
    (setf (cl-tmux/terminal/types:screen-copy-selecting        s) t
          (cl-tmux/terminal/types:screen-copy-mark             s) (cons 0 2)
          (cl-tmux/terminal/types:screen-copy-cursor           s) (cons 0 5)
          (cl-tmux/terminal/types:screen-copy-rect-select-p    s) t)
    (cl-tmux/commands::copy-mode-clear-selection s)
    (is-false (cl-tmux/terminal/types:screen-copy-selecting s)
              "selection flag must be cleared")
    (is (null (cl-tmux/terminal/types:screen-copy-mark s))
        "mark must be dropped")
    (is-false (cl-tmux/terminal/types:screen-copy-rect-select-p s)
              "rectangle-select flag must be reset")
    (is (equal (cons 0 5) (cl-tmux/terminal/types:screen-copy-cursor s))
        "cursor position must be preserved (stay put in copy mode)")
    (is-true (cl-tmux/terminal/types:screen-copy-mode-p s)
             "must remain in copy mode (clear-selection does not cancel)")
    (is-true (cl-tmux/terminal/types:screen-dirty-p s)
             "screen must be dirty after clearing")))

(test copy-mode-clear-selection-noop-without-selection
  "copy-mode-clear-selection is a clean no-op when there is no selection/mark."
  (let ((s (%copy-mode-screen)))
    (setf (cl-tmux/terminal/types:screen-copy-selecting s) nil
          (cl-tmux/terminal/types:screen-copy-mark      s) nil
          (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 3)
          (cl-tmux/terminal/types:screen-dirty-p        s) nil)
    (finishes (cl-tmux/commands::copy-mode-clear-selection s))
    (is (equal (cons 0 3) (cl-tmux/terminal/types:screen-copy-cursor s))
        "cursor unchanged")
    (is-false (cl-tmux/terminal/types:screen-dirty-p s)
              "no dirty mark when there was nothing to clear")))

(test copy-mode-clear-selection-x-command-mapped
  "The send -X name clear-selection (and its alias stop-selection) map to the
   :copy-mode-clear-selection dispatch keyword."
  (is (eq :copy-mode-clear-selection
          (cdr (assoc "clear-selection" cl-tmux::*copy-mode-x-commands*
                      :test #'string-equal)))
      "clear-selection must be a known send -X command")
  (is (eq :copy-mode-clear-selection
          (cdr (assoc "stop-selection" cl-tmux::*copy-mode-x-commands*
                      :test #'string-equal)))
      "stop-selection must alias clear-selection"))

(test copy-mode-x-line-positions-vs-history-extremes
  "top/middle/bottom-line (vi H/M/L) move within the viewport; history-top/bottom
   (vi g/G) jump to the scrollback extremes — they must map to distinct actions."
  (flet ((x (name) (cdr (assoc name cl-tmux::*copy-mode-x-commands*
                               :test #'string-equal))))
    (is (eq :copy-mode-high   (x "top-line"))    "top-line → high (viewport top)")
    (is (eq :copy-mode-middle (x "middle-line")) "middle-line → middle (was missing)")
    (is (eq :copy-mode-low    (x "bottom-line")) "bottom-line → low (viewport bottom)")
    (is (eq :copy-mode-top    (x "history-top")) "history-top → scrollback top")
    (is (eq :copy-mode-bottom (x "history-bottom")) "history-bottom → scrollback bottom")))

(test copy-mode-high-middle-low-set-viewport-row
  "copy-mode-high/middle/low move the cursor to viewport row 0 / mid / height-1
   without changing the scroll offset."
  (let ((s (%copy-mode-screen)))
    (setf (cl-tmux/terminal/types:screen-copy-offset s) 7
          (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 3))
    (cl-tmux/commands::copy-mode-low s)
    (is (= (1- (screen-height s)) (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "low → last viewport row")
    (cl-tmux/commands::copy-mode-high s)
    (is (= 0 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "high → viewport row 0")
    (cl-tmux/commands::copy-mode-middle s)
    (is (= (floor (screen-height s) 2)
           (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "middle → middle viewport row")
    (is (= 7 (cl-tmux/terminal/types:screen-copy-offset s))
        "scroll offset must be unchanged (H/M/L do not scroll)")
    (is (= 3 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "column must be preserved")))

;;; ── WORD motion: copy-mode-space-{forward,backward,end} (vi W/B/E) ───────────

(test copy-mode-space-motion-is-whitespace-delimited
  "WORD motion (W/B/E) treats punctuation as part of the WORD — only whitespace
   separates — unlike w/b/e which honour word-separators (here '-')."
  (let ((s (%copy-mode-screen :content "foo-bar baz")))
    ;; forward: w stops at 'bar' (col 4, '-' is a separator); W skips to 'baz' (8).
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-word-forward s)
    (is (= 4 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))) "w → start of 'bar'")
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-space-forward s)
    (is (= 8 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "W → start of 'baz' (foo-bar is one WORD)")
    ;; backward from 'baz' (8): b → 'bar' (4); B → start of 'foo-bar' WORD (0).
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 8))
    (cl-tmux/commands::copy-mode-word-backward s)
    (is (= 4 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))) "b → start of 'bar'")
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 8))
    (cl-tmux/commands::copy-mode-space-backward s)
    (is (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "B → start of 'foo-bar' WORD")))

(test copy-mode-space-end-lands-on-word-final-char
  "copy-mode-space-end (vi E) moves to the last char of the current/next WORD."
  (let ((s (%copy-mode-screen :content "foo-bar baz")))
    ;; From col 0, E → last char of 'foo-bar' (col 6, the 'r').
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-space-end s)
    (is (= 6 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "E → last char of the 'foo-bar' WORD")))

(test copy-mode-x-word-vs-space-mappings
  "send -X next-word/etc. map to word motion; next-space/etc. to WORD motion."
  (flet ((x (name) (cdr (assoc name cl-tmux::*copy-mode-x-commands*
                               :test #'string-equal))))
    (is (eq :copy-mode-word-forward  (x "next-word")))
    (is (eq :copy-mode-space-forward (x "next-space")))
    (is (eq :copy-mode-space-backward (x "previous-space")))
    (is (eq :copy-mode-space-end      (x "next-space-end")))))

;;; ── back-to-indentation (vi ^): first non-blank vs line-start (vi 0) ─────────

(test copy-mode-back-to-indentation-stops-at-first-non-blank
  "copy-mode-back-to-indentation (vi ^) moves to the first non-blank column —
   unlike copy-mode-line-start (vi 0), which always goes to column 0."
  (let ((s (%copy-mode-screen :content "   foo")))
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 5))
    (cl-tmux/commands::copy-mode-back-to-indentation s)
    (is (= 3 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "^ must land on the first non-blank char (col 3)")
    ;; line-start still goes to column 0 — the two are distinct.
    (cl-tmux/commands::copy-mode-line-start s)
    (is (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "0 (line-start) must go to column 0")))

(test copy-mode-back-to-indentation-blank-line-goes-to-zero
  "On an all-blank row, ^ falls back to column 0."
  (let ((s (%copy-mode-screen)))            ; default content is blank
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 4))
    (cl-tmux/commands::copy-mode-back-to-indentation s)
    (is (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "an entirely blank row → column 0")))

(test copy-mode-x-back-to-indentation-mapped
  "send -X back-to-indentation maps to the distinct :copy-mode-back-to-indentation
   action, not line-start."
  (is (eq :copy-mode-back-to-indentation
          (cdr (assoc "back-to-indentation" cl-tmux::*copy-mode-x-commands*
                      :test #'string-equal)))))

(test copy-mode-other-end-preserves-selection-text
  "Swapping the two ends must not change the selected text or normalised bounds —
   this is the defining invariant of other-end."
  (let ((s (%copy-mode-screen :content "foo bar baz")))
    (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
          (cl-tmux/terminal/types:screen-copy-mark      s) (cons 0 4)
          (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 6))
    (let ((text-before (cl-tmux/commands::%selection-text s)))
      (multiple-value-bind (sr0 er0 sc0 ec0) (cl-tmux/commands::%selection-bounds s)
        (cl-tmux/commands::copy-mode-other-end s)
        (let ((text-after (cl-tmux/commands::%selection-text s)))
          (multiple-value-bind (sr1 er1 sc1 ec1) (cl-tmux/commands::%selection-bounds s)
            (is (string= text-before text-after)
                "selected text must be identical after other-end")
            (is (and (= sr0 sr1) (= er0 er1) (= sc0 sc1) (= ec0 ec1))
                "normalised selection bounds must be identical after other-end")))))))

(test copy-mode-other-end-double-swap-restores-original
  "Two successive swaps restore the original cursor and mark."
  (let ((s (%copy-mode-screen)))
    (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
          (cl-tmux/terminal/types:screen-copy-mark      s) (cons 0 2)
          (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 5))
    (cl-tmux/commands::copy-mode-other-end s)
    (cl-tmux/commands::copy-mode-other-end s)
    (is (equal (cons 0 5) (cl-tmux/terminal/types:screen-copy-cursor s))
        "cursor must return to its original position after two swaps")
    (is (equal (cons 0 2) (cl-tmux/terminal/types:screen-copy-mark s))
        "mark must return to its original position after two swaps")))

;;; ── copy-mode-select-word ────────────────────────────────────────────────────

(test copy-mode-select-word-selects-word-under-cursor
  "copy-mode-select-word selects exactly the word under the cursor.
   The %selection-text round-trip pins the column off-by-one: for \"bar\" at
   cols 4-6 the mark sits at col 4 and the cursor at col 7 (exclusive end)."
  (let ((s (%copy-mode-screen :content "foo bar baz")))
    ;; "foo bar baz": b=4 a=5 r=6 — put the cursor inside "bar" on row 0.
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 5))
    (cl-tmux/commands::copy-mode-select-word s)
    (is-true (cl-tmux/terminal/types:screen-copy-selecting s)
             "selecting must be T after select-word")
    (is (equal (cons 0 4) (cl-tmux/terminal/types:screen-copy-mark s))
        "mark must sit on the first word character (col 4)")
    (is (equal (cons 0 7) (cl-tmux/terminal/types:screen-copy-cursor s))
        "cursor must sit just past the last word character (col 7)")
    (is (string= "bar" (cl-tmux/commands::%selection-text s))
        "%selection-text must extract exactly the word \"bar\"")))

(test copy-mode-select-word-on-separator-selects-single-cell
  "copy-mode-select-word on a separator (space) selects just the single cell."
  (let ((s (%copy-mode-screen :content "foo bar baz")))
    ;; Column 3 is the space between "foo" and "bar".
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 3))
    (finishes (cl-tmux/commands::copy-mode-select-word s))
    (is-true (cl-tmux/terminal/types:screen-copy-selecting s)
             "selecting must be T after select-word on a separator")
    (is (equal (cons 0 3) (cl-tmux/terminal/types:screen-copy-mark s))
        "mark must sit on the single cell under the cursor")
    (is (equal (cons 0 4) (cl-tmux/terminal/types:screen-copy-cursor s))
        "cursor must sit one column past the single cell")))

(test copy-mode-select-word-at-rightmost-column-keeps-last-char
  "A word ending at the rightmost column must NOT lose its final character: the
   cursor's exclusive end is allowed to reach width.  PINS the rightmost off-by-one."
  ;; Width-3 screen, content \"cat\": c=0 a=1 t=2 (t is at the last column).
  (let ((s (%copy-mode-screen :w 3 :h 3 :content "cat")))
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 1))
    (cl-tmux/commands::copy-mode-select-word s)
    (is (equal (cons 0 0) (cl-tmux/terminal/types:screen-copy-mark s))
        "mark must sit on the first word character (col 0)")
    (is (equal (cons 0 3) (cl-tmux/terminal/types:screen-copy-cursor s))
        "cursor exclusive end must reach width (col 3), not clamp to col 2")
    (is (string= "cat" (cl-tmux/commands::%selection-text s))
        "%selection-text must keep the rightmost-column character: \"cat\"")))

(test copy-mode-select-word-at-start-of-row-clamps-start
  "select-word with the cursor at column 0 leaves the mark at column 0."
  (let ((s (%copy-mode-screen :content "foo bar baz")))
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-select-word s)
    (is (equal (cons 0 0) (cl-tmux/terminal/types:screen-copy-mark s))
        "mark must clamp to column 0 at the start of the row")
    (is (string= "foo" (cl-tmux/commands::%selection-text s))
        "%selection-text must extract \"foo\"")))

(test copy-mode-select-word-stops-at-multi-space-gap
  "select-word must not span a multi-space gap between words."
  ;; \"ab   cd\": a=0 b=1 spaces=2,3,4 c=5 d=6.
  (let ((s (%copy-mode-screen :content "ab   cd")))
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 5))
    (cl-tmux/commands::copy-mode-select-word s)
    (is (equal (cons 0 5) (cl-tmux/terminal/types:screen-copy-mark s))
        "mark must stop at the start of \"cd\" (col 5), not cross the gap")
    (is (string= "cd" (cl-tmux/commands::%selection-text s))
        "%selection-text must extract \"cd\" without spanning the space gap")))

(test copy-mode-select-word-sets-dirty-flag
  "select-word marks the screen dirty."
  (let ((s (%copy-mode-screen :content "foo bar baz")))
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 5)
          (cl-tmux/terminal/types:screen-dirty-p     s) nil)
    (is-false (cl-tmux/terminal/types:screen-dirty-p s)
              "precondition: dirty-p NIL before select-word")
    (cl-tmux/commands::copy-mode-select-word s)
    (is-true (cl-tmux/terminal/types:screen-dirty-p s)
             "dirty-p must be T after select-word")))

(test copy-mode-select-word-no-op-when-not-in-copy-mode
  "select-word is a harmless no-op when copy mode is not active."
  (let ((s (make-screen 20 5)))
    (feed s "foo bar baz")
    ;; Do NOT enter copy mode.
    (finishes (cl-tmux/commands::copy-mode-select-word s))
    (is-false (cl-tmux/terminal/types:screen-copy-selecting s)
              "selecting must remain NIL when not in copy mode")
    (is (null (cl-tmux/terminal/types:screen-copy-mark s))
        "mark must remain NIL when not in copy mode")))

;;; ── copy-mode-move-cursor ────────────────────────────────────────────────────

(defmacro with-copy-mode-cursor ((screen-var row col &key (w 20) (h 5)) &body body)
  "Bind SCREEN-VAR to a fresh W x H copy-mode screen with cursor at (ROW . COL).
   Eliminates the three-step setup repeated across move-cursor tests."
  `(let ((,screen-var (make-screen ,w ,h)))
     (cl-tmux/commands::copy-mode-enter ,screen-var)
     (setf (cl-tmux/terminal/types:screen-copy-cursor ,screen-var) (cons ,row ,col))
     ,@body))

(test copy-mode-move-cursor-left-decrements-col
  "Moving :left decrements the column by 1."
  (with-copy-mode-cursor (s 2 5)
    (cl-tmux/commands::copy-mode-move-cursor s :left)
    (is (equal (cons 2 4) (cl-tmux/terminal/types:screen-copy-cursor s))
        "column must decrease by 1 on :left")
    (is-true (cl-tmux/terminal/types:screen-dirty-p s)
             "screen must be dirty after cursor move")))

(test copy-mode-move-cursor-right-increments-col
  "Moving :right increments the column by 1."
  (with-copy-mode-cursor (s 2 5)
    (cl-tmux/commands::copy-mode-move-cursor s :right)
    (is (equal (cons 2 6) (cl-tmux/terminal/types:screen-copy-cursor s))
        "column must increase by 1 on :right")))

(test copy-mode-move-cursor-up-decrements-row
  "Moving :up decrements the row by 1."
  (with-copy-mode-cursor (s 2 5)
    (cl-tmux/commands::copy-mode-move-cursor s :up)
    (is (equal (cons 1 5) (cl-tmux/terminal/types:screen-copy-cursor s))
        "row must decrease by 1 on :up")))

(test copy-mode-move-cursor-down-increments-row
  "Moving :down increments the row by 1."
  (with-copy-mode-cursor (s 2 5)
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

(test copy-mode-selection-cursor-can-reach-width
  "While selecting, :right may advance the cursor to WIDTH (the exclusive end past
   the last column) so the selection can include the rightmost cell — navigation
   still caps at WIDTH-1 (covered by the test above)."
  (let ((s (make-screen 5 3)))
    (feed s "abcde")
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-begin-selection s)
    (dotimes (i 6) (cl-tmux/commands::copy-mode-move-cursor s :right))
    (is (= 5 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "selecting cursor reaches width (5), got ~D"
        (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
    (is (string= "abcde" (cl-tmux/commands::%selection-text s))
        "selection includes the rightmost column 'e'")))

(test copy-mode-move-cursor-clamps-down-at-screen-edge
  "Moving :down when at the last row stays at (height - 1)."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 4 5))
    (cl-tmux/commands::copy-mode-move-cursor s :down)
    (is (= 4 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "row must clamp at height-1")))

(test copy-mode-enter-places-cursor-at-bottom-left
  "copy-mode-enter initialises the cursor at the bottom-left of the viewport (row height-1, col 0)."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (is (equal (cons 4 0) (cl-tmux/terminal/types:screen-copy-cursor s))
        "cursor must start at (height-1 . 0) — bottom-left of the viewport")))

(test copy-mode-move-cursor-nil-fallback
  "If copy-cursor is manually reset to NIL, move-cursor falls back to (height-1 . 0) before moving."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    ;; Force cursor to NIL to exercise the fallback path inside move-cursor.
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) nil)
    (cl-tmux/commands::copy-mode-move-cursor s :right)
    (is (equal (cons 4 1) (cl-tmux/terminal/types:screen-copy-cursor s))
        "nil cursor falls back to (height-1 . 0) then moves right to (height-1 . 1)")))

(test copy-mode-move-cursor-sets-mark-anchor-when-selecting-and-mark-nil
  "When copy-selecting is T and mark is NIL, the first move sets the mark anchor."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 1 3)
          (cl-tmux/terminal/types:screen-copy-selecting s) t
          (cl-tmux/terminal/types:screen-copy-mark      s) nil)
    (cl-tmux/commands::copy-mode-move-cursor s :right)
    (is-true (cl-tmux/terminal/types:screen-copy-mark s)
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

(test rename-window-empty-string-is-noop
  "rename-window with an empty name leaves the window name unchanged."
  (let ((win (make-window :id 1 :name "original" :width 20 :height 5 :panes nil)))
    (cl-tmux/commands:rename-window win "")
    (is (string= "original" (window-name win))
        "empty-string rename must not change the window name")))

(test rename-window-nil-name-is-noop
  "rename-window with a NIL name leaves the window name unchanged."
  (let ((win (make-window :id 1 :name "keep" :width 20 :height 5 :panes nil)))
    (cl-tmux/commands:rename-window win nil)
    (is (string= "keep" (window-name win))
        "nil rename must not change the window name")))

;;; ── kill-window (direct path) ────────────────────────────────────────────────

(test kill-window-explicit-window-arg-removes-that-window
  "kill-window with an explicit WINDOW removes that specific window even when it
   is not the active one."
  (let* ((p0  (%make-test-pane :id 1))
         (p1  (%make-test-pane :id 2))
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
  (let* ((p0  (%make-test-pane))
         (w1  (make-window :id 1 :name "w" :width 20 :height 5
                           :tree (make-layout-leaf p0) :panes (list p0)))
         (sess (make-session :id 1 :name "0" :windows (list w1))))
    (session-select-window sess w1)
    (is (eq :quit (kill-window sess))
        "killing the sole window must return :quit")
    (is (null (session-windows sess)) "session must have no windows")))

(test kill-window-active-switches-to-remaining
  "Killing the active window of two switches the active pointer to the survivor."
  (let* ((p0  (%make-test-pane :id 1))
         (p1  (%make-test-pane :id 2))
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

(test kill-window-active-reselects-mru-not-nearest
  "End-to-end: killing the active window selects the last-used (MRU) survivor, not
   the numerically-nearest one (tmux session_detach / session_last).  Timestamps
   are preset (session-select-window has 1-second universal-time resolution, so
   live switches would tie); killed=1 with remaining {0,2} is an id-distance tie
   the OLD %nearest-window rule broke toward the higher id (w2)."
  (let* ((p0 (%make-test-pane :id 1))
         (p1 (%make-test-pane :id 2))
         (p2 (%make-test-pane :id 3))
         (w0 (make-window :id 0 :name "a" :width 20 :height 5
                          :tree (make-layout-leaf p0) :panes (list p0)
                          :last-active-time 200))   ; MRU survivor
         (w1 (make-window :id 1 :name "b" :width 20 :height 5
                          :tree (make-layout-leaf p1) :panes (list p1)))
         (w2 (make-window :id 2 :name "c" :width 20 :height 5
                          :tree (make-layout-leaf p2) :panes (list p2)
                          :last-active-time 100))
         (sess (make-session :id 1 :name "0" :windows (list w0 w1 w2))))
    ;; Make w1 active (its timestamp becomes 'now', irrelevant — it is killed).
    (session-select-window sess w1)
    (kill-window sess)
    (is (eq w0 (session-active-window sess))
        "MRU survivor w0 (time 200 > w2's 100) is selected, NOT nearest-tie w2")))

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
                               :else-fn (lambda () (setf else-called t)))
    (is-true else-called "else-fn must be invoked for a non-zero-exit command")))

(test if-shell-nonzero-exit-no-else-fn-is-noop
  "if-shell with a non-zero exit and no ELSE-FN does not signal an error."
  (finishes (cl-tmux/commands:if-shell "false" (lambda () nil))))

(test if-shell-zero-exit-no-then-fn-is-noop
  "if-shell with a zero exit and NIL THEN-FN does not signal an error."
  (finishes (cl-tmux/commands:if-shell "true" nil)))

(test if-shell-timeout-returns-calls-else-fn
  "if-shell with a very short timeout calls ELSE-FN (timeout treated as non-zero exit)."
  (let ((else-called nil))
    (cl-tmux/commands:if-shell "sleep 60"
                               (lambda () nil)
                               :else-fn (lambda () (setf else-called t))
                               :timeout 1/1000)
    (is-true else-called "else-fn must be invoked when if-shell times out")))

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
  (let* ((p0  (%make-test-pane))
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
  (let* ((pane   (%make-test-pane))
         (result (capture-pane pane)))
    (is (stringp result) "capture-pane must return a string")))

(test capture-pane-visible-content-contains-fed-text
  "capture-pane returns the visible screen content including text fed to the pane."
  (let* ((pane   (%make-pane-with-content "ABC"))
         (result (capture-pane pane)))
    (is (stringp result) "capture-pane result must be a string")
    (is-true (search "ABC" result)
        "capture-pane output must contain the fed text \"ABC\" (got ~S)" result)))

(test capture-color-sgr-encodes-cell-colours
  "%capture-color-sgr maps a cell colour value to its SGR fragment."
  (is (string= "31"  (cl-tmux/commands::%capture-color-sgr 1 nil))  "fg standard")
  (is (string= "41"  (cl-tmux/commands::%capture-color-sgr 1 t))    "bg standard")
  (is (string= "94"  (cl-tmux/commands::%capture-color-sgr 12 nil)) "fg bright")
  (is (string= "104" (cl-tmux/commands::%capture-color-sgr 12 t))   "bg bright")
  (is (string= "38;5;200" (cl-tmux/commands::%capture-color-sgr 200 nil)) "fg 256")
  (is (string= "48;5;200" (cl-tmux/commands::%capture-color-sgr 200 t))   "bg 256")
  (is (string= "38;2;255;128;0"
               (cl-tmux/commands::%capture-color-sgr (logior #x1000000 #xff8000) nil))
      "fg true-colour"))

(test capture-cell-sgr-includes-attrs-and-colours
  "%capture-cell-sgr emits reset + attrs + fg + bg."
  (is (string= (format nil "~C[0;31;40m" #\Escape)
               (cl-tmux/commands::%capture-cell-sgr 1 0 0))
      "fg red, bg black, no attrs")
  (is (string= (format nil "~C[0;1;31;40m" #\Escape)
               (cl-tmux/commands::%capture-cell-sgr 1 0 1))
      "bold (attr bit 0) adds SGR 1"))

(test capture-pane-escapes-preserves-colour
  "capture-pane :escapes t keeps SGR colour sequences; plain capture does not."
  (let* ((screen (make-screen 10 2))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 2
                            :fd -1 :pid -1 :screen screen)))
    (feed screen (esc "[31m"))     ; foreground red
    (feed screen "hi")
    (let ((plain   (capture-pane pane))
          (colored (capture-pane pane :escapes t)))
      (is (search "hi" plain) "plain capture contains the text")
      (is (not (find (code-char 27) plain)) "plain capture has no escape bytes")
      (is (search "hi" colored) "colour capture contains the text")
      (is (search "31" colored) "colour capture includes the fg=red SGR (31)")
      (is (search (format nil "~C[0m" #\Escape) colored)
          "colour capture ends each row with a reset"))))

(test capture-pane-visible-only-excludes-scrollback
  "capture-pane without :include-scrollback only dumps visible rows, not scrollback."
  (let* ((screen (make-screen 20 5))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                            :fd -1 :pid -1 :screen screen))
         (sb-row (make-array 20 :initial-element
                             (cl-tmux/terminal/types:make-cell
                              :char #\X :fg 7 :bg 0 :attrs 0 :width 1))))
    (setf (cl-tmux/terminal/types:screen-scrollback screen) (list sb-row))
    (feed screen "visible")
    (let ((result (capture-pane pane)))
      (is-true (search "visible" result)
          "visible content must appear in capture-pane output")
      (is (null (search "XXXXXXXXXXXXXXXXX" result))
          "scrollback content must NOT appear when include-scrollback is nil"))))

(test capture-pane-with-scrollback-prepends-history
  "capture-pane with :include-scrollback T prepends scrollback rows before visible rows."
  (let* ((screen (make-screen 20 5))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                            :fd -1 :pid -1 :screen screen))
         (sb-row (make-array 20 :initial-element
                             (cl-tmux/terminal/types:make-cell
                              :char #\Q :fg 7 :bg 0 :attrs 0 :width 1))))
    (setf (cl-tmux/terminal/types:screen-scrollback screen) (list sb-row))
    (feed screen "visible")
    (let ((result (capture-pane pane :include-scrollback t)))
      (is-true (search "QQ" result)
          "scrollback content must appear when include-scrollback is T")
      (is-true (search "visible" result)
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

(test capture-pane-default-trims-trailing-spaces
  "capture-pane's default (no -J) strips trailing whitespace from each line —
   tmux's default behaviour.  A 'hi' on a 10-wide row captures as just \"hi\"."
  (let* ((screen (make-screen 10 1))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 1
                            :fd -1 :pid -1 :screen screen)))
    (feed screen "hi")
    (is (string= (format nil "hi~%") (capture-pane pane))
        "default capture trims the 8 trailing spaces")))

(test capture-pane-J-preserves-trailing-spaces
  "capture-pane -J (:join t) PRESERVES trailing spaces — the row keeps full width."
  (let* ((screen (make-screen 10 1))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 1
                            :fd -1 :pid -1 :screen screen)))
    (feed screen "hi")
    (is (string= (format nil "hi        ~%") (capture-pane pane :join t))
        "join capture keeps the row padded to its full width of 10")))

(test capture-pane-N-preserves-trailing-spaces
  "capture-pane -N (:preserve-trailing t) keeps trailing spaces like -J — the row
   stays padded to its full width."
  (let* ((screen (make-screen 10 1))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 1
                            :fd -1 :pid -1 :screen screen)))
    (feed screen "hi")
    (is (string= (format nil "hi        ~%") (capture-pane pane :preserve-trailing t))
        "-N keeps the row padded to its full width of 10")))

(test capture-pane-N-preserves-trailing-but-does-not-join
  "capture-pane -N preserves trailing spaces but, unlike -J, does NOT rejoin a
   wrapped line — the distinguishing behaviour between -N and -J."
  (let* ((screen (make-screen 5 3))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 5 :height 3
                            :fd -1 :pid -1 :screen screen)))
    (feed screen "ABCDEFGH")            ; wraps: row0 "ABCDE" → row1 "FGH"
    (let ((preserved (capture-pane pane :preserve-trailing t)))
      (is-true (search "FGH  " preserved)
               "-N keeps the FGH continuation row padded to full width (got ~S)"
               preserved)
      (is (null (search "ABCDEFGH" preserved))
          "-N must NOT join the wrapped line into one logical line (got ~S)"
          preserved))))

(test capture-pane-J-joins-wrapped-lines
  "capture-pane -J rejoins a line that wrapped at the right margin into one
   logical line (no newline at the wrap boundary); default capture keeps them
   on separate lines."
  (let* ((screen (make-screen 5 3))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 5 :height 3
                            :fd -1 :pid -1 :screen screen)))
    (feed screen "ABCDEFGH")          ; wraps: row0 "ABCDE" → row1 "FGH"
    (let ((joined  (capture-pane pane :join t))
          (default (capture-pane pane)))
      (is-true (search "ABCDEFGH" joined)
          "with -J the wrapped line is one logical line ABCDEFGH (got ~S)" joined)
      (is (null (search "ABCDEFGH" default))
          "without -J the wrapped halves stay on separate lines (got ~S)" default))))

(test capture-pane-J-keeps-unwrapped-lines-separate
  "capture-pane -J does NOT join lines that did not wrap (a hard CR+LF break)."
  (let* ((screen (make-screen 10 3))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 3
                            :fd -1 :pid -1 :screen screen)))
    (feed screen (format nil "foo~C~Cbar" #\Return #\Linefeed))
    (let ((joined (capture-pane pane :join t)))
      (is-true (search "foo" joined) "foo present")
      (is-true (search "bar" joined) "bar present")
      (is (null (search "foobar" joined))
          "foo and bar did not wrap — they stay separate, not joined (got ~S)" joined))))

(test shift-line-wrapped-up-moves-flags
  "%shift-line-wrapped-up (scroll-up of the wrap flags): a flag at row Y in the
   region moves to Y-1, mirroring the content shift."
  (let ((s (make-screen 5 4)))
    (cl-tmux/terminal/types:%mark-line-wrapped s 2)        ; row 2 wraps
    (cl-tmux/terminal/types:%shift-line-wrapped-up s 0 3)  ; scroll region rows 0..3
    (is-true  (cl-tmux/terminal/types:%line-wrapped-p s 1)
              "the row-2 wrap flag moved up to row 1")
    (is-false (cl-tmux/terminal/types:%line-wrapped-p s 2)
              "row 2 no longer carries the flag")))

(test line-wrapped-flag-cleared-on-erase
  "Erasing a row clears its wrap flag (erase-region), so a rewritten short line
   does not over-join under -J."
  (let ((s (make-screen 5 3)))
    (cl-tmux/terminal/types:%mark-line-wrapped s 0)
    (is-true (cl-tmux/terminal/types:%line-wrapped-p s 0) "row 0 marked wrapped")
    (cl-tmux/terminal/actions:erase-region s 0 0 4 0)      ; erase row 0
    (is-false (cl-tmux/terminal/types:%line-wrapped-p s 0)
              "erasing row 0 clears its wrap flag")))

(test capture-pane-blank-row-trims-to-empty-line
  "A fully blank row trims to an empty captured line (just the newline) by default,
   but stays full-width under -J."
  (let* ((screen (make-screen 5 1))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 5 :height 1
                            :fd -1 :pid -1 :screen screen)))
    (is (string= (format nil "~%")      (capture-pane pane))
        "blank row trims to an empty line")
    (is (string= (format nil "     ~%") (capture-pane pane :join t))
        "blank row stays 5 spaces wide under -J")))

(test capture-pane-escapes-trims-trailing-by-default
  "capture-pane -e also drops trailing blank cells by default — no trailing-space
   run survives, and no stray reset is emitted for the trimmed region."
  (let* ((screen (make-screen 10 1))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 1
                            :fd -1 :pid -1 :screen screen)))
    (feed screen "hi")
    (let ((result (capture-pane pane :escapes t)))
      (is (null (find #\Space result))
          "escaped default capture has no trailing spaces (got ~S)" result)
      (is-true (search "hi" result) "still contains the fed text")
      (is-true (search (format nil "~C[0m" #\Escape) result)
          "still ends the row with an SGR reset"))))

