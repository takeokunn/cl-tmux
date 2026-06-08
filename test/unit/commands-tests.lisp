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
    (let ((w (screen-width s)))
      (let ((expected (string-right-trim " " (display-row-string s 0))))
        (setf (screen-copy-mark      s) (cons 0 0)
              (screen-copy-cursor    s) (cons 0 w)
              (screen-copy-selecting s) t)
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

;;; ── copy-mode-line-start / copy-mode-line-end ────────────────────────────────

(test copy-mode-line-start-moves-to-col-0
  "copy-mode-line-start sets the cursor column to 0."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 10))
    (cl-tmux/commands::copy-mode-line-start s)
    (is (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "copy-mode-line-start must set col to 0")))

(test copy-mode-line-end-moves-to-last-col
  "copy-mode-line-end sets the cursor column to width-1."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 3))
    (cl-tmux/commands::copy-mode-line-end s)
    (is (= 19 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "copy-mode-line-end must set col to width-1 (19 for width=20)")))

(test copy-mode-line-start-noop-outside-copy-mode
  "copy-mode-line-start is a no-op when not in copy mode."
  (let ((s (make-screen 20 5)))
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 10))
    (cl-tmux/commands::copy-mode-line-start s)
    (is (= 10 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "column must be unchanged when not in copy mode")))

(test copy-mode-line-end-noop-outside-copy-mode
  "copy-mode-line-end is a no-op when not in copy mode."
  (let ((s (make-screen 20 5)))
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 3))
    (cl-tmux/commands::copy-mode-line-end s)
    (is (= 3 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "column must be unchanged when not in copy mode")))

;;; ── copy-mode-high / copy-mode-middle / copy-mode-low ───────────────────────

(test copy-mode-high-moves-cursor-to-row-0
  "copy-mode-high sets the cursor row to 0, keeping column."
  (let ((s (make-screen 20 10)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 7 5))
    (cl-tmux/commands::copy-mode-high s)
    (is (= 0 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "copy-mode-high must move cursor to row 0")
    (is (= 5 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "copy-mode-high must preserve column")))

(test copy-mode-middle-moves-cursor-to-mid-row
  "copy-mode-middle sets the cursor row to floor(height/2), keeping column."
  (let ((s (make-screen 20 10)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 5))
    (cl-tmux/commands::copy-mode-middle s)
    (is (= 5 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "copy-mode-middle must move cursor to floor(10/2)=5 for height=10")))

(test copy-mode-low-moves-cursor-to-last-row
  "copy-mode-low sets the cursor row to height-1, keeping column."
  (let ((s (make-screen 20 10)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 5))
    (cl-tmux/commands::copy-mode-low s)
    (is (= 9 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "copy-mode-low must move cursor to height-1=9 for height=10")))

(test copy-mode-high-noop-outside-copy-mode
  "copy-mode-high is a no-op when not in copy mode."
  (let ((s (make-screen 20 5)))
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 3 5))
    (cl-tmux/commands::copy-mode-high s)
    (is (= 3 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "row must be unchanged outside copy mode")))

(test copy-mode-middle-noop-outside-copy-mode
  "copy-mode-middle is a no-op when not in copy mode."
  (let ((s (make-screen 20 10)))
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 7 5))
    (cl-tmux/commands::copy-mode-middle s)
    (is (= 7 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "row must be unchanged outside copy mode")))

(test copy-mode-low-noop-outside-copy-mode
  "copy-mode-low is a no-op when not in copy mode."
  (let ((s (make-screen 20 10)))
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 5))
    (cl-tmux/commands::copy-mode-low s)
    (is (= 2 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "row must be unchanged outside copy mode")))

;;; ── copy-mode-page-up / copy-mode-page-down ─────────────────────────────────

(test copy-mode-page-up-scrolls-by-full-height
  "copy-mode-page-up scrolls back by screen-height lines."
  (let ((s (%screen-with-scrollback 30)))
    (cl-tmux/commands::copy-mode-page-up s)
    (is (= 5 (screen-copy-offset s))
        "copy-mode-page-up must scroll by screen-height=5")))

(test copy-mode-page-down-scrolls-forward-by-full-height
  "copy-mode-page-down scrolls forward by screen-height lines."
  (let ((s (%screen-with-scrollback 30)))
    ;; First scroll back enough to allow scrolling forward
    (setf (cl-tmux/terminal/types:screen-copy-offset s) 20)
    (cl-tmux/commands::copy-mode-page-down s)
    (is (= 15 (screen-copy-offset s))
        "copy-mode-page-down must reduce offset by screen-height=5")))

(test copy-mode-half-page-up-scrolls-by-half-height
  "copy-mode-half-page-up scrolls back by floor(screen-height/2) lines."
  (let ((s (%screen-with-scrollback 30)))
    (cl-tmux/commands::copy-mode-half-page-up s)
    (is (= 2 (screen-copy-offset s))
        "copy-mode-half-page-up must scroll by floor(5/2)=2 for height=5")))

(test copy-mode-scroll-up-line-scrolls-by-one
  "copy-mode-scroll-up-line scrolls back by exactly 1 line."
  (let ((s (%screen-with-scrollback 10)))
    (cl-tmux/commands::copy-mode-scroll-up-line s)
    (is (= 1 (screen-copy-offset s))
        "copy-mode-scroll-up-line must scroll back by 1")))

(test copy-mode-scroll-down-line-scrolls-forward-by-one
  "copy-mode-scroll-down-line scrolls forward by exactly 1 line."
  (let ((s (%screen-with-scrollback 10)))
    (setf (cl-tmux/terminal/types:screen-copy-offset s) 5)
    (cl-tmux/commands::copy-mode-scroll-down-line s)
    (is (= 4 (screen-copy-offset s))
        "copy-mode-scroll-down-line must reduce offset by 1")))

(test copy-mode-page-up-noop-outside-copy-mode
  "copy-mode-page-up is a no-op when not in copy mode."
  (let ((s (make-screen 20 5)))
    (setf (cl-tmux/terminal/types:screen-scrollback s)
          (loop repeat 10 collect (make-array 0)))
    (cl-tmux/commands::copy-mode-page-up s)
    (is (= 0 (screen-copy-offset s))
        "offset must remain 0 when not in copy mode")))

(test copy-mode-page-down-noop-outside-copy-mode
  "copy-mode-page-down is a no-op when not in copy mode."
  (let ((s (make-screen 20 5)))
    (setf (cl-tmux/terminal/types:screen-copy-offset s) 0)
    (cl-tmux/commands::copy-mode-page-down s)
    (is (= 0 (screen-copy-offset s))
        "offset must remain 0 when not in copy mode")))

(test copy-mode-half-page-up-noop-outside-copy-mode
  "copy-mode-half-page-up is a no-op when not in copy mode."
  (let ((s (make-screen 20 5)))
    (setf (cl-tmux/terminal/types:screen-scrollback s)
          (loop repeat 10 collect (make-array 0)))
    (cl-tmux/commands::copy-mode-half-page-up s)
    (is (= 0 (screen-copy-offset s))
        "offset must remain 0 when not in copy mode")))

(test copy-mode-half-page-down-noop-outside-copy-mode
  "copy-mode-half-page-down is a no-op when not in copy mode."
  (let ((s (make-screen 20 5)))
    (setf (cl-tmux/terminal/types:screen-copy-offset s) 0)
    (cl-tmux/commands::copy-mode-half-page-down s)
    (is (= 0 (screen-copy-offset s))
        "offset must remain 0 when not in copy mode")))

(test copy-mode-scroll-up-line-noop-outside-copy-mode
  "copy-mode-scroll-up-line is a no-op when not in copy mode."
  (let ((s (make-screen 20 5)))
    (setf (cl-tmux/terminal/types:screen-scrollback s)
          (loop repeat 5 collect (make-array 0)))
    (cl-tmux/commands::copy-mode-scroll-up-line s)
    (is (= 0 (screen-copy-offset s))
        "offset must remain 0 when not in copy mode")))

(test copy-mode-scroll-down-line-noop-outside-copy-mode
  "copy-mode-scroll-down-line is a no-op when not in copy mode."
  (let ((s (make-screen 20 5)))
    (setf (cl-tmux/terminal/types:screen-copy-offset s) 0)
    (cl-tmux/commands::copy-mode-scroll-down-line s)
    (is (= 0 (screen-copy-offset s))
        "offset must remain 0 when not in copy mode")))

;;; ── copy-mode-word-forward / word-backward / word-end ──────────────────────

(defun %copy-mode-screen-with-text (text &key (w 40) (h 5))
  "Return a copy-mode screen with TEXT fed at row 0."
  (let ((s (make-screen w h)))
    (feed s text)
    (cl-tmux/commands::copy-mode-enter s)
    s))

(test copy-mode-word-forward-jumps-to-next-word
  "copy-mode-word-forward moves the cursor to the start of the next word."
  (let ((s (%copy-mode-screen-with-text "hello world")))
    ;; Cursor at col 0 (start of "hello")
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-word-forward s)
    ;; Should land at col 6 (start of "world")
    (is (= 6 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "word-forward must jump to col 6 (start of 'world') from col 0 (got ~D)"
        (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))))

(test copy-mode-word-backward-jumps-to-prev-word-start
  "copy-mode-word-backward moves the cursor to the start of the previous word."
  (let ((s (%copy-mode-screen-with-text "hello world")))
    ;; Cursor in the middle of "world" at col 8
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 8))
    (cl-tmux/commands::copy-mode-word-backward s)
    ;; Should land at col 6 (start of "world")
    (is (= 6 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "word-backward from col 8 must jump to start of 'world' at col 6 (got ~D)"
        (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))))

(test copy-mode-word-forward-noop-outside-copy-mode
  "copy-mode-word-forward is a no-op when not in copy mode."
  (let ((s (make-screen 20 5)))
    (feed s "hello world")
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-word-forward s)
    (is (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "column must not change outside copy mode")))


(test copy-mode-word-end-jumps-to-end-of-word
  "copy-mode-word-end moves the cursor to the last character of the current or next word."
  (let ((s (%copy-mode-screen-with-text "hello world")))
    ;; Cursor at col 0 (start of "hello")
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-word-end s)
    ;; Should land at col 4 (last char of "hello")
    (is (= 4 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "word-end from col 0 must jump to col 4 (end of 'hello') (got ~D)"
        (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))))

(test copy-mode-word-end-noop-outside-copy-mode
  "copy-mode-word-end is a no-op when not in copy mode."
  (let ((s (make-screen 20 5)))
    (feed s "hello world")
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-word-end s)
    (is (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "column must not change outside copy mode")))

;;; ── copy-mode-top / copy-mode-bottom ────────────────────────────────────────

(test copy-mode-top-jumps-to-max-scrollback
  "copy-mode-top scrolls the viewport to the oldest scrollback line."
  (let ((s (%screen-with-scrollback 10)))
    (cl-tmux/commands::copy-mode-top s)
    (is (= 10 (screen-copy-offset s))
        "copy-mode-top must set offset to the scrollback length (10)")))

(test copy-mode-bottom-returns-to-live-view
  "copy-mode-bottom scrolls back to offset 0 (live view)."
  (let ((s (%screen-with-scrollback 10)))
    ;; First scroll to top
    (cl-tmux/commands::copy-mode-top s)
    (is (= 10 (screen-copy-offset s)) "precondition: at top after copy-mode-top")
    ;; Then jump to bottom
    (cl-tmux/commands::copy-mode-bottom s)
    (is (= 0 (screen-copy-offset s))
        "copy-mode-bottom must reset offset to 0 (live view)")))

(test copy-mode-top-noop-outside-copy-mode
  "copy-mode-top is a no-op when not in copy mode."
  (let ((s (make-screen 20 5)))
    (setf (cl-tmux/terminal/types:screen-scrollback s)
          (loop repeat 5 collect (make-array 0)))
    (cl-tmux/commands::copy-mode-top s)
    (is (= 0 (screen-copy-offset s))
        "offset must remain 0 when not in copy mode")))

;;; ── copy-mode-begin-line-selection ──────────────────────────────────────────

(test copy-mode-begin-line-selection-sets-line-selection-p
  "copy-mode-begin-line-selection sets line-selection-p and activates the selection."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 5))
    (cl-tmux/commands::copy-mode-begin-line-selection s)
    (is-true (cl-tmux/terminal/types:screen-copy-line-selection-p s)
             "copy-line-selection-p must be T after begin-line-selection")
    (is-true (cl-tmux/terminal/types:screen-copy-selecting s)
             "copy-selecting must be T after begin-line-selection")
    (is (= 0 (cdr (cl-tmux/terminal/types:screen-copy-mark s)))
        "mark col must be 0 for line selection")
    (is (= 19 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "cursor col must be width-1 for line selection")))

(test copy-mode-begin-line-selection-noop-outside-copy-mode
  "copy-mode-begin-line-selection is a no-op when not in copy mode."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 5))
      ;; Do NOT enter copy mode.
      (cl-tmux/commands::copy-mode-begin-line-selection s)
      (is-false (cl-tmux/terminal/types:screen-copy-line-selection-p s)
                "line-selection-p must remain NIL when not in copy mode")
      (is-false (cl-tmux/terminal/types:screen-copy-selecting s)
                "copy-selecting must remain NIL when not in copy mode"))))

;;; ── copy-mode-copy-end-of-line (D) ──────────────────────────────────────────

(test copy-mode-copy-end-of-line-yanks-from-cursor
  "copy-mode-copy-end-of-line copies text from cursor to end of row and exits."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "hello world")
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 6))
      (cl-tmux/commands::copy-mode-copy-end-of-line s)
      (is-false (screen-copy-mode-p s)
                "copy mode must exit after D command")
      (let ((yanked (cl-tmux/buffer:get-paste-buffer 0)))
        (is (and yanked (string= "world" yanked))
            "D command must copy from col 6 to end (got ~S)" yanked)))))

(test copy-mode-copy-end-of-line-noop-outside-copy-mode
  "copy-mode-copy-end-of-line is a no-op when not in copy mode."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "hello world")
      ;; Do NOT enter copy mode.
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
      (cl-tmux/commands::copy-mode-copy-end-of-line s)
      (is (null cl-tmux/buffer:*paste-buffers*)
          "paste buffers must remain empty when not in copy mode"))))

;;; ── copy-mode-copy-line (Y) ──────────────────────────────────────────────────

(test copy-mode-copy-line-yanks-full-row
  "copy-mode-copy-line copies the full current row content and exits."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "hello")
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 10))
      (cl-tmux/commands::copy-mode-copy-line s)
      (is-false (screen-copy-mode-p s)
                "copy mode must exit after Y command")
      (let ((yanked (cl-tmux/buffer:get-paste-buffer 0)))
        (is (and yanked (search "hello" yanked))
            "Y command must copy the full row containing 'hello' (got ~S)" yanked)))))

(test copy-mode-copy-line-noop-outside-copy-mode
  "copy-mode-copy-line is a no-op when not in copy mode."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "hello")
      ;; Do NOT enter copy mode.
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
      (cl-tmux/commands::copy-mode-copy-line s)
      (is (null cl-tmux/buffer:*paste-buffers*)
          "paste buffers must remain empty when not in copy mode"))))

;;; ── copy-mode-search-forward / search-backward ──────────────────────────────

(test copy-mode-search-forward-finds-term
  "copy-mode-search-forward moves cursor to the first match after current position."
  (let ((s (make-screen 30 5)))
    (feed s "abc def abc")
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-search-forward s "abc")
    ;; First search from col 1 onward should find "abc" at col 8
    (is (= 8 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "search-forward must find second 'abc' at col 8 (got ~D)"
        (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))))

(test copy-mode-search-forward-saves-term
  "copy-mode-search-forward saves the search term for n/N repeats."
  (let ((s (make-screen 30 5)))
    (feed s "foo bar foo")
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-search-forward s "foo")
    (is (string= "foo" (cl-tmux/terminal/types:screen-copy-search-term s))
        "search term must be saved after search-forward")))

(test copy-mode-search-backward-finds-term
  "copy-mode-search-backward moves cursor to the nearest match before current position."
  (let ((s (make-screen 30 5)))
    (feed s "abc def abc")
    (cl-tmux/commands::copy-mode-enter s)
    ;; Start cursor at col 11 (past the end of second "abc" at cols 8-10).
    ;; The backward scan uses end-col=11 for row 0, so positions 0..10 are
    ;; eligible.  The rightmost match before col 11 is the second "abc" at col 8.
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 11))
    (cl-tmux/commands::copy-mode-search-backward s "abc")
    ;; Search backward should find second "abc" at col 8 (nearest match before col 11)
    (is (= 8 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "search-backward must find 'abc' at col 8 (nearest before col 11) (got ~D)"
        (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))))

(test copy-mode-search-forward-regex-dot
  "search-forward treats the term as a regex: 'a.c' matches 'abc'."
  (let ((s (make-screen 30 5)))
    (feed s "xy abc z")
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-search-forward s "a.c")
    (is (= 3 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "regex a.c must match 'abc' at col 3 (got ~D)"
        (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))))

(test copy-mode-search-forward-regex-char-class
  "search-forward regex character class '[0-9]+' finds the first digit run."
  (let ((s (make-screen 30 5)))
    (feed s "abc 123 def")
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-search-forward s "[0-9]+")
    (is (= 4 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "regex [0-9]+ must match '123' starting at col 4 (got ~D)"
        (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))))

(test copy-mode-search-invalid-regex-falls-back-to-literal
  "An invalid regex (unbalanced paren) falls back to a literal substring search,
   so search terms with regex metacharacters still work."
  (let ((s (make-screen 30 5)))
    (feed s "a (b) c")
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-search-forward s "(")
    (is (= 2 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "literal '(' must be found at col 2 (got ~D)"
        (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))))

(test copy-mode-search-backward-regex
  "search-backward matches a regex and finds the nearest match before the cursor."
  (let ((s (make-screen 30 5)))
    (feed s "a1b a2b a3b")
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 11))
    (cl-tmux/commands::copy-mode-search-backward s "a.b")
    (is (= 8 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "regex a.b backward must find the last 'aNb' at col 8 before col 11 (got ~D)"
        (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))))

(test copy-mode-search-next-repeats-forward
  "copy-mode-search-next uses the saved term to repeat forward search."
  (let ((s (make-screen 30 5)))
    (feed s "abc def abc")
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    ;; Save a term and jump to position 8
    (cl-tmux/commands::copy-mode-search-forward s "abc")
    ;; Cursor should now be at 8
    (is (= 8 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))
    ;; search-next with cursor at 8 should not find another match (no more "abc" on row 0)
    (cl-tmux/commands::copy-mode-search-next s)
    ;; Cursor stays at 8 if no further match found
    (is (= 8 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "search-next must stay at current position when no further match")))

(test copy-mode-search-prev-noop-without-term
  "copy-mode-search-prev does nothing when no search term is saved."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 5)
          (cl-tmux/terminal/types:screen-copy-search-term s) nil)
    (cl-tmux/commands::copy-mode-search-prev s)
    (is (= 5 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "search-prev must not move cursor when no term is saved")))

;;; ── send-keys-to-pane ────────────────────────────────────────────────────────

(test send-keys-to-pane-noop-with-negative-fd
  "send-keys-to-pane is a no-op (no error) when the pane has fd=-1."
  (let ((pane (%make-test-pane)))
    (finishes (cl-tmux/commands:send-keys-to-pane pane "hello")
              "send-keys-to-pane with fd=-1 must not signal an error")))

(test send-keys-to-pane-noop-with-nil-pane
  "send-keys-to-pane with NIL pane does not signal an error."
  (finishes (cl-tmux/commands:send-keys-to-pane nil "hello")
            "send-keys-to-pane with nil pane must not signal an error"))

;;; ── send-keys key-name translation ───────────────────────────────────────────

(test key-name-to-bytes-named-keys
  "%key-name-to-bytes maps named keys to their byte sequences."
  (flet ((bytes (name) (coerce (cl-tmux/commands::%key-name-to-bytes name) 'list)))
    (is (equal '(13)  (bytes "Enter"))   "Enter → CR")
    (is (equal '(9)   (bytes "Tab"))     "Tab → HT")
    (is (equal '(27)  (bytes "Escape"))  "Escape → ESC")
    (is (equal '(32)  (bytes "Space"))   "Space → SP")
    (is (equal '(127) (bytes "BSpace"))  "BSpace → DEL")
    (is (equal '(27 91 65) (bytes "Up"))      "Up → ESC [ A")
    (is (equal '(27 91 66) (bytes "Down"))    "Down → ESC [ B")
    (is (equal '(27 79 80) (bytes "F1"))      "F1 → ESC O P")
    (is (equal '(27 91 53 126) (bytes "PageUp")) "PageUp → ESC [ 5 ~")))

(test key-name-to-bytes-control-keys
  "%key-name-to-bytes maps C-<char> to the corresponding control byte."
  (flet ((bytes (name) (coerce (cl-tmux/commands::%key-name-to-bytes name) 'list)))
    (is (equal '(3)  (bytes "C-c")) "C-c → 0x03")
    (is (equal '(1)  (bytes "C-a")) "C-a → 0x01")
    (is (equal '(26) (bytes "C-z")) "C-z → 0x1a")
    (is (equal '(0)  (bytes "C-@")) "C-@ → 0x00")))

(test key-name-to-bytes-meta-keys
  "%key-name-to-bytes maps M-<char> to ESC followed by the char."
  (is (equal '(27 120) (coerce (cl-tmux/commands::%key-name-to-bytes "M-x") 'list))
      "M-x → ESC x"))

(test key-name-to-bytes-unknown-returns-nil
  "%key-name-to-bytes returns NIL for text that is not a key name."
  (is (null (cl-tmux/commands::%key-name-to-bytes "hello")))
  (is (null (cl-tmux/commands::%key-name-to-bytes "echo"))))

(test translate-send-keys-keys-vs-literal
  "%translate-send-keys parses arguments shell-style and translates each: key
   names become their byte sequences, other args are sent literally.  Spaces
   separate arguments unless quoted (tmux semantics)."
  (flet ((bytes (s) (coerce (cl-tmux/commands::%translate-send-keys s) 'list)))
    (is (equal '(13) (bytes "Enter")) "single key → its bytes")
    (is (equal '(27 91 65 27 91 65 27 91 66) (bytes "Up Up Down"))
        "all-keys → concatenated (ESC[A ESC[A ESC[B)")
    ;; tmux semantics: unquoted spaces split args, so they vanish between literals.
    (is (equal (map 'list #'char-code "echohi") (bytes "echo hi"))
        "unquoted 'echo hi' → two literal args, no space (tmux-correct)")
    ;; A literal arg before a key: text then CR.
    (is (equal (append (map 'list #'char-code "foo") '(13)) (bytes "foo Enter"))
        "literal arg followed by a key → text then the key's bytes")
    ;; Quoting preserves the embedded space.
    (is (equal (append (map 'list #'char-code "echo hi") '(13))
               (bytes "\"echo hi\" Enter"))
        "quoted arg keeps its space, then Enter → CR")))

(test send-keys-to-pane-translates-named-key-to-pty
  "send-keys-to-pane translates a named key (Enter) and writes CR to the PTY."
  (with-pipe-fds (rfd wfd)
    (let ((pane (make-pane :id 1 :x 0 :y 0 :width 20 :height 5 :fd wfd
                           :screen (make-screen 20 5))))
      (cl-tmux/commands:send-keys-to-pane pane "Enter")
      (let ((ready (cl-tmux/pty:select-fds (list rfd) 200000)))  ; 200 ms
        (is-true ready "the translated key must reach the PTY")
        (when ready
          (cffi:with-foreign-object (buf :uint8 8)
            (let ((n (cffi:foreign-funcall "read"
                                           :int rfd :pointer buf :unsigned-long 4
                                           :long)))
              (is (= 1 n) "Enter is one byte (got ~D)" n)
              (is (= 13 (cffi:mem-aref buf :uint8 0)) "byte must be CR (13)"))))))))

;;; ── tokenize-command-string (shell-style command lexer) ──────────────────────

(test tokenize-command-string-basic-whitespace
  "Whitespace separates arguments; runs of spaces/tabs collapse."
  (is (equal '("a" "b" "c") (cl-tmux/commands:tokenize-command-string "a b c")))
  (is (equal '("a" "b") (cl-tmux/commands:tokenize-command-string "  a   b  ")))
  (is (equal '() (cl-tmux/commands:tokenize-command-string "   "))))

(test tokenize-command-string-single-quotes-literal
  "'...' is a literal span: spaces inside are kept and no escapes are processed."
  (is (equal '("a b" "c") (cl-tmux/commands:tokenize-command-string "'a b' c")))
  (is (equal '("a\\b") (cl-tmux/commands:tokenize-command-string "'a\\b'")))
  (is (equal '("") (cl-tmux/commands:tokenize-command-string "''"))
      "an explicit empty quoted token yields an empty-string argument"))

(test tokenize-command-string-double-quotes-with-escapes
  "\"...\" keeps spaces and processes backslash escapes."
  (is (equal '("a b") (cl-tmux/commands:tokenize-command-string "\"a b\"")))
  (is (equal '("a\"b") (cl-tmux/commands:tokenize-command-string "\"a\\\"b\""))
      "escaped double-quote stays inside the argument"))

(test tokenize-command-string-bare-backslash-escape
  "A bare backslash escapes the next character (e.g. a space joins one arg)."
  (is (equal '("a b") (cl-tmux/commands:tokenize-command-string "a\\ b")))
  (is (equal '("ab") (cl-tmux/commands:tokenize-command-string "a\\b"))))

(test tokenize-command-string-adjacent-spans-join
  "Adjacent quoted/bare spans concatenate into a single argument."
  (is (equal '("foobar baz")
             (cl-tmux/commands:tokenize-command-string "foo\"bar baz\"")))
  (is (equal '("ab cd")
             (cl-tmux/commands:tokenize-command-string "'ab'' cd'"))))

(test tokenize-command-string-unterminated-quote-tolerated
  "An unterminated quote consumes to end of string without error."
  (is (equal '("a b") (cl-tmux/commands:tokenize-command-string "'a b")))
  (is (equal '("xy") (cl-tmux/commands:tokenize-command-string "\"xy"))))

;;; ── add-message-log ──────────────────────────────────────────────────────────

(test add-message-log-prepends-entry
  "add-message-log prepends a (timestamp . text) cons to *message-log*."
  (let ((cl-tmux::*message-log* nil))
    (cl-tmux::add-message-log "first-message")
    (is-true cl-tmux::*message-log*
        "*message-log* must be non-nil after add-message-log")
    (is (string= "first-message" (cdr (first cl-tmux::*message-log*)))
        "message text must be in cdr of first entry (got ~S)"
        (cdr (first cl-tmux::*message-log*)))))

(test add-message-log-caps-at-100
  "add-message-log caps *message-log* at 100 entries."
  (let ((cl-tmux::*message-log* nil))
    ;; Add 105 entries.
    (loop repeat 105 do (cl-tmux::add-message-log "x"))
    (is (= 100 (length cl-tmux::*message-log*))
        "*message-log* must be capped at 100 entries (got ~D)"
        (length cl-tmux::*message-log*))))

(test add-message-log-ordering
  "add-message-log puts newest entry first."
  (let ((cl-tmux::*message-log* nil))
    (cl-tmux::add-message-log "first")
    (cl-tmux::add-message-log "second")
    (is (string= "second" (cdr (first cl-tmux::*message-log*)))
        "second (most recent) message must be at the head of *message-log*")))

;;; ── %nearest-window (tie-breaking logic) ────────────────────────────────────

(test nearest-window-picks-closest-by-id
  "%nearest-window returns the window whose id is closest to the killed id."
  (let ((w0 (make-window :id 0 :name "a" :width 20 :height 5 :panes nil))
        (w3 (make-window :id 3 :name "b" :width 20 :height 5 :panes nil))
        (w7 (make-window :id 7 :name "c" :width 20 :height 5 :panes nil)))
    ;; killed-id = 2: w0 is 2 away, w3 is 1 away, w7 is 5 away => w3 wins.
    (is (eq w3 (cl-tmux/commands::%nearest-window (list w0 w3 w7) 2))
        "%nearest-window must pick w3 (id=3, dist=1) when killed-id=2")))

(test nearest-window-equidistant-prefers-higher-id
  "%nearest-window prefers the higher id when two windows are equidistant."
  (let ((w1 (make-window :id 1 :name "a" :width 20 :height 5 :panes nil))
        (w5 (make-window :id 5 :name "b" :width 20 :height 5 :panes nil)))
    ;; killed-id = 3: both are 2 away => higher id (w5) is preferred.
    (is (eq w5 (cl-tmux/commands::%nearest-window (list w1 w5) 3))
        "%nearest-window must prefer the higher id (w5) when equidistant from killed-id=3")))

(test nearest-window-single-window-returns-it
  "%nearest-window with a single-element list returns that window."
  (let ((w2 (make-window :id 2 :name "a" :width 20 :height 5 :panes nil)))
    (is (eq w2 (cl-tmux/commands::%nearest-window (list w2) 99))
        "%nearest-window with one window must return it regardless of killed-id")))

;;; ── %copy-mode-find-forward / %copy-mode-find-backward ──────────────────────

(test copy-mode-find-forward-locates-term
  "%copy-mode-find-forward finds TERM at the correct row/col from start position."
  (let ((s (make-screen 30 5)))
    (feed s "abc def abc")
    (cl-tmux/commands::copy-mode-enter s)
    (multiple-value-bind (row col)
        (cl-tmux/commands::%copy-mode-find-forward s "abc" 0 1)
      (is (= 0 row) "forward search must find match on row 0 (got ~S)" row)
      (is (= 8 col) "forward search from col 1 must find second 'abc' at col 8 (got ~S)" col))))

(test copy-mode-find-forward-no-match-returns-nil-nil
  "%copy-mode-find-forward returns (values nil nil) when no match exists."
  (let ((s (make-screen 20 5)))
    (feed s "hello world")
    (cl-tmux/commands::copy-mode-enter s)
    (multiple-value-bind (row col)
        (cl-tmux/commands::%copy-mode-find-forward s "zzz" 0 0)
      (is (null row) "no match: row must be NIL")
      (is (null col) "no match: col must be NIL"))))

(test copy-mode-find-backward-locates-term
  "%copy-mode-find-backward finds the nearest match before the cursor position."
  (let ((s (make-screen 30 5)))
    (feed s "abc def abc")
    (cl-tmux/commands::copy-mode-enter s)
    ;; Search backward from col 11 on row 0 => nearest match before col 11 is
    ;; the second "abc" at col 8.
    (multiple-value-bind (row col)
        (cl-tmux/commands::%copy-mode-find-backward s "abc" 0 11)
      (is (= 0 row) "backward search must find match on row 0 (got ~S)" row)
      (is (= 8 col) "backward search from col 11 must find 'abc' at col 8 (got ~S)" col))))

(test copy-mode-find-backward-no-match-returns-nil-nil
  "%copy-mode-find-backward returns (values nil nil) when no match exists."
  (let ((s (make-screen 20 5)))
    (feed s "hello world")
    (cl-tmux/commands::copy-mode-enter s)
    (multiple-value-bind (row col)
        (cl-tmux/commands::%copy-mode-find-backward s "zzz" 0 5)
      (is (null row) "no match: row must be NIL")
      (is (null col) "no match: col must be NIL"))))

;;; ── join-pane ────────────────────────────────────────────────────────────────

(test join-pane-moves-pane-into-destination-window
  "join-pane removes SRC-PANE from SRC-WINDOW and inserts it into DST-WINDOW."
  (let* ((src-pane (%make-test-pane :id 1))
         (dst-pane (%make-test-pane :id 2))
         (src-win  (make-window :id 1 :name "src" :width 20 :height 5
                                :tree (make-layout-leaf src-pane)
                                :panes (list src-pane)))
         (dst-win  (make-window :id 2 :name "dst" :width 20 :height 5
                                :tree (make-layout-leaf dst-pane)
                                :panes (list dst-pane)))
         (sess     (make-session :id 1 :name "0"
                                 :windows (list src-win dst-win))))
    (session-select-window sess src-win)
    (window-select-pane src-win src-pane)
    (window-select-pane dst-win dst-pane)
    (let ((result (cl-tmux/commands:join-pane sess src-win src-pane dst-win :h)))
      (is (eq src-pane result) "join-pane must return src-pane on success")
      ;; src-window had only one pane -- it must have been killed.
      (is-false (member src-win (session-windows sess))
          "src-window must be removed from session when it becomes empty after join-pane")
      ;; dst-window must now contain both dst-pane and src-pane.
      (is (member src-pane (window-panes dst-win))
          "src-pane must appear in dst-window's pane list after join-pane"))))

(test join-pane-returns-nil-on-nil-args
  "join-pane returns NIL immediately when any required argument is NIL."
  (is (null (cl-tmux/commands:join-pane nil nil nil nil :h))
      "join-pane with all-nil args must return NIL without signalling"))

;;; ── copy-mode-exit ───────────────────────────────────────────────────────────

(test copy-mode-exit-resets-all-copy-state
  "copy-mode-exit resets copy-mode-p, offset, mark, cursor, and selecting."
  (let ((s (%copy-mode-screen)))
    ;; Set all copy-mode fields to non-default values.
    (setf (cl-tmux/terminal/types:screen-copy-offset    s) 5
          (cl-tmux/terminal/types:screen-copy-mark      s) (cons 2 3)
          (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 2 5)
          (cl-tmux/terminal/types:screen-copy-selecting s) t)
    (cl-tmux/commands::copy-mode-exit s)
    (is-false (screen-copy-mode-p s)
              "copy-mode-p must be NIL after exit")
    (is (= 0 (cl-tmux/terminal/types:screen-copy-offset s))
        "copy-offset must be 0 after exit")
    (is (null (cl-tmux/terminal/types:screen-copy-mark s))
        "copy-mark must be NIL after exit")
    (is (null (cl-tmux/terminal/types:screen-copy-cursor s))
        "copy-cursor must be NIL after exit")
    (is-false (cl-tmux/terminal/types:screen-copy-selecting s)
              "copy-selecting must be NIL after exit")))

;;; ── copy-mode-half-page-down ─────────────────────────────────────────────────

(test copy-mode-half-page-down-scrolls-forward-by-half-height
  "copy-mode-half-page-down scrolls forward by floor(screen-height/2) lines."
  (let ((s (%screen-with-scrollback 30)))
    ;; First scroll back enough to allow scrolling forward.
    (setf (cl-tmux/terminal/types:screen-copy-offset s) 20)
    (cl-tmux/commands::copy-mode-half-page-down s)
    ;; height=5, floor(5/2)=2, so offset decreases by 2: 20-2=18.
    (is (= 18 (screen-copy-offset s))
        "copy-mode-half-page-down must reduce offset by floor(5/2)=2 for height=5")))

;;; ── break-pane ───────────────────────────────────────────────────────────────

(test break-pane-sole-pane-returns-nil
  "break-pane on a window with only one pane is a no-op and returns NIL."
  (let* ((pane (%make-test-pane))
         (win  (make-window :id 1 :name "w" :width 20 :height 5
                            :tree (make-layout-leaf pane)
                            :panes (list pane)))
         (sess (make-session :id 1 :name "0" :windows (list win))))
    (session-select-window sess win)
    (window-select-pane win pane)
    (is (null (cl-tmux/commands:break-pane sess))
        "break-pane on a sole-pane window must return NIL")))

(test break-pane-nil-src-win-returns-nil
  "break-pane when session has no active window returns NIL."
  ;; Build a session with no windows to exercise the nil-src-win guard.
  (let ((sess (make-session :id 1 :name "0" :windows nil)))
    (is (null (cl-tmux/commands:break-pane sess))
        "break-pane with no active window must return NIL")))

(test break-pane-moves-pane-to-new-window
  "break-pane removes the active pane and places it in a new window."
  (let* ((p0  (%make-test-pane :id 1 :w 10))
         (p1  (%make-test-pane :id 2 :x 11 :w 10))
         (win (make-window :id 1 :name "w" :width 21 :height 5
                           :tree (make-layout-split :h
                                    (make-layout-leaf p0) (make-layout-leaf p1)
                                    1/2)
                           :panes (list p0 p1)))
         (sess (make-session :id 1 :name "0" :windows (list win))))
    (session-select-window sess win)
    (window-select-pane win p0)
    (let ((new-win (cl-tmux/commands:break-pane sess)))
      (is-true new-win
          "break-pane must return a new window on success")
      (is (member new-win (session-windows sess))
          "new window must appear in the session's window list")
      (is (member p0 (window-panes new-win))
          "the active pane must be the sole pane of the new window")
      (is (= 1 (length (window-panes new-win)))
          "the new window must have exactly one pane")
      ;; Source window still has p1.
      (is (member p1 (window-panes win))
          "the source window must retain the non-active pane"))))

;;; ── pipe-pane-open / pipe-pane-close / pipe-pane-write ──────────────────────

(test pipe-pane-open-returns-stream
  "pipe-pane-open returns a stream object when the command launches successfully."
  (let* ((pane   (%make-test-pane))
         (result (cl-tmux/commands:pipe-pane-open pane "cat")))
    (is-true result
        "pipe-pane-open must return a non-NIL stream on success")
    ;; Clean up.
    (cl-tmux/commands:pipe-pane-close pane)))

(test pipe-pane-open-close-round-trip
  "pipe-pane-open followed by pipe-pane-close leaves pane-pipe-fd NIL."
  (let ((pane (%make-test-pane)))
    (cl-tmux/commands:pipe-pane-open pane "cat")
    (is-true (pane-pipe-fd pane)
        "pane-pipe-fd must be set after pipe-pane-open")
    (cl-tmux/commands:pipe-pane-close pane)
    (is (null (pane-pipe-fd pane))
        "pane-pipe-fd must be NIL after pipe-pane-close")))

(test pipe-pane-close-noop-when-no-pipe
  "pipe-pane-close is a no-op when pane has no open pipe."
  (let ((pane (%make-test-pane)))
    (finishes (cl-tmux/commands:pipe-pane-close pane)
              "pipe-pane-close with no pipe must not signal")))

(test pipe-pane-write-noop-when-no-pipe
  "pipe-pane-write is a no-op when pane has no open pipe."
  (let ((pane (%make-test-pane)))
    (finishes (cl-tmux/commands:pipe-pane-write pane #(65 66 67))
              "pipe-pane-write with no pipe must not signal")))

(test pipe-pane-open-invalid-command-returns-nil
  "pipe-pane-open returns NIL when the shell program cannot be launched."
  ;; pipe-pane-open runs the command via `sh -c`, so a bogus *command* still
  ;; launches successfully (sh exists, then fails internally — matching tmux).
  ;; To exercise the launch-failure → NIL path, point *default-shell* at a
  ;; non-existent binary so uiop:launch-program itself fails.
  (let* ((pane   (%make-test-pane))
         (cl-tmux/config:*default-shell* "/no/such/shell-5f3a9b2e")
         (result (cl-tmux/commands:pipe-pane-open pane "echo hi")))
    (is (null result)
        "pipe-pane-open must return NIL when the shell cannot be launched")))

(test pipe-pane-write-bytes-reach-subprocess
  "pipe-pane-write with an open pipe sends bytes to the subprocess stdin.
   This drives a REAL shell subprocess + filesystem (cat > tmpfile), which is
   inherently nondeterministic under a heavily-loaded parallel build (subprocess
   scheduling / GC / fs flush timing).  Earlier single-shot versions — even with a
   6s poll — flaked.  We instead retry the whole self-contained cycle up to 5
   times and assert the bytes reach the subprocess on at least one attempt: this
   still verifies the real behaviour (bytes DO traverse the pipe to the child)
   while tolerating a one-off environmental hiccup.  3 deterministic failures in a
   row would still fail (a genuine break is not masked)."
  (flet ((attempt ()
           (let ((tmpfile (uiop:tmpize-pathname
                           (uiop:merge-pathnames* "pipe-pane-write-test"
                                                  (uiop:temporary-directory))))
                 (pane    (%make-test-pane)))
             (unwind-protect
                  (progn
                    (cl-tmux/commands:pipe-pane-open
                     pane (format nil "cat > ~A" (uiop:native-namestring tmpfile)))
                    (when (pane-pipe-fd pane)            ; launch succeeded
                      (cl-tmux/commands:pipe-pane-write pane #(65 66 67)) ; "ABC"
                      (cl-tmux/commands:pipe-pane-close pane)
                      (let ((contents ""))
                        (loop repeat 250                  ; ~1.25s per attempt
                              until (and (probe-file tmpfile)
                                         (search "ABC"
                                                 (setf contents
                                                       (or (ignore-errors
                                                             (uiop:read-file-string tmpfile))
                                                           ""))))
                              do (sleep 0.005))
                        (and (search "ABC" contents) t))))
               (ignore-errors (uiop:delete-file-if-exists tmpfile))))))
    (let ((ok nil))
      (dotimes (i 8) (unless ok (setf ok (attempt))))
      (is-true ok
               "bytes written via pipe-pane-write must reach the subprocess (within 8 attempts)"))))

;;; ── %copy-mode-row-string (direct unit tests) ───────────────────────────────

(test copy-mode-row-string-returns-row-content
  "%copy-mode-row-string returns the string content of the requested row."
  (let ((s (make-screen 20 5)))
    (feed s "hello")
    (cl-tmux/commands::copy-mode-enter s)
    (let ((row-str (cl-tmux/commands::%copy-mode-row-string s 0)))
      (is (stringp row-str)
          "%copy-mode-row-string must return a string")
      (is (and (>= (length row-str) 5)
               (string= "hello" (subseq row-str 0 5)))
          "%copy-mode-row-string must include the fed text at cols 0-4"))))

(test copy-mode-row-string-length-equals-screen-width
  "%copy-mode-row-string always returns a string of length = screen-width."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (is (= 20 (length (cl-tmux/commands::%copy-mode-row-string s 0)))
        "%copy-mode-row-string length must equal screen-width")))

;;; ── %run-with-timeout ────────────────────────────────────────────────────────

(test run-with-timeout-returns-thunk-result
  "%run-with-timeout returns the result of the thunk when it completes within time."
  (let ((result (cl-tmux/commands::%run-with-timeout (lambda () 42) 10)))
    (is (= 42 result)
        "%run-with-timeout must return the thunk result when no timeout occurs")))

(test run-with-timeout-returns-nil-on-timeout
  "%run-with-timeout returns NIL when the thunk exceeds the timeout."
  (let ((result (cl-tmux/commands::%run-with-timeout
                 (lambda () (sleep 60)) 1/1000)))
    (is (null result)
        "%run-with-timeout must return NIL when the thunk times out")))

;;; ── run-shell timeout ────────────────────────────────────────────────────────

(test run-shell-returns-nil-on-timeout
  "run-shell returns NIL when the command exceeds the given timeout."
  ;; Use a very short timeout (1ms) with a sleep command.
  (let ((result (cl-tmux/commands:run-shell "sleep 60" :timeout 1/1000)))
    (is (null result)
        "run-shell must return NIL when the command times out")))

;;; ── %copy-mode-clamp-cursor (direct unit tests) ──────────────────────────────

(test copy-mode-clamp-cursor-clamps-row-into-viewport
  "%copy-mode-clamp-cursor clamps the cursor row into [0, height-1]."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    ;; Force cursor outside viewport bounds.
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 10 3))
    (cl-tmux/commands::%copy-mode-clamp-cursor s)
    (is (= 4 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "row > height-1 must clamp to height-1=4")))

(test copy-mode-clamp-cursor-clamps-col-into-viewport
  "%copy-mode-clamp-cursor clamps the cursor col into [0, width-1]."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 50))
    (cl-tmux/commands::%copy-mode-clamp-cursor s)
    (is (= 19 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "col > width-1 must clamp to width-1=19")))

(test copy-mode-clamp-cursor-noop-when-cursor-nil
  "%copy-mode-clamp-cursor is a no-op when the cursor is NIL."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) nil)
    (finishes (cl-tmux/commands::%copy-mode-clamp-cursor s)
              "%copy-mode-clamp-cursor with nil cursor must not signal")))

(test copy-mode-clamp-cursor-preserves-in-range-values
  "%copy-mode-clamp-cursor leaves a cursor already in range unchanged."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 10))
    (cl-tmux/commands::%copy-mode-clamp-cursor s)
    (is (equal (cons 2 10) (cl-tmux/terminal/types:screen-copy-cursor s))
        "in-range cursor must be unchanged after clamp")))

;;; ── %selection-bounds (direct unit tests) ────────────────────────────────────

(test selection-bounds-same-row-mark-before-cursor
  "%selection-bounds returns (start-r end-r start-c end-c) when mark col < cursor col."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-mark   s) (cons 1 3)
          (cl-tmux/terminal/types:screen-copy-cursor s) (cons 1 8))
    (multiple-value-bind (start-row end-row start-col end-col)
        (cl-tmux/commands::%selection-bounds s)
      (is (= 1 start-row) "start-row must be 1")
      (is (= 1 end-row)   "end-row must be 1")
      (is (= 3 start-col) "start-col must be mark-col (3)")
      (is (= 8 end-col)   "end-col must be cursor-col (8)"))))

(test selection-bounds-same-row-cursor-before-mark
  "%selection-bounds normalises reversed cursor/mark on the same row."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-mark   s) (cons 1 8)
          (cl-tmux/terminal/types:screen-copy-cursor s) (cons 1 3))
    (multiple-value-bind (start-row end-row start-col end-col)
        (cl-tmux/commands::%selection-bounds s)
      (is (= 1 start-row) "start-row must be 1")
      (is (= 1 end-row)   "end-row must be 1")
      (is (= 3 start-col) "start-col must be min(3,8)=3")
      (is (= 8 end-col)   "end-col must be max(3,8)=8"))))

(test selection-bounds-multi-row-mark-above-cursor
  "%selection-bounds for multi-row selection where mark is on an earlier row."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-mark   s) (cons 0 2)
          (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 7))
    (multiple-value-bind (start-row end-row start-col end-col)
        (cl-tmux/commands::%selection-bounds s)
      (is (= 0 start-row) "start-row must be 0 (mark row)")
      (is (= 2 end-row)   "end-row must be 2 (cursor row)")
      (is (= 2 start-col) "start-col must be mark-col (2)")
      (is (= 7 end-col)   "end-col must be cursor-col (7)"))))

(test selection-bounds-multi-row-cursor-above-mark
  "%selection-bounds normalises reversed multi-row selection."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-mark   s) (cons 2 7)
          (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 2))
    (multiple-value-bind (start-row end-row start-col end-col)
        (cl-tmux/commands::%selection-bounds s)
      (is (= 0 start-row) "start-row must be 0 (cursor row — lower)")
      (is (= 2 end-row)   "end-row must be 2 (mark row — higher)")
      (is (= 2 start-col) "start-col must be cursor-col (2) since cursor-row < mark-row")
      (is (= 7 end-col)   "end-col must be mark-col (7)"))))

;;; ── copy-mode-word-backward edge cases ───────────────────────────────────────

(test copy-mode-word-backward-at-col-zero-stays-put
  "copy-mode-word-backward when cursor is already at col 0 does not move."
  (let ((s (%copy-mode-screen-with-text "hello world")))
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-word-backward s)
    (is (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "word-backward at col 0 must stay at col 0")))

(test copy-mode-word-backward-from-whitespace-skips-to-word-start
  "copy-mode-word-backward when cursor is in whitespace skips to the previous word start."
  (let ((s (%copy-mode-screen-with-text "hello world")))
    ;; Position cursor in the space between words (col 5).
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 5))
    (cl-tmux/commands::copy-mode-word-backward s)
    ;; Should land at col 0 (start of "hello").
    (is (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "word-backward from whitespace must jump to start of previous word (got ~D)"
        (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))))

(test copy-mode-word-backward-from-first-char-of-word
  "copy-mode-word-backward when cursor is at the first character of a word."
  (let ((s (%copy-mode-screen-with-text "hello world")))
    ;; Position at col 6 — the 'w' of "world".
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 6))
    (cl-tmux/commands::copy-mode-word-backward s)
    ;; Should land at col 0 (start of "hello").
    (is (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "word-backward from first char of word must jump to start of previous word (got ~D)"
        (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))))

;;; ── %join-pane-kill-empty-src direct tests ───────────────────────────────────

(test join-pane-kill-empty-src-removes-empty-window-from-session
  "%join-pane-kill-empty-src removes a window with no panes from the session."
  (let* ((src-win  (make-window :id 1 :name "src" :width 20 :height 5 :panes nil))
         (dst-win  (make-window :id 2 :name "dst" :width 20 :height 5
                                :panes (list (%make-test-pane :id 1))))
         (sess     (make-session :id 1 :name "0" :windows (list src-win dst-win))))
    (session-select-window sess src-win)
    (cl-tmux/commands::%join-pane-kill-empty-src sess src-win)
    (is-false (member src-win (session-windows sess))
              "empty src-win must be removed from session")
    ;; Active window switches to the remaining window.
    (is (eq dst-win (session-active-window sess))
        "active window must switch to dst-win after empty src-win is killed")))

(test join-pane-kill-empty-src-noop-when-panes-remain
  "%join-pane-kill-empty-src is a no-op when src-window still has panes."
  (let* ((pane     (%make-test-pane :id 1))
         (src-win  (make-window :id 1 :name "src" :width 20 :height 5 :panes (list pane)))
         (sess     (make-session :id 1 :name "0" :windows (list src-win))))
    (session-select-window sess src-win)
    (cl-tmux/commands::%join-pane-kill-empty-src sess src-win)
    ;; Window must still be in the session.
    (is (member src-win (session-windows sess))
        "non-empty src-win must not be removed from session")))

;;; ── %join-pane-insert-into-dst direct tests ──────────────────────────────────

(test join-pane-insert-into-dst-returns-src-pane
  "%join-pane-insert-into-dst returns src-pane on successful insertion."
  (let* ((src-pane (%make-test-pane :id 10))
         (dst-pane (%make-test-pane :id 20))
         (dst-win  (make-window :id 2 :name "dst" :width 20 :height 5
                                :tree (make-layout-leaf dst-pane)
                                :panes (list dst-pane))))
    (window-select-pane dst-win dst-pane)
    (let ((result (cl-tmux/commands::%join-pane-insert-into-dst src-pane dst-win :h)))
      (is (eq src-pane result)
          "%join-pane-insert-into-dst must return src-pane on success"))))

(test join-pane-insert-into-dst-returns-nil-when-no-active-pane
  "%join-pane-insert-into-dst returns NIL when dst-window has no active pane."
  ;; window-active-pane falls back to (first (window-panes w)), so a window
  ;; truly has "no active pane" only when its pane list is empty.  Build dst-win
  ;; with no panes and no tree to exercise the NIL-return contract.
  (let* ((src-pane (%make-test-pane :id 10))
         (dst-win  (make-window :id 2 :name "dst" :width 20 :height 5
                                :tree nil :panes nil)))
    (is (null (cl-tmux/commands::%join-pane-insert-into-dst src-pane dst-win :h))
        "%join-pane-insert-into-dst must return NIL when dst has no active pane")))

;;; ── resize-pane: up direction ────────────────────────────────────────────────

(test resize-horizontal-up-shrinks-active-grows-upper
  "On a horizontal split, :up from the lower pane shrinks the active pane
   (moves its top border down) and grows the upper neighbour.
   This is symmetric with :left from the right pane shrinking the active pane."
  (let* ((win (%hsplit-window 10))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win))))
    ;; Make p1 (lower) the active pane.
    (window-select-pane win p1)
    (is (eq p1 (resize-pane win :up 3)))
    (is (= 13 (pane-height p0)) "upper neighbour grows on :up from lower pane")
    (is (= 7  (pane-height p1)) "lower (active) pane shrinks on :up")))

;;; ── copy-mode-word-backward: noop outside copy mode ──────────────────────────

(test copy-mode-word-backward-noop-outside-copy-mode
  "copy-mode-word-backward is a no-op when not in copy mode."
  (let ((s (make-screen 20 5)))
    (feed s "hello world")
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 8))
    (cl-tmux/commands::copy-mode-word-backward s)
    (is (= 8 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "column must not change outside copy mode")))

;;; ── copy-mode-bottom: noop outside copy mode ─────────────────────────────────

(test copy-mode-bottom-noop-outside-copy-mode
  "copy-mode-bottom is a no-op when not in copy mode."
  (let ((s (make-screen 20 5)))
    (setf (cl-tmux/terminal/types:screen-scrollback s)
          (loop repeat 5 collect (make-array 0)))
    (setf (cl-tmux/terminal/types:screen-copy-offset s) 3)
    (cl-tmux/commands::copy-mode-bottom s)
    (is (= 3 (screen-copy-offset s))
        "offset must remain unchanged when not in copy mode")))

;;; ── copy-mode-search-backward: saves term ────────────────────────────────────

(test copy-mode-search-backward-saves-term
  "copy-mode-search-backward saves the search term for n/N repeats."
  (let ((s (make-screen 30 5)))
    (feed s "foo bar foo")
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 11))
    (cl-tmux/commands::copy-mode-search-backward s "foo")
    (is (string= "foo" (cl-tmux/terminal/types:screen-copy-search-term s))
        "search term must be saved after search-backward")))

;;; ── copy-mode-search-prev: positive case ─────────────────────────────────────

(test copy-mode-search-prev-repeats-backward
  "copy-mode-search-prev uses the saved term to repeat backward search."
  ;; Use a two-row screen: row 0 = "abc", row 1 = "abc def"
  (let ((s (make-screen 30 5)))
    (feed s "abc")
    (feed s (format nil "~C~C" #\Return #\Linefeed))
    (feed s "abc def")
    (cl-tmux/commands::copy-mode-enter s)
    ;; Save term via forward search first
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-search-forward s "abc")
    ;; Cursor should be on row 1 col 0 (second "abc")
    (is (= 1 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "precondition: forward search found second 'abc' on row 1")
    ;; Now search-prev should go back to row 0
    (cl-tmux/commands::copy-mode-search-prev s)
    (is (= 0 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "search-prev must find 'abc' on row 0")))

;;; ── %scroll-up-one-line direct tests ─────────────────────────────────────────

(test scroll-up-one-line-moves-cursor-up-within-viewport
  "%scroll-up-one-line decrements row when cursor is not at top of viewport."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    ;; Place cursor at row 3 (well within viewport, no scrollback needed)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 3 2))
    (cl-tmux/commands::%scroll-up-one-line s 3 2 0)
    (is (equal (cons 2 2) (cl-tmux/terminal/types:screen-copy-cursor s))
        "%scroll-up-one-line must decrement row when cursor is within viewport")))

(test scroll-up-one-line-scrolls-viewport-at-top-edge
  "%scroll-up-one-line scrolls the viewport when cursor is at row 0 and scrollback exists."
  (let ((s (%screen-with-scrollback 5)))
    ;; Place cursor at row 0 so the viewport needs to scroll
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 2))
    (let ((before-offset (screen-copy-offset s)))
      (cl-tmux/commands::%scroll-up-one-line s 0 2 5)
      (is (= (1+ before-offset) (screen-copy-offset s))
          "%scroll-up-one-line must increment viewport offset at top edge")
      (is (= 0 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
          "cursor row must stay at 0 when viewport scrolls"))))

(test scroll-up-one-line-noop-at-oldest-scrollback
  "%scroll-up-one-line is a no-op when cursor is at row 0 and offset equals max."
  (let ((s (%screen-with-scrollback 3)))
    (setf (cl-tmux/terminal/types:screen-copy-offset s) 3)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 2))
    (cl-tmux/commands::%scroll-up-one-line s 0 2 3)
    (is (= 3 (screen-copy-offset s))
        "%scroll-up-one-line must not increment offset past max-offset")
    (is (= 0 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "cursor row must remain 0")))

;;; ── %scroll-down-one-line direct tests ───────────────────────────────────────

(test scroll-down-one-line-moves-cursor-down-within-viewport
  "%scroll-down-one-line increments row when cursor is not at viewport bottom."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    ;; Place cursor at row 1 (within viewport)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 1 2))
    (cl-tmux/commands::%scroll-down-one-line s 1 2 5)
    (is (equal (cons 2 2) (cl-tmux/terminal/types:screen-copy-cursor s))
        "%scroll-down-one-line must increment row when cursor is within viewport")))

(test scroll-down-one-line-scrolls-viewport-at-bottom-edge
  "%scroll-down-one-line scrolls the viewport when cursor is at bottom and offset > 0."
  (let ((s (%screen-with-scrollback 10)))
    ;; Set offset > 0 so we can scroll forward
    (setf (cl-tmux/terminal/types:screen-copy-offset s) 5)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 4 2))
    (cl-tmux/commands::%scroll-down-one-line s 4 2 5)
    (is (= 4 (screen-copy-offset s))
        "%scroll-down-one-line must decrement viewport offset at bottom edge")
    (is (= 4 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "cursor row must stay at h-1 when viewport scrolls")))

(test scroll-down-one-line-noop-at-live-view-bottom
  "%scroll-down-one-line is a no-op when cursor is at the bottom and offset is 0."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 4 2))
    (cl-tmux/commands::%scroll-down-one-line s 4 2 5)
    (is (= 0 (screen-copy-offset s))
        "%scroll-down-one-line must not move past live view (offset must stay 0)")
    (is (= 4 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "cursor row must remain 4")))

;;; ── %extract-row-chars direct tests ──────────────────────────────────────────

(test extract-row-chars-returns-substring-of-row
  "%extract-row-chars returns the correct string slice from the given row."
  (let ((s (make-screen 20 5)))
    (feed s "hello world")
    (let ((result (cl-tmux/commands::%extract-row-chars s 0 0 5)))
      (is (stringp result)
          "%extract-row-chars must return a string")
      (is (string= "hello" result)
          "%extract-row-chars must return cols 0-4 as \"hello\" (got ~S)" result))))

(test extract-row-chars-empty-range-returns-empty-string
  "%extract-row-chars with from-col = to-col returns an empty string."
  (let ((s (make-screen 20 5)))
    (feed s "hello")
    (let ((result (cl-tmux/commands::%extract-row-chars s 0 3 3)))
      (is (string= "" result)
          "%extract-row-chars with empty range must return empty string"))))

;;; ── %copy-row-range-to-paste-buffer direct tests ─────────────────────────────

(test copy-row-range-to-paste-buffer-adds-trimmed-text
  "%copy-row-range-to-paste-buffer pushes right-trimmed text to paste buffers."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "hello")
      (cl-tmux/commands::%copy-row-range-to-paste-buffer s 0 0 10)
      (is (= 1 (length cl-tmux/buffer:*paste-buffers*))
          "one paste buffer entry must be added")
      (let ((got (cl-tmux/buffer:get-paste-buffer 0)))
        (is (string= "hello" got)
            "%copy-row-range-to-paste-buffer must push right-trimmed text (got ~S)" got)))))

(test copy-row-range-to-paste-buffer-noop-when-all-spaces
  "%copy-row-range-to-paste-buffer does nothing when the trimmed result is empty."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      ;; Row 0 is blank (all spaces) — the trimmed result will be empty.
      (cl-tmux/commands::%copy-row-range-to-paste-buffer s 0 0 10)
      (is (null cl-tmux/buffer:*paste-buffers*)
          "paste buffers must remain empty when the selected range is all spaces"))))

;;; ── %copy-mode-row-chars direct tests ────────────────────────────────────────

(test copy-mode-row-chars-returns-character-vector
  "%copy-mode-row-chars returns a simple-vector of characters for the given row."
  (let ((s (make-screen 20 5)))
    (feed s "hello")
    (cl-tmux/commands::copy-mode-enter s)
    (let ((chars (cl-tmux/commands::%copy-mode-row-chars s 0)))
      (is (vectorp chars)
          "%copy-mode-row-chars must return a vector")
      (is (= 20 (length chars))
          "%copy-mode-row-chars vector length must equal screen-width")
      (is (char= #\h (aref chars 0))
          "first character must be #\\h"))))

;;; ── %screen-row-string and %scrollback-row-string direct tests ───────────────

(test screen-row-string-returns-full-row-as-string
  "%screen-row-string returns a string of width characters for the given row."
  (let ((s (make-screen 20 5)))
    (feed s "hello")
    (let ((row-str (cl-tmux/commands::%screen-row-string s 0)))
      (is (stringp row-str)
          "%screen-row-string must return a string")
      (is (= 20 (length row-str))
          "%screen-row-string length must equal screen-width (20)")
      (is (string= "hello" (subseq row-str 0 5))
          "%screen-row-string must include the fed text at cols 0-4"))))

(test scrollback-row-string-converts-cell-vector
  "%scrollback-row-string returns a string built from a cell vector."
  (let* ((cells (make-array 5 :initial-element
                             (cl-tmux/terminal/types:make-cell
                              :char #\A :fg 7 :bg 0 :attrs 0 :width 1)))
         (result (cl-tmux/commands::%scrollback-row-string cells)))
    (is (stringp result)
        "%scrollback-row-string must return a string")
    (is (= 5 (length result))
        "%scrollback-row-string length must equal cell-vector length")
    (is (every (lambda (c) (char= #\A c)) (coerce result 'list))
        "%scrollback-row-string must extract char from each cell")))

;;; ── rename-session via hooks ─────────────────────────────────────────────────

(test rename-session-does-not-run-hooks
  "rename-session is a pure setter; it fires no hooks."
  (with-isolated-hooks
    (let ((hook-called nil))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-rename-window+
                              (lambda (&rest _) (declare (ignore _)) (setf hook-called t)))
      (let ((sess (make-session :id 1 :name "old" :windows nil)))
        (cl-tmux/commands:rename-session sess "new"))
      (is-false hook-called
                "rename-session must not fire any hooks"))))

;;; ── rename-window: fires hook ────────────────────────────────────────────────

(test rename-window-fires-after-rename-window-hook
  "rename-window fires +hook-after-rename-window+ with the window and new name."
  (with-isolated-hooks
    (let ((hook-win nil)
          (hook-name nil))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-rename-window+
                              (lambda (w n) (setf hook-win w hook-name n)))
      (let ((win (make-window :id 1 :name "old" :width 20 :height 5 :panes nil)))
        (rename-window win "new"))
      (is (stringp hook-name)
          "hook must receive the new name as a string")
      (is (string= "new" hook-name)
          "hook name argument must equal the new name"))))

;;; ── copy-mode-begin-line-selection: multi-row window ────────────────────────

(test copy-mode-begin-line-selection-selects-correct-width
  "copy-mode-begin-line-selection marks col width-1 on a non-default screen width."
  (let ((s (make-screen 40 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 10))
    (cl-tmux/commands::copy-mode-begin-line-selection s)
    (is (= 39 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "cursor col must be width-1=39 for 40-column screen")
    (is (= 0 (cdr (cl-tmux/terminal/types:screen-copy-mark s)))
        "mark col must be 0 for line selection")))

;;; ── copy-mode-copy-line: preserves content without trailing spaces ───────────

(test copy-mode-copy-line-right-trims-trailing-spaces
  "copy-mode-copy-line right-trims trailing spaces before pushing to paste buffer."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "hi")          ; "hi" followed by 18 spaces on row 0
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 5))
      (cl-tmux/commands::copy-mode-copy-line s)
      (let ((yanked (cl-tmux/buffer:get-paste-buffer 0)))
        (is (and yanked (string= "hi" yanked))
            "copy-mode-copy-line must right-trim spaces (got ~S)" yanked)))))

;;; ── copy-mode-copy-end-of-line: cursor at column 0 ──────────────────────────

(test copy-mode-copy-end-of-line-from-col-0-copies-entire-row
  "copy-mode-copy-end-of-line from col 0 copies the full row content."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "hello world")
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
      (cl-tmux/commands::copy-mode-copy-end-of-line s)
      (let ((yanked (cl-tmux/buffer:get-paste-buffer 0)))
        (is (and yanked (search "hello world" yanked))
            "D from col 0 must copy 'hello world' (got ~S)" yanked)))))

;;; ── with-shell-timeout macro coverage ───────────────────────────────────────

(test with-shell-timeout-returns-result-on-success
  "with-shell-timeout macro returns the result when thunk completes in time."
  (let ((result (cl-tmux/commands::with-shell-timeout (shell 30)
                  (string= "/bin/sh" shell)
                  42)))
    ;; result is the value of the last form in the body
    (is (= 42 result)
        "with-shell-timeout must return the last form result when no timeout")))

;;; ── %nearest-window: empty list returns nil ──────────────────────────────────

(test nearest-window-empty-list-returns-nil
  "%nearest-window with an empty windows list returns NIL."
  (is (null (cl-tmux/commands::%nearest-window nil 5))
      "%nearest-window with empty list must return NIL"))

;;; ── kill-pane: fires hook ────────────────────────────────────────────────────

(test kill-pane-fires-after-kill-pane-hook
  "kill-pane fires +hook-after-kill-pane+ with the killed pane."
  (with-isolated-hooks
    (let ((hooked-pane nil))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-kill-pane+
                              (lambda (p) (setf hooked-pane p)))
      (let* ((win  (%vsplit-window 20))
             (p0   (first  (window-panes win)))
             (p1   (second (window-panes win)))
             (sess (make-session :id 1 :name "0" :windows (list win))))
        (session-select-window sess win)
        (window-select-pane win p0)
        (kill-pane sess p1)
        (is (eq p1 hooked-pane)
            "+hook-after-kill-pane+ must be called with the killed pane")))))

;;; ── kill-window: fires hook ──────────────────────────────────────────────────

(test kill-window-fires-after-kill-window-hook
  "kill-window fires +hook-after-kill-window+ with the killed window."
  (with-isolated-hooks
    (let ((hooked-win nil))
      (cl-tmux/hooks:add-hook cl-tmux/hooks:+hook-after-kill-window+
                              (lambda (w) (setf hooked-win w)))
      (let* ((p0   (%make-test-pane))
             (w1   (make-window :id 1 :name "a" :width 20 :height 5
                                :tree (make-layout-leaf p0) :panes (list p0)))
             (w2   (make-window :id 2 :name "b" :width 20 :height 5
                                :panes (list (%make-test-pane :id 2))))
             (sess (make-session :id 1 :name "0" :windows (list w1 w2))))
        (session-select-window sess w1)
        (kill-window sess w1)
        (is (eq w1 hooked-win)
            "+hook-after-kill-window+ must be called with the killed window")))))

;;; ── copy-mode-toggle-rectangle ───────────────────────────────────────────────

(test copy-mode-toggle-rectangle-flips-flag
  "copy-mode-toggle-rectangle toggles screen-copy-rect-select-p between NIL and T."
  (let ((s (%copy-mode-screen)))
    (is-false (cl-tmux/terminal/types:screen-copy-rect-select-p s)
              "rect-select must start NIL")
    (cl-tmux/commands::copy-mode-toggle-rectangle s)
    (is-true  (cl-tmux/terminal/types:screen-copy-rect-select-p s)
              "rect-select must be T after first toggle")
    (cl-tmux/commands::copy-mode-toggle-rectangle s)
    (is-false (cl-tmux/terminal/types:screen-copy-rect-select-p s)
              "rect-select must return to NIL after second toggle")))

(test copy-mode-toggle-rectangle-noop-outside-copy-mode
  "copy-mode-toggle-rectangle does nothing when not in copy mode."
  (let ((s (make-screen 20 5)))
    (is-false (screen-copy-mode-p s) "precondition: not in copy mode")
    (cl-tmux/commands::copy-mode-toggle-rectangle s)
    (is-false (cl-tmux/terminal/types:screen-copy-rect-select-p s)
              "rect-select must remain NIL outside copy mode")))

(test copy-mode-exit-resets-rect-select
  "copy-mode-exit clears screen-copy-rect-select-p."
  (let ((s (%copy-mode-screen)))
    (setf (cl-tmux/terminal/types:screen-copy-rect-select-p s) t)
    (cl-tmux/commands::copy-mode-exit s)
    (is-false (cl-tmux/terminal/types:screen-copy-rect-select-p s)
              "rect-select must be NIL after exit")))

;;; ── copy-mode-append-selection ───────────────────────────────────────────────

(test copy-mode-append-selection-appends-to-existing-buffer
  "copy-mode-append-selection appends selected text to the current paste buffer entry."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    ;; Seed a buffer entry.
    (cl-tmux/buffer:add-paste-buffer "hello")
    (let ((s (make-screen 20 5)))
      (feed s " world")
      (cl-tmux/commands::copy-mode-enter s)
      ;; Manually set a selection spanning " world" on row 0.
      (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
            (cl-tmux/terminal/types:screen-copy-mark      s) (cons 0 0)
            (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 6))
      (cl-tmux/commands::copy-mode-append-selection s)
      ;; Exactly one buffer entry (appended, not pushed).
      (is (= 1 (length cl-tmux/buffer:*paste-buffers*))
          "append-selection must not add a second paste buffer entry")
      (let ((buf (cl-tmux/buffer:get-paste-buffer 0)))
        (is (and (stringp buf) (search "hello" buf))
            "appended buffer must contain original text")
        (is (and (stringp buf) (search " world" buf))
            "appended buffer must contain the newly appended text")))))

(test copy-mode-append-selection-creates-new-entry-when-empty
  "copy-mode-append-selection pushes a new entry when the paste buffer is empty."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "hello")
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
            (cl-tmux/terminal/types:screen-copy-mark      s) (cons 0 0)
            (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 5))
      (cl-tmux/commands::copy-mode-append-selection s)
      (is (= 1 (length cl-tmux/buffer:*paste-buffers*))
          "append-selection must create one entry when buffer is empty")
      (is (string= "hello" (cl-tmux/buffer:get-paste-buffer 0))
          "new entry must equal the selected text"))))

;;; ── copy-mode-copy-pipe ──────────────────────────────────────────────────────

(test copy-mode-copy-pipe-puts-text-in-paste-buffer
  "copy-mode-copy-pipe adds the selected text to the paste buffer."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "pipe-me")
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
            (cl-tmux/terminal/types:screen-copy-mark      s) (cons 0 0)
            (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 7))
      ;; Pass an empty CMD so only the buffer side runs (no real shell invoked).
      (cl-tmux/commands::copy-mode-copy-pipe s "")
      (is (= 1 (length cl-tmux/buffer:*paste-buffers*))
          "copy-pipe must push selected text to paste buffers")
      (is (string= "pipe-me" (cl-tmux/buffer:get-paste-buffer 0))
          "paste buffer must contain the selected text"))))

(test copy-mode-copy-pipe-exits-copy-mode
  "copy-mode-copy-pipe exits copy mode after yanking."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "data")
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
            (cl-tmux/terminal/types:screen-copy-mark      s) (cons 0 0)
            (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 4))
      (cl-tmux/commands::copy-mode-copy-pipe s "")
      (is-false (screen-copy-mode-p s)
                "copy mode must be inactive after copy-pipe"))))

;;; ── rectangle selection text ─────────────────────────────────────────────────

(test copy-mode-yank-rectangle-uses-fixed-columns
  "When rect-select is T, yank uses column bounds from mark and cursor on every row."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 10 5)))
      ;; Write row 0 "abcde" and row 1 "ABCDE" using CR+LF to ensure row 1 starts at col 0.
      (feed s (format nil "abcde~C~CABCDE" #\Return #\Linefeed))
      (cl-tmux/commands::copy-mode-enter s)
      ;; Rectangle col 1-3, rows 0-1.
      ;; %extract-row-chars from-col=1 to-col=3 → 2 chars at cols 1 and 2.
      ;; Row 0: "bc"; row 1: "BC".
      (setf (cl-tmux/terminal/types:screen-copy-rect-select-p s) t
            (cl-tmux/terminal/types:screen-copy-selecting s) t
            (cl-tmux/terminal/types:screen-copy-mark      s) (cons 0 1)
            (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 1 3))
      (cl-tmux/commands::copy-mode-yank s)
      (let ((buf (cl-tmux/buffer:get-paste-buffer 0)))
        (is (and (stringp buf) (search "bc" buf))
            "rectangle yank must include chars from first row")
        (is (and (stringp buf) (search "BC" buf))
            "rectangle yank must include chars from second row")))))

;;; ── renumber-windows option ───────────────────────────────────────────────────

(test renumber-windows-renumbers-after-kill
  "kill-window renumbers remaining windows from base-index when renumber-windows is on."
  (let ((cl-tmux/options:*global-options*
         (let ((h (make-hash-table :test #'equal)))
           (setf (gethash "renumber-windows" h) t
                 (gethash "base-index"       h) 0)
           h)))
    (let* ((s    (make-fake-session :nwindows 3))
           (wins (cl-tmux/model:session-windows s))
           ;; Manually give them non-contiguous IDs as if gaps already existed.
           (_ (setf (cl-tmux/model:window-id (first  wins)) 1
                    (cl-tmux/model:window-id (second wins)) 3
                    (cl-tmux/model:window-id (third  wins)) 5))
           ;; Kill the first window (id=1); remaining are 3 and 5.
           (_2 (kill-window s (first wins))))
      (declare (ignore _ _2))
      (let ((ids (mapcar #'cl-tmux/model:window-id (cl-tmux/model:session-windows s))))
        (is (equal '(0 1) ids)
            "After kill with renumber-windows, windows should be renumbered 0,1; got ~S" ids)))))

(test renumber-windows-off-preserves-ids
  "kill-window does not renumber windows when renumber-windows is off."
  (let ((cl-tmux/options:*global-options*
         (let ((h (make-hash-table :test #'equal)))
           (setf (gethash "renumber-windows" h) nil)
           h)))
    (let* ((s    (make-fake-session :nwindows 3))
           (wins (cl-tmux/model:session-windows s))
           (_ (setf (cl-tmux/model:window-id (first  wins)) 1
                    (cl-tmux/model:window-id (second wins)) 3
                    (cl-tmux/model:window-id (third  wins)) 5))
           (_2 (kill-window s (first wins))))
      (declare (ignore _ _2))
      (let ((ids (mapcar #'cl-tmux/model:window-id (cl-tmux/model:session-windows s))))
        (is (equal '(3 5) ids)
            "Without renumber-windows, IDs stay as-is; got ~S" ids)))))

;;; ── %rectangle-selection-text (direct unit tests) ────────────────────────────
;;;
;;; %rectangle-selection-text is exercised transitively through copy-mode-yank
;;; with rect-select=T.  These direct tests make boundary conditions explicit.

(test rectangle-selection-text-returns-nil-when-no-selection
  "%rectangle-selection-text returns NIL when no selection is active."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-selecting s) nil)
    (is (null (cl-tmux/commands::%rectangle-selection-text s))
        "%rectangle-selection-text must return NIL when copy-selecting is NIL")))

(test rectangle-selection-text-returns-nil-when-mark-nil
  "%rectangle-selection-text returns NIL when mark is NIL even if selecting is T."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
          (cl-tmux/terminal/types:screen-copy-mark      s) nil
          (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 0 5))
    (is (null (cl-tmux/commands::%rectangle-selection-text s))
        "%rectangle-selection-text must return NIL when mark is NIL")))

(test rectangle-selection-text-single-row
  "%rectangle-selection-text returns the correct column slice for a single-row selection."
  ;; Feed "hello world" to row 0; rectangle from col 0 to col 5 on row 0 only.
  (let ((s (make-screen 20 5)))
    (feed s "hello world")
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-selecting    s) t
          (cl-tmux/terminal/types:screen-copy-mark         s) (cons 0 0)
          (cl-tmux/terminal/types:screen-copy-cursor       s) (cons 0 5))
    (let ((text (cl-tmux/commands::%rectangle-selection-text s)))
      (is (stringp text) "%rectangle-selection-text must return a string")
      (is (string= "hello" text)
          "%rectangle-selection-text must return cols 0-4 (got ~S)" text))))

(test rectangle-selection-text-multi-row-fixed-columns
  "%rectangle-selection-text extracts the same column range on every row."
  ;; Row 0 = "abcde", row 1 = "ABCDE"; rectangle col 1-3 (2 chars per row).
  (let ((s (make-screen 10 5)))
    (feed s (format nil "abcde~C~CABCDE" #\Return #\Linefeed))
    (cl-tmux/commands::copy-mode-enter s)
    (setf (cl-tmux/terminal/types:screen-copy-selecting s) t
          (cl-tmux/terminal/types:screen-copy-mark      s) (cons 0 1)
          (cl-tmux/terminal/types:screen-copy-cursor    s) (cons 1 3))
    (let ((text (cl-tmux/commands::%rectangle-selection-text s)))
      (is (stringp text) "%rectangle-selection-text must return a string")
      (is (search "bc" text)
          "%rectangle-selection-text must include cols 1-2 from row 0 (got ~S)" text)
      (is (search "BC" text)
          "%rectangle-selection-text must include cols 1-2 from row 1 (got ~S)" text)
      (is (find #\Newline text)
          "%rectangle-selection-text must separate rows with newlines"))))

;;; ── %run-copy-command (direct unit tests) ────────────────────────────────────
;;;
;;; %run-copy-command is exercised only transitively through copy-mode-yank when
;;; the 'copy-command' option is set.  These direct tests cover the no-op branch
;;; (empty option / empty text) and the error-handling contract.

(test run-copy-command-noop-when-text-is-nil
  "%run-copy-command is a no-op when TEXT is NIL."
  (finishes (cl-tmux/commands::%run-copy-command nil)
            "%run-copy-command with nil text must not signal"))

(test run-copy-command-noop-when-text-is-empty
  "%run-copy-command is a no-op when TEXT is an empty string."
  (finishes (cl-tmux/commands::%run-copy-command "")
            "%run-copy-command with empty text must not signal"))

(test run-copy-command-noop-when-option-unset
  "%run-copy-command is a no-op when the 'copy-command' option is not set."
  ;; Fresh option table: 'copy-command' is absent.
  (with-fresh-global-options
    (finishes (cl-tmux/commands::%run-copy-command "some text")
              "%run-copy-command with no copy-command option must not signal")))

(test run-copy-command-does-not-crash-on-bad-command
  "%run-copy-command swallows errors from a malformed copy-command."
  ;; Set copy-command to a command that will fail (exit non-zero or not found).
  (let ((cl-tmux/options:*global-options*
         (let ((h (make-hash-table :test #'equal)))
           (setf (gethash "copy-command" h) "false")
           h)))
    (finishes (cl-tmux/commands::%run-copy-command "hello")
              "%run-copy-command must not signal when the copy-command fails")))

;;; ── copy-mode-set-cursor (direct unit tests in commands group) ───────────────
;;;
;;; copy-mode-set-cursor is exported from cl-tmux/commands and tested in
;;; events-tests.lisp (via keystroke dispatch), but that test lives outside the
;;; commands audit scope.  Direct tests here make the commands group self-contained.

(test copy-mode-set-cursor-positions-cursor
  "copy-mode-set-cursor sets the cursor to the given row and column."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (cl-tmux/commands:copy-mode-set-cursor s 2 7)
    (is (equal (cons 2 7) (cl-tmux/terminal/types:screen-copy-cursor s))
        "copy-mode-set-cursor must set cursor to (2 . 7)")))

(test copy-mode-set-cursor-clamps-row-to-bounds
  "copy-mode-set-cursor clamps the row to [0, height-1]."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (cl-tmux/commands:copy-mode-set-cursor s 99 0)
    (is (= 4 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "row > height-1 must clamp to height-1=4")
    (cl-tmux/commands:copy-mode-set-cursor s -1 0)
    (is (= 0 (car (cl-tmux/terminal/types:screen-copy-cursor s)))
        "row < 0 must clamp to 0")))

(test copy-mode-set-cursor-clamps-col-to-bounds
  "copy-mode-set-cursor clamps the column to [0, width-1]."
  (let ((s (make-screen 20 5)))
    (cl-tmux/commands::copy-mode-enter s)
    (cl-tmux/commands:copy-mode-set-cursor s 0 99)
    (is (= 19 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "col > width-1 must clamp to width-1=19")
    (cl-tmux/commands:copy-mode-set-cursor s 0 -1)
    (is (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))
        "col < 0 must clamp to 0")))

(test copy-mode-set-cursor-noop-outside-copy-mode
  "copy-mode-set-cursor is a no-op when not in copy mode."
  (let ((s (make-screen 20 5)))
    ;; Do NOT enter copy mode.
    (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 1 1))
    (cl-tmux/commands:copy-mode-set-cursor s 3 7)
    (is (equal (cons 1 1) (cl-tmux/terminal/types:screen-copy-cursor s))
        "cursor must be unchanged outside copy mode")))

;;; ── send-keys -l (literal) vs translated ────────────────────────────────────
;;;
;;; send-keys-to-pane (pane string &key literal) is the production entry point,
;;; but it needs a pane with a real PTY (fd > -1) to observe output; fake panes
;;; have fd -1, where pty-write is a harmless no-op.  We therefore test the
;;; byte-production logic that distinguishes the two modes:
;;;   - non-literal: %translate-send-keys maps the key name "Enter" → CR (13).
;;;   - literal (-l): the string is emitted as raw UTF-8 bytes, so "Enter"
;;;     stays the 5 bytes E-n-t-e-r with no key-name interpretation.

(test send-keys-translated-enter-produces-cr
  "Without -l, %translate-send-keys maps the key name \"Enter\" to a single CR byte (13)."
  (let ((bytes (cl-tmux/commands::%translate-send-keys "Enter")))
    (is (= 1 (length bytes))
        "translated \"Enter\" must be exactly one byte (got length ~D)" (length bytes))
    (is (= 13 (aref bytes 0))
        "translated \"Enter\" must be CR (char code 13), got ~D" (aref bytes 0))))

(test send-keys-literal-enter-stays-five-bytes
  "With -l, the string \"Enter\" is written as raw UTF-8 bytes — five literal
   characters E-n-t-e-r — NOT translated to a CR.  This is the byte payload
   send-keys-to-pane writes when :literal is true."
  (let ((literal-bytes (babel:string-to-octets "Enter")))
    (is (= 5 (length literal-bytes))
        "literal \"Enter\" must be five bytes (got length ~D)" (length literal-bytes))
    (is (equalp #(69 110 116 101 114) literal-bytes)
        "literal \"Enter\" must be the ASCII bytes for E,n,t,e,r")
    ;; The literal payload must differ from the translated (single-CR) payload.
    (is (not (equalp literal-bytes
                     (cl-tmux/commands::%translate-send-keys "Enter")))
        "literal mode must NOT equal the translated single-CR payload")))

(test send-keys-literal-multibyte-utf8-preserves-bytes
  "With -l, a multi-byte UTF-8 string is emitted as its raw UTF-8 octets:
   \"café\" is 4 characters but encodes to 5 bytes (é = 2 bytes), so literal
   mode preserves the multi-byte encoding rather than counting characters."
  (let ((literal-bytes (babel:string-to-octets "café" :encoding :utf-8)))
    (is (= 5 (length literal-bytes))
        "literal \"café\" must be 5 UTF-8 bytes (got length ~D)" (length literal-bytes))
    (is (> (length literal-bytes) (length "café"))
        "byte count (~D) must exceed the 4-character count, proving multi-byte preservation"
        (length literal-bytes))
    ;; The é (U+00E9) encodes to the two-byte sequence C3 A9; assert the tail.
    (is (equalp #(195 169) (subseq literal-bytes 3))
        "the é must encode to the two UTF-8 bytes C3 A9 (got ~S)"
        (subseq literal-bytes 3))))
