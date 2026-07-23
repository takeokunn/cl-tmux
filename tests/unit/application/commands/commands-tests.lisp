(in-package #:cl-tmux/test)

;;;; resize-pane, copy-mode-scroll, select-window, rename-session — part I

;;; ── Local fixtures (no PTY: fd -1, pid -1) ──────────────────────────────────
;;;
;;; Tree-based split windows: assembled with make-layout-leaf / make-layout-split
;;; to use the tree-layout path.  tl-leaf / tl-window helpers are defined
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

(describe "commands-suite"

  ;;; ── resize-pane: vertical split ─────────────────────────────────────────────

  ;; On a vertical split, :right grows the active (left) pane and shrinks its
  ;; right neighbour; resize-pane delegates to window-resize-active.
  (it "resize-vertical-right-grows-active-shrinks-neighbour"
    (let* ((win (%vsplit-window 20))
           (p0  (first  (window-panes win)))
           (p1  (second (window-panes win))))
      (expect (eq p0 (resize-pane win :right 5)))
      (expect (= 25 (pane-width p0)))
      (expect (= 15 (pane-width p1)))
      (expect (= 26 (pane-x p1)))))

  ;; When the active pane is the right one, :left adjusts against the previous
  ;; (left) neighbour rather than a non-existent right neighbour.
  (it "resize-vertical-left-picks-previous-neighbour"
    (let* ((win (%vsplit-window 20))
           (p0  (first  (window-panes win)))
           (p1  (second (window-panes win))))
      (window-select-pane win p1)            ; make the right pane active
      (expect (eq p1 (resize-pane win :left 5)))
      (expect (= 15 (pane-width p1)))
      (expect (= 25 (pane-width p0)))))

  ;; resize-pane with no tree (NIL) returns NIL immediately.
  (it "resize-pane-no-tree-returns-nil"
    (let* ((p0  (%make-test-pane))
           (win (make-window :id 1 :name "w" :width 20 :height 5
                             :panes (list p0) :tree nil)))
      (window-select-pane win p0)
      (expect (null (resize-pane win :right 5)))))

  ;;; ── resize-pane: horizontal split ───────────────────────────────────────────

  ;; On a horizontal split, :down grows the active (upper) pane and shrinks the
  ;; lower neighbour; the neighbour's y slides one row past the grown pane.
  (it "resize-horizontal-down-grows-active-shrinks-lower"
    (let* ((win (%hsplit-window 10))
           (p0  (first  (window-panes win)))
           (p1  (second (window-panes win))))
      (expect (eq p0 (resize-pane win :down 3)))
      (expect (= 13 (pane-height p0)))
      (expect (= 7  (pane-height p1)))))

  ;;; ── resize-pane -x / -y: absolute size (command arg form) ────────────────────

  ;; resize-pane -x N sets the active pane's width to N (both grow and shrink paths).
  (it "resize-pane-x-absolute-table"
    (dolist (row '(("25" 25 "resize-pane -x 25 grows pane from 20 to 25")
                   ("15" 15 "resize-pane -x 15 shrinks pane from 20 to 15")))
      (destructuring-bind (n-str expected desc) row
        (declare (ignore desc))
        (let* ((win (%vsplit-window 20))
               (p0  (first (window-panes win)))
               (s   (%make-session-with-window win)))
          (cl-tmux::%cmd-resize-pane-arg s (list "-x" n-str))
          (expect (= expected (pane-width p0)))))))

  ;; resize-pane -y N sets the active pane to an absolute height of N.
  (it "resize-pane-y-absolute-sets-height"
    (let* ((win (%hsplit-window 10))
           (p0  (first (window-panes win)))
           (s   (%make-session-with-window win)))
      (cl-tmux::%cmd-resize-pane-arg s '("-y" "13"))
      (expect (= 13 (pane-height p0)))))

  ;;; ── copy-mode-scroll ─────────────────────────────────────────────────────────

  ;; Scrolling back (positive delta) past the oldest line clamps the copy-offset
  ;; to the scrollback length.
  (it "copy-mode-scroll-back-clamps-to-scrollback-length"
    (let ((s (%screen-with-scrollback 3)))
      (cl-tmux/commands::copy-mode-scroll s 100)
      (expect (= 3 (screen-copy-offset s)))
      (expect (cl-tmux/terminal/types:screen-dirty-p s) :to-be-truthy)))

  ;; Scrolling forward (negative delta) past the live view clamps the offset at 0.
  (it "copy-mode-scroll-forward-clamps-at-zero"
    (let ((s (%screen-with-scrollback 3)))
      (cl-tmux/commands::copy-mode-scroll s 100)   ; first jump to the oldest line
      (expect (= 3 (screen-copy-offset s)))
      (cl-tmux/commands::copy-mode-scroll s -100)  ; then race back to live
      (expect (= 0 (screen-copy-offset s)))))

  ;; A full-row selection made while scrolled back into the scrollback yanks the
  ;; text the user SEES at that viewport row (via screen-display-cell), not the
  ;; live-grid row at the same index.  Regression guard for the screen-cell ->
  ;; screen-display-cell fix in %extract-row-chars.
  (it "copy-mode-selection-honours-scroll-offset"
    (let ((s (make-screen 8 3)))
      (feed-lines s "AAA" "BBB" "CCC" "DDD" "EEE")
      (cl-tmux/commands::copy-mode-enter s)
      (cl-tmux/commands::copy-mode-scroll s 1000)   ; scroll fully back
      (expect (plusp (screen-copy-offset s)))
      (let ((w      (screen-width s))
            (offset (screen-copy-offset s)))
        (let ((expected (string-right-trim " " (display-row-string s 0))))
          ;; Mark and cursor at viewport row 0; supply the offset so %selection-bounds
          ;; can correctly compute virtual rows even though we're setting mark manually.
          (setf (screen-copy-mark        s) (cons 0 0)
                (screen-copy-mark-offset s) offset
                (screen-copy-cursor      s) (cons 0 w)
                (screen-copy-selecting   s) t)
          (expect (string= expected
                           (string-right-trim " " (or (cl-tmux/commands::%selection-text s) ""))))))))

  ;; copy-mode-enter with :exit-on-bottom t sets the screen slot.
  (it "copy-mode-enter-e-sets-exit-on-bottom"
    (let ((s (make-screen 20 5)))
      (cl-tmux/commands::copy-mode-enter s :exit-on-bottom t)
      (expect (cl-tmux/terminal/types:screen-copy-exit-on-bottom s) :to-be-truthy)))

  ;; With exit-on-bottom (copy-mode -e), scrolling back down to the live bottom
  ;; (offset 0) auto-exits copy mode.
  (it "copy-mode-e-auto-exits-on-scroll-to-bottom"
    (let ((s (%screen-with-scrollback 3)))
      ;; Re-enter with -e semantics and scroll up into the scrollback.
      (cl-tmux/commands::copy-mode-enter s :exit-on-bottom t)
      (cl-tmux/commands::copy-mode-scroll s 2)        ; scroll back 2 lines (offset 2)
      (expect (= 2 (screen-copy-offset s)))
      (expect (screen-copy-mode-p s) :to-be-truthy)
      (cl-tmux/commands::copy-mode-scroll s -100)     ; race back to the live bottom
      (expect (screen-copy-mode-p s) :to-be-falsy)))

  ;; copy-mode -e does NOT exit while scrolling upward (positive delta).
  (it "copy-mode-e-no-exit-while-scrolling-up"
    (let ((s (%screen-with-scrollback 3)))
      (cl-tmux/commands::copy-mode-enter s :exit-on-bottom t)
      (cl-tmux/commands::copy-mode-scroll s 100)      ; scroll up to oldest
      (expect (screen-copy-mode-p s) :to-be-truthy)))

  ;; Outside copy mode, copy-mode-scroll does nothing: offset stays put and the
  ;; call returns NIL.
  (it "copy-mode-scroll-noop-when-not-in-copy-mode"
    (let ((s (make-screen 20 5)))
      (setf (screen-scrollback s) (list (make-array 0) (make-array 0)))
      (expect (screen-copy-mode-p s) :to-be-falsy)
      (expect (null (cl-tmux/commands::copy-mode-scroll s 100)))
      (expect (= 0 (screen-copy-offset s)))))

  ;;; ── select-window-by-number ─────────────────────────────────────────────────

  ;; select-window-by-number activates the Nth (0-based) window.
  (it "select-window-by-number-selects-nth-window"
    (with-fake-session (s :nwindows 3)
      (let ((w0 (first  (session-windows s)))
            (w2 (third  (session-windows s))))
        (expect (eq w0 (session-active-window s)))
        (select-window-by-number s 2)
        (expect (eq w2 (session-active-window s)))
        (select-window-by-number s 0)
        (expect (eq w0 (session-active-window s))))))

  ;; select-window-by-number with an out-of-range index leaves the active window unchanged.
  (it "select-window-by-number-out-of-range-is-noop"
    (let* ((s      (make-fake-session :nwindows 2))
           (before (session-active-window s)))
      (select-window-by-number s 99)
      (expect (eq before (session-active-window s)))))

  ;; After killing a middle window, select-window-by-number still finds the
  ;; window by its stored id, not by list position.
  (it "select-window-by-id-stable-after-kill"
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
      (expect (eq w2 (session-active-window sess)))))

  ;;; ── rename-session ──────────────────────────────────────────────────────────

  ;; rename-session sets the session's name to the supplied string.
  (it "rename-session-changes-session-name"
    (let ((sess (make-session :id 1 :name "old" :windows nil)))
      (cl-tmux/commands:rename-session sess "new")
      (expect (string= "new" (session-name sess)))))

  ;; rename-session with "" or NIL is a no-op: the session name remains unchanged.
  (it "rename-session-ignores-invalid-names-table"
    (dolist (row '((""  "empty string is a no-op")
                   (nil "nil is a no-op")))
      (destructuring-bind (new-name desc) row
        (declare (ignore desc))
        (let ((sess (make-session :id 1 :name "keep" :windows nil)))
          (cl-tmux/commands:rename-session sess new-name)
          (expect (string= "keep" (session-name sess))))))))
