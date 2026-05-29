(in-package #:cl-tmux/test)

;;;; Pane resize / copy-mode-scroll / kill-pane command logic (src/commands.lisp).

(def-suite commands-suite :description "Pane resize / copy-mode-scroll / kill-pane command logic (src/commands.lisp)")
(in-suite commands-suite)

;;; ── Local fixtures (no PTY: fd -1, pid -1) ──────────────────────────────────
;;;
;;; A vertically-split window: two side-by-side panes laid out exactly the way
;;; divide-window :vertical produces them.  41 cols / 2 panes => one separator
;;; column, each pane 20 wide, the second pane's x sitting one past the first.

(defun %make-split-window (layout w h pw0 ph0 px1 py1 pw1 ph1)
  "Build a 2-pane WINDOW (fd -1, no PTY) with an explicit LAYOUT orientation.
   Pane 0 sits at the origin; pane 1 at PX1,PY1.  The first pane is active."
  (let* ((p0  (make-pane :id 1 :x 0   :y 0   :width pw0 :height ph0
                         :fd -1 :pid -1 :screen (make-screen pw0 ph0)))
         (p1  (make-pane :id 2 :x px1 :y py1 :width pw1 :height ph1
                         :fd -1 :pid -1 :screen (make-screen pw1 ph1)))
         (win (make-window :id 1 :name "w" :width w :height h
                           :panes (list p0 p1) :layout layout)))
    (window-select-pane win p0)
    win))

(defun %vsplit-window (&optional (each 20))
  "Vertical split: two EACH-wide panes, one separator column between them."
  (let ((w (+ each 1 each)))
    (%make-split-window :vertical w 5
                        each 5            ; pane 0
                        (+ each 1) 0      ; pane 1 origin (x = each+1)
                        each 5)))         ; pane 1 size

(defun %hsplit-window (&optional (each 10))
  "Horizontal split: two EACH-tall panes, one separator row between them."
  (let ((h (+ each 1 each)))
    (%make-split-window :horizontal 20 h
                        20 each           ; pane 0
                        0 (+ each 1)      ; pane 1 origin (y = each+1)
                        20 each)))        ; pane 1 size

;;; ── resize-pane: vertical split ─────────────────────────────────────────────

(test resize-vertical-right-grows-active-shrinks-neighbour
  "On a vertical split, :right grows the active (left) pane and shrinks its
   right neighbour by the same amount; the neighbour's x slides to sit one
   column past the grown active pane."
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
    ;; direction :left => d = -delta; the active (p1) shrinks, neighbour (p0) grows.
    (is (= 15 (pane-width p1)) "active shrinks toward the left")
    (is (= 25 (pane-width p0)) "previous neighbour grows")))

(test resize-vertical-last-pane-right-is-no-op
  "The rightmost pane has no right neighbour, so :right resolves the neighbour
   to itself and changes nothing."
  (let* ((win (%vsplit-window 20))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win))))
    (window-select-pane win p1)
    (resize-pane win :right 5)
    (is (= 20 (pane-width p0)) "left pane unchanged")
    (is (= 20 (pane-width p1)) "rightmost pane unchanged")
    (is (= 21 (pane-x p1)) "rightmost pane x unchanged")))

(test resize-vertical-min-size-guard-rejects-shrink-below-two
  "The min-size guard is all-or-nothing: a delta that would shrink either pane
   to 2 columns or fewer leaves both panes untouched."
  (let* ((win (%vsplit-window 5))          ; both panes only 5 wide
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win))))
    (resize-pane win :right 5)             ; neighbour would become 0 (<= 2)
    (is (= 5 (pane-width p0)) "active pane width unchanged (guard rejected)")
    (is (= 5 (pane-width p1)) "neighbour width unchanged (guard rejected)")
    (is (= 6 (pane-x p1)) "neighbour x unchanged (guard rejected)")))

(test resize-wrong-direction-is-no-op
  "A direction that doesn't match the split orientation does nothing and
   returns NIL (vertical split asked to resize :up)."
  (let* ((win (%vsplit-window 20))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win))))
    (is (null (resize-pane win :up 5)) "off-axis direction returns NIL")
    (is (= 20 (pane-width p0)))
    (is (= 20 (pane-width p1)))))

;;; ── resize-pane: horizontal split ───────────────────────────────────────────

(test resize-horizontal-down-grows-active-shrinks-lower
  "On a horizontal split, :down grows the active (upper) pane and shrinks the
   lower neighbour; the neighbour's y slides one row past the grown pane."
  (let* ((win (%hsplit-window 10))
         (p0  (first  (window-panes win)))
         (p1  (second (window-panes win))))
    (is (eq p0 (resize-pane win :down 5)))
    (is (= 15 (pane-height p0)) "upper pane grows by amount")
    (is (= 5  (pane-height p1)) "lower pane shrinks by amount")
    (is (= 16 (pane-y p1))
        "lower pane y = upper.y + upper.height + 1 (separator row)")))

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
         (win (make-window :id 1 :name "w" :width 20 :height 5 :panes (list p0)))
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
         (w1  (make-window :id 1 :name "a" :width 20 :height 5 :panes (list p0)))
         (w2  (make-window :id 2 :name "b" :width 20 :height 5
                           :panes (list (make-pane :id 1 :fd -1 :pid -1
                                                   :screen (make-screen 20 5)))))
         (sess (make-session :id 1 :name "0" :windows (list w1 w2))))
    (window-select-pane w1 p0)
    (session-select-window sess w1)
    (is (null (kill-pane sess)) "session survives on the other window")
    (is (equal (list w2) (session-windows sess)) "emptied window removed")
    (is (eq w2 (session-active-window sess)) "active window switches to survivor")))
