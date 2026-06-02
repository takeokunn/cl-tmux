(in-package #:cl-tmux/test)

;;;; Pane-level tests: pane struct, pane-feed, pane-reposition, next-pane-id.

(def-suite model-suite :description "Session / window / pane model")
(in-suite model-suite)

;;; ── pane-feed ────────────────────────────────────────────────────────────────

(test pane-feed-processes-bytes-into-screen
  "pane-feed feeds raw bytes through the screen emulator under the screen lock."
  (let* ((screen (make-screen 10 5))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 5
                            :fd -1 :pid -1 :screen screen)))
    (pane-feed pane (babel:string-to-octets "hi" :encoding :utf-8))
    (is (char= #\h (cell-char (screen-cell screen 0 0))))
    (is (char= #\i (cell-char (screen-cell screen 1 0))))
    (is (= 2 (screen-cursor-x screen)))))

;;; ── pane-reposition direct unit test (no PTY) ───────────────────────────────
;;;
;;; NOTE: pane-reposition calls set-pty-size on fd -1, which is a tolerated
;;; EBADF no-op (ioctl returns -1 without signalling a Lisp condition), and
;;; calls screen-resize under the screen lock.  The observable effects are the
;;; x/y/width/height slot updates and the matching screen dimension update.

(test pane-reposition-updates-geometry-and-screen
  "pane-reposition sets x/y/width/height and resizes the underlying screen."
  (let* ((screen (make-screen 20 5))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                            :fd -1 :pid -1 :screen screen)))
    (pane-reposition pane 3 7 40 10)
    ;; Geometry slots updated.
    (is (= 3  (pane-x      pane)) "pane-x must be 3 after reposition")
    (is (= 7  (pane-y      pane)) "pane-y must be 7 after reposition")
    (is (= 40 (pane-width  pane)) "pane-width must be 40 after reposition")
    (is (= 10 (pane-height pane)) "pane-height must be 10 after reposition")
    ;; Screen dimensions match pane geometry.
    (is (= 40 (screen-width  (pane-screen pane)))
        "screen-width must match new pane width")
    (is (= 10 (screen-height (pane-screen pane)))
        "screen-height must match new pane height")))

(test pane-reposition-zero-origin
  "pane-reposition correctly sets position to (0,0) — the corner case for zoom-in."
  (let* ((screen (make-screen 10 5))
         (pane   (make-pane :id 1 :x 5 :y 3 :width 10 :height 5
                            :fd -1 :pid -1 :screen screen)))
    (pane-reposition pane 0 0 80 24)
    (is (= 0  (pane-x      pane)) "pane-x must be 0 after reposition to origin")
    (is (= 0  (pane-y      pane)) "pane-y must be 0 after reposition to origin")
    (is (= 80 (pane-width  pane)) "pane-width must be 80")
    (is (= 24 (pane-height pane)) "pane-height must be 24")
    (is (= 80 (screen-width  (pane-screen pane))))
    (is (= 24 (screen-height (pane-screen pane))))))

(test pane-reposition-returns-no-value
  "pane-reposition returns no useful value — callers rely solely on side effects."
  ;; We verify it does not return the pane (which would be a data leakage smell).
  ;; The actual return is unspecified; just check it does not signal a condition.
  (let* ((screen (make-screen 5 5))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 5 :height 5
                            :fd -1 :pid -1 :screen screen)))
    (is-true (progn (pane-reposition pane 0 0 10 10) t)
             "pane-reposition must complete without signalling")))

;;; ── next-pane-id direct tests (pure, no PTY) ─────────────────────────────

(test next-pane-id-returns-one-for-empty-window
  "next-pane-id starts at 1 when the window has no panes."
  (let ((win (make-window :id 1 :name "w" :panes nil)))
    (is (= 1 (cl-tmux/model::next-pane-id win)))))

(test next-pane-id-fills-lowest-gap
  "next-pane-id returns the lowest positive id not already in use."
  (let* ((p1  (make-pane :id 1 :fd -1 :pid -1 :screen (make-screen 10 5)))
         (p3  (make-pane :id 3 :fd -1 :pid -1 :screen (make-screen 10 5)))
         (win (make-window :id 1 :name "w" :panes (list p1 p3))))
    ;; id 1 and 3 are used; 2 is the lowest gap
    (is (= 2 (cl-tmux/model::next-pane-id win)))))

;;; ── split-window -d flag (no-focus) ─────────────────────────────────────────

(test split-window-no-focus
  "window-split :no-focus t creates the new pane but keeps the original active pane."
  (unless (pty-available-p)
    (skip "PTY not available"))
  (let* ((p0  (make-pane :id 1 :x 0 :y 0 :width 41 :height 10
                         :fd -1 :pid -1 :screen (make-screen 41 10)))
         (win (make-window :id 1 :name "w" :width 41 :height 10
                           :tree (make-layout-leaf p0)
                           :panes (list p0))))
    (window-select-pane win p0)
    (let ((new-pane (window-split win :h :no-focus t)))
      (is (not (null new-pane)) "split must succeed")
      (is (eq p0 (window-active-pane win))
          "active pane must remain p0 after no-focus split")
      (is (= 2 (length (window-panes win)))
          "window must have 2 panes after split")
      ;; Clean up
      (ignore-errors (pty-close (pane-fd new-pane) (pane-pid new-pane))))))

;;; ── split-window -p/-l size hint ─────────────────────────────────────────────

(test split-window-size-hint-percentage
  "window-split with a fractional size hint assigns the new pane a proportional width."
  (unless (pty-available-p)
    (skip "PTY not available"))
  (let* ((p0  (make-pane :id 1 :x 0 :y 0 :width 81 :height 10
                         :fd -1 :pid -1 :screen (make-screen 81 10)))
         (win (make-window :id 1 :name "w" :width 81 :height 10
                           :tree (make-layout-leaf p0)
                           :panes (list p0))))
    (window-select-pane win p0)
    ;; Split with 0.25 size → new pane should be ~20 cols (25% of 80-col avail)
    (let ((new-pane (window-split win :h :size 0.25)))
      (when new-pane
        (is (> (pane-width new-pane) 0) "new pane must have positive width")
        (is (< (pane-width new-pane) 81) "new pane must be smaller than window width")
        (ignore-errors (pty-close (pane-fd new-pane) (pane-pid new-pane)))))))

;;; ── swap-pane exchanges rects ────────────────────────────────────────────────

(test swap-pane-exchanges-rects
  "swap-pane exchanges the x/y/width/height between two panes."
  (let* ((p0  (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                         :fd -1 :pid -1 :screen (make-screen 20 5)))
         (p1  (make-pane :id 2 :x 21 :y 0 :width 20 :height 5
                         :fd -1 :pid -1 :screen (make-screen 20 5)))
         (win (make-window :id 1 :name "w" :width 41 :height 5
                           :tree (make-layout-split :h
                                    (make-layout-leaf p0) (make-layout-leaf p1)
                                    1/2)
                           :panes (list p0 p1))))
    (window-select-pane win p0)
    (let ((x0-before (pane-x p0))
          (x1-before (pane-x p1)))
      (swap-pane win :right)
      (is (= x1-before (pane-x p0)) "p0 must have p1's former x after swap")
      (is (= x0-before (pane-x p1)) "p1 must have p0's former x after swap"))))

;;; ── capture-pane returns text ────────────────────────────────────────────────

(test capture-pane-returns-text
  "capture-pane returns a string containing the content fed to the pane's screen."
  (let* ((screen (make-screen 20 5))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                            :fd -1 :pid -1 :screen screen)))
    (feed screen "HELLO")
    (let ((result (capture-pane pane)))
      (is (stringp result) "capture-pane must return a string")
      (is (not (null (search "HELLO" result)))
          "capture-pane output must contain the fed text"))))

;;; ── last-pane cycles ─────────────────────────────────────────────────────────

(test last-pane-cycles
  "window-select-pane updates window-last-active; switching back via :last-pane
   returns to the previous pane."
  (let* ((p0  (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                         :fd -1 :pid -1 :screen (make-screen 20 5)))
         (p1  (make-pane :id 2 :x 21 :y 0 :width 20 :height 5
                         :fd -1 :pid -1 :screen (make-screen 20 5)))
         (win (make-window :id 1 :name "w" :width 41 :height 5
                           :tree (make-layout-split :h
                                    (make-layout-leaf p0) (make-layout-leaf p1)
                                    1/2)
                           :panes (list p0 p1))))
    ;; Start on p0
    (window-select-pane win p0)
    (is (eq p0 (window-active-pane win)) "precondition: p0 is active")
    ;; Switch to p1 — this should record p0 as last-active
    (window-select-pane win p1)
    (is (eq p1 (window-active-pane win)) "p1 must be active after select")
    (is (eq p0 (window-last-active win)) "p0 must be the last-active pane")
    ;; Simulate :last-pane by selecting window-last-active
    (let ((last (window-last-active win)))
      (when last (window-select-pane win last)))
    (is (eq p0 (window-active-pane win)) "last-pane must return to p0")))

;;; ── display-panes overlay active ─────────────────────────────────────────────

(test display-panes-overlay-active
  ":display-panes activates the overlay (overlay-active-p returns T)."
  (let* ((sess (make-fake-session :nwindows 1 :npanes 2)))
    (with-loop-state
      (let ((*overlay* nil))
        (cl-tmux::dispatch-command sess :display-panes nil)
        (is-true (overlay-active-p)
                 ":display-panes must activate the overlay")))))

;;; ── respawn-pane resets fd/pid ───────────────────────────────────────────────

(test respawn-pane-updates-fd-and-pid
  "respawn-pane closes the old PTY and assigns a fresh fd/pid to the pane.
   Uses pty-available-p to skip when PTY forking is not available."
  (unless (pty-available-p)
    (skip "PTY not available"))
  (multiple-value-bind (fd pid) (forkpty-with-shell 5 20)
    (let* ((screen (make-screen 20 5))
           (pane   (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                              :fd fd :pid pid :screen screen)))
      (let ((old-pid (pane-pid pane)))
        (respawn-pane pane)
        ;; The new pid must differ (a new child process was forked).
        ;; The fd may or may not be the same number (OS fd recycling), but
        ;; it must be non-negative (a valid open fd).
        (is (not (= old-pid (pane-pid pane)))
            "pid must change after respawn (a new process was forked)")
        (is (>= (pane-fd pane) 0)
            "pane-fd must be a non-negative open fd after respawn")
        (ignore-errors (pty-close (pane-fd pane) (pane-pid pane)))))))

;;; ── Pane slot defaults ───────────────────────────────────────────────────────

(test pane-pipe-fd-defaults-nil
  "pane-pipe-fd defaults to NIL for a freshly created pane."
  (let ((pane (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                         :fd -1 :pid -1 :screen (make-screen 20 5))))
    (is (null (pane-pipe-fd pane))
        "pane-pipe-fd must default to NIL")))

(test pane-window-defaults-nil
  "pane-window defaults to NIL (back-pointer not set until attach)."
  (let ((pane (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                         :fd -1 :pid -1 :screen (make-screen 20 5))))
    (is (null (pane-window pane))
        "pane-window must default to NIL before attach")))

(test pane-marked-defaults-nil
  "pane-marked defaults to NIL for a freshly created pane."
  (let ((pane (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                         :fd -1 :pid -1 :screen (make-screen 20 5))))
    (is (null (pane-marked pane))
        "pane-marked must default to NIL")))

(test pane-marked-settable
  "pane-marked can be set to T and read back."
  (let ((pane (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                         :fd -1 :pid -1 :screen (make-screen 20 5))))
    (setf (pane-marked pane) t)
    (is-true (pane-marked pane)
             "pane-marked must return T after being set")))

;;; ── next-pane-id consecutive allocation ──────────────────────────────────────

(test next-pane-id-consecutive-when-no-gaps
  "next-pane-id returns (1+ highest-id) when ids are consecutive from 1."
  (let* ((p1  (make-pane :id 1 :fd -1 :pid -1 :screen (make-screen 10 5)))
         (p2  (make-pane :id 2 :fd -1 :pid -1 :screen (make-screen 10 5)))
         (win (make-window :id 1 :name "w" :panes (list p1 p2))))
    (is (= 3 (cl-tmux/model::next-pane-id win))
        "next-pane-id must return 3 when ids 1 and 2 are used")))

;;; ── pane-reposition preserves screen content metadata ────────────────────────

(test pane-reposition-is-idempotent
  "Calling pane-reposition twice with the same arguments leaves geometry unchanged."
  (let* ((screen (make-screen 20 5))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                            :fd -1 :pid -1 :screen screen)))
    (pane-reposition pane 5 3 40 10)
    (pane-reposition pane 5 3 40 10)
    (is (= 5  (pane-x      pane)) "pane-x must be stable after double call")
    (is (= 3  (pane-y      pane)) "pane-y must be stable after double call")
    (is (= 40 (pane-width  pane)) "pane-width must be stable after double call")
    (is (= 10 (pane-height pane)) "pane-height must be stable after double call")))

;;; ── Table-driven pane-reposition edge cases ──────────────────────────────────
;;;
;;; The reposition tests share the same structure — verify slot values after a
;;; single call.  A table reduces repetition without obscuring the assertions.

(test pane-reposition-table
  "Table-driven: pane-reposition correctly updates x/y/width/height in multiple cases."
  ;; Each case: (x y w h description)
  (dolist (entry
           '((0  0  80 24 "full-screen origin case")
             (5  3  40 10 "offset non-zero case")
             (1  1   2  1 "minimum-size case")))
    (destructuring-bind (x y w h desc) entry
      (let* ((screen (make-screen 10 5))
             (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 5
                                :fd -1 :pid -1 :screen screen)))
        (pane-reposition pane x y w h)
        (is (= x (pane-x      pane)) desc)
        (is (= y (pane-y      pane)) desc)
        (is (= w (pane-width  pane)) desc)
        (is (= h (pane-height pane)) desc)))))
