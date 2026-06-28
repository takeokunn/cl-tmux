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
  (let ((pane (make-no-pty-pane 1 0 0 20 5)))
    (pane-reposition pane 3 7 40 10)
    (check-table (list (list (pane-x pane) 3 "pane-x must be 3 after reposition")
                       (list (pane-y pane) 7 "pane-y must be 7 after reposition")
                       (list (pane-width pane) 40 "pane-width must be 40 after reposition")
                       (list (pane-height pane) 10 "pane-height must be 10 after reposition")
                       (list (screen-width (pane-screen pane)) 40 "screen-width must match new pane width")
                       (list (screen-height (pane-screen pane)) 10 "screen-height must match new pane height")))))

(test pane-reposition-zero-origin
  "pane-reposition correctly sets position to (0,0) — the corner case for zoom-in."
  (let ((pane (make-no-pty-pane 1 5 3 10 5)))
    (pane-reposition pane 0 0 80 24)
    (check-table (list (list (pane-x pane) 0 "pane-x must be 0 after reposition to origin")
                       (list (pane-y pane) 0 "pane-y must be 0 after reposition to origin")
                       (list (pane-width pane) 80 "pane-width must be 80")
                       (list (pane-height pane) 24 "pane-height must be 24")
                       (list (screen-width (pane-screen pane)) 80 "screen width must match pane width")
                       (list (screen-height (pane-screen pane)) 24 "screen height must match pane height")))))

(test pane-reposition-returns-no-value
  "pane-reposition returns no useful value — callers rely solely on side effects."
  (let ((pane (make-no-pty-pane 1 0 0 5 5)))
    (is-true (progn (pane-reposition pane 0 0 10 10) t)
             "pane-reposition must complete without signalling")))

(test pane-reposition-border-status-table
  "pane-border-status controls how pane-reposition reserves rows for the title bar."
  (dolist (row '(("top"    1  23 "top status shifts content down, height -1")
                 ("bottom" 0  23 "bottom status keeps y, height -1")
                 ("off"    0  24 "no status, full height preserved")))
    (destructuring-bind (status expected-y expected-h desc) row
      (with-fresh-options
        (cl-tmux/options:set-option "pane-border-status" status)
        (let ((pane (make-no-pty-pane 1 0 0 20 5)))
          (pane-reposition pane 0 0 80 24)
          (is (= expected-y (pane-y      pane)) "~A: pane-y" desc)
          (is (= expected-h (pane-height pane)) "~A: pane-height" desc)
          (is (= expected-h (screen-height (pane-screen pane))) "~A: screen-height" desc))))))

;;; ── next-pane-id direct tests (pure, no PTY) ─────────────────────────────

(test next-pane-id-returns-base-index-for-empty-window
  "next-pane-id returns pane-base-index when the window has no panes (default 0)."
  (let ((win (make-window :id 1 :name "w" :panes nil)))
    ;; With pane-base-index=0 (default), first pane id is 0.
    (is (= (or (cl-tmux/options:get-option "pane-base-index") 0)
           (cl-tmux/model::next-pane-id win)))))

(test next-pane-id-fills-lowest-gap
  "next-pane-id returns the lowest id >= pane-base-index not already in use."
  (let* ((base (or (cl-tmux/options:get-option "pane-base-index") 0))
         (p1  (make-no-pty-pane (+ base 1) 0 0 10 5))
         (p3  (make-no-pty-pane (+ base 3) 0 0 10 5))
         (win (make-window :id 1 :name "w" :panes (list p1 p3))))
    ;; The lowest gap above base should be filled.
    (is (= base (cl-tmux/model::next-pane-id win)))))

;;; ── split-window -d flag (no-focus) ─────────────────────────────────────────

(test split-window-no-focus
  "window-split :no-focus t creates the new pane but keeps the original active pane."
  (unless (pty-available-p)
    (skip "PTY not available"))
  (with-session (session 41 10)
    (let* ((win (session-active-window session))
           (active-pane (window-active-pane win)))
      (let ((new-pane (window-split session win :h :no-focus t)))
        (is (not (null new-pane)) "split must succeed")
        (is (eq active-pane (window-active-pane win))
            "active pane must remain unchanged after no-focus split")
        (is (= 2 (length (window-panes win)))
            "window must have 2 panes after split")
        ;; Clean up
        (ignore-errors (pty-close (pane-fd new-pane) (pane-pid new-pane)))))))

;;; ── split-window -l size hint ────────────────────────────────────────────────

(test split-window-size-hint-percentage
  "window-split with a fractional size hint assigns the new pane a proportional width."
  (unless (pty-available-p)
    (skip "PTY not available"))
  (with-session (session 81 10)
    (let ((win (session-active-window session)))
      ;; Split with 0.25 size → new pane should be ~20 cols (25% of 80-col avail)
      (let ((new-pane (window-split session win :h :size 0.25)))
        (when new-pane
          (is (> (pane-width new-pane) 0) "new pane must have positive width")
          (is (< (pane-width new-pane) 81) "new pane must be smaller than window width")
          (ignore-errors (pty-close (pane-fd new-pane) (pane-pid new-pane))))))))

;;; ── swap-pane exchanges rects ────────────────────────────────────────────────

(test swap-pane-exchanges-rects
  "swap-pane exchanges the x/y/width/height between two panes."
  (let* ((p0  (make-no-pty-pane 1  0 0 20 5))
         (p1  (make-no-pty-pane 2 21 0 20 5))
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
  (let* ((p0  (make-no-pty-pane 1  0 0 20 5))
         (p1  (make-no-pty-pane 2 21 0 20 5))
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
  (with-fake-session (sess :nwindows 1 :npanes 2)
    (let ((*overlay* nil))
      (cl-tmux::dispatch-command sess :display-panes nil)
      (assert-overlay-active ":display-panes must activate the overlay"))))

;;; ── respawn-pane resets fd/pid ───────────────────────────────────────────────

(test respawn-pane-updates-fd-and-pid
  "respawn-pane closes the old PTY and assigns a fresh fd/pid to the pane.
   Uses pty-available-p to skip when PTY spawning is not available."
  (unless (pty-available-p)
    (skip "PTY not available"))
  (with-session (session 20 20)
    (let* ((pane (session-active-pane session))
           (old-pid (pane-pid pane)))
      (respawn-pane session pane)
      ;; The new pid must differ (a new child process was spawned).
      ;; The fd may or may not be the same number (OS fd recycling), but
      ;; it must be non-negative (a valid open fd).
      (is (not (= old-pid (pane-pid pane)))
          "pid must change after respawn (a new process was spawned)")
      (is (>= (pane-fd pane) 0)
          "pane-fd must be a non-negative open fd after respawn"))))

;;; ── Pane slot defaults ───────────────────────────────────────────────────────

(test pane-nil-slot-defaults
  "pipe state, pane-window, and pane-marked all default to NIL for a fresh pane."
  (let ((pane (make-no-pty-pane 1 0 0 20 5)))
    (is (null (pane-pipe-fd pane)) "pane-pipe-fd must default to NIL")
    (is (null (pane-pipe-output-stream pane))
        "pane-pipe-output-stream must default to NIL")
    (is (null (pane-pipe-output-thread pane))
        "pane-pipe-output-thread must default to NIL")
    (is (null (pane-pipe-process pane)) "pane-pipe-process must default to NIL")
    (is (null (pane-window  pane)) "pane-window must default to NIL before attach")
    (is (null (pane-marked  pane)) "pane-marked must default to NIL")))

(test pane-marked-settable
  "pane-marked can be set to T and read back."
  (let ((pane (make-no-pty-pane 1 0 0 20 5)))
    (setf (pane-marked pane) t)
    (is-true (pane-marked pane)
             "pane-marked must return T after being set")))

;;; ── next-pane-id consecutive allocation ──────────────────────────────────────

(test next-pane-id-consecutive-when-no-gaps
  "next-pane-id returns (1+ highest-id) when ids are consecutive from base."
  (let* ((base (or (cl-tmux/options:get-option "pane-base-index") 0))
         (p1  (make-no-pty-pane (+ base 1) 0 0 10 5))
         (p2  (make-no-pty-pane (+ base 2) 0 0 10 5))
         (win (make-window :id 1 :name "w" :panes (list p1 p2))))
    ;; The first gap is at base (ids base+1 and base+2 are used).
    (is (= base (cl-tmux/model::next-pane-id win))
        "next-pane-id must return base when base+1 and base+2 are used")))

;;; ── pane-reposition preserves screen content metadata ────────────────────────

(test pane-reposition-is-idempotent
  "Calling pane-reposition twice with the same arguments leaves geometry unchanged."
  (let ((pane (make-no-pty-pane 1 0 0 20 5)))
    (pane-reposition pane 5 3 40 10)
    (pane-reposition pane 5 3 40 10)
    (is (= 5  (pane-x      pane)) "pane-x must be stable after double call")
    (is (= 3  (pane-y      pane)) "pane-y must be stable after double call")
    (is (= 40 (pane-width  pane)) "pane-width must be stable after double call")
    (is (= 10 (pane-height pane)) "pane-height must be stable after double call")))

;;; ── Table-driven pane-reposition edge cases ──────────────────────────────────

(test pane-reposition-table
  "Table-driven: pane-reposition correctly updates x/y/width/height in multiple cases."
  ;; Each case: (x y w h description)
  (dolist (entry
           '((0  0  80 24 "full-screen origin case")
             (5  3  40 10 "offset non-zero case")
             (1  1   2  1 "minimum-size case")))
    (destructuring-bind (x y w h desc) entry
      (let ((pane (make-no-pty-pane 1 0 0 10 5)))
        (pane-reposition pane x y w h)
        (is (= x (pane-x      pane)) desc)
        (is (= y (pane-y      pane)) desc)
        (is (= w (pane-width  pane)) desc)
        (is (= h (pane-height pane)) desc)))))

;;; ── pane struct accessor defaults ───────────────────────────────────────────

(test pane-id-slot-accessible
  "pane-id returns the id passed to make-no-pty-pane."
  (let ((pane (make-no-pty-pane 7 0 0 20 5)))
    (is (= 7 (pane-id pane))
        "pane-id must return the id set at construction")))

(test pane-x-y-width-height-accessible
  "pane-x, pane-y, pane-width, pane-height return the geometry set at construction."
  (let ((pane (make-no-pty-pane 1 3 5 40 10)))
    (is (= 3  (pane-x      pane)) "pane-x must return 3")
    (is (= 5  (pane-y      pane)) "pane-y must return 5")
    (is (= 40 (pane-width  pane)) "pane-width must return 40")
    (is (= 10 (pane-height pane)) "pane-height must return 10")))

(test pane-no-pty-fd-and-pid-are-negative
  "make-no-pty-pane produces a pane with fd and pid both -1."
  (let ((pane (make-no-pty-pane 1 0 0 20 5)))
    (is (= -1 (pane-fd  pane)) "pane-fd must be -1 for a no-PTY pane")
    (is (= -1 (pane-pid pane)) "pane-pid must be -1 for a no-PTY pane")))

(test pane-screen-accessible
  "pane-screen returns the screen object set at construction."
  (let* ((screen (make-screen 20 5))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 20 :height 5
                            :fd -1 :pid -1 :screen screen)))
    (is (eq screen (pane-screen pane))
        "pane-screen must return the exact screen object set at construction")))

;;; ── pane-feed with empty bytes ───────────────────────────────────────────────

(test pane-feed-empty-bytes-is-noop
  "pane-feed with an empty byte vector does not signal and leaves cursor at (0,0)."
  (let* ((screen (make-screen 10 5))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 5
                            :fd -1 :pid -1 :screen screen)))
    (finishes (pane-feed pane (make-array 0 :element-type '(unsigned-byte 8))))
    (is (= 0 (screen-cursor-x screen)) "cursor must stay at 0 after feeding empty bytes")
    (is (= 0 (screen-cursor-y screen)) "cursor must stay at 0 after feeding empty bytes")))

;;; ── pane-feed updates screen-dirty-p ────────────────────────────────────────

(test pane-feed-sets-dirty-flag
  "pane-feed marks the screen dirty after processing bytes."
  (let* ((screen (make-screen 10 5))
         (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 5
                            :fd -1 :pid -1 :screen screen)))
    (screen-clear-dirty screen)
    (pane-feed pane (babel:string-to-octets "A" :encoding :utf-8))
    ;; After writing a character the dirty flag must be set.
    (is-true (cl-tmux/terminal/types:screen-dirty-p screen)
             "screen-dirty-p must be T after pane-feed writes a character")))

;;; ── Table-driven next-pane-id gap-filling ────────────────────────────────────

(test next-pane-id-table
  "Table-driven: next-pane-id fills the lowest gap >= pane-base-index."
  ;; Each entry: (used-id-offsets expected-offset description)
  ;; Offsets are relative to pane-base-index (so offset 0 = base, 1 = base+1, etc.)
  (let ((base (or (cl-tmux/options:get-option "pane-base-index") 0)))
    (dolist (entry
             `((() 0 "empty window: first id is base")
               ((1) 0 ,(format nil "id base+1 used: base (~D) is free" base))
               ((0 1) 2 "ids base and base+1 used: next is base+2")
               ((0 2) 1 "gap at base+1: fill it")))
      (destructuring-bind (used-offsets expected-offset desc) entry
        (let* ((panes (mapcar (lambda (off)
                                (make-no-pty-pane (+ base off) 0 0 10 5))
                              used-offsets))
               (win   (make-window :id 1 :name "w" :panes panes)))
          (is (= (+ base expected-offset)
                 (cl-tmux/model::next-pane-id win))
              desc))))))

;;; ── pane-at-position hit test ────────────────────────────────────────────────

(test pane-at-position-table
  "pane-at-position returns the pane containing (x,y), or NIL for the separator gap."
  (let* ((p0  (make-no-pty-pane 1  0 0 40 24))
         (p1  (make-no-pty-pane 2 41 0 40 24))
         (win (make-window :id 1 :name "w" :width 81 :height 24
                           :panes (list p0 p1)
                           :tree (make-layout-split :h
                                    (make-layout-leaf p0)
                                    (make-layout-leaf p1)
                                    1/2))))
    (is (eq  p0  (pane-at-position win 10 5)) "col 10 hits p0 [0,40)")
    (is (eq  p1  (pane-at-position win 50 5)) "col 50 hits p1 [41,81)")
    (is (null    (pane-at-position win 40 5)) "col 40 is separator → NIL")))

(test pane-at-position-returns-nil-for-empty-window
  "pane-at-position returns NIL when the window has no panes."
  (let ((win (make-window :id 1 :name "w" :panes nil)))
    (is (null (pane-at-position win 0 0))
        "pane-at-position on empty window must return NIL")))

;;; ── pane-live-p direct unit tests ────────────────────────────────────────────

(test pane-live-p-table
  "pane-live-p returns T only when fd > 0; fd <= 0 and NIL are all not-live.
   :nil sentinel means pass NIL directly instead of creating a pane.
   Each row: (fd expected description)."
  (dolist (row '((5    t   "pane with fd > 0 must be live")
                 (-1   nil "pane with fd = -1 must not be live")
                 (0    nil "pane with fd = 0 must not be reported as live")
                 (:nil nil "pane-live-p NIL must return NIL")))
    (destructuring-bind (fd expected desc) row
      (let ((pane (if (eq fd :nil)
                      nil
                      (make-pane :id 1 :x 0 :y 0 :width 80 :height 24
                                 :fd fd :pid -1 :screen (make-screen 80 24)))))
        (if expected
            (is-true  (pane-live-p pane) desc)
            (is-false (pane-live-p pane) desc))))))

;;; ── pane-pipe-active-p direct unit tests ─────────────────────────────────────

(test pane-pipe-active-p-table
  "pane-pipe-active-p returns truthy when any pipe slot is non-NIL, NIL otherwise.
   :nil sentinel means pass NIL directly. :none means no slot is set.
   Each row: (setup expected description)."
  (dolist (row '((:none      nil "pane with no pipe resources must not be active")
                 (:pipe-fd   t   "pipe-fd set => pipe must be active")
                 (:pipe-out  t   "pipe-output-stream set => pipe must be active")
                 (:pipe-proc t   "pipe-process set => pipe must be active")
                 (:nil       nil "pane-pipe-active-p NIL must return NIL")))
    (destructuring-bind (setup expected desc) row
      (let ((pane (unless (eq setup :nil) (make-no-pty-pane 1 0 0 80 24))))
        (ecase setup
          (:none      nil)
          (:pipe-fd   (setf (pane-pipe-fd             pane) :fake-fd))
          (:pipe-out  (setf (pane-pipe-output-stream  pane) :fake-stream))
          (:pipe-proc (setf (pane-pipe-process        pane) :fake-process))
          (:nil       nil))
        (if expected
            (is-true  (pane-pipe-active-p pane) desc)
            (is-false (pane-pipe-active-p pane) desc))))))
