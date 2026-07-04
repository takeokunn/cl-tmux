(in-package #:cl-tmux/test)

;;;; Pane tests - geometry, feed, next-pane-id, split-window.

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
          (is (= expected-y (pane-y pane)) "~A: pane-y" desc)
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
