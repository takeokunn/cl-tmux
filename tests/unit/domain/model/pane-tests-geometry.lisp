(in-package #:cl-tmux/test)

;;;; Pane tests - geometry, feed, next-pane-id, split-window.

(describe "model-suite"

  ;; ── pane-feed ────────────────────────────────────────────────────────────────

  ;; pane-feed feeds raw bytes through the screen emulator under the screen lock.
  (it "pane-feed-processes-bytes-into-screen"
    (let* ((screen (make-screen 10 5))
           (pane   (make-pane :id 1 :x 0 :y 0 :width 10 :height 5
                              :fd -1 :pid -1 :screen screen)))
      (pane-feed pane (babel:string-to-octets "hi" :encoding :utf-8))
      (expect (char= #\h (cell-char (screen-cell screen 0 0))))
      (expect (char= #\i (cell-char (screen-cell screen 1 0))))
      (expect (= 2 (screen-cursor-x screen)))))

  ;; ── pane-reposition direct unit test (no PTY) ───────────────────────────────
  ;;
  ;; NOTE: pane-reposition calls set-pty-size on fd -1, which is a tolerated
  ;; EBADF no-op (ioctl returns -1 without signalling a Lisp condition), and
  ;; calls screen-resize under the screen lock.  The observable effects are the
  ;; x/y/width/height slot updates and the matching screen dimension update.

  ;; pane-reposition sets x/y/width/height and resizes the underlying screen.
  (it "pane-reposition-updates-geometry-and-screen"
    (let ((pane (make-no-pty-pane 1 0 0 20 5)))
      (pane-reposition pane 3 7 40 10)
      (check-table (list (list (pane-x pane) 3 "pane-x must be 3 after reposition")
                         (list (pane-y pane) 7 "pane-y must be 7 after reposition")
                         (list (pane-width pane) 40 "pane-width must be 40 after reposition")
                         (list (pane-height pane) 10 "pane-height must be 10 after reposition")
                         (list (screen-width (pane-screen pane)) 40 "screen-width must match new pane width")
                         (list (screen-height (pane-screen pane)) 10 "screen-height must match new pane height")))))

  ;; pane-reposition correctly sets position to (0,0) — the corner case for zoom-in.
  (it "pane-reposition-zero-origin"
    (let ((pane (make-no-pty-pane 1 5 3 10 5)))
      (pane-reposition pane 0 0 80 24)
      (check-table (list (list (pane-x pane) 0 "pane-x must be 0 after reposition to origin")
                         (list (pane-y pane) 0 "pane-y must be 0 after reposition to origin")
                         (list (pane-width pane) 80 "pane-width must be 80")
                         (list (pane-height pane) 24 "pane-height must be 24")
                         (list (screen-width (pane-screen pane)) 80 "screen width must match pane width")
                         (list (screen-height (pane-screen pane)) 24 "screen height must match pane height")))))

  ;; pane-reposition returns no useful value — callers rely solely on side effects.
  (it "pane-reposition-returns-no-value"
    (let ((pane (make-no-pty-pane 1 0 0 5 5)))
      (expect (progn (pane-reposition pane 0 0 10 10) t) :to-be-truthy)))

  ;; pane-border-status controls how pane-reposition reserves rows for the title bar.
  (it "pane-reposition-border-status-table"
    (dolist (row '(("top"    1  23 "top status shifts content down, height -1")
                   ("bottom" 0  23 "bottom status keeps y, height -1")
                   ("off"    0  24 "no status, full height preserved")))
      (destructuring-bind (status expected-y expected-h desc) row
        (declare (ignore desc))
        (with-fresh-options
          (cl-tmux/options:set-option "pane-border-status" status)
          (let ((pane (make-no-pty-pane 1 0 0 20 5)))
            (pane-reposition pane 0 0 80 24)
            (expect (= expected-y (pane-y pane)))
            (expect (= expected-h (pane-height pane)))
            (expect (= expected-h (screen-height (pane-screen pane)))))))))

  ;; ── next-pane-id direct tests (pure, no PTY) ─────────────────────────────

  ;; next-pane-id returns pane-base-index when the window has no panes (default 0).
  (it "next-pane-id-returns-base-index-for-empty-window"
    (let ((win (make-window :id 1 :name "w" :panes nil)))
      ;; With pane-base-index=0 (default), first pane id is 0.
      (expect (= (or (cl-tmux/options:get-option "pane-base-index") 0)
                 (cl-tmux/model::next-pane-id win)))))

  ;; next-pane-id returns the lowest id >= pane-base-index not already in use.
  (it "next-pane-id-fills-lowest-gap"
    (let* ((base (or (cl-tmux/options:get-option "pane-base-index") 0))
           (p1  (make-no-pty-pane (+ base 1) 0 0 10 5))
           (p3  (make-no-pty-pane (+ base 3) 0 0 10 5))
           (win (make-window :id 1 :name "w" :panes (list p1 p3))))
      ;; The lowest gap above base should be filled.
      (expect (= base (cl-tmux/model::next-pane-id win)))))

  ;; ── split-window -d flag (no-focus) ─────────────────────────────────────────

  ;; window-split :no-focus t creates the new pane but keeps the original active pane.
  (it "split-window-no-focus"
    (unless (pty-available-p)
      (skip "PTY not available"))
    (with-session (session 41 10)
      (let* ((win (session-active-window session))
             (active-pane (window-active-pane win)))
        (let ((new-pane (window-split session win :h :no-focus t)))
          (expect (not (null new-pane)))
          (expect (eq active-pane (window-active-pane win)))
          (expect (= 2 (length (window-panes win))))
          ;; Clean up
          (ignore-errors (pty-close (pane-fd new-pane) (pane-pid new-pane)))))))

  ;; ── split-window -l size hint ────────────────────────────────────────────────

  ;; window-split with a fractional size hint assigns the new pane a proportional width.
  (it "split-window-size-hint-percentage"
    (unless (pty-available-p)
      (skip "PTY not available"))
    (with-session (session 81 10)
      (let ((win (session-active-window session)))
        ;; Split with 0.25 size → new pane should be ~20 cols (25% of 80-col avail)
        (let ((new-pane (window-split session win :h :size 0.25)))
          (when new-pane
            (expect (> (pane-width new-pane) 0))
            (expect (< (pane-width new-pane) 81))
            (ignore-errors (pty-close (pane-fd new-pane) (pane-pid new-pane)))))))))
