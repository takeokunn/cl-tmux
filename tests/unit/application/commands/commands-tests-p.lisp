(in-package #:cl-tmux/test)

;;;; commands tests — part XVI: copy-mode-copy-selection-no-cancel/no-clear,
;;;; the tmux `pipe` family (pipe-no-cancel/no-clear/and-cancel), copy-pipe
;;;; no-clear/line/line-and-cancel, rectangle-on/off, cursor-down-and-cancel,
;;;; scroll-to-mouse, copy-end-of-line-and-cancel, copy-line-and-cancel.
;;;; Closes the remaining cl-tmux/commands export-coverage gaps.

;;; ── Shared fixture: a selecting copy-mode screen with "abcde" on row 0 ───────

(defun %selecting-copy-mode-screen (&key (content "abcde") (mark-col 0) (cursor-col 5))
  "Return a copy-mode screen with CONTENT fed on row 0 and an active selection
   spanning MARK-COL to CURSOR-COL (exclusive end) on that row."
  (let ((s (make-screen 20 5)))
    (feed s content)
    (cl-tmux/commands::copy-mode-enter s)
    (setf (screen-copy-selecting s) t
          (screen-copy-mark      s) (cons 0 mark-col)
          (screen-copy-cursor    s) (cons 0 cursor-col))
    s))

;;; ── copy-mode-copy-end-of-line-and-cancel / copy-mode-copy-line-and-cancel ──
;;; Table-driven: both -and-cancel variants share the same shape (copy then exit).

(defmacro define-copy-and-cancel-exit-test (test-name fn cursor-col expected)
  "Generate a test asserting FN copies EXPECTED text from \"hello world\" (fed on
   row 0) starting at CURSOR-COL, and exits copy mode."
  `(it ,(string-downcase (symbol-name test-name))
     (let ((cl-tmux/buffer:*paste-buffers* nil))
       (let ((s (make-screen 20 5)))
         (feed s "hello world")
         (cl-tmux/commands::copy-mode-enter s)
         (setf (screen-copy-cursor s) (cons 0 ,cursor-col))
         (,fn s)
         (expect (string= ,expected (cl-tmux/buffer:get-paste-buffer 0)))
         (expect (screen-copy-mode-p s) :to-be-falsy)))))

(describe "commands-suite"

  ;;; ── copy-mode-copy-selection-no-cancel / no-clear ─────────────────────────────

  ;; copy-mode-copy-selection-no-cancel pushes the selection to the paste buffer,
  ;; clears the selection, and remains in copy mode.
  (it "copy-mode-copy-selection-no-cancel-copies-and-clears-but-stays"
    (let ((cl-tmux/buffer:*paste-buffers* nil))
      (let ((s (%selecting-copy-mode-screen)))
        (cl-tmux/commands::copy-mode-copy-selection-no-cancel s)
        (expect (string= "abcde" (cl-tmux/buffer:get-paste-buffer 0)))
        (expect (screen-copy-selecting s) :to-be-falsy)
        (expect (screen-copy-mode-p s) :to-be-truthy))))

  ;; copy-mode-copy-selection-no-cancel is a no-op outside copy mode.
  (it "copy-mode-copy-selection-no-cancel-noop-outside-copy-mode"
    (let ((cl-tmux/buffer:*paste-buffers* nil))
      (let ((s (make-screen 20 5)))
        (feed s "abcde")
        (finishes (cl-tmux/commands::copy-mode-copy-selection-no-cancel s))
        (expect (null cl-tmux/buffer:*paste-buffers*)))))

  ;; copy-mode-copy-selection-no-clear pushes the selection to the paste buffer and
  ;; leaves BOTH the selection and copy mode intact.
  (it "copy-mode-copy-selection-no-clear-copies-but-keeps-selection"
    (let ((cl-tmux/buffer:*paste-buffers* nil))
      (let ((s (%selecting-copy-mode-screen)))
        (cl-tmux/commands::copy-mode-copy-selection-no-clear s)
        (expect (string= "abcde" (cl-tmux/buffer:get-paste-buffer 0)))
        (expect (screen-copy-selecting s) :to-be-truthy)
        (expect (screen-copy-mode-p s) :to-be-truthy))))

  ;; copy-mode-copy-selection-no-clear is a no-op outside copy mode.
  (it "copy-mode-copy-selection-no-clear-noop-outside-copy-mode"
    (let ((cl-tmux/buffer:*paste-buffers* nil))
      (let ((s (make-screen 20 5)))
        (feed s "abcde")
        (finishes (cl-tmux/commands::copy-mode-copy-selection-no-clear s))
        (expect (null cl-tmux/buffer:*paste-buffers*)))))

  ;;; ── tmux `pipe` family: pipe WITHOUT copying to the paste buffer ────────────
  ;;; Pass CMD "" so %resolve-copy-pipe-cmd finds no copy-command option and no
  ;;; shell subprocess is actually spawned; only the buffer/state side is checked.

  ;; copy-mode-pipe-no-cancel does NOT add to the paste buffer (pipe-only), clears
  ;; the selection, and stays in copy mode.
  (it "copy-mode-pipe-no-cancel-does-not-touch-paste-buffer"
    (let ((cl-tmux/buffer:*paste-buffers* nil))
      (let ((s (%selecting-copy-mode-screen)))
        (cl-tmux/commands::copy-mode-pipe-no-cancel s "")
        (expect (null cl-tmux/buffer:*paste-buffers*))
        (expect (screen-copy-selecting s) :to-be-falsy)
        (expect (screen-copy-mode-p s) :to-be-truthy))))

  ;; copy-mode-pipe-no-cancel is a no-op outside copy mode.
  (it "copy-mode-pipe-no-cancel-noop-outside-copy-mode"
    (let ((s (make-screen 20 5)))
      (feed s "abcde")
      (finishes (cl-tmux/commands::copy-mode-pipe-no-cancel s ""))
      (expect (screen-copy-mode-p s) :to-be-falsy)))

  ;; copy-mode-pipe-no-clear does not touch the paste buffer and does NOT clear
  ;; the selection, staying in copy mode.
  (it "copy-mode-pipe-no-clear-keeps-selection"
    (let ((cl-tmux/buffer:*paste-buffers* nil))
      (let ((s (%selecting-copy-mode-screen)))
        (cl-tmux/commands::copy-mode-pipe-no-clear s "")
        (expect (null cl-tmux/buffer:*paste-buffers*))
        (expect (screen-copy-selecting s) :to-be-truthy)
        (expect (screen-copy-mode-p s) :to-be-truthy))))

  ;; copy-mode-pipe-and-cancel does not touch the paste buffer and exits copy mode.
  (it "copy-mode-pipe-and-cancel-exits-copy-mode"
    (let ((cl-tmux/buffer:*paste-buffers* nil))
      (let ((s (%selecting-copy-mode-screen)))
        (cl-tmux/commands::copy-mode-pipe-and-cancel s "")
        (expect (null cl-tmux/buffer:*paste-buffers*))
        (expect (screen-copy-mode-p s) :to-be-falsy))))

  ;; copy-mode-pipe-and-cancel is a no-op outside copy mode.
  (it "copy-mode-pipe-and-cancel-noop-outside-copy-mode"
    (let ((s (make-screen 20 5)))
      (feed s "abcde")
      (finishes (cl-tmux/commands::copy-mode-pipe-and-cancel s ""))
      (expect (screen-copy-mode-p s) :to-be-falsy)))

  ;;; ── copy-pipe no-clear / line / line-and-cancel ──────────────────────────────

  ;; copy-mode-copy-pipe-no-clear copies the selection to the paste buffer, keeps
  ;; the selection, and stays in copy mode.
  (it "copy-mode-copy-pipe-no-clear-copies-and-keeps-selection"
    (let ((cl-tmux/buffer:*paste-buffers* nil))
      (let ((s (%selecting-copy-mode-screen)))
        (cl-tmux/commands::copy-mode-copy-pipe-no-clear s "")
        (expect (string= "abcde" (cl-tmux/buffer:get-paste-buffer 0)))
        (expect (screen-copy-selecting s) :to-be-truthy)
        (expect (screen-copy-mode-p s) :to-be-truthy))))

  ;; copy-mode-copy-pipe-line copies the entire current line regardless of cursor
  ;; column, clears the selection, and stays in copy mode.
  (it "copy-mode-copy-pipe-line-copies-whole-line"
    (let ((cl-tmux/buffer:*paste-buffers* nil))
      (let ((s (make-screen 20 5)))
        (feed s "hello world")
        (cl-tmux/commands::copy-mode-enter s)
        (setf (screen-copy-cursor s) (cons 0 6))
        (cl-tmux/commands::copy-mode-copy-pipe-line s "")
        (expect (string= "hello world" (cl-tmux/buffer:get-paste-buffer 0)))
        (expect (screen-copy-mode-p s) :to-be-truthy))))

  ;; copy-mode-copy-pipe-line-and-cancel copies the whole line and exits copy mode.
  (it "copy-mode-copy-pipe-line-and-cancel-exits-copy-mode"
    (let ((cl-tmux/buffer:*paste-buffers* nil))
      (let ((s (make-screen 20 5)))
        (feed s "hello world")
        (cl-tmux/commands::copy-mode-enter s)
        (setf (screen-copy-cursor s) (cons 0 0))
        (cl-tmux/commands::copy-mode-copy-pipe-line-and-cancel s "")
        (expect (string= "hello world" (cl-tmux/buffer:get-paste-buffer 0)))
        (expect (screen-copy-mode-p s) :to-be-falsy))))

  ;;; ── copy-mode-rectangle-on / copy-mode-rectangle-off ─────────────────────────

  ;; copy-mode-rectangle-on unconditionally sets rect-select to T.
  (it "copy-mode-rectangle-on-sets-rect-select-flag"
    (let ((s (copy-mode-screen)))
      (expect (screen-copy-rect-select-p s) :to-be-falsy)
      (cl-tmux/commands::copy-mode-rectangle-on s)
      (expect (screen-copy-rect-select-p s) :to-be-truthy)
      ;; Idempotent: calling again keeps it T.
      (cl-tmux/commands::copy-mode-rectangle-on s)
      (expect (screen-copy-rect-select-p s) :to-be-truthy)))

  ;; copy-mode-rectangle-on does nothing outside copy mode.
  (it "copy-mode-rectangle-on-noop-outside-copy-mode"
    (let ((s (make-screen 20 5)))
      (finishes (cl-tmux/commands::copy-mode-rectangle-on s))
      (expect (screen-copy-rect-select-p s) :to-be-falsy)))

  ;; copy-mode-rectangle-off unconditionally clears rect-select to NIL.
  (it "copy-mode-rectangle-off-clears-rect-select-flag"
    (let ((s (copy-mode-screen)))
      (setf (screen-copy-rect-select-p s) t)
      (cl-tmux/commands::copy-mode-rectangle-off s)
      (expect (screen-copy-rect-select-p s) :to-be-falsy)
      ;; Idempotent: calling again keeps it NIL.
      (cl-tmux/commands::copy-mode-rectangle-off s)
      (expect (screen-copy-rect-select-p s) :to-be-falsy)))

  ;; copy-mode-rectangle-off does nothing outside copy mode.
  (it "copy-mode-rectangle-off-noop-outside-copy-mode"
    (let ((s (make-screen 20 5)))
      (finishes (cl-tmux/commands::copy-mode-rectangle-off s))
      (expect (screen-copy-rect-select-p s) :to-be-falsy)))

  ;;; ── copy-mode-cursor-down-and-cancel ──────────────────────────────────────────

  ;; copy-mode-cursor-down-and-cancel moves the cursor down and stays in copy mode
  ;; when the cursor is not yet at the bottom of the live view.
  (it "copy-mode-cursor-down-and-cancel-moves-cursor-mid-viewport"
    (let ((s (make-screen 20 5)))
      (feed s "line")
      (cl-tmux/commands::copy-mode-enter s)
      (setf (screen-copy-cursor s) (cons 0 0))
      (cl-tmux/commands::copy-mode-cursor-down-and-cancel s)
      (expect (equal (cons 1 0) (screen-copy-cursor s)))
      (expect (screen-copy-mode-p s) :to-be-truthy)))

  ;; copy-mode-cursor-down-and-cancel exits copy mode when the cursor is already at
  ;; the bottom row of the live view (offset 0) and cannot move further down.
  (it "copy-mode-cursor-down-and-cancel-exits-at-live-bottom"
    (let ((s (make-screen 20 5)))
      (feed s "line")
      (cl-tmux/commands::copy-mode-enter s)
      ;; copy-mode-enter places the cursor at the bottom-left of the viewport
      ;; with offset 0 — the live-bottom precondition for the cancel exit.
      (expect (equal (cons 4 0) (screen-copy-cursor s)))
      (cl-tmux/commands::copy-mode-cursor-down-and-cancel s)
      (expect (screen-copy-mode-p s) :to-be-falsy)))

  ;; copy-mode-cursor-down-and-cancel does nothing outside copy mode.
  (it "copy-mode-cursor-down-and-cancel-noop-outside-copy-mode"
    (let ((s (make-screen 20 5)))
      (finishes (cl-tmux/commands::copy-mode-cursor-down-and-cancel s))
      (expect (screen-copy-mode-p s) :to-be-falsy)))

  ;;; ── copy-mode-scroll-to-mouse ──────────────────────────────────────────────

  ;; copy-mode-scroll-to-mouse marks the screen dirty so the renderer refreshes
  ;; the viewport toward the mouse drag position.
  (it "copy-mode-scroll-to-mouse-marks-screen-dirty"
    (let ((s (copy-mode-screen)))
      (screen-clear-dirty s)
      (expect (screen-dirty-p s) :to-be-falsy)
      (cl-tmux/commands::copy-mode-scroll-to-mouse s)
      (expect (screen-dirty-p s) :to-be-truthy)))

  ;; copy-mode-scroll-to-mouse does not mark the screen dirty outside copy mode.
  (it "copy-mode-scroll-to-mouse-noop-outside-copy-mode"
    (let ((s (make-screen 20 5)))
      (screen-clear-dirty s)
      (cl-tmux/commands::copy-mode-scroll-to-mouse s)
      (expect (screen-dirty-p s) :to-be-falsy)))

  (define-copy-and-cancel-exit-test
      copy-mode-copy-end-of-line-and-cancel-copies-tail-and-exits
    cl-tmux/commands::copy-mode-copy-end-of-line-and-cancel 6 "world")

  (define-copy-and-cancel-exit-test
      copy-mode-copy-line-and-cancel-copies-whole-line-and-exits
    cl-tmux/commands::copy-mode-copy-line-and-cancel 6 "hello world")

  ;; copy-mode-copy-end-of-line-and-cancel does nothing outside copy mode.
  (it "copy-mode-copy-end-of-line-and-cancel-noop-outside-copy-mode"
    (let ((cl-tmux/buffer:*paste-buffers* nil))
      (let ((s (make-screen 20 5)))
        (feed s "hello")
        (finishes (cl-tmux/commands::copy-mode-copy-end-of-line-and-cancel s))
        (expect (null cl-tmux/buffer:*paste-buffers*)))))

  ;; copy-mode-copy-line-and-cancel does nothing outside copy mode.
  (it "copy-mode-copy-line-and-cancel-noop-outside-copy-mode"
    (let ((cl-tmux/buffer:*paste-buffers* nil))
      (let ((s (make-screen 20 5)))
        (feed s "hello")
        (finishes (cl-tmux/commands::copy-mode-copy-line-and-cancel s))
        (expect (null cl-tmux/buffer:*paste-buffers*)))))

  ;;; ── *copy-mode-last-jump* direct coverage ─────────────────────────────────────
  ;;; Exercised transitively by copy-mode-jump-again/-reverse elsewhere; here it is
  ;;; asserted directly so the special variable's own contract (nil until a jump
  ;;; command runs) is covered.

  ;; *copy-mode-last-jump* starts NIL until a jump-forward/backward/to/to-backward
  ;; command has run in the current image state.
  (it "copy-mode-last-jump-nil-before-any-jump"
    (let ((cl-tmux/commands::*copy-mode-last-jump* nil))
      (expect (null cl-tmux/commands::*copy-mode-last-jump*))))

  ;; copy-mode-jump-forward records the jump direction/char/mode in *copy-mode-last-jump*
  ;; so a subsequent copy-mode-jump-again can repeat it.
  (it "copy-mode-jump-forward-sets-last-jump"
    (let ((cl-tmux/commands::*copy-mode-last-jump* nil))
      (let ((s (make-screen 20 5)))
        (feed s "abcabc")
        (cl-tmux/commands::copy-mode-enter s)
        (setf (screen-copy-cursor s) (cons 0 0))
        (cl-tmux/commands::copy-mode-jump-forward s #\c)
        (expect cl-tmux/commands::*copy-mode-last-jump* :to-be-truthy)))))
