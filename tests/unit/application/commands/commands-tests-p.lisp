(in-package #:cl-tmux/test)

;;;; commands tests — part XVI: copy-mode-copy-selection-no-cancel/no-clear,
;;;; the tmux `pipe` family (pipe-no-cancel/no-clear/and-cancel), copy-pipe
;;;; no-clear/line/line-and-cancel, rectangle-on/off, cursor-down-and-cancel,
;;;; scroll-to-mouse, copy-end-of-line-and-cancel, copy-line-and-cancel.
;;;; Closes the remaining cl-tmux/commands export-coverage gaps.

(in-suite commands-suite)

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

;;; ── copy-mode-copy-selection-no-cancel / no-clear ─────────────────────────────

(test copy-mode-copy-selection-no-cancel-copies-and-clears-but-stays
  "copy-mode-copy-selection-no-cancel pushes the selection to the paste buffer,
   clears the selection, and remains in copy mode."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (%selecting-copy-mode-screen)))
      (cl-tmux/commands::copy-mode-copy-selection-no-cancel s)
      (is (string= "abcde" (cl-tmux/buffer:get-paste-buffer 0))
          "selection text must land in the paste buffer")
      (is-false (screen-copy-selecting s)
                "selection must be cleared after copy-selection-no-cancel")
      (is-true  (screen-copy-mode-p s)
                "copy mode must remain active (no-cancel means no exit)"))))

(test copy-mode-copy-selection-no-cancel-noop-outside-copy-mode
  "copy-mode-copy-selection-no-cancel is a no-op outside copy mode."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "abcde")
      (finishes (cl-tmux/commands::copy-mode-copy-selection-no-cancel s))
      (is (null cl-tmux/buffer:*paste-buffers*)
          "no paste buffer entry must be created outside copy mode"))))

(test copy-mode-copy-selection-no-clear-copies-but-keeps-selection
  "copy-mode-copy-selection-no-clear pushes the selection to the paste buffer and
   leaves BOTH the selection and copy mode intact."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (%selecting-copy-mode-screen)))
      (cl-tmux/commands::copy-mode-copy-selection-no-clear s)
      (is (string= "abcde" (cl-tmux/buffer:get-paste-buffer 0))
          "selection text must land in the paste buffer")
      (is-true (screen-copy-selecting s)
               "selection must NOT be cleared by copy-selection-no-clear")
      (is-true (screen-copy-mode-p s)
               "copy mode must remain active"))))

(test copy-mode-copy-selection-no-clear-noop-outside-copy-mode
  "copy-mode-copy-selection-no-clear is a no-op outside copy mode."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "abcde")
      (finishes (cl-tmux/commands::copy-mode-copy-selection-no-clear s))
      (is (null cl-tmux/buffer:*paste-buffers*)
          "no paste buffer entry must be created outside copy mode"))))

;;; ── tmux `pipe` family: pipe WITHOUT copying to the paste buffer ────────────
;;; Pass CMD "" so %resolve-copy-pipe-cmd finds no copy-command option and no
;;; shell subprocess is actually spawned; only the buffer/state side is checked.

(test copy-mode-pipe-no-cancel-does-not-touch-paste-buffer
  "copy-mode-pipe-no-cancel does NOT add to the paste buffer (pipe-only), clears
   the selection, and stays in copy mode."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (%selecting-copy-mode-screen)))
      (cl-tmux/commands::copy-mode-pipe-no-cancel s "")
      (is (null cl-tmux/buffer:*paste-buffers*)
          "pipe (no copy) must never populate the paste buffer")
      (is-false (screen-copy-selecting s)
                "selection must be cleared after pipe-no-cancel")
      (is-true (screen-copy-mode-p s)
               "copy mode must remain active after pipe-no-cancel"))))

(test copy-mode-pipe-no-cancel-noop-outside-copy-mode
  "copy-mode-pipe-no-cancel is a no-op outside copy mode."
  (let ((s (make-screen 20 5)))
    (feed s "abcde")
    (finishes (cl-tmux/commands::copy-mode-pipe-no-cancel s ""))
    (is-false (screen-copy-mode-p s) "still outside copy mode")))

(test copy-mode-pipe-no-clear-keeps-selection
  "copy-mode-pipe-no-clear does not touch the paste buffer and does NOT clear
   the selection, staying in copy mode."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (%selecting-copy-mode-screen)))
      (cl-tmux/commands::copy-mode-pipe-no-clear s "")
      (is (null cl-tmux/buffer:*paste-buffers*)
          "pipe-no-clear must never populate the paste buffer")
      (is-true (screen-copy-selecting s)
               "selection must NOT be cleared by pipe-no-clear")
      (is-true (screen-copy-mode-p s)
               "copy mode must remain active after pipe-no-clear"))))

(test copy-mode-pipe-and-cancel-exits-copy-mode
  "copy-mode-pipe-and-cancel does not touch the paste buffer and exits copy mode."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (%selecting-copy-mode-screen)))
      (cl-tmux/commands::copy-mode-pipe-and-cancel s "")
      (is (null cl-tmux/buffer:*paste-buffers*)
          "pipe-and-cancel must never populate the paste buffer")
      (is-false (screen-copy-mode-p s)
                "copy mode must exit after pipe-and-cancel"))))

(test copy-mode-pipe-and-cancel-noop-outside-copy-mode
  "copy-mode-pipe-and-cancel is a no-op outside copy mode."
  (let ((s (make-screen 20 5)))
    (feed s "abcde")
    (finishes (cl-tmux/commands::copy-mode-pipe-and-cancel s ""))
    (is-false (screen-copy-mode-p s) "still outside copy mode")))

;;; ── copy-pipe no-clear / line / line-and-cancel ──────────────────────────────

(test copy-mode-copy-pipe-no-clear-copies-and-keeps-selection
  "copy-mode-copy-pipe-no-clear copies the selection to the paste buffer, keeps
   the selection, and stays in copy mode."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (%selecting-copy-mode-screen)))
      (cl-tmux/commands::copy-mode-copy-pipe-no-clear s "")
      (is (string= "abcde" (cl-tmux/buffer:get-paste-buffer 0))
          "selection text must land in the paste buffer")
      (is-true (screen-copy-selecting s)
               "selection must NOT be cleared by copy-pipe-no-clear")
      (is-true (screen-copy-mode-p s)
               "copy mode must remain active after copy-pipe-no-clear"))))

(test copy-mode-copy-pipe-line-copies-whole-line
  "copy-mode-copy-pipe-line copies the entire current line regardless of cursor
   column, clears the selection, and stays in copy mode."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "hello world")
      (cl-tmux/commands::copy-mode-enter s)
      (setf (screen-copy-cursor s) (cons 0 6))
      (cl-tmux/commands::copy-mode-copy-pipe-line s "")
      (is (string= "hello world" (cl-tmux/buffer:get-paste-buffer 0))
          "copy-pipe-line must copy the full row regardless of cursor column")
      (is-true (screen-copy-mode-p s)
               "copy mode must remain active after copy-pipe-line"))))

(test copy-mode-copy-pipe-line-and-cancel-exits-copy-mode
  "copy-mode-copy-pipe-line-and-cancel copies the whole line and exits copy mode."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "hello world")
      (cl-tmux/commands::copy-mode-enter s)
      (setf (screen-copy-cursor s) (cons 0 0))
      (cl-tmux/commands::copy-mode-copy-pipe-line-and-cancel s "")
      (is (string= "hello world" (cl-tmux/buffer:get-paste-buffer 0))
          "copy-pipe-line-and-cancel must copy the full row")
      (is-false (screen-copy-mode-p s)
                "copy mode must exit after copy-pipe-line-and-cancel"))))

;;; ── copy-mode-rectangle-on / copy-mode-rectangle-off ─────────────────────────

(test copy-mode-rectangle-on-sets-rect-select-flag
  "copy-mode-rectangle-on unconditionally sets rect-select to T."
  (let ((s (copy-mode-screen)))
    (is-false (screen-copy-rect-select-p s) "rect-select must start NIL")
    (cl-tmux/commands::copy-mode-rectangle-on s)
    (is-true (screen-copy-rect-select-p s)
             "rect-select must be T after rectangle-on")
    ;; Idempotent: calling again keeps it T.
    (cl-tmux/commands::copy-mode-rectangle-on s)
    (is-true (screen-copy-rect-select-p s)
             "rectangle-on must remain idempotent")))

(test copy-mode-rectangle-on-noop-outside-copy-mode
  "copy-mode-rectangle-on does nothing outside copy mode."
  (let ((s (make-screen 20 5)))
    (finishes (cl-tmux/commands::copy-mode-rectangle-on s))
    (is-false (screen-copy-rect-select-p s)
              "rect-select must remain NIL outside copy mode")))

(test copy-mode-rectangle-off-clears-rect-select-flag
  "copy-mode-rectangle-off unconditionally clears rect-select to NIL."
  (let ((s (copy-mode-screen)))
    (setf (screen-copy-rect-select-p s) t)
    (cl-tmux/commands::copy-mode-rectangle-off s)
    (is-false (screen-copy-rect-select-p s)
              "rect-select must be NIL after rectangle-off")
    ;; Idempotent: calling again keeps it NIL.
    (cl-tmux/commands::copy-mode-rectangle-off s)
    (is-false (screen-copy-rect-select-p s)
              "rectangle-off must remain idempotent")))

(test copy-mode-rectangle-off-noop-outside-copy-mode
  "copy-mode-rectangle-off does nothing outside copy mode."
  (let ((s (make-screen 20 5)))
    (finishes (cl-tmux/commands::copy-mode-rectangle-off s))
    (is-false (screen-copy-rect-select-p s)
              "rect-select must remain NIL outside copy mode")))

;;; ── copy-mode-cursor-down-and-cancel ──────────────────────────────────────────

(test copy-mode-cursor-down-and-cancel-moves-cursor-mid-viewport
  "copy-mode-cursor-down-and-cancel moves the cursor down and stays in copy mode
   when the cursor is not yet at the bottom of the live view."
  (let ((s (make-screen 20 5)))
    (feed s "line")
    (cl-tmux/commands::copy-mode-enter s)
    (setf (screen-copy-cursor s) (cons 0 0))
    (cl-tmux/commands::copy-mode-cursor-down-and-cancel s)
    (is (equal (cons 1 0) (screen-copy-cursor s))
        "cursor must move one row down")
    (is-true (screen-copy-mode-p s)
             "copy mode must remain active when the cursor can still move")))

(test copy-mode-cursor-down-and-cancel-exits-at-live-bottom
  "copy-mode-cursor-down-and-cancel exits copy mode when the cursor is already at
   the bottom row of the live view (offset 0) and cannot move further down."
  (let ((s (make-screen 20 5)))
    (feed s "line")
    (cl-tmux/commands::copy-mode-enter s)
    ;; copy-mode-enter places the cursor at the bottom-left of the viewport
    ;; with offset 0 — the live-bottom precondition for the cancel exit.
    (is (equal (cons 4 0) (screen-copy-cursor s))
        "precondition: cursor starts at bottom-left row (height-1=4)")
    (cl-tmux/commands::copy-mode-cursor-down-and-cancel s)
    (is-false (screen-copy-mode-p s)
              "copy mode must exit when the cursor cannot move further down")))

(test copy-mode-cursor-down-and-cancel-noop-outside-copy-mode
  "copy-mode-cursor-down-and-cancel does nothing outside copy mode."
  (let ((s (make-screen 20 5)))
    (finishes (cl-tmux/commands::copy-mode-cursor-down-and-cancel s))
    (is-false (screen-copy-mode-p s) "still outside copy mode")))

;;; ── copy-mode-scroll-to-mouse ──────────────────────────────────────────────

(test copy-mode-scroll-to-mouse-marks-screen-dirty
  "copy-mode-scroll-to-mouse marks the screen dirty so the renderer refreshes
   the viewport toward the mouse drag position."
  (let ((s (copy-mode-screen)))
    (screen-clear-dirty s)
    (is-false (screen-dirty-p s) "precondition: screen clean before the call")
    (cl-tmux/commands::copy-mode-scroll-to-mouse s)
    (is-true (screen-dirty-p s)
             "screen must be marked dirty after scroll-to-mouse")))

(test copy-mode-scroll-to-mouse-noop-outside-copy-mode
  "copy-mode-scroll-to-mouse does not mark the screen dirty outside copy mode."
  (let ((s (make-screen 20 5)))
    (screen-clear-dirty s)
    (cl-tmux/commands::copy-mode-scroll-to-mouse s)
    (is-false (screen-dirty-p s)
              "screen must stay clean outside copy mode")))

;;; ── copy-mode-copy-end-of-line-and-cancel / copy-mode-copy-line-and-cancel ──
;;; Table-driven: both -and-cancel variants share the same shape (copy then exit).

(defmacro define-copy-and-cancel-exit-test (test-name fn cursor-col expected)
  "Generate a test asserting FN copies EXPECTED text from \"hello world\" (fed on
   row 0) starting at CURSOR-COL, and exits copy mode."
  `(test ,test-name
     ,(format nil "~A copies the expected row text and exits copy mode." fn)
     (let ((cl-tmux/buffer:*paste-buffers* nil))
       (let ((s (make-screen 20 5)))
         (feed s "hello world")
         (cl-tmux/commands::copy-mode-enter s)
         (setf (screen-copy-cursor s) (cons 0 ,cursor-col))
         (,fn s)
         (is (string= ,expected (cl-tmux/buffer:get-paste-buffer 0))
             "~A must push ~S to the paste buffer" ',fn ,expected)
         (is-false (screen-copy-mode-p s)
                   "~A must exit copy mode" ',fn)))))

(define-copy-and-cancel-exit-test
    copy-mode-copy-end-of-line-and-cancel-copies-tail-and-exits
  cl-tmux/commands::copy-mode-copy-end-of-line-and-cancel 6 "world")

(define-copy-and-cancel-exit-test
    copy-mode-copy-line-and-cancel-copies-whole-line-and-exits
  cl-tmux/commands::copy-mode-copy-line-and-cancel 6 "hello world")

(test copy-mode-copy-end-of-line-and-cancel-noop-outside-copy-mode
  "copy-mode-copy-end-of-line-and-cancel does nothing outside copy mode."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "hello")
      (finishes (cl-tmux/commands::copy-mode-copy-end-of-line-and-cancel s))
      (is (null cl-tmux/buffer:*paste-buffers*)
          "no paste buffer entry must be created outside copy mode"))))

(test copy-mode-copy-line-and-cancel-noop-outside-copy-mode
  "copy-mode-copy-line-and-cancel does nothing outside copy mode."
  (let ((cl-tmux/buffer:*paste-buffers* nil))
    (let ((s (make-screen 20 5)))
      (feed s "hello")
      (finishes (cl-tmux/commands::copy-mode-copy-line-and-cancel s))
      (is (null cl-tmux/buffer:*paste-buffers*)
          "no paste buffer entry must be created outside copy mode"))))

;;; ── *copy-mode-last-jump* direct coverage ─────────────────────────────────────
;;; Exercised transitively by copy-mode-jump-again/-reverse elsewhere; here it is
;;; asserted directly so the special variable's own contract (nil until a jump
;;; command runs) is covered.

(test copy-mode-last-jump-nil-before-any-jump
  "*copy-mode-last-jump* starts NIL until a jump-forward/backward/to/to-backward
   command has run in the current image state."
  (let ((cl-tmux/commands::*copy-mode-last-jump* nil))
    (is (null cl-tmux/commands::*copy-mode-last-jump*)
        "last-jump must be NIL before any jump command runs")))

(test copy-mode-jump-forward-sets-last-jump
  "copy-mode-jump-forward records the jump direction/char/mode in *copy-mode-last-jump*
   so a subsequent copy-mode-jump-again can repeat it."
  (let ((cl-tmux/commands::*copy-mode-last-jump* nil))
    (let ((s (make-screen 20 5)))
      (feed s "abcabc")
      (cl-tmux/commands::copy-mode-enter s)
      (setf (screen-copy-cursor s) (cons 0 0))
      (cl-tmux/commands::copy-mode-jump-forward s #\c)
      (is-true cl-tmux/commands::*copy-mode-last-jump*
               "last-jump must be set (non-NIL) after jump-forward"))))
