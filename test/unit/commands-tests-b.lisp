(in-package #:cl-tmux/test)

;;;; copy-mode line/page/word motions, top/bottom, begin-line-sel, copy-D/Y, search — part II

(in-suite commands-suite)

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

(test copy-mode-line-start-end-noop-table
  "copy-mode-line-start and copy-mode-line-end leave the column unchanged when not in copy mode."
  (dolist (c '((cl-tmux/commands::copy-mode-line-start 10 "line-start: col unchanged")
               (cl-tmux/commands::copy-mode-line-end    3  "line-end: col unchanged")))
    (destructuring-bind (fn col desc) c
      (let ((s (make-screen 20 5)))
        (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 col))
        (funcall fn s)
        (is (= col (cdr (cl-tmux/terminal/types:screen-copy-cursor s))) "~A" desc)))))

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

(test copy-mode-h-m-l-noop-outside-copy-mode-table
  "copy-mode-high/middle/low leave the cursor row unchanged when not in copy mode."
  (dolist (c '((cl-tmux/commands::copy-mode-high   3 "high: row unchanged")
               (cl-tmux/commands::copy-mode-middle  7 "middle: row unchanged")
               (cl-tmux/commands::copy-mode-low     2 "low: row unchanged")))
    (destructuring-bind (fn row desc) c
      (let ((s (make-screen 20 10)))
        (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons row 5))
        (funcall fn s)
        (is (= row (car (cl-tmux/terminal/types:screen-copy-cursor s))) "~A" desc)))))

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

(test copy-mode-scroll-up-noop-outside-copy-mode-table
  "Scroll-up functions are no-ops when not in copy mode (offset stays 0)."
  (dolist (fn '(cl-tmux/commands::copy-mode-page-up
                cl-tmux/commands::copy-mode-half-page-up
                cl-tmux/commands::copy-mode-scroll-up-line))
    (let ((s (make-screen 20 5)))
      (setf (cl-tmux/terminal/types:screen-scrollback s)
            (loop repeat 10 collect (make-array 0)))
      (funcall fn s)
      (is (= 0 (screen-copy-offset s)) "~A: offset must remain 0" fn))))

(test copy-mode-scroll-down-noop-outside-copy-mode-table
  "Scroll-down functions are no-ops when not in copy mode (offset stays 0)."
  (dolist (fn '(cl-tmux/commands::copy-mode-page-down
                cl-tmux/commands::copy-mode-half-page-down
                cl-tmux/commands::copy-mode-scroll-down-line))
    (let ((s (make-screen 20 5)))
      (setf (cl-tmux/terminal/types:screen-copy-offset s) 0)
      (funcall fn s)
      (is (= 0 (screen-copy-offset s)) "~A: offset must remain 0" fn))))

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
