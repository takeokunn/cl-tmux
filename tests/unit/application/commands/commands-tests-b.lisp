(in-package #:cl-tmux/test)

;;;; copy-mode line/page/word motions, top/bottom, begin-line-sel, copy-D/Y, search — part II

(describe "commands-suite"

  ;;; ── copy-mode-line-start / copy-mode-line-end ────────────────────────────────

  ;; copy-mode-line-start (vi 0) sets the cursor column to 0.
  (it "copy-mode-line-start-moves-to-col-0"
    (let ((s (make-screen 20 5)))
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 10))
      (cl-tmux/commands::copy-mode-line-start s)
      (expect (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))))

  ;; copy-mode-line-end (vi $) moves to the last NON-BLANK column (tmux's
  ;; cursor-end-of-line), not the screen edge; an entirely blank row goes to col 0;
  ;; rectangle-select mode goes to the last screen column.
  (it "copy-mode-line-end-moves-to-last-content-column"
    ;; Content row: '$' lands on the last content char, not the screen edge.
    (let ((s (copy-mode-screen :content "abc")))
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
      (cl-tmux/commands::copy-mode-line-end s)
      (expect (= 2 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))))
    ;; Entirely blank row: '$' falls back to column 0.
    (let ((s (copy-mode-screen)))
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 5))
      (cl-tmux/commands::copy-mode-line-end s)
      (expect (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))))
    ;; Rectangle-select mode: '$' goes to the last screen column (pane edge).
    (let ((s (copy-mode-screen :content "abc")))
      (setf (cl-tmux/terminal/types:screen-copy-rect-select-p s) t
            (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
      (cl-tmux/commands::copy-mode-line-end s)
      (expect (= (1- (cl-tmux/terminal/types:screen-width s))
                 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))))

  ;; copy-mode-line-start and copy-mode-line-end leave the column unchanged when not in copy mode.
  (it "copy-mode-line-start-end-noop-table"
    (dolist (c '((cl-tmux/commands::copy-mode-line-start 10 "line-start: col unchanged")
                 (cl-tmux/commands::copy-mode-line-end    3  "line-end: col unchanged")))
      (destructuring-bind (fn col desc) c
        (declare (ignore desc))
        (let ((s (make-screen 20 5)))
          (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 col))
          (funcall fn s)
          (expect (= col (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))))))

  ;;; ── copy-mode-high / copy-mode-middle / copy-mode-low ───────────────────────

  ;; copy-mode-high/middle/low each set the cursor row, preserving column.
  (it "copy-mode-h-m-l-moves-table"
    (dolist (c '((cl-tmux/commands::copy-mode-high   (7 . 5) 0 "high → row 0")
                 (cl-tmux/commands::copy-mode-middle  (0 . 5) 5 "middle → floor(10/2)=5")
                 (cl-tmux/commands::copy-mode-low     (0 . 5) 9 "low → height-1=9")))
      (destructuring-bind (fn init-cursor expected-row desc) c
        (declare (ignore desc))
        (let ((s (make-screen 20 10)))
          (cl-tmux/commands::copy-mode-enter s)
          (setf (cl-tmux/terminal/types:screen-copy-cursor s) init-cursor)
          (funcall fn s)
          (expect (= expected-row (car (cl-tmux/terminal/types:screen-copy-cursor s))))
          (expect (= 5 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))))))

  ;; copy-mode-high/middle/low leave the cursor row unchanged when not in copy mode.
  (it "copy-mode-h-m-l-noop-outside-copy-mode-table"
    (dolist (c '((cl-tmux/commands::copy-mode-high   3 "high: row unchanged")
                 (cl-tmux/commands::copy-mode-middle  7 "middle: row unchanged")
                 (cl-tmux/commands::copy-mode-low     2 "low: row unchanged")))
      (destructuring-bind (fn row desc) c
        (declare (ignore desc))
        (let ((s (make-screen 20 10)))
          (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons row 5))
          (funcall fn s)
          (expect (= row (car (cl-tmux/terminal/types:screen-copy-cursor s))))))))

  ;; copy-mode-cursor-centre-vertical/horizontal set the centre row/column.
  (it "copy-mode-cursor-centre-moves-table"
    (dolist (c '((cl-tmux/commands::copy-mode-cursor-centre-vertical
                  (2 . 7) 5 7 "vertical centre → row 5")
                 (cl-tmux/commands::copy-mode-cursor-centre-horizontal
                  (2 . 7) 10 2 "horizontal centre → col 10")))
      (destructuring-bind (fn init-cursor expected-value expected-keep desc) c
        (declare (ignore desc))
        (let ((s (make-screen 20 10)))
          (cl-tmux/commands::copy-mode-enter s)
          (setf (cl-tmux/terminal/types:screen-copy-cursor s) init-cursor)
          (funcall fn s)
          (ecase fn
            (cl-tmux/commands::copy-mode-cursor-centre-vertical
             (expect (= expected-value (car (cl-tmux/terminal/types:screen-copy-cursor s))))
             (expect (= expected-keep (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))))
            (cl-tmux/commands::copy-mode-cursor-centre-horizontal
             (expect (= expected-value (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))
             (expect (= expected-keep (car (cl-tmux/terminal/types:screen-copy-cursor s))))))))))

  ;; copy-mode-cursor-centre-vertical/horizontal leave the cursor unchanged when not in copy mode.
  (it "copy-mode-cursor-centre-noop-outside-copy-mode-table"
    (dolist (c '((cl-tmux/commands::copy-mode-cursor-centre-vertical 3 5 "vertical: row unchanged")
                 (cl-tmux/commands::copy-mode-cursor-centre-horizontal 7 2 "horizontal: col unchanged")))
      (destructuring-bind (fn row col desc) c
        (declare (ignore desc))
        (let ((s (make-screen 20 10)))
          (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons row col))
          (funcall fn s)
          (expect (= row (car (cl-tmux/terminal/types:screen-copy-cursor s))))
          (expect (= col (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))))))

  ;;; ── copy-mode-page-up / copy-mode-page-down ─────────────────────────────────

  ;; copy-mode-page-up scrolls back by screen-height lines.
  (it "copy-mode-page-up-scrolls-by-full-height"
    (let ((s (%screen-with-scrollback 30)))
      (cl-tmux/commands::copy-mode-page-up s)
      (expect (= 5 (screen-copy-offset s)))))

  ;; copy-mode-page-down scrolls forward by screen-height lines.
  (it "copy-mode-page-down-scrolls-forward-by-full-height"
    (let ((s (%screen-with-scrollback 30)))
      ;; First scroll back enough to allow scrolling forward
      (setf (cl-tmux/terminal/types:screen-copy-offset s) 20)
      (cl-tmux/commands::copy-mode-page-down s)
      (expect (= 15 (screen-copy-offset s)))))

  ;; copy-mode-half-page-up scrolls back by floor(screen-height/2) lines.
  (it "copy-mode-half-page-up-scrolls-by-half-height"
    (let ((s (%screen-with-scrollback 30)))
      (cl-tmux/commands::copy-mode-half-page-up s)
      (expect (= 2 (screen-copy-offset s)))))

  ;; copy-mode-scroll-up-line scrolls back by exactly 1 line.
  (it "copy-mode-scroll-up-line-scrolls-by-one"
    (let ((s (%screen-with-scrollback 10)))
      (cl-tmux/commands::copy-mode-scroll-up-line s)
      (expect (= 1 (screen-copy-offset s)))))

  ;; copy-mode-scroll-down-line scrolls forward by exactly 1 line.
  (it "copy-mode-scroll-down-line-scrolls-forward-by-one"
    (let ((s (%screen-with-scrollback 10)))
      (setf (cl-tmux/terminal/types:screen-copy-offset s) 5)
      (cl-tmux/commands::copy-mode-scroll-down-line s)
      (expect (= 4 (screen-copy-offset s)))))

  ;; Scroll-up functions (and copy-mode-top) are no-ops when not in copy mode (offset stays 0).
  (it "copy-mode-scroll-up-noop-outside-copy-mode-table"
    (dolist (fn '(cl-tmux/commands::copy-mode-page-up
                  cl-tmux/commands::copy-mode-half-page-up
                  cl-tmux/commands::copy-mode-scroll-up-line
                  cl-tmux/commands::copy-mode-top))
      (let ((s (make-screen 20 5)))
        (setf (cl-tmux/terminal/types:screen-scrollback s)
              (loop repeat 10 collect (make-array 0)))
        (funcall fn s)
        (expect (= 0 (screen-copy-offset s))))))

  ;; Scroll-down functions (and copy-mode-bottom) are no-ops when not in copy mode (offset stays 0).
  (it "copy-mode-scroll-down-noop-outside-copy-mode-table"
    (dolist (fn '(cl-tmux/commands::copy-mode-page-down
                  cl-tmux/commands::copy-mode-half-page-down
                  cl-tmux/commands::copy-mode-scroll-down-line
                  cl-tmux/commands::copy-mode-bottom))
      (let ((s (make-screen 20 5)))
        (setf (cl-tmux/terminal/types:screen-copy-offset s) 0)
        (funcall fn s)
        (expect (= 0 (screen-copy-offset s))))))

  ;;; ── copy-mode-word-forward / word-backward / word-end ──────────────────────

  ;; copy-mode-word-forward moves the cursor to the start of the next word.
  (it "copy-mode-word-forward-jumps-to-next-word"
    (let ((s (copy-mode-screen :w 40 :content "hello world")))
      ;; Cursor at col 0 (start of "hello")
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
      (cl-tmux/commands::copy-mode-word-forward s)
      ;; Should land at col 6 (start of "world")
      (expect (= 6 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))))

  ;; copy-mode-word-backward moves the cursor to the start of the previous word.
  (it "copy-mode-word-backward-jumps-to-prev-word-start"
    (let ((s (copy-mode-screen :w 40 :content "hello world")))
      ;; Cursor in the middle of "world" at col 8
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 8))
      (cl-tmux/commands::copy-mode-word-backward s)
      ;; Should land at col 6 (start of "world")
      (expect (= 6 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))))

  ;; Word-motion functions leave the column unchanged outside copy mode.
  (it "copy-mode-word-motion-noop-outside-copy-mode-table"
    (dolist (fn '(cl-tmux/commands::copy-mode-word-forward
                  cl-tmux/commands::copy-mode-word-end
                  cl-tmux/commands::copy-mode-word-backward))
      (let ((s (make-screen 20 5)))
        (feed s "hello world")
        (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
        (funcall fn s)
        (expect (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s)))))))

  ;; copy-mode-word-end moves the cursor to the last character of the current or next word.
  (it "copy-mode-word-end-jumps-to-end-of-word"
    (let ((s (copy-mode-screen :w 40 :content "hello world")))
      ;; Cursor at col 0 (start of "hello")
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
      (cl-tmux/commands::copy-mode-word-end s)
      ;; Should land at col 4 (last char of "hello")
      (expect (= 4 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))))

  ;;; ── copy-mode-top / copy-mode-bottom ────────────────────────────────────────

  ;; copy-mode-top scrolls the viewport to the oldest scrollback line.
  (it "copy-mode-top-jumps-to-max-scrollback"
    (let ((s (%screen-with-scrollback 10)))
      (cl-tmux/commands::copy-mode-top s)
      (expect (= 10 (screen-copy-offset s)))))

  ;; copy-mode-bottom scrolls back to offset 0 (live view).
  (it "copy-mode-bottom-returns-to-live-view"
    (let ((s (%screen-with-scrollback 10)))
      ;; First scroll to top
      (cl-tmux/commands::copy-mode-top s)
      (expect (= 10 (screen-copy-offset s)))
      ;; Then jump to bottom
      (cl-tmux/commands::copy-mode-bottom s)
      (expect (= 0 (screen-copy-offset s))))))
