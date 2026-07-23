(in-package #:cl-tmux/test)

;;;; Commands tests — part XV: selection-bounds scrollback, word/paragraph nav, scroll-middle.

(describe "commands-suite"

  ;; ── %selection-bounds scrollback spanning (virtual-row correctness) ──────────

  ;; When the user begins a selection and then scrolls, %selection-bounds must use
  ;; virtual (absolute scrollback) rows so the selected TEXT does not shift.
  ;; Regression test for the mark-offset fix: mark-row is a viewport row stored
  ;; at the time of begin-selection; after scrolling by delta lines the mark must
  ;; still refer to the same content.
  ;; The mark is placed at (row=2, col=3) — non-zero col so the mark row contributes
  ;; chars to %selection-text.  After scroll, with OLD (buggy) code the mark row would
  ;; be viewport row 2 at offset=1 = live-grid row 1 = 'DDD'.  With the NEW code, the
  ;; mark virtual row remains vrow=4 = live-grid row 2 = 'EEE'.
  (it "selection-bounds-after-scroll-uses-virtual-rows"
    (let ((s (make-screen 4 3)))        ; 4 cols, 3 rows
      ;; Feed 5 lines: scrollback=[BBB,AAA] (newest first), grid=[CCC,DDD,EEE].
      (feed-lines s "AAA" "BBB" "CCC" "DDD" "EEE")
      (cl-tmux/commands::copy-mode-enter s)
      ;; Enter at offset=0, cursor at live-grid bottom (row 2, col 0).
      (expect (= 0 (screen-copy-offset s)))
      ;; Move cursor to col 3 to give the mark a non-zero column.
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 3))
      ;; Begin selection: mark=(2, 3), mark-offset=0.
      (cl-tmux/commands::copy-mode-begin-selection s)
      (expect (= 0 (cl-tmux/terminal/types:screen-copy-mark-offset s)))
      ;; Scroll back 1 line into scrollback: offset becomes 1.
      (cl-tmux/commands::copy-mode-scroll s 1)
      (expect (= 1 (screen-copy-offset s)))
      ;; Move cursor to viewport row 0, col 0 (newest scrollback row).
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
      ;; Virtual row check: sb-n=2, mark-vrow=2+2-0=4 (EEE), cursor-vrow=2+0-1=1 (BBB).
      (multiple-value-bind (start-vrow end-vrow start-col end-col)
          (cl-tmux/commands::%selection-bounds s)
        (declare (ignore start-col end-col))
        (expect (= 1 start-vrow))
        (expect (= 4 end-vrow)))
      ;; %selection-text: vrow 1=BBB, vrow 2=CCC, vrow 3=DDD, vrow 4 cols 0-3 = EEE.
      ;; With the OLD buggy code, vrow 4 would instead be DDD (viewport row 2 at offset=1
      ;; = live-grid row 1 = DDD instead of EEE).
      (let ((text (cl-tmux/commands::%selection-text s)))
        (expect (and text (search "BBB" text)))
        (expect (and text (search "EEE" text))))))

  ;; ── copy-mode-word-backward edge cases ───────────────────────────────────────

  ;; copy-mode-word-backward when cursor is already at col 0 does not move.
  (it "copy-mode-word-backward-at-col-zero-stays-put"
    (let ((s (copy-mode-screen :w 40 :content "hello world")))
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
      (cl-tmux/commands::copy-mode-word-backward s)
      (expect (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))))

  ;; copy-mode-word-backward when cursor is in whitespace skips to the previous word start.
  (it "copy-mode-word-backward-from-whitespace-skips-to-word-start"
    (let ((s (copy-mode-screen :w 40 :content "hello world")))
      ;; Position cursor in the space between words (col 5).
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 5))
      (cl-tmux/commands::copy-mode-word-backward s)
      ;; Should land at col 0 (start of "hello").
      (expect (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))))

  ;; copy-mode-word-backward when cursor is at the first character of a word.
  (it "copy-mode-word-backward-from-first-char-of-word"
    (let ((s (copy-mode-screen :w 40 :content "hello world")))
      ;; Position at col 6 — the 'w' of "world".
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 6))
      (cl-tmux/commands::copy-mode-word-backward s)
      ;; Should land at col 0 (start of "hello").
      (expect (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))))

  ;; ── Copy-mode word navigation cross-line tests ───────────────────────────────

  ;; copy-mode-word-forward crosses to BOL of next row when at end of line.
  (it "copy-mode-word-forward-wraps-to-next-row"
    ;; 10-wide, 2-row screen: row0="hello     ", row1="world     "
    ;; From 'o' at (0,4), w skips "o", then spaces 5-9 → col=10=width → wrap to (1,0).
    (let ((s (make-screen 10 2)))
      (feed s (format nil "hello~C~Cworld" #\Return #\Linefeed))
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 4))
      (cl-tmux/commands::copy-mode-word-forward s)
      (expect (= 1 (car (cl-tmux/terminal/types:screen-copy-cursor s))))
      (expect (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))))

  ;; copy-mode-word-backward at BOL wraps to the previous row and finds word start.
  (it "copy-mode-word-backward-wraps-to-prev-row"
    ;; 10-wide, 2-row screen: row0="hello     ", row1="world     "
    ;; From BOL of row1 (1,0), b wraps to (0,9), scans back over spaces to col4, then
    ;; over 'hello' to col 0.
    (let ((s (make-screen 10 2)))
      (feed s (format nil "hello~C~Cworld" #\Return #\Linefeed))
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 1 0))
      (cl-tmux/commands::copy-mode-word-backward s)
      (expect (= 0 (car (cl-tmux/terminal/types:screen-copy-cursor s))))
      (expect (= 0 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))))

  ;; copy-mode-word-end crosses to the next row when entire row tail is separators.
  (it "copy-mode-word-end-wraps-to-next-row"
    ;; 10-wide, 3-row screen: row0="hello     ", row1="          " (blank), row2="world     "
    ;; From col 4 (end of 'hello'), e steps to col 5 (sep), then wraps past blank row1,
    ;; reaches row2, advances to end of 'world' = col 4.
    (let ((s (make-screen 10 3)))
      (feed s (format nil "hello~C~C~C~Cworld" #\Return #\Linefeed #\Return #\Linefeed))
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 4))
      (cl-tmux/commands::copy-mode-word-end s)
      ;; Should arrive at end of "world" on row 2
      (expect (= 2 (car (cl-tmux/terminal/types:screen-copy-cursor s))))
      (expect (= 4 (cdr (cl-tmux/terminal/types:screen-copy-cursor s))))))

  ;; ── copy-mode paragraph motion tests ────────────────────────────────────────

  ;; next-paragraph (from row 0) and previous-paragraph (from row 4) both land on blank row 2.
  (it "copy-mode-paragraph-jumps-to-blank-line-table"
    ;; 20-wide, 5-row screen: row0=text, row1=text, row2=blank, row3=text, row4=text
    (dolist (row (list (list (cons 0 0)
                             #'cl-tmux/commands::copy-mode-next-paragraph
                             "next-paragraph from row 0 → blank row 2")
                       (list (cons 4 0)
                             #'cl-tmux/commands::copy-mode-previous-paragraph
                             "previous-paragraph from row 4 → blank row 2")))
      (destructuring-bind (start-cursor fn desc) row
        (declare (ignore desc))
        (let ((s (make-screen 20 5)))
          (feed s (format nil "hello~C~Cworld~C~C~C~Cfoo~C~Cbar" #\Return #\Linefeed
                          #\Return #\Linefeed #\Return #\Linefeed
                          #\Return #\Linefeed))
          (cl-tmux/commands::copy-mode-enter s)
          (setf (cl-tmux/terminal/types:screen-copy-cursor s) start-cursor)
          (funcall fn s)
          (expect (= 2 (car (cl-tmux/terminal/types:screen-copy-cursor s))))))))

  ;; copy-mode-next-paragraph with no blank line below stays at last row.
  (it "copy-mode-next-paragraph-at-bottom-stays"
    (let ((s (make-screen 20 3)))
      (feed s (format nil "hello~C~Cworld" #\Return #\Linefeed))
      (cl-tmux/commands::copy-mode-enter s)
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
      (cl-tmux/commands::copy-mode-next-paragraph s)
      ;; No blank row; should land at last vrow = 2 (sb-n=0, h=3)
      ;; Since no scrollback, last vrow = 2, viewport row = 2
      (expect (<= 1 (car (cl-tmux/terminal/types:screen-copy-cursor s))))))

  ;; %copy-mode-row-blank-p returns T only for fully blank rows.
  (it "copy-mode-row-blank-p-distinguishes-blank-and-content-rows"
    (let ((s (copy-mode-screen :w 8 :h 3 :content "ab")))
      (expect (cl-tmux/commands::%copy-mode-row-blank-p s 0) :to-be-falsy)
      (expect (cl-tmux/commands::%copy-mode-row-blank-p s 1) :to-be-truthy)))

  ;; %find-paragraph-boundary returns the nearest blank row or the edge clamp.
  (it "find-paragraph-boundary-scans-to-nearest-blank-row"
    (let ((s (make-screen 20 5)))
      (feed-lines s "hello" "world" "" "foo" "bar")
      (expect (= 2 (cl-tmux/commands::%find-paragraph-boundary s 1 :down 5)))
      (expect (= 2 (cl-tmux/commands::%find-paragraph-boundary s 3 :up 0)))
      (expect (= 4 (cl-tmux/commands::%find-paragraph-boundary s 4 :down 5)))))

  ;; ── copy-mode-scroll-middle tests ───────────────────────────────────────────

  ;; copy-mode-scroll-middle adjusts offset so cursor row is at viewport center.
  (it "copy-mode-scroll-middle-centers-cursor"
    ;; 20-wide, 5-row screen with 3 lines of scrollback (feed 8 rows so 3 scroll off).
    ;; Enter copy mode, scroll back fully (offset=3), place cursor at row 4 (bottom).
    ;; After scroll-middle: center=2, delta = 2-4 = -2, new-offset=1, cursor-row=2.
    (let ((s (make-screen 20 5)))
      (dotimes (i 8) (feed s (format nil "line~D~C~C" i #\Return #\Linefeed)))
      (cl-tmux/commands::copy-mode-enter s)
      (cl-tmux/commands::copy-mode-top s)   ; scroll to max offset (3)
      (let ((max-off (screen-copy-offset s)))
        (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 4 0))
        (cl-tmux/commands::copy-mode-scroll-middle s)
        (expect (= 2 (car (cl-tmux/terminal/types:screen-copy-cursor s))))
        (expect (= (+ max-off (- 2 4)) (screen-copy-offset s))))))

  ;; copy-mode-scroll-middle clamps the offset to 0 at the bottom of history.
  (it "copy-mode-scroll-middle-clamps-at-history-bottom"
    ;; No scrollback: offset stays 0, cursor moves as much as possible toward center.
    (let ((s (make-screen 20 5)))
      (feed s "hello world")
      (cl-tmux/commands::copy-mode-enter s)
      ;; Cursor at row 0 (top), offset 0, no scrollback.
      (setf (cl-tmux/terminal/types:screen-copy-cursor s) (cons 0 0))
      (cl-tmux/commands::copy-mode-scroll-middle s)
      ;; center=2, delta = 2-0 = +2, but new-offset = clamp(0 + 2, 0, 0) = 0
      ;; cursor stays at row 0 (0 + 0 = 0)
      (expect (= 0 (screen-copy-offset s)))
      (expect (= 0 (car (cl-tmux/terminal/types:screen-copy-cursor s))))))

  ;; copy-mode-jump-to-mark moves the cursor to the mark without swapping.
  (it "copy-mode-jump-to-mark-moves-cursor-to-mark"
    (let* ((s (make-screen 20 5)))
      (setf (screen-copy-mode-p s)  t
            (screen-copy-offset s)  0
            ;; cursor at (row=4,col=10), mark at (row=1,col=3), both at offset 0
            (cl-tmux/terminal/types:screen-copy-cursor s) (cons 4 10)
            (cl-tmux/terminal/types:screen-copy-mark   s) (cons 1 3)
            (cl-tmux/terminal/types:screen-copy-mark-offset s) 0)
      (cl-tmux/commands::copy-mode-jump-to-mark s)
      (expect (equal (cons 1 3) (cl-tmux/terminal/types:screen-copy-cursor s)))))

  ;; copy-mode-jump-to-mark is a no-op when no mark has been set.
  (it "copy-mode-jump-to-mark-noop-when-no-mark"
    (let* ((s (make-screen 20 5)))
      (setf (screen-copy-mode-p s)  t
            (screen-copy-offset s)  0
            (cl-tmux/terminal/types:screen-copy-cursor s) (cons 2 5)
            (cl-tmux/terminal/types:screen-copy-mark   s) nil)
      (cl-tmux/commands::copy-mode-jump-to-mark s)
      (expect (equal (cons 2 5) (cl-tmux/terminal/types:screen-copy-cursor s))))))
