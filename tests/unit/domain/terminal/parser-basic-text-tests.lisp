(in-package #:cl-tmux/test)

;;;; parser tests — part C1: basic-text.

;;; ── SUITE: basic-text ───────────────────────────────────────────────────────

(describe "terminal-suite/basic-text"

  ;; Printing five ASCII characters places them in row 0 and advances cursor.
  (it "plain-text"
    (with-screen (s 20 5)
      (feed s "hello")
      (expect (string= "hello" (row-string s 0 :end 5)))
      (check-cursor s 5 0)))

  ;; CR+LF moves to column 0 of the next row.
  (it "crlf"
    (with-screen (s 20 5)
      (feed s "ab")
      (feed s (format nil "~C~C" #\Return #\Linefeed))
      (feed s "cd")
      (expect (string= "ab" (row-string s 0 :end 2)))
      (expect (string= "cd" (row-string s 1 :end 2)))
      (check-cursor s 2 1)))

  ;; A bare CR (#x0D) returns the cursor to column 0 on the same row, leaving
  ;; the already-written cells intact (overwrite begins at column 0).
  (it "carriage-return"
    (with-screen (s 20 5)
      (feed s "abc")                         ; cursor at (3, 0)
      (check-cursor s 3 0)
      (feed s (string #\Return))             ; CR → column 0, row unchanged
      (check-cursor s 0 0)
      ;; The previously written cells survive the CR.
      (expect (string= "abc" (row-string s 0 :end 3)))
      ;; Subsequent text overwrites from column 0.
      (feed s "XY")
      (expect (string= "XYc" (row-string s 0 :end 3)))
      (check-cursor s 2 0)))

  ;; CR after moving to a lower row resets the column to 0 but keeps the row.
  (it "carriage-return-keeps-row"
    (with-screen (s 20 5)
      (feed s (esc "[3;6H"))                 ; cursor → (5, 2)
      (check-cursor s 5 2)
      (feed s (string #\Return))             ; CR → column 0, still row 2
      (check-cursor s 0 2)))

  ;; A 4-wide screen wraps 'abcde' so row 0 = 'abcd', row 1 starts with 'e'.
  (it "line-wrap"
    (with-screen (s 4 3)
      (feed s "abcde")
      (expect (string= "abcd" (row-string s 0)))
      (expect (char= #\e (char-at s 0 1)))
      (check-cursor s 1 1)))

  ;; Backspace after 'abc' leaves the cursor at column 2.
  (it "backspace"
    (with-screen (s 10 2)
      (feed s "abc")
      (feed s (string #\Backspace))
      (check-cursor s 2 0)))

  ;; After 'a', a TAB advances to the next 8-column stop (column 8).
  (it "tab-stop"
    (with-screen (s 40 2)
      (feed s "a")
      (feed s (string #\Tab))
      (check-cursor s 8 0)))

  ;; Eight spaces bring the cursor to column 8; a TAB then jumps to column 16.
  (it "tab-already-at-stop"
    (with-screen (s 40 2)
      (feed s "        ")   ; 8 spaces → cursor at (8, 0)
      (feed s "a")          ; cursor at (9, 0)
      ;; back to col 8 manually so TAB fires to col 16
      (feed s (esc "[1;9H")) ; CUP row=1 col=9 (1-based) → (8, 0)
      (feed s (string #\Tab))
      (check-cursor s 16 0))))
