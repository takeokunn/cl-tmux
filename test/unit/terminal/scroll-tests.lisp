(in-package #:cl-tmux/test)

;;;; Tests for scroll.lisp, erase.lisp, and edit.lisp terminal operations.
;;;; Suites: scroll-ops, erase, scroll-region, delete-insert-chars.

;;; ── SUITE: scroll-ops ───────────────────────────────────────────────────────
;;;
;;; Direct tests for scroll-up-one and scroll-down-one (defined in scroll.lisp).

(def-suite scroll-ops
  :description "Direct calls to scroll-up-one and scroll-down-one"
  :in terminal-suite)
(in-suite scroll-ops)

(test scroll-up-one-pushes-to-scrollback
  "scroll-up-one adds the displaced top row to the scrollback buffer."
  (with-screen (s 5 3)
    (feed s "hello")
    (cl-tmux/terminal/actions:scroll-up-one s)
    (is (= 1 (length (cl-tmux/terminal/types:screen-scrollback s)))
        "scrollback should have 1 entry after one scroll")
    (let ((row (first (cl-tmux/terminal/types:screen-scrollback s))))
      (is (char= #\h (cell-char (aref row 0)))
          "scrollback row 0 should start with 'h'"))))

(test scroll-up-one-caps-at-max-scrollback
  "scroll-up-one trims the scrollback to the effective history-limit.
   trim-scroll-history honours the 'history-limit' option (default 2000)
   which supersedes +max-scrollback-lines+ (1000) at runtime."
  (let* ((cap (or (cl-tmux/options:get-option "history-limit")
                  cl-tmux/config:+max-scrollback-lines+)))
    (with-screen (s 5 3)
      (setf (cl-tmux/terminal/types:screen-scrollback s)
            (loop repeat cap
                  collect (make-array 5 :initial-element
                                      (cl-tmux/terminal/types:blank-cell))))
      (cl-tmux/terminal/actions:scroll-up-one s)
      (is (<= (length (cl-tmux/terminal/types:screen-scrollback s)) cap)
          "scrollback must not exceed the effective history-limit (~D)" cap))))

(test scroll-down-one-inserts-blank-top-row
  "scroll-down-one moves content down; the new top row is blank."
  (with-screen (s 5 3)
    (feed s "hi")
    (cl-tmux/terminal/actions:scroll-down-one s)
    (is (row-blank-p s 0) "row 0 must be blank after scroll-down-one")
    (is (char= #\h (char-at s 0 1)) "old row 0 content must be on row 1")))

;;; ── SUITE: erase ────────────────────────────────────────────────────────────

(def-suite erase
  :description "ED (erase display) and EL (erase line) modes"
  :in terminal-suite)
(in-suite erase)

(defun fill-screen (screen)
  "Fill every cell of SCREEN with 'X' and return SCREEN."
  (dotimes (y (screen-height screen) screen)
    (dotimes (x (screen-width screen))
      (feed screen "X"))))

(test ed-0
  "ESC[J erases from the cursor position to the end of the display."
  (with-screen (s 5 3)
    (fill-screen s)
    (feed s (esc "[2;3H"))   ; cursor at (2, 1)
    (feed s (esc "[0J"))     ; erase to end
    ;; row 0 must be fully filled
    (is (string= "XXXXX" (row-string s 0)))
    ;; cells from cursor position onwards on row 1 must be blank
    (is (char= #\Space (char-at s 2 1)))
    (is (char= #\Space (char-at s 4 1)))
    ;; row 2 must be fully blank
    (is (row-blank-p s 2))))

(test ed-1
  "ESC[1J erases from the start of the display to the cursor (inclusive)."
  (with-screen (s 5 3)
    (fill-screen s)
    (feed s (esc "[2;3H"))   ; cursor at (2, 1)
    (feed s (esc "[1J"))
    ;; row 0 must be blank
    (is (row-blank-p s 0))
    ;; cells up to and including cursor on row 1 must be blank
    (is (char= #\Space (char-at s 0 1)))
    (is (char= #\Space (char-at s 2 1)))
    ;; cell after cursor on row 1 is still filled
    (is (char= #\X (char-at s 3 1)))))

(test ed-2
  "ESC[2J erases the entire display."
  (with-screen (s 5 3)
    (fill-screen s)
    (feed s (esc "[2J"))
    (dotimes (y 3)
      (is (row-blank-p s y) "row ~D not blank after ED 2" y))))

(test el-0
  "ESC[K erases from the cursor to the end of the current line."
  (with-screen (s 10 2)
    (feed s "abcdefghij")        ; fill row 0
    (feed s (esc "[1;5H"))       ; cursor at (4, 0)
    (feed s (esc "[0K"))
    (is (string= "abcd" (row-string s 0 :end 4)))
    (is (char= #\Space (char-at s 4 0)))
    (is (char= #\Space (char-at s 9 0)))))

(test el-1
  "ESC[1K erases from the start of the line to the cursor (inclusive)."
  (with-screen (s 10 2)
    (feed s "abcdefghij")
    (feed s (esc "[1;4H"))       ; cursor at (3, 0)
    (feed s (esc "[1K"))
    (is (char= #\Space (char-at s 0 0)))
    (is (char= #\Space (char-at s 3 0)))
    (is (char= #\e (char-at s 4 0)))))

(test el-2
  "ESC[2K erases the entire current line."
  (with-screen (s 10 2)
    (feed s "abcdefghij")
    (feed s (esc "[1;5H"))
    (feed s (esc "[2K"))
    (is (row-blank-p s 0))
    ;; cursor y unchanged
    (is (= 0 (screen-cursor-y s)))))

;;; ── SUITE: scroll-region ────────────────────────────────────────────────────

(def-suite scroll-region
  :description "Scrolling, DECSTBM, reverse index, IL/DL"
  :in terminal-suite)
(in-suite scroll-region)

(test scroll-auto
  "Writing a 4th line into a 3-row screen scrolls the content up."
  (with-screen (s 5 3)
    (feed s (format nil "L1~C~CL2~C~CL3~C~CL4"
                    #\Return #\Linefeed
                    #\Return #\Linefeed
                    #\Return #\Linefeed))
    ;; After one scroll: row 0 = old row 1 = "L2", row 2 = "L4".
    (check-row s 0 "L2")
    (check-row s 2 "L4")))

(test decstbm
  "DECSTBM restricts scrolling to the specified region (rows 2-3 of 5)."
  (with-screen (s 5 5)
    ;; Write one identifiable line per row.
    (feed s (format nil "R0~C~CR1~C~CR2~C~CR3~C~CR4"
                    #\Return #\Linefeed
                    #\Return #\Linefeed
                    #\Return #\Linefeed
                    #\Return #\Linefeed))
    ;; Set scroll region to rows 2-4 (1-based: ESC[2;4r).
    (feed s (esc "[2;4r"))      ; scroll region = rows 1-3 (0-based)
    ;; Now move into the region and force a scroll.
    (feed s (esc "[4;1H"))      ; cursor to row 4 (0-based 3), col 1
    (feed s (format nil "~C~CNR" #\Return #\Linefeed))
    ;; Row 0 must be untouched.
    (is (string= "R0" (row-string s 0 :end 2))
        "row 0 should be untouched, got ~S" (row-string s 0 :end 2))))

(test reverse-index
  "ESC M at the top of the scroll region scrolls the region down."
  (with-screen (s 5 3)
    ;; Fill rows with identifiable content.
    (feed s (format nil "AA~C~CBB~C~CCC"
                    #\Return #\Linefeed
                    #\Return #\Linefeed))
    ;; Move cursor to row 0 (top) and send RI.
    (feed s (esc "[1;1H"))   ; cursor home
    (feed s (esc "M"))       ; ESC M = RI
    ;; The scroll region shifts down: old row 0 ("AA") should now be at row 1.
    (is (string= "AA" (row-string s 1 :end 2))
        "after RI, old row 0 should be at row 1; got ~S" (row-string s 1 :end 2))
    ;; New row 0 should be blank.
    (is (row-blank-p s 0) "new row 0 should be blank after RI")))

(test il-insert-lines
  "ESC[2L (insert 2 lines) pushes existing content down."
  (with-screen (s 5 4)
    (feed s (format nil "AA~C~CBB~C~CCC~C~CDD"
                    #\Return #\Linefeed
                    #\Return #\Linefeed
                    #\Return #\Linefeed))
    ;; Move to row 1 and insert 2 lines.
    (feed s (esc "[2;1H"))   ; cursor to row 2 (0-based 1)
    (feed s (esc "[2L"))     ; insert 2 lines
    ;; Row 0 untouched; rows 1-2 blank; old row 1 ("BB") now at row 3.
    (check-row s 0 "AA")
    (is (row-blank-p s 1))
    (is (row-blank-p s 2))
    (check-row s 3 "BB")))

(test dl-delete-lines
  "ESC[2M (delete 2 lines) pulls content up."
  (with-screen (s 5 4)
    (feed s (format nil "AA~C~CBB~C~CCC~C~CDD"
                    #\Return #\Linefeed
                    #\Return #\Linefeed
                    #\Return #\Linefeed))
    ;; Move to row 1 and delete 2 lines.
    (feed s (esc "[2;1H"))
    (feed s (esc "[2M"))
    ;; Row 0 untouched; old row 3 ("DD") shifts up to row 1; rows 2-3 blank.
    (check-row s 0 "AA")
    (check-row s 1 "DD")
    (is (row-blank-p s 2))
    (is (row-blank-p s 3))))

;;; ── SUITE: delete/insert characters (DCH / ICH) ─────────────────────────────
;;;
;;; Driven via the real CSI parser path: CSI n P (DCH) shifts the tail left
;;; and blanks the vacated end; CSI n @ (ICH) shifts the tail right and blanks
;;; the gap.  We also exercise the n >= width edge.

(def-suite delete-insert-chars
  :description "DCH (CSI P) and ICH (CSI @) via the CSI parser"
  :in terminal-suite)
(in-suite delete-insert-chars)

(test dch-shifts-left-and-blanks-tail
  "CSI 2 P at column 0 deletes 'ab' from 'abcde', shifting 'cde' left and
   blanking the two vacated cells at the end of the line."
  (with-screen (s 8 2)
    (feed s "abcde")
    (feed s (esc "[1;1H"))     ; cursor home (col 0, row 0)
    (feed s (csi "2" #\P))     ; DCH 2
    (is (char= #\c (char-at s 0 0)) "col0 should be 'c', got ~C" (char-at s 0 0))
    (is (char= #\d (char-at s 1 0)) "col1 should be 'd', got ~C" (char-at s 1 0))
    (is (char= #\e (char-at s 2 0)) "col2 should be 'e', got ~C" (char-at s 2 0))
    ;; The original 'cde' occupied cols 2..4; after a 2-shift the tail blanks.
    (is (char= #\Space (char-at s 3 0)) "col3 should be blank")
    (is (char= #\Space (char-at s 4 0)) "col4 should be blank")))

(test dch-at-midline
  "CSI 1 P at a non-zero column deletes one char and shifts the rest left."
  (with-screen (s 8 2)
    (feed s "abcde")
    (feed s (esc "[1;2H"))     ; cursor at col 1 (1-based col 2)
    (feed s (csi "1" #\P))     ; delete 'b'
    (is (string= "acde" (row-string s 0 :end 4))
        "expected 'acde', got ~S" (row-string s 0 :end 4))
    (is (char= #\Space (char-at s 4 0)) "vacated last cell should be blank")))

(test dch-default-param-deletes-one
  "CSI P with no parameter deletes a single character (p1* defaults to 1)."
  (with-screen (s 8 2)
    (feed s "abcde")
    (feed s (esc "[1;1H"))
    (feed s (csi "" #\P))      ; DCH default = 1
    (is (string= "bcde" (row-string s 0 :end 4))
        "expected 'bcde', got ~S" (row-string s 0 :end 4))))

(test dch-n-ge-width-clears-from-cursor
  "CSI n P with n >= remaining width blanks the whole line from the cursor.
   delete-chars caps the blank-fill at (max cx (- w n)); when n >= w the shift
   loop runs empty and every cell from cursor to end is blanked."
  (with-screen (s 5 2)
    (feed s "abcde")
    (feed s (esc "[1;1H"))     ; cursor at col 0
    (feed s (csi "9" #\P))     ; DCH 9 >= width 5
    (dotimes (x 5)
      (is (char= #\Space (char-at s x 0))
          "col ~D should be blank after oversized DCH, got ~C"
          x (char-at s x 0)))))

(test ich-shifts-right-and-blanks-gap
  "CSI 2 @ at column 0 inserts two blanks, pushing 'abcde' right; the trailing
   chars shifted past the right margin are lost."
  (with-screen (s 5 2)
    (feed s "abcde")
    (feed s (esc "[1;1H"))     ; cursor home
    (feed s (csi "2" #\@))     ; ICH 2
    ;; Two blanks inserted at cols 0,1; 'abc' shifts to cols 2,3,4; 'de' lost.
    (is (char= #\Space (char-at s 0 0)) "col0 should be blank gap")
    (is (char= #\Space (char-at s 1 0)) "col1 should be blank gap")
    (is (char= #\a (char-at s 2 0)) "col2 should be 'a', got ~C" (char-at s 2 0))
    (is (char= #\b (char-at s 3 0)) "col3 should be 'b', got ~C" (char-at s 3 0))
    (is (char= #\c (char-at s 4 0)) "col4 should be 'c', got ~C" (char-at s 4 0))))

(test ich-at-midline
  "CSI 1 @ at a non-zero column inserts one blank and pushes the tail right."
  (with-screen (s 6 2)
    (feed s "abcde")
    (feed s (esc "[1;3H"))     ; cursor at col 2 (1-based col 3)
    (feed s (csi "1" #\@))     ; insert one blank at col 2
    (is (char= #\a (char-at s 0 0)) "col0 unchanged 'a'")
    (is (char= #\b (char-at s 1 0)) "col1 unchanged 'b'")
    (is (char= #\Space (char-at s 2 0)) "col2 should be the inserted blank")
    (is (char= #\c (char-at s 3 0)) "col3 should be shifted 'c', got ~C"
        (char-at s 3 0))
    (is (char= #\d (char-at s 4 0)) "col4 should be shifted 'd', got ~C"
        (char-at s 4 0))))

(test ich-n-ge-width-blanks-from-cursor
  "CSI n @ with n >= remaining width blanks every cell from the cursor; the
   insert-chars blank-fill is capped at (min (1- w) (+ cx n -1))."
  (with-screen (s 5 2)
    (feed s "abcde")
    (feed s (esc "[1;1H"))
    (feed s (csi "9" #\@))     ; ICH 9 >= width 5
    (dotimes (x 5)
      (is (char= #\Space (char-at s x 0))
          "col ~D should be blank after oversized ICH, got ~C"
          x (char-at s x 0)))))

;;; ── SUITE: direct-row-primitives ────────────────────────────────────────────
;;;
;;; Coverage gap: %copy-row and %clear-row are used by scroll and edit operations
;;; but were previously only tested indirectly.  These tests call them directly.

(def-suite direct-row-primitives
  :description "Direct calls to %copy-row and %clear-row row primitives"
  :in terminal-suite)
(in-suite direct-row-primitives)

(test copy-row-copies-all-cells
  "%copy-row copies every cell from the source row to the destination row."
  (with-screen (s 5 3)
    (feed s "hello")                       ; row 0 = "hello"
    (cl-tmux/terminal/actions::%copy-row s 1 0)  ; copy row 0 to row 1
    (is (string= "hello" (row-string s 1))
        "row 1 must equal row 0 after %copy-row, got ~S"
        (row-string s 1))))

(test clear-row-blanks-all-cells
  "%clear-row replaces every cell in the target row with a blank cell."
  (with-screen (s 5 3)
    (feed s "hello")                       ; row 0 = "hello"
    (cl-tmux/terminal/actions::%clear-row s 0)
    (is (row-blank-p s 0) "row 0 must be blank after %clear-row")))

(test trim-scroll-history-caps-at-limit
  "trim-scroll-history removes entries beyond the effective history-limit."
  (with-screen (s 5 3)
    (let ((cap 5))
      ;; Pre-populate scrollback beyond the cap
      (setf (cl-tmux/terminal/types:screen-scrollback s)
            (loop repeat (+ cap 3)
                  collect (make-array 5 :initial-element
                                        (cl-tmux/terminal/types:blank-cell))))
      ;; Install a temporary limit function
      (let ((cl-tmux/terminal/actions:*history-limit-fn* (lambda () cap)))
        (cl-tmux/terminal/actions:trim-scroll-history s))
      (is (<= (length (cl-tmux/terminal/types:screen-scrollback s)) cap)
          "scrollback must not exceed cap (~D) after trim-scroll-history" cap))))

;;; ── SUITE: direct-action-erase ───────────────────────────────────────────────
;;;
;;; These tests call erase-region, erase-display, erase-line directly rather
;;; than through the CSI parser path, targeting edge cases that high-level
;;; tests are unlikely to assert explicitly.

(def-suite direct-action-erase
  :description "Direct calls to erase-region, erase-display (mode 3), erase-line"
  :in terminal-suite)
(in-suite direct-action-erase)

(test erase-region-clears-span-across-rows
  "erase-region blanks a linear span from (x0,y0) to (x1,y1) inclusive."
  (with-screen (s 5 4)
    (feed s "aabbccddee")           ; rows 0 and 1 filled
    ;; Erase from (3,0) to (1,1): last 2 cells of row 0 + first 2 of row 1.
    (cl-tmux/terminal/actions:erase-region s 3 0 1 1)
    (is (char= #\a (char-at s 0 0)) "col 0 row 0 must be preserved")
    (is (char= #\a (char-at s 1 0)) "col 1 row 0 must be preserved")
    (is (char= #\b (char-at s 2 0)) "col 2 row 0 must be preserved")
    (is (char= #\Space (char-at s 3 0)) "col 3 row 0 must be erased")
    (is (char= #\Space (char-at s 4 0)) "col 4 row 0 must be erased")
    (is (char= #\Space (char-at s 0 1)) "col 0 row 1 must be erased")
    (is (char= #\Space (char-at s 1 1)) "col 1 row 1 must be erased")))

(test erase-display-mode-3-clears-scrollback
  "erase-display mode 3 (ED 3) also clears the scrollback buffer."
  (with-screen (s 5 3)
    ;; Build up some scrollback by feeding lines that force scrolling.
    (feed s (format nil "L0~C~CL1~C~CL2~C~CL3" #\Return #\Linefeed
                                                  #\Return #\Linefeed
                                                  #\Return #\Linefeed))
    (is (plusp (length (cl-tmux/terminal/types:screen-scrollback s)))
        "scrollback must be non-empty after filling the screen")
    ;; Mode 3 = clear screen + clear scrollback
    (cl-tmux/terminal/actions:erase-display s 3)
    (is (null (cl-tmux/terminal/types:screen-scrollback s))
        "scrollback must be NIL after erase-display mode 3")))

(test erase-line-mode-0-erases-to-end
  "erase-line mode 0 erases from the cursor column to the end of the line."
  (with-screen (s 10 5)
    (feed s "hello")
    ;; Move cursor to col 2 via cursor-left.
    (cl-tmux/terminal/actions:cursor-left s 3)   ; cursor at col 2
    (cl-tmux/terminal/actions:erase-line s 0)
    (is (char= #\h (char-at s 0 0)) "col 0 must be preserved")
    (is (char= #\e (char-at s 1 0)) "col 1 must be preserved")
    (is (char= #\Space (char-at s 2 0)) "col 2 must be erased")
    (is (char= #\Space (char-at s 4 0)) "col 4 must be erased")))
