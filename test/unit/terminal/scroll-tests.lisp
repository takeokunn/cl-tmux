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

(test scroll-up-partial-region-does-not-push-to-scrollback
  "Scrolling within a partial scroll region (scroll-top > 0) must NOT add to the
   scrollback: only full-top-of-screen scrolling contributes to history, matching
   real tmux grid_scroll_history_up semantics."
  (with-screen (s 5 5)
    ;; Set scroll region to rows 2..4 (1-based), i.e. 0-based rows 1..3.
    (feed s (esc "[2;4r"))
    ;; Position cursor at row 4 (bottom of region, 0-based 3) and force a scroll.
    (feed s (esc "[4;1H"))
    (feed s (string #\Newline))
    (is (null (cl-tmux/terminal/types:screen-scrollback s))
        "partial scroll-region scrolling must not populate scrollback")))

(test scroll-up-alt-screen-does-not-push-to-scrollback
  "Scrolling in the alternate screen must not pollute the primary scrollback."
  (with-screen (s 5 3)
    (feed s "line0")
    ;; Enter alt screen, then force a scroll.
    (feed s (esc "[?1049h"))
    (feed s "altline0")
    (feed s (string #\Newline))
    (feed s (string #\Newline))
    (feed s (string #\Newline))
    ;; Alt screen may or may not scroll, but even if it does the scrollback must stay nil.
    (is (null (cl-tmux/terminal/types:screen-scrollback s))
        "alt-screen scrolling must not push to the primary scrollback")))

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

(test erase-display-erases-to-end-of-screen
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

(test erase-display-erases-from-start-to-cursor
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

(test erase-display-clears-entire-screen
  "ESC[2J erases the entire display."
  (with-screen (s 5 3)
    (fill-screen s)
    (feed s (esc "[2J"))
    (dotimes (y 3)
      (is (row-blank-p s y) "row ~D not blank after ED 2" y))))

(test scroll-on-clear-on-pushes-screen-to-history
  "With scroll-on-clear on, ESC[2J moves the visible content into the scrollback
   before erasing, so a full-screen clear stays in history."
  (let ((cl-tmux/terminal/actions::*scroll-on-clear-function* (lambda () t)))
    (with-screen (s 5 3)
      (fill-screen s)
      (is (null (cl-tmux/terminal/types:screen-scrollback s))
          "scrollback must be empty before the clear")
      (feed s (esc "[2J"))
      (is (= 3 (length (cl-tmux/terminal/types:screen-scrollback s)))
          "all 3 visible rows must be pushed to scrollback (got ~D)"
          (length (cl-tmux/terminal/types:screen-scrollback s)))
      (dotimes (y 3)
        (is (row-blank-p s y) "row ~D must be blank after the clear" y)))))

(test scroll-on-clear-off-discards-content
  "With scroll-on-clear off (no policy installed), ESC[2J erases without pushing to
   the scrollback — the existing default behaviour."
  (let ((cl-tmux/terminal/actions::*scroll-on-clear-function* nil))
    (with-screen (s 5 3)
      (fill-screen s)
      (feed s (esc "[2J"))
      (is (null (cl-tmux/terminal/types:screen-scrollback s))
          "scrollback must stay empty when scroll-on-clear is off"))))

(test scroll-on-clear-skips-alternate-screen
  "scroll-on-clear does not push to history on the alternate screen (no scrollback)."
  (let ((cl-tmux/terminal/actions::*scroll-on-clear-function* (lambda () t)))
    (with-screen (s 5 3)
      (feed s (esc "[?1049h"))      ; enter the alternate screen
      (fill-screen s)
      (feed s (esc "[2J"))
      (is (null (cl-tmux/terminal/types:screen-scrollback s))
          "an alt-screen clear must not push to the scrollback"))))

(test erase-line-erases-to-end-of-line
  "ESC[K erases from the cursor to the end of the current line."
  (with-screen (s 10 2)
    (feed s "abcdefghij")        ; fill row 0
    (feed s (esc "[1;5H"))       ; cursor at (4, 0)
    (feed s (esc "[0K"))
    (is (string= "abcd" (row-string s 0 :end 4)))
    (is (char= #\Space (char-at s 4 0)))
    (is (char= #\Space (char-at s 9 0)))))

(test erase-line-erases-from-start-to-cursor
  "ESC[1K erases from the start of the line to the cursor (inclusive)."
  (with-screen (s 10 2)
    (feed s "abcdefghij")
    (feed s (esc "[1;4H"))       ; cursor at (3, 0)
    (feed s (esc "[1K"))
    (is (char= #\Space (char-at s 0 0)))
    (is (char= #\Space (char-at s 3 0)))
    (is (char= #\e (char-at s 4 0)))))

(test erase-line-clears-entire-line
  "ESC[2K erases the entire current line."
  (with-screen (s 10 2)
    (feed s "abcdefghij")
    (feed s (esc "[1;5H"))
    (feed s (esc "[2K"))
    (is (row-blank-p s 0))
    ;; cursor y unchanged
    (is (= 0 (screen-cursor-y s)))))

;;; ── Direct erase-display tests covering guarded edge cases ──────────────────
;;;
;;; These call erase-display directly to exercise the edge at cy=0 for mode 1
;;; (the when guard in erase.lisp) and other paths not clearly covered by the
;;; high-level CSI path above.

(test erase-display-direct-mode-0-from-cy-zero
  "erase-display mode 0 with cursor at (0,0) erases the full screen."
  (with-screen (s 5 3)
    (fill-screen s)
    ;; Explicitly home cursor so mode 0 erases the entire screen.
    (setf (cl-tmux/terminal/types:screen-cursor-x s) 0
          (cl-tmux/terminal/types:screen-cursor-y s) 0)
    (cl-tmux/terminal/actions:erase-display s 0)
    (dotimes (y 3)
      (is (row-blank-p s y)
          "row ~D must be blank after erase-display mode 0 from (0,0)" y))))

(test erase-display-direct-mode-1-at-cy-zero-skips-above-rows
  "erase-display mode 1 with cy=0 erases only from (0,0) to cursor on row 0.
   The 'when (> cy 0)' guard in erase.lisp means no above-rows erase is attempted."
  (with-screen (s 5 3)
    (fill-screen s)
    ;; cursor at (2,0): mode 1 should blank cols 0-2 of row 0, leave rows 1-2 intact
    (setf (cl-tmux/terminal/types:screen-cursor-x s) 2
          (cl-tmux/terminal/types:screen-cursor-y s) 0)
    (cl-tmux/terminal/actions:erase-display s 1)
    (is (char= #\Space (char-at s 0 0)) "col 0 row 0 must be erased")
    (is (char= #\Space (char-at s 2 0)) "col 2 row 0 must be erased")
    (is (char= #\X     (char-at s 3 0)) "col 3 row 0 must be preserved")
    (is (string= "XXXXX" (row-string s 1)) "row 1 must be untouched")))

(test erase-display-direct-mode-2-clears-all-rows
  "erase-display mode 2 called directly blanks every row."
  (with-screen (s 5 3)
    (fill-screen s)
    (cl-tmux/terminal/actions:erase-display s 2)
    (dotimes (y 3)
      (is (row-blank-p s y) "row ~D must be blank after direct erase-display mode 2" y))))

;;; ── Direct erase-line tests for modes 1 and 2 ───────────────────────────────
;;;
;;; Coverage gap: modes 1 and 2 were only exercised through the CSI path.
;;; These call erase-line directly to give each mode an isolated assertion.

(test erase-line-direct-mode-1-erases-start-to-cursor
  "erase-line mode 1 called directly blanks from col 0 to the cursor (inclusive)."
  (with-screen (s 10 5)
    (feed s "hello")
    ;; cursor at col 3
    (setf (cl-tmux/terminal/types:screen-cursor-x s) 3
          (cl-tmux/terminal/types:screen-cursor-y s) 0)
    (cl-tmux/terminal/actions:erase-line s 1)
    (is (char= #\Space (char-at s 0 0)) "col 0 must be erased")
    (is (char= #\Space (char-at s 3 0)) "col 3 (cursor) must be erased")
    (is (char= #\o     (char-at s 4 0)) "col 4 must be preserved")))

(test erase-line-direct-mode-2-erases-entire-line
  "erase-line mode 2 called directly blanks the entire current line."
  (with-screen (s 10 5)
    (feed s "hello")
    (setf (cl-tmux/terminal/types:screen-cursor-y s) 0)
    (cl-tmux/terminal/actions:erase-line s 2)
    (is (row-blank-p s 0) "row 0 must be fully blank after erase-line mode 2")))

;;; ── SUITE: scroll-region ────────────────────────────────────────────────────

(def-suite scroll-region
  :description "Scrolling, DECSTBM, reverse index, IL/DL"
  :in terminal-suite)
(in-suite scroll-region)

(test scroll-auto
  "Writing a 4th line into a 3-row screen scrolls the content up."
  (with-screen (s 5 3)
    (feed-lines s "L1" "L2" "L3" "L4")
    ;; After one scroll: row 0 = old row 1 = "L2", row 2 = "L4".
    (check-row s 0 "L2")
    (check-row s 2 "L4")))

(test decstbm-restricts-scroll-to-region
  "DECSTBM restricts scrolling to the specified region (rows 2-3 of 5)."
  (with-screen (s 5 5)
    ;; Write one identifiable line per row.
    (feed-lines s "R0" "R1" "R2" "R3" "R4")
    ;; Set scroll region to rows 2-4 (1-based: ESC[2;4r).
    (feed s (esc "[2;4r"))      ; scroll region = rows 1-3 (0-based)
    ;; Now move into the region and force a scroll.
    (feed s (esc "[4;1H"))      ; cursor to row 4 (0-based 3), col 1
    (feed-lines s "" "NR")
    ;; Row 0 must be untouched.
    (is (string= "R0" (row-string s 0 :end 2))
        "row 0 should be untouched, got ~S" (row-string s 0 :end 2))))

(test reverse-index-scrolls-region-down
  "ESC M at the top of the scroll region scrolls the region down."
  (with-screen (s 5 3)
    ;; Fill rows with identifiable content.
    (feed-lines s "AA" "BB" "CC")
    ;; Move cursor to row 0 (top) and send RI.
    (feed s (esc "[1;1H"))   ; cursor home
    (feed s (esc "M"))       ; ESC M = RI
    ;; The scroll region shifts down: old row 0 ("AA") should now be at row 1.
    (is (string= "AA" (row-string s 1 :end 2))
        "after RI, old row 0 should be at row 1; got ~S" (row-string s 1 :end 2))
    ;; New row 0 should be blank.
    (is (row-blank-p s 0) "new row 0 should be blank after RI")))

(test il-insert-lines-pushes-content-down
  "ESC[2L (insert 2 lines) pushes existing content down."
  (with-screen (s 5 4)
    (feed-lines s "AA" "BB" "CC" "DD")
    ;; Move to row 1 and insert 2 lines.
    (feed s (esc "[2;1H"))   ; cursor to row 2 (0-based 1)
    (feed s (esc "[2L"))     ; insert 2 lines
    ;; Row 0 untouched; rows 1-2 blank; old row 1 ("BB") now at row 3.
    (check-row s 0 "AA")
    (is (row-blank-p s 1))
    (is (row-blank-p s 2))
    (check-row s 3 "BB")))

(test dl-delete-lines-pulls-content-up
  "ESC[2M (delete 2 lines) pulls content up."
  (with-screen (s 5 4)
    (feed-lines s "AA" "BB" "CC" "DD")
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
    ;; The original 'cde' occupied cols 2..4; after a 2-shift the tail blanks.
    (dolist (row '((#\c     0 "col0 should be 'c'")
                   (#\d     1 "col1 should be 'd'")
                   (#\e     2 "col2 should be 'e'")
                   (#\Space 3 "col3 should be blank")
                   (#\Space 4 "col4 should be blank")))
      (destructuring-bind (expected col desc) row
        (is (char= expected (char-at s col 0)) "~A" desc))))

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
    (dolist (row '((#\Space 0 "col0 should be blank gap")
                   (#\Space 1 "col1 should be blank gap")
                   (#\a     2 "col2 should be 'a'")
                   (#\b     3 "col3 should be 'b'")
                   (#\c     4 "col4 should be 'c'")))
      (destructuring-bind (expected col desc) row
        (is (char= expected (char-at s col 0)) "~A" desc))))

(test ich-at-midline
  "CSI 1 @ at a non-zero column inserts one blank and pushes the tail right."
  (with-screen (s 6 2)
    (feed s "abcde")
    (feed s (esc "[1;3H"))     ; cursor at col 2 (1-based col 3)
    (feed s (csi "1" #\@))     ; insert one blank at col 2
    (dolist (row '((#\a     0 "col0 unchanged 'a'")
                   (#\b     1 "col1 unchanged 'b'")
                   (#\Space 2 "col2 should be the inserted blank")
                   (#\c     3 "col3 should be shifted 'c'")
                   (#\d     4 "col4 should be shifted 'd'")))
      (destructuring-bind (expected col desc) row
        (is (char= expected (char-at s col 0)) "~A" desc))))

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

