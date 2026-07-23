(in-package #:cl-tmux/test)

;;;; Cursor tests — part III: cursor-ri (reverse index), cursor-nel (next-line), write-char-at-cursor wide, %advance-cursor no-wrap, cursor movement behavioral.

(describe "terminal-suite/direct-action-cursor"

  ;;; ── cursor-ri (ESC M — reverse index) ───────────────────────────────────────

  ;; cursor-ri moves the cursor up one row when not at the scroll-top.
  (it "cursor-ri-moves-up-within-region"
    (with-screen (s 10 10)
      (setf (cl-tmux/terminal/types:screen-cursor-y s) 5)
      (cl-tmux/terminal/actions:cursor-ri s)
      (expect (= 4 (screen-cursor-y s)))))

  ;; cursor-ri at the scroll-top scrolls the region down instead of moving up.
  (it "cursor-ri-at-scroll-top-scrolls-down"
    (with-screen (s 5 5)
      ;; Write a recognisable line at the top, then move to top and reverse-index
      (feed s "LINE0")
      (cl-tmux/terminal/actions:set-cursor s 0 0)
      (cl-tmux/terminal/actions:cursor-ri s)
      ;; The old row 0 should now be at row 1, and row 0 should be blank
      (expect (row-blank-p s 0))
      (check-row s 1 "LINE0")))

  ;; cursor-ri at a custom scroll-top scrolls that region only.
  (it "cursor-ri-at-scroll-top-non-default-region"
    (with-scroll-region (s 10 10 3 7 3)
      (cl-tmux/terminal/actions:cursor-ri s)
      ;; cursor stays at scroll-top (scroll happened, not cursor move)
      (expect (= 3 (screen-cursor-y s))))))

;;; ── cursor-nel (ESC E — Next Line) ──────────────────────────────────────────
;;;
;;; cursor-nel is exported from cl-tmux/terminal/actions and used by the parser
;;; (ESC E handler).  It performs CR then LF: moves the cursor to column 0 of
;;; the next row, scrolling at the bottom margin exactly like LF.

(describe "terminal-suite/cursor-nel-suite"

  ;; cursor-nel from an interior column moves to col 0 of the next row.
  (it "cursor-nel-moves-to-column-zero-of-next-row"
    (with-screen (s 10 5)
      (feed s "hello")                       ; cursor at col 5, row 0
      (cl-tmux/terminal/actions:cursor-nel s)
      (check-cursor s 0 1)))

  ;; cursor-nel from the last column also moves to col 0 of the next row.
  (it "cursor-nel-at-right-edge-moves-to-next-row"
    (with-screen (s 5 5)
      (setf (cl-tmux/terminal/types:screen-cursor-x s) 4
            (cl-tmux/terminal/types:screen-cursor-y s) 2)
      (cl-tmux/terminal/actions:cursor-nel s)
      (check-cursor s 0 3)))

  ;; cursor-nel at the bottom of the scroll region scrolls up; cursor stays on the
  ;; bottom row at column 0 (identical behaviour to LF at the bottom margin).
  (it "cursor-nel-at-bottom-margin-scrolls"
    (with-screen (s 5 3)
      (feed s "LINE0")
      (cl-tmux/terminal/actions:set-cursor s 3 2)  ; col 3, last row
      (cl-tmux/terminal/actions:cursor-nel s)
      ;; After scroll, cursor is on row 2 col 0; the old row 0 is in scrollback.
      (check-cursor s 0 2)
      (expect (plusp (length (cl-tmux/terminal/types:screen-scrollback s))))))

  ;; ESC E (NEL) through the parser advances to col 0 of the next row.
  (it "cursor-nel-via-parser"
    (with-screen (s 10 5)
      (feed s "hello")                       ; cursor at col 5, row 0
      (feed s (esc "E"))                     ; ESC E = NEL
      (check-cursor s 0 1)))

  ;;; ── write-char-at-cursor wide char ───────────────────────────────────────────

  ;; A double-width character that cannot fit in the last column wraps to the next row.
  (it "write-char-at-cursor-wide-char-wraps-at-right-edge"
    (with-screen (s 3 3)
      ;; Position cursor at column 2 (last col of a 3-wide screen)
      (feed s "ab")                          ; cursor at col 2
      ;; Feed a wide (CJK) character -- must wrap since only 1 column remains
      (utf8-feed s "あ")
      (expect (char= #\Space (char-at s 2 0)))
      (expect (char= #\あ (char-at s 0 1)))))

  ;; write-codepoint converts a code point to a char and writes it at the cursor.
  (it "write-codepoint-places-character"
    (with-screen (s 10 5)
      (cl-tmux/terminal/actions:write-codepoint s 65)  ; U+0041 = 'A'
      (expect (char= #\A (char-at s 0 0)))
      (check-cursor s 1 0)))

  ;; cursor-down/scroll increments cy when not at scroll-bottom.
  (it "cursor-down-slash-scroll-advances-within-region"
    (with-screen (s 10 5)
      (cl-tmux/terminal/actions:cursor-down/scroll s)  ; internal
      (check-cursor s 0 1)))

  ;; cursor-down/scroll scrolls up when at scroll-bottom.
  (it "cursor-down-slash-scroll-scrolls-at-bottom"
    (with-screen (s 5 3)
      (feed s "L0")
      (cl-tmux/terminal/actions:set-cursor s 0 2) ; bottom row
      (cl-tmux/terminal/actions:cursor-down/scroll s)
      ;; cursor must still be at row 2 (scroll happened)
      (check-cursor s 0 2)
      ;; scrollback must have grown
      (expect (plusp (length (cl-tmux/terminal/types:screen-scrollback s))))))

  ;; %advance-cursor increments cx when not at the right edge.
  (it "advance-cursor-stays-in-line-when-room"
    (with-screen (s 10 5)
      (cl-tmux/terminal/actions::%advance-cursor s 3)
      (check-cursor s 3 0)))

  ;; %advance-cursor at the right margin DEFERS the wrap (VT100 last-column flag):
  ;; the cursor parks at the last column and pending-wrap is set, rather than
  ;; wrapping immediately.
  (it "advance-cursor-defers-wrap-at-right-edge"
    (with-screen (s 5 3)
      (cl-tmux/terminal/actions:set-cursor s 4 0)  ; last column
      (cl-tmux/terminal/actions::%advance-cursor s 1)
      (check-cursor s 4 0)                          ; parked, NOT wrapped
      (expect (cl-tmux/terminal/types:screen-pending-wrap s) :to-be-truthy)))

  ;; A character written while pending-wrap is set wraps to col 0 of the next row
  ;; first, then writes there; pending-wrap is cleared.
  (it "deferred-wrap-next-char-wraps-and-clears"
    (with-screen (s 3 3)
      (feed s "abc")                  ; fills row 0; cursor parks at col 2, wrap pending
      (expect (cl-tmux/terminal/types:screen-pending-wrap s) :to-be-truthy)
      (check-cursor s 2 0)            ; parked at last column of row 0
      (feed s "d")                    ; triggers the deferred wrap
      (expect (char= #\d (char-at s 0 1)))
      (check-cursor s 1 1)
      (expect (cl-tmux/terminal/types:screen-pending-wrap s) :to-be-falsy)))

  ;; Filling a row then CR/LF must NOT insert a blank line: the next write lands on
  ;; the immediately following row (the classic pending-wrap correctness case).
  (it "deferred-wrap-newline-no-spurious-blank-line"
    (with-screen (s 3 4)
      (feed s "abc")                  ; fills row 0 (pending wrap)
      (feed s (format nil "~C~C" #\Return #\Linefeed))  ; CR LF
      (feed s "d")
      (expect (char= #\a (char-at s 0 0)))
      (expect (char= #\d (char-at s 0 1)))
      (expect (char= #\Space (char-at s 1 1)))))

  ;;; ── %advance-cursor no-wrap mode ─────────────────────────────────────────────
  ;;;
  ;;; Coverage gap: screen-autowrap=NIL clamps the cursor at the right edge instead
  ;;; of wrapping to the next row.  This path (lines 100-104 of cursor.lisp) was
  ;;; previously untested; modes-tests.lisp only verified the flag itself.

  ;; %advance-cursor with autowrap=NIL clamps the cursor to the last column and
  ;; does NOT wrap to the next row when advancing past the right edge.
  (it "advance-cursor-clamps-when-autowrap-off"
    (with-screen (s 5 3)
      ;; Disable autowrap
      (setf (cl-tmux/terminal/types:screen-autowrap s) nil)
      ;; Position at the last column
      (cl-tmux/terminal/actions:set-cursor s 4 0)
      ;; Attempt to advance by 1 (would normally wrap to row 1 col 0)
      (cl-tmux/terminal/actions::%advance-cursor s 1)
      ;; Cursor must remain at column 4 (width-1), row 0
      (check-cursor s 4 0)))

  ;; With autowrap=NIL, writing a character at the rightmost column overwrites
  ;; that cell in place; the cursor stays at the last column.
  (it "write-char-overwrites-at-right-edge-when-autowrap-off"
    (with-screen (s 5 3)
      (feed s (esc "[?7l"))        ; disable auto-wrap
      ;; Move to last column and write two chars
      (cl-tmux/terminal/actions:set-cursor s 4 0)
      (cl-tmux/terminal/actions:write-char-at-cursor s #\A)
      ;; Cursor still at last column (no wrap)
      (check-cursor s 4 0)
      ;; The cell at (4, 0) must contain A
      (expect (char= #\A (char-at s 4 0)))
      ;; row 1 must be completely blank (no wrap occurred)
      (expect (row-blank-p s 1))))

  ;;; ── cursor movement behavioral tests ────────────────────────────────────────
  ;;;
  ;;; These tests verify observable behavior: that cursor-up, cursor-down,
  ;;; cursor-left, and cursor-right actually move the cursor as documented.
  ;;; They replace implementation-probing tests that only checked fbound status.

  ;; cursor-up/down/left/right each move the cursor by N along their axis.
  (it "cursor-direction-moves-by-n-table"
    (dolist (row (list (list #'cl-tmux/terminal/actions:cursor-up    :y 6 2 4 "cursor-up 2 from row 6 → row 4")
                       (list #'cl-tmux/terminal/actions:cursor-down  :y 3 3 6 "cursor-down 3 from row 3 → row 6")
                       (list #'cl-tmux/terminal/actions:cursor-left  :x 7 3 4 "cursor-left 3 from col 7 → col 4")
                       (list #'cl-tmux/terminal/actions:cursor-right :x 2 4 6 "cursor-right 4 from col 2 → col 6")))
      (destructuring-bind (fn axis init-val count expected desc) row
        (declare (ignore desc))
        (with-screen (s 10 10)
          (if (eq axis :x)
              (setf (cl-tmux/terminal/types::screen-cursor-x s) init-val)
              (setf (cl-tmux/terminal/types::screen-cursor-y s) init-val))
          (funcall fn s count)
          (let ((actual (if (eq axis :x) (screen-cursor-x s) (screen-cursor-y s))))
            (expect (= expected actual))))))))

;;; ── SUITE: %place-wide-char ──────────────────────────────────────────────────
