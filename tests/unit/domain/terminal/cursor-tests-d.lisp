(in-package #:cl-tmux/test)

;;;; cursor tests — part D: cursor-lf direct, cursor-nl newline-mode,
;;;; %materialize-tab-stops, BCE erase-cell background propagation,
;;;; and table-driven cursor-up/down/left/right at boundaries.

;;; ── cursor-lf direct behaviour ───────────────────────────────────────────────
;;;
;;; cursor-lf is distinct from cursor-nl: it performs a bare line feed (moves
;;; down / scrolls) without a carriage return even when LNM is on.  IND (ESC D)
;;; calls cursor-lf directly.

;; NOTE: direct-action-cursor is defined (def-suite ... :in terminal-suite) in
;; tests/unit/domain/terminal/cursor-tests.lisp; this describe block covers
;; only the tests that originally lived in this file.
(describe "terminal-suite/direct-action-cursor"

  ;; cursor-lf from a row that is not the scroll-bottom simply increments cursor-y.
  (it "cursor-lf-moves-down-within-screen"
    (with-screen (s 10 5)
      (cl-tmux/terminal/actions:cursor-lf s)   ; row 0 → row 1
      (check-cursor s 0 1)
      (cl-tmux/terminal/actions:cursor-lf s)   ; row 1 → row 2
      (check-cursor s 0 2)))

  ;; cursor-lf cancels any pending wrap: after LF the pending-wrap flag is NIL.
  (it "cursor-lf-cancels-pending-wrap"
    (with-screen (s 3 3)
      (feed s "abc")                     ; fills row 0; pending-wrap set
      (expect (cl-tmux/terminal/types:screen-pending-wrap s) :to-be-truthy)
      (cl-tmux/terminal/actions:cursor-lf s)
      (expect (cl-tmux/terminal/types:screen-pending-wrap s) :to-be-falsy)))

  ;; cursor-lf at the scroll-bottom scrolls, keeping the cursor within the screen.
  (it "cursor-lf-at-scroll-bottom-does-not-exceed-screen"
    (with-screen (s 5 3)
      (cl-tmux/terminal/actions:set-cursor s 0 2)   ; bottom row
      (cl-tmux/terminal/actions:cursor-lf s)
      (expect (<= (screen-cursor-y s) 2)))))

;;; ── cursor-nl (LF/VT/FF with newline-mode) ───────────────────────────────────
;;;
;;; cursor-nl is what the C0 LF/VT/FF handlers call.  When LNM (mode 20) is on
;;; it performs a carriage return after the line feed so 'a' LF 'b' stacks at
;;; column 0.  When LNM is off it behaves identically to cursor-lf (the column
;;; is preserved).

(describe "terminal-suite/cursor-nl-mode-suite"

  ;; cursor-nl with LNM off (default) performs a bare line feed, keeping the column.
  (it "cursor-nl-default-lnm-off-preserves-column"
    (with-screen (s 10 5)
      (feed s "hello")                         ; cursor at col 5, row 0
      (cl-tmux/terminal/actions:cursor-nl s)   ; default LNM off
      ;; cursor moves down to row 1 but stays in col 5
      (check-cursor s 5 1)))

  ;; cursor-nl with LNM on (screen-newline-mode = T) performs CR then LF: cursor lands at col 0.
  (it "cursor-nl-with-lnm-on-resets-column-to-zero"
    (with-screen (s 10 5)
      (setf (cl-tmux/terminal/types:screen-newline-mode s) t)
      (feed s "hello")                         ; cursor at col 5, row 0
      (cl-tmux/terminal/actions:cursor-nl s)
      ;; column reset to 0 because LNM is on
      (check-cursor s 0 1)))

  ;; With LNM on, 'a' LF 'b' LF 'c' places each char at column 0 of successive rows.
  (it "cursor-nl-lnm-on-stacks-text-vertically"
    (with-screen (s 10 5)
      (setf (cl-tmux/terminal/types:screen-newline-mode s) t)
      (cl-tmux/terminal/actions:write-char-at-cursor s #\a)   ; col 0 row 0
      (cl-tmux/terminal/actions:cursor-nl s)                  ; LF + CR
      (cl-tmux/terminal/actions:write-char-at-cursor s #\b)   ; col 0 row 1
      (cl-tmux/terminal/actions:cursor-nl s)
      (cl-tmux/terminal/actions:write-char-at-cursor s #\c)   ; col 0 row 2
      (expect (char= #\a (char-at s 0 0)))
      (expect (char= #\b (char-at s 0 1)))
      (expect (char= #\c (char-at s 0 2)))))

  ;; With LNM off (default), writing then LF leaves the cursor in the same column.
  (it "cursor-nl-lnm-off-leaves-column-intact"
    (with-screen (s 10 5)
      (feed s "hi")                          ; cursor at col 2
      (cl-tmux/terminal/actions:cursor-nl s)
      ;; column unchanged
      (check-cursor s 2 1))))

;;; ── cursor-lf via IND (ESC D) through the parser ─────────────────────────────
;;;
;;; ESC D calls cursor-lf directly (not cursor-nl), so LNM mode must not affect it.

(describe "terminal-suite/ind-esc-d-suite"

  ;; ESC D (IND) moves the cursor down one row without a carriage return, even if LNM is on.
  (it "ind-via-parser-moves-down-preserving-column"
    (with-screen (s 10 5)
      (setf (cl-tmux/terminal/types:screen-newline-mode s) t)
      (feed s "hello")                   ; cursor at col 5, row 0
      (feed s (esc "D"))                 ; ESC D = IND = cursor-lf (not cursor-nl)
      ;; IND uses cursor-lf (not cursor-nl), so column is PRESERVED regardless of LNM
      (check-cursor s 5 1)))

  ;; ESC D at the scroll-bottom scrolls up rather than advancing past the bottom.
  (it "ind-via-parser-at-scroll-bottom-scrolls"
    (with-screen (s 5 3)
      (feed s "XXXXX")
      (cl-tmux/terminal/actions:set-cursor s 0 2)   ; last row
      (feed s (esc "D"))                             ; IND
      ;; cursor stays at row 2
      (expect (<= (screen-cursor-y s) 2)))))

;;; ── BCE background colour propagation through %erase-cell ────────────────────
;;;
;;; Cells cleared by erase-display/erase-line/scroll must carry the current
;;; SGR background (BCE — background colour erase).  These tests verify that
;;; the current background is picked up, not the default.

(describe "terminal-suite/bce-background-suite"

  ;; erase-region fills cells with the current background via %erase-cell.
  ;; When cur-bg != default, the erased cells reflect that colour.
  (it "erase-region-bce-carries-current-background"
    (with-screen (s 5 3)
      (feed s "aaaaa")
      ;; Set SGR background to colour 2 (green) via an escape sequence.
      (feed s (esc "[42m"))           ; SGR 42 = green background
      ;; Erase row 0 via mode 2 (CSI 2 J).
      (feed s (esc "[2J"))
      ;; Now every cell in row 0 should have bg = 2.
      (let ((cell (screen-cell s 0 0)))
        (expect (= 2 (cell-bg cell))))))

  ;; scroll-up-one exposes a new row at the bottom; that row should carry the
  ;; current background colour (BCE semantics via %erase-cell / %clear-row).
  (it "scroll-up-one-exposed-row-carries-bce-background"
    (with-screen (s 5 3)
      ;; Set background to colour 3 (yellow)
      (feed s (esc "[43m"))
      ;; Scroll up once
      (cl-tmux/terminal/actions:scroll-up-one s)
      ;; The newly exposed bottom row (row 2) must have bg=3
      (let ((cell (screen-cell s 0 2)))
        (expect (= 3 (cell-bg cell))))))

  ;; erase-line mode 0 clears cells from the cursor to end of line; those cells
  ;; carry the current SGR background colour.
  (it "erase-line-bce-carries-current-background"
    (with-screen (s 5 3)
      (feed s "abcde")                   ; write some content
      (feed s (esc "[44m"))              ; SGR 44 = blue background
      (feed s (esc "[1;1H"))             ; cursor home (col 0, row 0)
      (feed s (esc "[K"))                ; EL mode 0 (erase to end of line)
      (let ((cell (screen-cell s 0 0)))
        (expect (= 4 (cell-bg cell)))))))

;;; ── Table-driven cursor boundary tests ───────────────────────────────────────
;;;
;;; These tests verify clamping at both 0 and width-1 / height-1 for all four
;;; directions, consolidating the boundary assertions into one table.

(describe "terminal-suite/cursor-boundary-table-suite"

  ;; Each cursor direction clamps correctly at both the lower and upper boundary.
  (it "cursor-boundary-clamping-table"
    ;; Each entry: (fn-sym axis init-val count expected-val description)
    (let ((cases
           `((cursor-up    :y  0  1  0  "cursor-up at row 0 stays at 0")
             (cursor-down  :y  9  1  9  "cursor-down at height-1 stays at height-1")
             (cursor-left  :x  0  1  0  "cursor-left at col 0 stays at 0")
             (cursor-right :x  9  1  9  "cursor-right at width-1 stays at width-1"))))
      (dolist (c cases)
        (destructuring-bind (fn-sym axis init-val count expected desc) c
          (declare (ignore desc))
          (with-screen (s 10 10)
            (if (eq axis :x)
                (setf (cl-tmux/terminal/types:screen-cursor-x s) init-val)
                (setf (cl-tmux/terminal/types:screen-cursor-y s) init-val))
            (funcall (symbol-function (find-symbol (symbol-name fn-sym)
                                                   '#:cl-tmux/terminal/actions))
                     s count)
            (let ((actual (if (eq axis :x)
                              (screen-cursor-x s)
                              (screen-cursor-y s))))
              (expect (= expected actual))))))))

  ;; cursor-up/down from the middle by various counts produce the expected row.
  (it "cursor-up-down-table-driven"
    ;; (init-y direction count expected-y description)
    (let ((cases '((5 :up   2 3 "up 2 from row 5 → row 3")
                   (5 :down 3 8 "down 3 from row 5 → row 8")
                   (5 :up   5 0 "up 5 from row 5 → row 0 (clamp at scroll-top=0)")
                   (5 :down 4 9 "down 4 from row 5 → row 9 (clamp at scroll-bottom=9)"))))
      (dolist (c cases)
        (destructuring-bind (init-y dir count expected desc) c
          (declare (ignore desc))
          (with-screen (s 10 10)
            (setf (cl-tmux/terminal/types:screen-cursor-y s) init-y)
            (ecase dir
              (:up   (cl-tmux/terminal/actions:cursor-up   s count))
              (:down (cl-tmux/terminal/actions:cursor-down s count)))
            (expect (= expected (screen-cursor-y s)))))))))
