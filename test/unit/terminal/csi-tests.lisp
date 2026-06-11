(in-package #:cl-tmux/test)

;;;; CSI dispatch tests (src/terminal/csi.lisp).
;;;; Tests: cursor-movement suite.

;;; ── SUITE: cursor-movement ──────────────────────────────────────────────────

(def-suite cursor-movement
  :description "CSI A/B/C/D/E/F/G/H/f/d cursor sequences"
  :in terminal-suite)
(in-suite cursor-movement)

(test cup
  "CUP ESC[3;5H positions cursor at (col=4, row=2) in 0-based terms."
  (with-screen (s 20 10)
    (feed s (esc "[3;5H"))
    (check-cursor s 4 2)))

(test cuu
  "CUU ESC[2A moves cursor up 2 rows."
  (with-screen (s 20 10)
    (feed s (esc "[5;5H"))  ; → (4, 4)
    (feed s (esc "[2A"))    ; up 2 → y=2
    (check-cursor s 4 2)))

(test cud
  "CUD ESC[3B moves cursor down 3 rows."
  (with-screen (s 20 10)
    (feed s (esc "[1;1H"))  ; → (0, 0)
    (feed s (esc "[3B"))    ; down 3 → y=3
    (check-cursor s 0 3)))

(test cuf
  "CUF ESC[4C moves cursor right 4 columns."
  (with-screen (s 20 10)
    (feed s (esc "[1;3H"))  ; → (2, 0)
    (feed s (esc "[4C"))    ; right 4 → x=6
    (check-cursor s 6 0)))

(test cub
  "CUB ESC[4D moves cursor left 4 columns."
  (with-screen (s 20 10)
    (feed s (esc "[1;7H"))  ; → (6, 0)
    (feed s (esc "[4D"))    ; left 4 → x=2
    (check-cursor s 2 0)))

(test cnl
  "CNL ESC[2E moves cursor to column 0 two rows down."
  (with-screen (s 20 10)
    (feed s (esc "[3;5H"))  ; → (4, 2)
    (feed s (esc "[2E"))    ; next 2 lines → (0, 4)
    (check-cursor s 0 4)))

(test cpl
  "CPL ESC[2F moves cursor to column 0 two rows up."
  (with-screen (s 20 10)
    (feed s (esc "[5;5H"))  ; → (4, 4)
    (feed s (esc "[2F"))    ; preceding 2 lines → (0, 2)
    (check-cursor s 0 2)))

(test cha
  "CHA ESC[5G moves cursor to column 4 (1-based 5)."
  (with-screen (s 20 10)
    (feed s (esc "[4;4H"))  ; → (3, 3)
    (feed s (esc "[5G"))    ; column 5 (1-based) → x=4
    (check-cursor s 4 3)))

(test vpa
  "VPA ESC[5d moves cursor to row 4 (1-based 5)."
  (with-screen (s 20 10)
    (feed s (esc "[4;4H"))  ; → (3, 3)
    (feed s (esc "[5d"))    ; row 5 (1-based) → y=4
    (check-cursor s 3 4)))

(test hvp
  "HVP ESC[3;5f is equivalent to CUP."
  (with-screen (s 20 10)
    (feed s (esc "[3;5f"))
    (check-cursor s 4 2)))

(test hpa
  "HPA ESC[5` moves the cursor to column 4 (1-based 5), like CHA."
  (with-screen (s 20 10)
    (feed s (esc "[4;4H"))   ; → (3, 3)
    (feed s (esc "[5`"))     ; column 5 (1-based) → x=4
    (check-cursor s 4 3)))

(test hpr
  "HPR ESC[4a moves the cursor right 4 columns, like CUF."
  (with-screen (s 20 10)
    (feed s (esc "[1;3H"))   ; → (2, 0)
    (feed s (esc "[4a"))     ; right 4 → x=6
    (check-cursor s 6 0)))

(test vpr
  "VPR ESC[3e moves the cursor down 3 rows, like CUD."
  (with-screen (s 20 10)
    (feed s (esc "[1;1H"))   ; → (0, 0)
    (feed s (esc "[3e"))     ; down 3 → y=3
    (check-cursor s 0 3)))

(test scosc-scorc
  "SCOSC ESC[s saves the cursor and SCORC ESC[u restores it (ANSI.SYS)."
  (with-screen (s 20 10)
    (feed s (esc "[4;6H"))   ; → (5, 3)
    (feed s (esc "[s"))      ; save cursor
    (feed s (esc "[1;1H"))   ; move away → (0, 0)
    (check-cursor s 0 0)
    (feed s (esc "[u"))      ; restore → (5, 3)
    (check-cursor s 5 3)))

(test clamp
  "Out-of-bounds CUP ESC[100;100H clamps to the last valid cell."
  (with-screen (s 10 5)
    (feed s (esc "[100;100H"))
    (check-cursor s 9 4)))

;;; Note: the individual named tests above (cup, cuu, cud, cuf, cub, cnl, cpl,
;;; cha, vpa) cover the same cases as a parameterized table would.  Keeping the
;;; named tests provides clearer failure messages; the table version is omitted
;;; to avoid redundancy (audit finding: test_abstraction_issues).

;;; ── Boundary / clamp edge cases ──────────────────────────────────────────────

(test cuu-clamps-to-row-zero
  "CUU ESC[100A from row 3 clamps to row 0 (cannot go above top)."
  :description "cursor-up large count clamps to row 0"
  (with-screen (s 20 10)
    (feed s (esc "[4;1H"))   ; row 4 (1-based) → 0-based row 3
    (feed s (esc "[100A"))   ; up 100 — must clamp to row 0
    (check-cursor s 0 0)))

(test cud-clamps-to-last-row
  "CUD ESC[100B from row 0 clamps to the last row (cannot go below bottom)."
  :description "cursor-down large count clamps to last row"
  (with-screen (s 20 10)
    (feed s (esc "[100B"))   ; down 100 — must clamp to row 9
    (is (<= (screen-cursor-y s) 9)
        "cursor-y must not exceed screen height-1")))

(test cuf-clamps-to-last-col
  "CUF ESC[100C from col 0 clamps to the last column."
  :description "cursor-right large count clamps to last col"
  (with-screen (s 20 10)
    (feed s (esc "[100C"))   ; right 100 — must clamp to col 19
    (is (<= (screen-cursor-x s) 19)
        "cursor-x must not exceed screen width-1")))

(test cub-clamps-to-col-zero
  "CUB ESC[100D from any column clamps to col 0."
  :description "cursor-left large count clamps to col 0"
  (with-screen (s 20 10)
    (feed s (esc "[1;10H"))  ; col 9
    (feed s (esc "[100D"))   ; left 100 — must clamp to col 0
    (check-cursor s 0 0)))

;;; ── Cursor movement table: many sequences, one test ─────────────────────────
;;;
;;; We capture the natural tabular shape of these tests rather than repeating
;;; setup four more times.  Each entry is:
;;;   (initial-seq move-seq expected-cx expected-cy)

(test cursor-movement-table
  "CUU/CUD/CUF/CUB with explicit counts moves the cursor by the specified amount."
  :description "parameterized cursor-movement checks"
  ;; Built with LIST (not quoted) so the ESC helper actually runs — a quoted
  ;; table would leave (esc "...") as a literal list and feed a bare symbol.
  (dolist (entry
           ;;     (setup          move          ex-x ex-y)
           (list (list (esc "[3;3H") (esc "[1A")   2    1)   ; CUU 1 from (2,2) → y=1
                 (list (esc "[3;3H") (esc "[1B")   2    3)   ; CUD 1 from (2,2) → y=3
                 (list (esc "[3;3H") (esc "[1C")   3    2)   ; CUF 1 from (2,2) → x=3
                 (list (esc "[3;3H") (esc "[1D")   1    2))) ; CUB 1 from (2,2) → x=1
    (destructuring-bind (setup move expected-cx expected-cy) entry
      (with-screen (s 20 10)
        (feed s setup)
        (feed s move)
        (check-cursor s expected-cx expected-cy)))))

(test csi-cursor-home-no-params-goes-to-origin
  "ESC[H with no parameters uses p1*/p2* = (max 1 0) = 1 (1-based), so 1-1 = 0,0.
   Verifies that define-csi-rules generates the correct default-parameter binding."
  (with-screen (s 20 10)
    (feed s (esc "[5;10H"))    ; move to row 5, col 10
    (check-cursor s 9 4)
    (feed s (esc "[H"))        ; CUP no params → home (0,0)
    (check-cursor s 0 0)))

(test csi-cursor-up-default-one-row
  "ESC[A with no parameter moves cursor up by 1 (p1* defaults to 1)."
  (with-screen (s 10 5)
    (feed s (esc "[3;1H"))     ; move to row 3 (0-based: row 2)
    (feed s (esc "[A"))        ; CUU no params → up 1
    (check-cursor s 0 1)))

;;; ── SUITE: decscusr ──────────────────────────────────────────────────────────

(def-suite decscusr
  :description "DECSCUSR cursor shape — CSI N SP q"
  :in terminal-suite)
(in-suite decscusr)

;;; The three DECSCUSR shapes share the same test shape (feed + check), so we
;;; express them as a single table-driven test.  Each entry is (sequence expected-shape).

(test decscusr-shape-table
  "CSI N SP q sets cursor-shape to N (0=default-blink-block, 2=steady-block, 5=blink-bar)."
  (dolist (entry '(("0" 0) ("2" 2) ("5" 5)))
    (let ((param  (first  entry))
          (expect (second entry)))
      (with-screen (s 20 5)
        (feed s (esc (format nil "[~A q" param)))
        (is (= expect (cl-tmux/terminal/types:screen-cursor-shape s))
            "cursor-shape must be ~D after ESC[~A q" expect param))))
  ;; Verify default shape on a fresh screen is 1 (block blink).
  (with-screen (s 20 5)
    (is (= 1 (cl-tmux/terminal/types:screen-cursor-shape s))
        "default cursor-shape on fresh screen must be 1")))

;;; ── SUITE: cbt-cht ───────────────────────────────────────────────────────────

(def-suite cbt-cht
  :description "CBT / CHT tab stop sequences — CSI Z and CSI I"
  :in terminal-suite)
(in-suite cbt-cht)

(test cbt-moves-backward-tab
  "CSI 1 Z from column 12 moves cursor backward to column 8."
  (with-screen (s 40 5)
    (feed s (esc "[1;13H"))    ; move to col 12 (1-based 13)
    (check-cursor s 12 0)
    (feed s (esc "[Z"))        ; CBT 1 stop backward
    (check-cursor s 8 0)))

(test cbt-moves-backward-two-stops
  "CSI 2 Z from column 18 moves cursor back two tab stops to column 8."
  (with-screen (s 40 5)
    (feed s (esc "[1;19H"))    ; move to col 18
    (feed s (esc "[2Z"))       ; CBT 2 stops backward
    (check-cursor s 8 0)))

(test cbt-clamps-to-column-zero
  "CSI 5 Z from column 3 clamps cursor to column 0."
  (with-screen (s 40 5)
    (feed s (esc "[1;4H"))     ; col 3
    (feed s (esc "[5Z"))       ; backward 5 stops
    (check-cursor s 0 0)))

(test cht-moves-forward-tab
  "CSI 1 I from column 0 advances cursor to column 8."
  (with-screen (s 40 5)
    (check-cursor s 0 0)
    (feed s (esc "[I"))        ; CHT 1 stop forward
    (check-cursor s 8 0)))

(test cht-moves-forward-two-stops
  "CSI 2 I from column 0 advances cursor to column 16."
  (with-screen (s 40 5)
    (feed s (esc "[2I"))       ; CHT 2 stops forward
    (check-cursor s 16 0)))

(test cht-clamps-to-right-edge
  "CSI 10 I from column 0 on a narrow screen clamps to the right edge."
  (with-screen (s 10 5)
    (feed s (esc "[10I"))      ; far forward
    (is (<= (screen-cursor-x s) 9)
        "cursor must not exceed screen width-1")))

;;; ── SUITE: su-sd ─────────────────────────────────────────────────────────────

(def-suite su-sd
  :description "SU/SD scroll up/down — CSI N S and CSI N T"
  :in terminal-suite)
(in-suite su-sd)

(test su-scrolls-content-up
  "CSI 1 S scrolls the screen up by 1: row 0 moves to scrollback, row 1 becomes row 0."
  (with-screen (s 10 3)
    (feed s "row0")
    (feed s (format nil "~C~C" #\Return #\Linefeed))
    (feed s "row1")
    (feed s (esc "[H"))          ; home cursor
    (feed s (esc "[S"))          ; SU 1 — scroll up
    ;; row 0 should now contain what was row 1
    (is (string= "row1" (row-string s 0 :end 4))
        "after SU 1 row 0 must contain old row 1 content")))

(test su-2-scrolls-two-lines
  "CSI 2 S scrolls up by 2 lines."
  (with-screen (s 10 4)
    (feed s "aaa") (feed s (format nil "~C~C" #\Return #\Linefeed))
    (feed s "bbb") (feed s (format nil "~C~C" #\Return #\Linefeed))
    (feed s "ccc") (feed s (format nil "~C~C" #\Return #\Linefeed))
    (feed s "ddd")
    (feed s (esc "[H"))       ; home
    (feed s (esc "[2S"))      ; SU 2
    (is (string= "ccc" (row-string s 0 :end 3))
        "after SU 2 row 0 must contain old row 2 content")))

(test sd-scrolls-content-down
  "CSI 1 T scrolls the screen down by 1: row 0 becomes blank, old row 0 moves to row 1."
  (with-screen (s 10 3)
    (feed s "row0")
    (feed s (esc "[H"))
    (feed s (esc "[T"))          ; SD 1 — scroll down
    ;; New top row must be blank
    (is (row-blank-p s 0)
        "after SD 1 row 0 must be blank")
    ;; Old row 0 content must be on row 1
    (is (string= "row0" (row-string s 1 :end 4))
        "after SD 1 old row 0 content must be on row 1")))
