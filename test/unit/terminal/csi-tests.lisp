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

;;; ── SUITE: rep ───────────────────────────────────────────────────────────────

(def-suite rep
  :description "REP (CSI b) — repeat preceding character"
  :in terminal-suite)
(in-suite rep)

(test rep-repeats-last-char
  "CSI 3 b repeats the last printed character 3 times."
  (with-screen (s 20 5)
    (feed s "A")             ; writes 'A' at col 0, cursor at col 1
    (feed s (esc "[3b"))     ; REP 3: writes 'A' 3 more times
    (is (char= #\A (char-at s 0 0)) "col 0 must be A")
    (is (char= #\A (char-at s 1 0)) "col 1 must be A (first REP)")
    (is (char= #\A (char-at s 2 0)) "col 2 must be A (second REP)")
    (is (char= #\A (char-at s 3 0)) "col 3 must be A (third REP)")
    (check-cursor s 4 0)))

(test rep-noop-when-no-last-char
  "CSI N b is a no-op when no character has been written yet (screen-last-char is NIL)."
  (with-screen (s 20 5)
    ;; No characters written — last-char is NIL.
    (is (null (cl-tmux/terminal/types:screen-last-char s))
        "screen-last-char must be NIL on a fresh screen")
    (feed s (esc "[3b"))     ; REP 3 — no-op
    ;; Cursor must be at origin and screen must be blank.
    (check-cursor s 0 0)
    (is (row-blank-p s 0) "row 0 must remain blank after REP with no prior char")))

(test rep-uses-last-printed-char
  "screen-last-char is updated on each write; REP always uses the most recent."
  (with-screen (s 20 5)
    (feed s "AB")            ; writes A at 0, B at 1; last-char = B
    (is (char= #\B (cl-tmux/terminal/types:screen-last-char s))
        "screen-last-char must be B after writing AB")
    (feed s (esc "[2b"))     ; REP 2: writes B twice more
    (is (char= #\B (char-at s 2 0)) "col 2 must be B")
    (is (char= #\B (char-at s 3 0)) "col 3 must be B")))

;;; ── SUITE: da-response ───────────────────────────────────────────────────────

(def-suite da-response
  :description "DA1/DA2 device attribute responses"
  :in terminal-suite)
(in-suite da-response)

(test da1-response
  "CSI c (DA1) queues the VT100 response string ESC[?1;2c."
  (with-screen (s 20 5)
    (is (null (cl-tmux/terminal/types:screen-response-queue s))
        "response-queue must be empty initially")
    (feed s (esc "[c"))        ; DA1
    (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
      (is (consp q) "response-queue must be non-empty after CSI c")
      (is (some (lambda (r) (search "?1;2c" r)) q)
          "DA1 response must contain ?1;2c"))))

(test da2-response
  "CSI > c (DA2) queues the secondary device attribute response."
  (with-screen (s 20 5)
    (feed s (esc "[>c"))       ; DA2
    (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
      (is (consp q) "response-queue must be non-empty after CSI >c")
      (is (some (lambda (r) (search ">1;" r)) q)
          "DA2 response must contain >1;"))))

;;; ── DECRQM (request DEC private mode, CSI ? Ps $ p) ──────────────────────────

(test decrqm-reports-set-mode
  "DECRQM CSI ? 25 $ p reports the cursor-visibility mode as SET (Pm=1) by default."
  (with-screen (s 20 5)
    (feed s (esc "[?25$p"))
    (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
      (is (some (lambda (r) (search (format nil "~C[?25;1$y" #\Escape) r)) q)
          "DECRQM ?25 must report set (Pm=1) when the cursor is visible (got ~S)" q))))

(test decrqm-reports-reset-mode
  "After ?25l (hide cursor) DECRQM reports the mode as RESET (Pm=2)."
  (with-screen (s 20 5)
    (feed s (esc "[?25l"))     ; hide cursor
    (feed s (esc "[?25$p"))
    (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
      (is (some (lambda (r) (search (format nil "~C[?25;2$y" #\Escape) r)) q)
          "DECRQM ?25 must report reset (Pm=2) after ?25l (got ~S)" q))))

(test decrqm-unknown-mode-reports-zero
  "DECRQM for an unrecognised mode reports Pm=0 (not recognised)."
  (with-screen (s 20 5)
    (feed s (esc "[?9999$p"))
    (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
      (is (some (lambda (r) (search (format nil "~C[?9999;0$y" #\Escape) r)) q)
          "DECRQM unknown mode must report Pm=0 (got ~S)" q))))

;;; ── CPR (cursor position report, CSI 6 n) ────────────────────────────────────

(test cpr-at-home-replies-1-1
  "CSI 6 n (CPR) at the home position replies ESC[1;1R (1-based)."
  (with-screen (s 20 5)
    (feed s (esc "[6n"))       ; CPR — report cursor position
    (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
      (is (consp q) "response-queue must be non-empty after CSI 6n")
      (is (some (lambda (r) (search "[1;1R" r)) q)
          "CPR at home must report [1;1R"))))

(test cpr-reports-moved-cursor-position
  "After CUP to row 3, col 5, CSI 6 n reports the new 1-based position ESC[3;5R."
  (with-screen (s 20 5)
    (feed s (esc "[3;5H"))     ; CUP → row 3, col 5 (1-based)
    (feed s (esc "[6n"))       ; CPR
    (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
      (is (some (lambda (r) (search "[3;5R" r)) q)
          "CPR after CUP 3;5 must report [3;5R"))))

;;; ── DA response table: both responses enqueue without error ──────────────────
;;;
;;; The two DA variants (DA1/DA2) both follow the same pattern: feed the
;;; sequence, assert the queue is non-empty, assert a signature string.
;;; The table condenses the two individual tests into a loop so adding a new
;;; DA variant only requires a new row.

(test da-response-table
  "DA1 and DA2 both enqueue a response string with the expected signature."
  :description "parameterized DA1/DA2 response checks"
  (dolist (entry '(("[c"  "?1;2c")    ; DA1 signature
                   ("[>c" ">1;")))     ; DA2 signature
    (let ((seq (first entry))
          (sig (second entry)))
      (with-screen (s 20 5)
        (feed s (esc seq))
        (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
          (is (consp q) "response-queue must be non-empty after ~A" seq)
          (is (some (lambda (r) (search sig r)) q)
              "response must contain ~S" sig))))))

;;; ── REP count=0 is a no-op ───────────────────────────────────────────────────

(test rep-count-zero-is-noop
  "CSI 0 b (REP 0) is effectively a no-op: no additional cells written."
  :description "REP with count=0 writes nothing extra"
  (with-screen (s 20 5)
    (feed s "X")
    (let ((cx (screen-cursor-x s)))
      (feed s (esc "[0b"))
      ;; Cursor should stay at col cx (no writes for count=0).
      (is (= cx (screen-cursor-x s))
          "REP 0 must not advance the cursor"))))

;;; ── SUITE: ech ───────────────────────────────────────────────────────────────

(def-suite ech
  :description "ECH (CSI X) — erase characters without shifting"
  :in terminal-suite)
(in-suite ech)

(test ech-erases-n-chars-in-place
  "CSI 3 X erases 3 characters at the cursor position without moving the cursor."
  (with-screen (s 20 5)
    (feed s "ABCDE")              ; cells 0-4 = A B C D E, cursor at 5
    (feed s (esc "[1;4H"))        ; move cursor to col 3 (1-based)
    (check-cursor s 3 0)
    (feed s (esc "[3X"))          ; ECH 3 — erase cols 3,4,5
    ;; Columns 3,4,5 must now be blank; columns 0,1,2 intact; cursor unchanged.
    (is (char= #\A (char-at s 0 0)) "col 0 must be A")
    (is (char= #\B (char-at s 1 0)) "col 1 must be B")
    (is (char= #\C (char-at s 2 0)) "col 2 must be C")
    (is (char= #\Space (char-at s 3 0)) "col 3 must be blank after ECH")
    (is (char= #\Space (char-at s 4 0)) "col 4 must be blank after ECH")
    (is (char= #\Space (char-at s 5 0)) "col 5 must be blank after ECH")
    ;; Cursor must not have moved.
    (check-cursor s 3 0)))

(test ech-default-one-char
  "CSI X with no parameter erases 1 character (default p1* = 1)."
  (with-screen (s 10 5)
    (feed s "ABCD")
    (feed s (esc "[1;3H"))   ; cursor at col 2
    (feed s (esc "[X"))      ; ECH 1 (default)
    (is (char= #\A (char-at s 0 0)) "col 0 intact")
    (is (char= #\B (char-at s 1 0)) "col 1 intact")
    (is (char= #\Space (char-at s 2 0)) "col 2 erased")
    (is (char= #\D (char-at s 3 0)) "col 3 intact (ECH does not shift)")
    (check-cursor s 2 0)))

;;; ── SUITE: dsr ───────────────────────────────────────────────────────────────

(def-suite dsr
  :description "DSR (CSI 5 n) — Device Status Report (replies ESC[0n)"
  :in terminal-suite)
(in-suite dsr)

(test dsr-5n-replies-ok-without-altering-screen
  "CSI 5 n (DSR) queues the ESC[0n status reply without moving the cursor or
   altering screen content."
  (with-screen (s 20 5)
    (feed s "A")
    (feed s (esc "[5n"))   ; DSR — report status (queues ESC[0n)
    (feed s "B")
    ;; Screen content and cursor must be as if the report query were absent.
    (is (char= #\A (char-at s 0 0)) "col 0 must be A")
    (is (char= #\B (char-at s 1 0)) "col 1 must be B")
    (check-cursor s 2 0)
    ;; DSR must have queued the terminal-OK status reply.
    (is (some (lambda (r) (search "[0n" r))
              (cl-tmux/terminal/types:screen-response-queue s))
        "DSR (CSI 5 n) must enqueue the ESC[0n status reply")))

;;; ── SUITE: ich-dch ───────────────────────────────────────────────────────────

(def-suite ich-dch
  :description "ICH (CSI @) insert characters and DCH (CSI P) delete characters"
  :in terminal-suite)
(in-suite ich-dch)

(test ich-inserts-blanks-and-shifts-right
  "CSI 2 @ at column 1 inserts 2 blanks, pushing existing text right."
  (with-screen (s 10 5)
    (feed s "ABCDE")              ; row 0: A B C D E, cursor at 5
    (feed s (esc "[1;2H"))        ; cursor → col 1 (1-based 2)
    (check-cursor s 1 0)
    (feed s (esc "[2@"))          ; ICH 2 — insert 2 blanks at col 1
    ;; A stays at col 0; blanks at 1,2; B→3, C→4; D and E are pushed off.
    (is (char= #\A (char-at s 0 0)) "col 0 must be A (unchanged)")
    (is (char= #\Space (char-at s 1 0)) "col 1 must be blank after ICH")
    (is (char= #\Space (char-at s 2 0)) "col 2 must be blank after ICH")
    (is (char= #\B (char-at s 3 0)) "col 3 must be B (shifted right)")
    (is (char= #\C (char-at s 4 0)) "col 4 must be C (shifted right)")
    ;; Cursor must remain at the insertion point.
    (check-cursor s 1 0)))

(test ich-default-one-char
  "CSI @ with no parameter inserts 1 blank (default p1* = 1)."
  (with-screen (s 10 5)
    (feed s "XY")
    (feed s (esc "[1;1H"))   ; cursor at col 0
    (feed s (esc "[@"))      ; ICH 1 (default)
    (is (char= #\Space (char-at s 0 0)) "col 0 must be blank after ICH default")
    (is (char= #\X (char-at s 1 0))     "col 1 must be X (shifted right)")
    (is (char= #\Y (char-at s 2 0))     "col 2 must be Y (shifted right)")
    (check-cursor s 0 0)))

(test dch-deletes-and-shifts-left
  "CSI 2 P at column 1 deletes 2 characters, pulling remaining chars left."
  (with-screen (s 10 5)
    (feed s "ABCDE")              ; row 0: A B C D E, cursor at 5
    (feed s (esc "[1;2H"))        ; cursor → col 1
    (feed s (esc "[2P"))          ; DCH 2 — delete 2 chars at col 1
    ;; A stays; B and C removed; D→1, E→2; cols 3,4 become blank.
    (is (char= #\A (char-at s 0 0)) "col 0 must be A (unchanged)")
    (is (char= #\D (char-at s 1 0)) "col 1 must be D (shifted left)")
    (is (char= #\E (char-at s 2 0)) "col 2 must be E (shifted left)")
    (is (char= #\Space (char-at s 3 0)) "col 3 must be blank after DCH")
    (is (char= #\Space (char-at s 4 0)) "col 4 must be blank after DCH")
    (check-cursor s 1 0)))

(test dch-default-one-char
  "CSI P with no parameter deletes 1 character (default p1* = 1)."
  (with-screen (s 10 5)
    (feed s "ABCD")
    (feed s (esc "[1;2H"))   ; cursor at col 1
    (feed s (esc "[P"))      ; DCH 1 (default)
    (is (char= #\A (char-at s 0 0)) "col 0 must be A")
    (is (char= #\C (char-at s 1 0)) "col 1 must be C (B deleted, C shifted)")
    (is (char= #\D (char-at s 2 0)) "col 2 must be D (shifted)")
    (is (char= #\Space (char-at s 3 0)) "col 3 must be blank")
    (check-cursor s 1 0)))

;;; ── SUITE: il-dl ─────────────────────────────────────────────────────────────

(def-suite il-dl
  :description "IL (CSI L) insert lines and DL (CSI M) delete lines"
  :in terminal-suite)
(in-suite il-dl)

(test il-inserts-blank-line-at-cursor
  "CSI 1 L at row 1 inserts a blank line, pushing row 1 down to row 2."
  (with-screen (s 10 5)
    (feed-lines s "row0" "row1" "row2")
    (feed s (esc "[2;1H"))    ; cursor at row 1 (1-based 2)
    (feed s (esc "[L"))       ; IL 1 (default) — insert blank line
    ;; row 0 must be unchanged; row 1 blank; row 2 holds old row 1.
    (check-row s 0 "row0")
    (is (row-blank-p s 1) "row 1 must be blank after IL")
    (check-row s 2 "row1")))

(test il-two-lines
  "CSI 2 L inserts two blank lines, shifting subsequent rows down by 2."
  (with-screen (s 10 5)
    (feed-lines s "row0" "row1" "row2" "row3")
    (feed s (esc "[2;1H"))   ; cursor at row 1
    (feed s (esc "[2L"))     ; IL 2
    (check-row s 0 "row0")
    (is (row-blank-p s 1) "row 1 must be blank")
    (is (row-blank-p s 2) "row 2 must be blank")
    (check-row s 3 "row1")))

(test dl-deletes-current-line
  "CSI 1 M at row 1 removes that line, pulling row 2 up to row 1."
  (with-screen (s 10 5)
    (feed-lines s "row0" "row1" "row2")
    (feed s (esc "[2;1H"))    ; cursor at row 1
    (feed s (esc "[M"))       ; DL 1 (default)
    (check-row s 0 "row0")
    (check-row s 1 "row2")    ; row 2 moved up
    (is (row-blank-p s 2) "old row 2 position must be blank after DL")))

(test dl-two-lines
  "CSI 2 M deletes two lines starting at the cursor row."
  (with-screen (s 10 5)
    (feed-lines s "row0" "row1" "row2" "row3")
    (feed s (esc "[2;1H"))   ; cursor at row 1
    (feed s (esc "[2M"))     ; DL 2
    (check-row s 0 "row0")
    (check-row s 1 "row3")
    (is (row-blank-p s 2) "row 2 must be blank after deleting 2 lines")))

;;; ── SUITE: decstbm-csi ────────────────────────────────────────────────────────

(def-suite decstbm-csi
  :description "DECSTBM (CSI r) — scroll region set via the CSI r sequence"
  :in terminal-suite)
(in-suite decstbm-csi)

(test decstbm-csi-sets-scroll-region
  "ESC[3;8r sets the scroll region to rows 2-7 (0-based) and homes the cursor."
  (with-screen (s 10 10)
    (feed s (esc "[3;8H"))    ; move cursor away from home
    (feed s (esc "[3;8r"))    ; DECSTBM: top=3 (1-based) → 2, bottom=8 → 7
    (is (= 2 (cl-tmux/terminal/types:screen-scroll-top s))
        "scroll-top must be 2 (0-based) after ESC[3;8r")
    (is (= 7 (cl-tmux/terminal/types:screen-scroll-bottom s))
        "scroll-bottom must be 7 (0-based) after ESC[3;8r")
    ;; DECSTBM homes the cursor.
    (check-cursor s 0 0)))

(test decstbm-csi-no-params-resets-to-full-screen
  "ESC[r with no parameters resets the scroll region to full screen (rows 0 to height-1)."
  (with-screen (s 10 10)
    ;; First restrict the scroll region.
    (feed s (esc "[3;8r"))
    ;; Then reset with no params (p1=0 → top defaults to 1-1=0; p2=0 → bottom = height-1).
    (feed s (esc "[r"))
    (is (= 0 (cl-tmux/terminal/types:screen-scroll-top s))
        "scroll-top must be reset to 0 after ESC[r with no params")
    (is (= 9 (cl-tmux/terminal/types:screen-scroll-bottom s))
        "scroll-bottom must be reset to height-1 (9) after ESC[r with no params")))

(test decstbm-csi-scroll-region-constrains-scroll
  "After DECSTBM, scrolling operates within the defined region."
  (with-screen (s 10 5)
    (feed-lines s "row0" "row1" "row2" "row3")
    ;; Restrict scroll region to rows 1-2 (1-based 2;3r).
    (feed s (esc "[2;3r"))
    ;; From row 1 (inside region), scroll up by 1 line.
    (feed s (esc "[2;1H"))    ; cursor at row 1
    (feed s (esc "[S"))       ; SU 1
    ;; Row 0 must be unaffected (outside the scroll region).
    (check-row s 0 "row0")
    ;; Row 1 (top of region) should have moved to what was row 2.
    (check-row s 1 "row2")))

(test decstbm-csi-invalid-top-greater-than-bottom-resets-to-full-screen
  "DECSTBM with P1 > P2 (invalid margins) resets to full-screen per VT100 spec."
  (with-screen (s 10 10)
    ;; First set a valid region
    (feed s (esc "[3;8r"))
    (is (= 2 (cl-tmux/terminal/types:screen-scroll-top s)))
    (is (= 7 (cl-tmux/terminal/types:screen-scroll-bottom s)))
    ;; Now send invalid: top=8 (0-based 7) > bottom=3 (0-based 2)
    (feed s (esc "[8;3r"))
    (is (= 0 (cl-tmux/terminal/types:screen-scroll-top s))
        "invalid DECSTBM must reset top to 0")
    (is (= 9 (cl-tmux/terminal/types:screen-scroll-bottom s))
        "invalid DECSTBM must reset bottom to height-1")))

;;; ── SUITE: execute-csi-direct ────────────────────────────────────────────────

(def-suite execute-csi-direct
  :description "Direct calls to execute-csi"
  :in terminal-suite)
(in-suite execute-csi-direct)

(test execute-csi-cup-direct
  "execute-csi called directly with final #\\H and params '(3 5) positions cursor."
  (with-screen (s 20 10)
    (cl-tmux/terminal/csi:execute-csi s #\H nil nil '(3 5))
    ;; CUP: row=3 (1-based) → y=2; col=5 (1-based) → x=4
    (check-cursor s 4 2)))

(test execute-csi-sgr-direct
  "execute-csi with final #\\m and params '(31) sets foreground via the SGR path."
  (with-screen (s 20 10)
    (cl-tmux/terminal/csi:execute-csi s #\m nil nil '(31))
    (is (= 1 (cl-tmux/terminal/types:screen-cur-fg s))
        "execute-csi SGR 31 must set cur-fg to 1 (red)")))

(test execute-csi-unknown-final-is-noop
  "execute-csi with an unrecognized final byte is silently ignored (no error, no state change)."
  (with-screen (s 20 10)
    (finishes (cl-tmux/terminal/csi:execute-csi s #\z nil nil '()))
    ;; Screen state must be at defaults.
    (check-cursor s 0 0)
    (check-sgr-state s :fg 7 :bg 0 :attrs 0)))

(test execute-csi-unknown-intermed-is-noop
  "execute-csi with a recognized final but unrecognized intermed byte is silently ignored."
  (with-screen (s 20 10)
    ;; #\! intermed with #\H final is not defined — should be a no-op.
    (finishes (cl-tmux/terminal/csi:execute-csi s #\H #\! nil '(3 5)))
    ;; Cursor must remain at origin (no CUP fired).
    (check-cursor s 0 0)))

;;; ── SUITE: %csi-decstbm-params ───────────────────────────────────────────────

(def-suite csi-decstbm-params
  :description "Direct tests of %csi-decstbm-params helper"
  :in terminal-suite)
(in-suite csi-decstbm-params)

(test %csi-decstbm-params-converts-1based-to-0based
  "%csi-decstbm-params converts 1-based p1/p2 to 0-based (top, bottom) pair."
  (with-screen (s 10 10)
    ;; p1=3, p2=8 → top=2, bottom=7
    (multiple-value-bind (top bottom)
        (cl-tmux/terminal/csi::%csi-decstbm-params s 3 8)
      (is (= 2 top)   "top must be 0-based: (max 1 3)-1 = 2")
      (is (= 7 bottom) "bottom must be 0-based: 8-1 = 7"))))

(test %csi-decstbm-params-p1-zero-defaults-to-row-0
  "%csi-decstbm-params with p1=0 uses (max 1 0)=1 → 0-based top=0."
  (with-screen (s 10 10)
    (multiple-value-bind (top _)
        (cl-tmux/terminal/csi::%csi-decstbm-params s 0 5)
      (declare (ignore _))
      (is (= 0 top) "p1=0 must resolve to top=0"))))

(test %csi-decstbm-params-p2-zero-defaults-to-screen-height-minus-1
  "%csi-decstbm-params with p2=0 (omitted) defaults bottom to height-1."
  (with-screen (s 10 8)
    ;; screen height = 8
    (multiple-value-bind (_ bottom)
        (cl-tmux/terminal/csi::%csi-decstbm-params s 1 0)
      (declare (ignore _))
      (is (= 7 bottom) "p2=0 must resolve to bottom=height-1=7"))))

;;; ── SUITE: csi-unknown-sequences ─────────────────────────────────────────────

(def-suite csi-unknown-sequences
  :description "Unknown/unsupported CSI sequences are silently ignored"
  :in terminal-suite)
(in-suite csi-unknown-sequences)

(test csi-unknown-final-byte-does-not-crash
  "A CSI sequence with an unrecognized final byte is consumed without error."
  (with-screen (s 20 5)
    (feed s "A")
    (finishes (feed s (esc "[99z")))   ; '99z' has no rule
    (feed s "B")
    (is (char= #\A (char-at s 0 0)) "char before unknown CSI must be intact")
    (is (char= #\B (char-at s 1 0)) "char after unknown CSI must be written")))

(test csi-dec-private-unknown-mode-no-crash
  "DEC private mode with an unrecognized param number is silently ignored."
  (with-screen (s 20 5)
    (feed s "X")
    (finishes (feed s (esc "[?9876h")))  ; unknown DEC PM set
    (finishes (feed s (esc "[?9876l")))  ; unknown DEC PM reset
    (feed s "Y")
    (is (char= #\X (char-at s 0 0)) "char before unknown DEC PM must survive")
    (is (char= #\Y (char-at s 1 0)) "char after unknown DEC PM must be written")))

(test csi-multiple-unknown-sequences-in-sequence
  "Multiple back-to-back unknown CSI sequences are each consumed without crashing."
  (with-screen (s 20 5)
    (feed s "start")
    (finishes
      (progn
        (feed s (esc "[1z"))
        (feed s (esc "[2z"))
        (feed s (esc "[3z"))))
    (feed s "end")
    (check-row s 0 "startend")))

;;; ── SUITE: decom ──────────────────────────────────────────────────────────────

(def-suite decom
  :description "DECOM (?6) origin mode — CUP/HVP relative to the scroll region"
  :in terminal-suite)
(in-suite decom)

(test decom-cup-is-relative-to-scroll-region
  "With DECOM (?6h) set, CUP rows are relative to the scroll-region top."
  (with-screen (s 20 10)
    (feed s (esc "[3;6r"))   ; DECSTBM → scroll region rows 3-6 (0-based top=2, bottom=5)
    (feed s (esc "[?6h"))    ; DECOM on → cursor homes to (scroll-top=2, col 0)
    (is (= 2 (screen-cursor-y s)) "setting DECOM homes the cursor to the scroll-top")
    (is (= 0 (screen-cursor-x s)) "setting DECOM homes the cursor to column 0")
    (feed s (esc "[2;3H"))   ; CUP row 2 col 3 → origin-relative: row top+1=3, col 2
    (is (= 3 (screen-cursor-y s)) "CUP row 2 maps to scroll-top+1 under DECOM")
    (is (= 2 (screen-cursor-x s)) "CUP col 3 is absolute (col 2, 0-based)")))

(test decom-confines-cursor-to-scroll-region
  "With DECOM set, a CUP row past the scroll-region bottom is clamped to it."
  (with-screen (s 20 10)
    (feed s (esc "[3;6r"))
    (feed s (esc "[?6h"))
    (feed s (esc "[99;1H"))  ; CUP row 99 → clamped to scroll-bottom (row 5)
    (is (= 5 (screen-cursor-y s)) "DECOM must clamp the row to the scroll-region bottom")))

(test decom-reset-restores-absolute-cup
  "With DECOM reset (?6l, default), CUP rows are absolute."
  (with-screen (s 20 10)
    (feed s (esc "[3;6r"))
    (feed s (esc "[?6h"))
    (feed s (esc "[?6l"))    ; DECOM off → cursor homes to (0,0)
    (is (= 0 (screen-cursor-y s)) "resetting DECOM homes the cursor to row 0")
    (feed s (esc "[2;3H"))   ; CUP row 2 col 3 → absolute: row 1, col 2
    (is (= 1 (screen-cursor-y s)) "CUP row 2 maps to absolute row 1 without DECOM")))

;;; ── Coverage gap: %cup-row direct tests ──────────────────────────────────────
;;;
;;; Audit finding: %cup-row's non-DECOM branch and the DECOM clamping case were
;;; not separately asserted.  The DECOM tests above exercise the origin-mode path
;;; indirectly; these tests assert %cup-row directly.

(def-suite cup-row-direct
  :description "Direct coverage of %cup-row 1-based to 0-based row conversion"
  :in terminal-suite)
(in-suite cup-row-direct)

(test cup-row-non-decom-converts-1-based-to-0-based
  "%cup-row without DECOM converts a 1-based row to a 0-based row."
  (with-screen (s 20 10)
    ;; origin-mode NIL by default
    (is (= 0 (cl-tmux/terminal/csi::%cup-row s 1))
        "1-based row 1 → 0-based row 0")
    (is (= 4 (cl-tmux/terminal/csi::%cup-row s 5))
        "1-based row 5 → 0-based row 4")))

(test cup-row-decom-adds-scroll-top-offset
  "%cup-row with DECOM set adds the scroll-region top to the 1-based row."
  (with-screen (s 20 10)
    ;; Install a scroll region of rows 3-7 (0-based top=2)
    (feed s (esc "[3;8r"))     ; DECSTBM → top=2, bottom=7 (0-based)
    (feed s (esc "[?6h"))      ; DECOM on
    ;; Now %cup-row(1) should be scroll-top + 0 = 2
    (is (= 2 (cl-tmux/terminal/csi::%cup-row s 1))
        "DECOM: row 1 maps to scroll-top (row 2)")
    ;; %cup-row(2) should be scroll-top + 1 = 3
    (is (= 3 (cl-tmux/terminal/csi::%cup-row s 2))
        "DECOM: row 2 maps to scroll-top+1 (row 3)")))

(test cup-row-decom-clamps-to-scroll-bottom
  "%cup-row with DECOM clamps to scroll-region bottom when row exceeds it."
  (with-screen (s 20 10)
    (feed s (esc "[3;6r"))     ; DECSTBM → top=2, bottom=5 (0-based)
    (feed s (esc "[?6h"))      ; DECOM on
    ;; Row 99 (large) relative to top=2: 2 + 98 = 100, clamped to bottom=5
    (is (= 5 (cl-tmux/terminal/csi::%cup-row s 99))
        "DECOM: oversized row must be clamped to scroll-bottom (5)")))

;;; ── Coverage gap: enqueue-* helpers ─────────────────────────────────────────
;;;
;;; Audit finding: the extracted enqueue-dsr-reply, enqueue-cpr-reply,
;;; enqueue-da1-reply, and enqueue-da2-reply helpers are not tested directly.

(def-suite enqueue-helpers
  :description "Direct coverage of CSI response-queue enqueue helpers"
  :in terminal-suite)
(in-suite enqueue-helpers)

(test enqueue-dsr-reply-pushes-0n
  "enqueue-dsr-reply pushes ESC[0n onto the response queue."
  (with-screen (s 20 5)
    (cl-tmux/terminal/csi::enqueue-dsr-reply s)
    (is (some (lambda (r) (search "[0n" r))
              (cl-tmux/terminal/types:screen-response-queue s))
        "enqueue-dsr-reply must push a string containing '[0n'")))

(test enqueue-cpr-reply-reflects-cursor
  "enqueue-cpr-reply pushes ESC[row;colR reflecting the current cursor (1-based)."
  (with-screen (s 20 10)
    (feed s (esc "[3;5H"))     ; cursor → row 2, col 4 (0-based)
    (cl-tmux/terminal/csi::enqueue-cpr-reply s)
    (is (some (lambda (r) (search "[3;5R" r))
              (cl-tmux/terminal/types:screen-response-queue s))
        "enqueue-cpr-reply must contain '[3;5R' for cursor at (row=2,col=4)")))

(test enqueue-da1-reply-contains-signature
  "enqueue-da1-reply pushes a string containing the DA1 signature '?1;2c'."
  (with-screen (s 20 5)
    (cl-tmux/terminal/csi::enqueue-da1-reply s)
    (is (some (lambda (r) (search "?1;2c" r))
              (cl-tmux/terminal/types:screen-response-queue s))
        "enqueue-da1-reply must push a string containing '?1;2c'")))

(test enqueue-da2-reply-contains-signature
  "enqueue-da2-reply pushes a string containing the DA2 signature '>1;'."
  (with-screen (s 20 5)
    (cl-tmux/terminal/csi::enqueue-da2-reply s)
    (is (some (lambda (r) (search ">1;" r))
              (cl-tmux/terminal/types:screen-response-queue s))
        "enqueue-da2-reply must push a string containing '>1;'")))
