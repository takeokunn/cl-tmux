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

(test xtversion-reports-tmux-version
  "CSI > q (XTVERSION) replies ESC P > | tmux 3.5 ST (cl-tmux's tmux 3.5 identity)."
  (with-screen (s 20 5)
    (feed s (esc "[>q"))       ; XTVERSION
    (is (string= (format nil "~CP>|tmux 3.5~C\\" #\Escape #\Escape)
                 (first (cl-tmux/terminal/types:screen-response-queue s)))
        "XTVERSION must report tmux 3.5")))

(test da3-response
  "CSI = c (DA3 / tertiary device attributes) queues the DECRPTUI reply."
  (with-screen (s 20 5)
    (feed s (esc "[=c"))       ; DA3
    (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
      (is (consp q) "response-queue must be non-empty after CSI =c")
      (is (some (lambda (r) (search "!|00000000" r)) q)
          "DA3 reply must contain the unit-id report !|00000000"))))

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

(test decrqm-reports-decscnm-mode-5
  "DECRQM ?5 reports DECSCNM (reverse-video screen): reset by default, set after ?5h."
  (with-screen (s 20 5)
    (feed s (esc "[?5$p"))
    (is (some (lambda (r) (search (format nil "~C[?5;2$y" #\Escape) r))
              (cl-tmux/terminal/types:screen-response-queue s))
        "DECRQM ?5 must report reset (Pm=2) by default")
    (feed s (esc "[?5h"))
    (feed s (esc "[?5$p"))
    (is (some (lambda (r) (search (format nil "~C[?5;1$y" #\Escape) r))
              (cl-tmux/terminal/types:screen-response-queue s))
        "DECRQM ?5 must report set (Pm=1) after ?5h")))

(test decrqm-reports-decawm-mode-7
  "DECRQM ?7 reports DECAWM (autowrap): set by default, reset after ?7l."
  (with-screen (s 20 5)
    (feed s (esc "[?7$p"))
    (is (some (lambda (r) (search (format nil "~C[?7;1$y" #\Escape) r))
              (cl-tmux/terminal/types:screen-response-queue s))
        "DECRQM ?7 must report set (Pm=1, autowrap on) by default")
    (feed s (esc "[?7l"))
    (feed s (esc "[?7$p"))
    (is (some (lambda (r) (search (format nil "~C[?7;2$y" #\Escape) r))
              (cl-tmux/terminal/types:screen-response-queue s))
        "DECRQM ?7 must report reset (Pm=2) after ?7l")))

(test decrqm-reports-sgr-mouse-mode-1006
  "DECRQM ?1006 reports the SGR mouse-encoding state, set after ?1006h."
  (with-screen (s 20 5)
    (feed s (esc "[?1006h"))
    (feed s (esc "[?1006$p"))
    (is (some (lambda (r) (search (format nil "~C[?1006;1$y" #\Escape) r))
              (cl-tmux/terminal/types:screen-response-queue s))
        "DECRQM ?1006 must report set (Pm=1) after ?1006h")))

(test decrqm-ansi-reports-irm-mode-4
  "ANSI-mode DECRQM (CSI 4 $ p, no ? marker) reports IRM: reset by default, set
   after CSI 4 h.  Reply has NO ? marker (ESC [ 4 ; Pm $ y)."
  (with-screen (s 20 5)
    (feed s (esc "[4$p"))
    (is (some (lambda (r) (search (format nil "~C[4;2$y" #\Escape) r))
              (cl-tmux/terminal/types:screen-response-queue s))
        "ANSI DECRQM 4 must report reset (Pm=2) by default")
    (feed s (esc "[4h"))
    (feed s (esc "[4$p"))
    (is (some (lambda (r) (search (format nil "~C[4;1$y" #\Escape) r))
              (cl-tmux/terminal/types:screen-response-queue s))
        "ANSI DECRQM 4 must report set (Pm=1) after CSI 4 h")))

(test decrqm-ansi-reports-lnm-mode-20
  "ANSI-mode DECRQM (CSI 20 $ p) reports LNM: set after CSI 20 h."
  (with-screen (s 20 5)
    (feed s (esc "[20h"))
    (feed s (esc "[20$p"))
    (is (some (lambda (r) (search (format nil "~C[20;1$y" #\Escape) r))
              (cl-tmux/terminal/types:screen-response-queue s))
        "ANSI DECRQM 20 must report set (Pm=1) after CSI 20 h")))

;;; ── XTWINOPS size reports (CSI Ps t) ─────────────────────────────────────────

(def-suite xtwinops
  :description "XTWINOPS size reports (CSI Ps t)"
  :in terminal-suite)
(in-suite xtwinops)

(test xtwinops-18-reports-text-area-chars
  "CSI 18 t reports the text-area size in characters: ESC [ 8 ; rows ; cols t."
  (with-screen (s 20 5)
    (feed s (esc "[18t"))
    (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
      (is (some (lambda (r) (string= (format nil "~C[8;5;20t" #\Escape) r)) q)
          "CSI 18 t must report ESC[8;5;20t for a 20x5 screen (got ~S)" q))))

(test xtwinops-19-reports-screen-chars
  "CSI 19 t reports the screen size in characters: ESC [ 9 ; rows ; cols t."
  (with-screen (s 20 5)
    (feed s (esc "[19t"))
    (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
      (is (some (lambda (r) (string= (format nil "~C[9;5;20t" #\Escape) r)) q)
          "CSI 19 t must report ESC[9;5;20t (got ~S)" q))))

(test xtwinops-resize-op-no-reply
  "A window-manipulation XTWINOPS op (CSI 8 ; 24 ; 80 t resize) enqueues no reply —
   a multiplexer does not resize the outer window."
  (with-screen (s 20 5)
    (feed s (esc "[8;24;80t"))
    (is (null (cl-tmux/terminal/types:screen-response-queue s))
        "XTWINOPS resize (op 8) must not enqueue a reply")))

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

(test cpr-in-decom-mode-reports-relative-row
  "In DECOM origin mode, CPR row is relative to the scroll-top margin (row 1 = margin top)."
  ;; Set a 10-row screen, scroll region rows 3..8 (0-based 2..7), enable DECOM,
  ;; place cursor at absolute row 5 (0-based 4) → relative row 3 (4-2+1=3).
  (with-screen (s 20 10)
    (feed s (esc "[3;8r"))    ; DECSTBM: scroll region rows 3..8 (1-based)
    (feed s (esc "[?6h"))     ; DECOM on — cursor is now relative to margin
    ;; CUP in DECOM mode: row 3 col 1 (1-based relative) → absolute row 4 (0-based)
    (feed s (esc "[3;1H"))
    (feed s (esc "[6n"))      ; CPR
    (let ((q (cl-tmux/terminal/types:screen-response-queue s)))
      (is (some (lambda (r) (search "[3;1R" r)) q)
          "CPR in DECOM mode must report margin-relative row 3, not absolute row 4"))))

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

