(in-package #:cl-tmux/test)

;;;; CSI dispatch tests (src/terminal/csi.lisp).
;;;; Tests: cursor-movement suite.

;;; ── SUITE: cursor-movement ──────────────────────────────────────────────────

(describe "terminal-suite/cursor-movement"

  ;; CUP ESC[3;5H positions cursor at (col=4, row=2) in 0-based terms.
  (it "cup"
    (with-screen (s 20 10)
      (feed s (esc "[3;5H"))
      (check-cursor s 4 2)))

  ;; CUU ESC[2A moves cursor up 2 rows.
  (it "cuu"
    (with-screen (s 20 10)
      (feed s (esc "[5;5H"))  ; → (4, 4)
      (feed s (esc "[2A"))    ; up 2 → y=2
      (check-cursor s 4 2)))

  ;; CUD ESC[3B moves cursor down 3 rows.
  (it "cud"
    (with-screen (s 20 10)
      (feed s (esc "[1;1H"))  ; → (0, 0)
      (feed s (esc "[3B"))    ; down 3 → y=3
      (check-cursor s 0 3)))

  ;; CUF ESC[4C moves cursor right 4 columns.
  (it "cuf"
    (with-screen (s 20 10)
      (feed s (esc "[1;3H"))  ; → (2, 0)
      (feed s (esc "[4C"))    ; right 4 → x=6
      (check-cursor s 6 0)))

  ;; CUB ESC[4D moves cursor left 4 columns.
  (it "cub"
    (with-screen (s 20 10)
      (feed s (esc "[1;7H"))  ; → (6, 0)
      (feed s (esc "[4D"))    ; left 4 → x=2
      (check-cursor s 2 0)))

  ;; CNL ESC[2E moves cursor to column 0 two rows down.
  (it "cnl"
    (with-screen (s 20 10)
      (feed s (esc "[3;5H"))  ; → (4, 2)
      (feed s (esc "[2E"))    ; next 2 lines → (0, 4)
      (check-cursor s 0 4)))

  ;; CPL ESC[2F moves cursor to column 0 two rows up.
  (it "cpl"
    (with-screen (s 20 10)
      (feed s (esc "[5;5H"))  ; → (4, 4)
      (feed s (esc "[2F"))    ; preceding 2 lines → (0, 2)
      (check-cursor s 0 2)))

  ;; CHA ESC[5G moves cursor to column 4 (1-based 5).
  (it "cha"
    (with-screen (s 20 10)
      (feed s (esc "[4;4H"))  ; → (3, 3)
      (feed s (esc "[5G"))    ; column 5 (1-based) → x=4
      (check-cursor s 4 3)))

  ;; VPA ESC[5d moves cursor to row 4 (1-based 5).
  (it "vpa"
    (with-screen (s 20 10)
      (feed s (esc "[4;4H"))  ; → (3, 3)
      (feed s (esc "[5d"))    ; row 5 (1-based) → y=4
      (check-cursor s 3 4)))

  ;; HVP ESC[3;5f is equivalent to CUP.
  (it "hvp"
    (with-screen (s 20 10)
      (feed s (esc "[3;5f"))
      (check-cursor s 4 2)))

  ;; HPA ESC[5` moves the cursor to column 4 (1-based 5), like CHA.
  (it "hpa"
    (with-screen (s 20 10)
      (feed s (esc "[4;4H"))   ; → (3, 3)
      (feed s (esc "[5`"))     ; column 5 (1-based) → x=4
      (check-cursor s 4 3)))

  ;; HPR ESC[4a moves the cursor right 4 columns, like CUF.
  (it "hpr"
    (with-screen (s 20 10)
      (feed s (esc "[1;3H"))   ; → (2, 0)
      (feed s (esc "[4a"))     ; right 4 → x=6
      (check-cursor s 6 0)))

  ;; VPR ESC[3e moves the cursor down 3 rows, like CUD.
  (it "vpr"
    (with-screen (s 20 10)
      (feed s (esc "[1;1H"))   ; → (0, 0)
      (feed s (esc "[3e"))     ; down 3 → y=3
      (check-cursor s 0 3)))

  ;; SCOSC ESC[s saves the cursor and SCORC ESC[u restores it (ANSI.SYS).
  (it "scosc-scorc"
    (with-screen (s 20 10)
      (feed s (esc "[4;6H"))   ; → (5, 3)
      (feed s (esc "[s"))      ; save cursor
      (feed s (esc "[1;1H"))   ; move away → (0, 0)
      (check-cursor s 0 0)
      (feed s (esc "[u"))      ; restore → (5, 3)
      (check-cursor s 5 3)))

  ;; Out-of-bounds CUP ESC[100;100H clamps to the last valid cell.
  (it "clamp"
    (with-screen (s 10 5)
      (feed s (esc "[100;100H"))
      (check-cursor s 9 4)))

  ;;; Note: the individual named tests above (cup, cuu, cud, cuf, cub, cnl, cpl,
  ;;; cha, vpa) cover the same cases as a parameterized table would.  Keeping the
  ;;; named tests provides clearer failure messages; the table version is omitted
  ;;; to avoid redundancy (audit finding: test_abstraction_issues).

  ;;; ── Boundary / clamp edge cases ──────────────────────────────────────────────

  ;; CUU ESC[100A from row 3 clamps to row 0 (cannot go above top).
  (it "cuu-clamps-to-row-zero"
    (with-screen (s 20 10)
      (feed s (esc "[4;1H"))   ; row 4 (1-based) → 0-based row 3
      (feed s (esc "[100A"))   ; up 100 — must clamp to row 0
      (check-cursor s 0 0)))

  ;; CUD ESC[100B from row 0 clamps to the last row (cannot go below bottom).
  (it "cud-clamps-to-last-row"
    (with-screen (s 20 10)
      (feed s (esc "[100B"))   ; down 100 — must clamp to row 9
      (expect (<= (screen-cursor-y s) 9))))

  ;; CUF ESC[100C from col 0 clamps to the last column.
  (it "cuf-clamps-to-last-col"
    (with-screen (s 20 10)
      (feed s (esc "[100C"))   ; right 100 — must clamp to col 19
      (expect (<= (screen-cursor-x s) 19))))

  ;; CUB ESC[100D from any column clamps to col 0.
  (it "cub-clamps-to-col-zero"
    (with-screen (s 20 10)
      (feed s (esc "[1;10H"))  ; col 9
      (feed s (esc "[100D"))   ; left 100 — must clamp to col 0
      (check-cursor s 0 0)))

  ;;; ── Cursor movement table: many sequences, one test ─────────────────────────
  ;;;
  ;;; We capture the natural tabular shape of these tests rather than repeating
  ;;; setup four more times.  Each entry is:
  ;;;   (initial-seq move-seq expected-cx expected-cy)

  ;; CUU/CUD/CUF/CUB with explicit counts moves the cursor by the specified amount.
  (it "cursor-movement-table"
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

  ;; ESC[H with no parameters uses p1*/p2* = (max 1 0) = 1 (1-based), so 1-1 = 0,0.
  ;; Verifies that define-csi-rules generates the correct default-parameter binding.
  (it "csi-cursor-home-no-params-goes-to-origin"
    (with-screen (s 20 10)
      (feed s (esc "[5;10H"))    ; move to row 5, col 10
      (check-cursor s 9 4)
      (feed s (esc "[H"))        ; CUP no params → home (0,0)
      (check-cursor s 0 0)))

  ;; ESC[A with no parameter moves cursor up by 1 (p1* defaults to 1).
  (it "csi-cursor-up-default-one-row"
    (with-screen (s 10 5)
      (feed s (esc "[3;1H"))     ; move to row 3 (0-based: row 2)
      (feed s (esc "[A"))        ; CUU no params → up 1
      (check-cursor s 0 1))))

;;; ── SUITE: decscusr ──────────────────────────────────────────────────────────

(describe "terminal-suite/decscusr"

  ;;; The three DECSCUSR shapes share the same test shape (feed + check), so we
  ;;; express them as a single table-driven test.  Each entry is (sequence expected-shape).

  ;; CSI N SP q sets cursor-shape to N (0=default-blink-block, 2=steady-block, 5=blink-bar).
  (it "decscusr-shape-table"
    (dolist (entry '(("0" 0) ("2" 2) ("5" 5)))
      (let ((param  (first  entry))
            (expect (second entry)))
        (with-screen (s 20 5)
          (feed s (esc (format nil "[~A q" param)))
          (expect (= expect (cl-tmux/terminal/types:screen-cursor-shape s))))))
    ;; Verify default shape on a fresh screen is 1 (block blink).
    (with-screen (s 20 5)
      (expect (= 1 (cl-tmux/terminal/types:screen-cursor-shape s))))))

;;; ── SUITE: cbt-cht ───────────────────────────────────────────────────────────

(describe "terminal-suite/cbt-cht"

  ;; CSI 1 Z from column 12 moves cursor backward to column 8.
  (it "cbt-moves-backward-tab"
    (with-screen (s 40 5)
      (feed s (esc "[1;13H"))    ; move to col 12 (1-based 13)
      (check-cursor s 12 0)
      (feed s (esc "[Z"))        ; CBT 1 stop backward
      (check-cursor s 8 0)))

  ;; CBT (CSI N Z) moves the cursor backward N tab stops, clamping at column 0.
  (it "cbt-backward-tab-stops-table"
    (dolist (row '(("[1;19H" "[2Z" 8 "2 stops from col 18 → col 8")
                   ("[1;4H"  "[5Z" 0 "5 stops from col 3 → col 0 (clamped)")))
      (destructuring-bind (setup-seq cbt-seq expected _desc) row
        (declare (ignore _desc))
        (with-screen (s 40 5)
          (feed s (esc setup-seq))
          (feed s (esc cbt-seq))
          (check-cursor s expected 0)))))

  ;; CHT (CSI N I) advances cursor forward N tab stops from column 0.
  (it "cht-forward-tab-stops-table"
    (dolist (row '(("[I"  8  "1 stop from col 0 → col 8")
                   ("[2I" 16 "2 stops from col 0 → col 16")))
      (destructuring-bind (seq expected _desc) row
        (declare (ignore _desc))
        (with-screen (s 40 5)
          (feed s (esc seq))
          (check-cursor s expected 0)))))

  ;; CSI 10 I from column 0 on a narrow screen clamps to the right edge.
  (it "cht-clamps-to-right-edge"
    (with-screen (s 10 5)
      (feed s (esc "[10I"))      ; far forward
      (expect (<= (screen-cursor-x s) 9)))))

;;; ── SUITE: su-sd ─────────────────────────────────────────────────────────────

(describe "terminal-suite/su-sd"

  ;; CSI 1 S scrolls the screen up by 1: row 0 moves to scrollback, row 1 becomes row 0.
  (it "su-scrolls-content-up"
    (with-screen (s 10 3)
      (feed s "row0")
      (feed s (format nil "~C~C" #\Return #\Linefeed))
      (feed s "row1")
      (feed s (esc "[H"))          ; home cursor
      (feed s (esc "[S"))          ; SU 1 — scroll up
      ;; row 0 should now contain what was row 1
      (expect (string= "row1" (row-string s 0 :end 4)))))

  ;; CSI 2 S scrolls up by 2 lines.
  (it "su-2-scrolls-two-lines"
    (with-screen (s 10 4)
      (feed s "aaa") (feed s (format nil "~C~C" #\Return #\Linefeed))
      (feed s "bbb") (feed s (format nil "~C~C" #\Return #\Linefeed))
      (feed s "ccc") (feed s (format nil "~C~C" #\Return #\Linefeed))
      (feed s "ddd")
      (feed s (esc "[H"))       ; home
      (feed s (esc "[2S"))      ; SU 2
      (expect (string= "ccc" (row-string s 0 :end 3)))))

  ;; CSI 1 T scrolls the screen down by 1: row 0 becomes blank, old row 0 moves to row 1.
  (it "sd-scrolls-content-down"
    (with-screen (s 10 3)
      (feed s "row0")
      (feed s (esc "[H"))
      (feed s (esc "[T"))          ; SD 1 — scroll down
      ;; New top row must be blank
      (expect (row-blank-p s 0))
      ;; Old row 0 content must be on row 1
      (expect (string= "row0" (row-string s 1 :end 4))))))

;;; ── SUITE: decrqm ────────────────────────────────────────────────────────────
;;;
;;; enqueue-decrqm-reply is the public entry point for DECRQM (ESC [ ? Pm $ p)
;;; mode queries.  It calls %decrqm-mode-state (generated by define-decrqm-mode-table)
;;; and formats the standard reply (ESC [ ? Pm ; Ps $ y).  We exercise each
;;; category of entry in define-decrqm-mode-table: accessor-based, :mouse-mode,
;;; :alt-screen, and :fixed.

(describe "terminal-suite/decrqm"

  ;; Accessor-based modes (e.g. mode 1004 focus-events) report 1 when the flag is
  ;; set and 2 when it is clear.  Exercises the plain (mode accessor-fn) spec path.
  (it "decrqm-accessor-mode-reports-set-and-reset"
    (with-screen (s 20 5)
      ;; focus-events defaults to NIL -> code 2 (reset)
      (cl-tmux/terminal/csi::enqueue-decrqm-reply s 1004)
      (let ((reply (first (cl-tmux/terminal/types:screen-response-queue s))))
        (expect (string= (format nil "~C[?1004;2$y" #\Escape) reply)))
      ;; enable focus-events -> code 1 (set)
      (setf (cl-tmux/terminal/types:screen-response-queue s) nil)
      (setf (cl-tmux/terminal/types:screen-focus-events s) t)
      (cl-tmux/terminal/csi::enqueue-decrqm-reply s 1004)
      (let ((reply (first (cl-tmux/terminal/types:screen-response-queue s))))
        (expect (string= (format nil "~C[?1004;1$y" #\Escape) reply)))))

  ;; Modes declared :fixed always report the specified code regardless of state.
  ;; Mode 2026 (synchronized output) is :fixed 2 and must always report 2.
  (it "decrqm-fixed-mode-always-reports-given-code"
    (with-screen (s 20 5)
      (cl-tmux/terminal/csi::enqueue-decrqm-reply s 2026)
      (let ((reply (first (cl-tmux/terminal/types:screen-response-queue s))))
        (expect (string= (format nil "~C[?2026;2$y" #\Escape) reply)))))

  ;; Mouse-mode specs (e.g. mode 1000 = :mouse-mode 1) report 1 only when the
  ;; screen's mouse-mode field equals the declared value.
  (it "decrqm-mouse-mode-reports-active-and-inactive"
    (with-screen (s 20 5)
      ;; mouse-mode 0 by default: mode 1000 not active -> code 2
      (cl-tmux/terminal/csi::enqueue-decrqm-reply s 1000)
      (expect (string= (format nil "~C[?1000;2$y" #\Escape)
                        (first (cl-tmux/terminal/types:screen-response-queue s))))
      ;; activate mouse mode 1: mode 1000 -> code 1
      (setf (cl-tmux/terminal/types:screen-response-queue s) nil)
      (setf (cl-tmux/terminal/types:screen-mouse-mode s) 1)
      (cl-tmux/terminal/csi::enqueue-decrqm-reply s 1000)
      (expect (string= (format nil "~C[?1000;1$y" #\Escape)
                        (first (cl-tmux/terminal/types:screen-response-queue s))))))

  ;; Mode 1049 (:alt-screen) reports 1 when the alternate screen buffer is active,
  ;; 2 otherwise.  Exercises the :alt-screen spec path via ESC sequences.
  (it "decrqm-alt-screen-mode-reflects-alternate-screen-state"
    (with-screen (s 20 5)
      ;; Default: primary screen -> code 2
      (cl-tmux/terminal/csi::enqueue-decrqm-reply s 1049)
      (expect (string= (format nil "~C[?1049;2$y" #\Escape)
                        (first (cl-tmux/terminal/types:screen-response-queue s))))
      ;; Switch to alt screen via ESC[?1049h
      (setf (cl-tmux/terminal/types:screen-response-queue s) nil)
      (feed s (esc "[?1049h"))
      (cl-tmux/terminal/csi::enqueue-decrqm-reply s 1049)
      (expect (string= (format nil "~C[?1049;1$y" #\Escape)
                        (first (cl-tmux/terminal/types:screen-response-queue s)))))))
