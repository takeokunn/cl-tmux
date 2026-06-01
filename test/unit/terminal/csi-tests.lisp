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

(test clamp
  "Out-of-bounds CUP ESC[100;100H clamps to the last valid cell."
  (with-screen (s 10 5)
    (feed s (esc "[100;100H"))
    (check-cursor s 9 4)))

(test cursor-movement-table
  "Table-driven: verify each cursor CSI sequence independently."
  (let ((cases
          ;; (setup-seq  motion-seq  expected-cx  expected-cy)
          `(("" ,(esc "[5;5H") 4 4)
            (,(esc "[5;5H") ,(esc "[2A") 4 2)
            (,(esc "[1;1H") ,(esc "[3B") 0 3)
            (,(esc "[1;3H") ,(esc "[4C") 6 0)
            (,(esc "[1;7H") ,(esc "[4D") 2 0)
            (,(esc "[3;5H") ,(esc "[2E") 0 4)
            (,(esc "[5;5H") ,(esc "[2F") 0 2))))
    (dolist (c cases)
      (destructuring-bind (setup motion ecx ecy) c
        (with-screen (s 20 10)
          (unless (string= setup "") (feed s setup))
          (feed s motion)
          (is (= ecx (screen-cursor-x s))
              "cx ~D expected ~D after ~S" (screen-cursor-x s) ecx motion)
          (is (= ecy (screen-cursor-y s))
              "cy ~D expected ~D after ~S" (screen-cursor-y s) ecy motion))))))

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
