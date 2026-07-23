(in-package #:cl-tmux/test)

;;;; csi tests — part C: csi-unknown-sequences, DECOM/origin-mode,
;;;; cup-row-direct, enqueue-* helpers, XTPUSHTITLE/XTPOPTITLE,
;;;; DEC Rectangle operations (DECERA/DECFRA/DECCRA).

(describe "terminal-suite/csi-unknown-sequences"

  ;; A CSI sequence with an unrecognized final byte is consumed without error.
  (it "csi-unknown-final-byte-does-not-crash"
    (with-screen (s 20 5)
      (feed s "A")
      (finishes (feed s (esc "[99z")))   ; '99z' has no rule
      (feed s "B")
      (expect (char= #\A (char-at s 0 0)))
      (expect (char= #\B (char-at s 1 0)))))

  ;; DEC private mode with an unrecognized param number is silently ignored.
  (it "csi-dec-private-unknown-mode-no-crash"
    (with-screen (s 20 5)
      (feed s "X")
      (finishes (feed s (esc "[?9876h")))  ; unknown DEC PM set
      (finishes (feed s (esc "[?9876l")))  ; unknown DEC PM reset
      (feed s "Y")
      (expect (char= #\X (char-at s 0 0)))
      (expect (char= #\Y (char-at s 1 0)))))

  ;; Multiple back-to-back unknown CSI sequences are each consumed without crashing.
  (it "csi-multiple-unknown-sequences-in-sequence"
    (with-screen (s 20 5)
      (feed s "start")
      (finishes
        (progn
          (feed s (esc "[1z"))
          (feed s (esc "[2z"))
          (feed s (esc "[3z"))))
      (feed s "end")
      (check-row s 0 "startend"))))

;;; ── SUITE: decom ──────────────────────────────────────────────────────────────

(describe "terminal-suite/decom"

  ;; With DECOM (?6h) set, CUP rows are relative to the scroll-region top.
  (it "decom-cup-is-relative-to-scroll-region"
    (with-screen (s 20 10)
      (feed s (esc "[3;6r"))   ; DECSTBM → scroll region rows 3-6 (0-based top=2, bottom=5)
      (feed s (esc "[?6h"))    ; DECOM on → cursor homes to (scroll-top=2, col 0)
      (expect (= 2 (screen-cursor-y s)))
      (expect (= 0 (screen-cursor-x s)))
      (feed s (esc "[2;3H"))   ; CUP row 2 col 3 → origin-relative: row top+1=3, col 2
      (expect (= 3 (screen-cursor-y s)))
      (expect (= 2 (screen-cursor-x s)))))

  ;; With DECOM set, a CUP row past the scroll-region bottom is clamped to it.
  (it "decom-confines-cursor-to-scroll-region"
    (with-screen (s 20 10)
      (feed s (esc "[3;6r"))
      (feed s (esc "[?6h"))
      (feed s (esc "[99;1H"))  ; CUP row 99 → clamped to scroll-bottom (row 5)
      (expect (= 5 (screen-cursor-y s)))))

  ;; With DECOM reset (?6l, default), CUP rows are absolute.
  (it "decom-reset-restores-absolute-cup"
    (with-screen (s 20 10)
      (feed s (esc "[3;6r"))
      (feed s (esc "[?6h"))
      (feed s (esc "[?6l"))    ; DECOM off → cursor homes to (0,0)
      (expect (= 0 (screen-cursor-y s)))
      (feed s (esc "[2;3H"))   ; CUP row 2 col 3 → absolute: row 1, col 2
      (expect (= 1 (screen-cursor-y s))))))

;;; ── Coverage gap: %cup-row direct tests ──────────────────────────────────────
;;;
;;; Audit finding: %cup-row's non-DECOM branch and the DECOM clamping case were
;;; not separately asserted.  The DECOM tests above exercise the origin-mode path
;;; indirectly; these tests assert %cup-row directly.

(describe "terminal-suite/cup-row-direct"

  ;; %cup-row without DECOM converts a 1-based row to a 0-based row.
  (it "cup-row-non-decom-converts-1-based-to-0-based"
    (with-screen (s 20 10)
      ;; origin-mode NIL by default
      (expect (= 0 (cl-tmux/terminal/csi::%cup-row s 1)))
      (expect (= 4 (cl-tmux/terminal/csi::%cup-row s 5)))))

  ;; %cup-row with DECOM set adds the scroll-region top to the 1-based row.
  (it "cup-row-decom-adds-scroll-top-offset"
    (with-screen (s 20 10)
      ;; Install a scroll region of rows 3-7 (0-based top=2)
      (feed s (esc "[3;8r"))     ; DECSTBM → top=2, bottom=7 (0-based)
      (feed s (esc "[?6h"))      ; DECOM on
      ;; Now %cup-row(1) should be scroll-top + 0 = 2
      (expect (= 2 (cl-tmux/terminal/csi::%cup-row s 1)))
      ;; %cup-row(2) should be scroll-top + 1 = 3
      (expect (= 3 (cl-tmux/terminal/csi::%cup-row s 2)))))

  ;; %cup-row with DECOM clamps to scroll-region bottom when row exceeds it.
  (it "cup-row-decom-clamps-to-scroll-bottom"
    (with-screen (s 20 10)
      (feed s (esc "[3;6r"))     ; DECSTBM → top=2, bottom=5 (0-based)
      (feed s (esc "[?6h"))      ; DECOM on
      ;; Row 99 (large) relative to top=2: 2 + 98 = 100, clamped to bottom=5
      (expect (= 5 (cl-tmux/terminal/csi::%cup-row s 99))))))

;;; ── Coverage gap: enqueue-* helpers ─────────────────────────────────────────
;;;
;;; Audit finding: the extracted enqueue-dsr-reply, enqueue-cpr-reply,
;;; enqueue-da1-reply, and enqueue-da2-reply helpers are not tested directly.

(describe "terminal-suite/enqueue-helpers"

  ;; enqueue-dsr/da1/da2-reply each push a string with the expected fixed signature.
  (it "enqueue-static-reply-signatures-table"
    (dolist (row (list (list #'cl-tmux/terminal/csi::enqueue-dsr-reply "[0n"   "dsr → [0n")
                       (list #'cl-tmux/terminal/csi::enqueue-da1-reply "?1;2c" "da1 → ?1;2c")
                       (list #'cl-tmux/terminal/csi::enqueue-da2-reply ">1;"   "da2 → >1;")))
      (destructuring-bind (fn expected-sub desc) row
        (declare (ignore desc))
        (with-screen (s 20 5)
          (funcall fn s)
          (expect (some (lambda (r) (search expected-sub r))
                    (cl-tmux/terminal/types:screen-response-queue s)))))))

  ;; enqueue-cpr-reply pushes ESC[row;colR reflecting the current cursor (1-based).
  (it "enqueue-cpr-reply-reflects-cursor"
    (with-screen (s 20 10)
      (feed s (esc "[3;5H"))     ; cursor → row 2, col 4 (0-based)
      (cl-tmux/terminal/csi::enqueue-cpr-reply s)
      (expect (some (lambda (r) (search "[3;5R" r))
                (cl-tmux/terminal/types:screen-response-queue s))))))

;;; ── XTPUSHTITLE / XTPOPTITLE (CSI > Ps t / CSI < Ps t) ─────────────────────

(describe "terminal-suite/xtpushtitle-xtpoptitle"

  ;; CSI > t (XTPUSHTITLE) pushes the current title onto the title stack.
  (it "xtpushtitle-saves-current-title"
    (with-screen (s 20 5)
      (setf (cl-tmux/terminal/types:screen-title s) "initial")
      (feed s (esc "[>t"))   ; push
      (expect (equal '("initial") (cl-tmux/terminal/types:screen-title-stack s)))))

  ;; CSI < t (XTPOPTITLE) pops and restores the most recently pushed title.
  (it "xtpoptitle-restores-saved-title"
    (with-screen (s 20 5)
      (setf (cl-tmux/terminal/types:screen-title s) "original")
      (feed s (esc "[>t"))          ; push "original"
      (setf (cl-tmux/terminal/types:screen-title s) "changed")
      (feed s (esc "[<t"))          ; pop → restore "original"
      (expect (string= "original" (cl-tmux/terminal/types:screen-title s)))
      (expect (null (cl-tmux/terminal/types:screen-title-stack s)))))

  ;; CSI < t (XTPOPTITLE) on an empty stack is a no-op: title unchanged.
  (it "xtpoptitle-on-empty-stack-is-noop"
    (with-screen (s 20 5)
      (setf (cl-tmux/terminal/types:screen-title s) "kept")
      (feed s (esc "[<t"))          ; pop on empty stack — no-op
      (expect (string= "kept" (cl-tmux/terminal/types:screen-title s)))))

  ;; XTPUSHTITLE discards the oldest entry when the stack exceeds 8 entries.
  (it "xtpushtitle-stack-bounded-at-8"
    (with-screen (s 20 5)
      ;; Push 9 times — stack cap is 8.
      (dotimes (i 9)
        (setf (cl-tmux/terminal/types:screen-title s) (format nil "t~D" i))
        (feed s (esc "[>t")))
      (expect (<= (length (cl-tmux/terminal/types:screen-title-stack s)) 8)))))

;;; ── DEC Rectangle operations (DECERA / DECFRA / DECCRA) ─────────────────────

(describe "terminal-suite/dec-rect-ops"

  ;; ── DECERA ───────────────────────────────────────────────────────────────────

  ;; DECERA ($ z) replaces cells inside the rectangle with blanks.
  (it "decera-erases-interior-rectangle"
    (with-screen (s 10 5)
      ;; Fill entire screen with 'A'.
      (dotimes (y 5)
        (dotimes (x 10)
          (setf (cl-tmux/terminal/types:screen-cell s x y)
                (cl-tmux/terminal/types:make-cell :char #\A))))
      ;; Erase rows 2-3 (1-based), columns 3-6 (1-based) → 0-based: rows 1-2, cols 2-5.
      (feed s (esc "[2;3;3;6$z"))
      ;; Cells inside the rectangle must be spaces.
      (loop for y from 1 to 2 do
        (loop for x from 2 to 5 do
          (expect (char= #\Space (cl-tmux/terminal/types:cell-char
                              (cl-tmux/terminal/types:screen-cell s x y))))))
      ;; Cells outside must still be 'A'.
      (expect (char= #\A (cl-tmux/terminal/types:cell-char
                      (cl-tmux/terminal/types:screen-cell s 0 0))))
      (expect (char= #\A (cl-tmux/terminal/types:cell-char
                      (cl-tmux/terminal/types:screen-cell s 9 4))))))

  ;; DECERA with top > bottom or left > right does not modify the screen.
  (it "decera-degenerate-rect-is-noop"
    (with-screen (s 10 5)
      (dotimes (y 5)
        (dotimes (x 10)
          (setf (cl-tmux/terminal/types:screen-cell s x y)
                (cl-tmux/terminal/types:make-cell :char #\B))))
      ;; top=3 > bottom=1 → degenerate, no erase.
      (feed s (esc "[3;1;1;5$z"))
      (expect (char= #\B (cl-tmux/terminal/types:cell-char
                      (cl-tmux/terminal/types:screen-cell s 0 0))))))

  ;; ── DECFRA ───────────────────────────────────────────────────────────────────

  ;; DECFRA ($ x) fills a rectangle with the given character.
  (it "decfra-fills-rectangle-with-character"
    (with-screen (s 10 5)
      ;; Fill rectangle rows 1-3 (1-based), cols 2-5 (1-based) with '*' (code 42).
      (feed s (esc "[42;1;2;3;5$x"))
      ;; 0-based: rows 0-2, cols 1-4.
      (loop for y from 0 to 2 do
        (loop for x from 1 to 4 do
          (expect (char= #\* (cl-tmux/terminal/types:cell-char
                          (cl-tmux/terminal/types:screen-cell s x y))))))
      ;; Outside the rect: still default space.
      (expect (char= #\Space (cl-tmux/terminal/types:cell-char
                          (cl-tmux/terminal/types:screen-cell s 0 0))))))

  ;; DECFRA with char-code 0 defaults to space (guard against null character).
  (it "decfra-zero-char-code-uses-space"
    (with-screen (s 10 5)
      ;; char=0 → should fill with space, not null byte.
      (feed s (esc "[0;1;1;2;2$x"))
      (expect (char= #\Space (cl-tmux/terminal/types:cell-char
                          (cl-tmux/terminal/types:screen-cell s 0 0))))))

  ;; ── DECCRA ───────────────────────────────────────────────────────────────────

  ;; DECCRA ($ v) copies source rectangle to target position.
  (it "deccra-copies-rectangle-to-target"
    (with-screen (s 20 5)
      ;; Write 'A' at rows 0-1 (0-based), cols 0-2 (0-based).
      (dotimes (y 2)
        (dotimes (x 3)
          (setf (cl-tmux/terminal/types:screen-cell s x y)
                (cl-tmux/terminal/types:make-cell :char #\A))))
      ;; DECCRA: src top=1 left=1 bottom=2 right=3 page=0, tgt top=3 left=6 page=0.
      ;; 0-based src: rows 0-1, cols 0-2. Target 0-based: row 2, col 5.
      (feed s (esc "[1;1;2;3;0;3;6;0$v"))
      ;; Target cells (0-based rows 2-3, cols 5-7) must now be 'A'.
      (loop for y from 2 to 3 do
        (loop for x from 5 to 7 do
          (expect (char= #\A (cl-tmux/terminal/types:cell-char
                          (cl-tmux/terminal/types:screen-cell s x y))))))
      ;; Source cells must be unchanged.
      (loop for y from 0 to 1 do
        (loop for x from 0 to 2 do
          (expect (char= #\A (cl-tmux/terminal/types:cell-char
                          (cl-tmux/terminal/types:screen-cell s x y))))))))

  ;; DECCRA handles overlapping src/tgt by buffering — avoids partial-copy corruption.
  (it "deccra-overlapping-regions-are-correct"
    (with-screen (s 20 5)
      ;; Write distinct chars in a row: 'A' 'B' 'C' 'D' 'E' at row 0, cols 0-4.
      (loop for x from 0 to 4 do
        (setf (cl-tmux/terminal/types:screen-cell s x 0)
              (cl-tmux/terminal/types:make-cell :char (code-char (+ (char-code #\A) x)))))
      ;; Copy src row=1 col=1 to row=1 col=3 (0-based: row 0, cols 0-2)
      ;; to target row=1 col=2 (1-based) — target overlaps source starting at col 1.
      ;; After: row 0 cols 1-3 = 'A' 'B' 'C'
      (feed s (esc "[1;1;1;3;0;1;2;0$v"))
      (expect (char= #\A (cl-tmux/terminal/types:cell-char (cl-tmux/terminal/types:screen-cell s 1 0))))
      (expect (char= #\B (cl-tmux/terminal/types:cell-char (cl-tmux/terminal/types:screen-cell s 2 0))))
      (expect (char= #\C (cl-tmux/terminal/types:cell-char (cl-tmux/terminal/types:screen-cell s 3 0))))
      ;; Source col 0 (outside tgt) must still be A.
      (expect (char= #\A (cl-tmux/terminal/types:cell-char (cl-tmux/terminal/types:screen-cell s 0 0)))))))
