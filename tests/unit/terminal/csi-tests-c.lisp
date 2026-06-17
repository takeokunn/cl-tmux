(in-package #:cl-tmux/test)

;;;; csi tests — part C: csi-unknown-sequences, DECOM/origin-mode,
;;;; cup-row-direct, enqueue-* helpers, XTPUSHTITLE/XTPOPTITLE,
;;;; DEC Rectangle operations (DECERA/DECFRA/DECCRA).

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

(test enqueue-static-reply-signatures-table
  "enqueue-dsr/da1/da2-reply each push a string with the expected fixed signature."
  (dolist (row (list (list #'cl-tmux/terminal/csi::enqueue-dsr-reply "[0n"   "dsr → [0n")
                     (list #'cl-tmux/terminal/csi::enqueue-da1-reply "?1;2c" "da1 → ?1;2c")
                     (list #'cl-tmux/terminal/csi::enqueue-da2-reply ">1;"   "da2 → >1;")))
    (destructuring-bind (fn expected-sub desc) row
      (with-screen (s 20 5)
        (funcall fn s)
        (is (some (lambda (r) (search expected-sub r))
                  (cl-tmux/terminal/types:screen-response-queue s))
            "~A" desc)))))

(test enqueue-cpr-reply-reflects-cursor
  "enqueue-cpr-reply pushes ESC[row;colR reflecting the current cursor (1-based)."
  (with-screen (s 20 10)
    (feed s (esc "[3;5H"))     ; cursor → row 2, col 4 (0-based)
    (cl-tmux/terminal/csi::enqueue-cpr-reply s)
    (is (some (lambda (r) (search "[3;5R" r))
              (cl-tmux/terminal/types:screen-response-queue s))
        "enqueue-cpr-reply must contain '[3;5R' for cursor at (row=2,col=4)")))

;;; ── XTPUSHTITLE / XTPOPTITLE (CSI > Ps t / CSI < Ps t) ─────────────────────

(test xtpushtitle-saves-current-title
  "CSI > t (XTPUSHTITLE) pushes the current title onto the title stack."
  (with-screen (s 20 5)
    (setf (cl-tmux/terminal/types:screen-title s) "initial")
    (feed s (esc "[>t"))   ; push
    (is (equal '("initial") (cl-tmux/terminal/types:screen-title-stack s))
        "title-stack must contain the saved title after a push")))

(test xtpoptitle-restores-saved-title
  "CSI < t (XTPOPTITLE) pops and restores the most recently pushed title."
  (with-screen (s 20 5)
    (setf (cl-tmux/terminal/types:screen-title s) "original")
    (feed s (esc "[>t"))          ; push "original"
    (setf (cl-tmux/terminal/types:screen-title s) "changed")
    (feed s (esc "[<t"))          ; pop → restore "original"
    (is (string= "original" (cl-tmux/terminal/types:screen-title s))
        "title must be restored to 'original' after pop")
    (is (null (cl-tmux/terminal/types:screen-title-stack s))
        "title-stack must be empty after the pop")))

(test xtpoptitle-on-empty-stack-is-noop
  "CSI < t (XTPOPTITLE) on an empty stack is a no-op: title unchanged."
  (with-screen (s 20 5)
    (setf (cl-tmux/terminal/types:screen-title s) "kept")
    (feed s (esc "[<t"))          ; pop on empty stack — no-op
    (is (string= "kept" (cl-tmux/terminal/types:screen-title s))
        "title must remain 'kept' after pop on empty stack")))

(test xtpushtitle-stack-bounded-at-8
  "XTPUSHTITLE discards the oldest entry when the stack exceeds 8 entries."
  (with-screen (s 20 5)
    ;; Push 9 times — stack cap is 8.
    (dotimes (i 9)
      (setf (cl-tmux/terminal/types:screen-title s) (format nil "t~D" i))
      (feed s (esc "[>t")))
    (is (<= (length (cl-tmux/terminal/types:screen-title-stack s)) 8)
        "title-stack must never exceed 8 entries")))

;;; ── DEC Rectangle operations (DECERA / DECFRA / DECCRA) ─────────────────────

(def-suite dec-rect-ops
  :description "DECERA ($ z) / DECFRA ($ x) / DECCRA ($ v) rectangle operations"
  :in terminal-suite)
(in-suite dec-rect-ops)

;; ── DECERA ───────────────────────────────────────────────────────────────────

(test decera-erases-interior-rectangle
  "DECERA ($ z) replaces cells inside the rectangle with blanks."
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
        (is (char= #\Space (cl-tmux/terminal/types:cell-char
                            (cl-tmux/terminal/types:screen-cell s x y)))
            "cell (~D,~D) must be erased to space" x y)))
    ;; Cells outside must still be 'A'.
    (is (char= #\A (cl-tmux/terminal/types:cell-char
                    (cl-tmux/terminal/types:screen-cell s 0 0)))
        "cell (0,0) outside rect must remain A")
    (is (char= #\A (cl-tmux/terminal/types:cell-char
                    (cl-tmux/terminal/types:screen-cell s 9 4)))
        "cell (9,4) outside rect must remain A")))

(test decera-degenerate-rect-is-noop
  "DECERA with top > bottom or left > right does not modify the screen."
  (with-screen (s 10 5)
    (dotimes (y 5)
      (dotimes (x 10)
        (setf (cl-tmux/terminal/types:screen-cell s x y)
              (cl-tmux/terminal/types:make-cell :char #\B))))
    ;; top=3 > bottom=1 → degenerate, no erase.
    (feed s (esc "[3;1;1;5$z"))
    (is (char= #\B (cl-tmux/terminal/types:cell-char
                    (cl-tmux/terminal/types:screen-cell s 0 0)))
        "cell (0,0) must remain B when rect is degenerate")))

;; ── DECFRA ───────────────────────────────────────────────────────────────────

(test decfra-fills-rectangle-with-character
  "DECFRA ($ x) fills a rectangle with the given character."
  (with-screen (s 10 5)
    ;; Fill rectangle rows 1-3 (1-based), cols 2-5 (1-based) with '*' (code 42).
    (feed s (esc "[42;1;2;3;5$x"))
    ;; 0-based: rows 0-2, cols 1-4.
    (loop for y from 0 to 2 do
      (loop for x from 1 to 4 do
        (is (char= #\* (cl-tmux/terminal/types:cell-char
                        (cl-tmux/terminal/types:screen-cell s x y)))
            "cell (~D,~D) must be * after DECFRA" x y)))
    ;; Outside the rect: still default space.
    (is (char= #\Space (cl-tmux/terminal/types:cell-char
                        (cl-tmux/terminal/types:screen-cell s 0 0)))
        "cell (0,0) outside rect must remain space")))

(test decfra-zero-char-code-uses-space
  "DECFRA with char-code 0 defaults to space (guard against null character)."
  (with-screen (s 10 5)
    ;; char=0 → should fill with space, not null byte.
    (feed s (esc "[0;1;1;2;2$x"))
    (is (char= #\Space (cl-tmux/terminal/types:cell-char
                        (cl-tmux/terminal/types:screen-cell s 0 0)))
        "char-code 0 must produce space in filled cell")))

;; ── DECCRA ───────────────────────────────────────────────────────────────────

(test deccra-copies-rectangle-to-target
  "DECCRA ($ v) copies source rectangle to target position."
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
        (is (char= #\A (cl-tmux/terminal/types:cell-char
                        (cl-tmux/terminal/types:screen-cell s x y)))
            "target cell (~D,~D) must be A after DECCRA" x y)))
    ;; Source cells must be unchanged.
    (loop for y from 0 to 1 do
      (loop for x from 0 to 2 do
        (is (char= #\A (cl-tmux/terminal/types:cell-char
                        (cl-tmux/terminal/types:screen-cell s x y)))
            "source cell (~D,~D) must remain A after DECCRA" x y)))))

(test deccra-overlapping-regions-are-correct
  "DECCRA handles overlapping src/tgt by buffering — avoids partial-copy corruption."
  (with-screen (s 20 5)
    ;; Write distinct chars in a row: 'A' 'B' 'C' 'D' 'E' at row 0, cols 0-4.
    (loop for x from 0 to 4 do
      (setf (cl-tmux/terminal/types:screen-cell s x 0)
            (cl-tmux/terminal/types:make-cell :char (code-char (+ (char-code #\A) x)))))
    ;; Copy src row=1 col=1 to row=1 col=3 (0-based: row 0, cols 0-2)
    ;; to target row=1 col=2 (1-based) — target overlaps source starting at col 1.
    ;; After: row 0 cols 1-3 = 'A' 'B' 'C'
    (feed s (esc "[1;1;1;3;0;1;2;0$v"))
    (is (char= #\A (cl-tmux/terminal/types:cell-char (cl-tmux/terminal/types:screen-cell s 1 0)))
        "target col 1 must be A")
    (is (char= #\B (cl-tmux/terminal/types:cell-char (cl-tmux/terminal/types:screen-cell s 2 0)))
        "target col 2 must be B")
    (is (char= #\C (cl-tmux/terminal/types:cell-char (cl-tmux/terminal/types:screen-cell s 3 0)))
        "target col 3 must be C")
    ;; Source col 0 (outside tgt) must still be A.
    (is (char= #\A (cl-tmux/terminal/types:cell-char (cl-tmux/terminal/types:screen-cell s 0 0)))
        "source col 0 must remain A")))
