(in-package #:cl-tmux/test)

;;;; Cell tests (src/terminal/cell.lisp).
;;;; Tests: double-width (char-width, wide char layout).
;;;;
;;;; This file also declares the top-level terminal-suite, because it is the
;;;; first terminal test file loaded (cell.lisp precedes screen.lisp in the
;;;; build order).

;;; ── Top-level terminal suite (must be declared here — first file loaded) ─────

(def-suite terminal-suite :description "VT100/ANSI terminal emulator")

;;; ── SUITE: double-width (CJK) ───────────────────────────────────────────────

(def-suite double-width
  :description "East-Asian wide character cell occupancy and cursor advance"
  :in terminal-suite)
(in-suite double-width)

(test char-width-classification
  "char-width returns 2 for wide CJK/kana and 1 for ASCII and box drawing."
  (is (= 1 (char-width #\a)))
  (is (= 1 (char-width #\Space)))
  (is (= 2 (char-width #\あ)) "Hiragana is double-width")
  (is (= 2 (char-width #\中)) "CJK ideograph is double-width")
  (is (= 1 (char-width #\│)) "box drawing stays single-width"))

(test wide-char-occupies-two-columns
  "A wide char fills a lead cell + continuation cell and advances the cursor 2."
  (with-screen (s 10 2)
    (utf8-feed s "あ")
    (is (char= #\あ (char-at s 0 0)))
    (is (= 2 (cell-width (cell-at s 0 0))) "lead cell width 2")
    (is (= 0 (cell-width (cell-at s 1 0))) "continuation cell width 0")
    (check-cursor s 2 0)))

(test wide-char-wraps-at-right-edge
  "A wide char that cannot fit in the last column wraps to the next row."
  (with-screen (s 3 2)
    (feed s "ab")            ; cursor at column 2 (last column of a 3-wide screen)
    (utf8-feed s "あ")       ; cannot fit one column → wraps to row 1
    (is (char= #\a  (char-at s 0 0)))
    (is (char= #\b  (char-at s 1 0)))
    (is (char= #\Space (char-at s 2 0)) "vacated last column is blank")
    (is (char= #\あ (char-at s 0 1)) "wide char wrapped to next row")
    (check-cursor s 2 1)))

;;; ── blank-cell ───────────────────────────────────────────────────────────────

(test blank-cell-returns-default-cell
  "blank-cell returns a space-character cell with default colours and width 1."
  (let ((c (cl-tmux/terminal/types:blank-cell)))
    (is (char= #\Space (cell-char c))  "blank-cell char must be space")
    (is (= 7 (cell-fg    c))           "blank-cell fg must be default (7)")
    (is (= 0 (cell-bg    c))           "blank-cell bg must be default (0)")
    (is (= 0 (cell-attrs c))           "blank-cell attrs must be 0")
    (is (= 1 (cell-width c))           "blank-cell width must be 1")))

;;; ── safe-code-char ───────────────────────────────────────────────────────────

(test safe-code-char-valid-codepoint
  "safe-code-char returns the character for a valid code point."
  (is (char= #\A (cl-tmux/terminal/types:safe-code-char 65)))
  (is (char= #\a (cl-tmux/terminal/types:safe-code-char 97))))

(test safe-code-char-invalid-codepoint-returns-replacement
  "safe-code-char returns U+FFFD for a code point outside char-code-limit."
  ;; char-code-limit is implementation-defined but always > 0x110000 on SBCL.
  ;; Use a known-bad value well above Unicode range.
  (let ((replacement (cl-tmux/terminal/types:safe-code-char
                      (+ char-code-limit 1))))
    (is (= #xFFFD (char-code replacement))
        "out-of-range code point must return U+FFFD")))

(test define-wide-char-ranges-macro-is-defined
  "define-wide-char-ranges is a defined macro."
  (is (macro-function 'cl-tmux/terminal/types::define-wide-char-ranges)))
