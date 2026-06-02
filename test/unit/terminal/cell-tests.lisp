(in-package #:cl-tmux/test)

;;;; Cell tests (src/terminal/cell.lisp).
;;;; Tests: attribute constants, cell struct, blank-cell, clamp,
;;;;        safe-code-char, char-width classification, combining chars,
;;;;        DEC-graphics remapping, and wide-char layout helpers.
;;;;
;;;; This file also declares the top-level terminal-suite, because it is the
;;;; first terminal test file loaded (cell.lisp precedes screen.lisp in the
;;;; build order).

;;; ── Top-level terminal suite (must be declared here — first file loaded) ─────

(def-suite terminal-suite :description "VT100/ANSI terminal emulator")

;;; ── SUITE: attribute bit constants ───────────────────────────────────────────

(def-suite attr-constants
  :description "Attribute bit constant values and mutual exclusivity"
  :in terminal-suite)
(in-suite attr-constants)

(test attr-bold-is-bit-0
  :description "The bold attribute constant occupies bit 0."
  (is (= #b00000001 cl-tmux/terminal/types:+attr-bold+)))

(test attr-dim-is-bit-1
  :description "The dim attribute constant occupies bit 1."
  (is (= #b00000010 cl-tmux/terminal/types:+attr-dim+)))

(test attr-reverse-is-bit-2
  :description "The reverse-video attribute constant occupies bit 2."
  (is (= #b00000100 cl-tmux/terminal/types:+attr-reverse+)))

(test attr-underline-is-bit-3
  :description "The underline attribute constant occupies bit 3."
  (is (= #b00001000 cl-tmux/terminal/types:+attr-underline+)))

(test attr-blink-is-bit-4
  :description "The blink attribute constant occupies bit 4."
  (is (= #b00010000 cl-tmux/terminal/types:+attr-blink+)))

(test attr-italic-is-bit-5
  :description "The italic attribute constant occupies bit 5."
  (is (= #b00100000 cl-tmux/terminal/types:+attr-italic+)))

(test attr-conceal-is-bit-6
  :description "The conceal attribute constant occupies bit 6."
  (is (= #b01000000 cl-tmux/terminal/types:+attr-conceal+)))

(test attr-strikethrough-is-bit-7
  :description "The strikethrough attribute constant occupies bit 7."
  (is (= #b10000000 cl-tmux/terminal/types:+attr-strikethrough+)))

(test attr2-double-underline-is-bit-0
  :description "The double-underline extended attribute occupies attrs2 bit 0."
  (is (= #b00000001 cl-tmux/terminal/types:+attr2-double-underline+)))

(test attr2-overline-is-bit-1
  :description "The overline extended attribute occupies attrs2 bit 1."
  (is (= #b00000010 cl-tmux/terminal/types:+attr2-overline+)))

(test attr-constants-are-distinct-single-bits
  :description "All eight primary attribute constants are distinct powers of 2."
  (let ((constants (list cl-tmux/terminal/types:+attr-bold+
                         cl-tmux/terminal/types:+attr-dim+
                         cl-tmux/terminal/types:+attr-reverse+
                         cl-tmux/terminal/types:+attr-underline+
                         cl-tmux/terminal/types:+attr-blink+
                         cl-tmux/terminal/types:+attr-italic+
                         cl-tmux/terminal/types:+attr-conceal+
                         cl-tmux/terminal/types:+attr-strikethrough+)))
    (is (= 8 (length (remove-duplicates constants)))
        "All eight attribute constants must be distinct")
    (is (every #'(lambda (c) (= 1 (logcount c))) constants)
        "Each constant must be a single-bit power of 2")))

;;; ── SUITE: cell struct ───────────────────────────────────────────────────────

(def-suite cell-struct
  :description "make-cell constructor and slot default values"
  :in terminal-suite)
(in-suite cell-struct)

(test make-cell-default-slots
  :description "make-cell with no arguments returns a space/default-color/no-attrs cell."
  (let ((c (cl-tmux/terminal/types:make-cell)))
    (is (char= #\Space (cell-char c))  "default char must be Space")
    (is (= 7   (cell-fg    c))         "default fg must be 7 (white)")
    (is (= 0   (cell-bg    c))         "default bg must be 0 (black)")
    (is (= 0   (cell-attrs c))         "default attrs must be 0")
    (is (= 0   (cl-tmux/terminal/types:cell-attrs2    c)) "default attrs2 must be 0")
    (is (= 0   (cl-tmux/terminal/types:cell-ul-color  c)) "default ul-color must be 0")
    (is (null  (cl-tmux/terminal/types:cell-combining c)) "default combining must be NIL")
    (is (= 1   (cell-width c))         "default width must be 1")))

(test make-cell-custom-slots
  :description "make-cell with explicit keyword arguments stores all supplied values."
  (let ((c (cl-tmux/terminal/types:make-cell :char #\A :fg 2 :bg 5 :attrs 3
                                              :attrs2 1 :ul-color 4 :width 2)))
    (is (char= #\A (cell-char c)))
    (is (= 2 (cell-fg    c)))
    (is (= 5 (cell-bg    c)))
    (is (= 3 (cell-attrs c)))
    (is (= 1 (cl-tmux/terminal/types:cell-attrs2   c)))
    (is (= 4 (cl-tmux/terminal/types:cell-ul-color c)))
    (is (= 2 (cell-width c)))))

(test make-cell-continuation-width-zero
  :description "make-cell :width 0 produces a valid continuation placeholder."
  (let ((c (cl-tmux/terminal/types:make-cell :char #\Space :width 0)))
    (is (= 0 (cell-width c)) "continuation cell width must be 0")))

;;; ── blank-cell ───────────────────────────────────────────────────────────────

(test blank-cell-returns-default-cell
  "blank-cell returns a space-character cell with default colours and width 1."
  (let ((c (cl-tmux/terminal/types:blank-cell)))
    (is (char= #\Space (cell-char c))  "blank-cell char must be space")
    (is (= 7 (cell-fg    c))           "blank-cell fg must be default (7)")
    (is (= 0 (cell-bg    c))           "blank-cell bg must be default (0)")
    (is (= 0 (cell-attrs c))           "blank-cell attrs must be 0")
    (is (= 1 (cell-width c))           "blank-cell width must be 1")))

(test blank-cell-returns-fresh-instance-each-call
  :description "Each call to blank-cell returns a structurally equal but distinct object."
  (let ((c1 (cl-tmux/terminal/types:blank-cell))
        (c2 (cl-tmux/terminal/types:blank-cell)))
    (is (not (eq c1 c2)) "blank-cell must return a fresh struct each call")))

;;; ── clamp ────────────────────────────────────────────────────────────────────

(def-suite clamp-suite
  :description "clamp utility: boundary and interior behaviour"
  :in terminal-suite)
(in-suite clamp-suite)

(test clamp-value-below-lo-returns-lo
  :description "clamp returns LO when V < LO."
  (is (= 0  (cl-tmux/terminal/types:clamp -5 0 10)))
  (is (= 3  (cl-tmux/terminal/types:clamp  1 3  9))))

(test clamp-value-above-hi-returns-hi
  :description "clamp returns HI when V > HI."
  (is (= 10 (cl-tmux/terminal/types:clamp 99 0 10)))
  (is (= 9  (cl-tmux/terminal/types:clamp 20 3  9))))

(test clamp-value-within-range-returned-unchanged
  :description "clamp returns V unchanged when LO <= V <= HI."
  (is (= 5  (cl-tmux/terminal/types:clamp  5 0 10)))
  (is (= 0  (cl-tmux/terminal/types:clamp  0 0 10)))
  (is (= 10 (cl-tmux/terminal/types:clamp 10 0 10))))

(test clamp-lo-equals-hi-returns-lo
  :description "When LO equals HI, clamp always returns that value."
  (is (= 7 (cl-tmux/terminal/types:clamp  0 7 7)))
  (is (= 7 (cl-tmux/terminal/types:clamp  7 7 7)))
  (is (= 7 (cl-tmux/terminal/types:clamp 99 7 7))))

;;; ── safe-code-char ───────────────────────────────────────────────────────────

(test safe-code-char-valid-codepoint
  "safe-code-char returns the character for a valid code point."
  (is (char= #\A (cl-tmux/terminal/types:safe-code-char 65)))
  (is (char= #\a (cl-tmux/terminal/types:safe-code-char 97))))

(test safe-code-char-zero-returns-null-char
  :description "safe-code-char with code point 0 returns the NUL character."
  (is (= 0 (char-code (cl-tmux/terminal/types:safe-code-char 0)))))

(test safe-code-char-invalid-codepoint-returns-replacement
  "safe-code-char returns U+FFFD for a code point outside char-code-limit."
  ;; char-code-limit is implementation-defined but always > 0x110000 on SBCL.
  ;; Use a known-bad value well above Unicode range.
  (let ((replacement (cl-tmux/terminal/types:safe-code-char
                      (+ char-code-limit 1))))
    (is (= #xFFFD (char-code replacement))
        "out-of-range code point must return U+FFFD")))

;;; ── SUITE: char-width / double-width ─────────────────────────────────────────

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

(test char-width-hangul-jamo-is-wide
  :description "U+1100 (Hangul Jamo range start) has display width 2."
  (is (= 2 (char-width (code-char #x1100)))))

(test char-width-fullwidth-ascii-is-wide
  :description "U+FF21 (Fullwidth Latin Capital A) has display width 2."
  (is (= 2 (char-width (code-char #xFF21)))))

(test char-width-ascii-range-is-single
  :description "All printable ASCII characters have display width 1."
  (loop for cp from 32 to 126
        do (is (= 1 (char-width (code-char cp)))
               "ASCII ~C (U+~4,'0X) must have width 1"
               (code-char cp) cp)))

(test define-wide-char-ranges-macro-is-defined
  "define-wide-char-ranges is a defined macro."
  (is (macro-function 'cl-tmux/terminal/types::define-wide-char-ranges)))

;;; ── wide-char layout ─────────────────────────────────────────────────────────

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

;;; ── SUITE: combining characters ──────────────────────────────────────────────

(def-suite combining-chars
  :description "Unicode combining character detection and cell appending"
  :in terminal-suite)
(in-suite combining-chars)

(test combining-char-p-diacritic-marks-return-true
  :description "Code points in the Combining Diacritical Marks block are combining."
  ;; U+0300 COMBINING GRAVE ACCENT (first in the block)
  (is-true  (cl-tmux/terminal/actions:combining-char-p (code-char #x0300))
            "U+0300 must be a combining char")
  ;; U+036F last in Combining Diacritical Marks
  (is-true  (cl-tmux/terminal/actions:combining-char-p (code-char #x036F))
            "U+036F must be a combining char"))

(test combining-char-p-ascii-returns-false
  :description "Ordinary ASCII characters are not combining."
  (is-false (cl-tmux/terminal/actions:combining-char-p #\a)
            "letter a must not be a combining char")
  (is-false (cl-tmux/terminal/actions:combining-char-p #\Space)
            "space must not be a combining char"))

(test combining-char-p-half-marks-return-true
  :description "Combining Half Marks (U+FE20-FE2F) are combining."
  (is-true (cl-tmux/terminal/actions:combining-char-p (code-char #xFE20))
           "U+FE20 Combining Ligature Left Half must be combining"))

(test write-char-at-cursor-combining-char-appended-not-advanced
  :description "Writing a combining char appends it to the previous cell; cursor does not move."
  (with-screen (s 10 5)
    (feed s "a")                        ; base character at col 0; cursor now at col 1
    ;; Write a combining grave accent (U+0300)
    (cl-tmux/terminal/actions:write-char-at-cursor s (code-char #x0300))
    ;; Cursor must still be at col 1 (not advanced)
    (check-cursor s 1 0)
    ;; The combining list of cell (0,0) must contain the diacritic
    (let ((combining (cl-tmux/terminal/types:cell-combining (cell-at s 0 0))))
      (is (member (code-char #x0300) combining)
          "U+0300 must be in the combining list of the base cell"))))

(test write-char-at-cursor-combining-at-col-zero-appended-to-col-zero
  :description "A combining char at column 0 is appended to cell (0,0) — no underflow."
  (with-screen (s 10 5)
    ;; cursor starts at col 0; write a combining char without first writing a base
    (cl-tmux/terminal/actions:write-char-at-cursor s (code-char #x0301))
    ;; cursor must remain at col 0
    (check-cursor s 0 0)
    ;; No error and combining list on (0,0) must contain the mark
    (is (member (code-char #x0301) (cl-tmux/terminal/types:cell-combining (cell-at s 0 0)))
        "combining mark must be appended to cell (0,0)")))

;;; ── SUITE: DEC special graphics character set ────────────────────────────────

(def-suite dec-graphics
  :description "DEC special graphics (ESC ( 0) charset remapping"
  :in terminal-suite)
(in-suite dec-graphics)

(test dec-graphics-table-macro-is-defined
  :description "define-dec-graphics-table macro is fbound."
  (is (macro-function 'cl-tmux/terminal/actions::define-dec-graphics-table)))

(test dec-graphics-remaps-box-drawing-chars
  :description "With charset :dec-graphics, ASCII j/k/l/m map to box-drawing Unicode."
  (with-screen (s 10 5)
    (setf (cl-tmux/terminal/types:screen-charset s) :dec-graphics)
    ;; 'j' -> lower-right corner ┘
    (cl-tmux/terminal/actions:write-char-at-cursor s #\j)
    (is (char= #\┘ (char-at s 0 0)) "j should map to lower-right corner ┘")
    ;; 'q' -> horizontal line ─
    (cl-tmux/terminal/actions:write-char-at-cursor s #\q)
    (is (char= #\─ (char-at s 1 0)) "q should map to horizontal line ─")
    ;; 'x' -> vertical line │
    (cl-tmux/terminal/actions:write-char-at-cursor s #\x)
    (is (char= #\│ (char-at s 2 0)) "x should map to vertical line │")))

(test dec-graphics-unmapped-char-passes-through
  :description "A character not in the DEC table passes through unchanged."
  (with-screen (s 10 5)
    (setf (cl-tmux/terminal/types:screen-charset s) :dec-graphics)
    ;; 'A' has no entry in the DEC table → should write as-is
    (cl-tmux/terminal/actions:write-char-at-cursor s #\A)
    (is (char= #\A (char-at s 0 0)) "unmapped ASCII should be written unchanged")))

(test remap-charset-char-ascii-mode-returns-unchanged
  :description "%remap-charset-char with charset :ascii returns char unchanged."
  (with-screen (s 10 5)
    ;; screen-charset defaults to :ascii
    (is (char= #\j
               (cl-tmux/terminal/actions::%remap-charset-char s #\j))
        "In :ascii mode, 'j' must not be remapped")))

(test remap-charset-char-dec-graphics-remaps-j
  :description "%remap-charset-char with charset :dec-graphics remaps j to ┘."
  (with-screen (s 10 5)
    (setf (cl-tmux/terminal/types:screen-charset s) :dec-graphics)
    (is (char= #\┘
               (cl-tmux/terminal/actions::%remap-charset-char s #\j))
        "In :dec-graphics mode, 'j' must map to lower-right corner ┘")))
