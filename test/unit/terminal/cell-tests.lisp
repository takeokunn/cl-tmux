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

(test attr-bit-values-table
  "Each attribute constant occupies exactly the declared bit position in its byte."
  (dolist (row (list (list #b00000001 cl-tmux/terminal/types:+attr-bold+              "bold is bit 0")
                     (list #b00000010 cl-tmux/terminal/types:+attr-dim+               "dim is bit 1")
                     (list #b00000100 cl-tmux/terminal/types:+attr-reverse+           "reverse is bit 2")
                     (list #b00001000 cl-tmux/terminal/types:+attr-underline+         "underline is bit 3")
                     (list #b00010000 cl-tmux/terminal/types:+attr-blink+             "blink is bit 4")
                     (list #b00100000 cl-tmux/terminal/types:+attr-italic+            "italic is bit 5")
                     (list #b01000000 cl-tmux/terminal/types:+attr-conceal+           "conceal is bit 6")
                     (list #b10000000 cl-tmux/terminal/types:+attr-strikethrough+     "strikethrough is bit 7")
                     (list #b00000001 cl-tmux/terminal/types:+attr2-double-underline+ "double-underline is attrs2 bit 0")
                     (list #b00000010 cl-tmux/terminal/types:+attr2-overline+         "overline is attrs2 bit 1")))
    (destructuring-bind (expected constant desc) row
      (is (= expected constant) "~A" desc))))

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
    (is (char= #\Space (cell-char c)) "default char must be Space")
    (dolist (row (list (list (cell-fg   c)                             7 "default fg must be 7 (white)")
                       (list (cell-bg   c)                             0 "default bg must be 0 (black)")
                       (list (cell-attrs c)                            0 "default attrs must be 0")
                       (list (cl-tmux/terminal/types:cell-attrs2    c) 0 "default attrs2 must be 0")
                       (list (cl-tmux/terminal/types:cell-ul-color  c) 0 "default ul-color must be 0")
                       (list (cell-width c)                            1 "default width must be 1")))
      (destructuring-bind (actual expected desc) row
        (is (= expected actual) "~A" desc)))
    (is (null (cl-tmux/terminal/types:cell-combining c)) "default combining must be NIL")))

(test make-cell-custom-slots
  :description "make-cell with explicit keyword arguments stores all supplied values."
  (let ((c (cl-tmux/terminal/types:make-cell :char #\A :fg 2 :bg 5 :attrs 3
                                              :attrs2 1 :ul-color 4 :width 2)))
    (is (char= #\A (cell-char c)) "char must be #\\A")
    (dolist (row (list (list (cell-fg   c)                            2 "fg")
                       (list (cell-bg   c)                            5 "bg")
                       (list (cell-attrs c)                           3 "attrs")
                       (list (cl-tmux/terminal/types:cell-attrs2   c) 1 "attrs2")
                       (list (cl-tmux/terminal/types:cell-ul-color c) 4 "ul-color")
                       (list (cell-width c)                           2 "width")))
      (destructuring-bind (actual expected desc) row
        (is (= expected actual) "~A" desc)))))

(test make-cell-continuation-width-zero
  :description "make-cell :width 0 produces a valid continuation placeholder."
  (let ((c (cl-tmux/terminal/types:make-cell :char #\Space :width 0)))
    (is (= 0 (cell-width c)) "continuation cell width must be 0")))

(test cell-p-returns-true-for-cell
  :description "cell-p returns T for a struct produced by make-cell."
  (let ((c (cl-tmux/terminal/types:make-cell)))
    (is-true (cl-tmux/terminal/types:cell-p c)
             "cell-p must return T for a make-cell instance")))

(test cell-p-returns-false-for-non-cell
  :description "cell-p returns NIL for non-cell objects."
  (is-false (cl-tmux/terminal/types:cell-p 42)
            "cell-p must return NIL for an integer")
  (is-false (cl-tmux/terminal/types:cell-p "hello")
            "cell-p must return NIL for a string")
  (is-false (cl-tmux/terminal/types:cell-p nil)
            "cell-p must return NIL for NIL"))

(test make-cell-combining-slot
  :description "make-cell :combining with a list stores the combining characters."
  (let ((c (cl-tmux/terminal/types:make-cell
            :combining (list (code-char #x0300) (code-char #x0301)))))
    (is (= 2 (length (cl-tmux/terminal/types:cell-combining c)))
        "combining list must hold the two supplied marks")))

;;; ── blank-cell ───────────────────────────────────────────────────────────────

(test blank-cell-returns-default-cell
  "blank-cell returns a space-character cell with default colours and width 1."
  (let ((c (cl-tmux/terminal/types:blank-cell)))
    (is (char= #\Space (cell-char c)) "blank-cell char must be space")
    (dolist (row (list (list (cell-fg    c) 7 "blank-cell fg must be default (7)")
                       (list (cell-bg    c) 0 "blank-cell bg must be default (0)")
                       (list (cell-attrs c) 0 "blank-cell attrs must be 0")
                       (list (cell-width c) 1 "blank-cell width must be 1")))
      (destructuring-bind (actual expected desc) row
        (is (= expected actual) "~A" desc)))))

(test blank-cell-returns-fresh-instance-each-call
  :description "Each call to blank-cell returns a structurally equal but distinct object."
  (let ((c1 (cl-tmux/terminal/types:blank-cell))
        (c2 (cl-tmux/terminal/types:blank-cell)))
    (is (not (eq c1 c2)) "blank-cell must return a fresh struct each call")))

;;; ── %make-blank-cells ────────────────────────────────────────────────────────

(def-suite make-blank-cells-suite
  :description "%make-blank-cells: vector allocation and cell defaults"
  :in terminal-suite)
(in-suite make-blank-cells-suite)

(test make-blank-cells-returns-simple-vector-of-correct-length
  :description "%make-blank-cells returns a simple-vector of the requested length."
  (let ((v (cl-tmux/terminal/types:%make-blank-cells 10)))
    (is (simple-vector-p v) "%make-blank-cells must return a simple-vector")
    (is (= 10 (length v))   "vector length must equal the requested count")))

(test make-blank-cells-all-elements-are-blank
  :description "Every element returned by %make-blank-cells is a default space cell."
  (let ((v (cl-tmux/terminal/types:%make-blank-cells 5)))
    (dotimes (i 5)
      (let ((c (aref v i)))
        (is (cl-tmux/terminal/types:cell-p c)
            "element ~D must be a cell" i)
        (is (char= #\Space (cell-char c))
            "element ~D char must be space" i)
        (is (= 7 (cell-fg c))
            "element ~D fg must be 7" i)
        (is (= 0 (cell-bg c))
            "element ~D bg must be 0" i)
        (is (= 1 (cell-width c))
            "element ~D width must be 1" i)))))

(test make-blank-cells-zero-length-returns-empty-vector
  :description "%make-blank-cells with n=0 returns an empty simple-vector."
  (let ((v (cl-tmux/terminal/types:%make-blank-cells 0)))
    (is (simple-vector-p v) "result must be a simple-vector")
    (is (= 0 (length v))    "empty vector must have length 0")))

;;; ── clamp ────────────────────────────────────────────────────────────────────

(def-suite clamp-suite
  :description "clamp utility: boundary and interior behaviour"
  :in terminal-suite)
(in-suite clamp-suite)

;;; Table-driven clamp tests: (v lo hi expected description)
;;;
;;; This single table test covers all boundary cases: below lo, above hi,
;;; at boundaries, within range, and lo=hi degenerate.  The four individual
;;; named tests that previously existed were fully redundant with this table
;;; and have been removed to eliminate noise without losing coverage.
(test clamp-table
  :description "clamp correctly handles below, above, at, and within bounds."
  (dolist (case '((-5  0 10  0  "below lo clamps to lo")
                  ( 1  3  9  3  "below lo clamps to lo (3..9)")
                  (99  0 10 10  "above hi clamps to hi")
                  (20  3  9  9  "above hi clamps to hi (3..9)")
                  ( 5  0 10  5  "within range returned unchanged")
                  ( 0  0 10  0  "at lo boundary returned unchanged")
                  (10  0 10 10  "at hi boundary returned unchanged")
                  ( 0  7  7  7  "lo=hi, v below: always returns lo/hi")
                  ( 7  7  7  7  "lo=hi, v equal: always returns lo/hi")
                  (99  7  7  7  "lo=hi, v above: always returns lo/hi")))
    (destructuring-bind (v lo hi expected desc) case
      (is (= expected (cl-tmux/terminal/types:clamp v lo hi)) desc))))

;;; ── safe-code-char ───────────────────────────────────────────────────────────

(def-suite safe-code-char-suite
  :description "safe-code-char: valid, boundary, and invalid code-point handling"
  :in terminal-suite)
(in-suite safe-code-char-suite)

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

;;; Table-driven safe-code-char tests: (cp expected-char)
(test safe-code-char-table
  :description "safe-code-char table: well-known code-points map to expected characters."
  (dolist (case '((65  #\A    "U+0041 = LATIN CAPITAL LETTER A")
                  (97  #\a    "U+0061 = LATIN SMALL LETTER A")
                  (32  #\Space "U+0020 = SPACE")
                  (10  #\Newline "U+000A = LINE FEED")))
    (destructuring-bind (cp expected-char desc) case
      (is (char= expected-char (cl-tmux/terminal/types:safe-code-char cp)) desc))))

;;; ── SUITE: char-width / double-width ─────────────────────────────────────────

(def-suite double-width
  :description "East-Asian wide character cell occupancy and cursor advance"
  :in terminal-suite)
(in-suite double-width)

(test char-width-classification
  "char-width returns 2 for wide CJK/kana and 1 for ASCII and box drawing."
  (dolist (row '((#\a   1 "ASCII a is single-width")
                 (#\Space 1 "Space is single-width")
                 (#\あ  2 "Hiragana is double-width")
                 (#\中  2 "CJK ideograph is double-width")
                 (#\│   1 "box drawing stays single-width")))
    (destructuring-bind (char expected desc) row
      (is (= expected (char-width char)) "~A" desc))))

(test char-width-ascii-range-is-single
  :description "All printable ASCII characters have display width 1."
  (loop for cp from 32 to 126
        do (is (= 1 (char-width (code-char cp)))
               "ASCII ~C (U+~4,'0X) must have width 1"
               (code-char cp) cp)))

(test define-wide-char-ranges-macro-is-defined
  "define-wide-char-ranges is a defined macro accessible via single-colon export."
  (is (macro-function 'cl-tmux/terminal/types:define-wide-char-ranges)))

;;; Table-driven char-width boundary tests: (cp expected-width description)
(test char-width-range-boundaries-table
  :description "char-width returns the correct width at all range boundaries."
  (dolist (case `((#x1100 2 "U+1100 Hangul Jamo start")
                  (#x115F 2 "U+115F Hangul Jamo end")
                  (#x2E80 2 "U+2E80 CJK Radicals start")
                  (#x303E 2 "U+303E CJK Radicals end")
                  (#x3041 2 "U+3041 Hiragana start")
                  (#x33FF 2 "U+33FF CJK compat end")
                  (#x3400 2 "U+3400 CJK Extension A start")
                  (#x4DBF 2 "U+4DBF CJK Extension A end")
                  (#x4E00 2 "U+4E00 CJK Unified start")
                  (#x9FFF 2 "U+9FFF CJK Unified end")
                  (#xAC00 2 "U+AC00 Hangul syllables start")
                  (#xD7A3 2 "U+D7A3 Hangul syllables end")
                  (#xFF00 2 "U+FF00 Fullwidth ASCII start")
                  (#xFF21 2 "U+FF21 Fullwidth Latin Capital A (mid-range)")
                  (#xFF60 2 "U+FF60 Fullwidth ASCII end")
                  (#xFFE0 2 "U+FFE0 Fullwidth signs start")
                  (#xFFE6 2 "U+FFE6 Fullwidth signs end")
                  (#x1F2FF 1 "U+1F2FF below Emoji block — must be width 1")
                  (#x1F300 2 "U+1F300 Emoji/pictograph block start")
                  (#x1FAFF 2 "U+1FAFF Emoji/pictograph block end")
                  (#x20000 2 "U+20000 CJK Extension B start")))
    (destructuring-bind (cp expected-width desc) case
      (when (< cp char-code-limit)
        (is (= expected-width (char-width (code-char cp))) desc)))))

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
    (utf8-feed s "あ")       ; cannot fit one column -> wraps to row 1
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
  :description "A combining char at column 0 is appended to cell (0,0) -- no underflow."
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
    ;; 'j' -> lower-right corner
    (cl-tmux/terminal/actions:write-char-at-cursor s #\j)
    (is (char= #\┘ (char-at s 0 0)) "j should map to lower-right corner")
    ;; 'q' -> horizontal line
    (cl-tmux/terminal/actions:write-char-at-cursor s #\q)
    (is (char= #\─ (char-at s 1 0)) "q should map to horizontal line")
    ;; 'x' -> vertical line
    (cl-tmux/terminal/actions:write-char-at-cursor s #\x)
    (is (char= #\│ (char-at s 2 0)) "x should map to vertical line")))

(test dec-graphics-unmapped-char-passes-through
  :description "A character not in the DEC table passes through unchanged."
  (with-screen (s 10 5)
    (setf (cl-tmux/terminal/types:screen-charset s) :dec-graphics)
    ;; 'A' has no entry in the DEC table -> should write as-is
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
  :description "%remap-charset-char with charset :dec-graphics remaps j to lower-right corner."
  (with-screen (s 10 5)
    (setf (cl-tmux/terminal/types:screen-charset s) :dec-graphics)
    (is (char= #\┘
               (cl-tmux/terminal/actions::%remap-charset-char s #\j))
        "In :dec-graphics mode, 'j' must map to lower-right corner")))

;;; ── Background-colour erase (BCE) ────────────────────────────────────────────
;;;
;;; Cells cleared by ED/EL/ECH, the blanks from IL/DL/ICH/DCH, and lines exposed
;;; by scrolling must take the CURRENT SGR background colour, not the default.

(def-suite bce-suite :description "Background-colour erase" :in terminal-suite)
(in-suite bce-suite)

(test ed-clears-to-current-background
  "ESC[44m then ESC[2J fills the display with the current background (bg=4)."
  (with-screen (s 6 3)
    (feed s (esc "[44m"))          ; SGR 44 → background colour 4
    (feed s (esc "[2J"))           ; ED 2 → erase whole display
    (is (= 4 (bg-at s 0 0)) "erased cell must carry the current bg (4)")
    (is (= 4 (bg-at s 5 2)) "every erased cell carries the bg")
    (is (char= #\Space (char-at s 0 0)) "erased cell is blank")))

(test el-clears-to-current-background
  "ESC[41m then ESC[K erases to end of line with the current background (bg=1)."
  (with-screen (s 6 3)
    (feed s (esc "[41m"))          ; background colour 1
    (feed s (esc "[K"))            ; EL 0 → cursor to end of line
    (is (= 1 (bg-at s 0 0)) "EL must erase to the current bg")))

(test erase-without-background-is-default
  "With no background set, erasing leaves default bg (0) — BCE is a no-op then."
  (with-screen (s 6 3)
    (feed s "abc")
    (feed s (esc "[2J"))
    (is (= 0 (bg-at s 0 0)) "default bg (0) when no SGR background was set")))

(test bce-resets-foreground-and-attrs
  "A BCE-erased cell carries only the background; fg and attrs reset to default."
  (with-screen (s 6 3)
    (feed s (esc "[1;31;44m"))     ; bold, fg red, bg blue
    (feed s (esc "[2J"))
    (is (= 4 (bg-at s 0 0))   "bg carries over")
    (is (= 7 (fg-at s 0 0))   "fg resets to default (7)")
    (is (= 0 (attrs-at s 0 0)) "attrs reset to 0 (no bold on erased cell)")))
