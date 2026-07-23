(in-package #:cl-tmux/test)

;;;; Cell tests (src/terminal/cell.lisp).
;;;; Tests: attribute constants, cell struct, blank-cell, clamp,
;;;;        safe-code-char, char-width classification, combining chars,
;;;;        and wide-char layout helpers.
;;;; Display-facing charset, BCE, constants, and hyperlink tests live in
;;;; cell-display-tests.lisp.

;;; ── SUITE: attribute bit constants ───────────────────────────────────────────

(describe "terminal-suite/attr-constants"

  ;; Each attribute constant occupies exactly the declared bit position in its byte.
  (it "attr-bit-values-table"
    (check-table (list (list #b00000001 cl-tmux/terminal/types:+attr-bold+              "bold is bit 0")
                       (list #b00000010 cl-tmux/terminal/types:+attr-dim+               "dim is bit 1")
                       (list #b00000100 cl-tmux/terminal/types:+attr-reverse+           "reverse is bit 2")
                       (list #b00001000 cl-tmux/terminal/types:+attr-underline+         "underline is bit 3")
                       (list #b00010000 cl-tmux/terminal/types:+attr-blink+             "blink is bit 4")
                       (list #b00100000 cl-tmux/terminal/types:+attr-italic+            "italic is bit 5")
                       (list #b01000000 cl-tmux/terminal/types:+attr-conceal+           "conceal is bit 6")
                       (list #b10000000 cl-tmux/terminal/types:+attr-strikethrough+     "strikethrough is bit 7")
                       (list #b00000001 cl-tmux/terminal/types:+attr2-double-underline+ "double-underline is attrs2 bit 0")
                       (list #b00000010 cl-tmux/terminal/types:+attr2-overline+         "overline is attrs2 bit 1"))
                 :test #'equal))

  ;; All eight primary attribute constants are distinct powers of 2.
  (it "attr-constants-are-distinct-single-bits"
    (let ((constants (list cl-tmux/terminal/types:+attr-bold+
                           cl-tmux/terminal/types:+attr-dim+
                           cl-tmux/terminal/types:+attr-reverse+
                           cl-tmux/terminal/types:+attr-underline+
                           cl-tmux/terminal/types:+attr-blink+
                           cl-tmux/terminal/types:+attr-italic+
                           cl-tmux/terminal/types:+attr-conceal+
                           cl-tmux/terminal/types:+attr-strikethrough+)))
      (expect (= 8 (length (remove-duplicates constants))))
      (expect (every #'(lambda (c) (= 1 (logcount c))) constants)))))

;;; ── SUITE: cell struct ───────────────────────────────────────────────────────

(describe "terminal-suite/cell-struct"

  ;; make-cell with no arguments returns a space/default-color/no-attrs cell.
  (it "make-cell-default-slots"
    (let ((c (cl-tmux/terminal/types:make-cell)))
      (expect (char= #\Space (cell-char c)))
      (check-table (list (list (cell-fg   c)                             cl-tmux/terminal/types:+default-color+ "default fg must be the default-colour sentinel")
                         (list (cell-bg   c)                             cl-tmux/terminal/types:+default-color+ "default bg must be the default-colour sentinel")
                         (list (cell-attrs c)                            0 "default attrs must be 0")
                         (list (cl-tmux/terminal/types:cell-attrs2    c) 0 "default attrs2 must be 0")
                         (list (cl-tmux/terminal/types:cell-ul-color  c) 0 "default ul-color must be 0")
                         (list (cell-width c)                            1 "default width must be 1"))
                   :test #'equal)
      (expect (null (cl-tmux/terminal/types:cell-combining c)))))

  ;; make-cell with explicit keyword arguments stores all supplied values.
  (it "make-cell-custom-slots"
    (let ((c (cl-tmux/terminal/types:make-cell :char #\A :fg 2 :bg 5 :attrs 3
                                                :attrs2 1 :ul-color 4 :width 2)))
      (expect (char= #\A (cell-char c)))
      (check-table (list (list (cell-fg   c)                            2 "fg")
                         (list (cell-bg   c)                            5 "bg")
                         (list (cell-attrs c)                           3 "attrs")
                         (list (cl-tmux/terminal/types:cell-attrs2   c) 1 "attrs2")
                         (list (cl-tmux/terminal/types:cell-ul-color c) 4 "ul-color")
                         (list (cell-width c)                           2 "width"))
                   :test #'equal)))

  ;; make-cell :width 0 produces a valid continuation placeholder.
  (it "make-cell-continuation-width-zero"
    (let ((c (cl-tmux/terminal/types:make-cell :char #\Space :width 0)))
      (expect (= 0 (cell-width c)))))

  ;; cell-p returns T for a struct produced by make-cell.
  (it "cell-p-returns-true-for-cell"
    (let ((c (cl-tmux/terminal/types:make-cell)))
      (expect (cl-tmux/terminal/types:cell-p c) :to-be-truthy)))

  ;; cell-p returns NIL for non-cell objects.
  (it "cell-p-returns-false-for-non-cell"
    (expect (cl-tmux/terminal/types:cell-p 42) :to-be-falsy)
    (expect (cl-tmux/terminal/types:cell-p "hello") :to-be-falsy)
    (expect (cl-tmux/terminal/types:cell-p nil) :to-be-falsy))

  ;; make-cell :combining with a list stores the combining characters.
  (it "make-cell-combining-slot"
    (let ((c (cl-tmux/terminal/types:make-cell
              :combining (list (code-char #x0300) (code-char #x0301)))))
      (expect (= 2 (length (cl-tmux/terminal/types:cell-combining c))))))

  ;; blank-cell returns a space-character cell with default colours and width 1.
  (it "blank-cell-returns-default-cell"
    (let ((c (cl-tmux/terminal/types:blank-cell)))
      (expect (char= #\Space (cell-char c)))
      (check-table (list (list (cell-fg    c) cl-tmux/terminal/types:+default-color+ "blank-cell fg must be the default-colour sentinel")
                         (list (cell-bg    c) cl-tmux/terminal/types:+default-color+ "blank-cell bg must be the default-colour sentinel")
                         (list (cell-attrs c) 0 "blank-cell attrs must be 0")
                         (list (cell-width c) 1 "blank-cell width must be 1"))
                   :test #'equal)))

  ;; Each call to blank-cell returns a structurally equal but distinct object.
  (it "blank-cell-returns-fresh-instance-each-call"
    (let ((c1 (cl-tmux/terminal/types:blank-cell))
          (c2 (cl-tmux/terminal/types:blank-cell)))
      (expect (not (eq c1 c2))))))

;;; ── %make-blank-cells ────────────────────────────────────────────────────────

(describe "terminal-suite/make-blank-cells-suite"

  ;; %make-blank-cells returns a simple-vector of the requested length.
  (it "make-blank-cells-returns-simple-vector-of-correct-length"
    (let ((v (cl-tmux/terminal/types:%make-blank-cells 10)))
      (expect (simple-vector-p v))
      (expect (= 10 (length v)))))

  ;; Every element returned by %make-blank-cells is a default space cell.
  (it "make-blank-cells-all-elements-are-blank"
    (let ((v (cl-tmux/terminal/types:%make-blank-cells 5)))
      (dotimes (i 5)
        (let ((c (aref v i)))
          (expect (cl-tmux/terminal/types:cell-p c))
          (expect (char= #\Space (cell-char c)))
          (expect (= cl-tmux/terminal/types:+default-color+ (cell-fg c)))
          (expect (= cl-tmux/terminal/types:+default-color+ (cell-bg c)))
          (expect (= 1 (cell-width c)))))))

  ;; %make-blank-cells with n=0 returns an empty simple-vector.
  (it "make-blank-cells-zero-length-returns-empty-vector"
    (let ((v (cl-tmux/terminal/types:%make-blank-cells 0)))
      (expect (simple-vector-p v))
      (expect (= 0 (length v))))))

;;; ── clamp ────────────────────────────────────────────────────────────────────

(describe "terminal-suite/clamp-suite"

  ;; Table-driven clamp tests: (v lo hi expected description)
  ;;
  ;; This single table test covers all boundary cases: below lo, above hi,
  ;; at boundaries, within range, and lo=hi degenerate.  The four individual
  ;; named tests that previously existed were fully redundant with this table
  ;; and have been removed to eliminate noise without losing coverage.
  ;;
  ;; clamp correctly handles below, above, at, and within bounds.
  (it "clamp-table"
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
        (declare (ignore desc))
        (expect (= expected (cl-tmux/terminal/types:clamp v lo hi)))))))

;;; ── safe-code-char ───────────────────────────────────────────────────────────

(describe "terminal-suite/safe-code-char-suite"

  ;; safe-code-char returns the character for a valid code point.
  (it "safe-code-char-valid-codepoint"
    (expect (char= #\A (cl-tmux/terminal/types:safe-code-char 65)))
    (expect (char= #\a (cl-tmux/terminal/types:safe-code-char 97))))

  ;; safe-code-char with code point 0 returns the NUL character.
  (it "safe-code-char-zero-returns-null-char"
    (expect (= 0 (char-code (cl-tmux/terminal/types:safe-code-char 0)))))

  ;; safe-code-char returns U+FFFD for a code point outside char-code-limit.
  (it "safe-code-char-invalid-codepoint-returns-replacement"
    ;; char-code-limit is implementation-defined but always > 0x110000 on SBCL.
    ;; Use a known-bad value well above Unicode range.
    (let ((replacement (cl-tmux/terminal/types:safe-code-char
                        (+ char-code-limit 1))))
      (expect (= #xFFFD (char-code replacement)))))

  ;; Table-driven safe-code-char tests: (cp expected-char)
  ;;
  ;; safe-code-char table: well-known code-points map to expected characters.
  (it "safe-code-char-table"
    (check-table (list (list (cl-tmux/terminal/types:safe-code-char 65) #\A    "U+0041 = LATIN CAPITAL LETTER A")
                       (list (cl-tmux/terminal/types:safe-code-char 97) #\a    "U+0061 = LATIN SMALL LETTER A")
                       (list (cl-tmux/terminal/types:safe-code-char 32) #\Space "U+0020 = SPACE")
                       (list (cl-tmux/terminal/types:safe-code-char 10) #\Newline "U+000A = LINE FEED"))
                 :test #'equal)))

;;; ── SUITE: char-width / double-width ─────────────────────────────────────────

(describe "terminal-suite/double-width"

  ;; char-width returns 2 for wide CJK/kana and 1 for ASCII and box drawing.
  (it "char-width-classification"
    (check-table (list (list (char-width #\a)     1 "ASCII a is single-width")
                       (list (char-width #\Space) 1 "Space is single-width")
                       (list (char-width #\あ)    2 "Hiragana is double-width")
                       (list (char-width #\中)    2 "CJK ideograph is double-width")
                       (list (char-width #\│)     1 "box drawing stays single-width"))
                 :test #'equal))

  ;; All printable ASCII characters have display width 1.
  (it "char-width-ascii-range-is-single"
    (loop for cp from 32 to 126
          do (expect (= 1 (char-width (code-char cp))))))

  ;; define-wide-char-ranges is a defined macro accessible via single-colon export.
  (it "define-wide-char-ranges-macro-is-defined"
    (expect (macro-function 'cl-tmux/terminal/types:define-wide-char-ranges)))

  ;; Table-driven char-width boundary tests: (cp expected-width description)
  ;;
  ;; char-width returns the correct width at all range boundaries.
  (it "char-width-range-boundaries-table"
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
        (declare (ignore desc))
        (when (< cp char-code-limit)
          (expect (= expected-width (char-width (code-char cp))))))))

  ;; ── wide-char layout ─────────────────────────────────────────────────────

  ;; A wide char fills a lead cell + continuation cell and advances the cursor 2.
  (it "wide-char-occupies-two-columns"
    (with-screen (s 10 2)
      (utf8-feed s "あ")
      (expect (char= #\あ (char-at s 0 0)))
      (expect (= 2 (cell-width (cell-at s 0 0))))
      (expect (= 0 (cell-width (cell-at s 1 0))))
      (check-cursor s 2 0)))

  ;; A wide char that cannot fit in the last column wraps to the next row.
  (it "wide-char-wraps-at-right-edge"
    (with-screen (s 3 2)
      (feed s "ab")            ; cursor at column 2 (last column of a 3-wide screen)
      (utf8-feed s "あ")       ; cannot fit one column -> wraps to row 1
      (expect (char= #\a  (char-at s 0 0)))
      (expect (char= #\b  (char-at s 1 0)))
      (expect (char= #\Space (char-at s 2 0)))
      (expect (char= #\あ (char-at s 0 1)))
      (check-cursor s 2 1))))

;;; ── SUITE: combining characters ──────────────────────────────────────────────

(describe "terminal-suite/cell-combining-chars"

  ;; Code points in the Combining Diacritical Marks block are combining.
  (it "combining-char-p-diacritic-marks-return-true"
    ;; U+0300 COMBINING GRAVE ACCENT (first in the block)
    (expect (cl-tmux/terminal/actions:combining-char-p (code-char #x0300)) :to-be-truthy)
    ;; U+036F last in Combining Diacritical Marks
    (expect (cl-tmux/terminal/actions:combining-char-p (code-char #x036F)) :to-be-truthy))

  ;; Ordinary ASCII characters are not combining.
  (it "combining-char-p-ascii-returns-false"
    (expect (cl-tmux/terminal/actions:combining-char-p #\a) :to-be-falsy)
    (expect (cl-tmux/terminal/actions:combining-char-p #\Space) :to-be-falsy))

  ;; Combining Half Marks (U+FE20-FE2F) are combining.
  (it "combining-char-p-half-marks-return-true"
    (expect (cl-tmux/terminal/actions:combining-char-p (code-char #xFE20)) :to-be-truthy))

  ;; Writing a combining char appends it to the previous cell; cursor does not move.
  (it "write-char-at-cursor-combining-char-appended-not-advanced"
    (with-screen (s 10 5)
      (feed s "a")                        ; base character at col 0; cursor now at col 1
      ;; Write a combining grave accent (U+0300)
      (cl-tmux/terminal/actions:write-char-at-cursor s (code-char #x0300))
      ;; Cursor must still be at col 1 (not advanced)
      (check-cursor s 1 0)
      ;; The combining list of cell (0,0) must contain the diacritic
      (let ((combining (cl-tmux/terminal/types:cell-combining (cell-at s 0 0))))
        (expect (member (code-char #x0300) combining)))))

  ;; A combining char at column 0 is appended to cell (0,0) -- no underflow.
  (it "write-char-at-cursor-combining-at-col-zero-appended-to-col-zero"
    (with-screen (s 10 5)
      ;; cursor starts at col 0; write a combining char without first writing a base
      (cl-tmux/terminal/actions:write-char-at-cursor s (code-char #x0301))
      ;; cursor must remain at col 0
      (check-cursor s 0 0)
      ;; No error and combining list on (0,0) must contain the mark
      (expect (member (code-char #x0301) (cl-tmux/terminal/types:cell-combining (cell-at s 0 0)))))))
