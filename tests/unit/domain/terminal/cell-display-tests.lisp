(in-package #:cl-tmux/test)

;;;; Cell display/charset tests (src/domain/terminal/cell.lisp and related screen actions).
;;;; cell-tests.lisp declares terminal-suite and shared low-level fixtures first.

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
    (is (= cl-tmux/terminal/types:+default-color+ (bg-at s 0 0)) "default bg sentinel when no SGR background was set")))

(test bce-resets-foreground-and-attrs
  "A BCE-erased cell carries only the background; fg and attrs reset to default."
  (with-screen (s 6 3)
    (feed s (esc "[1;31;44m"))     ; bold, fg red, bg blue
    (feed s (esc "[2J"))
    (is (= 4 (bg-at s 0 0))   "bg carries over")
    (is (= cl-tmux/terminal/types:+default-color+ (fg-at s 0 0))   "fg resets to the default sentinel")
    (is (= 0 (attrs-at s 0 0)) "attrs reset to 0 (no bold on erased cell)")))

;;; ── SUITE: named cross-file constants ────────────────────────────────────────
;;;
;;; Verify the values of constants that are referenced across multiple files.
;;; Any change to these values is a breaking change to color or geometry
;;; handling — tests here document the canonical values.

(def-suite cross-file-constants
  :description "Cross-file magic constants: true-color flag, default-color, screen geometry, OSC defaults"
  :in terminal-suite)
(in-suite cross-file-constants)

(test true-color-flag-is-bit-24
  "+true-color-flag+ must equal #x1000000 (bit 24 of a colour slot)."
  (is (= #x1000000 cl-tmux/terminal/types:+true-color-flag+)
      "+true-color-flag+ must be #x1000000 (bit 24)"))

(test true-color-flag-does-not-overlap-palette-range
  "+true-color-flag+ must be strictly above palette indices 0..255."
  (is (> cl-tmux/terminal/types:+true-color-flag+ 255)
      "+true-color-flag+ must be above palette index 255")
  (is (> cl-tmux/terminal/types:+true-color-flag+ cl-tmux/terminal/types:+default-color+)
      "+true-color-flag+ must be above +default-color+ sentinel"))

(test default-color-sentinel-is-256
  "+default-color+ must equal 256 (just above the 0-255 palette range)."
  (is (= 256 cl-tmux/terminal/types:+default-color+)
      "+default-color+ must be 256"))

(test title-stack-max-depth-is-8
  "+title-stack-max-depth+ must equal 8 (matches xterm)."
  (is (= 8 cl-tmux/terminal/types:+title-stack-max-depth+)
      "+title-stack-max-depth+ must be 8"))

(test osc-default-fg-is-white
  "+osc-default-fg+ must equal #xFFFFFF (white on-screen default)."
  (is (= #xFFFFFF cl-tmux/terminal/types:+osc-default-fg+)
      "+osc-default-fg+ must be #xFFFFFF"))

(test osc-default-bg-is-black
  "+osc-default-bg+ must equal #x000000 (black on-screen default)."
  (is (= #x000000 cl-tmux/terminal/types:+osc-default-bg+)
      "+osc-default-bg+ must be #x000000"))

(test default-screen-width-is-80
  "+default-screen-width+ must equal 80 (VT100 standard column count)."
  (is (= 80 cl-tmux/terminal/types:+default-screen-width+)
      "+default-screen-width+ must be 80"))

(test default-screen-height-is-24
  "+default-screen-height+ must equal 24 (VT100 standard row count)."
  (is (= 24 cl-tmux/terminal/types:+default-screen-height+)
      "+default-screen-height+ must be 24"))

(test constants-table
  "Table-driven check: all exported named constants have the expected numeric values."
  (check-table (list (list cl-tmux/terminal/types:+true-color-flag+          #x1000000 "+true-color-flag+ = #x1000000")
                     (list cl-tmux/terminal/types:+default-color+            256        "+default-color+ = 256")
                     (list cl-tmux/terminal/types:+title-stack-max-depth+    8          "+title-stack-max-depth+ = 8")
                     (list cl-tmux/terminal/types:+osc-default-fg+           #xFFFFFF   "+osc-default-fg+ = #xFFFFFF")
                     (list cl-tmux/terminal/types:+osc-default-bg+           #x000000   "+osc-default-bg+ = #x000000")
                     (list cl-tmux/terminal/types:+default-screen-width+     80         "+default-screen-width+ = 80")
                     (list cl-tmux/terminal/types:+default-screen-height+    24         "+default-screen-height+ = 24")
                     (list cl-tmux/terminal/types:+unicode-replacement-char+ #xFFFD     "+unicode-replacement-char+ = #xFFFD"))
               :test #'equal))

(test unicode-replacement-char-constant-is-fffd
  "+unicode-replacement-char+ must equal #xFFFD (U+FFFD REPLACEMENT CHARACTER)."
  (is (= #xFFFD cl-tmux/terminal/types:+unicode-replacement-char+)
      "+unicode-replacement-char+ must be #xFFFD"))

(test safe-code-char-uses-replacement-char-for-invalid
  "safe-code-char falls back to the +unicode-replacement-char+ code point for invalid inputs."
  (let ((result (cl-tmux/terminal/types:safe-code-char (+ char-code-limit 999))))
    (is (= cl-tmux/terminal/types:+unicode-replacement-char+ (char-code result))
        "safe-code-char must return the character at +unicode-replacement-char+ for an out-of-range input")))

;;; ── SUITE: cell-hyperlink slot ───────────────────────────────────────────────

(def-suite cell-hyperlink-suite
  :description "cell-hyperlink slot: default NIL and keyword constructor"
  :in terminal-suite)
(in-suite cell-hyperlink-suite)

(test make-cell-hyperlink-defaults-nil
  "make-cell with no :hyperlink argument leaves the slot NIL."
  (let ((c (cl-tmux/terminal/types:make-cell)))
    (is (null (cl-tmux/terminal/types:cell-hyperlink c))
        "default cell-hyperlink must be NIL")))

(test make-cell-hyperlink-can-be-set
  "make-cell :hyperlink stores the URI string in the hyperlink slot."
  (let ((c (cl-tmux/terminal/types:make-cell
            :char #\A :hyperlink "https://example.com")))
    (is (string= "https://example.com"
                 (cl-tmux/terminal/types:cell-hyperlink c))
        "cell-hyperlink must hold the supplied URI string")))

(test make-cell-hyperlink-empty-string
  "make-cell :hyperlink \"\" stores an empty string (distinct from NIL)."
  (let ((c (cl-tmux/terminal/types:make-cell :hyperlink "")))
    (is (stringp (cl-tmux/terminal/types:cell-hyperlink c))
        "cell-hyperlink must be a string when set to \"\"")
    (is (string= "" (cl-tmux/terminal/types:cell-hyperlink c))
        "cell-hyperlink must equal the empty string")))

(test blank-cell-hyperlink-is-nil
  "blank-cell returns a cell whose hyperlink slot is NIL."
  (let ((c (cl-tmux/terminal/types:blank-cell)))
    (is (null (cl-tmux/terminal/types:cell-hyperlink c))
        "blank-cell hyperlink must be NIL")))
