(in-package #:cl-tmux/test)

;;;; Cell display/charset tests (src/domain/terminal/cell.lisp and related screen actions).
;;;; cell-tests.lisp declares terminal-suite and shared low-level fixtures first.

;;; ── SUITE: DEC special graphics character set ────────────────────────────────

(describe "terminal-suite/dec-graphics"

  ;; define-dec-graphics-table macro is fbound.
  (it "dec-graphics-table-macro-is-defined"
    (expect (macro-function 'cl-tmux/terminal/actions::define-dec-graphics-table)))

  ;; With charset :dec-graphics, ASCII j/k/l/m map to box-drawing Unicode.
  (it "dec-graphics-remaps-box-drawing-chars"
    (with-screen (s 10 5)
      (setf (cl-tmux/terminal/types:screen-charset s) :dec-graphics)
      ;; 'j' -> lower-right corner
      (cl-tmux/terminal/actions:write-char-at-cursor s #\j)
      (expect (char= #\┘ (char-at s 0 0)))
      ;; 'q' -> horizontal line
      (cl-tmux/terminal/actions:write-char-at-cursor s #\q)
      (expect (char= #\─ (char-at s 1 0)))
      ;; 'x' -> vertical line
      (cl-tmux/terminal/actions:write-char-at-cursor s #\x)
      (expect (char= #\│ (char-at s 2 0)))))

  ;; A character not in the DEC table passes through unchanged.
  (it "dec-graphics-unmapped-char-passes-through"
    (with-screen (s 10 5)
      (setf (cl-tmux/terminal/types:screen-charset s) :dec-graphics)
      ;; 'A' has no entry in the DEC table -> should write as-is
      (cl-tmux/terminal/actions:write-char-at-cursor s #\A)
      (expect (char= #\A (char-at s 0 0)))))

  ;; %remap-charset-char with charset :ascii returns char unchanged.
  (it "remap-charset-char-ascii-mode-returns-unchanged"
    (with-screen (s 10 5)
      ;; screen-charset defaults to :ascii
      (expect (char= #\j
                     (cl-tmux/terminal/actions::%remap-charset-char s #\j)))))

  ;; %remap-charset-char with charset :dec-graphics remaps j to lower-right corner.
  (it "remap-charset-char-dec-graphics-remaps-j"
    (with-screen (s 10 5)
      (setf (cl-tmux/terminal/types:screen-charset s) :dec-graphics)
      (expect (char= #\┘
                     (cl-tmux/terminal/actions::%remap-charset-char s #\j))))))

;;; ── Background-colour erase (BCE) ────────────────────────────────────────────
;;;
;;; Cells cleared by ED/EL/ECH, the blanks from IL/DL/ICH/DCH, and lines exposed
;;; by scrolling must take the CURRENT SGR background colour, not the default.

(describe "terminal-suite/bce-suite"

  ;; ESC[44m then ESC[2J fills the display with the current background (bg=4).
  (it "ed-clears-to-current-background"
    (with-screen (s 6 3)
      (feed s (esc "[44m"))          ; SGR 44 → background colour 4
      (feed s (esc "[2J"))           ; ED 2 → erase whole display
      (expect (= 4 (bg-at s 0 0)))
      (expect (= 4 (bg-at s 5 2)))
      (expect (char= #\Space (char-at s 0 0)))))

  ;; ESC[41m then ESC[K erases to end of line with the current background (bg=1).
  (it "el-clears-to-current-background"
    (with-screen (s 6 3)
      (feed s (esc "[41m"))          ; background colour 1
      (feed s (esc "[K"))            ; EL 0 → cursor to end of line
      (expect (= 1 (bg-at s 0 0)))))

  ;; With no background set, erasing leaves default bg (0) — BCE is a no-op then.
  (it "erase-without-background-is-default"
    (with-screen (s 6 3)
      (feed s "abc")
      (feed s (esc "[2J"))
      (expect (= cl-tmux/terminal/types:+default-color+ (bg-at s 0 0)))))

  ;; A BCE-erased cell carries only the background; fg and attrs reset to default.
  (it "bce-resets-foreground-and-attrs"
    (with-screen (s 6 3)
      (feed s (esc "[1;31;44m"))     ; bold, fg red, bg blue
      (feed s (esc "[2J"))
      (expect (= 4 (bg-at s 0 0)))
      (expect (= cl-tmux/terminal/types:+default-color+ (fg-at s 0 0)))
      (expect (= 0 (attrs-at s 0 0))))))

;;; ── SUITE: named cross-file constants ────────────────────────────────────────
;;;
;;; Verify the values of constants that are referenced across multiple files.
;;; Any change to these values is a breaking change to color or geometry
;;; handling — tests here document the canonical values.

(describe "terminal-suite/cross-file-constants"

  ;; +true-color-flag+ must equal #x1000000 (bit 24 of a colour slot).
  (it "true-color-flag-is-bit-24"
    (expect (= #x1000000 cl-tmux/terminal/types:+true-color-flag+)))

  ;; +true-color-flag+ must be strictly above palette indices 0..255.
  (it "true-color-flag-does-not-overlap-palette-range"
    (expect (> cl-tmux/terminal/types:+true-color-flag+ 255))
    (expect (> cl-tmux/terminal/types:+true-color-flag+ cl-tmux/terminal/types:+default-color+)))

  ;; +default-color+ must equal 256 (just above the 0-255 palette range).
  (it "default-color-sentinel-is-256"
    (expect (= 256 cl-tmux/terminal/types:+default-color+)))

  ;; +title-stack-max-depth+ must equal 8 (matches xterm).
  (it "title-stack-max-depth-is-8"
    (expect (= 8 cl-tmux/terminal/types:+title-stack-max-depth+)))

  ;; +osc-default-fg+ must equal #xFFFFFF (white on-screen default).
  (it "osc-default-fg-is-white"
    (expect (= #xFFFFFF cl-tmux/terminal/types:+osc-default-fg+)))

  ;; +osc-default-bg+ must equal #x000000 (black on-screen default).
  (it "osc-default-bg-is-black"
    (expect (= #x000000 cl-tmux/terminal/types:+osc-default-bg+)))

  ;; +default-screen-width+ must equal 80 (VT100 standard column count).
  (it "default-screen-width-is-80"
    (expect (= 80 cl-tmux/terminal/types:+default-screen-width+)))

  ;; +default-screen-height+ must equal 24 (VT100 standard row count).
  (it "default-screen-height-is-24"
    (expect (= 24 cl-tmux/terminal/types:+default-screen-height+)))

  ;; Table-driven check: all exported named constants have the expected numeric values.
  (it "constants-table"
    (check-table (list (list cl-tmux/terminal/types:+true-color-flag+          #x1000000 "+true-color-flag+ = #x1000000")
                       (list cl-tmux/terminal/types:+default-color+            256        "+default-color+ = 256")
                       (list cl-tmux/terminal/types:+title-stack-max-depth+    8          "+title-stack-max-depth+ = 8")
                       (list cl-tmux/terminal/types:+osc-default-fg+           #xFFFFFF   "+osc-default-fg+ = #xFFFFFF")
                       (list cl-tmux/terminal/types:+osc-default-bg+           #x000000   "+osc-default-bg+ = #x000000")
                       (list cl-tmux/terminal/types:+default-screen-width+     80         "+default-screen-width+ = 80")
                       (list cl-tmux/terminal/types:+default-screen-height+    24         "+default-screen-height+ = 24")
                       (list cl-tmux/terminal/types:+unicode-replacement-char+ #xFFFD     "+unicode-replacement-char+ = #xFFFD"))
                 :test #'equal))

  ;; +unicode-replacement-char+ must equal #xFFFD (U+FFFD REPLACEMENT CHARACTER).
  (it "unicode-replacement-char-constant-is-fffd"
    (expect (= #xFFFD cl-tmux/terminal/types:+unicode-replacement-char+)))

  ;; safe-code-char falls back to the +unicode-replacement-char+ code point for invalid inputs.
  (it "safe-code-char-uses-replacement-char-for-invalid"
    (let ((result (cl-tmux/terminal/types:safe-code-char (+ char-code-limit 999))))
      (expect (= cl-tmux/terminal/types:+unicode-replacement-char+ (char-code result))))))

;;; ── SUITE: cell-hyperlink slot ───────────────────────────────────────────────

(describe "terminal-suite/cell-hyperlink-suite"

  ;; make-cell with no :hyperlink argument leaves the slot NIL.
  (it "make-cell-hyperlink-defaults-nil"
    (let ((c (cl-tmux/terminal/types:make-cell)))
      (expect (null (cl-tmux/terminal/types:cell-hyperlink c)))))

  ;; make-cell :hyperlink stores the URI string in the hyperlink slot.
  (it "make-cell-hyperlink-can-be-set"
    (let ((c (cl-tmux/terminal/types:make-cell
              :char #\A :hyperlink "https://example.com")))
      (expect (string= "https://example.com"
                       (cl-tmux/terminal/types:cell-hyperlink c)))))

  ;; make-cell :hyperlink "" stores an empty string (distinct from NIL).
  (it "make-cell-hyperlink-empty-string"
    (let ((c (cl-tmux/terminal/types:make-cell :hyperlink "")))
      (expect (stringp (cl-tmux/terminal/types:cell-hyperlink c)))
      (expect (string= "" (cl-tmux/terminal/types:cell-hyperlink c)))))

  ;; blank-cell returns a cell whose hyperlink slot is NIL.
  (it "blank-cell-hyperlink-is-nil"
    (let ((c (cl-tmux/terminal/types:blank-cell)))
      (expect (null (cl-tmux/terminal/types:cell-hyperlink c))))))
