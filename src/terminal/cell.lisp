(in-package #:cl-tmux/terminal/types)

;;;; Immutable cell type and Unicode character-width table.
;;;;
;;;; This file contains only pure, stateless definitions:
;;;;   - attribute bit constants
;;;;   - named constants for cross-file magic values and terminal geometry
;;;;   - the CELL defstruct and BLANK-CELL constructor
;;;;   - CLAMP and SAFE-CODE-CHAR utilities
;;;;   - the DEFINE-WIDE-CHAR-RANGES macro and its invocation

;;; ── Attribute bit constants ────────────────────────────────────────────────
;;;
;;; Prolog-like fact table — each constant is one named bit in the attrs byte.
;;; Bit layout (LSB first): bold dim reverse underline blink italic conceal
;;; strikethrough (bits 0-7).  Double-underline and overline are stored in
;;; a separate 16-bit word in the cell struct (see below).

(defconstant +attr-bold+          #b00000001)
(defconstant +attr-dim+           #b00000010)
(defconstant +attr-reverse+       #b00000100)
(defconstant +attr-underline+     #b00001000)
(defconstant +attr-blink+         #b00010000)
(defconstant +attr-italic+        #b00100000)
(defconstant +attr-conceal+       #b01000000)
(defconstant +attr-strikethrough+ #b10000000)

;;; Extended attribute bits stored in the cell's attrs2 slot (16-bit).
;;; These are less common and placed in a second word to keep attrs as (unsigned-byte 8).
(defconstant +attr2-double-underline+ #b00000001)  ; SGR 21
(defconstant +attr2-overline+         #b00000010)  ; SGR 53

;;; ── Named constants for cross-file magic values ────────────────────────────
;;;
;;; These are the single source of truth for values used across multiple files.
;;; Consumers (sgr.lisp, parser.lisp) reference these symbols rather than
;;; repeating the numeric literals.

(defconstant +true-color-flag+ #x1000000
  "Bit 24 of a colour slot: when set, bits 23-16 are R, 15-8 are G, 7-0 are B.
   Values 0-255 are palette indices; values >= +true-color-flag+ are true-colour RGB.")

(defconstant +default-color+ 256
  "Sentinel colour value meaning \"the terminal default\" (SGR 39 fg / SGR 49 bg).
   Mirrors tmux's grid_cell fg/bg == 8 (COLOUR_DEFAULT), but placed at 256 — just
   above the 0-255 palette and without the +true-color-flag+ bit — so it is
   distinct from palette index 7 (white) and 0 (black).  Cells carrying this value
   are the only ones window-style / window-active-style may recolour.")

(defconstant +unicode-replacement-char+ #xFFFD
  "Unicode code point U+FFFD REPLACEMENT CHARACTER.
   Used as a fallback for invalid or unrepresentable code points.")

;;; ── Default terminal geometry ──────────────────────────────────────────────
;;;
;;; These are the canonical VT100 / xterm default dimensions used as initforms
;;; in the screen defstruct and as cross-file reference values.

(defconstant +default-screen-width+  80
  "Default virtual terminal width in columns (VT100 standard).")

(defconstant +default-screen-height+ 24
  "Default virtual terminal height in rows (VT100 standard).")

(defconstant +title-stack-max-depth+ 8
  "Maximum depth of the XTPUSHTITLE / XTPOPTITLE title stack (matches xterm).")

;;; ── Cell ───────────────────────────────────────────────────────────────────

(defstruct cell
  "One character position on the virtual screen.

   WIDTH encodes East-Asian double-width handling:
     1 — normal single-column cell
     2 — lead cell of a double-width character
     0 — continuation placeholder occupied by the wide char to its left

   Color encoding (fg, bg, ul-color):
     0-255            — palette index (0-7 standard, 8-15 bright, 16-255 extended)
     >= +true-color-flag+ — true-colour RGB: bits 23-16 R, 15-8 G, 7-0 B"
  (char  #\Space :type character)
  (fg    +default-color+ :type (unsigned-byte 25))  ; see color encoding; +default-color+ = terminal default fg (SGR 39)
  (bg    +default-color+ :type (unsigned-byte 25))  ; see color encoding; +default-color+ = terminal default bg (SGR 49)
  (attrs 0       :type (unsigned-byte 8))   ; bit-field: see +attr-* constants
  ;; Extended attributes: double-underline (bit 0), overline (bit 1)
  (attrs2 0      :type (unsigned-byte 8))
  ;; Underline color (SGR 58): same encoding as fg/bg. 0 = default (use fg).
  (ul-color 0   :type (unsigned-byte 25))
  ;; Combining characters appended after the base char (zero-width marks).
  ;; NIL when no combining chars are present; a list of characters otherwise.
  (combining nil :type list)
  ;; OSC 8 hyperlink URI active when this cell was written, or NIL.  The renderer
  ;; re-emits OSC 8 around runs of cells sharing a hyperlink so the outer terminal
  ;; makes them clickable (transparency for ls --hyperlink, gcc, pagers, ...).
  (hyperlink nil :type (or null string))
  (width 1       :type (integer 0 2)))      ; 1 normal, 2 wide lead, 0 continuation

(defun blank-cell ()
  "Return a fresh default (space, default fg/bg sentinel, no attrs, single-width) cell."
  (make-cell))

(declaim (inline clamp))
(defun clamp (v lo hi)
  "Clamp integer V to the closed interval [LO, HI]."
  (max lo (min hi v)))

(defun safe-code-char (cp)
  "CODE-CHAR guarded against invalid code points; falls back to U+FFFD."
  (or (and (< cp char-code-limit) (code-char cp))
      (code-char +unicode-replacement-char+)))

(defmacro define-wide-char-ranges (&rest ranges)
  "Generate CHAR-WIDTH from a declarative Unicode wide-char range table.
   Each RANGE is (lo hi description) where lo and hi are code-point integers
   and description is a string annotation (compile-time only)."
  `(defun char-width (ch)
     "Display column width of CH: 2 for East-Asian Wide / Fullwidth characters
      (CJK, kana, hangul, fullwidth forms, most emoji), 1 otherwise.
      Ambiguous-width ranges (box drawing) are treated as 1."
     (let ((cp (char-code ch)))
       (if (or ,@(mapcar (lambda (range) `(<= ,(first range) cp ,(second range)))
                         ranges))
           2 1))))

(define-wide-char-ranges
  (#x1100  #x115F  "Hangul Jamo")
  (#x2E80  #x303E  "CJK radicals, Kangxi, CJK symbols")
  (#x3041  #x33FF  "Hiragana, Katakana, CJK compat")
  (#x3400  #x4DBF  "CJK Extension A")
  (#x4E00  #x9FFF  "CJK Unified Ideographs")
  (#xA000  #xA4CF  "Yi syllables")
  (#xAC00  #xD7A3  "Hangul syllables")
  (#xF900  #xFAFF  "CJK Compatibility Ideographs")
  (#xFE30  #xFE4F  "CJK Compatibility Forms")
  (#xFF00  #xFF60  "Fullwidth ASCII forms")
  (#xFFE0  #xFFE6  "Fullwidth signs")
  (#x1F300 #x1FAFF "Emoji and pictographs")
  (#x20000 #x3FFFD "CJK Extension B and beyond"))
