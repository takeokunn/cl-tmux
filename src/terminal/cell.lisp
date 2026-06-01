(in-package #:cl-tmux/terminal/types)

;;;; Immutable cell type and Unicode character-width table.
;;;;
;;;; This file contains only pure, stateless definitions:
;;;;   - attribute bit constants
;;;;   - the CELL defstruct and BLANK-CELL constructor
;;;;   - CLAMP and SAFE-CODE-CHAR utilities
;;;;   - the DEFINE-WIDE-CHAR-RANGES macro and its invocation

;;; ── Attribute bit constants ────────────────────────────────────────────────
;;;
;;; Prolog-like fact table — each constant is one named bit in the attrs byte.
;;; Bit layout (LSB first): bold dim reverse underline blink italic conceal
;;; strikethrough (bits 0-7).

(defconstant +attr-bold+          #b00000001)
(defconstant +attr-dim+           #b00000010)
(defconstant +attr-reverse+       #b00000100)
(defconstant +attr-underline+     #b00001000)
(defconstant +attr-blink+         #b00010000)
(defconstant +attr-italic+        #b00100000)
(defconstant +attr-conceal+       #b01000000)
(defconstant +attr-strikethrough+ #b10000000)

;;; ── Cell ───────────────────────────────────────────────────────────────────

(defstruct cell
  "One character position on the virtual screen.

   WIDTH encodes East-Asian double-width handling:
     1 — normal single-column cell
     2 — lead cell of a double-width character
     0 — continuation placeholder occupied by the wide char to its left"
  (char  #\Space :type character)
  ;; Color encoding: 0-255 = palette (0-7 standard, 8-15 bright, 16-255 extended 256-color);
  ;; bit 24 set (#x1000000+) = true-color: bits 16-23 R, bits 8-15 G, bits 0-7 B.
  ;; Default fg = 7, default bg = 0.
  (fg    7       :type (unsigned-byte 25))
  (bg    0       :type (unsigned-byte 25))
  (attrs 0       :type (unsigned-byte 8))   ; bit-field: see +attr-* constants
  (width 1       :type (integer 0 2)))      ; 1 normal, 2 wide lead, 0 continuation

(defun blank-cell ()
  "Return a fresh default (space, colour 7/0, no attrs, single-width) cell."
  (make-cell))

(declaim (inline clamp))
(defun clamp (v lo hi)
  "Clamp integer V to the closed interval [LO, HI]."
  (max lo (min hi v)))

(defun safe-code-char (cp)
  "CODE-CHAR guarded against invalid code points; falls back to U+FFFD."
  (or (and (< cp char-code-limit) (code-char cp))
      (code-char #xFFFD)))

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
