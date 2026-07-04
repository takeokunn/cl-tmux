(in-package #:cl-tmux/test)

;;;; parser tests — part B: combining-chars, ACS line-drawing, DCS passthrough,
;;;; XTGETTCAP, and DECRQSS helpers.

;;; ── SUITE: combining-chars ───────────────────────────────────────────────────

(def-suite combining-chars
  :description "Unicode combining character handling in the emulator"
  :in terminal-suite)
(in-suite combining-chars)

(test combining-char-predicate-ranges
  "combining-char-p is T for code points in combining ranges, NIL otherwise."
  (is (cl-tmux/terminal/actions:combining-char-p (code-char #x0300))
      "U+0300 (combining grave) must be detected as combining")
  (is (cl-tmux/terminal/actions:combining-char-p (code-char #x036F))
      "U+036F (last combining diacritical) must be detected")
  (is-false (cl-tmux/terminal/actions:combining-char-p #\a)
            "ASCII 'a' must NOT be combining")
  (is-false (cl-tmux/terminal/actions:combining-char-p #\Space)
            "Space must NOT be combining"))

(test combining-char-appended-to-cell
  "A combining character appended after a base char is stored in the previous
   cell's combining list without advancing the cursor."
  (when (< #x0301 char-code-limit)   ; U+0301 = combining acute accent
    (with-screen (s 20 5)
      (feed s "e")                   ; base character 'e' at (0,0)
      (check-cursor s 1 0)
      ;; Feed U+0301 (combining acute) as UTF-8: C3 B4 → no, U+0301 = 0xCC 0x81
      (screen-process-bytes s (make-array 2 :element-type '(unsigned-byte 8)
                                            :initial-contents '(#xCC #x81)))
      ;; Cursor should NOT have advanced
      (check-cursor s 1 0)
      ;; The combining char must be in the previous cell's combining list
      (let ((cell (screen-cell s 0 0)))
        (is (member (code-char #x0301) (cl-tmux/terminal/types:cell-combining cell))
            "U+0301 must be in the cell's combining list")))))

;;; ── SUITE: acs-line-drawing ──────────────────────────────────────────────────

(def-suite acs-line-drawing
  :description "ACS / DEC special graphics character set switching"
  :in terminal-suite)
(in-suite acs-line-drawing)

(test acs-charset-switch
  "ESC ( 0 switches to DEC graphics; ESC ( B switches back to ASCII."
  (with-screen (s 20 5)
    (is (eq :ascii (cl-tmux/terminal/types:screen-charset s))
        "charset must default to :ascii")
    ;; ESC ( 0
    (feed s (format nil "~C(0" #\Escape))
    (is (eq :dec-graphics (cl-tmux/terminal/types:screen-charset s))
        "charset must be :dec-graphics after ESC ( 0")
    ;; ESC ( B
    (feed s (format nil "~C(B" #\Escape))
    (is (eq :ascii (cl-tmux/terminal/types:screen-charset s))
        "charset must return to :ascii after ESC ( B")))

(test acs-line-drawing-maps-chars-table
  "In DEC graphics mode, each ASCII char maps to the correct box-drawing Unicode codepoint."
  (dolist (row '(("q" #\─ "DEC graphics 'q' → ─ (U+2500)")
                 ("x" #\│ "DEC graphics 'x' → │ (U+2502)")))
    (destructuring-bind (input expected desc) row
      (with-screen (s 20 5)
        (feed s (format nil "~C(0" #\Escape))
        (feed s input)
        (is (char= expected (char-at s 0 0)) "~A" desc)))))

(test acs-ascii-mode-unaffected
  "In ASCII mode (default), 'q' writes literal 'q'."
  (with-screen (s 20 5)
    ;; Ensure we are in ASCII mode
    (feed s (format nil "~C(B" #\Escape))
    (feed s "q")
    (is (char= #\q (char-at s 0 0))
        "ASCII mode: 'q' must write literal 'q', not a box-drawing char")))

;;; ── Coverage: %dec-graphics-char corner cases ────────────────────────────────
;;;
;;; The existing tests cover only 'q' and 'x'.  These tests add coverage for
;;; the corner characters, junctions, the catch-all unmapped branch, and the
;;; macro itself (define-dec-graphics-table).

(defmacro check-dec-graphics (char expected-char description)
  "Assert that %dec-graphics-char maps CHAR to EXPECTED-CHAR with DESCRIPTION."
  `(is (char= ,expected-char (cl-tmux/terminal/actions::%dec-graphics-char ,char))
       ,description))

(test dec-graphics-corner-characters
  "DEC graphics corner and junction characters map to the correct box-drawing codepoints."
  (check-dec-graphics #\j #\┘ "j must map to lower-right corner (┘)")
  (check-dec-graphics #\k #\┐ "k must map to upper-right corner (┐)")
  (check-dec-graphics #\l #\┌ "l must map to upper-left corner (┌)")
  (check-dec-graphics #\m #\└ "m must map to lower-left corner (└)")
  (check-dec-graphics #\n #\┼ "n must map to crossing (┼)")
  (check-dec-graphics #\t #\├ "t must map to left tee (├)")
  (check-dec-graphics #\u #\┤ "u must map to right tee (┤)")
  (check-dec-graphics #\v #\┴ "v must map to bottom tee (┴)")
  (check-dec-graphics #\w #\┬ "w must map to top tee (┬)"))

(test dec-graphics-special-characters
  "DEC graphics special characters map to the correct Unicode codepoints."
  (check-dec-graphics #\a #\▒ "a must map to checkerboard (▒)")
  (check-dec-graphics #\` #\◆ "` must map to diamond (◆)")
  (check-dec-graphics #\f #\° "f must map to degree symbol (°)")
  (check-dec-graphics #\g #\± "g must map to plus-minus (±)"))

(test dec-graphics-scan-lines
  "DEC graphics horizontal scan lines (o,p,q,r,s) map to their distinct vertical
   positions: q (scan line 5) is the box-drawing horizontal; o/p sit above, r/s below."
  (check-dec-graphics #\o #\⎺ "o must map to scan line 1 (top)")
  (check-dec-graphics #\p #\⎻ "p must map to scan line 3")
  (check-dec-graphics #\q #\─ "q must map to scan line 5 (horizontal line)")
  (check-dec-graphics #\r #\⎼ "r must map to scan line 7")
  (check-dec-graphics #\s #\⎽ "s must map to scan line 9 (bottom)"))

(test dec-graphics-math-and-symbol-characters
  "Upper half of the DEC special-graphics set: relational/math symbols + others
   (previously passed through literally, breaking apps that emit them)."
  (check-dec-graphics #\y #\≤ "y must map to less-than-or-equal (≤)")
  (check-dec-graphics #\z #\≥ "z must map to greater-than-or-equal (≥)")
  (check-dec-graphics #\{ #\π "{ must map to pi (π)")
  (check-dec-graphics #\| #\≠ "| must map to not-equal (≠)")
  (check-dec-graphics #\} #\£ "} must map to UK pound sign (£)")
  (check-dec-graphics #\~ #\· "~ must map to centred dot (·)")
  (check-dec-graphics #\_ #\Space "_ must map to a blank"))

(test dec-graphics-unmapped-char-returned-unchanged
  "An unmapped character (not in the DEC special graphics set) is returned as-is.
   Digits and uppercase letters are NOT part of the set, so they pass through."
  (check-dec-graphics #\5 #\5 "unmapped '5' must be returned unchanged")
  (check-dec-graphics #\A #\A "unmapped 'A' must be returned unchanged"))

(test dec-graphics-via-emulator-corner-chars
  "Writing corner characters through the emulator in DEC graphics mode places
   the correct box-drawing characters on the screen."
  (with-screen (s 20 5)
    (feed s (format nil "~C(0" #\Escape))  ; switch to DEC graphics
    (feed s "jklm")
    (is (char= #\┘ (char-at s 0 0)) "j at col 0 must be ┘")
    (is (char= #\┐ (char-at s 1 0)) "k at col 1 must be ┐")
    (is (char= #\┌ (char-at s 2 0)) "l at col 2 must be ┌")
    (is (char= #\└ (char-at s 3 0)) "m at col 3 must be └")))

(test define-dec-graphics-table-macro-is-defined
  "define-dec-graphics-table is a defined macro in the actions package."
  (is (macro-function 'cl-tmux/terminal/actions::define-dec-graphics-table)))

;;; ── SUITE: dcs-parsing ───────────────────────────────────────────────────────

(def-suite dcs-parsing
  :description "DCS (Device Control String) sequence pass-through"
  :in terminal-suite)
(in-suite dcs-parsing)

(test dcs-consumed-silently
  "ESC P ... ESC \\ DCS sequence is consumed without crashing or corrupting output."
  (with-screen (s 20 5)
    (feed s "a")
    ;; ESC P (DCS) ... payload "1$p" ... ESC \ (ST)
    ;; ESC P = #x1B #x50, payload "1$p", ESC \ = #x1B #x5C
    (screen-process-bytes s
      (make-array 7 :element-type '(unsigned-byte 8)
                    :initial-contents (list #x1B #x50
                                           (char-code #\1)
                                           (char-code #\$)
                                           (char-code #\p)
                                           #x1B #x5C)))
    (feed s "b")
    (is (char= #\a (char-at s 0 0)) "char before DCS must be unaffected")
    (is (char= #\b (char-at s 1 0)) "char after DCS must appear at column 1")))

(test dcs-parser-returns-ground-state-after-st
  "After an ESC P ... ESC \\ sequence, the parser is back in ground state."
  (with-screen (s 20 5)
    ;; Feed a DCS then a printable char
    ;; ESC P = #x1B #x50, payload "Hello", ESC \ = #x1B #x5C
    (screen-process-bytes s
      (make-array 9 :element-type '(unsigned-byte 8)
                    :initial-contents (list #x1B #x50
                                           (char-code #\H) (char-code #\e)
                                           (char-code #\l) (char-code #\l)
                                           (char-code #\o)
                                           #x1B #x5C)))
    ;; If parser ended up in ground state, feeding printable bytes works.
    (feed s "X")
    ;; X must land at column 0 (nothing was written by the DCS body)
    (is (char= #\X (char-at s 0 0))
        "printable after DCS-ST must be placed at column 0")))

;;; ── XTGETTCAP (DCS + q <hex caps> ST) ────────────────────────────────────────

(defun %feed-dcs (s payload)
  "Feed a DCS sequence (ESC P PAYLOAD ST) to screen S via screen-process-bytes."
  (screen-process-bytes s
    (babel:string-to-octets (format nil "~CP~A~C\\" #\Escape payload #\Escape)
                            :encoding :utf-8)))

(test hex-decode-encode-roundtrip
  "%hex-decode-string / %hex-encode-string convert XTGETTCAP hex cap names."
  (flet ((decode (s) (cl-tmux/terminal/parser::%hex-decode-string s))
         (encode (s) (cl-tmux/terminal/parser::%hex-encode-string s)))
    (dolist (c `(("Tc"   ,(lambda () (decode "5463"))   "5463 -> Tc")
                 ("5463" ,(lambda () (encode "Tc"))     "Tc -> 5463")
                 ("256"  ,(lambda () (decode "323536")) "323536 -> 256")
                 (nil    ,(lambda () (decode "5"))      "odd-length -> NIL")))
      (destructuring-bind (expected fn desc) c
        (is (equal expected (funcall fn)) "~A" desc)))))

(test xtgettcap-responses-table
  "XTGETTCAP replies DCS 1+r for known caps (Tc, RGB, colors) and DCS 0+r for unknown caps."
  (dolist (row (list (list "+q5463"         (format nil "~CP1+r5463~C\\"          #\Escape #\Escape) "Tc → DCS 1+r 5463")
                     (list "+q524742"        (format nil "~CP1+r524742~C\\"        #\Escape #\Escape) "RGB → DCS 1+r 524742")
                     (list "+q636f6c6f7273"  (format nil "~CP1+r636f6c6f7273=323536~C\\" #\Escape #\Escape) "colors → DCS 1+r with =323536")
                     (list "+q5878"          (format nil "~CP0+r5878~C\\"          #\Escape #\Escape) "unknown cap → DCS 0+r")))
    (destructuring-bind (dcs-input expected desc) row
      (with-screen (s 20 5)
        (%feed-dcs s dcs-input)
        (is (string= expected (first (cl-tmux/terminal/types:screen-response-queue s)))
            "~A" desc)))))

;;; ── DECRQSS (DCS $ q <setting> ST) ───────────────────────────────────────────

(test decrqss-sgr-reports-current-pen
  "DECRQSS $q m reports the current SGR pen: ESC P 1 $ r <params> m ST."
  (with-screen (s 20 5)
    (feed s (esc "[1;31m"))        ; bold red pen
    (%feed-dcs s "$qm")
    (is (string= (format nil "~CP1$r0;1;31m~C\\" #\Escape #\Escape)
                 (first (cl-tmux/terminal/types:screen-response-queue s)))
        "DECRQSS m must report 0;1;31 for a bold-red pen")))

(test decrqss-scroll-region-reports-margins
  "DECRQSS $q r reports the scroll region (1-based): ESC P 1 $ r top;bottom r ST."
  (with-screen (s 20 5)
    (%feed-dcs s "$qr")
    (is (string= (format nil "~CP1$r1;5r~C\\" #\Escape #\Escape)
                 (first (cl-tmux/terminal/types:screen-response-queue s)))
        "DECRQSS r must report 1;5 for a full 5-row screen")))

(test decrqss-cursor-style-reports-shape
  "DECRQSS $q SP q reports the DECSCUSR cursor shape."
  (with-screen (s 20 5)
    (feed s (esc "[3 q"))          ; DECSCUSR shape 3
    (%feed-dcs s "$q q")
    (is (string= (format nil "~CP1$r3 q~C\\" #\Escape #\Escape)
                 (first (cl-tmux/terminal/types:screen-response-queue s)))
        "DECRQSS SP q must report shape 3")))

(test decrqss-unknown-reports-invalid
  "DECRQSS for an unsupported setting replies ESC P 0 $ r ST (invalid)."
  (with-screen (s 20 5)
    (%feed-dcs s "$qx")
    (is (string= (format nil "~CP0$r~C\\" #\Escape #\Escape)
                 (first (cl-tmux/terminal/types:screen-response-queue s)))
        "an unsupported DECRQSS request must reply 0$r")))
