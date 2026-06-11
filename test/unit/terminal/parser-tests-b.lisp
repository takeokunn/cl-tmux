(in-package #:cl-tmux/test)

;;;; parser tests — part B: combining-chars, ACS line-drawing, DCS passthrough,
;;;; ground-state control bytes, direct DCS/OSC continuations, OSC dispatch,
;;;; OSC-52, OSC-7 cwd, define-osc-rules, make-bytes/feed-osc helpers.

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

(test acs-line-drawing-maps-q-to-horizontal-bar
  "In DEC graphics mode, writing 'q' places the horizontal bar character U+2500 (─)."
  (with-screen (s 20 5)
    ;; Switch to DEC graphics
    (feed s (format nil "~C(0" #\Escape))
    (feed s "q")
    ;; Should have written ─ (U+2500 = horizontal bar)
    (is (char= #\─ (char-at s 0 0))
        "DEC graphics 'q' must map to ─ (U+2500)")))

(test acs-line-drawing-maps-x-to-vertical-bar
  "In DEC graphics mode, writing 'x' places the vertical bar character U+2502 (│)."
  (with-screen (s 20 5)
    (feed s (format nil "~C(0" #\Escape))
    (feed s "x")
    (is (char= #\│ (char-at s 0 0))
        "DEC graphics 'x' must map to │ (U+2502)")))

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
  (is (string= "Tc"   (cl-tmux/terminal/parser::%hex-decode-string "5463")) "5463 → Tc")
  (is (string= "5463" (cl-tmux/terminal/parser::%hex-encode-string "Tc"))   "Tc → 5463")
  (is (string= "256"  (cl-tmux/terminal/parser::%hex-decode-string "323536")) "323536 → 256")
  (is (null (cl-tmux/terminal/parser::%hex-decode-string "5")) "odd-length hex → NIL"))

(test xtgettcap-reports-truecolor-tc
  "XTGETTCAP +q 5463 (Tc) replies DCS 1 + r 5463 ST — true-colour present."
  (with-screen (s 20 5)
    (%feed-dcs s "+q5463")
    (is (string= (format nil "~CP1+r5463~C\\" #\Escape #\Escape)
                 (first (cl-tmux/terminal/types:screen-response-queue s)))
        "Tc must be reported present as DCS 1+r 5463")))

(test xtgettcap-reports-rgb
  "XTGETTCAP +q 524742 (RGB) replies DCS 1 + r 524742 ST."
  (with-screen (s 20 5)
    (%feed-dcs s "+q524742")
    (is (string= (format nil "~CP1+r524742~C\\" #\Escape #\Escape)
                 (first (cl-tmux/terminal/types:screen-response-queue s)))
        "RGB must be reported present")))

(test xtgettcap-reports-colors-256
  "XTGETTCAP +q 636f6c6f7273 (colors) replies DCS 1 + r 636f6c6f7273=323536 ST (256)."
  (with-screen (s 20 5)
    (%feed-dcs s "+q636f6c6f7273")
    (is (string= (format nil "~CP1+r636f6c6f7273=323536~C\\" #\Escape #\Escape)
                 (first (cl-tmux/terminal/types:screen-response-queue s)))
        "colors must be reported as 256 (hex 323536)")))

(test xtgettcap-unknown-cap-reports-failure
  "XTGETTCAP for an unknown cap replies DCS 0 + r <hexname> ST (failure)."
  (with-screen (s 20 5)
    (%feed-dcs s "+q5878")    ; \"Xx\" — not a recognised capability
    (is (string= (format nil "~CP0+r5878~C\\" #\Escape #\Escape)
                 (first (cl-tmux/terminal/types:screen-response-queue s)))
        "an unknown cap must be reported as failure (0+r)")))

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

;;; ── ground-state control-byte coverage ──────────────────────────────────────

(def-suite ground-state-control-bytes
  :description "ground-state handling of DEL, SO, SI, stray continuation bytes, and unhandled C0"
  :in terminal-suite)
(in-suite ground-state-control-bytes)

(test ground-state-del-is-ignored
  "ground-state on DEL (#x7F) does not write a character and returns ground-state."
  (let ((s (make-screen 10 5)))
    (let ((next (cl-tmux/terminal/parser:ground-state s #x7F)))
      (is (eq #'cl-tmux/terminal/parser:ground-state next)
          "ground-state must return ground-state for DEL")
      (is (char= #\Space (char-at s 0 0))
          "DEL must not write a visible character"))))

(test ground-state-so-is-ignored
  "ground-state on SO (#x0E, charset shift-out) returns ground-state without writing."
  (let ((s (make-screen 10 5)))
    (let ((next (cl-tmux/terminal/parser:ground-state s #x0E)))
      (is (eq #'cl-tmux/terminal/parser:ground-state next)
          "SO must return ground-state")
      (is (char= #\Space (char-at s 0 0))
          "SO must not write a visible character"))))

(test ground-state-si-is-ignored
  "ground-state on SI (#x0F, charset shift-in) returns ground-state without writing."
  (let ((s (make-screen 10 5)))
    (let ((next (cl-tmux/terminal/parser:ground-state s #x0F)))
      (is (eq #'cl-tmux/terminal/parser:ground-state next)
          "SI must return ground-state")
      (is (char= #\Space (char-at s 0 0))
          "SI must not write a visible character"))))

(test ground-state-stray-continuation-byte-emits-replacement
  "ground-state on a stray UTF-8 continuation byte (#x80) writes U+FFFD."
  (let ((s (make-screen 10 5)))
    (cl-tmux/terminal/parser:ground-state s #x80)
    (is (char= (code-char #xFFFD) (char-at s 0 0))
        "stray continuation byte must produce U+FFFD replacement character")))

(test ground-state-unhandled-c0-is-ignored
  "ground-state on unhandled C0 bytes (e.g. #x01, #x02) returns ground-state silently."
  (let ((s (make-screen 10 5)))
    (let ((next (cl-tmux/terminal/parser:ground-state s #x01)))
      (is (eq #'cl-tmux/terminal/parser:ground-state next)
          "unhandled C0 (#x01) must return ground-state"))
    (let ((next2 (cl-tmux/terminal/parser:ground-state s #x02)))
      (is (eq #'cl-tmux/terminal/parser:ground-state next2)
          "unhandled C0 (#x02) must return ground-state"))))

(test escape-state-unrecognized-byte-returns-ground
  "escape-state on an unrecognized byte (e.g. #x40 = '@') returns ground-state."
  (let ((s (make-screen 10 5)))
    (let ((next (cl-tmux/terminal/parser:escape-state s #x40)))
      (is (eq #'cl-tmux/terminal/parser:ground-state next)
          "unrecognized ESC byte must return ground-state"))))

(test escape-state-m-reverse-index-returns-ground
  "escape-state on #x4D ('M' = RI / reverse index) moves cursor up and returns ground-state."
  (with-screen (s 10 5)
    (feed s (esc "[3;1H"))    ; move to row 2 (0-based)
    (let ((next (cl-tmux/terminal/parser:escape-state s #x4D)))
      (is (eq #'cl-tmux/terminal/parser:ground-state next)
          "ESC M must return ground-state")
      ;; Cursor should have moved up one row (from 2 to 1).
      (is (= 1 (screen-cursor-y s))
          "ESC M (RI) must move cursor up one row"))))

(test escape-state-7-saves-cursor
  "escape-state on #x37 ('7' = DECSC) saves cursor and returns ground-state."
  (with-screen (s 10 5)
    (feed s (esc "[3;6H"))    ; cursor → (5, 2)
    (let ((next (cl-tmux/terminal/parser:escape-state s #x37)))
      (is (eq #'cl-tmux/terminal/parser:ground-state next)
          "ESC 7 must return ground-state")
      ;; Saved cursor should be non-nil.
      (is (not (null (cl-tmux/terminal/types:screen-saved-cursor s)))
          "ESC 7 must have saved the cursor"))))

(test escape-state-8-restores-cursor
  "escape-state on #x38 ('8' = DECRC) restores cursor and returns ground-state."
  (with-screen (s 10 5)
    (feed s (esc "[3;6H"))    ; cursor → (5, 2)
    (feed s (esc "7"))        ; ESC 7 — save
    (feed s (esc "[1;1H"))    ; move to origin
    (let ((next (cl-tmux/terminal/parser:escape-state s #x38)))
      (is (eq #'cl-tmux/terminal/parser:ground-state next)
          "ESC 8 must return ground-state")
      ;; Cursor should be restored to (5, 2).
      (check-cursor s 5 2))))

(test escape-state-P-dcs-returns-continuation
  "escape-state on #x50 ('P' = DCS introducer) returns a DCS accumulator function."
  (with-screen (s 10 5)
    (let ((next (cl-tmux/terminal/parser:escape-state s #x50)))
      (is (functionp next)
          "ESC P must return a DCS accumulator continuation function"))))

(test escape-state-open-paren-returns-charset-designator
  "escape-state on #x28 ('(' = G0 designator introducer) returns a designator
   continuation that designates G0 to the next byte's charset."
  (with-screen (s 10 5)
    (let ((next (cl-tmux/terminal/parser:escape-state s #x28)))
      (is (functionp next) "ESC ( must return a charset-designator continuation")
      (funcall next s 48)                ; '0' → DEC graphics
      (is (eq :dec-graphics (cl-tmux/terminal/types:screen-g0-charset s))
          "ESC ( 0 must designate G0 to :dec-graphics"))))

(test escape-state-close-bracket-returns-osc-state
  "escape-state on #x5D (']' = OSC introducer) returns osc-state."
  (with-screen (s 10 5)
    (let ((next (cl-tmux/terminal/parser:escape-state s #x5D)))
      (is (eq #'cl-tmux/terminal/parser:osc-state next)
          "ESC ] must return osc-state"))))

;;; ── make-dcs-k direct tests ──────────────────────────────────────────────────

(def-suite direct-dcs-suite
  :description "Direct calls to make-dcs-k DCS accumulator"
  :in terminal-suite)
(in-suite direct-dcs-suite)

(test make-dcs-k-consumes-payload-bytes
  "make-dcs-k continuation consumes non-ESC payload bytes and returns a continuation."
  (let* ((s  (make-screen 10 5))
         (k0 (cl-tmux/terminal/parser::make-dcs-k))
         ;; Feed a non-ESC payload byte
         (k1 (funcall k0 s (char-code #\H))))
    (is (functionp k1)
        "make-dcs-k must return a function after consuming a payload byte")))

(test make-dcs-k-terminates-on-esc-backslash
  "make-dcs-k returns ground-state after receiving ESC (#x1B) then backslash (#x5C)."
  (let* ((s   (make-screen 10 5))
         (k0  (cl-tmux/terminal/parser::make-dcs-k))
         ;; Feed some payload
         (k1  (funcall k0 s (char-code #\X)))
         ;; Feed ESC → waiting for backslash
         (k2  (funcall k1 s #x1B))
         ;; Feed backslash = ST confirmed
         (result (funcall k2 s #x5C)))
    (is (eq #'cl-tmux/terminal/parser:ground-state result)
        "make-dcs-k must return ground-state after ESC+backslash ST")))

(test make-dcs-k-non-backslash-after-esc-continues
  "make-dcs-k after ESC followed by a non-backslash keeps consuming."
  (let* ((s   (make-screen 10 5))
         (k0  (cl-tmux/terminal/parser::make-dcs-k))
         (k1  (funcall k0 s #x1B))     ; ESC → waiting for backslash
         ;; Feed a non-backslash byte — should continue consuming DCS
         (k2  (funcall k1 s (char-code #\A))))
    (is (functionp k2)
        "non-backslash after ESC inside DCS must return a continuation, not ground-state")))

;;; ── make-osc-k / make-osc-st-k direct tests ──────────────────────────────────
