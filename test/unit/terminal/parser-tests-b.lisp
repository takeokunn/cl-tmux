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

(def-suite direct-osc-continuations
  :description "Direct calls to make-osc-k and make-osc-st-k"
  :in terminal-suite)
(in-suite direct-osc-continuations)

;;; Helper: build an adjustable byte vector pre-filled with STRING.
;;; Eliminates the repeated 3-line buffer-construction pattern.
(defun make-osc-payload-buf (string)
  "Return a fresh adjustable (unsigned-byte 8) buffer pre-filled with the
   bytes of STRING (one byte per character, Latin-1 encoded)."
  (let ((buf (make-array (length string)
                         :element-type '(unsigned-byte 8)
                         :fill-pointer 0
                         :adjustable   t)))
    (loop for ch across string
          do (vector-push-extend (char-code ch) buf))
    buf))

(test make-osc-k-accumulates-and-dispatches-on-bel
  "make-osc-k accumulates payload bytes and dispatches to %dispatch-osc on BEL."
  (with-screen (s 20 5)
    ;; Simulate: OSC 0 ; title (bytes for "0;hello")
    (let ((buf (make-osc-payload-buf "0;hello"))
          (k   nil))
      (setf k (cl-tmux/terminal/parser::make-osc-k buf))
      ;; Feed BEL to terminate
      (let ((result (funcall k s #x07)))
        (is (eq #'cl-tmux/terminal/parser:ground-state result)
            "make-osc-k must return ground-state after BEL")
        (is (string= "hello" (cl-tmux/terminal/types:screen-title s))
            "make-osc-k BEL must dispatch OSC 0 and set screen-title")))))

(test make-osc-k-esc-transitions-to-st-state
  "make-osc-k on ESC (#x1B) returns a continuation waiting for backslash."
  (with-screen (s 10 5)
    (let* ((buf (make-osc-payload-buf ""))
           (k   (cl-tmux/terminal/parser::make-osc-k buf))
           (k2  (funcall k s #x1B)))
      (is (functionp k2)
          "make-osc-k on ESC must return a function (bridge continuation)"))))

(test make-osc-st-k-backslash-dispatches-and-grounds
  "make-osc-st-k on backslash dispatches and returns ground-state."
  (with-screen (s 20 5)
    ;; Payload: "2;xterm-st-title"
    (let* ((buf    (make-osc-payload-buf "2;xterm-st-title"))
           (k      (cl-tmux/terminal/parser::make-osc-st-k buf))
           (result (funcall k s #x5C)))      ; backslash = ST confirmed
      (is (eq #'cl-tmux/terminal/parser:ground-state result)
          "make-osc-st-k on backslash must return ground-state")
      (is (string= "xterm-st-title" (cl-tmux/terminal/types:screen-title s))
          "make-osc-st-k must dispatch OSC 2 and set screen-title"))))

(test make-osc-st-k-non-backslash-returns-ground
  "make-osc-st-k on a non-backslash byte returns ground-state without dispatching."
  (with-screen (s 20 5)
    (let* ((buf    (make-osc-payload-buf "0;title"))
           (k      (cl-tmux/terminal/parser::make-osc-st-k buf))
           (result (funcall k s (char-code #\X)))) ; not a backslash
      (is (eq #'cl-tmux/terminal/parser:ground-state result)
          "make-osc-st-k on non-backslash must still return ground-state")
      ;; Title must NOT have been set (malformed ST discarded)
      (is (not (string= "title" (cl-tmux/terminal/types:screen-title s)))
          "make-osc-st-k non-backslash must not dispatch the OSC"))))

;;; ── SUITE: osc-dispatch-edge-cases ──────────────────────────────────────────

(def-suite osc-dispatch-edge-cases
  :description "OSC dispatch edge cases: no-semicolon payload, unknown command"
  :in terminal-suite)
(in-suite osc-dispatch-edge-cases)

(test osc-payload-no-semicolon-is-noop
  "An OSC payload with no semicolon is silently discarded (no command to dispatch)."
  (with-screen (s 20 5)
    ;; Feed OSC with no semicolon: just the command number, BEL terminated.
    ;; This should not crash and must not set screen-title.
    (finishes
      (screen-process-bytes s
        (babel:string-to-octets
          (format nil "~C]notanumber~C" #\Escape (code-char 7))
          :encoding :utf-8)))
    ;; screen-title must remain at its default (NIL or empty string).
    (let ((title (cl-tmux/terminal/types:screen-title s)))
      (is (or (null title) (string= "" title))
          "screen-title must be unset after invalid OSC payload"))))

(test osc-unknown-command-is-silently-ignored
  "An OSC payload with a valid integer command but no matching rule is silently ignored."
  (with-screen (s 20 5)
    ;; OSC 99 is not handled — must not crash.
    (finishes
      (screen-process-bytes s
        (babel:string-to-octets
          (format nil "~C]99;some-data~C" #\Escape (code-char 7))
          :encoding :utf-8)))
    ;; screen-title must remain unset (OSC 99 has no handler).
    (let ((title (cl-tmux/terminal/types:screen-title s)))
      (is (or (null title) (string= "" title))
          "unknown OSC command must not alter screen-title"))))

(test osc-empty-payload-bel-is-noop
  "An OSC terminated immediately by BEL (empty payload) is consumed without error."
  (with-screen (s 20 5)
    (feed s "A")
    ;; ESC ] BEL — empty payload
    (screen-process-bytes s
      (make-array 3 :element-type '(unsigned-byte 8)
                    :initial-contents (list #x1B #x5D #x07)))
    (feed s "B")
    (is (char= #\A (char-at s 0 0)) "char before empty OSC must survive")
    (is (char= #\B (char-at s 1 0)) "char after empty OSC must be written")))

;;; ── SUITE: osc52-coverage ────────────────────────────────────────────────────

(def-suite osc52-coverage
  :description "OSC 52 clipboard handler: callback path and nil handler (silently dropped)"
  :in terminal-suite)
(in-suite osc52-coverage)

(test osc52-handler-invoked-with-decoded-text
  "When *osc52-handler* is set, OSC 52 with a valid Base64 payload invokes it
   with the decoded text string."
  (with-screen (s 20 5)
    ;; Base64-encode \"hello\" → SGVsbG8=
    (let* ((received nil)
           (cl-tmux/terminal/parser:*osc52-handler*
             (lambda (text) (setf received text))))
      ;; Base64 of "hello" is aGVsbG8=  (SGVsbG8= would decode to "Hello").
      ;; Feed OSC 52 ; c ; aGVsbG8= BEL  (c = clipboard target, ignored)
      (screen-process-bytes s
        (babel:string-to-octets
          (format nil "~C]52;c;aGVsbG8=~C" #\Escape (code-char 7))
          :encoding :utf-8))
      (is (string= "hello" received)
          "osc52-handler must be called with decoded text 'hello'"))))

(test osc52-nil-handler-silently-dropped
  "When *osc52-handler* is NIL, an OSC 52 sequence is consumed without error."
  (with-screen (s 20 5)
    (let ((cl-tmux/terminal/parser:*osc52-handler* nil))
      (finishes
        (screen-process-bytes s
          (babel:string-to-octets
            (format nil "~C]52;c;SGVsbG8=~C" #\Escape (code-char 7))
            :encoding :utf-8))))))

(test osc52-read-request-silently-ignored
  "OSC 52 with payload '?' (clipboard read request) is silently ignored."
  (with-screen (s 20 5)
    (let* ((received :not-called)
           (cl-tmux/terminal/parser:*osc52-handler*
             (lambda (text) (setf received text))))
      (screen-process-bytes s
        (babel:string-to-octets
          (format nil "~C]52;c;?~C" #\Escape (code-char 7))
          :encoding :utf-8))
      (is (eq :not-called received)
          "handler must NOT be invoked for a clipboard read request ('?')"))))

;;; ── OSC 7: current working directory (file://host/path) ──────────────────────

(test osc7-path-extraction
  "%osc7-path extracts the path from a file:// URL, with or without a host."
  (flet ((p (s) (cl-tmux/terminal/parser::%osc7-path s)))
    (is (string= "/home/u"     (p "file://host/home/u")) "with host")
    (is (string= "/home/u"     (p "file:///home/u"))     "empty host")
    (is (string= "/"           (p "file://host"))        "host but no path → /")
    (is (string= "not-a-url"   (p "not-a-url"))          "non-file:// → unchanged")))

(test osc7-sets-screen-cwd-end-to-end
  "Feeding ESC ] 7 ; file://host/path BEL sets screen-cwd to the path."
  (with-screen (s 20 5)
    (screen-process-bytes s
      (babel:string-to-octets
        (format nil "~C]7;file://myhost/home/user/project~C" #\Escape (code-char 7))
        :encoding :utf-8))
    (is (string= "/home/user/project" (cl-tmux/terminal/types:screen-cwd s))
        "screen-cwd must be the OSC 7 path after the sequence (got ~S)"
        (cl-tmux/terminal/types:screen-cwd s))))

(test percent-decode-cases
  "%percent-decode handles %20 spaces, UTF-8 multibyte, no-% passthrough, and an
   incomplete trailing % (left literal)."
  (flet ((d (s) (cl-tmux/terminal/parser::%percent-decode s)))
    (is (string= "a b"   (d "a%20b"))      "%20 → space")
    (is (string= "abc"   (d "abc"))        "no % → unchanged")
    (is (string= "/"     (d "%2F"))        "%2F → /")
    (is (string= "a%"    (d "a%"))         "incomplete trailing % is literal")
    (is (string= "a%zz"  (d "a%zz"))       "non-hex after % is literal")
    (is (string= "✓"     (d "%E2%9C%93"))  "UTF-8 multibyte (U+2713) decodes")))

(test osc7-path-percent-decoded
  "OSC 7 paths are percent-decoded — e.g. macOS '/Application Support'."
  (is (string= "/My Docs"
               (cl-tmux/terminal/parser::%osc7-path "file://host/My%20Docs")))
  (is (string= "/Library/Application Support"
               (cl-tmux/terminal/parser::%osc7-path
                "file:///Library/Application%20Support"))))

(test screen-cwd-defaults-empty
  "screen-cwd is empty on a fresh screen (no OSC 7 reported yet)."
  (with-screen (s 20 5)
    (is (string= "" (cl-tmux/terminal/types:screen-cwd s))
        "a fresh screen has no reported cwd")))

;;; ── Coverage gap: define-osc-rules macro ─────────────────────────────────────
;;;
;;; Audit finding: define-osc-rules was not tested as a macro in isolation.
;;; Symmetry with the define-state and define-dec-graphics-table assertions.

(test define-osc-rules-macro-is-defined
  "define-osc-rules is a defined macro in the parser package."
  (is (macro-function 'cl-tmux/terminal/parser::define-osc-rules)
      "define-osc-rules must be a macro"))

;;; ── Coverage gap: make-dcs-st-k direct test ──────────────────────────────────
;;;
;;; make-dcs-st-k was extracted from the inline lambda inside make-dcs-k.
;;; Test it directly to confirm symmetry with make-osc-st-k.

(def-suite direct-dcs-st-suite
  :description "Direct calls to make-dcs-st-k bridge continuation"
  :in terminal-suite)
(in-suite direct-dcs-st-suite)

(defun %fresh-dcs-buffer ()
  "A fresh empty adjustable octet buffer for make-dcs-st-k / make-dcs-k tests."
  (make-array 16 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t))

(test make-dcs-st-k-backslash-returns-ground
  "make-dcs-st-k on backslash (#x5C) returns ground-state (ST confirmed)."
  (let* ((s   (make-screen 10 5))
         (k   (cl-tmux/terminal/parser::make-dcs-st-k (%fresh-dcs-buffer)))
         (result (funcall k s #x5C)))
    (is (eq #'cl-tmux/terminal/parser:ground-state result)
        "make-dcs-st-k on backslash must return ground-state")))

(test make-dcs-st-k-non-backslash-resumes-consuming
  "make-dcs-st-k on a non-backslash byte resumes DCS consumption (returns a continuation)."
  (let* ((s   (make-screen 10 5))
         (k   (cl-tmux/terminal/parser::make-dcs-st-k (%fresh-dcs-buffer)))
         (result (funcall k s (char-code #\A))))
    (is (functionp result)
        "make-dcs-st-k on non-backslash must return a continuation (keeps consuming DCS)")))

;;; ── tmux DCS passthrough (allow-passthrough) ─────────────────────────────────

(test dcs-passthrough-tmux-prefix-queues-inner-sequence
  "A \\ePtmux;<payload>\\e\\\\ DCS with doubled ESCs queues the un-doubled inner
   sequence on the screen's passthrough-queue."
  (let ((s (make-screen 10 5)))
    ;; Feed: ESC P t m u x ;  ESC ESC ] 1 3 3 7  ESC \   (doubled inner ESC)
    ;; Inner un-doubled should be: ESC ] 1 3 3 7
    (cl-tmux/terminal/emulator:screen-process-bytes
     s (coerce (list #x1B #x50               ; ESC P (DCS)
                     116 109 117 120 59      ; tmux;
                     #x1B #x1B 93 49 51 51 55 ; \e\e ] 1 3 3 7  (doubled ESC)
                     #x1B #x5C)              ; ESC \  (ST)
               '(vector (unsigned-byte 8))))
    (let ((queue (cl-tmux/terminal/types:screen-passthrough-queue s)))
      (is (= 1 (length queue)) "one passthrough sequence queued")
      (let ((seq (first queue)))
        (is (char= #\Escape (char seq 0)) "inner sequence starts with un-doubled ESC")
        (is (string= "]1337" (subseq seq 1)) "inner payload after the single ESC")))))

(test dcs-non-tmux-prefix-is-discarded
  "A non-tmux DCS (e.g. Sixel) is consumed and NOT queued for passthrough."
  (let ((s (make-screen 10 5)))
    ;; ESC P q <sixel-ish bytes> ESC \  — prefix is 'q', not 'tmux;'
    (cl-tmux/terminal/emulator:screen-process-bytes
     s (coerce (list #x1B #x50 113 35 48 #x1B #x5C) '(vector (unsigned-byte 8))))
    (is (null (cl-tmux/terminal/types:screen-passthrough-queue s))
        "non-tmux DCS must not populate the passthrough-queue")))

;;; ── Coverage gap: make-bytes / feed-osc helpers ──────────────────────────────
;;;
;;; Audit finding: the pattern
;;;   (make-array N :element-type '(unsigned-byte 8) :initial-contents '(...))
;;; is repeated 7+ times in parser-tests.lisp.  Centralise it as make-bytes.
;;; The pattern
;;;   (screen-process-bytes s (babel:string-to-octets (format nil "~C]N;...~C" ...) :encoding :utf-8))
;;; is repeated 10+ times.  Centralise it as feed-osc.

(defun make-bytes (&rest byte-values)
  "Return a simple (unsigned-byte 8) vector containing BYTE-VALUES."
  (make-array (length byte-values)
              :element-type '(unsigned-byte 8)
              :initial-contents byte-values))

(defun feed-osc (screen command-number body-string)
  "Feed an OSC sequence with integer COMMAND-NUMBER and BODY-STRING to SCREEN,
   terminated by BEL (ASCII 7).  Uses UTF-8 encoding to match real terminal behaviour."
  (screen-process-bytes screen
    (babel:string-to-octets
      (format nil "~C]~D;~A~C" #\Escape command-number body-string (code-char 7))
      :encoding :utf-8)))

;;; Verify the helpers function correctly before relying on them in later tests.

(test make-bytes-helper
  "make-bytes returns a (unsigned-byte 8) vector with the given byte values."
  (let ((bytes (make-bytes #x1B #x5D #x07)))
    (is (= 3 (length bytes)) "length must be 3")
    (is (= #x1B (aref bytes 0)) "first byte must be ESC")
    (is (= #x5D (aref bytes 1)) "second byte must be ]")
    (is (= #x07 (aref bytes 2)) "third byte must be BEL")))

(test feed-osc-helper
  "feed-osc sends an OSC sequence that causes the expected side-effect."
  (with-screen (s 20 5)
    (feed-osc s 0 "test-title")
    (is (string= "test-title" (cl-tmux/terminal/types:screen-title s))
        "feed-osc for OSC 0 must set screen-title")))

;;; ── Coverage gap: zero-length buffer in screen-process-bytes ─────────────────
;;;
;;; Audit finding: screen-process-bytes with start=0, end=0 on a zero-length
;;; buffer was not tested.

(def-suite parser-suite
  :description "Parser and emulator coverage gap tests"
  :in terminal-suite)
(in-suite parser-suite)

(test screen-process-bytes-zero-length-buffer-is-noop
  "screen-process-bytes on a zero-length buffer (start=end=0) is a no-op."
  (with-screen (s 10 5)
    (let ((buf (make-array 0 :element-type '(unsigned-byte 8))))
      (screen-process-bytes s buf :start 0 :end 0))
    (is (char= #\Space (char-at s 0 0))
        "zero-length buffer must leave screen unchanged")))

;;; ── Coverage gap: %base64-decode edge cases ──────────────────────────────────
;;;
;;; Audit finding: Base64 padding ('='), truncated input, and invalid characters
;;; were not directly asserted.

(def-suite base64-decode-suite
  :description "Direct coverage of %base64-decode edge cases"
  :in terminal-suite)
(in-suite base64-decode-suite)

(test base64-decode-basic-string
  "%base64-decode decodes a standard Base64 string ('hello' = aGVsbG8=)."
  (let ((result (cl-tmux/terminal/parser::%base64-decode "aGVsbG8=")))
    (is (not (null result)) "must return a byte vector, not NIL")
    (is (string= "hello"
                 (babel:octets-to-string result :encoding :utf-8))
        "aGVsbG8= must decode to 'hello'")))

(test base64-decode-empty-string
  "%base64-decode on an empty string returns an empty byte vector."
  (let ((result (cl-tmux/terminal/parser::%base64-decode "")))
    (is (or (null result) (zerop (length result)))
        "empty input must produce empty output or NIL")))

(test base64-decode-truncated-group
  "%base64-decode on input shorter than 4 chars does not crash."
  (finishes (cl-tmux/terminal/parser::%base64-decode "YQ"))
  ;; 'YQ' decodes to 'a' (no padding); should succeed without error.
  (let ((result (cl-tmux/terminal/parser::%base64-decode "YQ==")))
    (is (not (null result)) "padded 2-char group must decode successfully")))

;;; ── Coverage gap: %parse-osc-command error branch ────────────────────────────
;;;
;;; Audit finding: the error-return branch (non-integer command field) was not
;;; directly asserted.

(test parse-osc-command-returns-nil-for-non-integer
  "%parse-osc-command returns NIL when the command field is not a valid integer."
  (let ((result (cl-tmux/terminal/parser::%parse-osc-command "notanumber" 10)))
    (is (null result)
        "%parse-osc-command must return NIL for a non-integer command field")))

(test parse-osc-command-returns-integer-for-valid-input
  "%parse-osc-command returns the integer for a valid command field."
  (let ((result (cl-tmux/terminal/parser::%parse-osc-command "52;data" 2)))
    (is (= 52 result)
        "%parse-osc-command must return 52 for '52' prefix")))

;;; ── Coverage gap: %handle-osc-52 no-inner-semicolon branch ──────────────────
;;;
;;; Audit finding: the branch where the OSC 52 body has no semicolon was not
;;; directly tested.

(test handle-osc-52-no-inner-semicolon-is-noop
  "%handle-osc-52 is a no-op when the body has no semicolon (malformed OSC 52)."
  (let ((received :not-called)
        (cl-tmux/terminal/parser:*osc52-handler*
          (lambda (text) (setf received text))))
    (finishes (cl-tmux/terminal/parser::%handle-osc-52 "nodatahere"))
    (is (eq :not-called received)
        "%handle-osc-52 with no semicolon must not invoke the handler")))

;;; ── CSI colon sub-parameters (ISO 8613-6) ───────────────────────────────────
;;;
;;; A colon introduces sub-parameters within one CSI parameter (SGR 4:3 undercurl,
;;; 38:2::R:G:B true-colour).  The parser keeps the leading value and skips the
;;; rest, so such a sequence neither aborts (printing stray bytes) nor mis-applies.

(def-suite csi-colon-subparams :description "CSI colon sub-parameter handling"
  :in parser-suite)
(in-suite csi-colon-subparams)

(test csi-colon-undercurl-keeps-leading-underline
  "CSI 4:3 m (undercurl) keeps the leading 4 → underline; no stray bytes print."
  (with-screen (s 8 2)
    (feed s (esc "[4:3m"))            ; undercurl via colon sub-parameter
    (feed s "X")
    (is (char= #\X (char-at s 0 0))
        "X must be the first cell — the colon sequence printed nothing")
    (is (logbitp 3 (attrs-at s 0 0))
        "the leading 4 must set the underline attribute (bit 3)")))

(test csi-colon-multi-param-mixed
  "CSI 0;4:3;1 m applies reset, underline (from 4:3), bold — colon does not
   bleed into the neighbouring parameters."
  (with-screen (s 8 2)
    (feed s (esc "[0;4:3;1m"))
    (feed s "Y")
    (is (char= #\Y (char-at s 0 0)) "Y is the first cell")
    (is (logbitp 3 (attrs-at s 0 0)) "underline set (from 4:3)")
    (is (logbitp 0 (attrs-at s 0 0)) "bold set (from the trailing ;1)")))

(test csi-colon-truecolor-form-does-not-abort
  "CSI 38:2::255:0:0 m (colon true-colour) must not abort and spew bytes; the
   following text writes cleanly at column 0."
  (with-screen (s 8 2)
    (feed s (esc "[38:2::255:0:0m"))
    (feed s "Z")
    (is (char= #\Z (char-at s 0 0))
        "Z must be the first cell — no stray sub-parameter bytes printed")))
