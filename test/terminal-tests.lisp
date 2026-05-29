(in-package #:cl-tmux/test)

;;;; VT100/ANSI emulator tests — comprehensive, higher-abstraction rewrite.
;;;;
;;;; Each suite targets one concern: basic text, cursor movement, erase,
;;;; SGR attributes, UTF-8 decoding, scroll region, resize, and miscellaneous
;;;; special sequences.  Table-driven helpers are used throughout to keep
;;;; individual tests concise.

;;; ── Shared helpers ─────────────────────────────────────────────────────────
;;;
;;; The screen-builder DSL (with-screen, feed, octets, esc, csi, row-string,
;;; char-at, fg-at, bg-at, attrs-at) lives in test/helpers.lisp, which the
;;; system loads first.  Only emulator-specific helpers are defined below.

(defmacro check-cursor (screen cx cy)
  "Assert that SCREEN's cursor is at column CX, row CY."
  `(progn
     (is (= ,cx (screen-cursor-x ,screen))
         "cursor-x: expected ~D got ~D" ,cx (screen-cursor-x ,screen))
     (is (= ,cy (screen-cursor-y ,screen))
         "cursor-y: expected ~D got ~D" ,cy (screen-cursor-y ,screen))))

(defun row-blank-p (screen y)
  "Return T when every cell in row Y of SCREEN contains a space."
  (every (lambda (c) (char= #\Space c))
         (coerce (row-string screen y) 'list)))

(defun utf8-feed (screen lisp-string)
  "Encode LISP-STRING as UTF-8 and feed the bytes to SCREEN."
  (screen-process-bytes screen
                        (babel:string-to-octets lisp-string :encoding :utf-8))
  screen)

;;; ── Top-level suite (collected by suite.lisp) ─────────────────────────────

(def-suite terminal-suite :description "VT100/ANSI terminal emulator")

;;; ── SUITE: basic-text ───────────────────────────────────────────────────────

(def-suite basic-text
  :description "Printable characters, CR/LF, wrap, BS, TAB"
  :in terminal-suite)
(in-suite basic-text)

(test plain-text
  "Printing five ASCII characters places them in row 0 and advances cursor."
  (with-screen (s 20 5)
    (feed s "hello")
    (is (string= "hello" (row-string s 0 :end 5)))
    (check-cursor s 5 0)))

(test crlf
  "CR+LF moves to column 0 of the next row."
  (with-screen (s 20 5)
    (feed s "ab")
    (feed s (format nil "~C~C" #\Return #\Linefeed))
    (feed s "cd")
    (is (string= "ab" (row-string s 0 :end 2)))
    (is (string= "cd" (row-string s 1 :end 2)))
    (check-cursor s 2 1)))

(test carriage-return
  "A bare CR (#x0D) returns the cursor to column 0 on the same row, leaving
   the already-written cells intact (overwrite begins at column 0)."
  (with-screen (s 20 5)
    (feed s "abc")                         ; cursor at (3, 0)
    (check-cursor s 3 0)
    (feed s (string #\Return))             ; CR → column 0, row unchanged
    (check-cursor s 0 0)
    ;; The previously written cells survive the CR.
    (is (string= "abc" (row-string s 0 :end 3))
        "CR must not erase already-written cells, got ~S" (row-string s 0 :end 3))
    ;; Subsequent text overwrites from column 0.
    (feed s "XY")
    (is (string= "XYc" (row-string s 0 :end 3))
        "writing after CR must overwrite from column 0, got ~S"
        (row-string s 0 :end 3))
    (check-cursor s 2 0)))

(test carriage-return-keeps-row
  "CR after moving to a lower row resets the column to 0 but keeps the row."
  (with-screen (s 20 5)
    (feed s (esc "[3;6H"))                 ; cursor → (5, 2)
    (check-cursor s 5 2)
    (feed s (string #\Return))             ; CR → column 0, still row 2
    (check-cursor s 0 2)))

(test line-wrap
  "A 4-wide screen wraps 'abcde' so row 0 = 'abcd', row 1 starts with 'e'."
  (with-screen (s 4 3)
    (feed s "abcde")
    (is (string= "abcd" (row-string s 0)))
    (is (char= #\e (char-at s 0 1)))
    (check-cursor s 1 1)))

(test backspace
  "Backspace after 'abc' leaves the cursor at column 2."
  (with-screen (s 10 2)
    (feed s "abc")
    (feed s (string #\Backspace))
    (check-cursor s 2 0)))

(test tab-stop
  "After 'a', a TAB advances to the next 8-column stop (column 8)."
  (with-screen (s 40 2)
    (feed s "a")
    (feed s (string #\Tab))
    (check-cursor s 8 0)))

(test tab-already-at-stop
  "Eight spaces bring the cursor to column 8; a TAB then jumps to column 16."
  (with-screen (s 40 2)
    (feed s "        ")   ; 8 spaces → cursor at (8, 0)
    (feed s "a")          ; cursor at (9, 0)
    ;; back to col 8 manually so TAB fires to col 16
    (feed s (esc "[1;9H")) ; CUP row=1 col=9 (1-based) → (8, 0)
    (feed s (string #\Tab))
    (check-cursor s 16 0)))

;;; ── SUITE: cursor-movement ──────────────────────────────────────────────────

(def-suite cursor-movement
  :description "CSI A/B/C/D/E/F/G/H/f/d cursor sequences"
  :in terminal-suite)
(in-suite cursor-movement)

(test cup
  "CUP ESC[3;5H positions cursor at (col=4, row=2) in 0-based terms."
  (with-screen (s 20 10)
    (feed s (esc "[3;5H"))
    (check-cursor s 4 2)))

(test cuu
  "CUU ESC[2A moves cursor up 2 rows."
  (with-screen (s 20 10)
    (feed s (esc "[5;5H"))  ; → (4, 4)
    (feed s (esc "[2A"))    ; up 2 → y=2
    (check-cursor s 4 2)))

(test cud
  "CUD ESC[3B moves cursor down 3 rows."
  (with-screen (s 20 10)
    (feed s (esc "[1;1H"))  ; → (0, 0)
    (feed s (esc "[3B"))    ; down 3 → y=3
    (check-cursor s 0 3)))

(test cuf
  "CUF ESC[4C moves cursor right 4 columns."
  (with-screen (s 20 10)
    (feed s (esc "[1;3H"))  ; → (2, 0)
    (feed s (esc "[4C"))    ; right 4 → x=6
    (check-cursor s 6 0)))

(test cub
  "CUB ESC[4D moves cursor left 4 columns."
  (with-screen (s 20 10)
    (feed s (esc "[1;7H"))  ; → (6, 0)
    (feed s (esc "[4D"))    ; left 4 → x=2
    (check-cursor s 2 0)))

(test cnl
  "CNL ESC[2E moves cursor to column 0 two rows down."
  (with-screen (s 20 10)
    (feed s (esc "[3;5H"))  ; → (4, 2)
    (feed s (esc "[2E"))    ; next 2 lines → (0, 4)
    (check-cursor s 0 4)))

(test cpl
  "CPL ESC[2F moves cursor to column 0 two rows up."
  (with-screen (s 20 10)
    (feed s (esc "[5;5H"))  ; → (4, 4)
    (feed s (esc "[2F"))    ; preceding 2 lines → (0, 2)
    (check-cursor s 0 2)))

(test cha
  "CHA ESC[5G moves cursor to column 4 (1-based 5)."
  (with-screen (s 20 10)
    (feed s (esc "[4;4H"))  ; → (3, 3)
    (feed s (esc "[5G"))    ; column 5 (1-based) → x=4
    (check-cursor s 4 3)))

(test vpa
  "VPA ESC[5d moves cursor to row 4 (1-based 5)."
  (with-screen (s 20 10)
    (feed s (esc "[4;4H"))  ; → (3, 3)
    (feed s (esc "[5d"))    ; row 5 (1-based) → y=4
    (check-cursor s 3 4)))

(test hvp
  "HVP ESC[3;5f is equivalent to CUP."
  (with-screen (s 20 10)
    (feed s (esc "[3;5f"))
    (check-cursor s 4 2)))

(test clamp
  "Out-of-bounds CUP ESC[100;100H clamps to the last valid cell."
  (with-screen (s 10 5)
    (feed s (esc "[100;100H"))
    (check-cursor s 9 4)))

(test cursor-movement-table
  "Table-driven: verify each cursor CSI sequence independently."
  (let ((cases
          ;; (setup-seq  motion-seq  expected-cx  expected-cy)
          `(("" ,(esc "[5;5H") 4 4)
            (,(esc "[5;5H") ,(esc "[2A") 4 2)
            (,(esc "[1;1H") ,(esc "[3B") 0 3)
            (,(esc "[1;3H") ,(esc "[4C") 6 0)
            (,(esc "[1;7H") ,(esc "[4D") 2 0)
            (,(esc "[3;5H") ,(esc "[2E") 0 4)
            (,(esc "[5;5H") ,(esc "[2F") 0 2))))
    (dolist (c cases)
      (destructuring-bind (setup motion ecx ecy) c
        (with-screen (s 20 10)
          (unless (string= setup "") (feed s setup))
          (feed s motion)
          (is (= ecx (screen-cursor-x s))
              "cx ~D expected ~D after ~S" (screen-cursor-x s) ecx motion)
          (is (= ecy (screen-cursor-y s))
              "cy ~D expected ~D after ~S" (screen-cursor-y s) ecy motion))))))

;;; ── SUITE: erase ────────────────────────────────────────────────────────────

(def-suite erase
  :description "ED (erase display) and EL (erase line) modes"
  :in terminal-suite)
(in-suite erase)

(defun fill-screen (screen)
  "Fill every cell of SCREEN with 'X' and return SCREEN."
  (dotimes (y (screen-height screen) screen)
    (dotimes (x (screen-width screen))
      (feed screen "X"))))

(test ed-0
  "ESC[J erases from the cursor position to the end of the display."
  (with-screen (s 5 3)
    (fill-screen s)
    (feed s (esc "[2;3H"))   ; cursor at (2, 1)
    (feed s (esc "[0J"))     ; erase to end
    ;; row 0 must be fully filled
    (is (string= "XXXXX" (row-string s 0)))
    ;; cells from cursor position onwards on row 1 must be blank
    (is (char= #\Space (char-at s 2 1)))
    (is (char= #\Space (char-at s 4 1)))
    ;; row 2 must be fully blank
    (is (row-blank-p s 2))))

(test ed-1
  "ESC[1J erases from the start of the display to the cursor (inclusive)."
  (with-screen (s 5 3)
    (fill-screen s)
    (feed s (esc "[2;3H"))   ; cursor at (2, 1)
    (feed s (esc "[1J"))
    ;; row 0 must be blank
    (is (row-blank-p s 0))
    ;; cells up to and including cursor on row 1 must be blank
    (is (char= #\Space (char-at s 0 1)))
    (is (char= #\Space (char-at s 2 1)))
    ;; cell after cursor on row 1 is still filled
    (is (char= #\X (char-at s 3 1)))))

(test ed-2
  "ESC[2J erases the entire display."
  (with-screen (s 5 3)
    (fill-screen s)
    (feed s (esc "[2J"))
    (dotimes (y 3)
      (is (row-blank-p s y) "row ~D not blank after ED 2" y))))

(test el-0
  "ESC[K erases from the cursor to the end of the current line."
  (with-screen (s 10 2)
    (feed s "abcdefghij")        ; fill row 0
    (feed s (esc "[1;5H"))       ; cursor at (4, 0)
    (feed s (esc "[0K"))
    (is (string= "abcd" (row-string s 0 :end 4)))
    (is (char= #\Space (char-at s 4 0)))
    (is (char= #\Space (char-at s 9 0)))))

(test el-1
  "ESC[1K erases from the start of the line to the cursor (inclusive)."
  (with-screen (s 10 2)
    (feed s "abcdefghij")
    (feed s (esc "[1;4H"))       ; cursor at (3, 0)
    (feed s (esc "[1K"))
    (is (char= #\Space (char-at s 0 0)))
    (is (char= #\Space (char-at s 3 0)))
    (is (char= #\e (char-at s 4 0)))))

(test el-2
  "ESC[2K erases the entire current line."
  (with-screen (s 10 2)
    (feed s "abcdefghij")
    (feed s (esc "[1;5H"))
    (feed s (esc "[2K"))
    (is (row-blank-p s 0))
    ;; cursor y unchanged
    (is (= 0 (screen-cursor-y s)))))

;;; ── SUITE: sgr ──────────────────────────────────────────────────────────────

(def-suite sgr
  :description "Select Graphic Rendition — colour and attribute codes"
  :in terminal-suite)
(in-suite sgr)

(test sgr-foreground-table
  "Standard foreground SGR codes 31-37 set fg indices 1-7."
  (loop for code from 31 to 37
        for expected-fg from 1 to 7
        do (with-screen (s 10 2)
             (feed s (esc "[~DmX" code))
             (is (= expected-fg (fg-at s 0 0))
                 "SGR ~D: expected fg ~D got ~D"
                 code expected-fg (fg-at s 0 0)))))

(test sgr-background-table
  "Standard background SGR codes 41-47 set bg indices 1-7."
  (loop for code from 41 to 47
        for expected-bg from 1 to 7
        do (with-screen (s 10 2)
             (feed s (esc "[~DmX" code))
             (is (= expected-bg (bg-at s 0 0))
                 "SGR ~D: expected bg ~D got ~D"
                 code expected-bg (bg-at s 0 0)))))

(test sgr-bright-foreground-table
  "Bright foreground SGR codes 90-97 set fg indices 8-15."
  (loop for code from 90 to 97
        for expected-fg from 8 to 15
        do (with-screen (s 10 2)
             (feed s (esc "[~DmX" code))
             (is (= expected-fg (fg-at s 0 0))
                 "SGR ~D: expected fg ~D got ~D"
                 code expected-fg (fg-at s 0 0)))))

(test sgr-bold
  "SGR 1 sets the bold attribute bit."
  (with-screen (s 10 2)
    (feed s (esc "[1mB"))
    (is (logbitp 0 (attrs-at s 0 0)) "bold bit not set")))

(test sgr-dim
  "SGR 2 sets the dim attribute bit."
  (with-screen (s 10 2)
    (feed s (esc "[2mD"))
    (is (not (zerop (logand (attrs-at s 0 0) #b010))) "dim bit not set")))

(test sgr-reverse
  "SGR 7 sets the reverse-video attribute bit."
  (with-screen (s 10 2)
    (feed s (esc "[7mR"))
    (is (not (zerop (logand (attrs-at s 0 0) #b100))) "reverse bit not set")))

(test sgr-reset
  "SGR 0 after setting colours and bold restores defaults on the next cell."
  (with-screen (s 10 2)
    ;; Write X with red bold, then reset, then write Y.
    (feed s (esc "[31;1mX"))
    (feed s (esc "[0mY"))
    ;; Y should carry default fg (7), default bg (0), no attrs
    (is (= 7 (fg-at    s 1 0)) "fg not reset: got ~D" (fg-at s 1 0))
    (is (= 0 (bg-at    s 1 0)) "bg not reset: got ~D" (bg-at s 1 0))
    (is (= 0 (attrs-at s 1 0)) "attrs not reset: got ~D" (attrs-at s 1 0))))

(test sgr-compound
  "ESC[1;31;42m sets bold, fg=1, bg=2 simultaneously."
  (with-screen (s 10 2)
    (feed s (esc "[1;31;42mX"))
    (is (= 1 (fg-at s 0 0))   "fg expected 1")
    (is (= 2 (bg-at s 0 0))   "bg expected 2")
    (is (logbitp 0 (attrs-at s 0 0)) "bold bit not set")))

(test sgr-bright-red
  "ESC[91m sets fg=9 (bright red)."
  (with-screen (s 10 2)
    (feed s (esc "[91mR"))
    (is (= 9 (fg-at s 0 0)) "expected fg 9 (bright red)")))

;;; ── SUITE: utf8 ─────────────────────────────────────────────────────────────

(def-suite utf8
  :description "Multi-byte UTF-8 character decoding"
  :in terminal-suite)
(in-suite utf8)

(test utf8-2byte
  "U+00E9 (é) is decoded from its 2-byte UTF-8 encoding."
  (with-screen (s 10 2)
    (utf8-feed s "é")
    (is (char= #\é (char-at s 0 0)))))

(test utf8-3byte
  "U+3042 (あ) is decoded from its 3-byte UTF-8 encoding."
  (with-screen (s 10 2)
    (utf8-feed s "あ")
    (is (char= #\あ (char-at s 0 0)))))

(test utf8-4byte
  "A 4-byte UTF-8 code point is decoded correctly (e.g. U+1F600 if in limit)."
  ;; U+1F600 = 😀; only test if the Lisp runtime supports it.
  (when (< #x1F600 char-code-limit)
    (with-screen (s 10 2)
      ;; Feed the 4-byte UTF-8 sequence for U+1F600: F0 9F 98 80
      (screen-process-bytes s (make-array 4 :element-type '(unsigned-byte 8)
                                            :initial-contents '(#xF0 #x9F #x98 #x80)))
      (is (char= (code-char #x1F600) (char-at s 0 0))))))

(test utf8-split
  "U+3042 split across two feed calls (E3 | 81 82) assembles correctly."
  (with-screen (s 10 2)
    (screen-process-bytes s (make-array 1 :element-type '(unsigned-byte 8)
                                          :initial-contents '(#xE3)))
    (screen-process-bytes s (make-array 2 :element-type '(unsigned-byte 8)
                                          :initial-contents '(#x81 #x82)))
    (is (char= #\あ (char-at s 0 0)))))

(test utf8-mixed
  "ASCII + wide CJK + ASCII: the CJK char occupies two columns, so the
   trailing ASCII lands at column 3 (column 2 is the continuation cell)."
  (with-screen (s 10 2)
    (utf8-feed s "aあb")
    (is (char= #\a  (char-at s 0 0)))
    (is (char= #\あ (char-at s 1 0)))
    (is (= 2 (cell-width (cell-at s 1 0))) "あ must be a double-width lead cell")
    (is (= 0 (cell-width (cell-at s 2 0))) "column 2 must be a continuation cell")
    (is (char= #\b  (char-at s 3 0)) "trailing ASCII lands after the wide char")))

(test utf8-box-drawing
  "Box-drawing characters are decoded and placed correctly."
  (with-screen (s 10 2)
    (utf8-feed s "│─")
    (is (char= #\│ (char-at s 0 0)))
    (is (char= #\─ (char-at s 1 0)))))

(test utf8-malformed
  "A bare #xFF byte (invalid UTF-8) produces U+FFFD at the cursor."
  (with-screen (s 10 2)
    (screen-process-bytes s (make-array 1 :element-type '(unsigned-byte 8)
                                          :initial-contents '(#xFF)))
    (is (char= (code-char #xFFFD) (char-at s 0 0)))))

;;; ── SUITE: double-width (CJK) ───────────────────────────────────────────────

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

;;; ── SUITE: scroll-region ────────────────────────────────────────────────────

(def-suite scroll-region
  :description "Scrolling, DECSTBM, reverse index, IL/DL"
  :in terminal-suite)
(in-suite scroll-region)

(test scroll-auto
  "Writing a 4th line into a 3-row screen scrolls the content up."
  (with-screen (s 5 3)
    (feed s (format nil "L1~C~CL2~C~CL3~C~CL4"
                    #\Return #\Linefeed
                    #\Return #\Linefeed
                    #\Return #\Linefeed))
    ;; After one scroll: row 0 = old row 1 = "L2", row 2 = "L4".
    (is (string= "L2" (row-string s 0 :end 2))
        "row 0 should be 'L2' after scroll, got ~S" (row-string s 0 :end 2))
    (is (string= "L4" (row-string s 2 :end 2))
        "row 2 should be 'L4' after scroll, got ~S" (row-string s 2 :end 2))))

(test decstbm
  "DECSTBM restricts scrolling to the specified region (rows 2-3 of 5)."
  (with-screen (s 5 5)
    ;; Write one identifiable line per row.
    (feed s (format nil "R0~C~CR1~C~CR2~C~CR3~C~CR4"
                    #\Return #\Linefeed
                    #\Return #\Linefeed
                    #\Return #\Linefeed
                    #\Return #\Linefeed))
    ;; Set scroll region to rows 2-4 (1-based: ESC[2;4r).
    (feed s (esc "[2;4r"))      ; scroll region = rows 1-3 (0-based)
    ;; Now move into the region and force a scroll.
    (feed s (esc "[4;1H"))      ; cursor to row 4 (0-based 3), col 1
    (feed s (format nil "~C~CNR" #\Return #\Linefeed))
    ;; Row 0 must be untouched.
    (is (string= "R0" (row-string s 0 :end 2))
        "row 0 should be untouched, got ~S" (row-string s 0 :end 2))))

(test reverse-index
  "ESC M at the top of the scroll region scrolls the region down."
  (with-screen (s 5 3)
    ;; Fill rows with identifiable content.
    (feed s (format nil "AA~C~CBB~C~CCC"
                    #\Return #\Linefeed
                    #\Return #\Linefeed))
    ;; Move cursor to row 0 (top) and send RI.
    (feed s (esc "[1;1H"))   ; cursor home
    (feed s (esc "M"))       ; ESC M = RI
    ;; The scroll region shifts down: old row 0 ("AA") should now be at row 1.
    (is (string= "AA" (row-string s 1 :end 2))
        "after RI, old row 0 should be at row 1; got ~S" (row-string s 1 :end 2))
    ;; New row 0 should be blank.
    (is (row-blank-p s 0) "new row 0 should be blank after RI")))

(test il-insert-lines
  "ESC[2L (insert 2 lines) pushes existing content down."
  (with-screen (s 5 4)
    (feed s (format nil "AA~C~CBB~C~CCC~C~CDD"
                    #\Return #\Linefeed
                    #\Return #\Linefeed
                    #\Return #\Linefeed))
    ;; Move to row 1 and insert 2 lines.
    (feed s (esc "[2;1H"))   ; cursor to row 2 (0-based 1)
    (feed s (esc "[2L"))     ; insert 2 lines
    ;; Row 0 untouched; rows 1-2 blank; old row 1 ("BB") now at row 3.
    (is (string= "AA" (row-string s 0 :end 2)))
    (is (row-blank-p s 1))
    (is (row-blank-p s 2))
    (is (string= "BB" (row-string s 3 :end 2)))))

(test dl-delete-lines
  "ESC[2M (delete 2 lines) pulls content up."
  (with-screen (s 5 4)
    (feed s (format nil "AA~C~CBB~C~CCC~C~CDD"
                    #\Return #\Linefeed
                    #\Return #\Linefeed
                    #\Return #\Linefeed))
    ;; Move to row 1 and delete 2 lines.
    (feed s (esc "[2;1H"))
    (feed s (esc "[2M"))
    ;; Row 0 untouched; old row 3 ("DD") shifts up to row 1; rows 2-3 blank.
    (is (string= "AA" (row-string s 0 :end 2)))
    (is (string= "DD" (row-string s 1 :end 2)))
    (is (row-blank-p s 2))
    (is (row-blank-p s 3))))

;;; ── SUITE: resize ───────────────────────────────────────────────────────────

(def-suite resize
  :description "Screen resize behaviour"
  :in terminal-suite)
(in-suite resize)

(test resize-larger
  "Resizing to a larger screen preserves existing content and updates dimensions."
  (with-screen (s 10 5)
    (feed s "hello")
    (screen-resize s 20 8)
    (is (= 20 (screen-width  s)))
    (is (= 8  (screen-height s)))
    (is (string= "hello" (row-string s 0 :end 5)))))

(test resize-smaller-clamps-cursor
  "Shrinking the screen clamps an out-of-bounds cursor into the new bounds."
  (with-screen (s 20 10)
    (feed s (esc "[10;20H"))  ; cursor near bottom-right
    (screen-resize s 5 3)
    (is (<= (screen-cursor-x s) 4)
        "cursor-x ~D exceeds new width-1=4" (screen-cursor-x s))
    (is (<= (screen-cursor-y s) 2)
        "cursor-y ~D exceeds new height-1=2" (screen-cursor-y s))))

(test resize-noop
  "Resizing to the same dimensions leaves content and cursor unchanged."
  (with-screen (s 10 5)
    (feed s "abc")
    (let ((cx (screen-cursor-x s))
          (cy (screen-cursor-y s)))
      (screen-resize s 10 5)
      (is (string= "abc" (row-string s 0 :end 3)))
      (is (= cx (screen-cursor-x s)))
      (is (= cy (screen-cursor-y s))))))

;;; ── SUITE: special ──────────────────────────────────────────────────────────

(def-suite special
  :description "Miscellaneous terminal behaviour: BEL, OSC, RIS, DEC PM, alt screen"
  :in terminal-suite)
(in-suite special)

(test bel-ignored
  "BEL (byte #x07) does not alter the screen or cursor."
  (with-screen (s 10 2)
    (feed s "ab")
    (screen-process-bytes s (make-array 1 :element-type '(unsigned-byte 8)
                                          :initial-contents '(#x07)))
    ;; Screen content and cursor must be unchanged.
    (is (char= #\a (char-at s 0 0)))
    (is (char= #\b (char-at s 1 0)))
    (check-cursor s 2 0)))

(test osc-bel-ignored
  "An OSC sequence terminated by BEL is consumed without crashing."
  (with-screen (s 10 2)
    (feed s "a")
    ;; OSC 0 ; title BEL — common in xterm
    (screen-process-bytes s
      (babel:string-to-octets
        (format nil "~C]0;window title~C" #\Escape #\Bel)
        :encoding :utf-8))
    (feed s "b")
    (is (char= #\a (char-at s 0 0)))
    (is (char= #\b (char-at s 1 0)))))

(test osc-st-ignored
  "An OSC sequence terminated by ESC \\ (ST) is consumed without crashing."
  (with-screen (s 10 2)
    (feed s "a")
    ;; OSC terminated by ST = ESC \
    (screen-process-bytes s
      (babel:string-to-octets
        (format nil "~C]0;title~C\\" #\Escape #\Escape)
        :encoding :utf-8))
    (feed s "b")
    (is (char= #\a (char-at s 0 0)))
    (is (char= #\b (char-at s 1 0)))))

(test csi-unknown
  "An unrecognised CSI final character is silently ignored; parser recovers."
  (with-screen (s 10 2)
    (feed s "a")
    ;; ESC [ z  — 'z' is not a standard CSI final
    (feed s (esc "[z"))
    (feed s "b")
    (is (char= #\a (char-at s 0 0)))
    (is (char= #\b (char-at s 1 0)))))

(test ris
  "ESC c (RIS) clears the screen and homes the cursor."
  (with-screen (s 10 5)
    (feed s "hello")
    (feed s (esc "[3;3H"))
    (feed s (esc "c"))          ; ESC c = RIS
    (check-cursor s 0 0)
    (is (row-blank-p s 0) "row 0 should be blank after RIS")
    (is (row-blank-p s 1) "row 1 should be blank after RIS")))

(test dec-pm-hide-show-cursor
  "ESC[?25l (hide cursor) and ESC[?25h (show cursor) do not crash."
  (with-screen (s 10 2)
    (feed s "a")
    (feed s (esc "[?25l"))    ; hide cursor — accepted silently
    (feed s "b")
    (feed s (esc "[?25h"))    ; show cursor — accepted silently
    (feed s "c")
    ;; All three characters must be on screen.
    (is (char= #\a (char-at s 0 0)))
    (is (char= #\b (char-at s 1 0)))
    (is (char= #\c (char-at s 2 0)))))

(test alt-screen-no-crash
  "ESC[?1049h / ESC[?1049l (enter/exit alt screen) do not crash the emulator."
  (with-screen (s 10 5)
    (feed s "primary")
    ;; Enter alternate screen.
    (feed s (esc "[?1049h"))
    (feed s "alt")
    ;; Exit alternate screen.
    (feed s (esc "[?1049l"))
    ;; After exiting, the primary screen content should be accessible.
    ;; At minimum the emulator must still be in a consistent state.
    (is (integerp (screen-cursor-x s)))
    (is (integerp (screen-cursor-y s)))))

(test alt-screen-save-restore
  "Entering then exiting the alt screen restores the primary screen content."
  (with-screen (s 10 5)
    (feed s "hello")
    (feed s (esc "[?1049h"))  ; enter alt screen — primary grid saved
    (feed s "ALT")            ; mutate the (blank) alternate screen
    (feed s (esc "[?1049l"))  ; exit alt screen — primary grid restored
    (is (string= "hello" (row-string s 0 :end 5))
        "primary content not restored after alt-screen round-trip: ~S"
        (row-string s 0 :end 5))))

(test decsc-decrc
  "ESC 7 saves the cursor position and SGR state; ESC 8 restores them."
  (with-screen (s 20 5)
    (feed s (esc "[3;6H"))     ; cursor → (5, 2)
    (feed s (esc "[31;1m"))    ; fg = 1 (red), bold on
    (feed s (esc "7"))         ; DECSC — save
    (feed s (esc "[1;1H"))     ; cursor → (0, 0)
    (feed s (esc "[0m"))       ; reset SGR
    (feed s (esc "8"))         ; DECRC — restore
    (check-cursor s 5 2)
    (feed s "X")               ; written with the restored SGR
    (is (= 1 (fg-at s 5 2)) "DECRC must restore fg")
    (is (logbitp 0 (attrs-at s 5 2)) "DECRC must restore bold")))

(test decrc-without-save-homes-cursor
  "ESC 8 with no prior DECSC homes the cursor (VT100 default)."
  (with-screen (s 20 5)
    (feed s (esc "[3;6H"))
    (feed s (esc "8"))
    (check-cursor s 0 0)))

;;; ── SUITE: copy-mode scrollback projection ──────────────────────────────────

(def-suite copy-mode
  :description "Scrollback capture and copy-mode viewport projection"
  :in terminal-suite)
(in-suite copy-mode)

(defun feed-lines (screen &rest lines)
  "Feed LINES to SCREEN separated by CR/LF, scrolling as needed.  Returns SCREEN."
  (loop for (line . more) on lines
        do (feed screen line)
        when more do (feed screen (format nil "~C~C" #\Return #\Linefeed)))
  screen)

(defun display-row-string (screen y &key end)
  "Characters of viewport row Y via screen-display-cell (honors copy-offset)."
  (let ((end (or end (screen-width screen))))
    (with-output-to-string (s)
      (loop for x below end
            do (write-char (cell-char (screen-display-cell screen x y)) s)))))

(test scrollback-accumulates
  "Auto-scrolling a full screen pushes displaced top rows into the scrollback."
  (with-screen (s 5 3)
    (feed-lines s "L0" "L1" "L2" "L3" "L4")   ; 5 lines into a 3-row screen
    ;; Two scrolls happened, so the two oldest rows are in scrollback,
    ;; newest-first: L1 then L0.
    (is (= 2 (length (screen-scrollback s))))
    ;; Live grid now shows the most recent three lines.
    (is (string= "L2" (row-string s 0 :end 2)))
    (is (string= "L4" (row-string s 2 :end 2)))))

(test copy-offset-projects-history
  "screen-display-cell shifts the viewport into scrollback by copy-offset rows."
  (with-screen (s 5 3)
    (feed-lines s "L0" "L1" "L2" "L3" "L4")
    (setf (screen-copy-mode-p s) t)
    ;; Offset 0: viewport is the live grid unchanged.
    (setf (screen-copy-offset s) 0)
    (is (string= "L2" (display-row-string s 0 :end 2)))
    (is (string= "L4" (display-row-string s 2 :end 2)))
    ;; Offset 1: top row is newest scrollback line (L1); live grid pushed down.
    (setf (screen-copy-offset s) 1)
    (is (string= "L1" (display-row-string s 0 :end 2)))
    (is (string= "L2" (display-row-string s 1 :end 2)))
    (is (string= "L3" (display-row-string s 2 :end 2)))
    ;; Offset 2: the two scrollback lines (L0, L1) sit above the live top (L2).
    (setf (screen-copy-offset s) 2)
    (is (string= "L0" (display-row-string s 0 :end 2)))
    (is (string= "L1" (display-row-string s 1 :end 2)))
    (is (string= "L2" (display-row-string s 2 :end 2)))))

(test copy-mode-off-ignores-offset
  "A stale copy-offset is ignored entirely when copy mode is off."
  (with-screen (s 5 3)
    (feed-lines s "L0" "L1" "L2" "L3" "L4")
    (setf (screen-copy-mode-p s) nil
          (screen-copy-offset s) 2)  ; should have no effect
    (is (string= "L2" (display-row-string s 0 :end 2)))
    (is (string= "L4" (display-row-string s 2 :end 2)))))

;;; ── SUITE: scroll-region cursor clamping ────────────────────────────────────
;;;
;;; cl-tmux/terminal/actions::cursor-up/down clamp to the SCROLL REGION
;;; boundaries (scroll-top / scroll-bottom), which diverges from set-cursor
;;; (the CSI A/B handler), which clamps to the full screen (0 .. height-1).
;;; cursor-left/right clamp to column 0 / width-1.  These tests call the
;;; action functions directly, having first installed a non-trivial scroll
;;; region via the real slot accessors.

(def-suite scroll-region-clamp
  :description "Direct cursor-up/down/left/right clamp to scroll-region margins"
  :in terminal-suite)
(in-suite scroll-region-clamp)

(test cursor-up-clamps-to-scroll-top
  "cursor-up with a large count stops at scroll-top, NOT at row 0.
   This is the divergence from set-cursor, which would clamp to 0."
  (with-screen (s 10 10)
    ;; Scroll region = rows 3..7 (0-based inclusive).
    (setf (cl-tmux/terminal/types::screen-scroll-top s) 3
          (cl-tmux/terminal/types::screen-scroll-bottom s) 7)
    ;; Position cursor inside the region.
    (setf (cl-tmux/terminal/types::screen-cy s) 6)
    (cl-tmux/terminal/actions::cursor-up s 100)
    (is (= 3 (screen-cursor-y s))
        "cursor-up should clamp to scroll-top 3, got ~D" (screen-cursor-y s))
    ;; Sanity: it stopped at scroll-top, not at the screen top (0).
    (is (/= 0 (screen-cursor-y s))
        "cursor-up must not pass scroll-top down to row 0")))

(test cursor-down-clamps-to-scroll-bottom
  "cursor-down with a large count stops at scroll-bottom, NOT at height-1."
  (with-screen (s 10 10)
    (setf (cl-tmux/terminal/types::screen-scroll-top s) 3
          (cl-tmux/terminal/types::screen-scroll-bottom s) 7)
    (setf (cl-tmux/terminal/types::screen-cy s) 4)
    (cl-tmux/terminal/actions::cursor-down s 100)
    (is (= 7 (screen-cursor-y s))
        "cursor-down should clamp to scroll-bottom 7, got ~D" (screen-cursor-y s))
    ;; Sanity: it stopped at scroll-bottom, not at the screen bottom (9).
    (is (/= 9 (screen-cursor-y s))
        "cursor-down must not pass scroll-bottom down to height-1")))

(test cursor-left-clamps-to-column-zero
  "cursor-left with a large count clamps to column 0."
  (with-screen (s 10 10)
    (setf (cl-tmux/terminal/types::screen-cx s) 5)
    (cl-tmux/terminal/actions::cursor-left s 100)
    (is (= 0 (screen-cursor-x s))
        "cursor-left should clamp to column 0, got ~D" (screen-cursor-x s))))

(test cursor-right-clamps-to-width-minus-one
  "cursor-right with a large count clamps to width-1."
  (with-screen (s 10 10)
    (setf (cl-tmux/terminal/types::screen-cx s) 2)
    (cl-tmux/terminal/actions::cursor-right s 100)
    (is (= 9 (screen-cursor-x s))
        "cursor-right should clamp to width-1 (9), got ~D" (screen-cursor-x s))))

(test cursor-up-down-respect-region-from-mid
  "From a row inside the region, a small cursor-up/down stays within the
   region and does not overshoot the margins."
  (with-screen (s 10 10)
    (setf (cl-tmux/terminal/types::screen-scroll-top s) 2
          (cl-tmux/terminal/types::screen-scroll-bottom s) 8)
    (setf (cl-tmux/terminal/types::screen-cy s) 5)
    (cl-tmux/terminal/actions::cursor-up s 2)
    (is (= 3 (screen-cursor-y s)) "cursor-up 2 from row 5 → row 3")
    (cl-tmux/terminal/actions::cursor-down s 4)
    (is (= 7 (screen-cursor-y s)) "cursor-down 4 from row 3 → row 7")))

;;; ── SUITE: delete/insert characters (DCH / ICH) ─────────────────────────────
;;;
;;; Driven via the real CSI parser path: CSI n P (DCH) shifts the tail left
;;; and blanks the vacated end; CSI n @ (ICH) shifts the tail right and blanks
;;; the gap.  We also exercise the n >= width edge.

(def-suite delete-insert-chars
  :description "DCH (CSI P) and ICH (CSI @) via the CSI parser"
  :in terminal-suite)
(in-suite delete-insert-chars)

(test dch-shifts-left-and-blanks-tail
  "CSI 2 P at column 0 deletes 'ab' from 'abcde', shifting 'cde' left and
   blanking the two vacated cells at the end of the line."
  (with-screen (s 8 2)
    (feed s "abcde")
    (feed s (esc "[1;1H"))     ; cursor home (col 0, row 0)
    (feed s (csi "2" #\P))     ; DCH 2
    (is (char= #\c (char-at s 0 0)) "col0 should be 'c', got ~C" (char-at s 0 0))
    (is (char= #\d (char-at s 1 0)) "col1 should be 'd', got ~C" (char-at s 1 0))
    (is (char= #\e (char-at s 2 0)) "col2 should be 'e', got ~C" (char-at s 2 0))
    ;; The original 'cde' occupied cols 2..4; after a 2-shift the tail blanks.
    (is (char= #\Space (char-at s 3 0)) "col3 should be blank")
    (is (char= #\Space (char-at s 4 0)) "col4 should be blank")))

(test dch-at-midline
  "CSI 1 P at a non-zero column deletes one char and shifts the rest left."
  (with-screen (s 8 2)
    (feed s "abcde")
    (feed s (esc "[1;2H"))     ; cursor at col 1 (1-based col 2)
    (feed s (csi "1" #\P))     ; delete 'b'
    (is (string= "acde" (row-string s 0 :end 4))
        "expected 'acde', got ~S" (row-string s 0 :end 4))
    (is (char= #\Space (char-at s 4 0)) "vacated last cell should be blank")))

(test dch-default-param-deletes-one
  "CSI P with no parameter deletes a single character (p1* defaults to 1)."
  (with-screen (s 8 2)
    (feed s "abcde")
    (feed s (esc "[1;1H"))
    (feed s (csi "" #\P))      ; DCH default = 1
    (is (string= "bcde" (row-string s 0 :end 4))
        "expected 'bcde', got ~S" (row-string s 0 :end 4))))

(test dch-n-ge-width-clears-from-cursor
  "CSI n P with n >= remaining width blanks the whole line from the cursor.
   delete-chars caps the blank-fill at (max cx (- w n)); when n >= w the shift
   loop runs empty and every cell from cursor to end is blanked."
  (with-screen (s 5 2)
    (feed s "abcde")
    (feed s (esc "[1;1H"))     ; cursor at col 0
    (feed s (csi "9" #\P))     ; DCH 9 >= width 5
    (dotimes (x 5)
      (is (char= #\Space (char-at s x 0))
          "col ~D should be blank after oversized DCH, got ~C"
          x (char-at s x 0)))))

(test ich-shifts-right-and-blanks-gap
  "CSI 2 @ at column 0 inserts two blanks, pushing 'abcde' right; the trailing
   chars shifted past the right margin are lost."
  (with-screen (s 5 2)
    (feed s "abcde")
    (feed s (esc "[1;1H"))     ; cursor home
    (feed s (csi "2" #\@))     ; ICH 2
    ;; Two blanks inserted at cols 0,1; 'abc' shifts to cols 2,3,4; 'de' lost.
    (is (char= #\Space (char-at s 0 0)) "col0 should be blank gap")
    (is (char= #\Space (char-at s 1 0)) "col1 should be blank gap")
    (is (char= #\a (char-at s 2 0)) "col2 should be 'a', got ~C" (char-at s 2 0))
    (is (char= #\b (char-at s 3 0)) "col3 should be 'b', got ~C" (char-at s 3 0))
    (is (char= #\c (char-at s 4 0)) "col4 should be 'c', got ~C" (char-at s 4 0))))

(test ich-at-midline
  "CSI 1 @ at a non-zero column inserts one blank and pushes the tail right."
  (with-screen (s 6 2)
    (feed s "abcde")
    (feed s (esc "[1;3H"))     ; cursor at col 2 (1-based col 3)
    (feed s (csi "1" #\@))     ; insert one blank at col 2
    (is (char= #\a (char-at s 0 0)) "col0 unchanged 'a'")
    (is (char= #\b (char-at s 1 0)) "col1 unchanged 'b'")
    (is (char= #\Space (char-at s 2 0)) "col2 should be the inserted blank")
    (is (char= #\c (char-at s 3 0)) "col3 should be shifted 'c', got ~C"
        (char-at s 3 0))
    (is (char= #\d (char-at s 4 0)) "col4 should be shifted 'd', got ~C"
        (char-at s 4 0))))

(test ich-n-ge-width-blanks-from-cursor
  "CSI n @ with n >= remaining width blanks every cell from the cursor; the
   insert-chars blank-fill is capped at (min (1- w) (+ cx n -1))."
  (with-screen (s 5 2)
    (feed s "abcde")
    (feed s (esc "[1;1H"))
    (feed s (csi "9" #\@))     ; ICH 9 >= width 5
    (dotimes (x 5)
      (is (char= #\Space (char-at s x 0))
          "col ~D should be blank after oversized ICH, got ~C"
          x (char-at s x 0)))))
