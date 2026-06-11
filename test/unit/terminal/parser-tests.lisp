(in-package #:cl-tmux/test)

;;;; Parser tests (src/terminal/parser.lisp).
;;;; Tests: utf8, special, basic-text suites.

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

;;; ── SUITE: special ──────────────────────────────────────────────────────────
;;;
;;; Parser-level behaviour only: BEL, OSC, unknown CSI, DEC cursor-visibility.
;;; Mode/state tests (RIS, alt-screen, DECSC/DECRC) live in modes-tests.lisp.

(def-suite special
  :description "Parser behaviour: BEL, OSC, unknown CSI, DEC PM cursor visibility"
  :in terminal-suite)
(in-suite special)

(test bel-sets-bell-pending
  "BEL (byte #x07) sets screen-bell-pending to T without altering the screen or cursor."
  (with-screen (s 10 2)
    (feed s "ab")
    (is-false (cl-tmux/terminal/types:screen-bell-pending s)
              "bell-pending must be NIL before BEL")
    (screen-process-bytes s (make-array 1 :element-type '(unsigned-byte 8)
                                          :initial-contents '(#x07)))
    ;; Screen content and cursor must be unchanged.
    (is (char= #\a (char-at s 0 0)))
    (is (char= #\b (char-at s 1 0)))
    (check-cursor s 2 0)
    ;; bell-pending must now be set.
    (is (cl-tmux/terminal/types:screen-bell-pending s)
        "bell-pending must be T after BEL byte")))

(test osc-0-sets-screen-title
  "OSC 0 ; title BEL sets screen-title to the title string."
  (with-screen (s 20 5)
    (screen-process-bytes s
      (babel:string-to-octets
        (format nil "~C]0;my-window~C" #\Escape (code-char 7))
        :encoding :utf-8))
    (is (string= "my-window" (cl-tmux/terminal/types:screen-title s))
        "screen-title must be set to 'my-window' after OSC 0")))

(test osc-2-sets-screen-title
  "OSC 2 ; title BEL also sets screen-title (same as OSC 0)."
  (with-screen (s 20 5)
    (screen-process-bytes s
      (babel:string-to-octets
        (format nil "~C]2;xterm-title~C" #\Escape (code-char 7))
        :encoding :utf-8))
    (is (string= "xterm-title" (cl-tmux/terminal/types:screen-title s))
        "screen-title must be set to 'xterm-title' after OSC 2")))

(test osc-1-sets-screen-title
  "OSC 1 ; name BEL (icon name) also sets screen-title — cl-tmux keeps a single
   title, so OSC 0/1/2 all set it."
  (with-screen (s 20 5)
    (screen-process-bytes s
      (babel:string-to-octets
        (format nil "~C]1;icon-name~C" #\Escape (code-char 7))
        :encoding :utf-8))
    (is (string= "icon-name" (cl-tmux/terminal/types:screen-title s))
        "screen-title must be set to 'icon-name' after OSC 1")))

;;; ── OSC 10/11 dynamic colours ────────────────────────────────────────────────

(defun %feed-osc (s payload)
  "Feed an OSC sequence (ESC ] PAYLOAD ST) to screen S via screen-process-bytes."
  (screen-process-bytes s
    (babel:string-to-octets (format nil "~C]~A~C\\" #\Escape payload #\Escape)
                            :encoding :utf-8)))

(test parse-osc-color-forms
  "%parse-osc-color parses #RRGGBB, #RGB and rgb:R/G/B; rejects junk."
  (is (= #xFF8000 (cl-tmux/terminal/parser::%parse-osc-color "#ff8000")) "#RRGGBB")
  (is (= #xFF0000 (cl-tmux/terminal/parser::%parse-osc-color "#f00")) "#RGB expands (0xF→0xFF)")
  (is (= #xFF0000 (cl-tmux/terminal/parser::%parse-osc-color "rgb:ffff/0000/0000"))
      "rgb: with 16-bit channels scales down to 8-bit")
  (is (= #x00FF00 (cl-tmux/terminal/parser::%parse-osc-color "rgb:00/ff/00"))
      "rgb: with 8-bit channels")
  (is (null (cl-tmux/terminal/parser::%parse-osc-color "tomato")) "named colour → NIL")
  (is (null (cl-tmux/terminal/parser::%parse-osc-color "rgb:zz/00/00")) "bad hex → NIL"))

(test osc-11-query-reports-default-background
  "OSC 11 ; ? queries the default background; cl-tmux replies on the response-queue
   with the stored colour (black by default), so apps can detect a dark theme."
  (with-screen (s 20 5)
    (%feed-osc s "11;?")
    (let ((replies (cl-tmux/terminal/types:screen-response-queue s)))
      (is (= 1 (length replies)) "exactly one OSC 11 reply is enqueued")
      (is (string= (format nil "~C]11;rgb:0000/0000/0000~C\\" #\Escape #\Escape)
                   (first replies))
          "reply reports black background (got ~S)" (first replies)))))

(test osc-10-query-reports-default-foreground
  "OSC 10 ; ? reports the default foreground (white) as rgb:ffff/ffff/ffff."
  (with-screen (s 20 5)
    (%feed-osc s "10;?")
    (is (string= (format nil "~C]10;rgb:ffff/ffff/ffff~C\\" #\Escape #\Escape)
                 (first (cl-tmux/terminal/types:screen-response-queue s)))
        "OSC 10 query reports white foreground")))

(test osc-11-set-updates-default-background
  "OSC 11 ; rgb:ffff/0000/0000 sets the stored background to 0xFF0000 and replies
   to nothing (only queries reply)."
  (with-screen (s 20 5)
    (%feed-osc s "11;rgb:ffff/0000/0000")
    (is (= #xFF0000 (cl-tmux/terminal/types:screen-osc-default-bg s))
        "OSC 11 set updates screen-osc-default-bg to 0xFF0000")
    (is (null (cl-tmux/terminal/types:screen-response-queue s))
        "a SET must not enqueue a reply")))

(test osc-11-query-after-set-roundtrips
  "After OSC 11 sets the background, OSC 11 ; ? reports the new colour back."
  (with-screen (s 20 5)
    (%feed-osc s "11;#3366ff")
    (%feed-osc s "11;?")
    (is (string= (format nil "~C]11;rgb:3333/6666/ffff~C\\" #\Escape #\Escape)
                 (first (cl-tmux/terminal/types:screen-response-queue s)))
        "query after #3366ff reports rgb:3333/6666/ffff")))

(test osc-111-resets-default-background
  "OSC 111 (no parameter) resets the default background to black after a set —
   exercising the parameterless OSC dispatch path."
  (with-screen (s 20 5)
    (%feed-osc s "11;#ffffff")
    (is (= #xFFFFFF (cl-tmux/terminal/types:screen-osc-default-bg s)) "bg set to white")
    (%feed-osc s "111")
    (is (= #x000000 (cl-tmux/terminal/types:screen-osc-default-bg s))
        "OSC 111 resets the background to black")))

;;; ── OSC 4 palette queries ────────────────────────────────────────────────────

(test xterm-palette-rgb-values
  "%xterm-palette-rgb maps indices to the standard xterm 256-colour palette."
  (is (= #x000000 (cl-tmux/terminal/parser::%xterm-palette-rgb 0))   "index 0 = black")
  (is (= #xffffff (cl-tmux/terminal/parser::%xterm-palette-rgb 15))  "index 15 = white")
  (is (= #x000000 (cl-tmux/terminal/parser::%xterm-palette-rgb 16))  "cube origin = black")
  (is (= #x0000ff (cl-tmux/terminal/parser::%xterm-palette-rgb 21))  "index 21 = pure blue")
  (is (= #xff0000 (cl-tmux/terminal/parser::%xterm-palette-rgb 196)) "index 196 = pure red")
  (is (= #xffffff (cl-tmux/terminal/parser::%xterm-palette-rgb 231)) "cube max = white")
  (is (= #x080808 (cl-tmux/terminal/parser::%xterm-palette-rgb 232)) "grayscale ramp start")
  (is (= #xeeeeee (cl-tmux/terminal/parser::%xterm-palette-rgb 255)) "grayscale ramp end")
  (is (null (cl-tmux/terminal/parser::%xterm-palette-rgb 256)) "out of range → NIL"))

(test osc-4-query-reports-palette-colour
  "OSC 4 ; 196 ; ? reports palette index 196 as pure red (rgb:ffff/0000/0000)."
  (with-screen (s 20 5)
    (%feed-osc s "4;196;?")
    (is (string= (format nil "~C]4;196;rgb:ffff/0000/0000~C\\" #\Escape #\Escape)
                 (first (cl-tmux/terminal/types:screen-response-queue s)))
        "OSC 4 query must report index 196 as red")))

(test osc-4-query-multiple-indices
  "OSC 4 ; 0 ; ? ; 15 ; ? enqueues one reply per queried index."
  (with-screen (s 20 5)
    (%feed-osc s "4;0;?;15;?")
    (is (= 2 (length (cl-tmux/terminal/types:screen-response-queue s)))
        "two OSC 4 queries must enqueue two replies")))

(test osc-4-set-does-not-reply
  "OSC 4 ; 1 ; rgb:... (a SET, not a query) enqueues no reply (set is ignored)."
  (with-screen (s 20 5)
    (%feed-osc s "4;1;rgb:ffff/0000/ffff")
    (is (null (cl-tmux/terminal/types:screen-response-queue s))
        "an OSC 4 SET must not enqueue a reply")))

;;; ── OSC 8 hyperlinks ─────────────────────────────────────────────────────────

(test osc-8-stamps-cell-hyperlink
  "OSC 8 ; ; URI sets the current hyperlink so the next written cell carries it;
   OSC 8 ; ; clears it (the following cell has no hyperlink)."
  (with-screen (s 20 5)
    (%feed-osc s "8;;https://example.com")
    (feed s "X")
    (%feed-osc s "8;;")        ; clear the hyperlink
    (feed s "Y")
    (is (string= "https://example.com"
                 (cl-tmux/terminal/types:cell-hyperlink
                  (cl-tmux/terminal/types:screen-cell s 0 0)))
        "the cell written under OSC 8 must carry the hyperlink")
    (is (null (cl-tmux/terminal/types:cell-hyperlink
               (cl-tmux/terminal/types:screen-cell s 1 0)))
        "the cell after OSC 8 ; ; must have no hyperlink")))

(test osc-bel-no-crash
  "An OSC sequence terminated by BEL is consumed without crashing."
  (with-screen (s 10 2)
    (feed s "a")
    ;; OSC 0 ; title BEL -- common in xterm
    (screen-process-bytes s
      (babel:string-to-octets
        (format nil "~C]0;window title~C" #\Escape (code-char 7))
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

(test csi-private-lt-marker-consumed-not-stray
  "CSI < t (XTPOPTITLE) and CSI = c (DA3) use the < / = private markers; the byte
   must route to the marker slot, not abort the sequence and print the final byte
   as a stray char."
  (with-screen (s 10 2)
    (feed s "a")
    (feed s (esc "[<t"))       ; XTPOPTITLE — pop title (no-op), prints nothing
    (feed s "b")
    (is (char= #\a (char-at s 0 0)) "'a' at column 0")
    (is (char= #\b (char-at s 1 0))
        "'b' at column 1 — no stray 't' printed between them")))

(test esc-hash-8-decaln-fills-screen-with-e
  "ESC # 8 (DECALN) fills the entire screen with 'E' (the VT100 alignment test)."
  (with-screen (s 4 2)
    (feed s (esc "#8"))
    (dotimes (y 2)
      (dotimes (x 4)
        (is (char= #\E (char-at s x y))
            "cell (~D,~D) must be 'E' after DECALN" x y)))))

(test esc-hash-selector-consumed-not-stray
  "ESC # <selector> consumes the selector byte; ESC # 5 (DECSWL, no-op) prints
   nothing — the byte must not abort the sequence and print as a stray char."
  (with-screen (s 10 2)
    (feed s "a")
    (feed s (esc "#5"))        ; DECSWL — single-width line, no-op
    (feed s "b")
    (is (char= #\a (char-at s 0 0)) "'a' at column 0")
    (is (char= #\b (char-at s 1 0))
        "'b' at column 1 — no stray '5' printed between them")))

(test esc-star-plus-g2-g3-designator-consumed-not-stray
  "ESC * X (designate G2) and ESC + X (designate G3) consume the designator byte
   without printing it as a stray char (G2/G3 accepted but not modeled)."
  (with-screen (s 10 2)
    (feed s "a")
    (feed s (esc "*0"))        ; designate G2 = DEC graphics (consumes '0')
    (feed s (esc "+B"))        ; designate G3 = ASCII (consumes 'B')
    (feed s "b")
    (is (char= #\a (char-at s 0 0)) "'a' at column 0")
    (is (char= #\b (char-at s 1 0))
        "'b' at column 1 — no stray '0' or 'B' from the G2/G3 designators")))

(test esc-space-and-percent-two-byte-seqs-consumed-not-stray
  "ESC SP F (S7C1T) and ESC % G (select UTF-8) consume their trailing byte without
   printing it as a stray char."
  (with-screen (s 10 2)
    (feed s "a")
    (feed s (esc " F"))        ; ESC SP F — S7C1T (consumes 'F')
    (feed s (esc "%G"))        ; ESC % G — select UTF-8 (consumes 'G')
    (feed s "b")
    (is (char= #\a (char-at s 0 0)) "'a' at column 0")
    (is (char= #\b (char-at s 1 0))
        "'b' at column 1 — no stray 'F'/'G' from the two-byte ESC sequences")))

(test csi-unknown
  "An unrecognised CSI final character is silently ignored; parser recovers."
  (with-screen (s 10 2)
    (feed s "a")
    ;; ESC [ z  -- 'z' is not a standard CSI final
    (feed s (esc "[z"))
    (feed s "b")
    (is (char= #\a (char-at s 0 0)))
    (is (char= #\b (char-at s 1 0)))))

(test dec-pm-hide-show-cursor
  "ESC[?25l (hide cursor) and ESC[?25h (show cursor) do not crash."
  (with-screen (s 10 2)
    (feed s "a")
    (feed s (esc "[?25l"))    ; hide cursor -- accepted silently
    (feed s "b")
    (feed s (esc "[?25h"))    ; show cursor -- accepted silently
    (feed s "c")
    ;; All three characters must be on screen.
    (is (char= #\a (char-at s 0 0)))
    (is (char= #\b (char-at s 1 0)))
    (is (char= #\c (char-at s 2 0)))))

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

;;; ── SUITE: parser-inline-predicates ─────────────────────────────────────────
;;;
;;; These tests call the inline predicate helpers in cl-tmux/terminal/parser
;;; directly, verifying boundary conditions that the parser integration tests
;;; do not assert explicitly.

(def-suite parser-inline-predicates
  :description "Direct tests of printable-ascii-p, utf8-lead-p, utf8-continuation-p, utf8-lead-decode"
  :in terminal-suite)
(in-suite parser-inline-predicates)

(test printable-ascii-p-range
  "printable-ascii-p is T for #x20-#x7E and NIL outside that range."
  (is-true  (cl-tmux/terminal/parser::printable-ascii-p #x20))
  (is-true  (cl-tmux/terminal/parser::printable-ascii-p #x41)) ; A
  (is-true  (cl-tmux/terminal/parser::printable-ascii-p #x7E))
  (is-false (cl-tmux/terminal/parser::printable-ascii-p #x1F))
  (is-false (cl-tmux/terminal/parser::printable-ascii-p #x7F)))

(test utf8-lead-p-identifies-lead-bytes
  "utf8-lead-p is T for #xC0-#xFE and NIL for ASCII or continuation bytes."
  (is-true  (cl-tmux/terminal/parser::utf8-lead-p #xC2))
  (is-true  (cl-tmux/terminal/parser::utf8-lead-p #xE3))
  (is-true  (cl-tmux/terminal/parser::utf8-lead-p #xF0))
  (is-false (cl-tmux/terminal/parser::utf8-lead-p #x41))  ; ASCII A
  (is-false (cl-tmux/terminal/parser::utf8-lead-p #x80))  ; continuation
  (is-false (cl-tmux/terminal/parser::utf8-lead-p #xFF))) ; excluded

(test utf8-continuation-p-identifies-continuation-bytes
  "utf8-continuation-p is T for #x80-#xBF."
  (is-true  (cl-tmux/terminal/parser::utf8-continuation-p #x80))
  (is-true  (cl-tmux/terminal/parser::utf8-continuation-p #xBF))
  (is-false (cl-tmux/terminal/parser::utf8-continuation-p #x41))
  (is-false (cl-tmux/terminal/parser::utf8-continuation-p #xC0)))

(test utf8-lead-decode-returns-initial-accumulators
  "utf8-lead-decode gives (acc, remaining-bytes) for 2/3/4-byte sequences."
  (multiple-value-bind (acc left) (cl-tmux/terminal/parser::utf8-lead-decode #xC2)
    (is (= 2 acc)  "C2: acc should be 2 (low 5 bits of #xC2)")
    (is (= 1 left) "C2: 1 continuation byte expected"))
  (multiple-value-bind (acc left) (cl-tmux/terminal/parser::utf8-lead-decode #xE3)
    (is (= 3 acc)  "E3: acc should be 3 (low 4 bits of #xE3)")
    (is (= 2 left) "E3: 2 continuation bytes expected"))
  (multiple-value-bind (acc left) (cl-tmux/terminal/parser::utf8-lead-decode #xF0)
    (is (= 0 acc)  "F0: acc should be 0 (low 3 bits of #xF0)")
    (is (= 3 left) "F0: 3 continuation bytes expected")))

;;; ── CPS parser state function tests (direct) ─────────────────────────────────

(def-suite direct-parser-cps-suite
  :description "Direct calls to CPS parser state functions"
  :in terminal-suite)
(in-suite direct-parser-cps-suite)

;; ground-state ─────────────────────────────────────────────────────────────────

(test ground-state-printable-writes-and-stays-ground
  "ground-state processes a printable byte, writes the character, and returns ground-state."
  (with-screen (s 10 5)
    (let ((next (cl-tmux/terminal/parser:ground-state s 65))) ; 65 = #\A
      (is (eq #'cl-tmux/terminal/parser:ground-state next)
          "ground-state must return ground-state for printable ASCII")
      (is (char= #\A (char-at s 0 0))
          "character must be written to the screen"))))

(test ground-state-escape-returns-escape-state
  "ground-state on ESC (#x1B) returns escape-state without writing a char."
  (with-screen (s 10 5)
    (let ((next (cl-tmux/terminal/parser:ground-state s #x1B)))
      (is (eq #'cl-tmux/terminal/parser:escape-state next)
          "ground-state must return escape-state on ESC byte")
      (is (char= #\Space (char-at s 0 0))
          "ESC must not write a visible character"))))

;; escape-state ─────────────────────────────────────────────────────────────────

(test escape-state-bracket-returns-csi-k
  "escape-state on #x5B (\"[\") returns a CSI accumulator continuation."
  (with-screen (s 10 5)
    (let ((next (cl-tmux/terminal/parser:escape-state s #x5B)))
      (is (functionp next) "ESC [ must return a CSI continuation function (not a named state)"))))

(test escape-state-c-returns-ground-and-resets
  "escape-state on #x63 (\"c\" = RIS) resets the screen and returns ground-state."
  (with-screen (s 10 5)
    (feed s "hello")
    (let ((next (cl-tmux/terminal/parser:escape-state s #x63)))
      (is (eq #'cl-tmux/terminal/parser:ground-state next))
      (is (row-blank-p s 0) "RIS via escape-state must clear the screen"))))

;; charset designators (ESC ( / ESC ) ) + SO/SI locking shifts ──────────────────

(test charset-designator-always-returns-ground-state
  "A charset designator continuation consumes any designator byte and always
   returns ground-state."
  (with-screen (s 10 5)
    (is (eq #'cl-tmux/terminal/parser:ground-state
            (funcall (cl-tmux/terminal/parser:make-charset-designator-k :g0) s 66)))  ; B = ASCII
    (is (eq #'cl-tmux/terminal/parser:ground-state
            (funcall (cl-tmux/terminal/parser:make-charset-designator-k :g0) s 48))))) ; 0 = graphics

(test esc-paren-0-designates-and-activates-g0-line-drawing
  "ESC ( 0 designates G0 to DEC graphics AND activates it (G0 is invoked by default)."
  (with-screen (s 10 5)
    (feed s (format nil "~C(0" #\Escape))
    (is (eq :dec-graphics (cl-tmux/terminal/types:screen-g0-charset s))
        "ESC ( 0 must designate G0 to :dec-graphics")
    (is (eq :dec-graphics (cl-tmux/terminal/types:screen-charset s))
        "ESC ( 0 must activate line-drawing (G0 is the active set)")))

(test esc-close-paren-0-designates-g1-without-activating
  "ESC ) 0 designates G1 to DEC graphics but does NOT activate it (needs SO)."
  (with-screen (s 10 5)
    (feed s (format nil "~C)0" #\Escape))
    (is (eq :dec-graphics (cl-tmux/terminal/types:screen-g1-charset s))
        "ESC ) 0 must designate G1 to :dec-graphics")
    (is (eq :ascii (cl-tmux/terminal/types:screen-charset s))
        "ESC ) 0 must NOT activate line-drawing until a SO locking shift")))

(test so-invokes-g1-si-invokes-g0
  "SO (0x0E) invokes G1; SI (0x0F) invokes G0 (VT100 locking shifts)."
  (with-screen (s 10 5)
    (feed s (format nil "~C)0" #\Escape))            ; designate G1 = line-drawing
    (feed s (string (code-char #x0E)))               ; SO
    (is (eq :g1 (cl-tmux/terminal/types:screen-active-g s)) "SO must invoke G1")
    (is (eq :dec-graphics (cl-tmux/terminal/types:screen-charset s))
        "SO must activate G1's line-drawing charset")
    (feed s (string (code-char #x0F)))               ; SI
    (is (eq :g0 (cl-tmux/terminal/types:screen-active-g s)) "SI must invoke G0")
    (is (eq :ascii (cl-tmux/terminal/types:screen-charset s))
        "SI must restore G0's ASCII charset")))

(test g1-line-drawing-via-so-remaps-characters
  "End-to-end: ESC ) 0, SO, 'q' renders the box-drawing horizontal line ─."
  (with-screen (s 10 5)
    (feed s (format nil "~C)0~Cq" #\Escape (code-char #x0E)))  ; ESC ) 0, SO, 'q'
    (is (string= "─" (row-string s 0 :end 1))
        "Under invoked G1 line-drawing, 'q' must render as ─")))

;; IND (ESC D) / NEL (ESC E) line control ──────────────────────────────────────

(test esc-d-ind-moves-cursor-down-keeping-column
  "ESC D (IND) moves the cursor down one row, keeping the column (no carriage return)."
  (with-screen (s 20 5)
    (feed s (esc "[3;5H"))   ; CUP → row 3, col 5 (cursor x=4, y=2)
    (feed s (esc "D"))       ; ESC D → IND
    (is (= 4 (cl-tmux/terminal/types:screen-cursor-x s))
        "IND must keep the column (no CR)")
    (is (= 3 (cl-tmux/terminal/types:screen-cursor-y s))
        "IND must move the cursor down one row")))

(test esc-e-nel-moves-to-start-of-next-line
  "ESC E (NEL) moves the cursor to column 0 of the next row (CR + LF)."
  (with-screen (s 20 5)
    (feed s (esc "[3;5H"))   ; CUP → row 3, col 5
    (feed s (esc "E"))       ; ESC E → NEL
    (is (= 0 (cl-tmux/terminal/types:screen-cursor-x s))
        "NEL must move the cursor to column 0")
    (is (= 3 (cl-tmux/terminal/types:screen-cursor-y s))
        "NEL must move the cursor down one row")))

;; osc-state ────────────────────────────────────────────────────────────────────

(test osc-state-bel-terminates-to-ground
  "osc-state on BEL (#x07) returns ground-state (OSC terminated)."
  (with-screen (s 10 5)
    (is (eq #'cl-tmux/terminal/parser:ground-state
            (cl-tmux/terminal/parser:osc-state s #x07)))))

(test osc-state-other-bytes-stay-in-osc
  "osc-state on non-terminator bytes returns a function (OSC payload accumulator)."
  (with-screen (s 10 5)
    ;; The new accumulator-based implementation returns a closure (not #'osc-state)
    ;; that continues collecting OSC payload bytes.  We verify it is a FUNCTION
    ;; that will eventually transition to ground-state on BEL or ST.
    (let ((k65 (cl-tmux/terminal/parser:osc-state s 65)))   ; 'A'
      (is (functionp k65) "osc-state on 'A' must return a function"))
    (let ((k59 (cl-tmux/terminal/parser:osc-state s 59)))   ; ';'
      (is (functionp k59) "osc-state on ';' must return a function")
      ;; Verify that sending BEL to the accumulator transitions to ground-state.
      (is (eq #'cl-tmux/terminal/parser:ground-state
              (funcall k59 s #x07))
          "accumulator on BEL must return ground-state"))))

;; osc-st-state ─────────────────────────────────────────────────────────────────

(test osc-st-state-backslash-returns-ground
  "osc-st-state on #x5C (backslash = ST) returns ground-state."
  (with-screen (s 10 5)
    (is (eq #'cl-tmux/terminal/parser:ground-state
            (cl-tmux/terminal/parser::osc-st-state s #x5C)))))

(test osc-st-state-non-backslash-returns-to-osc
  "osc-st-state on a non-backslash byte returns osc-state (ST not confirmed)."
  (with-screen (s 10 5)
    (is (eq #'cl-tmux/terminal/parser:osc-state
            (cl-tmux/terminal/parser::osc-st-state s 65)))))

;; make-csi-k ───────────────────────────────────────────────────────────────────

(test make-csi-k-accumulates-digits-and-dispatches
  "make-csi-k closure collects digit bytes into params and dispatches on final byte."
  (with-screen (s 10 5)
    ;; Build: CSI 31 m (SGR red foreground)
    ;; make-csi-k starts with empty params
    (let* ((k0 (cl-tmux/terminal/parser:make-csi-k))
           (k1 (funcall k0 s 51))   ; '3' = #x33
           (k2 (funcall k1 s 49))   ; '1' = #x31
           (result (funcall k2 s 109))) ; 'm' = #x6D = SGR final
      (is (eq #'cl-tmux/terminal/parser:ground-state result)
          "make-csi-k must return ground-state after dispatching the final byte")
      (is (= 1 (cl-tmux/terminal/types:screen-cur-fg s))
          "SGR 31 dispatched from make-csi-k must set fg to 1 (red)"))))

(test make-csi-k-semicolon-separates-params
  "A semicolon inside a CSI sequence separates parameters."
  (with-screen (s 10 5)
    ;; Build: CSI 1;31 m (bold + red foreground)
    (let* ((k0 (cl-tmux/terminal/parser:make-csi-k))
           (k1 (funcall k0 s 49))    ; '1'
           (k2 (funcall k1 s 59))    ; ';'
           (k3 (funcall k2 s 51))    ; '3'
           (k4 (funcall k3 s 49))    ; '1'
           (result (funcall k4 s 109))) ; 'm'
      (is (eq #'cl-tmux/terminal/parser:ground-state result))
      (is (= 1 (cl-tmux/terminal/types:screen-cur-fg s))
          "fg must be 1 (red) after CSI 1;31 m")
      (is (logbitp 0 (cl-tmux/terminal/types:screen-cur-attrs s))
          "bold bit must be set after CSI 1;31 m"))))

(test make-csi-k-dec-marker-question-sets-intermed
  "make-csi-k on '?' (#x3F) stores #\\? as the intermediate byte."
  (with-screen (s 10 5)
    ;; Build CSI ? 25 h (DEC PM set 25 = show cursor)
    ;; First hide cursor so we can verify the set flips it.
    (setf (cl-tmux/terminal/types:screen-cursor-visible s) nil)
    (let* ((k0 (cl-tmux/terminal/parser:make-csi-k))
           (k1 (funcall k0 s #x3F))    ; '?'
           (k2 (funcall k1 s 50))      ; '2'
           (k3 (funcall k2 s 53))      ; '5'
           (result (funcall k3 s #x68))) ; 'h' = ?25h
      (is (eq #'cl-tmux/terminal/parser:ground-state result)
          "CSI ?25h must dispatch and return ground-state")
      (is (cl-tmux/terminal/types:screen-cursor-visible s)
          "?25h must set cursor-visible to T"))))

(test make-csi-k-sec-da-marker-sets-intermed
  "make-csi-k on '>' (#x3E) stores #\\> as the intermediate byte for secondary DA."
  (with-screen (s 10 5)
    ;; Build CSI > c (DA2)
    (let* ((k0 (cl-tmux/terminal/parser:make-csi-k))
           (k1 (funcall k0 s #x3E))    ; '>'
           (result (funcall k1 s #x63))) ; 'c' = DA2
      (is (eq #'cl-tmux/terminal/parser:ground-state result)
          "CSI >c must dispatch and return ground-state")
      ;; DA2 response must have been queued.
      (is (consp (cl-tmux/terminal/types:screen-response-queue s))
          "CSI >c must enqueue a DA2 response"))))

(test make-csi-k-intermediate-byte-space-sets-intermed
  "make-csi-k on SPACE (#x20, an intermediate byte) stores #\\Space."
  (with-screen (s 10 5)
    ;; Build CSI 2 SP q (DECSCUSR: steady block = 2)
    ;; Use shape 2 (non-default) so the assertion is distinct from the default (1).
    (let* ((k0 (cl-tmux/terminal/parser:make-csi-k))
           (k1 (funcall k0 s 50))    ; '2' = param 2
           (k2 (funcall k1 s #x20))  ; SPACE = intermediate
           (result (funcall k2 s #x71))) ; 'q' = DECSCUSR final
      (is (eq #'cl-tmux/terminal/parser:ground-state result)
          "CSI 2 SP q must dispatch and return ground-state")
      (is (= 2 (cl-tmux/terminal/types:screen-cursor-shape s))
          "DECSCUSR CSI 2 SP q must set cursor-shape to 2 (steady block)"))))

(test make-csi-k-non-final-invalid-byte-aborts-to-ground
  "make-csi-k on a byte below #x40 (not a digit, semicolon, or marker) aborts to ground-state."
  (with-screen (s 10 5)
    ;; #x01 is below the CSI final range and not a recognised parameter byte.
    (let* ((k0 (cl-tmux/terminal/parser:make-csi-k))
           (result (funcall k0 s #x01)))
      (is (eq #'cl-tmux/terminal/parser:ground-state result)
          "invalid byte inside CSI must abort to ground-state"))))

;; make-utf8-k ──────────────────────────────────────────────────────────────────

(test make-utf8-k-assembles-two-byte-sequence
  "make-utf8-k with remaining=1 writes the character on the final continuation byte."
  (with-screen (s 10 5)
    ;; U+00E9 (é) = C3 A9 in UTF-8
    ;; Lead byte C3: acc = (C3 & 1F) = 3, remaining = 1
    (let* ((k0 (cl-tmux/terminal/parser:make-utf8-k 3 1))
           (result (funcall k0 s #xA9)))  ; continuation byte
      (is (eq #'cl-tmux/terminal/parser:ground-state result)
          "must return ground-state after the final continuation byte")
      (is (char= #\é (char-at s 0 0))
          "U+00E9 (é) must be written to the screen"))))

(test make-utf8-k-assembles-three-byte-sequence
  "make-utf8-k with remaining=2 collects two continuation bytes."
  (with-screen (s 10 5)
    ;; U+3042 (あ) = E3 81 82 in UTF-8
    ;; Lead byte E3: acc = (E3 & 0F) = 3, remaining = 2
    (let* ((k0 (cl-tmux/terminal/parser:make-utf8-k 3 2))
           (k1 (funcall k0 s #x81))    ; first continuation byte
           (result (funcall k1 s #x82))) ; second continuation byte
      (is (eq #'cl-tmux/terminal/parser:ground-state result))
      (is (char= #\あ (char-at s 0 0))
          "U+3042 (あ) must be written after two continuation bytes"))))

(test make-utf8-k-malformed-non-continuation-emits-fffd
  "make-utf8-k on a non-continuation byte emits U+FFFD and reprocesses the byte in ground-state."
  (with-screen (s 10 5)
    ;; Start a 2-byte sequence (remaining=1) then feed an ASCII byte (not a continuation).
    ;; The #\A byte (#x41) is not a continuation (0xC0 & 0x41 != 0x80), so:
    ;;   - U+FFFD is written at col 0
    ;;   - #\A is reprocessed in ground-state and written at col 1
    (let* ((k0 (cl-tmux/terminal/parser:make-utf8-k 2 1)))
      (funcall k0 s #x41))   ; ASCII 'A' — not a continuation byte
    (is (char= (code-char #xFFFD) (char-at s 0 0))
        "malformed UTF-8 must emit U+FFFD at col 0")
    (is (char= #\A (char-at s 1 0))
        "reprocessed ASCII byte must be written at col 1")))

;;; ── define-state macro ───────────────────────────────────────────────────────

(test define-state-macro-is-defined
  "define-state is a defined macro in the parser package."
  (is (macro-function 'cl-tmux/terminal/parser::define-state)
      "define-state must be a macro"))

