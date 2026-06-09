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

(test dec-graphics-dash-variants
  "DEC graphics dash variants (o, p, r, s) all map to the horizontal line (─)."
  (check-dec-graphics #\o #\─ "o must map to horizontal line")
  (check-dec-graphics #\p #\─ "p must map to horizontal line")
  (check-dec-graphics #\r #\─ "r must map to horizontal line")
  (check-dec-graphics #\s #\─ "s must map to horizontal line"))

(test dec-graphics-unmapped-char-returned-unchanged
  "An unmapped character (not in the DEC special graphics set) is returned as-is."
  ;; 'z' is not in the DEC graphics mapping — it should pass through unchanged.
  (check-dec-graphics #\z #\z "unmapped 'z' must be returned unchanged")
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
