(in-package #:cl-tmux/test)

;;;; parser tests — part C: basic-text, parser-inline-predicates,
;;;; direct CPS state functions, define-state macro.

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

(test osc-st-state-byte-dispatch
  "osc-st-state returns ground-state on backslash (#x5C) and osc-state on any other byte."
  (with-screen (s 10 5)
    (is (eq #'cl-tmux/terminal/parser:ground-state
            (cl-tmux/terminal/parser::osc-st-state s #x5C))
        "backslash must return ground-state")
    (is (eq #'cl-tmux/terminal/parser:osc-state
            (cl-tmux/terminal/parser::osc-st-state s 65))
        "non-backslash must return osc-state")))

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

