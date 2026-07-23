(in-package #:cl-tmux/test)

;;;; parser tests — part C3: direct CPS state functions and define-state.

;;; ── CPS parser state function tests (direct) ─────────────────────────────────

(describe "terminal-suite/direct-parser-cps-suite"

  ;; ground-state ─────────────────────────────────────────────────────────────────

  ;; ground-state processes a printable byte, writes the character, and returns ground-state.
  (it "ground-state-printable-writes-and-stays-ground"
    (with-screen (s 10 5)
      (let ((next (cl-tmux/terminal/parser:ground-state s 65))) ; 65 = #\A
        (expect (eq #'cl-tmux/terminal/parser:ground-state next))
        (expect (char= #\A (char-at s 0 0))))))

  ;; ground-state on ESC (#x1B) returns escape-state without writing a char.
  (it "ground-state-escape-returns-escape-state"
    (with-screen (s 10 5)
      (let ((next (cl-tmux/terminal/parser:ground-state s #x1B)))
        (expect (eq #'cl-tmux/terminal/parser:escape-state next))
        (expect (char= #\Space (char-at s 0 0))))))

  ;; escape-state ─────────────────────────────────────────────────────────────────

  ;; escape-state on #x5B ("[") returns a CSI accumulator continuation.
  (it "escape-state-bracket-returns-csi-k"
    (with-screen (s 10 5)
      (let ((next (cl-tmux/terminal/parser:escape-state s #x5B)))
        (expect (functionp next)))))

  ;; escape-state on #x63 ("c" = RIS) resets the screen and returns ground-state.
  (it "escape-state-c-returns-ground-and-resets"
    (with-screen (s 10 5)
      (feed s "hello")
      (let ((next (cl-tmux/terminal/parser:escape-state s #x63)))
        (expect (eq #'cl-tmux/terminal/parser:ground-state next))
        (expect (row-blank-p s 0)))))

  ;; charset designators (ESC ( / ESC ) ) + SO/SI locking shifts ──────────────────

  ;; A charset designator continuation consumes any designator byte and always
  ;; returns ground-state.
  (it "charset-designator-always-returns-ground-state"
    (with-screen (s 10 5)
      (expect (eq #'cl-tmux/terminal/parser:ground-state
              (funcall (cl-tmux/terminal/parser:make-charset-designator-k :g0) s 66)))  ; B = ASCII
      (expect (eq #'cl-tmux/terminal/parser:ground-state
              (funcall (cl-tmux/terminal/parser:make-charset-designator-k :g0) s 48))))) ; 0 = graphics

  ;; ESC ( 0 designates G0 to DEC graphics AND activates it (G0 is invoked by default).
  (it "esc-paren-0-designates-and-activates-g0-line-drawing"
    (with-screen (s 10 5)
      (feed s (format nil "~C(0" #\Escape))
      (expect (eq :dec-graphics (cl-tmux/terminal/types:screen-g0-charset s)))
      (expect (eq :dec-graphics (cl-tmux/terminal/types:screen-charset s)))))

  ;; ESC ) 0 designates G1 to DEC graphics but does NOT activate it (needs SO).
  (it "esc-close-paren-0-designates-g1-without-activating"
    (with-screen (s 10 5)
      (feed s (format nil "~C)0" #\Escape))
      (expect (eq :dec-graphics (cl-tmux/terminal/types:screen-g1-charset s)))
      (expect (eq :ascii (cl-tmux/terminal/types:screen-charset s)))))

  ;; SO (0x0E) invokes G1; SI (0x0F) invokes G0 (VT100 locking shifts).
  (it "so-invokes-g1-si-invokes-g0"
    (with-screen (s 10 5)
      (feed s (format nil "~C)0" #\Escape))            ; designate G1 = line-drawing
      (feed s (string (code-char #x0E)))               ; SO
      (expect (eq :g1 (cl-tmux/terminal/types:screen-active-g s)))
      (expect (eq :dec-graphics (cl-tmux/terminal/types:screen-charset s)))
      (feed s (string (code-char #x0F)))               ; SI
      (expect (eq :g0 (cl-tmux/terminal/types:screen-active-g s)))
      (expect (eq :ascii (cl-tmux/terminal/types:screen-charset s)))))

  ;; End-to-end: ESC ) 0, SO, 'q' renders the box-drawing horizontal line ─.
  (it "g1-line-drawing-via-so-remaps-characters"
    (with-screen (s 10 5)
      (feed s (format nil "~C)0~Cq" #\Escape (code-char #x0E)))  ; ESC ) 0, SO, 'q'
      (expect (string= "─" (row-string s 0 :end 1)))))

  ;; IND (ESC D) / NEL (ESC E) line control ──────────────────────────────────────

  ;; ESC D (IND) moves the cursor down one row, keeping the column (no carriage return).
  (it "esc-d-ind-moves-cursor-down-keeping-column"
    (with-screen (s 20 5)
      (feed s (esc "[3;5H"))   ; CUP → row 3, col 5 (cursor x=4, y=2)
      (feed s (esc "D"))       ; ESC D → IND
      (expect (= 4 (cl-tmux/terminal/types:screen-cursor-x s)))
      (expect (= 3 (cl-tmux/terminal/types:screen-cursor-y s)))))

  ;; ESC E (NEL) moves the cursor to column 0 of the next row (CR + LF).
  (it "esc-e-nel-moves-to-start-of-next-line"
    (with-screen (s 20 5)
      (feed s (esc "[3;5H"))   ; CUP → row 3, col 5
      (feed s (esc "E"))       ; ESC E → NEL
      (expect (= 0 (cl-tmux/terminal/types:screen-cursor-x s)))
      (expect (= 3 (cl-tmux/terminal/types:screen-cursor-y s)))))

  ;; osc-state ────────────────────────────────────────────────────────────────────

  ;; osc-state on BEL (#x07) returns ground-state (OSC terminated).
  (it "osc-state-bel-terminates-to-ground"
    (with-screen (s 10 5)
      (expect (eq #'cl-tmux/terminal/parser:ground-state
              (cl-tmux/terminal/parser:osc-state s #x07)))))

  ;; osc-state on non-terminator bytes returns a function (OSC payload accumulator).
  (it "osc-state-other-bytes-stay-in-osc"
    (with-screen (s 10 5)
      ;; The new accumulator-based implementation returns a closure (not #'osc-state)
      ;; that continues collecting OSC payload bytes.  We verify it is a FUNCTION
      ;; that will eventually transition to ground-state on BEL or ST.
      (let ((k65 (cl-tmux/terminal/parser:osc-state s 65)))   ; 'A'
        (expect (functionp k65)))
      (let ((k59 (cl-tmux/terminal/parser:osc-state s 59)))   ; ';'
        (expect (functionp k59))
        ;; Verify that sending BEL to the accumulator transitions to ground-state.
        (expect (eq #'cl-tmux/terminal/parser:ground-state
                (funcall k59 s #x07))))))

  ;; osc-st-state ─────────────────────────────────────────────────────────────────

  ;; osc-st-state returns ground-state on backslash (#x5C) and osc-state on any other byte.
  (it "osc-st-state-byte-dispatch"
    (with-screen (s 10 5)
      (expect (eq #'cl-tmux/terminal/parser:ground-state
              (cl-tmux/terminal/parser::osc-st-state s #x5C)))
      (expect (eq #'cl-tmux/terminal/parser:osc-state
              (cl-tmux/terminal/parser::osc-st-state s 65)))))

  ;; make-csi-k ───────────────────────────────────────────────────────────────────

  ;; make-csi-k closure collects digit bytes into params and dispatches on final byte.
  (it "make-csi-k-accumulates-digits-and-dispatches"
    (with-screen (s 10 5)
      ;; Build: CSI 31 m (SGR red foreground)
      ;; make-csi-k starts with empty params
      (let* ((k0 (cl-tmux/terminal/parser:make-csi-k))
             (k1 (funcall k0 s 51))   ; '3' = #x33
             (k2 (funcall k1 s 49))   ; '1' = #x31
             (result (funcall k2 s 109))) ; 'm' = #x6D = SGR final
        (expect (eq #'cl-tmux/terminal/parser:ground-state result))
        (expect (= 1 (cl-tmux/terminal/types:screen-cur-fg s))))))

  ;; A semicolon inside a CSI sequence separates parameters.
  (it "make-csi-k-semicolon-separates-params"
    (with-screen (s 10 5)
      ;; Build: CSI 1;31 m (bold + red foreground)
      (let* ((k0 (cl-tmux/terminal/parser:make-csi-k))
             (k1 (funcall k0 s 49))    ; '1'
             (k2 (funcall k1 s 59))    ; ';'
             (k3 (funcall k2 s 51))    ; '3'
             (k4 (funcall k3 s 49))    ; '1'
             (result (funcall k4 s 109))) ; 'm'
        (expect (eq #'cl-tmux/terminal/parser:ground-state result))
        (expect (= 1 (cl-tmux/terminal/types:screen-cur-fg s)))
        (expect (logbitp 0 (cl-tmux/terminal/types:screen-cur-attrs s))))))

  ;; make-csi-k on '?' (#x3F) stores #\? as the intermediate byte.
  (it "make-csi-k-dec-marker-question-sets-intermed"
    (with-screen (s 10 5)
      ;; Build CSI ? 25 h (DEC PM set 25 = show cursor)
      ;; First hide cursor so we can verify the set flips it.
      (setf (cl-tmux/terminal/types:screen-cursor-visible s) nil)
      (let* ((k0 (cl-tmux/terminal/parser:make-csi-k))
             (k1 (funcall k0 s #x3F))    ; '?'
             (k2 (funcall k1 s 50))      ; '2'
             (k3 (funcall k2 s 53))      ; '5'
             (result (funcall k3 s #x68))) ; 'h' = ?25h
        (expect (eq #'cl-tmux/terminal/parser:ground-state result))
        (expect (cl-tmux/terminal/types:screen-cursor-visible s)))))

  ;; make-csi-k on '>' (#x3E) stores #\> as the intermediate byte for secondary DA.
  (it "make-csi-k-sec-da-marker-sets-intermed"
    (with-screen (s 10 5)
      ;; Build CSI > c (DA2)
      (let* ((k0 (cl-tmux/terminal/parser:make-csi-k))
             (k1 (funcall k0 s #x3E))    ; '>'
             (result (funcall k1 s #x63))) ; 'c' = DA2
        (expect (eq #'cl-tmux/terminal/parser:ground-state result))
        ;; DA2 response must have been queued.
        (expect (consp (cl-tmux/terminal/types:screen-response-queue s))))))

  ;; make-csi-k on SPACE (#x20, an intermediate byte) stores #\Space.
  (it "make-csi-k-intermediate-byte-space-sets-intermed"
    (with-screen (s 10 5)
      ;; Build CSI 2 SP q (DECSCUSR: steady block = 2)
      ;; Use shape 2 (non-default) so the assertion is distinct from the default (1).
      (let* ((k0 (cl-tmux/terminal/parser:make-csi-k))
             (k1 (funcall k0 s 50))    ; '2' = param 2
             (k2 (funcall k1 s #x20))  ; SPACE = intermediate
             (result (funcall k2 s #x71))) ; 'q' = DECSCUSR final
        (expect (eq #'cl-tmux/terminal/parser:ground-state result))
        (expect (= 2 (cl-tmux/terminal/types:screen-cursor-shape s))))))

  ;; make-csi-k on a byte below #x40 (not a digit, semicolon, or marker) aborts to ground-state.
  (it "make-csi-k-non-final-invalid-byte-aborts-to-ground"
    (with-screen (s 10 5)
      ;; #x01 is below the CSI final range and not a recognised parameter byte.
      (let* ((k0 (cl-tmux/terminal/parser:make-csi-k))
             (result (funcall k0 s #x01)))
        (expect (eq #'cl-tmux/terminal/parser:ground-state result)))))

  ;; make-utf8-k ──────────────────────────────────────────────────────────────────

  ;; make-utf8-k with remaining=1 writes the character on the final continuation byte.
  (it "make-utf8-k-assembles-two-byte-sequence"
    (with-screen (s 10 5)
      ;; U+00E9 (é) = C3 A9 in UTF-8
      ;; Lead byte C3: acc = (C3 & 1F) = 3, remaining = 1
      (let* ((k0 (cl-tmux/terminal/parser:make-utf8-k 3 1))
             (result (funcall k0 s #xA9)))  ; continuation byte
        (expect (eq #'cl-tmux/terminal/parser:ground-state result))
        (expect (char= #\é (char-at s 0 0))))))

  ;; make-utf8-k with remaining=2 collects two continuation bytes.
  (it "make-utf8-k-assembles-three-byte-sequence"
    (with-screen (s 10 5)
      ;; U+3042 (あ) = E3 81 82 in UTF-8
      ;; Lead byte E3: acc = (E3 & 0F) = 3, remaining = 2
      (let* ((k0 (cl-tmux/terminal/parser:make-utf8-k 3 2))
             (k1 (funcall k0 s #x81))    ; first continuation byte
             (result (funcall k1 s #x82))) ; second continuation byte
        (expect (eq #'cl-tmux/terminal/parser:ground-state result))
        (expect (char= #\あ (char-at s 0 0))))))

  ;; make-utf8-k on a non-continuation byte emits U+FFFD and reprocesses the byte in ground-state.
  (it "make-utf8-k-malformed-non-continuation-emits-fffd"
    (with-screen (s 10 5)
      ;; Start a 2-byte sequence (remaining=1) then feed an ASCII byte (not a continuation).
      ;; The #\A byte (#x41) is not a continuation (0xC0 & 0x41 != 0x80), so:
      ;;   - U+FFFD is written at col 0
      ;;   - #\A is reprocessed in ground-state and written at col 1
      (let* ((k0 (cl-tmux/terminal/parser:make-utf8-k 2 1)))
        (funcall k0 s #x41))   ; ASCII 'A' — not a continuation byte
      (expect (char= (code-char #xFFFD) (char-at s 0 0)))
      (expect (char= #\A (char-at s 1 0)))))

  ;;; ── define-state macro ───────────────────────────────────────────────────────

  ;; define-state is a defined macro in the parser package.
  (it "define-state-macro-is-defined"
    (expect (macro-function 'cl-tmux/terminal/parser::define-state))))
