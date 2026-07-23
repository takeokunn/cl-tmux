(in-package #:cl-tmux/test)

;;;; parser tests — part B: combining-chars, ACS line-drawing, DCS passthrough,
;;;; XTGETTCAP, and DECRQSS helpers.

;;; ── SUITE: combining-chars ───────────────────────────────────────────────────

(describe "terminal-suite/combining-chars"

  ;; combining-char-p is T for code points in combining ranges, NIL otherwise.
  (it "combining-char-predicate-ranges"
    (expect (cl-tmux/terminal/actions:combining-char-p (code-char #x0300)) :to-be-truthy)
    (expect (cl-tmux/terminal/actions:combining-char-p (code-char #x036F)) :to-be-truthy)
    (expect (cl-tmux/terminal/actions:combining-char-p #\a) :to-be-falsy)
    (expect (cl-tmux/terminal/actions:combining-char-p #\Space) :to-be-falsy))

  ;; A combining character appended after a base char is stored in the previous
  ;; cell's combining list without advancing the cursor.
  (it "combining-char-appended-to-cell"
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
          (expect (member (code-char #x0301) (cl-tmux/terminal/types:cell-combining cell))))))))

;;; ── SUITE: acs-line-drawing ──────────────────────────────────────────────────

;;; ── Coverage: %dec-graphics-char corner cases ────────────────────────────────
;;;
;;; The existing tests cover only 'q' and 'x'.  These tests add coverage for
;;; the corner characters, junctions, the catch-all unmapped branch, and the
;;; macro itself (define-dec-graphics-table).

(defmacro check-dec-graphics (char expected-char description)
  "Assert that %dec-graphics-char maps CHAR to EXPECTED-CHAR with DESCRIPTION."
  (declare (ignore description))
  `(expect (char= ,expected-char (cl-tmux/terminal/actions::%dec-graphics-char ,char))))

(describe "terminal-suite/acs-line-drawing"

  ;; ESC ( 0 switches to DEC graphics; ESC ( B switches back to ASCII.
  (it "acs-charset-switch"
    (with-screen (s 20 5)
      (expect (eq :ascii (cl-tmux/terminal/types:screen-charset s)))
      ;; ESC ( 0
      (feed s (format nil "~C(0" #\Escape))
      (expect (eq :dec-graphics (cl-tmux/terminal/types:screen-charset s)))
      ;; ESC ( B
      (feed s (format nil "~C(B" #\Escape))
      (expect (eq :ascii (cl-tmux/terminal/types:screen-charset s)))))

  ;; In DEC graphics mode, each ASCII char maps to the correct box-drawing Unicode codepoint.
  (it "acs-line-drawing-maps-chars-table"
    (dolist (row '(("q" #\─ "DEC graphics 'q' → ─ (U+2500)")
                   ("x" #\│ "DEC graphics 'x' → │ (U+2502)")))
      (destructuring-bind (input expected desc) row
        (declare (ignore desc))
        (with-screen (s 20 5)
          (feed s (format nil "~C(0" #\Escape))
          (feed s input)
          (expect (char= expected (char-at s 0 0)))))))

  ;; In ASCII mode (default), 'q' writes literal 'q'.
  (it "acs-ascii-mode-unaffected"
    (with-screen (s 20 5)
      ;; Ensure we are in ASCII mode
      (feed s (format nil "~C(B" #\Escape))
      (feed s "q")
      (expect (char= #\q (char-at s 0 0)))))

  ;; DEC graphics corner and junction characters map to the correct box-drawing codepoints.
  (it "dec-graphics-corner-characters"
    (check-dec-graphics #\j #\┘ "j must map to lower-right corner (┘)")
    (check-dec-graphics #\k #\┐ "k must map to upper-right corner (┐)")
    (check-dec-graphics #\l #\┌ "l must map to upper-left corner (┌)")
    (check-dec-graphics #\m #\└ "m must map to lower-left corner (└)")
    (check-dec-graphics #\n #\┼ "n must map to crossing (┼)")
    (check-dec-graphics #\t #\├ "t must map to left tee (├)")
    (check-dec-graphics #\u #\┤ "u must map to right tee (┤)")
    (check-dec-graphics #\v #\┴ "v must map to bottom tee (┴)")
    (check-dec-graphics #\w #\┬ "w must map to top tee (┬)"))

  ;; DEC graphics special characters map to the correct Unicode codepoints.
  (it "dec-graphics-special-characters"
    (check-dec-graphics #\a #\▒ "a must map to checkerboard (▒)")
    (check-dec-graphics #\` #\◆ "` must map to diamond (◆)")
    (check-dec-graphics #\f #\° "f must map to degree symbol (°)")
    (check-dec-graphics #\g #\± "g must map to plus-minus (±)"))

  ;; DEC graphics horizontal scan lines (o,p,q,r,s) map to their distinct vertical
  ;; positions: q (scan line 5) is the box-drawing horizontal; o/p sit above, r/s below.
  (it "dec-graphics-scan-lines"
    (check-dec-graphics #\o #\⎺ "o must map to scan line 1 (top)")
    (check-dec-graphics #\p #\⎻ "p must map to scan line 3")
    (check-dec-graphics #\q #\─ "q must map to scan line 5 (horizontal line)")
    (check-dec-graphics #\r #\⎼ "r must map to scan line 7")
    (check-dec-graphics #\s #\⎽ "s must map to scan line 9 (bottom)"))

  ;; Upper half of the DEC special-graphics set: relational/math symbols + others
  ;; (previously passed through literally, breaking apps that emit them).
  (it "dec-graphics-math-and-symbol-characters"
    (check-dec-graphics #\y #\≤ "y must map to less-than-or-equal (≤)")
    (check-dec-graphics #\z #\≥ "z must map to greater-than-or-equal (≥)")
    (check-dec-graphics #\{ #\π "{ must map to pi (π)")
    (check-dec-graphics #\| #\≠ "| must map to not-equal (≠)")
    (check-dec-graphics #\} #\£ "} must map to UK pound sign (£)")
    (check-dec-graphics #\~ #\· "~ must map to centred dot (·)")
    (check-dec-graphics #\_ #\Space "_ must map to a blank"))

  ;; An unmapped character (not in the DEC special graphics set) is returned as-is.
  ;; Digits and uppercase letters are NOT part of the set, so they pass through.
  (it "dec-graphics-unmapped-char-returned-unchanged"
    (check-dec-graphics #\5 #\5 "unmapped '5' must be returned unchanged")
    (check-dec-graphics #\A #\A "unmapped 'A' must be returned unchanged"))

  ;; Writing corner characters through the emulator in DEC graphics mode places
  ;; the correct box-drawing characters on the screen.
  (it "dec-graphics-via-emulator-corner-chars"
    (with-screen (s 20 5)
      (feed s (format nil "~C(0" #\Escape))  ; switch to DEC graphics
      (feed s "jklm")
      (expect (char= #\┘ (char-at s 0 0)))
      (expect (char= #\┐ (char-at s 1 0)))
      (expect (char= #\┌ (char-at s 2 0)))
      (expect (char= #\└ (char-at s 3 0)))))

  ;; define-dec-graphics-table is a defined macro in the actions package.
  (it "define-dec-graphics-table-macro-is-defined"
    (expect (macro-function 'cl-tmux/terminal/actions::define-dec-graphics-table))))

;;; ── SUITE: dcs-parsing ───────────────────────────────────────────────────────

(defun %feed-dcs (s payload)
  "Feed a DCS sequence (ESC P PAYLOAD ST) to screen S via screen-process-bytes."
  (screen-process-bytes s
    (babel:string-to-octets (format nil "~CP~A~C\\" #\Escape payload #\Escape)
                            :encoding :utf-8)))

(describe "terminal-suite/dcs-parsing"

  ;; ESC P ... ESC \ DCS sequence is consumed without crashing or corrupting output.
  (it "dcs-consumed-silently"
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
      (expect (char= #\a (char-at s 0 0)))
      (expect (char= #\b (char-at s 1 0)))))

  ;; After an ESC P ... ESC \ sequence, the parser is back in ground state.
  (it "dcs-parser-returns-ground-state-after-st"
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
      (expect (char= #\X (char-at s 0 0)))))

  ;;; ── XTGETTCAP (DCS + q <hex caps> ST) ────────────────────────────────────────

  ;; %hex-decode-string / %hex-encode-string convert XTGETTCAP hex cap names.
  (it "hex-decode-encode-roundtrip"
    (flet ((decode (s) (cl-tmux/terminal/parser::%hex-decode-string s))
           (encode (s) (cl-tmux/terminal/parser::%hex-encode-string s)))
      (dolist (c `(("Tc"   ,(lambda () (decode "5463"))   "5463 -> Tc")
                   ("5463" ,(lambda () (encode "Tc"))     "Tc -> 5463")
                   ("256"  ,(lambda () (decode "323536")) "323536 -> 256")
                   (nil    ,(lambda () (decode "5"))      "odd-length -> NIL")))
        (destructuring-bind (expected fn desc) c
          (declare (ignore desc))
          (expect (equal expected (funcall fn)))))))

  ;; XTGETTCAP replies DCS 1+r for known caps (Tc, RGB, colors) and DCS 0+r for unknown caps.
  (it "xtgettcap-responses-table"
    (dolist (row (list (list "+q5463"         (format nil "~CP1+r5463~C\\"          #\Escape #\Escape) "Tc → DCS 1+r 5463")
                       (list "+q524742"        (format nil "~CP1+r524742~C\\"        #\Escape #\Escape) "RGB → DCS 1+r 524742")
                       (list "+q636f6c6f7273"  (format nil "~CP1+r636f6c6f7273=323536~C\\" #\Escape #\Escape) "colors → DCS 1+r with =323536")
                       (list "+q5878"          (format nil "~CP0+r5878~C\\"          #\Escape #\Escape) "unknown cap → DCS 0+r")))
      (destructuring-bind (dcs-input expected desc) row
        (declare (ignore desc))
        (with-screen (s 20 5)
          (%feed-dcs s dcs-input)
          (expect (string= expected (first (cl-tmux/terminal/types:screen-response-queue s))))))))

  ;;; ── DECRQSS (DCS $ q <setting> ST) ───────────────────────────────────────────

  ;; DECRQSS $q m reports the current SGR pen: ESC P 1 $ r <params> m ST.
  (it "decrqss-sgr-reports-current-pen"
    (with-screen (s 20 5)
      (feed s (esc "[1;31m"))        ; bold red pen
      (%feed-dcs s "$qm")
      (expect (string= (format nil "~CP1$r0;1;31m~C\\" #\Escape #\Escape)
                       (first (cl-tmux/terminal/types:screen-response-queue s))))))

  ;; DECRQSS $q r reports the scroll region (1-based): ESC P 1 $ r top;bottom r ST.
  (it "decrqss-scroll-region-reports-margins"
    (with-screen (s 20 5)
      (%feed-dcs s "$qr")
      (expect (string= (format nil "~CP1$r1;5r~C\\" #\Escape #\Escape)
                       (first (cl-tmux/terminal/types:screen-response-queue s))))))

  ;; DECRQSS $q SP q reports the DECSCUSR cursor shape.
  (it "decrqss-cursor-style-reports-shape"
    (with-screen (s 20 5)
      (feed s (esc "[3 q"))          ; DECSCUSR shape 3
      (%feed-dcs s "$q q")
      (expect (string= (format nil "~CP1$r3 q~C\\" #\Escape #\Escape)
                       (first (cl-tmux/terminal/types:screen-response-queue s))))))

  ;; DECRQSS for an unsupported setting replies ESC P 0 $ r ST (invalid).
  (it "decrqss-unknown-reports-invalid"
    (with-screen (s 20 5)
      (%feed-dcs s "$qx")
      (expect (string= (format nil "~CP0$r~C\\" #\Escape #\Escape)
                       (first (cl-tmux/terminal/types:screen-response-queue s)))))))
