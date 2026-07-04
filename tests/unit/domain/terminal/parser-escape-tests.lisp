(in-package #:cl-tmux/test)

;;;; Parser tests (src/terminal/parser.lisp).
;;;; ESC/CSI coverage.

(in-suite special)

(test csi-private-lt-marker-consumed-not-stray
  "CSI < t (XTPOPTITLE) and CSI = c (DA3) use the < / = private markers; the byte
   must route to the marker slot, not abort the sequence and print the final byte
   as a stray char."
  (with-screen (s 10 2)
    (feed s "a")
    (feed s (esc "[<t"))       ; XTPOPTITLE - pop title (no-op), prints nothing
    (feed s "b")
    (is (char= #\a (char-at s 0 0)) "'a' at column 0")
    (is (char= #\b (char-at s 1 0))
        "'b' at column 1 - no stray 't' printed between them")))

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
   nothing - the byte must not abort the sequence and print as a stray char."
  (with-screen (s 10 2)
    (feed s "a")
    (feed s (esc "#5"))        ; DECSWL - single-width line, no-op
    (feed s "b")
    (is (char= #\a (char-at s 0 0)) "'a' at column 0")
    (is (char= #\b (char-at s 1 0))
        "'b' at column 1 - no stray '5' printed between them")))

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
        "'b' at column 1 - no stray '0' or 'B' from the G2/G3 designators")))

(test esc-space-and-percent-two-byte-seqs-consumed-not-stray
  "ESC SP F (S7C1T) and ESC % G (select UTF-8) consume their trailing byte without
   printing it as a stray char."
  (with-screen (s 10 2)
    (feed s "a")
    (feed s (esc " F"))        ; ESC SP F - S7C1T (consumes 'F')
    (feed s (esc "%G"))        ; ESC % G - select UTF-8 (consumes 'G')
    (feed s "b")
    (is (char= #\a (char-at s 0 0)) "'a' at column 0")
    (is (char= #\b (char-at s 1 0))
        "'b' at column 1 - no stray 'F'/'G' from the two-byte ESC sequences")))

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
    (feed s (esc "[?25l"))    ; hide cursor - accepted silently
    (feed s "b")
    (feed s (esc "[?25h"))    ; show cursor - accepted silently
    (feed s "c")
    ;; All three characters must be on screen.
    (is (char= #\a (char-at s 0 0)))
    (is (char= #\b (char-at s 1 0)))
    (is (char= #\c (char-at s 2 0)))))
