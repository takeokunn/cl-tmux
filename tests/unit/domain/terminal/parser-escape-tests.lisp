(in-package #:cl-tmux/test)

;;;; Parser tests (src/terminal/parser.lisp).
;;;; ESC/CSI coverage.

(describe "special"

  ;; CSI < t (XTPOPTITLE) and CSI = c (DA3) use the < / = private markers; the byte
  ;; must route to the marker slot, not abort the sequence and print the final byte
  ;; as a stray char.
  (it "csi-private-lt-marker-consumed-not-stray"
    (with-screen (s 10 2)
      (feed s "a")
      (feed s (esc "[<t"))       ; XTPOPTITLE - pop title (no-op), prints nothing
      (feed s "b")
      (expect (char= #\a (char-at s 0 0)))
      (expect (char= #\b (char-at s 1 0)))))

  ;; ESC # 8 (DECALN) fills the entire screen with 'E' (the VT100 alignment test).
  (it "esc-hash-8-decaln-fills-screen-with-e"
    (with-screen (s 4 2)
      (feed s (esc "#8"))
      (dotimes (y 2)
        (dotimes (x 4)
          (expect (char= #\E (char-at s x y)))))))

  ;; ESC # <selector> consumes the selector byte; ESC # 5 (DECSWL, no-op) prints
  ;; nothing - the byte must not abort the sequence and print as a stray char.
  (it "esc-hash-selector-consumed-not-stray"
    (with-screen (s 10 2)
      (feed s "a")
      (feed s (esc "#5"))        ; DECSWL - single-width line, no-op
      (feed s "b")
      (expect (char= #\a (char-at s 0 0)))
      (expect (char= #\b (char-at s 1 0)))))

  ;; ESC * X (designate G2) and ESC + X (designate G3) consume the designator byte
  ;; without printing it as a stray char (G2/G3 accepted but not modeled).
  (it "esc-star-plus-g2-g3-designator-consumed-not-stray"
    (with-screen (s 10 2)
      (feed s "a")
      (feed s (esc "*0"))        ; designate G2 = DEC graphics (consumes '0')
      (feed s (esc "+B"))        ; designate G3 = ASCII (consumes 'B')
      (feed s "b")
      (expect (char= #\a (char-at s 0 0)))
      (expect (char= #\b (char-at s 1 0)))))

  ;; ESC SP F (S7C1T) and ESC % G (select UTF-8) consume their trailing byte without
  ;; printing it as a stray char.
  (it "esc-space-and-percent-two-byte-seqs-consumed-not-stray"
    (with-screen (s 10 2)
      (feed s "a")
      (feed s (esc " F"))        ; ESC SP F - S7C1T (consumes 'F')
      (feed s (esc "%G"))        ; ESC % G - select UTF-8 (consumes 'G')
      (feed s "b")
      (expect (char= #\a (char-at s 0 0)))
      (expect (char= #\b (char-at s 1 0)))))

  ;; An unrecognised CSI final character is silently ignored; parser recovers.
  (it "csi-unknown"
    (with-screen (s 10 2)
      (feed s "a")
      ;; ESC [ z  -- 'z' is not a standard CSI final
      (feed s (esc "[z"))
      (feed s "b")
      (expect (char= #\a (char-at s 0 0)))
      (expect (char= #\b (char-at s 1 0)))))

  ;; ESC[?25l (hide cursor) and ESC[?25h (show cursor) do not crash.
  (it "dec-pm-hide-show-cursor"
    (with-screen (s 10 2)
      (feed s "a")
      (feed s (esc "[?25l"))    ; hide cursor - accepted silently
      (feed s "b")
      (feed s (esc "[?25h"))    ; show cursor - accepted silently
      (feed s "c")
      ;; All three characters must be on screen.
      (expect (char= #\a (char-at s 0 0)))
      (expect (char= #\b (char-at s 1 0)))
      (expect (char= #\c (char-at s 2 0))))))
