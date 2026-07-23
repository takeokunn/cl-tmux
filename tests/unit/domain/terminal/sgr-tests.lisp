(in-package #:cl-tmux/test)

;;;; SGR tests (src/terminal/sgr.lisp).
;;;; Tests: sgr suite.

;;; ── SUITE: sgr ──────────────────────────────────────────────────────────────

(describe "terminal-suite/sgr"

  ;; Standard foreground SGR codes 31-37 set fg indices 1-7.
  (it "sgr-foreground-table"
    (loop for code from 31 to 37
          for expected-fg from 1 to 7
          do (with-screen (s 10 2)
               (feed s (esc "[~DmX" code))
               (expect (= expected-fg (fg-at s 0 0))))))

  ;; Standard background SGR codes 41-47 set bg indices 1-7.
  (it "sgr-background-table"
    (loop for code from 41 to 47
          for expected-bg from 1 to 7
          do (with-screen (s 10 2)
               (feed s (esc "[~DmX" code))
               (expect (= expected-bg (bg-at s 0 0))))))

  ;; Bright foreground SGR codes 90-97 set fg indices 8-15.
  (it "sgr-bright-foreground-table"
    (loop for code from 90 to 97
          for expected-fg from 8 to 15
          do (with-screen (s 10 2)
               (feed s (esc "[~DmX" code))
               (expect (= expected-fg (fg-at s 0 0))))))

  ;; SGR 1/2/4/5/7/8/9 each set their respective attribute bit on the written cell.
  (it "sgr-basic-attrs-set-table"
    (dolist (c '(("[1mX" 0 "SGR 1 → bold (bit 0)")
                 ("[2mX" 1 "SGR 2 → dim (bit 1)")
                 ("[7mX" 2 "SGR 7 → reverse (bit 2)")
                 ("[4mX" 3 "SGR 4 → underline (bit 3)")
                 ("[5mX" 4 "SGR 5 → blink (bit 4)")
                 ("[8mX" 6 "SGR 8 → conceal (bit 6)")
                 ("[9mX" 7 "SGR 9 → strikethrough (bit 7)")))
      (destructuring-bind (seq bit desc) c
        (declare (ignore desc))
        (with-screen (s 10 2)
          (feed s (esc seq))
          (expect (logbitp bit (attrs-at s 0 0)))))))

  ;; SGR 28/29 each clear their respective attribute bit.
  (it "sgr-attr-clear-table"
    (dolist (c '(("[8mX" "[28mY" 6 "SGR 28 clears conceal (bit 6)")
                 ("[9mX" "[29mY" 7 "SGR 29 clears strikethrough (bit 7)")))
      (destructuring-bind (set-seq clear-seq bit desc) c
        (declare (ignore desc))
        (with-screen (s 10 2)
          (feed s (esc set-seq))
          (feed s (esc clear-seq))
          (expect (logbitp bit (attrs-at s 1 0)) :to-be-falsy)))))

  ;; SGR 0 after setting colours and bold restores defaults on the next cell.
  (it "sgr-reset"
    (with-screen (s 10 2)
      (feed s (esc "[31;1mX"))
      (feed s (esc "[0mY"))
      (check-cell s 1 0 :fg cl-tmux/terminal/types:+default-color+ :bg cl-tmux/terminal/types:+default-color+ :attrs 0)))

  ;; SGR 39 resets the foreground colour to the default (7) without touching bg or attrs.
  (it "sgr-default-fg-39"
    (with-screen (s 10 2)
      (feed s (esc "[31m"))    ; fg → 1 (red)
      (feed s (esc "[39mX"))   ; fg → default sentinel
      (check-sgr-state s :fg cl-tmux/terminal/types:+default-color+ :bg cl-tmux/terminal/types:+default-color+ :attrs 0)
      (expect (= cl-tmux/terminal/types:+default-color+ (fg-at s 0 0)))))

  ;; SGR 49 resets the background colour to the default (0) without touching fg or attrs.
  (it "sgr-default-bg-49"
    (with-screen (s 10 2)
      (feed s (esc "[42m"))    ; bg → 2 (green)
      (feed s (esc "[49mX"))   ; bg → default sentinel
      (check-sgr-state s :fg cl-tmux/terminal/types:+default-color+ :bg cl-tmux/terminal/types:+default-color+ :attrs 0)
      (expect (= cl-tmux/terminal/types:+default-color+ (bg-at s 0 0)))))

  ;; SGR 22 clears both the bold bit (0) and the dim bit (1).
  (it "sgr-bold-dim-off-22"
    (with-screen (s 10 2)
      (feed s (esc "[1;2m"))   ; bold + dim on
      (feed s (esc "[22mX"))   ; both off
      (expect (zerop (logand (attrs-at s 0 0) #b011)))))

  ;; ESC[1;31;42m sets bold, fg=1, bg=2 simultaneously.
  (it "sgr-compound"
    (with-screen (s 10 2)
      (feed s (esc "[1;31;42mX"))
      (expect (= 1 (fg-at s 0 0)))
      (expect (= 2 (bg-at s 0 0)))
      (expect (logbitp 0 (attrs-at s 0 0)))))

  ;; ESC[91m sets fg=9 (bright red).
  (it "sgr-bright-red"
    (with-screen (s 10 2)
      (feed s (esc "[91mR"))
      (expect (= 9 (fg-at s 0 0)))))

  ;; SGR 3 sets the italic attribute bit (5) and must NOT set the dim bit (1).
  (it "sgr-italic-sets-italic-bit-not-dim"
    (with-screen (s 10 2)
      (feed s (esc "[3mX"))
      (expect (logbitp 5 (attrs-at s 0 0)))
      (expect (logbitp 1 (attrs-at s 0 0)) :to-be-falsy)))

  ;; SGR 23 clears the italic bit (5) without touching other attributes.
  (it "sgr-italic-off-23"
    (with-screen (s 10 2)
      (feed s (esc "[3;1mX"))  ; italic + bold on
      (feed s (esc "[23mY"))   ; italic off
      (expect (logbitp 5 (attrs-at s 1 0)) :to-be-falsy)
      (expect (logbitp 0 (attrs-at s 1 0)))))

  ;; apply-sgr 38;5;N/48;5;N sets fg/bg to the 256-colour palette index N.
  (it "sgr-256color-apply-sgr-table"
    (dolist (c '((38 cl-tmux/terminal/types:screen-cur-fg 200 "256-color fg=200")
                 (48 cl-tmux/terminal/types:screen-cur-bg  42 "256-color bg=42")))
      (destructuring-bind (code accessor n desc) c
        (declare (ignore desc))
        (with-screen (s 10 2)
          (cl-tmux/terminal/sgr:apply-sgr s (list code 5 n))
          (expect (= n (funcall accessor s)))))))

  ;; apply-sgr 38;2;R;G;B and 48;2;R;G;B encode true-colour fg/bg (bit 24 = truecolor flag).
  (it "sgr-truecolor-apply-sgr-table"
    (dolist (c (list
                (list '(38 2 255 128 0) 'cl-tmux/terminal/types:screen-cur-fg
                      (logior #x1000000 (ash 255 16) (ash 128 8) 0) "truecolor fg 255;128;0")
                (list '(48 2 0 128 255) 'cl-tmux/terminal/types:screen-cur-bg
                      (logior #x1000000 (ash 0 16) (ash 128 8) 255) "truecolor bg 0;128;255")))
      (destructuring-bind (params accessor expected desc) c
        (declare (ignore desc))
        (with-screen (s 10 2)
          (cl-tmux/terminal/sgr:apply-sgr s params)
          (expect (= expected (funcall accessor s)))))))

  ;; ESC[38;5;N m and ESC[48;5;N m set fg/bg 256-colour indices via the terminal emulator.
  (it "sgr-256color-emulator-table"
    (dolist (c '(("[38;5;200mX" fg-at 200 "256-color fg=200 via ESC[38;5;200m")
                 ("[48;5;42mX"  bg-at  42 "256-color bg=42  via ESC[48;5;42m")))
      (destructuring-bind (seq cell-fn n desc) c
        (declare (ignore desc))
        (with-screen (s 10 2)
          (feed s (esc seq))
          (expect (= n (funcall cell-fn s 0 0)))))))

  ;; SGR 38;2;0;0;0 encodes true-black: bit 24 set, R=G=B=0.
  (it "sgr-truecolor-black"
    (with-screen (s 10 2)
      (cl-tmux/terminal/sgr:apply-sgr s '(38 2 0 0 0))
      (expect (= #x1000000 (cl-tmux/terminal/types:screen-cur-fg s)))))

  ;;; ── colon-delimited (ISO 8613-6) SGR ─────────────────────────────────────────
  ;;;
  ;;; Modern apps (neovim, many TUIs) emit true-colour as 38:2:R:G:B and 256-colour
  ;;; as 38:5:N with COLON separators, optionally with a colourspace-id field
  ;;; (38:2:cs:R:G:B) which may be empty (38:2::R:G:B).  The parser groups a colon
  ;;; parameter into a list so apply-sgr applies it (rather than dropping it).

  ;; apply-sgr with a colon group (a list) sets the true-colour encoding.
  (it "sgr-colon-group-direct-truecolor"
    (with-screen (s 10 2)
      (cl-tmux/terminal/sgr:apply-sgr s '((38 2 255 128 0)))
      (expect (= (logior #x1000000 (ash 255 16) (ash 128 8) 0)
             (cl-tmux/terminal/types:screen-cur-fg s)))))

  ;; ESC[38:2:255:128:0m (ISO 8613-6 colon true-colour) sets fg like the ; form.
  (it "sgr-colon-truecolor-fg-via-emulator"
    (with-screen (s 10 2)
      (feed s (esc "[38:2:255:128:0mX"))
      (expect (= (logior #x1000000 (ash 255 16) (ash 128 8) 0) (fg-at s 0 0)))))

  ;; ESC[38:2::255:128:0m (empty colourspace-id field) still applies the RGB —
  ;; the RGB are taken as the last three sub-parameters, skipping the empty field.
  (it "sgr-colon-truecolor-empty-colorspace"
    (with-screen (s 10 2)
      (feed s (esc "[38:2::255:128:0mX"))
      (expect (= (logior #x1000000 (ash 255 16) (ash 128 8) 0) (fg-at s 0 0)))))

  ;; ESC[38:2:1:255:128:0m (explicit colourspace-id 1) skips the CS field, applies RGB.
  (it "sgr-colon-truecolor-explicit-colorspace"
    (with-screen (s 10 2)
      (feed s (esc "[38:2:1:255:128:0mX"))
      (expect (= (logior #x1000000 (ash 255 16) (ash 128 8) 0) (fg-at s 0 0)))))

  ;; ESC[38:5:200m (colon 256-colour) sets fg=200.
  (it "sgr-colon-256color-via-emulator"
    (with-screen (s 10 2)
      (feed s (esc "[38:5:200mX"))
      (expect (= 200 (fg-at s 0 0)))))

  ;; ESC[48:2:0:128:255m sets the background true-colour.
  (it "sgr-colon-truecolor-bg-via-emulator"
    (with-screen (s 10 2)
      (feed s (esc "[48:2:0:128:255mX"))
      (expect (= (logior #x1000000 (ash 0 16) (ash 128 8) 255) (bg-at s 0 0)))))

  ;; ESC[1;38:2:255:0:0m — a colon group amid ;-params: bold AND true-red fg.
  (it "sgr-colon-mixed-with-semicolon-params"
    (with-screen (s 10 2)
      (feed s (esc "[1;38:2:255:0:0mX"))
      (expect (= (logior #x1000000 (ash 255 16)) (fg-at s 0 0)))
      (expect (logbitp 0 (attrs-at s 0 0)) :to-be-truthy)))

  ;; A (4 3) colon group (undercurl) applies underline (4) — same pen attrs as SGR 4.
  (it "sgr-colon-undercurl-applies-underline"
    (with-screen (s 10 2)
      (cl-tmux/terminal/sgr:apply-sgr s '(4))
      (let ((plain-underline (cl-tmux/terminal/types:screen-cur-attrs s)))
        (cl-tmux/terminal/sgr:apply-sgr s '(0))           ; reset pen
        (cl-tmux/terminal/sgr:apply-sgr s '((4 3)))       ; undercurl colon group
        (expect (= plain-underline (cl-tmux/terminal/types:screen-cur-attrs s))))))

  ;;; ── %pen-to-sgr-params (inverse SGR, for DECRQSS) ────────────────────────────

  ;; %pen-to-sgr-params reconstructs the SGR parameter string from fg/bg/attrs/unicode.
  (it "pen-to-sgr-params-table"
    (dolist (c (list
                (list cl-tmux/terminal/types:+default-color+ cl-tmux/terminal/types:+default-color+ 0 0  "0"          "default pen")
                (list 1 cl-tmux/terminal/types:+default-color+ 1 0  "0;1;31"       "bold red fg (default bg)")
                (list (logior #x1000000 (ash 255 16) (ash 128 8) 0) cl-tmux/terminal/types:+default-color+ 0 0
                      "0;38;2;255;128;0"        "truecolor fg (default bg)")
                (list cl-tmux/terminal/types:+default-color+ 12 0 0 "0;104"        "bright bg 12 (default fg)")))
      (destructuring-bind (fg bg attrs unicode expected desc) c
        (declare (ignore desc))
        (expect (string= expected
                     (cl-tmux/terminal/sgr:%pen-to-sgr-params fg bg attrs unicode))))))

  ;;; ── Coverage gap: %pen-to-sgr-params attrs2 (double-underline / overline) ────
  ;;;
  ;;; pen-to-sgr-params-table above only ever passes attrs2=0, so the two
  ;;; attrs2-driven emission branches (SGR 21 double-underline, SGR 53 overline)
  ;;; were never exercised by the inverse-SGR reconstruction.

  ;; %pen-to-sgr-params emits ;21 for double-underline and ;53 for overline when
  ;; set in ATTRS2, and both together when both bits are set.
  (it "pen-to-sgr-params-attrs2-table"
    (dolist (c (list
                (list cl-tmux/terminal/types:+attr2-double-underline+
                      "0;21" "double-underline bit alone → ;21")
                (list cl-tmux/terminal/types:+attr2-overline+
                      "0;53" "overline bit alone → ;53")
                (list (logior cl-tmux/terminal/types:+attr2-double-underline+
                              cl-tmux/terminal/types:+attr2-overline+)
                      "0;21;53" "both bits → ;21;53 in declaration order")))
      (destructuring-bind (attrs2 expected desc) c
        (declare (ignore desc))
        (expect (string= expected
                     (cl-tmux/terminal/sgr:%pen-to-sgr-params
                      cl-tmux/terminal/types:+default-color+
                      cl-tmux/terminal/types:+default-color+
                      0 attrs2))))))

  ;; SGR 0 after setting italic, conceal, and strikethrough zeroes all attr bits.
  (it "sgr-reset-clears-new-attrs"
    (with-screen (s 10 2)
      (feed s (esc "[3;8;9mX"))    ; italic + conceal + strikethrough on
      (feed s (esc "[0mY"))        ; SGR reset
      (check-cell s 1 0 :fg cl-tmux/terminal/types:+default-color+ :bg cl-tmux/terminal/types:+default-color+ :attrs 0)))

  ;; SGR 22 (bold+dim off) must NOT clear the italic bit (5).
  (it "sgr-22-does-not-clear-italic"
    (with-screen (s 10 2)
      (feed s (esc "[1;2;3mX"))    ; bold + dim + italic on
      (feed s (esc "[22mY"))       ; bold + dim off
      (expect (logbitp 5 (attrs-at s 1 0))))))
